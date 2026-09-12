"""Feature preparation shared by training and daily prediction."""

from __future__ import annotations

import re
from collections.abc import Sequence

import numpy as np
import pandas as pd


def _normalise_datetime_index(stock: pd.DataFrame) -> pd.DataFrame:
    if not isinstance(stock.index, pd.DatetimeIndex):
        stock = stock.copy()
        stock.index = pd.to_datetime(stock.index)
    if stock.index.tz is not None:
        stock = stock.copy()
        stock.index = stock.index.tz_localize(None)
    stock.index.name = "Date"
    return stock.sort_index()


def prepare_dataset(
    stock: pd.DataFrame,
    stock_symbols: Sequence[str],
    up_down_threshold: float = 0.01,
) -> pd.DataFrame:
    """Create UPDW and previous-observation UPDW features like the R function.

    The output is newest-first. Missing values are intentionally replaced by zero
    for phase-1 compatibility, even though that conflates missing and negative data.
    """

    stock = _normalise_datetime_index(stock)
    columns: dict[str, pd.Series] = {}

    for symbol in stock_symbols:
        open_name = f"{symbol}.Open"
        close_name = f"{symbol}.Close"
        missing = [name for name in (open_name, close_name) if name not in stock.columns]
        if missing:
            raise KeyError(f"Missing price columns for {symbol}: {', '.join(missing)}")

        opcl = 1.0 - stock[open_name] / stock[close_name]
        updw = pd.Series(
            np.where(opcl.isna(), np.nan, (opcl >= up_down_threshold).astype(float)),
            index=stock.index,
        )
        columns[f"{symbol}.UPDW"] = updw
        columns[f"{symbol}.DAY_MINUS_1_UPDW"] = updw.shift(1)

    prepared = pd.DataFrame(columns, index=stock.index)
    prepared = prepared.reindex(sorted(prepared.columns), axis=1)
    prepared = prepared.sort_index(ascending=False)

    # R date components are zero-based: Sunday, January 1 and January are all 0.
    prepared["wday"] = (prepared.index.dayofweek + 1) % 7
    prepared["yday"] = prepared.index.dayofyear - 1
    prepared["mon"] = prepared.index.month - 1

    # After descending sort, the final row is the earliest observation whose lag is NA.
    prepared = prepared.iloc[:-1]
    return prepared.fillna(0)


def prepare_prediction_row(prepared: pd.DataFrame) -> pd.DataFrame:
    """Turn the newest observed UPDW values into tomorrow's DAY_MINUS_1 inputs."""

    if prepared.empty:
        raise ValueError("At least two market observations are required for prediction")
    row = prepared.iloc[[0]].copy()
    row = row.loc[:, [name for name in row.columns if "DAY_MINUS_1_UPDW" not in name]]
    row = row.rename(
        columns={
            name: name.removesuffix(".UPDW") + ".DAY_MINUS_1_UPDW"
            for name in row.columns
            if name.endswith(".UPDW")
        }
    )
    return row


def predictor_columns(
    dataset: pd.DataFrame,
    feature_symbols: Sequence[str],
    date_feature_regex: str = "",
) -> list[str]:
    """Select predictors in dataset order, matching the intent of the R regex."""

    requested = {f"{symbol}.DAY_MINUS_1_UPDW" for symbol in feature_symbols}
    date_pattern = re.compile(date_feature_regex) if date_feature_regex else None
    return [
        name
        for name in dataset.columns
        if name in requested or (date_pattern is not None and date_pattern.search(name))
    ]

