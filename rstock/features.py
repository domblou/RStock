"""Feature preparation shared by training and daily prediction."""

from __future__ import annotations

import re
from collections.abc import Sequence

import numpy as np
import pandas as pd


def _normalise_datetime_index(stock: pd.DataFrame) -> pd.DataFrame:
    stock = stock.copy()
    if not isinstance(stock.index, pd.DatetimeIndex):
        stock.index = pd.to_datetime(stock.index, errors="raise")
    if stock.index.hasnans:
        raise ValueError("Market data index cannot contain NaT")
    if stock.index.tz is not None:
        stock.index = stock.index.tz_localize(None)
    stock.index = stock.index.normalize()
    if stock.index.has_duplicates:
        raise ValueError("Market data index must contain one row per date")
    stock.index.name = "Date"
    return stock.sort_index()


def prepare_dataset(
    stock: pd.DataFrame,
    stock_symbols: Sequence[str],
    up_down_threshold: float = 0.01,
) -> pd.DataFrame:
    """Create outcomes and previous real-observation features without imputation."""

    stock = _normalise_datetime_index(stock)
    columns: dict[str, pd.Series] = {}

    for symbol in stock_symbols:
        open_name = f"{symbol}.Open"
        close_name = f"{symbol}.Close"
        missing = [name for name in (open_name, close_name) if name not in stock.columns]
        if missing:
            raise KeyError(f"Missing price columns for {symbol}: {', '.join(missing)}")

        opcl = (1.0 - stock[open_name] / stock[close_name]).replace(
            [np.inf, -np.inf], np.nan
        )
        updw = pd.Series(
            np.where(opcl.isna(), np.nan, (opcl >= up_down_threshold).astype(float)),
            index=stock.index,
        )
        columns[f"{symbol}.UPDW"] = updw
        columns[f"{symbol}.DAY_MINUS_1_UPDW"] = updw.dropna().shift(1).reindex(stock.index)

    prepared = pd.DataFrame(columns, index=stock.index)
    prepared = prepared.reindex(sorted(prepared.columns), axis=1)
    prepared = prepared.sort_index()

    # R date components are zero-based: Sunday, January 1 and January are all 0.
    prepared["wday"] = (prepared.index.dayofweek + 1) % 7
    prepared["yday"] = prepared.index.dayofyear - 1
    prepared["mon"] = prepared.index.month - 1

    return prepared


def prepare_prediction_row(
    prepared: pd.DataFrame,
    *,
    as_of_date: object | None = None,
    target_date: object | None = None,
) -> pd.DataFrame:
    """Build future predictors from the latest values known at ``as_of_date``."""

    if prepared.empty:
        raise ValueError("At least two market observations are required for prediction")
    ordered = _normalise_datetime_index(prepared)
    as_of = pd.Timestamp(as_of_date).normalize() if as_of_date is not None else ordered.index.max()
    eligible = ordered.loc[ordered.index <= as_of]
    if eligible.empty:
        raise ValueError("No observations are available on or before as_of_date")
    target = pd.Timestamp(target_date).normalize() if target_date is not None else as_of

    values: dict[str, float] = {}
    for name in (column for column in eligible if column.endswith(".UPDW")):
        known = eligible[name].dropna()
        values[name.removesuffix(".UPDW") + ".DAY_MINUS_1_UPDW"] = (
            float(known.iloc[-1]) if not known.empty else np.nan
        )
    values["wday"] = (target.dayofweek + 1) % 7
    values["yday"] = target.dayofyear - 1
    values["mon"] = target.month - 1
    return pd.DataFrame([values], index=pd.DatetimeIndex([target], name="Date"))


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
