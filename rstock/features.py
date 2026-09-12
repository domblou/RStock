"""Feature preparation shared by training and daily prediction."""

from __future__ import annotations

import re
from collections.abc import Sequence

import numpy as np
import pandas as pd

from .returns import calculate_returns, previous_observed_values


def intraday_target_column(symbol: str) -> str:
    return f"{symbol}.intraday_target"


def intraday_down_target_column(symbol: str) -> str:
    return f"{symbol}.intraday_down_target"


def intraday_return_column(symbol: str) -> str:
    return f"{symbol}.intraday_return"


def overnight_return_column(symbol: str) -> str:
    return f"{symbol}.overnight_return"


def close_to_close_return_column(symbol: str) -> str:
    return f"{symbol}.close_to_close_return"


def mfe_column(symbol: str) -> str:
    return f"{symbol}.mfe"


def mae_column(symbol: str) -> str:
    return f"{symbol}.mae"


def intraday_lag_column(symbol: str, lag: int) -> str:
    return f"{symbol}_intraday_J-{lag}"


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
    intraday_target_threshold: float = 0.01,
    lag_depth: int = 3,
    intraday_down_threshold: float = 0.01,
) -> pd.DataFrame:
    """Create intraday targets, diagnostics and strictly pre-open lag features."""

    if lag_depth < 1:
        raise ValueError("lag_depth must be positive")
    if intraday_target_threshold < 0 or intraday_down_threshold < 0:
        raise ValueError("Intraday target thresholds cannot be negative")
    stock = _normalise_datetime_index(stock)
    columns: dict[str, pd.Series] = {}

    for symbol in stock_symbols:
        open_name = f"{symbol}.Open"
        high_name = f"{symbol}.High"
        low_name = f"{symbol}.Low"
        close_name = f"{symbol}.Close"
        missing = [
            name
            for name in (open_name, high_name, low_name, close_name)
            if name not in stock.columns
        ]
        if missing:
            raise KeyError(f"Missing price columns for {symbol}: {', '.join(missing)}")

        returns = calculate_returns(
            stock[open_name],
            stock[close_name],
            stock[high_name],
            stock[low_name],
        )
        intraday = returns["intraday_return"]
        target = pd.Series(
            np.where(
                intraday.isna(),
                np.nan,
                (intraday >= intraday_target_threshold).astype(float),
            ),
            index=stock.index,
        )
        down_target = pd.Series(
            np.where(
                intraday.isna(),
                np.nan,
                (intraday <= -intraday_down_threshold).astype(float),
            ),
            index=stock.index,
        )
        columns[overnight_return_column(symbol)] = returns["overnight_return"]
        columns[intraday_return_column(symbol)] = intraday
        columns[close_to_close_return_column(symbol)] = returns[
            "close_to_close_return"
        ]
        columns[intraday_target_column(symbol)] = target
        columns[intraday_down_target_column(symbol)] = down_target
        columns[mfe_column(symbol)] = returns["mfe"]
        columns[mae_column(symbol)] = returns["mae"]
        for lag in range(1, lag_depth + 1):
            columns[intraday_lag_column(symbol, lag)] = previous_observed_values(
                intraday, stock.index, lag
            )

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
    lag_depth: int = 3,
) -> pd.DataFrame:
    """Build future predictors from the latest values known at ``as_of_date``."""

    if lag_depth < 1:
        raise ValueError("lag_depth must be positive")
    if prepared.empty:
        raise ValueError("At least two market observations are required for prediction")
    ordered = _normalise_datetime_index(prepared)
    as_of = pd.Timestamp(as_of_date).normalize() if as_of_date is not None else ordered.index.max()
    eligible = ordered.loc[ordered.index <= as_of]
    if eligible.empty:
        raise ValueError("No observations are available on or before as_of_date")
    target = pd.Timestamp(target_date).normalize() if target_date is not None else as_of

    values: dict[str, float] = {}
    suffix = ".intraday_return"
    for name in (column for column in eligible if column.endswith(suffix)):
        symbol = name.removesuffix(suffix)
        known = eligible[name].dropna()
        for lag in range(1, lag_depth + 1):
            values[intraday_lag_column(symbol, lag)] = (
                float(known.iloc[-lag]) if len(known) >= lag else np.nan
            )
    values["wday"] = (target.dayofweek + 1) % 7
    values["yday"] = target.dayofyear - 1
    values["mon"] = target.month - 1
    return pd.DataFrame([values], index=pd.DatetimeIndex([target], name="Date"))


def predictor_columns(
    dataset: pd.DataFrame,
    feature_symbols: Sequence[str],
    lag_depth: int = 3,
    date_feature_regex: str = "",
) -> list[str]:
    """Select only lagged intraday and explicitly allowed calendar features."""

    if lag_depth < 1:
        raise ValueError("lag_depth must be positive")
    requested = {
        intraday_lag_column(symbol, lag)
        for symbol in feature_symbols
        for lag in range(1, lag_depth + 1)
    }
    date_pattern = re.compile(date_feature_regex) if date_feature_regex else None
    return [
        name
        for name in dataset.columns
        if name in requested
        or (
            name in {"wday", "yday", "mon"}
            and date_pattern is not None
            and date_pattern.search(name)
        )
    ]
