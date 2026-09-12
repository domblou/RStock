"""Economically distinct market returns aligned to observed sessions."""

from __future__ import annotations

import numpy as np
import pandas as pd


def previous_observed_values(
    values: pd.Series,
    target_index: pd.DatetimeIndex,
    lag: int,
) -> pd.Series:
    """Return the nth value strictly before each target date.

    Positions advance only when the source symbol has a real observation, so
    weekends, holidays and symbol-specific gaps never count as synthetic lags.
    """

    if lag < 1:
        raise ValueError("lag must be positive")
    observed = pd.to_numeric(values, errors="coerce").dropna().sort_index()
    result = pd.Series(np.nan, index=target_index, dtype=float)
    if observed.empty:
        return result
    positions = observed.index.searchsorted(target_index, side="left") - lag
    valid = positions >= 0
    result.iloc[np.flatnonzero(valid)] = observed.to_numpy(dtype=float)[positions[valid]]
    return result


def calculate_returns(
    open_prices: pd.Series,
    close_prices: pd.Series,
    high_prices: pd.Series | None = None,
    low_prices: pd.Series | None = None,
) -> pd.DataFrame:
    """Calculate returns and, when OHLC is supplied, intraday excursions."""

    if not open_prices.index.equals(close_prices.index):
        raise ValueError("Open and close prices must use the same dates")
    index = pd.DatetimeIndex(open_prices.index)
    open_values = pd.to_numeric(open_prices, errors="coerce")
    close_values = pd.to_numeric(close_prices, errors="coerce")
    previous_close = previous_observed_values(close_values, index, 1)
    result = pd.DataFrame(
        {
            "overnight_return": open_values / previous_close - 1.0,
            "intraday_return": close_values / open_values - 1.0,
            "close_to_close_return": close_values / previous_close - 1.0,
        },
        index=index,
    )
    if (high_prices is None) != (low_prices is None):
        raise ValueError("High and low prices must be supplied together")
    if high_prices is not None and low_prices is not None:
        if not high_prices.index.equals(open_prices.index) or not low_prices.index.equals(
            open_prices.index
        ):
            raise ValueError("OHLC prices must use the same dates")
        high_values = pd.to_numeric(high_prices, errors="coerce")
        low_values = pd.to_numeric(low_prices, errors="coerce")
        result["mfe"] = high_values / open_values - 1.0
        result["mae"] = low_values / open_values - 1.0
    return result.replace([np.inf, -np.inf], np.nan)
