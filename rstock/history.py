"""Persistence of downloaded market history in a typed long format."""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from .returns import calculate_returns


HISTORY_COLUMNS = ["Date", "Symbol", "Field", "Value"]


def market_data_to_history(stock: pd.DataFrame) -> pd.DataFrame:
    """Add distinct return diagnostics and pivot to typed long history."""

    wide = stock.copy()
    wide.index = pd.to_datetime(wide.index, errors="raise").normalize()
    wide.index.name = "Date"
    open_columns = [name for name in wide.columns if name.endswith(".Open")]
    for open_name in open_columns:
        symbol = open_name.removesuffix(".Open")
        close_name = f"{symbol}.Close"
        if close_name in wide:
            high_name = f"{symbol}.High"
            low_name = f"{symbol}.Low"
            high = wide[high_name] if high_name in wide else None
            low = wide[low_name] if low_name in wide else None
            returns = calculate_returns(wide[open_name], wide[close_name], high, low)
            wide[f"{symbol}.OvernightReturn"] = returns["overnight_return"]
            wide[f"{symbol}.IntradayReturn"] = returns["intraday_return"]
            wide[f"{symbol}.CloseToCloseReturn"] = returns[
                "close_to_close_return"
            ]
            if "mfe" in returns:
                wide[f"{symbol}.MFE"] = returns["mfe"]
                wide[f"{symbol}.MAE"] = returns["mae"]
    wide = wide.reindex(sorted(wide.columns), axis=1)
    long = wide.reset_index().melt(id_vars="Date", var_name="variable", value_name="Value")

    parts = long["variable"].str.rsplit(".", n=1, expand=True)
    if parts.shape[1] != 2:
        raise ValueError("Market-data columns must use SYMBOL.Field names")
    long["Symbol"] = parts[0]
    long["Field"] = parts[1]
    return long[HISTORY_COLUMNS].sort_values("Date", kind="stable").reset_index(drop=True)


def append_symbol_history(stock: pd.DataFrame, existing: pd.DataFrame | None = None) -> pd.DataFrame:
    """Merge all observed rows by typed key, filling gaps and refreshing values."""

    incoming = market_data_to_history(stock)
    incoming = incoming.dropna(subset=["Value"])
    if existing is None or existing.empty:
        return incoming
    existing = _normalise_history(existing)
    combined = pd.concat([existing, incoming], ignore_index=True)
    return (
        combined.drop_duplicates(["Date", "Symbol", "Field"], keep="last")
        .sort_values(["Date", "Symbol", "Field"], kind="stable")
        .reset_index(drop=True)
    )


def read_symbol_history(path: Path) -> pd.DataFrame | None:
    if not path.exists():
        return None
    return _normalise_history(pd.read_csv(path))


def _normalise_history(history: pd.DataFrame) -> pd.DataFrame:
    missing = [column for column in HISTORY_COLUMNS if column not in history]
    if missing:
        raise ValueError(f"History is missing columns: {', '.join(missing)}")
    result = history[HISTORY_COLUMNS].copy()
    result["Date"] = pd.to_datetime(result["Date"], errors="raise").dt.normalize()
    result["Symbol"] = result["Symbol"].astype(str)
    result["Field"] = result["Field"].astype(str)
    result["Value"] = pd.to_numeric(result["Value"], errors="coerce")
    return result


def write_symbol_history(history: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    history.to_csv(path, index=False)
