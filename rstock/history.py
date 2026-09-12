"""Persistence of downloaded price history in the legacy long format."""

from __future__ import annotations

from pathlib import Path

import pandas as pd


HISTORY_COLUMNS = ["Date", "Symbol", "Field", "Value"]


def market_data_to_history(stock: pd.DataFrame, *, legacy_field_split: bool = True) -> pd.DataFrame:
    """Add OpCl and pivot prices to Date/Symbol/Field/Value rows.

    ``legacy_field_split=True`` intentionally splits at the first dot, reproducing
    the R bug for dotted tickers. It is explicit so a later migration phase can fix it.
    """

    wide = stock.copy()
    open_columns = [name for name in wide.columns if name.endswith(".Open")]
    for open_name in open_columns:
        symbol = open_name.removesuffix(".Open")
        close_name = f"{symbol}.Close"
        if close_name in wide:
            wide[f"{symbol}.OpCl"] = 1.0 - wide[open_name] / wide[close_name]
    wide = wide.reindex(sorted(wide.columns), axis=1)
    wide.index = pd.to_datetime(wide.index).strftime("%Y-%m-%d")
    wide.index.name = "Date"
    long = wide.reset_index().melt(id_vars="Date", var_name="variable", value_name="Value")

    if legacy_field_split:
        parts = long["variable"].str.split(".", n=1, expand=True)
    else:
        parts = long["variable"].str.rsplit(".", n=1, expand=True)
    if parts.shape[1] != 2:
        raise ValueError("Market-data columns must use SYMBOL.Field names")
    long["Symbol"] = parts[0]
    long["Field"] = parts[1]
    return long[HISTORY_COLUMNS].sort_values("Date", kind="stable").reset_index(drop=True)


def append_symbol_history(stock: pd.DataFrame, existing: pd.DataFrame | None = None) -> pd.DataFrame:
    """Append only dates after the legacy history's maximum date."""

    incoming = market_data_to_history(stock)
    if existing is None or existing.empty:
        return incoming
    existing = existing.copy()
    existing["Date"] = existing["Date"].astype(str)
    additions = incoming[incoming["Date"] > existing["Date"].max()]
    return pd.concat([existing, additions], ignore_index=True)


def read_symbol_history(path: Path) -> pd.DataFrame | None:
    if not path.exists():
        return None
    return pd.read_csv(path, dtype=str)


def write_symbol_history(history: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    history.to_csv(path, index=False)

