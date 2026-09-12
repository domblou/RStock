"""Market-data acquisition, isolated from feature engineering."""

from __future__ import annotations

from collections.abc import Callable
from datetime import date
from typing import Any, Protocol

import pandas as pd


Downloader = Callable[..., pd.DataFrame]


class MarketDataProvider(Protocol):
    source_name: str

    def fetch(
        self, provider_symbol: str, start: date, end_exclusive: date
    ) -> pd.DataFrame: ...


def _yfinance_download(*args: Any, **kwargs: Any) -> pd.DataFrame:
    try:
        import yfinance as yf
    except ImportError as exc:  # pragma: no cover - depends on optional runtime install
        raise RuntimeError("Install the project dependencies to download market data") from exc
    return yf.download(*args, **kwargs)


class YahooFinanceProvider:
    """Yahoo Finance adapter returning one canonical OHLCV frame per symbol."""

    source_name = "Yahoo Finance"

    def __init__(self, downloader: Downloader | None = None) -> None:
        self._downloader = downloader or _yfinance_download

    def fetch(
        self, provider_symbol: str, start: date, end_exclusive: date
    ) -> pd.DataFrame:
        raw = self._downloader(
            provider_symbol,
            start=start.isoformat(),
            end=end_exclusive.isoformat(),
            progress=False,
            auto_adjust=False,
            actions=False,
            threads=False,
        )
        return normalise_yahoo_prices(raw, provider_symbol)


def normalise_yahoo_prices(raw: pd.DataFrame, provider_symbol: str) -> pd.DataFrame:
    """Normalise one Yahoo response while retaining its raw OHLCV fields."""

    if raw.empty:
        return pd.DataFrame()
    result = raw.copy()
    if isinstance(result.columns, pd.MultiIndex):
        # yfinance has used both (field, ticker) and (ticker, field) layouts.
        for level in range(result.columns.nlevels):
            values = result.columns.get_level_values(level)
            if provider_symbol in values:
                result = result.xs(provider_symbol, axis=1, level=level, drop_level=True)
                break
    result.index = pd.to_datetime(result.index)
    if result.index.tz is not None:
        result.index = result.index.tz_localize(None)
    field_names = {
        "Open": "Open",
        "High": "High",
        "Low": "Low",
        "Close": "Close",
        "Volume": "Volume",
        "Adj Close": "Adjusted",
        "Adjusted": "Adjusted",
    }
    available = {
        target: result[source]
        for source, target in field_names.items()
        if source in result.columns
    }
    if "Open" not in available or "Close" not in available:
        raise ValueError("provider response has no Open/Close columns")
    normalised = pd.DataFrame(available, index=result.index)
    normalised.index = normalised.index.normalize()
    normalised.index.name = "Date"
    return normalised[~normalised.index.duplicated(keep="last")].sort_index()


def prefix_symbol_columns(prices: pd.DataFrame, symbol: str) -> pd.DataFrame:
    return prices.rename(columns={name: f"{symbol}.{name}" for name in prices.columns})
