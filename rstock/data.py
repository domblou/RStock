"""Market-data acquisition, isolated from feature engineering."""

from __future__ import annotations

from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from datetime import date, timedelta
from typing import Any

import pandas as pd


Downloader = Callable[..., pd.DataFrame]


@dataclass(slots=True)
class DownloadResult:
    prices: pd.DataFrame
    symbols: list[str]
    failed_symbols: list[str]


def _yfinance_download(*args: Any, **kwargs: Any) -> pd.DataFrame:
    try:
        import yfinance as yf
    except ImportError as exc:  # pragma: no cover - depends on optional runtime install
        raise RuntimeError("Install the project dependencies to download market data") from exc
    return yf.download(*args, **kwargs)


def _flatten_single_symbol(
    raw: pd.DataFrame, provider_symbol: str, canonical_symbol: str
) -> pd.DataFrame:
    if raw.empty:
        raise ValueError("provider returned no rows")
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
        f"{canonical_symbol}.{target}": result[source]
        for source, target in field_names.items()
        if source in result.columns
    }
    if (
        f"{canonical_symbol}.Open" not in available
        or f"{canonical_symbol}.Close" not in available
    ):
        raise ValueError("provider response has no Open/Close columns")
    return pd.DataFrame(available, index=result.index)


def download_market_data(
    symbols: Sequence[str],
    history_days: int,
    *,
    downloader: Downloader | None = None,
    today: date | None = None,
    provider_symbols: Mapping[str, str] | None = None,
) -> DownloadResult:
    """Download each symbol independently so one provider error does not abort a run."""

    if history_days < 1:
        raise ValueError("history_days must be positive")
    provider = downloader or _yfinance_download
    start = (today or date.today()) - timedelta(days=history_days)
    frames: list[pd.DataFrame] = []
    successful: list[str] = []
    failed: list[str] = []

    for symbol in symbols:
        provider_symbol = (provider_symbols or {}).get(symbol, symbol)
        try:
            raw = provider(
                provider_symbol,
                start=start.isoformat(),
                progress=False,
                auto_adjust=False,
                actions=False,
            )
            frames.append(_flatten_single_symbol(raw, provider_symbol, symbol))
            successful.append(symbol)
        except Exception:  # provider errors are intentionally isolated per symbol
            failed.append(symbol)

    prices = pd.concat(frames, axis=1).sort_index() if frames else pd.DataFrame()
    prices.index.name = "Date"
    return DownloadResult(prices=prices, symbols=successful, failed_symbols=failed)
