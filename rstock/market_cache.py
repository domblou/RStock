"""Persistent, backend-neutral access to cached market data."""

from __future__ import annotations

import json
import logging
import os
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Callable, Protocol
from urllib.parse import quote

import exchange_calendars as xcals
import pandas as pd

from .config import RStockConfig
from .data import MarketDataProvider, YahooFinanceProvider, prefix_symbol_columns
from .progress import (
    CancellationCheck,
    ProgressCallback,
    check_cancellation,
    report_progress,
)
from .symbols import validate_symbol_universe


LOGGER = logging.getLogger(__name__)
CACHE_SCHEMA_VERSION = 1
WINDOWS_RESERVED_NAMES = {
    "CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4", "COM5",
    "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5",
    "LPT6", "LPT7", "LPT8", "LPT9",
}


@dataclass(frozen=True, slots=True)
class CacheEvent:
    symbol: str
    status: str
    rows: int
    message: str


@dataclass(slots=True)
class MarketDataResult:
    prices: pd.DataFrame
    symbols: list[str]
    failed_symbols: list[str]
    events: list[CacheEvent]


class MarketDataStore(Protocol):
    """Storage contract that a future DuckDB backend can implement."""

    def read(self, symbol: str) -> pd.DataFrame | None: ...
    def write(self, symbol: str, prices: pd.DataFrame) -> None: ...
    def cache_file(self, symbol: str) -> str: ...
    def read_metadata(self) -> dict[str, object]: ...
    def write_metadata(self, metadata: dict[str, object]) -> None: ...


def _safe_filename(symbol: str) -> str:
    encoded = quote(symbol, safe="._-")
    if encoded.upper() in WINDOWS_RESERVED_NAMES:
        encoded = f"_{encoded}"
    return f"{encoded}.parquet"


def _normalise_prices(prices: pd.DataFrame) -> pd.DataFrame:
    result = prices.copy()
    result.index = pd.to_datetime(result.index, errors="raise")
    if result.index.tz is not None:
        result.index = result.index.tz_localize(None)
    result.index = result.index.normalize()
    result.index.name = "Date"
    return result[~result.index.duplicated(keep="last")].sort_index()


def _frames_equal(left: pd.DataFrame, right: pd.DataFrame) -> bool:
    try:
        pd.testing.assert_frame_equal(left, right, check_dtype=False)
    except AssertionError:
        return False
    return True


class ParquetMarketDataStore:
    """One Parquet file per canonical symbol plus a small JSON manifest."""

    def __init__(self, market_directory: Path, metadata_path: Path) -> None:
        self.market_directory = market_directory
        self.metadata_path = metadata_path

    def path_for(self, symbol: str) -> Path:
        return self.market_directory / _safe_filename(symbol)

    def cache_file(self, symbol: str) -> str:
        return f"market/{_safe_filename(symbol)}"

    def read(self, symbol: str) -> pd.DataFrame | None:
        path = self.path_for(symbol)
        if not path.exists():
            return None
        return _normalise_prices(pd.read_parquet(path))

    def write(self, symbol: str, prices: pd.DataFrame) -> None:
        self.market_directory.mkdir(parents=True, exist_ok=True)
        destination = self.path_for(symbol)
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{destination.name}.", suffix=".tmp", dir=self.market_directory
        )
        os.close(descriptor)
        temporary = Path(temporary_name)
        try:
            _normalise_prices(prices).to_parquet(temporary, engine="pyarrow", index=True)
            temporary.replace(destination)
        finally:
            if temporary.exists():
                temporary.unlink()

    def read_metadata(self) -> dict[str, object]:
        if not self.metadata_path.exists():
            return {"schema_version": CACHE_SCHEMA_VERSION, "symbols": {}}
        metadata = json.loads(self.metadata_path.read_text(encoding="utf-8"))
        if metadata.get("schema_version") != CACHE_SCHEMA_VERSION:
            raise ValueError("Unsupported market cache metadata schema")
        if not isinstance(metadata.get("symbols"), dict):
            raise ValueError("Invalid market cache metadata")
        return metadata

    def write_metadata(self, metadata: dict[str, object]) -> None:
        self.metadata_path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{self.metadata_path.name}.",
            suffix=".tmp",
            dir=self.metadata_path.parent,
        )
        os.close(descriptor)
        temporary = Path(temporary_name)
        try:
            temporary.write_text(
                json.dumps(metadata, indent=2, sort_keys=True), encoding="utf-8"
            )
            temporary.replace(self.metadata_path)
        finally:
            if temporary.exists():
                temporary.unlink()


@dataclass(slots=True)
class _SymbolResult:
    symbol: str
    prices: pd.DataFrame | None
    event: CacheEvent
    metadata: dict[str, object] | None
    failed: bool


class MarketDataService:
    """Coordinates incremental provider requests and per-symbol persistence."""

    def __init__(
        self,
        store: MarketDataStore,
        provider: MarketDataProvider,
        *,
        max_workers: int = 8,
        clock: Callable[[], datetime] | None = None,
    ) -> None:
        if max_workers < 1:
            raise ValueError("max_workers must be positive")
        self.store = store
        self.provider = provider
        self.max_workers = max_workers
        self._clock = clock or (lambda: datetime.now(timezone.utc))

    @staticmethod
    def _last_completed_session(
        calendar_name: str, as_of: date, refreshed_at: datetime
    ) -> date:
        """Return the latest session whose daily bar is safe to cache."""

        calendar = xcals.get_calendar(calendar_name)
        session = calendar.date_to_session(pd.Timestamp(as_of), direction="previous")
        if session.date() != as_of:
            return session.date()
        if as_of != refreshed_at.date():
            return session.date()
        if refreshed_at < calendar.session_close(session):
            return calendar.previous_session(session).date()
        return session.date()

    def _metadata_entry(
        self,
        row: pd.Series,
        prices: pd.DataFrame,
        refreshed_at: datetime,
        as_of: date,
        coverage_start: date,
    ) -> dict[str, object]:
        return {
            "symbol": row["Symbol"],
            "provider_symbol": row["ProviderSymbol"],
            "exchange": row["Exchange"],
            "calendar": row["Calendar"],
            "first_date": prices.index.min().date().isoformat(),
            "last_date": prices.index.max().date().isoformat(),
            "last_refreshed_at": refreshed_at.isoformat(),
            "last_request_as_of": as_of.isoformat(),
            "coverage_start": coverage_start.isoformat(),
            "source": self.provider.source_name,
            "cache_file": self.store.cache_file(str(row["Symbol"])),
        }

    def _refresh_symbol(
        self,
        row: pd.Series,
        requested_start: date,
        as_of: date,
        previous_metadata: dict[str, object] | None,
        force: bool,
        refreshed_at: datetime,
    ) -> _SymbolResult:
        symbol = str(row["Symbol"])
        provider_symbol = str(row["ProviderSymbol"])
        existing = self.store.read(symbol)
        available_as_of = self._last_completed_session(
            str(row["Calendar"]), as_of, refreshed_at
        )
        requested_end = available_as_of + timedelta(days=1)
        previous_coverage_start = None
        if previous_metadata and previous_metadata.get("coverage_start"):
            previous_coverage_start = date.fromisoformat(
                str(previous_metadata["coverage_start"])
            )

        if existing is not None and not existing.empty and not force:
            coverage_starts_early_enough = (
                previous_coverage_start is not None
                and previous_coverage_start <= requested_start
            )
            cache_contains_latest_completed_session = (
                existing.index.max().date() >= available_as_of
            )
            if (
                coverage_starts_early_enough
                and cache_contains_latest_completed_session
            ):
                selected = existing.loc[
                    (existing.index.date >= requested_start)
                    & (existing.index.date <= as_of)
                ]
                return _SymbolResult(
                    symbol,
                    selected,
                    CacheEvent(symbol, "cache_hit", len(selected), "read from cache"),
                    previous_metadata,
                    False,
                )

        ranges: list[tuple[date, date]] = []
        if force or existing is None or existing.empty:
            ranges.append((requested_start, requested_end))
        else:
            first_date = existing.index.min().date()
            last_date = existing.index.max().date()
            if (
                first_date > requested_start
                and (
                    previous_coverage_start is None
                    or previous_coverage_start > requested_start
                )
            ):
                ranges.append((requested_start, first_date))
            if last_date < available_as_of:
                ranges.append((last_date + timedelta(days=1), requested_end))

        try:
            additions = [
                self.provider.fetch(provider_symbol, start, end)
                for start, end in ranges
                if start < end
            ]
        except Exception as error:
            if existing is not None and not existing.empty:
                selected = existing.loc[
                    (existing.index.date >= requested_start)
                    & (existing.index.date <= as_of)
                ]
                event = CacheEvent(
                    symbol,
                    "failed_using_cache",
                    len(selected),
                    f"download failed; existing cache retained: {error}",
                )
                return _SymbolResult(symbol, selected, event, previous_metadata, True)
            event = CacheEvent(symbol, "failed", 0, f"download failed: {error}")
            return _SymbolResult(symbol, None, event, None, True)

        non_empty = [_normalise_prices(frame) for frame in additions if not frame.empty]
        if force:
            merged = pd.concat(non_empty).pipe(_normalise_prices) if non_empty else None
        else:
            frames = ([existing] if existing is not None else []) + non_empty
            merged = pd.concat(frames).pipe(_normalise_prices) if frames else None
        if merged is None or merged.empty:
            event = CacheEvent(symbol, "failed", 0, "no market data available")
            return _SymbolResult(symbol, existing, event, previous_metadata, True)

        changed = existing is None or not _frames_equal(existing, merged)
        if changed:
            self.store.write(symbol, merged)
        coverage_start = requested_start
        if not force and previous_coverage_start is not None:
            coverage_start = min(previous_coverage_start, requested_start)
        metadata = self._metadata_entry(
            row, merged, refreshed_at, as_of, coverage_start
        )
        selected = merged.loc[
            (merged.index.date >= requested_start) & (merged.index.date <= as_of)
        ]
        if force:
            status, message = "refreshed", "cache fully refreshed"
        elif existing is None:
            status, message = "downloaded", "full requested history downloaded"
        elif changed:
            status, message = "updated", "missing market dates appended"
        else:
            status, message = "unchanged", "provider returned no new market data"
        return _SymbolResult(
            symbol,
            selected,
            CacheEvent(symbol, status, len(selected), message),
            metadata,
            False,
        )

    def get_market_data(
        self,
        universe: pd.DataFrame,
        history_days: int,
        *,
        as_of: date | None = None,
        force_refresh: bool = False,
        force_symbols: set[str] | None = None,
        progress_callback: ProgressCallback | None = None,
        cancellation_check: CancellationCheck | None = None,
    ) -> MarketDataResult:
        if history_days < 1:
            raise ValueError("history_days must be positive")
        requested = validate_symbol_universe(universe)
        if requested.empty:
            raise ValueError("At least one market symbol must be requested")
        as_of_date = as_of or date.today()
        requested_start = as_of_date - timedelta(days=history_days)
        forced = force_symbols or set()
        unknown_forced = forced - set(requested["Symbol"])
        if unknown_forced:
            raise ValueError(f"Forced symbols are not in the requested universe: {unknown_forced}")

        manifest = self.store.read_metadata()
        symbol_metadata = dict(manifest.get("symbols", {}))
        refreshed_at = self._clock()
        results_by_symbol: dict[str, _SymbolResult] = {}
        report_progress(
            progress_callback,
            "market_data",
            substage="symbols",
            completed_units=0,
            total_units=len(requested),
        )
        with ThreadPoolExecutor(max_workers=min(self.max_workers, len(requested))) as executor:
            futures = {
                executor.submit(
                    self._refresh_symbol,
                    row,
                    requested_start,
                    as_of_date,
                    symbol_metadata.get(str(row["Symbol"])),
                    force_refresh or str(row["Symbol"]) in forced,
                    refreshed_at,
                ): str(row["Symbol"])
                for _, row in requested.iterrows()
            }
            for completed, future in enumerate(as_completed(futures), start=1):
                check_cancellation(cancellation_check)
                symbol = futures[future]
                try:
                    result = future.result()
                except Exception as error:
                    result = _SymbolResult(
                        symbol,
                        None,
                        CacheEvent(symbol, "failed", 0, f"cache operation failed: {error}"),
                        symbol_metadata.get(symbol),
                        True,
                    )
                results_by_symbol[result.symbol] = result
                log = LOGGER.error if result.failed else LOGGER.info
                log("market-cache %s: %s", result.symbol, result.event.message)
                report_progress(
                    progress_callback,
                    "market_data",
                    substage=result.symbol,
                    completed_units=completed,
                    total_units=len(requested),
                    details={"status": result.event.status},
                )

        ordered_results = [results_by_symbol[symbol] for symbol in requested["Symbol"]]
        for result in ordered_results:
            if result.metadata is not None:
                symbol_metadata[result.symbol] = result.metadata
        manifest = {
            "schema_version": CACHE_SCHEMA_VERSION,
            "updated_at": refreshed_at.isoformat(),
            "source": self.provider.source_name,
            "symbols": symbol_metadata,
        }
        self.store.write_metadata(manifest)

        usable = [
            result
            for result in ordered_results
            if result.prices is not None and not result.prices.empty
        ]
        frames = [prefix_symbol_columns(result.prices, result.symbol) for result in usable]
        prices = pd.concat(frames, axis=1).sort_index() if frames else pd.DataFrame()
        prices.index.name = "Date"
        return MarketDataResult(
            prices=prices,
            symbols=[result.symbol for result in usable],
            failed_symbols=[result.symbol for result in ordered_results if result.failed],
            events=[result.event for result in ordered_results],
        )


def market_data_service(
    config: RStockConfig,
    *,
    provider: MarketDataProvider | None = None,
    store: MarketDataStore | None = None,
) -> MarketDataService:
    backend = store or ParquetMarketDataStore(
        config.market_data_path, config.market_cache_metadata_path
    )
    return MarketDataService(
        backend,
        provider or YahooFinanceProvider(),
        max_workers=config.market_cache_workers,
    )
