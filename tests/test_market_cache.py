import json
from datetime import date

import pandas as pd

from rstock.market_cache import MarketDataService, ParquetMarketDataStore


def _universe(*symbols):
    return pd.DataFrame(
        {
            "Symbol": list(symbols),
            "ProviderSymbol": list(symbols),
            "Exchange": ["NASDAQ"] * len(symbols),
            "Calendar": ["XNYS"] * len(symbols),
        }
    )


def _prices(dates, base=10.0):
    index = pd.to_datetime(dates)
    return pd.DataFrame(
        {
            "Open": [base + offset for offset in range(len(index))],
            "High": [base + offset + 1 for offset in range(len(index))],
            "Low": [base + offset - 1 for offset in range(len(index))],
            "Close": [base + offset + 0.5 for offset in range(len(index))],
            "Volume": [1000 + offset for offset in range(len(index))],
            "Adjusted": [base + offset + 0.4 for offset in range(len(index))],
        },
        index=index,
    )


class FakeProvider:
    source_name = "Yahoo Finance"

    def __init__(self, frames, failures=()):
        self.frames = frames
        self.failures = set(failures)
        self.calls = []

    def fetch(self, provider_symbol, start, end_exclusive):
        self.calls.append((provider_symbol, start, end_exclusive))
        if provider_symbol in self.failures:
            raise RuntimeError("provider unavailable")
        frame = self.frames[provider_symbol]
        return frame.loc[
            (frame.index.date >= start) & (frame.index.date < end_exclusive)
        ].copy()


def _service(tmp_path, provider):
    store = ParquetMarketDataStore(
        tmp_path / "data" / "market",
        tmp_path / "data" / "metadata" / "market_cache.json",
    )
    return MarketDataService(store, provider, max_workers=1), store


def test_first_request_writes_one_typed_sorted_parquet_and_useful_metadata(tmp_path):
    frame = _prices(["2024-01-05", "2024-01-03", "2024-01-04"])
    provider = FakeProvider({"AAPL": frame})
    service, store = _service(tmp_path, provider)

    result = service.get_market_data(
        _universe("AAPL"), 10, as_of=date(2024, 1, 10)
    )

    cached = store.read("AAPL")
    assert result.events[0].status == "downloaded"
    assert cached.index.tolist() == list(
        pd.to_datetime(["2024-01-03", "2024-01-04", "2024-01-05"])
    )
    assert isinstance(cached.index, pd.DatetimeIndex)
    assert cached.columns.tolist() == [
        "Open", "High", "Low", "Close", "Volume", "Adjusted"
    ]
    metadata = json.loads(store.metadata_path.read_text(encoding="utf-8"))
    entry = metadata["symbols"]["AAPL"]
    assert provider.calls == [("AAPL", date(2023, 12, 31), date(2024, 1, 11))]
    assert entry["symbol"] == "AAPL"
    assert entry["provider_symbol"] == "AAPL"
    assert entry["exchange"] == "NASDAQ"
    assert entry["calendar"] == "XNYS"
    assert entry["first_date"] == "2024-01-03"
    assert entry["last_date"] == "2024-01-05"
    assert entry["source"] == "Yahoo Finance"
    assert "Open" not in entry and "Close" not in entry


def test_same_request_reads_cache_without_network_or_parquet_rewrite(tmp_path):
    provider = FakeProvider({"AAPL": _prices(["2024-01-03", "2024-01-04"])})
    service, store = _service(tmp_path, provider)
    universe = _universe("AAPL")
    service.get_market_data(universe, 10, as_of=date(2024, 1, 10))
    path = store.path_for("AAPL")
    modified = path.stat().st_mtime_ns

    result = service.get_market_data(universe, 10, as_of=date(2024, 1, 10))

    assert len(provider.calls) == 1
    assert result.events[0].status == "cache_hit"
    assert path.stat().st_mtime_ns == modified


def test_incremental_request_fetches_after_last_date_and_merges_without_duplicates(tmp_path):
    provider = FakeProvider({"AAPL": _prices(["2024-01-03", "2024-01-04"])})
    service, store = _service(tmp_path, provider)
    universe = _universe("AAPL")
    service.get_market_data(universe, 10, as_of=date(2024, 1, 4))
    provider.frames["AAPL"] = _prices(
        ["2024-01-03", "2024-01-04", "2024-01-05", "2024-01-08"]
    )

    result = service.get_market_data(universe, 10, as_of=date(2024, 1, 8))

    assert provider.calls[-1] == ("AAPL", date(2024, 1, 5), date(2024, 1, 9))
    assert result.events[0].status == "updated"
    cached = store.read("AAPL")
    assert cached.index.is_unique
    assert cached.index.max() == pd.Timestamp("2024-01-08")
    assert len(cached) == 4


def test_longer_requested_history_backfills_before_cached_first_date(tmp_path):
    provider = FakeProvider(
        {"AAPL": _prices(["2024-01-02", "2024-01-03", "2024-01-08"])}
    )
    service, store = _service(tmp_path, provider)
    universe = _universe("AAPL")
    service.get_market_data(universe, 2, as_of=date(2024, 1, 10))

    result = service.get_market_data(universe, 10, as_of=date(2024, 1, 10))

    assert provider.calls[-2] == ("AAPL", date(2023, 12, 31), date(2024, 1, 8))
    assert provider.calls[-1] == ("AAPL", date(2024, 1, 9), date(2024, 1, 11))
    assert result.events[0].status == "updated"
    assert store.read("AAPL").index.min() == pd.Timestamp("2024-01-02")


def test_empty_incremental_response_does_not_rewrite_parquet(tmp_path):
    provider = FakeProvider({"AAPL": _prices(["2024-01-03", "2024-01-04"])})
    service, store = _service(tmp_path, provider)
    universe = _universe("AAPL")
    service.get_market_data(universe, 10, as_of=date(2024, 1, 4))
    path = store.path_for("AAPL")
    modified = path.stat().st_mtime_ns

    result = service.get_market_data(universe, 10, as_of=date(2024, 1, 5))

    assert result.events[0].status == "unchanged"
    assert path.stat().st_mtime_ns == modified


def test_forced_symbol_refresh_downloads_full_requested_period(tmp_path):
    provider = FakeProvider({"AAPL": _prices(["2024-01-03", "2024-01-04"])})
    service, _ = _service(tmp_path, provider)
    universe = _universe("AAPL")
    service.get_market_data(universe, 10, as_of=date(2024, 1, 4))

    result = service.get_market_data(
        universe,
        10,
        as_of=date(2024, 1, 4),
        force_symbols={"AAPL"},
    )

    assert len(provider.calls) == 2
    assert provider.calls[-1][1:] == (date(2023, 12, 25), date(2024, 1, 5))
    assert result.events[0].status == "refreshed"


def test_symbol_failure_is_isolated_and_cached_data_survives_failed_update(tmp_path):
    provider = FakeProvider(
        {"AAPL": _prices(["2024-01-03"]), "BAD": pd.DataFrame()},
        failures={"BAD"},
    )
    service, store = _service(tmp_path, provider)

    first = service.get_market_data(
        _universe("AAPL", "BAD"), 10, as_of=date(2024, 1, 3)
    )
    assert first.symbols == ["AAPL"]
    assert first.failed_symbols == ["BAD"]

    provider.failures.add("AAPL")
    second = service.get_market_data(
        _universe("AAPL"), 10, as_of=date(2024, 1, 4)
    )

    assert second.symbols == ["AAPL"]
    assert second.failed_symbols == ["AAPL"]
    assert second.events[0].status == "failed_using_cache"
    assert store.read("AAPL") is not None


def test_cache_filename_cannot_create_nested_paths(tmp_path):
    provider = FakeProvider({"A/B": _prices(["2024-01-03"])})
    service, store = _service(tmp_path, provider)
    service.get_market_data(_universe("A/B"), 2, as_of=date(2024, 1, 3))

    path = store.path_for("A/B")
    assert path.parent == store.market_directory
    assert path.name == "A%2FB.parquet"


def test_corrupt_symbol_cache_does_not_abort_other_symbols(tmp_path):
    provider = FakeProvider(
        {"AAPL": _prices(["2024-01-03"]), "BAD": _prices(["2024-01-03"])}
    )
    service, store = _service(tmp_path, provider)
    store.market_directory.mkdir(parents=True)
    store.path_for("BAD").write_text("not parquet", encoding="utf-8")

    result = service.get_market_data(
        _universe("AAPL", "BAD"), 2, as_of=date(2024, 1, 3)
    )

    assert result.symbols == ["AAPL"]
    assert result.failed_symbols == ["BAD"]
    assert result.events[1].status == "failed"
