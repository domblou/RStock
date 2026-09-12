from datetime import date

import pandas as pd

from rstock.data import download_market_data


def test_download_isolates_symbol_failures_and_flattens_provider_fields():
    calls = []

    def provider(symbol, **kwargs):
        calls.append((symbol, kwargs))
        if symbol == "BAD":
            raise RuntimeError("not found")
        return pd.DataFrame(
            {"Open": [10.0], "Close": [11.0], "Adj Close": [10.5]},
            index=pd.to_datetime(["2024-01-09"]),
        )

    result = download_market_data(
        ["AAA", "BAD"], 10, downloader=provider, today=date(2024, 1, 10)
    )

    assert result.symbols == ["AAA"]
    assert result.failed_symbols == ["BAD"]
    assert result.prices.columns.tolist() == ["AAA.Open", "AAA.Close", "AAA.Adjusted"]
    assert calls[0][1]["start"] == "2023-12-31"


def test_provider_symbol_can_differ_from_canonical_punctuated_symbol():
    def provider(symbol, **kwargs):
        assert symbol == "BRK-B"
        return pd.DataFrame(
            {"Open": [100.0], "Close": [101.0]},
            index=pd.to_datetime(["2024-01-09"]),
        )

    result = download_market_data(
        ["BRK.B"],
        10,
        downloader=provider,
        today=date(2024, 1, 10),
        provider_symbols={"BRK.B": "BRK-B"},
    )

    assert result.prices.columns.tolist() == ["BRK.B.Open", "BRK.B.Close"]
