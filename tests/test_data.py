from datetime import date

import pandas as pd

from rstock.data import YahooFinanceProvider, prefix_symbol_columns


def test_yahoo_provider_requests_exact_incremental_range_and_normalises_fields():
    calls = []

    def downloader(symbol, **kwargs):
        calls.append((symbol, kwargs))
        return pd.DataFrame(
            {"Open": [10.0], "Close": [11.0], "Adj Close": [10.5]},
            index=pd.to_datetime(["2024-01-09"]),
        )

    prices = YahooFinanceProvider(downloader).fetch(
        "AAPL", date(2024, 1, 1), date(2024, 1, 10)
    )

    assert calls[0][0] == "AAPL"
    assert calls[0][1]["start"] == "2024-01-01"
    assert calls[0][1]["end"] == "2024-01-10"
    assert calls[0][1]["threads"] is False
    assert prices.columns.tolist() == ["Open", "Close", "Adjusted"]
    assert isinstance(prices.index, pd.DatetimeIndex)


def test_internal_punctuated_symbol_is_independent_from_yahoo_symbol():
    prices = pd.DataFrame(
        {"Open": [100.0], "Close": [101.0]},
        index=pd.to_datetime(["2024-01-09"]),
    )

    canonical = prefix_symbol_columns(prices, "BRK.B")

    assert canonical.columns.tolist() == ["BRK.B.Open", "BRK.B.Close"]
