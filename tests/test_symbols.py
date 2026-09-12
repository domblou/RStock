import pandas as pd
import pytest

from rstock.symbols import parse_symbol_directories, validate_symbol_universe


def test_directories_identify_exchanges_and_explicit_us_calendar():
    nasdaq = pd.DataFrame({"Symbol": ["AAA", "BRK.B", "File Creation Time: 1"]})
    other = pd.DataFrame(
        {"NASDAQ Symbol": ["BRK.B", "CCC-P"], "Exchange": ["N", "P"]}
    )

    universe = parse_symbol_directories([nasdaq, other])

    assert universe.to_dict("records") == [
        {
            "Symbol": "AAA", "ProviderSymbol": "AAA", "Exchange": "NASDAQ",
            "Calendar": "XNYS",
        },
        {
            "Symbol": "BRK.B", "ProviderSymbol": "BRK-B", "Exchange": "NASDAQ",
            "Calendar": "XNYS",
        },
        {
            "Symbol": "CCC-P", "ProviderSymbol": "CCC-P", "Exchange": "NYSE Arca",
            "Calendar": "XNYS",
        },
    ]


def test_custom_universe_must_declare_market_metadata():
    with pytest.raises(ValueError, match="missing"):
        validate_symbol_universe(pd.DataFrame({"Symbol": ["AAPL"]}))
