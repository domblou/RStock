import pandas as pd

from rstock.symbols import parse_symbol_directories


def test_parse_symbol_directories_supports_both_listing_layouts_and_deduplicates():
    nasdaq = pd.DataFrame({"Symbol": ["AAA", "BBB", "File Creation Time: 1"]})
    other = pd.DataFrame({"NASDAQ Symbol": ["BBB", "CCC"]})

    assert parse_symbol_directories([nasdaq, other]) == ["AAA", "BBB", "CCC"]

