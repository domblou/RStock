"""Stock-symbol acquisition and CSV persistence."""

from __future__ import annotations

from collections.abc import Iterable
from io import StringIO
from pathlib import Path
from urllib.request import Request, urlopen

import pandas as pd

from .calendars import US_EQUITIES_CALENDAR, validate_calendar_name


NASDAQ_LISTED_URL = "https://www.nasdaqtrader.com/dynamic/SymDir/nasdaqlisted.txt"
OTHER_LISTED_URL = "https://www.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt"


UNIVERSE_COLUMNS = ["Symbol", "ProviderSymbol", "Exchange", "Calendar"]
OTHER_EXCHANGES = {
    "A": "NYSE American",
    "N": "NYSE",
    "P": "NYSE Arca",
    "V": "IEX",
    "Z": "Cboe BZX",
}


def parse_symbol_directories(directories: Iterable[pd.DataFrame]) -> pd.DataFrame:
    """Extract the US-listed universe with explicit exchange/calendar metadata."""

    result: list[dict[str, str]] = []
    seen: set[str] = set()
    candidate_columns = ("Symbol", "NASDAQ Symbol", "ACT Symbol")
    for directory in directories:
        column = next((name for name in candidate_columns if name in directory), None)
        if column is None:
            raise ValueError("Symbol directory has no recognised symbol column")
        for row_index, value in directory[column].dropna().astype(str).items():
            symbol = value.strip()
            if not symbol or symbol.startswith("File Creation Time") or symbol in seen:
                continue
            seen.add(symbol)
            if column == "Symbol":
                exchange = "NASDAQ"
            else:
                exchange_code = str(directory.loc[row_index, "Exchange"]).strip()
                exchange = OTHER_EXCHANGES.get(exchange_code, f"US exchange {exchange_code}")
            result.append(
                {
                    "Symbol": symbol,
                    "ProviderSymbol": symbol.replace(".", "-"),
                    "Exchange": exchange,
                    "Calendar": US_EQUITIES_CALENDAR,
                }
            )
    return pd.DataFrame(result, columns=UNIVERSE_COLUMNS)


def _download_directory(url: str) -> pd.DataFrame:
    request = Request(url, headers={"User-Agent": "RStock/0.1"})
    with urlopen(request, timeout=30) as response:
        body = response.read().decode("utf-8")
    return pd.read_csv(StringIO(body), sep="|")


def fetch_stock_universe(
    urls: Iterable[str] = (NASDAQ_LISTED_URL, OTHER_LISTED_URL),
) -> pd.DataFrame:
    """Fetch the US-listed universe provided by Nasdaq Trader."""

    return parse_symbol_directories(_download_directory(url) for url in urls)


def write_symbol_universe(universe: pd.DataFrame, destination: Path) -> None:
    validated = validate_symbol_universe(universe)

    destination.parent.mkdir(parents=True, exist_ok=True)
    validated.to_csv(destination, index=False)


def read_symbol_universe(source: Path) -> pd.DataFrame:
    frame = pd.read_csv(source, dtype=str)
    return validate_symbol_universe(frame)


def validate_symbol_universe(universe: pd.DataFrame) -> pd.DataFrame:
    missing = [column for column in UNIVERSE_COLUMNS if column not in universe]
    if missing:
        raise ValueError(
            "Symbol universe must declare Symbol, ProviderSymbol, Exchange and Calendar; missing: "
            + ", ".join(missing)
        )
    result = universe[UNIVERSE_COLUMNS].copy()
    if result.isna().any().any() or (result == "").any().any():
        raise ValueError("Symbol universe cannot contain blank metadata")
    if result["Symbol"].duplicated().any():
        raise ValueError("Symbol universe contains duplicate symbols")
    for calendar_name in result["Calendar"].unique():
        validate_calendar_name(str(calendar_name))
    return result.reset_index(drop=True)
