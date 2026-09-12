"""Stock-symbol acquisition and CSV persistence."""

from __future__ import annotations

from collections.abc import Iterable
from io import StringIO
from pathlib import Path
from urllib.request import Request, urlopen

import pandas as pd


NASDAQ_LISTED_URL = "https://www.nasdaqtrader.com/dynamic/SymDir/nasdaqlisted.txt"
OTHER_LISTED_URL = "https://www.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt"


def parse_symbol_directories(directories: Iterable[pd.DataFrame]) -> list[str]:
    """Extract a stable, unique symbol list from Nasdaq Trader directories."""

    result: list[str] = []
    seen: set[str] = set()
    candidate_columns = ("Symbol", "NASDAQ Symbol", "ACT Symbol")
    for directory in directories:
        column = next((name for name in candidate_columns if name in directory), None)
        if column is None:
            raise ValueError("Symbol directory has no recognised symbol column")
        for value in directory[column].dropna().astype(str):
            symbol = value.strip()
            if not symbol or symbol.startswith("File Creation Time") or symbol in seen:
                continue
            seen.add(symbol)
            result.append(symbol)
    return result


def _download_directory(url: str) -> pd.DataFrame:
    request = Request(url, headers={"User-Agent": "RStock/0.1"})
    with urlopen(request, timeout=30) as response:
        body = response.read().decode("utf-8")
    return pd.read_csv(StringIO(body), sep="|")


def fetch_stock_symbols(urls: Iterable[str] = (NASDAQ_LISTED_URL, OTHER_LISTED_URL)) -> list[str]:
    """Fetch the broad US listing used as the Python replacement for stockSymbols()."""

    return parse_symbol_directories(_download_directory(url) for url in urls)


def write_symbols(symbols: Iterable[str], destination: Path) -> None:
    """Write the legacy one-column CSV (whose column name is ``x``)."""

    destination.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame({"x": list(symbols)}).to_csv(destination, index=False)


def read_symbols(source: Path) -> list[str]:
    frame = pd.read_csv(source, dtype=str)
    if frame.shape[1] != 1:
        raise ValueError(f"Expected one symbol column in {source}")
    return frame.iloc[:, 0].dropna().astype(str).tolist()

