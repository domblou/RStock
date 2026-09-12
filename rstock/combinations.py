"""Generation of target/feature symbol combinations."""

from __future__ import annotations

from itertools import combinations, permutations

import pandas as pd


def generate_symbol_sets(symbols: list[str], permutation_depth: int = 1) -> pd.DataFrame:
    """Reproduce ``RStock.GenerateSets`` with explicit Python combinatorics.

    Despite the legacy name, only pairs are ordered permutations. Larger sets are
    one observation symbol followed by an unordered combination of other symbols.
    Rows are padded to the maximum width so their legacy model names remain stable.
    """

    symbols = list(symbols)
    if permutation_depth >= len(symbols):
        raise ValueError("permutation_depth must be lower than the number of symbols")
    if permutation_depth < 1:
        raise ValueError("permutation_depth must be at least 1")

    width = permutation_depth + 1
    rows: list[list[str | None]] = []

    for pair in permutations(symbols, 2):
        rows.append([*pair, *([None] * (width - 2))])

    for feature_count in range(2, permutation_depth + 1):
        for observation in symbols:
            candidates = [symbol for symbol in symbols if symbol != observation]
            for features in combinations(candidates, feature_count):
                rows.append(
                    [observation, *features, *([None] * (width - feature_count - 1))]
                )

    return pd.DataFrame(rows, columns=[f"V{i}" for i in range(width)])


def legacy_set_name(row: pd.Series, symbol_columns: list[str] | None = None) -> str:
    """Build the dash-delimited identifier used by the R implementation."""

    columns = symbol_columns or [name for name in row.index if name.startswith("V")]
    values = ["NA" if pd.isna(row[name]) else str(row[name]) for name in columns]
    return "-".join(values)


def symbols_from_set(row: pd.Series) -> tuple[str, list[str]]:
    """Return the observation and non-empty feature symbols from a generated row."""

    symbol_columns = sorted(
        (name for name in row.index if name.startswith("V")),
        key=lambda name: int(name[1:]),
    )
    observation = str(row[symbol_columns[0]])
    features = [
        str(row[name])
        for name in symbol_columns[1:]
        if not pd.isna(row[name]) and str(row[name]) != observation
    ]
    return observation, features

