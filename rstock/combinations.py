"""Generation of target/feature symbol combinations."""

from __future__ import annotations

import json
from itertools import combinations, permutations
from math import comb

import pandas as pd


def count_symbol_sets(symbol_count: int, permutation_depth: int) -> int:
    if symbol_count < 0 or permutation_depth < 1 or permutation_depth >= symbol_count:
        raise ValueError("permutation_depth must be between 1 and symbol_count - 1")
    return symbol_count * sum(
        comb(symbol_count - 1, feature_count)
        for feature_count in range(1, permutation_depth + 1)
    )


def generate_symbol_sets(
    symbols: list[str],
    permutation_depth: int = 1,
    *,
    max_sets: int = 100_000,
) -> pd.DataFrame:
    """Generate target/feature sets after checking their combinatorial size."""

    symbols = list(symbols)
    if permutation_depth >= len(symbols):
        raise ValueError("permutation_depth must be lower than the number of symbols")
    if permutation_depth < 1:
        raise ValueError("permutation_depth must be at least 1")
    if len(set(symbols)) != len(symbols):
        raise ValueError("symbols must be unique")
    expected = count_symbol_sets(len(symbols), permutation_depth)
    if expected > max_sets:
        raise ValueError(
            f"Generating {expected:,} symbol sets exceeds max_generated_sets={max_sets:,}"
        )

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


def symbol_set_id(row: pd.Series, symbol_columns: list[str] | None = None) -> str:
    """Build an unambiguous JSON identifier that supports punctuated tickers."""

    columns = symbol_columns or [name for name in row.index if name.startswith("V")]
    values = [str(row[name]) for name in columns if not pd.isna(row[name])]
    return json.dumps(values, ensure_ascii=False, separators=(",", ":"))


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
