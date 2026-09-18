"""Generation of target/feature symbol combinations."""

from __future__ import annotations

import json
from itertools import combinations
from math import comb

import pandas as pd

def count_target_symbol_sets(predictor_count: int, permutation_depth: int) -> int:
    """Count one target's unordered feature sets without materialising them."""

    if predictor_count < 0:
        raise ValueError("predictor_count must be non-negative")
    if permutation_depth < 1:
        raise ValueError("permutation_depth must be at least 1")
    return sum(
        comb(predictor_count, feature_count)
        for feature_count in range(1, min(permutation_depth, predictor_count) + 1)
    )


def count_symbol_sets(symbol_count: int, permutation_depth: int) -> int:
    if symbol_count < 0 or permutation_depth < 1 or permutation_depth >= symbol_count:
        raise ValueError("permutation_depth must be between 1 and symbol_count - 1")
    return symbol_count * count_target_symbol_sets(
        symbol_count - 1, permutation_depth
    )


def generate_symbol_sets(
    symbols: list[str],
    permutation_depth: int = 1,
    *,
    target_symbols: list[str] | None = None,
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
    targets = list(symbols if target_symbols is None else target_symbols)
    if not targets:
        raise ValueError("target_symbols must not be empty")
    if len(set(targets)) != len(targets):
        raise ValueError("target_symbols must be unique")
    if not set(targets) <= set(symbols):
        raise ValueError("target_symbols must be included in symbols")
    expected = len(targets) * count_target_symbol_sets(
        len(symbols) - 1, permutation_depth
    )
    if expected > max_sets:
        raise ValueError(
            f"Generating {expected:,} symbol sets exceeds max_generated_sets={max_sets:,}"
        )

    width = permutation_depth + 1
    rows: list[list[str | None]] = []

    for observation in targets:
        for feature in symbols:
            if feature != observation:
                rows.append([observation, feature, *([None] * (width - 2))])

    for feature_count in range(2, permutation_depth + 1):
        for observation in targets:
            candidates = [symbol for symbol in symbols if symbol != observation]
            for features in combinations(candidates, feature_count):
                rows.append(
                    [observation, *features, *([None] * (width - feature_count - 1))]
                )

    return pd.DataFrame(rows, columns=[f"V{i}" for i in range(width)])


def generate_target_symbol_sets(
    predictors_by_target: dict[str, tuple[str, ...]],
    permutation_depth: int,
    *,
    max_sets: int = 100_000,
) -> pd.DataFrame:
    """Generate sets from an independently filtered predictor pool per target."""

    if permutation_depth < 1:
        raise ValueError("permutation_depth must be at least 1")
    expected = sum(
        count_target_symbol_sets(len(predictors), permutation_depth)
        for predictors in predictors_by_target.values()
    )
    if expected > max_sets:
        raise ValueError(
            f"Generating {expected:,} symbol sets exceeds max_generated_sets={max_sets:,}"
        )
    width = permutation_depth + 1
    rows: list[list[str | None]] = []
    for depth in range(1, permutation_depth + 1):
        for target, predictors in predictors_by_target.items():
            unique = tuple(dict.fromkeys(predictors))
            if target in unique:
                raise ValueError("A target cannot also be one of its predictors")
            for selected in combinations(unique, depth):
                rows.append([
                    target,
                    *selected,
                    *([None] * (width - depth - 1)),
                ])
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
