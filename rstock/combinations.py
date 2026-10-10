"""Generation of target/feature symbol combinations."""

from __future__ import annotations

import json
from collections.abc import Sequence
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
    predictive_model_type: str = "external_only",
) -> pd.DataFrame:
    """Generate target/feature sets after checking their combinatorial size."""

    symbols = list(symbols)
    if predictive_model_type in {"constant_probability", "target_only"}:
        targets = list(symbols if target_symbols is None else target_symbols)
        if not targets or len(set(targets)) != len(targets) or not set(targets) <= set(symbols):
            raise ValueError("Invalid target_symbols")
        if len(targets) > max_sets:
            raise ValueError("Generating targets exceeds max_generated_sets")
        return pd.DataFrame({"V0": targets})
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
    """Build the ordered execution identifier used by existing artifacts."""

    columns = symbol_columns or [name for name in row.index if name.startswith("V")]
    values = [str(row[name]) for name in columns if not pd.isna(row[name])]
    return json.dumps(values, ensure_ascii=False, separators=(",", ":"))


def canonical_combination_id(
    target: str, direction: str, predictors: Sequence[str]
) -> str:
    """Identify a scientific combination without changing its feature order.

    This key is for cross-run matching. ``symbol_set_id`` remains the ordered
    execution key for models, checkpoints, thresholds and historical artifacts.
    """

    if target is None or direction is None or any(pd.isna(value) for value in predictors):
        raise ValueError("A combination needs complete target, direction and predictors")
    target = str(target)
    direction = str(direction)
    ordered = tuple(str(value) for value in predictors)
    if not target or not direction or not ordered:
        raise ValueError("A combination needs a target, direction and predictors")
    if any(not value for value in ordered) or target in ordered or len(set(ordered)) != len(ordered):
        raise ValueError("A combination needs distinct, non-empty predictor symbols")
    return json.dumps(
        [target, direction, *sorted(ordered)],
        ensure_ascii=False,
        separators=(",", ":"),
    )


def canonical_combination_id_from_set(
    set_id: object, direction: object, *, target: object = None,
    predictors: object = None,
) -> str | None:
    """Resolve an ordered historical Set at read time, without rewriting it.

    Explicit target/predictors are accepted only when they are structured. An
    incomplete or ambiguous legacy identity has no canonical key.
    """

    symbols: object = None
    if isinstance(set_id, str):
        try:
            symbols = json.loads(set_id)
        except json.JSONDecodeError:
            if "<-" in set_id:
                left, right = set_id.split("<-", 1)
                symbols = [left.strip(), *(part.strip() for part in right.split("+"))]
    if isinstance(symbols, list) and len(symbols) >= 2:
        if target is not None and str(target) != str(symbols[0]):
            return None
        target, predictors = symbols[0], symbols[1:]
    elif isinstance(predictors, str):
        try:
            predictors = json.loads(predictors)
        except json.JSONDecodeError:
            predictors = [part.strip() for part in predictors.split("+")]
    if not isinstance(predictors, (list, tuple)):
        return None
    try:
        return canonical_combination_id(str(target or ""), str(direction or ""), predictors)
    except ValueError:
        return None


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


def predictive_model_identity(set_id: str, model_type: str, direction: str) -> str:
    symbols = json.loads(set_id)
    if not isinstance(symbols, list) or not symbols or direction not in {"Up", "Down"}:
        raise ValueError("Invalid predictive model identity")
    return json.dumps({"version": 1, "type": model_type, "target": symbols[0],
                       "externals": sorted(symbols[1:]), "direction": direction},
                      ensure_ascii=False, sort_keys=True, separators=(",", ":"))
