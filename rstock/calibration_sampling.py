"""Versioned deterministic sampling policies shared by calibration workflows."""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from math import floor
from typing import Sequence

import pandas as pd

from .combinations import symbol_set_id, symbols_from_set


PER_TARGET_V1 = "per_target_v1"
GLOBAL_STRATIFIED_V2 = "global_stratified_v2"


@dataclass(frozen=True, slots=True)
class CalibrationSample:
    combinations: pd.DataFrame
    manifest: dict[str, object]


def policy_name(version: int, *, qualified_walk_forward_source: bool) -> str:
    """Resolve policy from persisted version and scientific provenance."""

    if qualified_walk_forward_source and version >= 2:
        return GLOBAL_STRATIFIED_V2
    return PER_TARGET_V1


def _digest(*values: object) -> str:
    return hashlib.sha256("".join(str(value) for value in values).encode("utf-8")).hexdigest()


def _sample_digest(set_ids: Sequence[str]) -> str:
    canonical = json.dumps(sorted(set_ids), ensure_ascii=False, separators=(",", ":"))
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def _rows_by_unique_set(
    generated_sets: pd.DataFrame,
) -> dict[str, tuple[str, pd.Series]]:
    rows: dict[str, tuple[str, pd.Series]] = {}
    for _, row in generated_sets.iterrows():
        set_id = symbol_set_id(row)
        target, _ = symbols_from_set(row)
        rows.setdefault(set_id, (target, row.copy()))
    if not rows:
        raise ValueError("No combinations are available for calibration")
    return rows


def _manifest(
    *,
    policy: str,
    population_ids: Sequence[str],
    selected_ids: Sequence[str],
    cap: int | None,
    seed: int,
    target_distribution: dict[str, dict[str, int]],
    directional_model_cap: int | None = None,
) -> dict[str, object]:
    values: dict[str, object] = {
        "schema_version": 1,
        "policy_version": policy,
        "population_size": len(population_ids),
        "sample_size": len(selected_ids),
        "cap": cap,
        "seed": seed,
        "target_distribution": target_distribution,
        "sample_sha256": _sample_digest(selected_ids),
        "selected_set_ids": list(selected_ids),
    }
    if directional_model_cap is not None:
        values.update(
            directional_model_cap=directional_model_cap,
            population_directional_models=2 * len(population_ids),
            sampled_directional_models=2 * len(selected_ids),
            complete_direction_pairs=True,
        )
    return values


def per_target_v1_sample(
    generated_sets: pd.DataFrame, *, per_target: int, seed: int
) -> CalibrationSample:
    """Historical equal-quota behavior, preserved byte-for-byte in ordering."""

    if per_target < 1:
        raise ValueError("per_target must be positive")
    ranked: list[tuple[str, str, str, pd.Series]] = []
    population_counts: dict[str, int] = {}
    for _, row in generated_sets.iterrows():
        target, _ = symbols_from_set(row)
        set_id = symbol_set_id(row)
        digest = hashlib.sha256(f"{seed}:{set_id}".encode()).hexdigest()
        ranked.append((target, digest, set_id, row))
        population_counts[target] = population_counts.get(target, 0) + 1
    by_target: dict[str, list[tuple[str, str, pd.Series]]] = {}
    for target, digest, set_id, row in ranked:
        by_target.setdefault(target, []).append((digest, set_id, row))
    selected: list[pd.Series] = []
    selected_ids: list[str] = []
    distribution: dict[str, dict[str, int]] = {}
    for target in sorted(by_target):
        choices = sorted(by_target[target], key=lambda item: (item[0], item[1]))
        picked = choices[:per_target]
        selected.extend(item[2] for item in picked)
        selected_ids.extend(item[1] for item in picked)
        distribution[target] = {
            "population": population_counts[target],
            "allocated": len(picked),
        }
    if not selected:
        raise ValueError("No combinations are available for calibration")
    frame = pd.DataFrame(selected).reset_index(drop=True)
    population_ids = [symbol_set_id(row) for _, row in generated_sets.iterrows()]
    return CalibrationSample(
        frame,
        _manifest(
            policy=PER_TARGET_V1,
            population_ids=population_ids,
            selected_ids=selected_ids,
            cap=None,
            seed=seed,
            target_distribution=distribution,
        ),
    )


def global_stratified_v2_sample(
    generated_sets: pd.DataFrame,
    *,
    cap: int | None,
    seed: int,
    directional_model_cap: int | None = None,
) -> CalibrationSample:
    """Allocate a global cap by largest remainders, then hash-rank each target."""

    if cap is not None and cap < 1:
        raise ValueError("global sampling cap must be positive or null")
    unique = _rows_by_unique_set(generated_sets)
    by_target: dict[str, list[tuple[str, pd.Series]]] = {}
    for set_id, (target, row) in unique.items():
        by_target.setdefault(target, []).append((set_id, row))
    population_size = len(unique)
    sample_size = population_size if cap is None else min(cap, population_size)
    allocations: dict[str, int] = {}
    remainders: list[tuple[float, str, str]] = []
    for target, rows in by_target.items():
        exact = sample_size * len(rows) / population_size
        allocations[target] = floor(exact)
        remainders.append(
            (exact - floor(exact), _digest(seed, GLOBAL_STRATIFIED_V2, target), target)
        )
    remaining = sample_size - sum(allocations.values())
    for _, _, target in sorted(remainders, key=lambda item: (-item[0], item[1]))[
        :remaining
    ]:
        allocations[target] += 1

    selected_rows: list[pd.Series] = []
    selected_ids: list[str] = []
    distribution: dict[str, dict[str, int]] = {}
    for target in sorted(by_target):
        ranked = sorted(
            by_target[target],
            key=lambda item: (
                _digest(seed, GLOBAL_STRATIFIED_V2, item[0]),
                item[0],
            ),
        )
        picked = ranked[: allocations[target]]
        selected_ids.extend(item[0] for item in picked)
        selected_rows.extend(item[1] for item in picked)
        distribution[target] = {
            "population": len(ranked),
            "allocated": len(picked),
        }
    if not selected_rows:
        raise ValueError("No combinations are available for calibration")
    return CalibrationSample(
        pd.DataFrame(selected_rows).reset_index(drop=True),
        _manifest(
            policy=GLOBAL_STRATIFIED_V2,
            population_ids=list(unique),
            selected_ids=selected_ids,
            cap=cap,
            seed=seed,
            target_distribution=distribution,
            directional_model_cap=directional_model_cap,
        ),
    )


def exhaustive_v2_sample(
    generated_sets: pd.DataFrame, *, seed: int
) -> CalibrationSample:
    """Use the complete deduplicated WF-qualified population in stable order."""

    return global_stratified_v2_sample(generated_sets, cap=None, seed=seed)
