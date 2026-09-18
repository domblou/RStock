"""Lazy, deterministic planning of target/predictor combinations."""

from __future__ import annotations

import hashlib
import json
from bisect import bisect_right
from dataclasses import dataclass
from math import comb
from typing import Iterator, Mapping, Sequence

import pandas as pd

from .combinations import count_target_symbol_sets


COMBINATION_PLAN_VERSION = 2
RAW_POPULATION = "raw"
PREFILTERED_POPULATION = "prefiltered"
_ORDERING_POLICY = "depth_target_lexicographic_v1"


@dataclass(frozen=True, slots=True)
class _Segment:
    target: str
    predictors: tuple[str, ...]
    feature_count: int
    start: int
    stop: int


class CombinationPlan:
    """An indexable combination population that never materializes all rows.

    The canonical order intentionally matches ``generate_symbol_sets``:
    feature depth first, target second, then lexicographic combinations in the
    original predictor order.
    """

    __slots__ = (
        "permutation_depth",
        "population_kind",
        "predictor_symbols",
        "target_symbols",
        "_predictors_by_target",
        "_segments",
        "_segment_stops",
        "_count",
        "_plan_sha256",
    )

    def __init__(
        self,
        predictor_symbols: Sequence[str],
        permutation_depth: int,
        *,
        target_symbols: Sequence[str] | None = None,
    ) -> None:
        predictors = _unique_symbols(predictor_symbols, "predictor_symbols")
        targets = _unique_symbols(
            predictors if target_symbols is None else target_symbols,
            "target_symbols",
        )
        if not targets:
            raise ValueError("target_symbols must not be empty")
        if not set(targets) <= set(predictors):
            raise ValueError("target_symbols must be included in predictor_symbols")
        if permutation_depth < 1 or permutation_depth >= len(predictors):
            raise ValueError(
                "permutation_depth must be between 1 and predictor count - 1"
            )
        candidates = tuple(
            (target, tuple(symbol for symbol in predictors if symbol != target))
            for target in targets
        )
        self._initialize(
            predictor_symbols=predictors,
            target_symbols=targets,
            predictors_by_target=candidates,
            permutation_depth=permutation_depth,
            population_kind=RAW_POPULATION,
        )

    @classmethod
    def from_target_predictors(
        cls,
        predictors_by_target: Mapping[str, Sequence[str]],
        permutation_depth: int,
    ) -> "CombinationPlan":
        """Build the effective post-prefilter population in historical order."""

        if permutation_depth < 1:
            raise ValueError("permutation_depth must be at least 1")
        normalized: list[tuple[str, tuple[str, ...]]] = []
        predictor_union: list[str] = []
        for raw_target, raw_predictors in predictors_by_target.items():
            target = str(raw_target)
            if not target:
                raise ValueError("target symbols must not be empty")
            predictors = tuple(dict.fromkeys(str(value) for value in raw_predictors))
            if any(not symbol for symbol in predictors):
                raise ValueError("predictor symbols must not be empty")
            if target in predictors:
                raise ValueError("A target cannot also be one of its predictors")
            normalized.append((target, predictors))
            predictor_union.extend(predictors)
        normalized_targets = tuple(target for target, _ in normalized)
        if len(set(normalized_targets)) != len(normalized_targets):
            raise ValueError("target symbols must be unique")
        plan = object.__new__(cls)
        plan._initialize(
            predictor_symbols=tuple(dict.fromkeys(predictor_union)),
            target_symbols=normalized_targets,
            predictors_by_target=tuple(normalized),
            permutation_depth=permutation_depth,
            population_kind=PREFILTERED_POPULATION,
        )
        return plan

    def _initialize(
        self,
        *,
        predictor_symbols: tuple[str, ...],
        target_symbols: tuple[str, ...],
        predictors_by_target: tuple[tuple[str, tuple[str, ...]], ...],
        permutation_depth: int,
        population_kind: str,
    ) -> None:
        object.__setattr__(self, "predictor_symbols", predictor_symbols)
        object.__setattr__(self, "target_symbols", target_symbols)
        object.__setattr__(self, "permutation_depth", int(permutation_depth))
        object.__setattr__(self, "population_kind", population_kind)
        object.__setattr__(self, "_predictors_by_target", predictors_by_target)
        segments: list[_Segment] = []
        offset = 0
        for feature_count in range(1, permutation_depth + 1):
            for target, predictors in predictors_by_target:
                segment_count = (
                    comb(len(predictors), feature_count)
                    if feature_count <= len(predictors)
                    else 0
                )
                if segment_count == 0:
                    continue
                segments.append(
                    _Segment(
                        target=target,
                        predictors=predictors,
                        feature_count=feature_count,
                        start=offset,
                        stop=offset + segment_count,
                    )
                )
                offset += segment_count
        object.__setattr__(self, "_segments", tuple(segments))
        object.__setattr__(self, "_segment_stops", tuple(item.stop for item in segments))
        object.__setattr__(self, "_count", offset)
        object.__setattr__(self, "_plan_sha256", self._calculate_sha256())

    @property
    def plan_version(self) -> int:
        return COMBINATION_PLAN_VERSION

    @property
    def plan_sha256(self) -> str:
        return self._plan_sha256

    @property
    def columns(self) -> tuple[str, ...]:
        return tuple(f"V{index}" for index in range(self.permutation_depth + 1))

    @property
    def predictors_by_target(self) -> dict[str, tuple[str, ...]]:
        return dict(self._predictors_by_target)

    def to_dict(self) -> dict[str, object]:
        """Return the compact frozen definition, never the expanded rows."""

        return {
            "combination_plan_version": self.plan_version,
            "combination_plan_sha256": self.plan_sha256,
            "population_kind": self.population_kind,
            "permutation_depth": self.permutation_depth,
            "predictor_symbols": list(self.predictor_symbols),
            "target_symbols": list(self.target_symbols),
            "predictors_by_target": {
                target: list(predictors)
                for target, predictors in self._predictors_by_target
            },
        }

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "CombinationPlan":
        """Restore a frozen plan and reject any identity drift."""

        version = int(values.get("combination_plan_version", 0))
        if version != COMBINATION_PLAN_VERSION:
            raise ValueError("Unsupported combination plan version")
        kind = str(values.get("population_kind", ""))
        depth = int(values["permutation_depth"])
        if kind == RAW_POPULATION:
            plan = cls(
                tuple(str(value) for value in values["predictor_symbols"]),
                depth,
                target_symbols=tuple(
                    str(value) for value in values["target_symbols"]
                ),
            )
        elif kind == PREFILTERED_POPULATION:
            raw_mapping = values.get("predictors_by_target")
            if not isinstance(raw_mapping, Mapping):
                raise ValueError("Invalid prefiltered combination plan")
            plan = cls.from_target_predictors(
                {
                    str(target): tuple(str(value) for value in predictors)
                    for target, predictors in raw_mapping.items()
                },
                depth,
            )
        else:
            raise ValueError("Unsupported combination plan population")
        expected = str(values.get("combination_plan_sha256", ""))
        if not expected or plan.plan_sha256 != expected:
            raise ValueError("Combination plan fingerprint mismatch")
        return plan

    def count(self) -> int:
        return self._count

    def __len__(self) -> int:
        return self._count

    def __iter__(self) -> Iterator[tuple[str | None, ...]]:
        return self.iter_range()

    def __getitem__(
        self, index: int | slice
    ) -> tuple[str | None, ...] | pd.DataFrame:
        if isinstance(index, slice):
            if index.step not in {None, 1}:
                raise ValueError("CombinationPlan slices do not support a step")
            start, stop, _ = index.indices(self._count)
            return self.slice(start, stop)
        return self.row_at(index)

    def row_at(self, index: int) -> tuple[str | None, ...]:
        if index < 0:
            index += self._count
        if index < 0 or index >= self._count:
            raise IndexError("combination index out of range")
        segment_index = bisect_right(self._segment_stops, index)
        segment = self._segments[segment_index]
        selected_indexes = _unrank_combination(
            len(segment.predictors),
            segment.feature_count,
            index - segment.start,
        )
        selected = tuple(segment.predictors[item] for item in selected_indexes)
        return self._row(segment.target, selected)

    def iter_range(
        self, start: int = 0, stop: int | None = None
    ) -> Iterator[tuple[str | None, ...]]:
        stop = self._count if stop is None else stop
        _validate_range(start, stop, self._count)
        if start == stop:
            return
        segment_index = bisect_right(self._segment_stops, start)
        while segment_index < len(self._segments):
            segment = self._segments[segment_index]
            if segment.start >= stop:
                break
            local_start = max(start, segment.start) - segment.start
            local_stop = min(stop, segment.stop) - segment.start
            for selected_indexes in _iter_index_combinations(
                len(segment.predictors),
                segment.feature_count,
                local_start,
                local_stop,
            ):
                selected = tuple(
                    segment.predictors[item] for item in selected_indexes
                )
                yield self._row(segment.target, selected)
            segment_index += 1

    def slice(self, start: int, stop: int) -> pd.DataFrame:
        _validate_range(start, stop, self._count)
        frame = pd.DataFrame.from_records(
            self.iter_range(start, stop), columns=list(self.columns)
        )
        if not frame.empty:
            frame = frame.astype({column: "str" for column in frame.columns})
        return frame

    def _row(
        self, target: str, selected: tuple[str, ...]
    ) -> tuple[str | None, ...]:
        padding = (None,) * (self.permutation_depth - len(selected))
        return (target, *selected, *padding)

    def _calculate_sha256(self) -> str:
        population: object
        if self.population_kind == RAW_POPULATION:
            population = {
                "predictor_symbols": list(self.predictor_symbols),
                "target_symbols": list(self.target_symbols),
            }
        else:
            population = [
                {"target": target, "predictors": list(predictors)}
                for target, predictors in self._predictors_by_target
            ]
        payload = {
            "combination_plan_version": COMBINATION_PLAN_VERSION,
            "ordering_policy": _ORDERING_POLICY,
            "population_kind": self.population_kind,
            "permutation_depth": self.permutation_depth,
            "population": population,
        }
        canonical = json.dumps(
            payload, ensure_ascii=False, sort_keys=True, separators=(",", ":")
        )
        return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


@dataclass(frozen=True, slots=True)
class CombinationPlanPreview:
    plan_version: int
    plan_sha256: str
    target_count: int
    context_count: int
    predictor_count: int
    permutation_depth: int
    raw_combination_count: int
    max_combinations_per_batch: int | None
    preview_batch_count: int
    max_combinations_after_prefilter: int
    max_batches_after_prefilter: int
    prefiltered_combination_count: int | None = None
    effective_batch_count: int | None = None

    @property
    def effective_combination_count(self) -> int | None:
        return self.prefiltered_combination_count

    def to_dict(self) -> dict[str, int | str | None]:
        return {
            "combination_plan_version": self.plan_version,
            "combination_plan_sha256": self.plan_sha256,
            "target_count": self.target_count,
            "context_count": self.context_count,
            "predictor_count": self.predictor_count,
            "depth": self.permutation_depth,
            "raw_combination_count": self.raw_combination_count,
            "max_combinations_per_batch": self.max_combinations_per_batch,
            "preview_batch_count": self.preview_batch_count,
            "max_combinations_after_prefilter": self.max_combinations_after_prefilter,
            "max_batches_after_prefilter": self.max_batches_after_prefilter,
            "prefiltered_combination_count": self.prefiltered_combination_count,
            "effective_batch_count": self.effective_batch_count,
        }


def build_combination_plan(
    *,
    target_symbols: Sequence[str],
    predictor_symbols: Sequence[str],
    permutation_depth: int,
) -> CombinationPlan:
    """Shared raw-plan factory for execution and pre-submission preview."""

    return CombinationPlan(
        predictor_symbols,
        permutation_depth,
        target_symbols=target_symbols,
    )


def build_combination_preview(
    raw_plan: CombinationPlan,
    *,
    context_symbols: Sequence[str] = (),
    max_combinations_per_batch: int | None,
    prefilter_enabled: bool = False,
    prefilter_top_n: int | None = None,
    effective_plan: CombinationPlan | None = None,
) -> CombinationPlanPreview:
    """Summarize raw preview counts and optional post-prefilter counts."""

    if raw_plan.population_kind != RAW_POPULATION:
        raise ValueError("raw_plan must describe the raw population")
    contexts = _unique_symbols(context_symbols, "context_symbols")
    if not set(contexts) <= set(raw_plan.predictor_symbols):
        raise ValueError("context_symbols must be included in predictor_symbols")
    raw_count = raw_plan.count()
    if prefilter_enabled and (prefilter_top_n is None or prefilter_top_n < 1):
        raise ValueError("prefilter_top_n must be positive when prefilter is enabled")
    max_after_prefilter = (
        raw_count
        if not prefilter_enabled
        else sum(
            count_target_symbol_sets(
                min(len(predictors), int(prefilter_top_n)),
                raw_plan.permutation_depth,
            )
            for predictors in raw_plan.predictors_by_target.values()
        )
    )
    if effective_plan is not None:
        if effective_plan.population_kind != PREFILTERED_POPULATION:
            raise ValueError("effective_plan must describe a prefiltered population")
        if effective_plan.permutation_depth != raw_plan.permutation_depth:
            raise ValueError("raw and effective plans must use the same depth")
        if not set(effective_plan.target_symbols) <= set(raw_plan.target_symbols):
            raise ValueError("effective plan targets must belong to the raw plan")
    effective_count = None if effective_plan is None else effective_plan.count()
    return CombinationPlanPreview(
        plan_version=raw_plan.plan_version,
        plan_sha256=raw_plan.plan_sha256,
        target_count=len(raw_plan.target_symbols),
        context_count=len(contexts),
        predictor_count=len(raw_plan.predictor_symbols),
        permutation_depth=raw_plan.permutation_depth,
        raw_combination_count=raw_count,
        max_combinations_per_batch=max_combinations_per_batch,
        preview_batch_count=_batch_count(raw_count, max_combinations_per_batch),
        max_combinations_after_prefilter=max_after_prefilter,
        max_batches_after_prefilter=_batch_count(
            max_after_prefilter, max_combinations_per_batch
        ),
        prefiltered_combination_count=effective_count,
        effective_batch_count=(
            None
            if effective_count is None
            else _batch_count(effective_count, max_combinations_per_batch)
        ),
    )


def _batch_count(count: int, capacity: int | None) -> int:
    if capacity is not None and capacity < 1:
        raise ValueError("max_combinations_per_batch must be positive or null")
    if count == 0:
        return 0
    if capacity is None:
        return 1
    return (count + capacity - 1) // capacity


def _unique_symbols(values: Sequence[str], name: str) -> tuple[str, ...]:
    normalized = tuple(str(value) for value in values)
    if any(not symbol for symbol in normalized):
        raise ValueError(f"{name} must not contain empty symbols")
    if len(set(normalized)) != len(normalized):
        raise ValueError(f"{name} must be unique")
    return normalized


def _validate_range(start: int, stop: int, count: int) -> None:
    if not isinstance(start, int) or isinstance(start, bool):
        raise TypeError("start must be an integer")
    if not isinstance(stop, int) or isinstance(stop, bool):
        raise TypeError("stop must be an integer")
    if start < 0 or stop < start or stop > count:
        raise IndexError("combination range is outside the plan")


def _unrank_combination(n: int, k: int, rank: int) -> tuple[int, ...]:
    total = comb(n, k)
    if rank < 0 or rank >= total:
        raise IndexError("combination rank out of range")
    selected: list[int] = []
    next_index = 0
    for position in range(k):
        remaining = k - position - 1
        maximum = n - remaining
        for candidate in range(next_index, maximum):
            block = comb(n - candidate - 1, remaining) if remaining else 1
            if rank < block:
                selected.append(candidate)
                next_index = candidate + 1
                break
            rank -= block
    return tuple(selected)


def _iter_index_combinations(
    n: int, k: int, start: int, stop: int
) -> Iterator[tuple[int, ...]]:
    if start >= stop:
        return
    current = list(_unrank_combination(n, k, start))
    for rank in range(start, stop):
        yield tuple(current)
        if rank + 1 == stop:
            break
        for position in range(k - 1, -1, -1):
            if current[position] < n - k + position:
                current[position] += 1
                for following in range(position + 1, k):
                    current[following] = current[following - 1] + 1
                break
