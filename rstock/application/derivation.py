"""Versioned, immutable planning inputs for derived End-to-End experiments.

This module describes lineage and invalidation. Execution is deliberately owned
by the existing End-to-End orchestrator.
"""

from __future__ import annotations

import json
from copy import deepcopy
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Mapping


DERIVATION_SCHEMA_VERSION = 1

# Insertion order is the scientific execution order. The optional downstream
# stages are part of the same dependency graph, even though Forward runs in a
# separate worker and promotion has no child run.
STAGE_DEPENDENCIES: dict[str, tuple[str, ...]] = {
    "walk_forward": (),
    "xgboost_calibration": ("walk_forward",),
    "threshold_parameter_calibration": (
        "walk_forward", "xgboost_calibration",
    ),
    "threshold_calibration": (
        "walk_forward", "xgboost_calibration",
        "threshold_parameter_calibration",
    ),
    "promotion": ("threshold_calibration",),
    "forward_simulation": ("threshold_calibration",),
}
SCIENTIFIC_STAGE_KEYS = tuple(STAGE_DEPENDENCIES)[:4]
FORK_STAGE_KEYS = SCIENTIFIC_STAGE_KEYS[1:]
STAGE_MODES = frozenset({"inherited", "recomputed", "not_executed"})

# An owner is the earliest stage from which changing this field is permitted.
# Fields used by the Walk-forward remain locked for every supported fork.
# Other fields are added here only after their effective consumer is verified.
STAGE_PARAMETER_FIELDS: dict[str, frozenset[str]] = {
    "xgboost_calibration": frozenset({
        "xgboost_global_max_qualified_combinations",
        "combinations_per_target",
    }),
    "threshold_parameter_calibration": frozenset({
        "threshold_parameter_calibration_max_models",
    }),
    "threshold_calibration": frozenset({
        "threshold_calibration_min_signals_per_window",
        "threshold_calibration_min_robust_signals",
        "threshold_calibration_min_window_fraction",
        "threshold_calibration_precision_tolerance",
        "threshold_calibration_quantiles",
        "threshold_calibration_grid_decimals",
    }),
    "promotion": frozenset({
        "promotion_min_holdout_signals",
        "promotion_min_holdout_auc",
        "promotion_min_holdout_precision",
        "promotion_min_mean_directional_return",
        "promotion_max_opposite_movement_frequency",
    }),
    "forward_simulation": frozenset({
        "forward_simulation_mode",
        "forward_simulation_end_date",
    }),
}
PARAMETER_OWNER = {
    field: stage
    for stage, fields in STAGE_PARAMETER_FIELDS.items()
    for field in fields
}


def _nonempty(value: object, name: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a non-empty string")
    return value


def _digest(value: object, name: str) -> str:
    result = _nonempty(value, name)
    if len(result) != 64 or any(char not in "0123456789abcdef" for char in result):
        raise ValueError(f"{name} must be a SHA-256 hex digest")
    return result


def stage_modes(
    fork_stage: str, *, promotion_enabled: bool = False,
    forward_enabled: bool = False,
) -> dict[str, str]:
    """Invalidate the fork and its transitive dependants in the stage DAG."""
    if fork_stage not in FORK_STAGE_KEYS:
        raise ValueError(f"Unsupported derivation point: {fork_stage}")
    invalidated = {fork_stage}
    for stage, dependencies in STAGE_DEPENDENCIES.items():
        if any(dependency in invalidated for dependency in dependencies):
            invalidated.add(stage)
    modes = {
        stage: "recomputed" if stage in invalidated else "inherited"
        for stage in STAGE_DEPENDENCIES
    }
    if not promotion_enabled:
        modes["promotion"] = "not_executed"
    if not forward_enabled:
        modes["forward_simulation"] = "not_executed"
    return modes


def validate_overrides(
    fork_stage: str, overrides: tuple["ParameterOverride", ...],
    *, promotion_enabled: bool = False, forward_enabled: bool = False,
) -> None:
    modes = stage_modes(
        fork_stage, promotion_enabled=promotion_enabled,
        forward_enabled=forward_enabled,
    )
    seen: set[str] = set()
    for override in overrides:
        if override.field in seen:
            raise ValueError(f"Duplicate derivation override: {override.field}")
        seen.add(override.field)
        owner = PARAMETER_OWNER.get(override.field)
        if owner is None:
            raise ValueError(f"Unsupported derivation parameter: {override.field}")
        if modes[owner] != "recomputed":
            raise ValueError(
                f"Parameter {override.field} belongs to an inherited or disabled stage"
            )


@dataclass(frozen=True, slots=True)
class InheritedStage:
    source_run_id: str
    configuration_fingerprint: str
    required_artifact_digests: dict[str, str]

    def __post_init__(self) -> None:
        _nonempty(self.source_run_id, "source_run_id")
        _digest(self.configuration_fingerprint, "configuration_fingerprint")
        if not self.required_artifact_digests:
            raise ValueError("Inherited stage requires artifact digests")
        for path, digest in self.required_artifact_digests.items():
            _nonempty(path, "artifact path")
            if path.startswith(("/", "\\")) or ".." in path.replace("\\", "/").split("/"):
                raise ValueError("Inherited artifact path must be relative to its run")
            _digest(digest, "artifact digest")
        object.__setattr__(self, "required_artifact_digests", deepcopy(self.required_artifact_digests))

    def to_dict(self) -> dict[str, Any]:
        return {
            "source_run_id": self.source_run_id,
            "configuration_fingerprint": self.configuration_fingerprint,
            "required_artifact_digests": deepcopy(self.required_artifact_digests),
        }

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "InheritedStage":
        digests = values.get("required_artifact_digests")
        if not isinstance(digests, Mapping):
            raise ValueError("Inherited artifact digests are missing")
        return cls(
            source_run_id=_nonempty(values.get("source_run_id"), "source_run_id"),
            configuration_fingerprint=_digest(
                values.get("configuration_fingerprint"), "configuration_fingerprint"
            ),
            required_artifact_digests={str(key): str(value) for key, value in digests.items()},
        )


@dataclass(frozen=True, slots=True)
class ParameterOverride:
    field: str
    old_value: Any
    new_value: Any

    def __post_init__(self) -> None:
        _nonempty(self.field, "override field")
        try:
            json.dumps([self.old_value, self.new_value], allow_nan=False)
        except (TypeError, ValueError) as error:
            raise ValueError("Override values must be finite JSON values") from error
        if self.old_value == self.new_value:
            raise ValueError(f"Override {self.field} does not change its value")
        object.__setattr__(self, "old_value", deepcopy(self.old_value))
        object.__setattr__(self, "new_value", deepcopy(self.new_value))

    def to_dict(self) -> dict[str, Any]:
        return {
            "field": self.field,
            "old_value": deepcopy(self.old_value),
            "new_value": deepcopy(self.new_value),
        }

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "ParameterOverride":
        if "old_value" not in values or "new_value" not in values:
            raise ValueError("Override requires old_value and new_value")
        return cls(
            field=_nonempty(values.get("field"), "override field"),
            old_value=values["old_value"],
            new_value=values["new_value"],
        )


@dataclass(frozen=True, slots=True)
class Derivation:
    source_end_to_end_run_id: str
    fork_stage: str
    source_manifest_sha256: str
    inherited_stages: dict[str, InheritedStage]
    overrides: tuple[ParameterOverride, ...]
    created_at: str
    prepared_snapshot_sha256: str | None = None
    schema_version: int = DERIVATION_SCHEMA_VERSION

    def __post_init__(self) -> None:
        if self.schema_version != DERIVATION_SCHEMA_VERSION:
            raise ValueError("Unsupported derivation schema")
        _nonempty(self.source_end_to_end_run_id, "source_end_to_end_run_id")
        _digest(self.source_manifest_sha256, "source_manifest_sha256")
        if self.prepared_snapshot_sha256 is not None:
            _digest(self.prepared_snapshot_sha256, "prepared_snapshot_sha256")
        if self.fork_stage not in FORK_STAGE_KEYS:
            raise ValueError(f"Unsupported derivation point: {self.fork_stage}")
        try:
            datetime.fromisoformat(self.created_at.replace("Z", "+00:00"))
        except (TypeError, ValueError) as error:
            raise ValueError("created_at must be an ISO timestamp") from error
        if not self.inherited_stages:
            raise ValueError("A derivation must inherit at least the Walk-forward")
        if not all(isinstance(ref, InheritedStage) for ref in self.inherited_stages.values()):
            raise ValueError("Invalid inherited stage reference")
        if not all(isinstance(item, ParameterOverride) for item in self.overrides):
            raise ValueError("Invalid derivation override")
        object.__setattr__(self, "inherited_stages", dict(self.inherited_stages))
        object.__setattr__(self, "overrides", tuple(self.overrides))

    def validate_plan(
        self, *, promotion_enabled: bool, forward_enabled: bool,
    ) -> dict[str, str]:
        modes = stage_modes(
            self.fork_stage,
            promotion_enabled=promotion_enabled,
            forward_enabled=forward_enabled,
        )
        expected = {
            stage for stage in SCIENTIFIC_STAGE_KEYS if modes[stage] == "inherited"
        }
        if set(self.inherited_stages) != expected:
            raise ValueError("Inherited stage references do not match the fork plan")
        validate_overrides(
            self.fork_stage, self.overrides,
            promotion_enabled=promotion_enabled,
            forward_enabled=forward_enabled,
        )
        return modes

    def to_dict(self) -> dict[str, Any]:
        values = {
            "schema_version": self.schema_version,
            "source_end_to_end_run_id": self.source_end_to_end_run_id,
            "fork_stage": self.fork_stage,
            "source_manifest_sha256": self.source_manifest_sha256,
            "inherited_stages": {
                stage: ref.to_dict() for stage, ref in self.inherited_stages.items()
            },
            "overrides": [item.to_dict() for item in self.overrides],
            "created_at": self.created_at,
        }
        if self.prepared_snapshot_sha256 is not None:
            values["prepared_snapshot_sha256"] = self.prepared_snapshot_sha256
        return values

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "Derivation":
        references = values.get("inherited_stages")
        overrides = values.get("overrides")
        if not isinstance(references, Mapping) or not isinstance(overrides, list):
            raise ValueError("Invalid derivation references or overrides")
        if not all(isinstance(item, Mapping) for item in references.values()):
            raise ValueError("Invalid inherited stage reference")
        if not all(isinstance(item, Mapping) for item in overrides):
            raise ValueError("Invalid derivation override")
        return cls(
            schema_version=int(values.get("schema_version", 0)),
            source_end_to_end_run_id=_nonempty(
                values.get("source_end_to_end_run_id"), "source_end_to_end_run_id"
            ),
            fork_stage=_nonempty(values.get("fork_stage"), "fork_stage"),
            source_manifest_sha256=_digest(
                values.get("source_manifest_sha256"), "source_manifest_sha256"
            ),
            inherited_stages={
                str(stage): InheritedStage.from_dict(ref)
                for stage, ref in references.items()
            },
            overrides=tuple(ParameterOverride.from_dict(item) for item in overrides),
            created_at=_nonempty(values.get("created_at"), "created_at"),
            prepared_snapshot_sha256=(
                None if values.get("prepared_snapshot_sha256") is None
                else _digest(values["prepared_snapshot_sha256"], "prepared_snapshot_sha256")
            ),
        )
