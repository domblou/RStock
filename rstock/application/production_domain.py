"""Versioned domain records for the operational RStock lifecycle."""

from __future__ import annotations

from dataclasses import asdict, dataclass, field
from enum import Enum
from typing import Any


class ProductionModelStatus(str, Enum):
    CANDIDATE = "candidate"
    TRAINED = "trained"
    ACTIVE = "active"
    INACTIVE = "inactive"
    RETIRED = "retired"


@dataclass(slots=True)
class ProductionModel:
    model_id: str
    target: str
    predictors: tuple[str, ...]
    lag_depth: int
    target_definition: str
    up_target_threshold: float
    down_target_threshold: float
    xgboost_parameters: dict[str, int | float]
    up_threshold: float
    down_threshold: float
    qualification_rules: dict[str, Any]
    source_walk_forward_run: str
    source_xgboost_calibration_run: str | None
    source_threshold_calibration_run: str | None
    development_metrics: dict[str, Any]
    holdout_metrics: dict[str, Any]
    created_at: str
    status: ProductionModelStatus = ProductionModelStatus.CANDIDATE
    schema_version: int = 1
    feature_version: str = "rstock_features_v1"
    artifact_version: int | None = None
    training_metadata: dict[str, Any] = field(default_factory=dict)
    down_xgboost_parameters: dict[str, int | float] | None = None
    xgboost_seed: int = 1234
    xgboost_threads: int = 2
    source_configuration: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not self.target or not self.predictors:
            raise ValueError("A production model requires a target and predictors")
        if self.target in self.predictors or len(set(self.predictors)) != len(self.predictors):
            raise ValueError("Production target and predictors must be distinct")
        if self.lag_depth < 1:
            raise ValueError("lag_depth must be positive")
        if self.up_target_threshold < 0 or self.down_target_threshold < 0:
            raise ValueError("Target thresholds cannot be negative")
        if not 0 <= self.up_threshold <= 1 or not 0 <= self.down_threshold <= 1:
            raise ValueError("Decision thresholds must be between zero and one")
        if not self.xgboost_parameters:
            raise ValueError("XGBoost parameters are required")
        if self.xgboost_threads < 1:
            raise ValueError("xgboost_threads must be positive")

    @property
    def symbols(self) -> tuple[str, ...]:
        return tuple(dict.fromkeys((self.target, *self.predictors)))

    def to_dict(self) -> dict[str, Any]:
        values = asdict(self)
        values["predictors"] = list(self.predictors)
        values["status"] = self.status.value
        return values

    @classmethod
    def from_dict(cls, values: dict[str, Any]) -> "ProductionModel":
        if int(values.get("schema_version", 0)) != 1:
            raise ValueError("Unsupported production model schema")
        restored = dict(values)
        restored["predictors"] = tuple(str(item) for item in restored["predictors"])
        restored["status"] = ProductionModelStatus(restored["status"])
        # Registry schema 1 existed briefly before these reproducibility fields
        # were explicit. Keep those local records readable with historical
        # defaults while every newly promoted model persists exact values.
        restored.setdefault("xgboost_seed", 1234)
        restored.setdefault("xgboost_threads", 2)
        restored.setdefault("source_configuration", {})
        return cls(**restored)


@dataclass(frozen=True, slots=True)
class OperationalUniverse:
    model_ids: tuple[str, ...]
    symbols: tuple[str, ...]
    used_by: dict[str, tuple[str, ...]]
