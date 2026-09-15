"""Serializable application-domain objects."""

from __future__ import annotations

import hashlib
import json
from dataclasses import asdict, dataclass, fields
from enum import Enum
from pathlib import Path
from typing import Any

from rstock.config import HISTORICAL_MISSING_CONFIG_DEFAULTS, RStockConfig

from .universes import UniverseSelection


class JobType(str, Enum):
    WALK_FORWARD = "walk_forward"
    XGBOOST_CALIBRATION = "xgboost_calibration"
    THRESHOLD_PARAMETER_CALIBRATION = "threshold_parameter_calibration"
    THRESHOLD_CALIBRATION = "threshold_calibration"
    FULL_TRAINING = "full_training"
    DAILY_PREDICTION = "daily_prediction"
    DATA_UPDATE = "data_update"
    DAILY_SCREENING = "daily_screening"
    PRODUCTION_TRAINING = "production_training"
    MARKET_UPDATE = "market_update"
    REALIZED_VALIDATION = "realized_validation"
    OPERATIONAL_RUN = "operational_run"

    @property
    def implemented(self) -> bool:
        return self in {
            JobType.WALK_FORWARD,
            JobType.XGBOOST_CALIBRATION,
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            JobType.THRESHOLD_CALIBRATION,
            JobType.PRODUCTION_TRAINING,
            JobType.MARKET_UPDATE,
            JobType.DAILY_PREDICTION,
            JobType.DAILY_SCREENING,
            JobType.REALIZED_VALIDATION,
            JobType.OPERATIONAL_RUN,
        }


class JobStatus(str, Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"
    INTERRUPTED = "interrupted"

    @property
    def terminal(self) -> bool:
        return self in {
            JobStatus.COMPLETED,
            JobStatus.FAILED,
            JobStatus.CANCELLED,
            JobStatus.INTERRUPTED,
        }


def _config_to_dict(config: RStockConfig) -> dict[str, Any]:
    values = asdict(config)
    values["project_root"] = str(config.project_root)
    return values


def _config_from_dict(values: dict[str, Any]) -> RStockConfig:
    allowed = {field.name for field in fields(RStockConfig)}
    unknown = set(values) - allowed
    if unknown:
        raise ValueError(f"Unknown RStock configuration fields: {sorted(unknown)}")
    restored = {**HISTORICAL_MISSING_CONFIG_DEFAULTS, **values}
    restored["project_root"] = Path(restored["project_root"])
    for field_name in ("selected_symbols", "threshold_calibration_quantiles"):
        if restored.get(field_name) is not None:
            restored[field_name] = tuple(restored[field_name])
    return RStockConfig(**restored)


@dataclass(frozen=True, slots=True)
class ExperimentSpec:
    """Complete reproducible configuration saved before a worker is launched."""

    job_type: JobType
    config: RStockConfig
    symbols: tuple[str, ...]
    calendar: str = "XNYS"
    combinations_per_target: int = 3
    evaluate_final_holdout: bool = True
    universe_selection: UniverseSelection = UniverseSelection()
    model_id: str | None = None
    primary_universe_id: str | None = None
    context_universe_ids: tuple[str, ...] = ()
    context_sample_size: int | None = None
    context_selection_method: str | None = None
    context_seed: int | None = None
    target_symbols: tuple[str, ...] = ()
    context_symbols: tuple[str, ...] = ()
    predictor_symbols: tuple[str, ...] = ()
    source_experiment_run: str | None = None
    source_walk_forward_run: str | None = None
    source_xgboost_calibration_run: str | None = None
    frozen_xgboost_parameters: dict[str, dict[str, int | float]] | None = None
    frozen_xgboost_parameters_sha256: str | None = None
    xgboost_resolution_version: int = 1
    source_threshold_parameter_calibration_run: str | None = None
    frozen_threshold_calibration_parameters: dict[str, object] | None = None
    frozen_threshold_calibration_parameters_sha256: str | None = None
    run_description: str | None = None
    historical_data_cutoff: str | None = None
    source_prepared_dataset_sha256: str | None = None

    def __post_init__(self) -> None:
        if not self.job_type.implemented:
            raise ValueError(f"Job type is reserved but not implemented: {self.job_type.value}")
        if len(set(self.symbols)) != len(self.symbols):
            raise ValueError("Symbols must be unique")
        targets = tuple(dict.fromkeys(self.target_symbols or self.symbols))
        target_set = set(targets)
        context = tuple(
            symbol
            for symbol in dict.fromkeys(self.context_symbols)
            if symbol not in target_set
        )
        predictors = tuple(dict.fromkeys((*targets, *context)))
        context_ids = tuple(dict.fromkeys(self.context_universe_ids))
        primary_id = self.primary_universe_id or self.universe_selection.universe
        object.__setattr__(self, "primary_universe_id", primary_id)
        object.__setattr__(self, "context_universe_ids", context_ids)
        object.__setattr__(self, "target_symbols", targets)
        object.__setattr__(self, "context_symbols", context)
        object.__setattr__(self, "predictor_symbols", predictors)
        description = None if self.run_description is None else str(self.run_description).strip()
        object.__setattr__(self, "run_description", description or None)
        # ``symbols`` remains the backward-compatible market-data universe.
        object.__setattr__(self, "symbols", predictors)
        frozen = self.frozen_xgboost_parameters
        if frozen is not None:
            normalized = {
                direction: {
                    str(name): value
                    for name, value in dict(frozen.get(direction, {})).items()
                }
                for direction in ("Up", "Down")
            }
            canonical = json.dumps(normalized, sort_keys=True, separators=(",", ":"))
            digest = hashlib.sha256(canonical.encode()).hexdigest()
            object.__setattr__(self, "frozen_xgboost_parameters", normalized)
            object.__setattr__(self, "frozen_xgboost_parameters_sha256", digest)
        else:
            object.__setattr__(self, "frozen_xgboost_parameters_sha256", None)
        frozen_threshold = self.frozen_threshold_calibration_parameters
        if frozen_threshold is not None:
            normalized_threshold = {
                str(name): list(value) if isinstance(value, tuple) else value
                for name, value in frozen_threshold.items()
            }
            canonical_threshold = json.dumps(
                normalized_threshold, sort_keys=True, separators=(",", ":")
            )
            threshold_digest = hashlib.sha256(canonical_threshold.encode()).hexdigest()
            object.__setattr__(
                self, "frozen_threshold_calibration_parameters", normalized_threshold
            )
            object.__setattr__(
                self, "frozen_threshold_calibration_parameters_sha256", threshold_digest
            )
        else:
            object.__setattr__(
                self, "frozen_threshold_calibration_parameters_sha256", None
            )
        if len(self.symbols) < 2:
            raise ValueError("At least two symbols are required")
        if self.combinations_per_target < 1:
            raise ValueError("combinations_per_target must be positive")
        if self.job_type == JobType.PRODUCTION_TRAINING and not self.model_id:
            raise ValueError("production_training requires model_id")

    def to_dict(self) -> dict[str, Any]:
        return {
            "schema_version": 1,
            "job_type": self.job_type.value,
            "symbols": list(self.symbols),
            "primary_universe_id": self.primary_universe_id,
            "context_universe_ids": list(self.context_universe_ids),
            "context_sample_size": self.context_sample_size,
            "context_selection_method": self.context_selection_method,
            "context_seed": self.context_seed,
            "target_symbols": list(self.target_symbols),
            "context_symbols": list(self.context_symbols),
            "predictor_symbols": list(self.predictor_symbols),
            "source_experiment_run": self.source_experiment_run,
            "source_walk_forward_run": self.source_walk_forward_run,
            "source_xgboost_calibration_run": self.source_xgboost_calibration_run,
            "frozen_xgboost_parameters": self.frozen_xgboost_parameters,
            "frozen_xgboost_parameters_sha256": self.frozen_xgboost_parameters_sha256,
            "xgboost_resolution_version": self.xgboost_resolution_version,
            "source_threshold_parameter_calibration_run": (
                self.source_threshold_parameter_calibration_run
            ),
            "frozen_threshold_calibration_parameters": (
                self.frozen_threshold_calibration_parameters
            ),
            "frozen_threshold_calibration_parameters_sha256": (
                self.frozen_threshold_calibration_parameters_sha256
            ),
            "run_description": self.run_description,
            "historical_data_cutoff": self.historical_data_cutoff,
            "source_prepared_dataset_sha256": self.source_prepared_dataset_sha256,
            "calendar": self.calendar,
            "combinations_per_target": self.combinations_per_target,
            "evaluate_final_holdout": self.evaluate_final_holdout,
            # symbols is the resolved, immutable list used by this run. The
            # nested selection records how that list was constructed for audit.
            "universe_selection": self.universe_selection.as_dict(),
            "model_id": self.model_id,
            "rstock_config": _config_to_dict(self.config),
        }

    @classmethod
    def from_dict(cls, values: dict[str, Any]) -> "ExperimentSpec":
        if values.get("schema_version") != 1:
            raise ValueError("Unsupported experiment configuration schema")
        return cls(
            job_type=JobType(values["job_type"]),
            config=_config_from_dict(values["rstock_config"]),
            symbols=tuple(str(symbol) for symbol in values["symbols"]),
            calendar=str(values.get("calendar", "XNYS")),
            combinations_per_target=int(values.get("combinations_per_target", 3)),
            evaluate_final_holdout=bool(values.get("evaluate_final_holdout", True)),
            universe_selection=UniverseSelection.from_dict(values.get("universe_selection")),
            model_id=None if values.get("model_id") is None else str(values["model_id"]),
            primary_universe_id=(
                None
                if values.get("primary_universe_id") is None
                else str(values["primary_universe_id"])
            ),
            context_universe_ids=tuple(
                str(item) for item in values.get("context_universe_ids", ())
            ),
            context_sample_size=(
                None
                if values.get("context_sample_size") is None
                else int(values["context_sample_size"])
            ),
            context_selection_method=(
                None
                if values.get("context_selection_method") is None
                else str(values["context_selection_method"])
            ),
            context_seed=(
                None if values.get("context_seed") is None else int(values["context_seed"])
            ),
            target_symbols=tuple(str(item) for item in values.get("target_symbols", ())),
            context_symbols=tuple(str(item) for item in values.get("context_symbols", ())),
            predictor_symbols=tuple(
                str(item) for item in values.get("predictor_symbols", ())
            ),
            source_experiment_run=(
                None
                if values.get("source_experiment_run") is None
                else str(values["source_experiment_run"])
            ),
            source_walk_forward_run=(
                None
                if values.get("source_walk_forward_run") is None
                else str(values["source_walk_forward_run"])
            ),
            source_xgboost_calibration_run=(
                None
                if values.get("source_xgboost_calibration_run") is None
                else str(values["source_xgboost_calibration_run"])
            ),
            frozen_xgboost_parameters=(
                None
                if not isinstance(values.get("frozen_xgboost_parameters"), dict)
                else values["frozen_xgboost_parameters"]
            ),
            frozen_xgboost_parameters_sha256=(
                None
                if values.get("frozen_xgboost_parameters_sha256") is None
                else str(values["frozen_xgboost_parameters_sha256"])
            ),
            xgboost_resolution_version=int(values.get("xgboost_resolution_version", 0)),
            source_threshold_parameter_calibration_run=(
                None
                if values.get("source_threshold_parameter_calibration_run") is None
                else str(values["source_threshold_parameter_calibration_run"])
            ),
            frozen_threshold_calibration_parameters=(
                None
                if not isinstance(
                    values.get("frozen_threshold_calibration_parameters"), dict
                )
                else values["frozen_threshold_calibration_parameters"]
            ),
            frozen_threshold_calibration_parameters_sha256=(
                None
                if values.get("frozen_threshold_calibration_parameters_sha256") is None
                else str(values["frozen_threshold_calibration_parameters_sha256"])
            ),
            run_description=(
                None if values.get("run_description") is None
                else str(values["run_description"])
            ),
            historical_data_cutoff=(
                None
                if values.get("historical_data_cutoff") is None
                else str(values["historical_data_cutoff"])
            ),
            source_prepared_dataset_sha256=(
                None
                if values.get("source_prepared_dataset_sha256") is None
                else str(values["source_prepared_dataset_sha256"])
            ),
        )

    @property
    def fingerprint(self) -> str:
        canonical = json.dumps(self.to_dict(), sort_keys=True, separators=(",", ":"))
        return hashlib.sha256(canonical.encode()).hexdigest()
