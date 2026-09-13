"""Serializable application-domain objects."""

from __future__ import annotations

import hashlib
import json
from dataclasses import asdict, dataclass, fields
from enum import Enum
from pathlib import Path
from typing import Any

from rstock.config import RStockConfig

from .universes import UniverseSelection


class JobType(str, Enum):
    WALK_FORWARD = "walk_forward"
    XGBOOST_CALIBRATION = "xgboost_calibration"
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

    @property
    def terminal(self) -> bool:
        return self in {JobStatus.COMPLETED, JobStatus.FAILED, JobStatus.CANCELLED}


def _config_to_dict(config: RStockConfig) -> dict[str, Any]:
    values = asdict(config)
    values["project_root"] = str(config.project_root)
    return values


def _config_from_dict(values: dict[str, Any]) -> RStockConfig:
    allowed = {field.name for field in fields(RStockConfig)}
    unknown = set(values) - allowed
    if unknown:
        raise ValueError(f"Unknown RStock configuration fields: {sorted(unknown)}")
    restored = dict(values)
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
    target_symbols: tuple[str, ...] = ()
    context_symbols: tuple[str, ...] = ()
    predictor_symbols: tuple[str, ...] = ()

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
        # ``symbols`` remains the backward-compatible market-data universe.
        object.__setattr__(self, "symbols", predictors)
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
            "target_symbols": list(self.target_symbols),
            "context_symbols": list(self.context_symbols),
            "predictor_symbols": list(self.predictor_symbols),
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
            target_symbols=tuple(str(item) for item in values.get("target_symbols", ())),
            context_symbols=tuple(str(item) for item in values.get("context_symbols", ())),
            predictor_symbols=tuple(
                str(item) for item in values.get("predictor_symbols", ())
            ),
        )

    @property
    def fingerprint(self) -> str:
        canonical = json.dumps(self.to_dict(), sort_keys=True, separators=(",", ":"))
        return hashlib.sha256(canonical.encode()).hexdigest()
