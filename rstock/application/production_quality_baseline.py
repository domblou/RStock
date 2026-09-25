"""Immutable promotion baselines and compact production-model lineage.

This module intentionally reads only persisted promotion inputs.  It contains
no production simulation, rolling health policy, or promotion decision logic.
"""

from __future__ import annotations

import hashlib
import json
import logging
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Mapping

import numpy as np
import pandas as pd

from .production_domain import ProductionModel
from .production_quality_repository import ProductionQualityRepository
from .production_repository import ProductionRepository
from .repository import RunRepository
from .universes import UniverseService


BASELINE_SCHEMA_VERSION = 1
BASELINE_METRICS_VERSION = 1
LINEAGE_SCHEMA_VERSION = 1
SIGNAL_RULE_VERSION = 1

AVAILABLE = "available"
UNAVAILABLE_MISSING_ARTIFACT = "unavailable_missing_artifact"
UNAVAILABLE_AMBIGUOUS_SOURCE = "unavailable_ambiguous_source"
UNAVAILABLE_LEGACY = "unavailable_legacy"

LOGGER = logging.getLogger(__name__)


def _timestamp() -> str:
    return datetime.now(timezone.utc).isoformat()


def _safe_mapping(value: object) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def _set_id(model: ProductionModel) -> str:
    persisted = _safe_mapping(model.development_metrics).get("Set")
    if persisted is not None and str(persisted):
        return str(persisted)
    return json.dumps([model.target, *model.predictors], separators=(",", ":"))


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _source_end_to_end_run_id(model: ProductionModel) -> str | None:
    source = _safe_mapping(model.source_configuration)
    value = source.get("source_end_to_end_run") or source.get("source_experiment_run")
    return None if value is None else str(value)


def _base_payload(model: ProductionModel) -> dict[str, Any]:
    return {
        "baseline_schema_version": BASELINE_SCHEMA_VERSION,
        "baseline_metrics_version": BASELINE_METRICS_VERSION,
        "model_id": model.model_id,
        "model_version": model.artifact_version,
        "created_at": _timestamp(),
        "promotion_date": model.created_at,
        "source_end_to_end_run_id": _source_end_to_end_run_id(model),
        "source_walk_forward_run_id": model.source_walk_forward_run,
        "source_threshold_calibration_run_id": model.source_threshold_calibration_run,
        "source_holdout_file": None,
        "source_holdout_sha256": None,
        "set_id": _set_id(model),
        "target": model.target,
        "predictors": list(model.predictors),
        "up_threshold": model.up_threshold,
        "down_threshold": model.down_threshold,
        "signal_rule_version": SIGNAL_RULE_VERSION,
        "holdout_start": None,
        "holdout_end": None,
        "metrics": _empty_metrics(),
        "availability_status": UNAVAILABLE_LEGACY,
    }


def _empty_metrics() -> dict[str, Any]:
    return {
        "holdout_sessions": 0,
        "evaluable_observations": 0,
        "excluded_observations": 0,
        "exclusion_reasons": {},
        "signal_count": 0,
        "signal_rate": None,
        "positive_trade_count": 0,
        "win_rate_strict_gt_0": None,
        "mean_intraday_return": None,
        "median_intraday_return": None,
        "cumulative_return_sum": None,
        "mean_mfe": None,
        "mean_mae": None,
        "max_adverse_trade_return": None,
        "max_favorable_trade_return": None,
    }


def _unavailable(model: ProductionModel, status: str) -> dict[str, Any]:
    payload = _base_payload(model)
    payload["availability_status"] = status
    return payload


def build_promotion_baseline(
    model: ProductionModel, runs: RunRepository
) -> dict[str, Any]:
    """Build the immutable baseline from the exact paired holdout predictions."""

    threshold_run = model.source_threshold_calibration_run
    if not threshold_run:
        return _unavailable(model, UNAVAILABLE_LEGACY)
    path = runs.run_directory(threshold_run) / "results" / "holdout_predictions.csv"
    if not path.exists():
        return _unavailable(model, UNAVAILABLE_MISSING_ARTIFACT)
    try:
        frame = pd.read_csv(path)
    except (OSError, pd.errors.ParserError, UnicodeDecodeError):
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)
    required = {
        "Set", "Direction", "Window", "Date", "Probability", "IntradayReturn", "MFE", "MAE"
    }
    if not required.issubset(frame.columns):
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)
    subset = frame[frame["Set"].astype(str) == _set_id(model)].copy()
    if subset.empty:
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)
    windows = subset["Window"].dropna().unique()
    if len(windows) != 1:
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)
    subset = subset[subset["Direction"].astype(str).isin(("Up", "Down"))].copy()
    subset["_date"] = pd.to_datetime(subset["Date"], errors="coerce").dt.date
    if subset.empty or subset["_date"].isna().all():
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)
    duplicates = subset.duplicated(["_date", "Direction"], keep=False)
    if duplicates.any():
        return _unavailable(model, UNAVAILABLE_AMBIGUOUS_SOURCE)

    payload = _base_payload(model)
    payload["source_holdout_file"] = str(path.relative_to(runs.root)).replace("\\", "/")
    payload["source_holdout_sha256"] = _sha256(path)
    payload["availability_status"] = AVAILABLE
    dates = sorted(item for item in subset["_date"].dropna().unique())
    payload["holdout_start"] = dates[0].isoformat()
    payload["holdout_end"] = dates[-1].isoformat()

    exclusions: dict[str, int] = {}
    evaluated: list[dict[str, float]] = []
    for date, group in subset.groupby("_date", sort=True, dropna=True):
        indexed = group.set_index("Direction")
        if not {"Up", "Down"}.issubset(indexed.index):
            exclusions["unpaired_direction"] = exclusions.get("unpaired_direction", 0) + 1
            continue
        up, down = indexed.loc["Up"], indexed.loc["Down"]
        values = pd.to_numeric(
            pd.Series([up["Probability"], down["Probability"], up["IntradayReturn"], up["MFE"], up["MAE"]]),
            errors="coerce",
        ).to_numpy(dtype=float)
        if not np.isfinite(values).all():
            exclusions["non_finite_holdout_value"] = exclusions.get("non_finite_holdout_value", 0) + 1
            continue
        evaluated.append({
            "date": date,
            "up_probability": values[0],
            "down_probability": values[1],
            "intraday_return": values[2],
            "mfe": values[3],
            "mae": values[4],
        })
    metrics = _empty_metrics()
    metrics["holdout_sessions"] = len(dates)
    metrics["evaluable_observations"] = len(evaluated)
    metrics["excluded_observations"] = sum(exclusions.values())
    metrics["exclusion_reasons"] = exclusions
    signals = [
        row for row in evaluated
        if row["up_probability"] >= float(model.up_threshold)
        and row["down_probability"] < float(model.down_threshold)
    ]
    metrics["signal_count"] = len(signals)
    metrics["signal_rate"] = None if not evaluated else len(signals) / len(evaluated)
    if signals:
        returns = np.array([row["intraday_return"] for row in signals], dtype=float)
        metrics.update({
            "positive_trade_count": int((returns > 0).sum()),
            "win_rate_strict_gt_0": float((returns > 0).mean()),
            "mean_intraday_return": float(returns.mean()),
            "median_intraday_return": float(np.median(returns)),
            "cumulative_return_sum": float(returns.sum()),
            "mean_mfe": float(np.mean([row["mfe"] for row in signals])),
            "mean_mae": float(np.mean([row["mae"] for row in signals])),
            "max_adverse_trade_return": float(returns.min()),
            "max_favorable_trade_return": float(returns.max()),
        })
    payload["metrics"] = metrics
    return payload


def _temporal_validation(model: ProductionModel, runs: RunRepository) -> tuple[str, str | None, str | None]:
    provenance = _safe_mapping(_safe_mapping(model.training_metadata).get("validation_provenance"))
    run_id = provenance.get("temporal_validation_run_id")
    if not run_id:
        enabled = _safe_mapping(model.source_configuration).get("temporal_validation_enabled")
        return ("not_available" if enabled else "not_applicable", None, None)
    run_id = str(run_id)
    # The validation child is the provenance identifier, while the comparison
    # is persisted by the End-to-end parent that owns the scientific decision.
    result_owner = _source_end_to_end_run_id(model) or run_id
    path = runs.run_directory(result_owner) / "results" / "temporal_validation_comparison.json"
    if not path.exists():
        return "not_available", "Artifact de validation temporelle absent.", run_id
    try:
        result = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return "not_available", "Artifact de validation temporelle illisible.", run_id
    status = str(result.get("final_status") or "not_available")
    if status == "passed":
        return "passed", None, run_id
    if status == "failed":
        failed = [str(gate.get("reason")) for gate in _safe_mapping(result.get("gates")).values() if _safe_mapping(gate).get("status") == "failed" and gate.get("reason")]
        return "failed", " ".join(failed) or None, run_id
    return "not_available", str(result.get("error") or "Validation temporelle non exploitable."), run_id


def build_model_lineage(
    model: ProductionModel,
    runs: RunRepository,
    *,
    project_root: Path | None = None,
    promotion_time_universe_names: Mapping[str, str | None] | None = None,
) -> dict[str, Any]:
    source = _safe_mapping(model.source_configuration)
    config = _safe_mapping(source.get("rstock_config"))
    roles = _safe_mapping(_safe_mapping(model.training_metadata).get("universe_roles"))
    primary_id = roles.get("primary_universe_id") or source.get("primary_universe_id")
    context_ids = roles.get("context_universe_ids") or source.get("context_universe_ids") or []
    context_id = context_ids[0] if isinstance(context_ids, list) and context_ids else None
    names = dict(promotion_time_universe_names or {})
    if project_root is not None:
        for key, value in (("primary", primary_id), ("context", context_id)):
            if names.get(key) is None and value is not None:
                try:
                    names[key] = UniverseService(root=project_root).record(str(value)).name
                except ValueError:
                    names[key] = None
    temporal_status, temporal_reason, temporal_run_id = _temporal_validation(model, runs)
    return {
        "lineage_schema_version": LINEAGE_SCHEMA_VERSION,
        "artifact_version": model.artifact_version,
        "model_id": model.model_id,
        "model_version": model.artifact_version,
        "target": model.target,
        "predictors": list(model.predictors),
        "set_id": _set_id(model),
        "status": model.status.value,
        "promotion_date": model.created_at,
        "created_at": _timestamp(),
        "primary_universe_id": primary_id,
        "primary_universe_name_at_promotion": names.get("primary"),
        "market_context_universe_id": context_id,
        "market_context_universe_name_at_promotion": names.get("context"),
        "source_end_to_end_run_id": _source_end_to_end_run_id(model),
        "source_walk_forward_run_id": model.source_walk_forward_run,
        "source_xgboost_calibration_run_id": model.source_xgboost_calibration_run,
        "source_threshold_calibration_run_id": model.source_threshold_calibration_run,
        "predictor_prefilter_enabled": config.get("predictor_prefilter_enabled"),
        "predictor_prefilter_top_n": config.get("predictor_prefilter_top_n"),
        "cutoff_date": (
            source.get("resolved_market_session_cutoff")
            or source.get("historical_data_cutoff")
        ),
        "wf_median_auc": (
            _safe_mapping(model.development_metrics).get("ROCAUCMedian")
            or _safe_mapping(model.development_metrics).get("MedianAUC")
        ),
        "holdout_auc": (
            _safe_mapping(model.holdout_metrics).get("FinalUpROCAUC")
            or _safe_mapping(model.holdout_metrics).get("AUC")
        ),
        "temporal_validation_status": temporal_status,
        "temporal_validation_reason": temporal_reason,
        "temporal_validation_run_id": temporal_run_id,
    }


class PromotionQualityService:
    """Optional, non-blocking materialization around an already-promoted model."""

    def __init__(self, project_root: Path, runs: RunRepository, production: ProductionRepository) -> None:
        self.project_root = Path(project_root)
        self.runs = runs
        self.production = production
        self.quality = ProductionQualityRepository(project_root)

    def materialize_after_promotion(self, model: ProductionModel) -> None:
        lineage = build_model_lineage(model, self.runs, project_root=self.project_root)
        self.quality.write_lineage(model.model_id, lineage)
        baseline = build_promotion_baseline(model, self.runs)
        self.quality.write_baseline(model.model_id, baseline)

    def rebuild_baseline_for_model(self, model_id: str) -> dict[str, Any]:
        model = self.production.get(model_id)
        baseline = build_promotion_baseline(model, self.runs)
        self.quality.write_baseline(model_id, baseline, overwrite=True)
        return baseline

    def rebuild_lineage_for_model(self, model_id: str) -> dict[str, Any]:
        model = self.production.get(model_id)
        existing = self.quality.load_lineage(model_id) or {}
        names = {
            "primary": existing.get("primary_universe_name_at_promotion"),
            "context": existing.get("market_context_universe_name_at_promotion"),
        }
        lineage = build_model_lineage(
            model, self.runs, promotion_time_universe_names=names
        )
        self.quality.write_lineage(model_id, lineage, overwrite=True)
        return lineage
