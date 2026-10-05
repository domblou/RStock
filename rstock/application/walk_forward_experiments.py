"""Frozen-snapshot derivation of a completed Walk-forward run."""

from __future__ import annotations

import hashlib
from dataclasses import fields, replace
from math import isfinite
from typing import Any, Mapping

import pandas as pd

from rstock.checkpoints import CheckpointManager
from rstock.combination_planning import CombinationPlan
from rstock.config import RStockConfig
from rstock.model_selection import model_selection_parameters
from rstock.modeling import historical_xgboost_parameters

from .derived_snapshot import load_source_prepared_snapshot
from .domain import ExperimentSpec, JobType
from .repository import RunRepository, utc_now


WF_GEOMETRY_FIELDS = (
    "walk_forward_window_mode", "walk_forward_min_train_size",
    "walk_forward_train_size", "walk_forward_test_size",
    "walk_forward_step_size", "final_holdout_size",
)
WF_XGBOOST_FIELDS = (
    "xgb_max_depth", "xgb_eta", "xgb_rounds", "xgb_min_child_weight",
    "xgb_subsample", "xgb_colsample_bytree", "xgb_gamma",
    "xgb_reg_alpha", "xgb_reg_lambda", "xgb_seed",
)
WF_QUALIFICATION_FIELDS = (
    "qualification_min_median_auc",
    "qualification_min_pct_windows_above_random",
    "qualification_min_worst_window_auc", "qualification_min_windows",
    "qualification_min_positive_observations", "qualification_max_auc_std",
    "final_confirmation_min_auc", "prediction_threshold",
)
WF_SELECTION_FIELDS = (
    "model_selection_predictive_quality_weight",
    "model_selection_stability_weight", "model_selection_holdout_weight",
    "model_selection_signal_quality_weight",
    "model_selection_sample_adequacy_weight",
)
WF_DERIVATION_CONFIG_FIELDS = frozenset(
    (*WF_GEOMETRY_FIELDS, *WF_XGBOOST_FIELDS,
     *WF_QUALIFICATION_FIELDS, *WF_SELECTION_FIELDS)
)
WF_DERIVATION_FIELDS = WF_DERIVATION_CONFIG_FIELDS | {"evaluate_final_holdout"}


def _source_checkpoint(repository: RunRepository, run_id: str, spec: ExperimentSpec) -> CheckpointManager:
    config = spec.config
    return CheckpointManager(
        repository.run_directory(run_id), run_id=run_id,
        job_type=JobType.WALK_FORWARD.value,
        configuration_fingerprint=repository.configuration_fingerprint(run_id),
        batch_sizes={
            "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
            "walk_forward": config.walk_forward_batch_size,
            "final_holdout": config.final_holdout_batch_size,
        },
    )


def _candidate_source(repository: RunRepository, run_id: str, spec: ExperimentSpec) -> tuple[str, str]:
    checkpoint = _source_checkpoint(repository, run_id, spec)
    name = (
        "effective_combination_plan" if checkpoint.artifact_exists("effective_combination_plan")
        else "generated_sets"
    )
    if not checkpoint.artifact_exists(name):
        raise ValueError("Source Walk-forward candidate artifact is missing")
    payload = checkpoint.load_artifact(name)
    if name == "effective_combination_plan":
        if CombinationPlan.from_dict(payload).count() < 1:
            raise ValueError("Source Walk-forward candidate plan is empty")
    elif not isinstance(payload, pd.DataFrame) or payload.empty:
        raise ValueError("Source Walk-forward candidates are empty")
    raw = (repository.run_directory(run_id) / "checkpoints" / "artifacts"
           / f"{name}.pkl").read_bytes()
    return name, hashlib.sha256(raw).hexdigest()


def load_frozen_walk_forward_candidates(
    repository: RunRepository, spec: ExperimentSpec,
) -> CombinationPlan | pd.DataFrame:
    """Verify the source identity and return its exact input population."""
    provenance = spec.walk_forward_derivation
    if not isinstance(provenance, dict) or provenance.get("schema_version") != 1:
        raise ValueError("Walk-forward derivation provenance is missing")
    source_id = provenance.get("source_run_id")
    if source_id != spec.source_walk_forward_run or source_id != spec.source_experiment_run:
        raise ValueError("Walk-forward derivation parent differs")
    source = repository.load_spec(source_id)
    if source.job_type is not JobType.WALK_FORWARD:
        raise ValueError("Walk-forward derivation parent has the wrong job type")
    if repository.configuration_fingerprint(source_id) != provenance.get("source_fingerprint"):
        raise ValueError("Walk-forward derivation parent configuration changed")
    if (spec.requested_historical_cutoff != source.requested_historical_cutoff
            or spec.resolved_market_session_cutoff != source.resolved_market_session_cutoff
            or spec.historical_data_cutoff != provenance.get("prepared_dataset_as_of")):
        raise ValueError("Walk-forward derivation cutoff changed")
    frozen = {item.name for item in fields(RStockConfig)} - WF_DERIVATION_CONFIG_FIELDS
    if any(getattr(source.config, field) != getattr(spec.config, field) for field in frozen):
        raise ValueError("Walk-forward derivation changed a frozen parameter")
    if any(getattr(source, field) != getattr(spec, field) for field in (
        "symbols", "target_symbols", "context_symbols", "predictor_symbols", "calendar",
    )):
        raise ValueError("Walk-forward derivation population changed")
    artifact = provenance.get("candidate_artifact")
    if artifact not in {"effective_combination_plan", "generated_sets"}:
        raise ValueError("Walk-forward derivation candidate reference is invalid")
    path = (repository.run_directory(source_id) / "checkpoints" / "artifacts"
            / f"{artifact}.pkl")
    try:
        actual_sha = hashlib.sha256(path.read_bytes()).hexdigest()
    except OSError as error:
        raise ValueError("Walk-forward derivation candidates are missing") from error
    if actual_sha != provenance.get("candidate_sha256"):
        raise ValueError("Walk-forward derivation candidates changed")
    name, sha = _candidate_source(repository, source_id, source)
    if name != provenance.get("candidate_artifact") or sha != provenance.get("candidate_sha256"):
        raise ValueError("Walk-forward derivation candidates changed")
    payload = _source_checkpoint(repository, source_id, source).load_artifact(name)
    return CombinationPlan.from_dict(payload) if name == "effective_combination_plan" else payload


def _validate_changes(source: ExperimentSpec, changes: Mapping[str, Any]) -> dict[str, Any]:
    if not changes or set(changes) - WF_DERIVATION_FIELDS:
        raise ValueError("Unsupported or empty Walk-forward derivation")
    effective = {
        field: value for field, value in changes.items()
        if value != getattr(source.config if field in WF_DERIVATION_CONFIG_FIELDS else source, field)
    }
    if not effective:
        raise ValueError("At least one Walk-forward parameter must change")
    config = replace(source.config, **{
        field: value for field, value in effective.items()
        if field in WF_DERIVATION_CONFIG_FIELDS
    })
    historical_xgboost_parameters(config)
    model_selection_parameters(config)
    for field in (
        "walk_forward_min_train_size", "walk_forward_train_size",
        "walk_forward_test_size", "walk_forward_step_size", "final_holdout_size",
        "qualification_min_windows", "xgb_seed",
    ):
        value = getattr(config, field)
        minimum = 0 if field == "xgb_seed" else 1
        if not isinstance(value, int) or isinstance(value, bool) or value < minimum:
            raise ValueError(f"{field} must be an integer >= {minimum}")
    positive = config.qualification_min_positive_observations
    if not isinstance(positive, int) or isinstance(positive, bool) or positive < 0:
        raise ValueError("qualification_min_positive_observations must be nonnegative")
    for field in (
        "qualification_min_median_auc", "qualification_min_pct_windows_above_random",
        "qualification_min_worst_window_auc", "final_confirmation_min_auc",
        "prediction_threshold",
    ):
        value = getattr(config, field)
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not isfinite(value) or not 0 <= value <= 1:
            raise ValueError(f"{field} must be between zero and one")
    if not isfinite(config.qualification_max_auc_std) or config.qualification_max_auc_std < 0:
        raise ValueError("qualification_max_auc_std must be nonnegative")
    if "evaluate_final_holdout" in effective and not isinstance(effective["evaluate_final_holdout"], bool):
        raise ValueError("evaluate_final_holdout must be boolean")
    return effective


def build_derived_walk_forward_spec(
    repository: RunRepository, source_run_id: str, changes: Mapping[str, Any],
) -> ExperimentSpec:
    source = repository.load_spec(source_run_id)
    if source.job_type is not JobType.WALK_FORWARD:
        raise ValueError("Walk-forward derivation requires a Walk-forward parent")
    if repository.status(source_run_id).get("status") != "completed":
        raise ValueError("Source Walk-forward must be completed")
    if repository.storage(source_run_id)["state"] != "full":
        raise ValueError("Source Walk-forward has been purged")
    effective = _validate_changes(source, changes)
    trace = repository.summary(source_run_id).get("traceability")
    if not isinstance(trace, dict) or not trace.get("prepared_dataset_sha256"):
        raise ValueError("Source Walk-forward dataset digest is missing")
    snapshot_path = (repository.run_directory(source_run_id) / "checkpoints"
                     / "artifacts" / "prepared_snapshot.pkl")
    try:
        snapshot_sha = hashlib.sha256(snapshot_path.read_bytes()).hexdigest()
    except OSError as error:
        raise ValueError("Source Walk-forward prepared snapshot is missing") from error
    anchor = str(pd.Timestamp(trace["prepared_market_last_date"]).date())
    frozen = replace(
        source, historical_data_cutoff=anchor,
        source_walk_forward_run=source_run_id,
        source_prepared_dataset_sha256=str(trace["prepared_dataset_sha256"]),
        prepared_dataset_digest_required=True, prepared_snapshot_required=True,
    )
    load_source_prepared_snapshot(
        repository, frozen, expected_snapshot_sha256=snapshot_sha,
    )
    name, candidate_sha = _candidate_source(repository, source_run_id, source)
    provenance = {
        "schema_version": 1, "source_run_id": source_run_id,
        "source_fingerprint": repository.configuration_fingerprint(source_run_id),
        "prepared_snapshot_sha256": snapshot_sha,
        "prepared_dataset_sha256": str(trace["prepared_dataset_sha256"]),
        "prepared_dataset_as_of": anchor,
        "requested_historical_cutoff": source.requested_historical_cutoff,
        "resolved_market_session_cutoff": source.resolved_market_session_cutoff,
        "candidate_artifact": name, "candidate_sha256": candidate_sha,
        "created_at": utc_now(),
        "overrides": {
            field: {
                "old_value": getattr(source.config if field in WF_DERIVATION_CONFIG_FIELDS else source, field),
                "new_value": value,
            } for field, value in sorted(effective.items())
        },
    }
    config_changes = {field: value for field, value in effective.items()
                      if field in WF_DERIVATION_CONFIG_FIELDS}
    return replace(
        source, config=replace(source.config, **config_changes),
        evaluate_final_holdout=effective.get("evaluate_final_holdout", source.evaluate_final_holdout),
        source_experiment_run=source_run_id, source_walk_forward_run=source_run_id,
        source_end_to_end_run=None, historical_data_cutoff=anchor,
        source_prepared_dataset_sha256=str(trace["prepared_dataset_sha256"]),
        prepared_dataset_digest_required=True, prepared_snapshot_required=True,
        forced_symbol_sets=None, forced_period_lock=None,
        auto_promote_candidates=False, forward_simulation_enabled=False,
        walk_forward_derivation=provenance,
    )
