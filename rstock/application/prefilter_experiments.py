"""Frozen, independently executable Predictor prefilter derivations."""

from __future__ import annotations

import hashlib
from dataclasses import fields, replace
from pathlib import Path
from typing import Any, Mapping

from rstock.config import RStockConfig

from .derived_snapshot import load_source_prepared_snapshot
from .domain import ExperimentSpec, JobType
from .repository import RunRepository, utc_now


PREFILTER_DERIVATION_FIELDS = frozenset({
    "predictor_prefilter_top_n",
    "predictor_prefilter_min_median_auc",
    "predictor_prefilter_min_pct_above_random",
    "predictor_prefilter_min_worst_auc",
    "predictor_prefilter_max_auc_std",
    "predictor_prefilter_correlation_threshold",
})
PREFILTER_METHOD_FIELDS = frozenset({
    "prefilter_method", "stability_origin_count", "stability_step_sessions",
})


def prefilter_checkpoint_batch_sizes(config: RStockConfig) -> dict[str, int]:
    """One checkpoint identity for worker initialization, execution and resume."""
    return {
        "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
    }


def _snapshot_path(repository: RunRepository, run_id: str) -> Path:
    return (repository.run_directory(run_id) / "checkpoints" / "artifacts"
            / "prepared_snapshot.pkl")


def _validate_changes(source: ExperimentSpec, changes: Mapping[str, object]) -> dict[str, object]:
    config = source.config
    if not changes or set(changes) - (PREFILTER_DERIVATION_FIELDS | PREFILTER_METHOD_FIELDS):
        raise ValueError("Unsupported or empty Predictor prefilter derivation")
    effective = {
        key: value for key, value in changes.items()
        if value != getattr(config if key in PREFILTER_DERIVATION_FIELDS else source, key)
    }
    if not effective:
        raise ValueError("At least one prefilter parameter must change")
    top_n = effective.get("predictor_prefilter_top_n", config.predictor_prefilter_top_n)
    if not isinstance(top_n, int) or isinstance(top_n, bool) or top_n < 1:
        raise ValueError("predictor_prefilter_top_n must be a positive integer")
    for field in PREFILTER_DERIVATION_FIELDS - {"predictor_prefilter_top_n"}:
        value = effective.get(field, getattr(config, field))
        if (isinstance(value, bool) or not isinstance(value, (int, float))
                or not 0.0 <= float(value) <= 1.0):
            raise ValueError(f"{field} must be between zero and one")
    method = effective.get("prefilter_method", source.prefilter_method)
    if method not in {"single_origin", "temporal_stability"}:
        raise ValueError("Unsupported Predictor prefilter method")
    for field in ("stability_origin_count", "stability_step_sessions"):
        value = effective.get(field, getattr(source, field))
        if not isinstance(value, int) or isinstance(value, bool) or value < 1:
            raise ValueError(f"{field} must be a positive integer")
    return effective


def validate_prefilter_source(
    repository: RunRepository, spec: ExperimentSpec,
) -> tuple[object, list[str], list[str], dict[str, str]]:
    """Verify the parent's identity, frozen session, snapshot bytes and data digest."""
    provenance = spec.prefilter_derivation
    if not isinstance(provenance, dict) or provenance.get("schema_version") != 1:
        raise ValueError("Predictor prefilter derivation provenance is missing")
    source_id = provenance.get("source_run_id")
    if not isinstance(source_id, str) or source_id != spec.source_experiment_run:
        raise ValueError("Predictor prefilter source run differs from provenance")
    source = repository.load_spec(source_id)
    if source.job_type is not JobType.PREDICTOR_PREFILTER:
        raise ValueError("Predictor prefilter source has the wrong job type")
    if repository.configuration_fingerprint(source_id) != provenance.get("source_fingerprint"):
        raise ValueError("Predictor prefilter source configuration has changed")
    if (spec.historical_data_cutoff != provenance.get("prepared_dataset_as_of")
            or spec.source_prepared_dataset_sha256 != provenance.get("prepared_dataset_sha256")):
        raise ValueError("Predictor prefilter cutoff or dataset digest differs")
    if source.historical_data_cutoff != spec.historical_data_cutoff:
        raise ValueError("Predictor prefilter source cutoff differs")
    frozen_fields = {item.name for item in fields(RStockConfig)} - PREFILTER_DERIVATION_FIELDS
    if any(getattr(source.config, field) != getattr(spec.config, field)
           for field in frozen_fields):
        raise ValueError("Predictor prefilter preparation or ML configuration changed")
    if (source.symbols != spec.symbols or source.target_symbols != spec.target_symbols
            or source.predictor_symbols != spec.predictor_symbols):
        raise ValueError("Predictor prefilter universe changed")
    summary = repository.summary(source_id)
    if summary.get("prepared_dataset_as_of") != spec.historical_data_cutoff:
        raise ValueError("Predictor prefilter source session changed")
    return load_source_prepared_snapshot(
        repository, spec, source_run_id=source_id,
        source_job_type=JobType.PREDICTOR_PREFILTER,
        expected_snapshot_sha256=str(provenance.get("prepared_snapshot_sha256")),
    )


def build_derived_prefilter_spec(
    repository: RunRepository, source_run_id: str, changes: Mapping[str, Any],
) -> ExperimentSpec:
    source = repository.load_spec(source_run_id)
    if source.job_type is not JobType.PREDICTOR_PREFILTER:
        raise ValueError("Predictor prefilter derivation requires a prefilter source")
    if repository.status(source_run_id).get("status") != "completed":
        raise ValueError("Source Predictor prefilter must be completed")
    if repository.storage(source_run_id)["state"] != "full":
        raise ValueError("Source Predictor prefilter has been purged")
    effective = _validate_changes(source, changes)
    summary = repository.summary(source_run_id)
    trace = summary.get("traceability")
    if not isinstance(trace, dict) or not trace.get("prepared_dataset_sha256"):
        raise ValueError("Source Predictor prefilter dataset digest is missing")
    as_of = summary.get("prepared_dataset_as_of")
    if not isinstance(as_of, str) or as_of != source.historical_data_cutoff:
        raise ValueError("Source Predictor prefilter cutoff is ambiguous")
    snapshot_path = _snapshot_path(repository, source_run_id)
    try:
        snapshot_sha = hashlib.sha256(snapshot_path.read_bytes()).hexdigest()
    except OSError as error:
        raise ValueError("Source Predictor prefilter prepared snapshot is missing") from error
    digest = str(trace["prepared_dataset_sha256"])
    frozen_source = replace(
        source, source_prepared_dataset_sha256=digest,
        prepared_dataset_digest_required=True,
    )
    load_source_prepared_snapshot(
        repository, frozen_source, source_run_id=source_run_id,
        source_job_type=JobType.PREDICTOR_PREFILTER,
        expected_snapshot_sha256=snapshot_sha,
    )
    provenance = {
        "schema_version": 1,
        "source_run_id": source_run_id,
        "source_fingerprint": repository.configuration_fingerprint(source_run_id),
        "prepared_snapshot_sha256": snapshot_sha,
        "prepared_dataset_sha256": digest,
        "prepared_dataset_as_of": as_of,
        "created_at": utc_now(),
        "overrides": {
            field: {"old_value": getattr(
                source.config if field in PREFILTER_DERIVATION_FIELDS else source, field
            ), "new_value": value}
            for field, value in sorted(effective.items())
        },
    }
    config_changes = {
        field: value for field, value in effective.items()
        if field in PREFILTER_DERIVATION_FIELDS
    }
    method_changes = {
        field: value for field, value in effective.items()
        if field in PREFILTER_METHOD_FIELDS
    }
    return replace(
        source, config=replace(source.config, **config_changes), **method_changes,
        source_experiment_run=source_run_id,
        source_prepared_dataset_sha256=digest,
        prepared_dataset_digest_required=True,
        prepared_snapshot_required=True,
        prefilter_derivation=provenance,
    )
