"""Preflight and immutable specification for a derived End-to-End run."""

from __future__ import annotations

import hashlib
import json
from dataclasses import replace
from typing import Any, Mapping

import pandas as pd

from .derivation import (
    Derivation, InheritedStage, ParameterOverride, derivation_graph,
    SCIENTIFIC_STAGE_KEYS, SPLIT_SCIENTIFIC_STAGE_KEYS, stage_modes,
)
from .derived_snapshot import load_source_prepared_snapshot
from .domain import ExperimentSpec, JobType
from .end_to_end import PIPELINE_MANIFEST, artifact_digests, load_pipeline_manifest
from .repository import RunRepository, utc_now


def source_parameter_value(
    repository: RunRepository, source_spec: ExperimentSpec,
    source_manifest: dict[str, Any], field: str,
) -> Any:
    if field == "combinations_per_target":
        return source_spec.combinations_per_target
    if field == "evaluate_final_holdout":
        return source_spec.evaluate_final_holdout
    if field.startswith("forward_simulation_"):
        return getattr(source_spec, field)
    if field.startswith("threshold_calibration_"):
        stage = next(
            item for item in source_manifest["stages"]
            if item["stage_key"] == "threshold_parameter_calibration"
        )
        path = (
            repository.run_directory(str(stage["child_run_id"]))
            / "results" / "selected_threshold_calibration_configuration.json"
        )
        selection = json.loads(path.read_text(encoding="utf-8"))
        parameters = selection.get("parameters")
        if not isinstance(parameters, dict) or field not in parameters:
            raise ValueError(f"Selected threshold parameter is unavailable: {field}")
        return parameters[field]
    value = getattr(source_spec.config, field)
    return list(value) if isinstance(value, tuple) else value


def build_derived_spec(
    repository: RunRepository,
    source_end_to_end_run_id: str,
    fork_stage: str,
    changes: Mapping[str, Any],
    *,
    forward_enabled: bool = False,
) -> ExperimentSpec:
    """Validate frozen sources and return a new, unsubmitted End-to-End spec."""
    source_spec = repository.load_spec(source_end_to_end_run_id)
    if source_spec.job_type is not JobType.END_TO_END:
        raise ValueError("Derivation requires an End-to-End source")
    if source_spec.derivation is not None:
        raise ValueError("Derivation of a derived End-to-End is deferred")
    if source_spec.forced_symbol_sets is not None:
        raise ValueError("Forced End-to-End derivation is deferred")
    if repository.status(source_end_to_end_run_id).get("status") != "completed":
        raise ValueError("Source End-to-End must be completed")
    if repository.storage(source_end_to_end_run_id)["state"] != "full":
        raise ValueError("Source End-to-End has been purged")
    manifest = load_pipeline_manifest(repository, source_end_to_end_run_id)
    if manifest is None or manifest["schema_version"] not in {1, 3}:
        raise ValueError("Source End-to-End manifest is unavailable")
    if manifest.get("temporal_validation_enabled") is not source_spec.temporal_validation_enabled:
        raise ValueError("Source temporal validation provenance is inconsistent")
    schema_version = 2 if manifest["schema_version"] == 3 else 1
    _, parameter_fields = derivation_graph(schema_version)
    parameter_owner = {field: stage for stage, fields in parameter_fields.items()
                       for field in fields}
    scientific_keys = (SPLIT_SCIENTIFIC_STAGE_KEYS if schema_version == 2
                       else SCIENTIFIC_STAGE_KEYS)
    modes = stage_modes(fork_stage, schema_version=schema_version)
    inherited: dict[str, InheritedStage] = {}
    for stage in manifest["stages"]:
        key = stage["stage_key"]
        if key not in scientific_keys or modes[key] != "inherited":
            continue
        run_id = str(stage["child_run_id"])
        if repository.status(run_id).get("status") != "completed":
            raise ValueError(f"Inherited stage is not completed: {key}")
        if repository.storage(run_id)["state"] != "full":
            raise ValueError(f"Inherited stage has been purged: {key}")
        fingerprint = repository.configuration_fingerprint(run_id)
        if fingerprint != stage.get("expected_fingerprint"):
            raise ValueError(f"Inherited stage fingerprint differs: {key}")
        digests = artifact_digests(repository, run_id, key)
        if digests != stage.get("artifact_digests"):
            raise ValueError(f"Inherited stage artifact digests differ: {key}")
        inherited[key] = InheritedStage(run_id, fingerprint, digests)
    walk_forward_id = str(manifest["stages"][0]["child_run_id"])
    traceability = repository.summary(walk_forward_id).get("traceability")
    if not isinstance(traceability, dict) or not traceability.get("prepared_dataset_sha256"):
        raise ValueError("Source Walk-forward digest is unavailable")
    expected_digest = str(traceability["prepared_dataset_sha256"])
    snapshot_spec = replace(
        source_spec, job_type=JobType.XGBOOST_CALIBRATION,
        temporal_validation_enabled=False,
        source_walk_forward_run=walk_forward_id,
        source_prepared_dataset_sha256=expected_digest,
        prepared_dataset_digest_required=True,
        prepared_snapshot_required=True,
    )
    prepared, _, _, _ = load_source_prepared_snapshot(repository, snapshot_spec)
    source_as_of = manifest.get("prepared_dataset_as_of")
    if source_as_of is None:
        # Historic manifests may predate this field. The verified Walk-forward
        # snapshot and its persisted traceability must agree on the session.
        try:
            traced = pd.Timestamp(traceability["prepared_market_last_date"])
            observed = pd.Timestamp(prepared.index.max())
        except (KeyError, TypeError, ValueError) as error:
            raise ValueError("Source Walk-forward session is unavailable") from error
        if (
            pd.isna(traced) or pd.isna(observed)
            or traced.tzinfo is not None or observed.tzinfo is not None
            or traced != observed or traced != traced.normalize()
        ):
            raise ValueError("Source Walk-forward session is ambiguous")
        anchor = traced.date().isoformat()
    else:
        anchor = str(source_as_of)
        if not anchor.strip():
            raise ValueError("Source dataset session is missing")
    overrides = []
    config_changes: dict[str, Any] = {}
    other_changes: dict[str, Any] = {}
    for field, new_value in changes.items():
        if field not in parameter_owner:
            raise ValueError(f"Unsupported derivation parameter: {field}")
        old_value = source_parameter_value(repository, source_spec, manifest, field)
        if old_value == new_value:
            continue
        overrides.append(ParameterOverride(field, old_value, new_value))
        if field in {"combinations_per_target", "evaluate_final_holdout"} or field.startswith("forward_simulation_"):
            other_changes[field] = new_value
        else:
            config_changes[field] = (
                tuple(new_value) if field == "threshold_calibration_quantiles"
                else new_value
            )
    if not overrides:
        raise ValueError("At least one parameter must change")
    source_path = repository.run_directory(source_end_to_end_run_id) / PIPELINE_MANIFEST
    derivation = Derivation(
        schema_version=schema_version,
        source_end_to_end_run_id=source_end_to_end_run_id,
        fork_stage=fork_stage,
        source_manifest_sha256=hashlib.sha256(source_path.read_bytes()).hexdigest(),
        inherited_stages=inherited,
        overrides=tuple(overrides),
        created_at=utc_now(),
        prepared_snapshot_sha256=hashlib.sha256((
            repository.run_directory(walk_forward_id)
            / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
        ).read_bytes()).hexdigest(),
        source_temporal_validation_enabled=source_spec.temporal_validation_enabled,
    )
    derived = replace(
        source_spec,
        config=replace(source_spec.config, **config_changes),
        historical_data_cutoff=anchor,
        derivation=derivation,
        temporal_validation_enabled=False,
        auto_promote_candidates=False,
        forward_simulation_enabled=forward_enabled,
        **other_changes,
    )
    if (
        forward_enabled
        and derived.forward_simulation_mode == "custom_end_date"
        and derived.forward_simulation_end_date is None
    ):
        raise ValueError("Derived custom Forward requires an end date")
    return derived
