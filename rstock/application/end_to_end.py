"""Durable orchestration for the scientific End-to-end pipeline."""

from __future__ import annotations

import hashlib
import json
from dataclasses import replace
from pathlib import Path
from typing import Any, Callable

import pandas as pd

from rstock.modeling import selected_xgboost_parameters
from rstock.progress import CancellationCheck, ProgressCallback, check_cancellation

from .auto_promotion import AutoPromotionRunner, PROMOTION_CHECKPOINT
from .domain import ExperimentSpec, JobType, RunMetadata, RunPurpose, RunRole
from rstock.calendars import forward_market_sessions, resolve_market_session_on_or_before
from .repository import RunRepository, utc_now
from .temporal_validation import (
    TEMPORAL_VALIDATION_RESULT,
    TemporalValidationRunner,
)
from .forward_simulation import build_forward_model_snapshot


PIPELINE_SCHEMA_VERSION = 1
PIPELINE_MANIFEST = "orchestration/pipeline.json"
CHILD_ID_POLICY_DETERMINISTIC = 1
CHILD_ID_POLICY_RESERVED = 2

SCIENTIFIC_STAGES: tuple[tuple[str, JobType, tuple[str, ...]], ...] = (
    ("walk_forward", JobType.WALK_FORWARD, ()),
    ("xgboost_calibration", JobType.XGBOOST_CALIBRATION, ("walk_forward",)),
    (
        "threshold_parameter_calibration",
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        ("walk_forward", "xgboost_calibration"),
    ),
    (
        "threshold_calibration",
        JobType.THRESHOLD_CALIBRATION,
        (
            "walk_forward",
            "xgboost_calibration",
            "threshold_parameter_calibration",
        ),
    ),
)
FORCED_SCIENTIFIC_STAGES: tuple[tuple[str, JobType, tuple[str, ...]], ...] = (
    ("walk_forward", JobType.WALK_FORWARD, ()),
    (
        "fixed_candidate_evaluation",
        JobType.FIXED_CANDIDATE_EVALUATION,
        ("walk_forward",),
    ),
)
TEMPORAL_VALIDATION_STAGE = "temporal_validation_end_to_end"
FORCED_CANDIDATE_VALIDATION_STAGE = "forced_candidate_validation_end_to_end"
HISTORICAL_FORCED_VALIDATION_CHECKPOINT = (
    "orchestration/forced_candidate_validation_backfill.json"
)
HISTORICAL_FORCED_VALIDATION_RELATION_KEY = "historical_forced_candidate_validation"
THREE_PASS_PIPELINE_VERSION = 2

REQUIRED_ARTIFACTS: dict[str, tuple[str, ...]] = {
    "walk_forward": (
        "summary.json",
        "results/qualification.csv",
        "results/run_configuration.json",
    ),
    "xgboost_calibration": (
        "results/selected_configurations.json",
        "results/sampling_manifest.json",
    ),
    "threshold_parameter_calibration": (
        "results/selected_threshold_calibration_configuration.json",
        "results/sampling_manifest.json",
    ),
    "threshold_calibration": (
        "results/selected_thresholds_by_set.json",
        "results/run_configuration.json",
    ),
    "fixed_candidate_evaluation": (
        "results/selected_thresholds_by_set.json",
        "results/holdout_metrics.csv",
        "results/run_configuration.json",
    ),
}


def _scientific_stages_for_spec(
    spec: ExperimentSpec,
) -> tuple[tuple[str, JobType, tuple[str, ...]], ...]:
    return (
        FORCED_SCIENTIFIC_STAGES
        if spec.forced_symbol_sets is not None
        else SCIENTIFIC_STAGES
    )


def _scientific_stages_for_manifest(
    manifest: dict[str, Any],
) -> tuple[tuple[str, JobType, tuple[str, ...]], ...]:
    return (
        FORCED_SCIENTIFIC_STAGES
        if manifest.get("forced_candidate_validation", False)
        else SCIENTIFIC_STAGES
    )


def _relation_key(stage_key: str) -> str:
    return f"pipeline_stage:{stage_key}"


def build_pipeline_manifest(
    repository: RunRepository,
    root_run_id: str,
    spec: ExperimentSpec,
    *,
    child_id_policy_version: int = CHILD_ID_POLICY_RESERVED,
    reserved_child_ids: dict[str, str] | None = None,
) -> dict[str, Any]:
    scientific_stages = _scientific_stages_for_spec(spec)
    if child_id_policy_version not in {
        CHILD_ID_POLICY_DETERMINISTIC,
        CHILD_ID_POLICY_RESERVED,
    }:
        raise ValueError("Politique d'ID enfant End-to-end incompatible")
    if reserved_child_ids is not None:
        child_ids = dict(reserved_child_ids)
    elif child_id_policy_version == CHILD_ID_POLICY_DETERMINISTIC:
        child_ids = {
            stage_key: repository.deterministic_child_run_id(
                root_run_id, _relation_key(stage_key)
            )
            for stage_key, _, _ in scientific_stages
        }
    else:
        child_ids = {
            stage_key: repository.generate_run_id()
            for stage_key, _, _ in scientific_stages
        }
    stage_keys = {stage_key for stage_key, _, _ in scientific_stages}
    three_pass = (
        spec.temporal_validation_enabled
        and spec.pipeline_version >= THREE_PASS_PIPELINE_VERSION
    )
    if spec.temporal_validation_enabled:
        stage_keys.add(TEMPORAL_VALIDATION_STAGE)
    if three_pass:
        stage_keys.add(FORCED_CANDIDATE_VALIDATION_STAGE)
    if reserved_child_ids is not None and set(child_ids) != stage_keys:
        raise ValueError("Réservations enfant End-to-end incomplètes")
    if reserved_child_ids is None and spec.temporal_validation_enabled:
        child_ids[TEMPORAL_VALIDATION_STAGE] = repository.generate_run_id()
    if reserved_child_ids is None and three_pass:
        child_ids[FORCED_CANDIDATE_VALIDATION_STAGE] = repository.generate_run_id()
    stages: list[dict[str, Any]] = []
    for stage_key, job_type, dependencies in scientific_stages:
        stages.append(
            {
                "stage_key": stage_key,
                "expected_job_type": job_type.value,
                "child_run_id": child_ids[stage_key],
                "expected_fingerprint": None,
                "dependency_run_ids": [child_ids[item] for item in dependencies],
                "artifact_digests": {},
            }
        )
    if spec.temporal_validation_enabled:
        stages.append(
            {
                "stage_key": TEMPORAL_VALIDATION_STAGE,
                "expected_job_type": JobType.END_TO_END.value,
                "child_run_id": child_ids[TEMPORAL_VALIDATION_STAGE],
                "expected_fingerprint": None,
                "dependency_run_ids": [child_ids["threshold_calibration"]],
                "artifact_digests": {},
            }
        )
    if three_pass:
        stages.append(
            {
                "stage_key": FORCED_CANDIDATE_VALIDATION_STAGE,
                "expected_job_type": JobType.FORCED_CANDIDATE_VALIDATION.value,
                "child_run_id": child_ids[FORCED_CANDIDATE_VALIDATION_STAGE],
                "expected_fingerprint": None,
                "dependency_run_ids": [
                    child_ids["threshold_calibration"],
                    child_ids[TEMPORAL_VALIDATION_STAGE],
                ],
                "artifact_digests": {},
            }
        )
    # Promotion is deliberately an internal pipeline step, not a JobType. Its
    # execution is introduced in phase 6; reserving it now keeps the manifest
    # shape stable and makes a later resume unambiguous.
    stages.append(
        {
            "stage_key": "promotion",
            "expected_job_type": None,
            "child_run_id": None,
            "expected_fingerprint": None,
            "dependency_run_ids": [
                child_ids[FORCED_CANDIDATE_VALIDATION_STAGE]
                if three_pass
                else child_ids[
                    "fixed_candidate_evaluation"
                    if spec.forced_symbol_sets is not None
                    else "threshold_calibration"
                ]
            ],
            "artifact_digests": {},
        }
    )
    return {
        "schema_version": PIPELINE_SCHEMA_VERSION,
        "child_id_policy_version": child_id_policy_version,
        "pipeline_version": spec.pipeline_version,
        "root_run_id": root_run_id,
        "auto_promote_candidates": spec.auto_promote_candidates,
        "temporal_validation_enabled": spec.temporal_validation_enabled,
        "forced_candidate_validation": spec.forced_symbol_sets is not None,
        "stages": stages,
    }


def load_pipeline_manifest(
    repository: RunRepository, run_id: str
) -> dict[str, Any] | None:
    path = repository.run_directory(run_id) / PIPELINE_MANIFEST
    if not path.exists():
        return None
    values = repository.read_json(run_id, PIPELINE_MANIFEST)
    validate_pipeline_manifest(values, root_run_id=run_id)
    return values


def validate_pipeline_manifest(
    manifest: dict[str, Any], *, root_run_id: str
) -> None:
    if manifest.get("schema_version") != PIPELINE_SCHEMA_VERSION:
        raise ValueError("Version du manifest End-to-end incompatible")
    if manifest.get("root_run_id") != root_run_id:
        raise ValueError("Le manifest End-to-end appartient à un autre run")
    stages = manifest.get("stages")
    if not isinstance(stages, list):
        raise ValueError("Étapes End-to-end absentes du manifest")
    scientific_stages = _scientific_stages_for_manifest(manifest)
    expected = [item[0] for item in scientific_stages]
    if manifest.get("temporal_validation_enabled", False):
        expected.append(TEMPORAL_VALIDATION_STAGE)
        if int(manifest.get("pipeline_version", 1)) >= THREE_PASS_PIPELINE_VERSION:
            expected.append(FORCED_CANDIDATE_VALIDATION_STAGE)
    expected.append("promotion")
    if [item.get("stage_key") for item in stages if isinstance(item, dict)] != expected:
        raise ValueError("Étapes End-to-end incompatibles avec le pipeline")
    policy_version = int(
        manifest.get("child_id_policy_version", CHILD_ID_POLICY_DETERMINISTIC)
    )
    if policy_version not in {
        CHILD_ID_POLICY_DETERMINISTIC,
        CHILD_ID_POLICY_RESERVED,
    }:
        raise ValueError("Politique d'ID enfant End-to-end incompatible")
    child_ids: dict[str, str] = {}
    for item, (stage_key, job_type, dependencies) in zip(
        stages, scientific_stages
    ):
        if item.get("expected_job_type") != job_type.value:
            raise ValueError(f"JobType incompatible pour l'étape {stage_key}")
        if not item.get("child_run_id"):
            raise ValueError(f"Réservation enfant absente pour l'étape {stage_key}")
        if not isinstance(item.get("dependency_run_ids"), list):
            raise ValueError(f"Dépendances invalides pour l'étape {stage_key}")
        if not isinstance(item.get("artifact_digests"), dict):
            raise ValueError(f"Digests invalides pour l'étape {stage_key}")
        child_ids[stage_key] = str(item["child_run_id"])
        if policy_version == CHILD_ID_POLICY_DETERMINISTIC and item.get(
            "child_run_id"
        ) != RunRepository.deterministic_child_run_id(
            root_run_id, _relation_key(stage_key)
        ):
            raise ValueError(f"Réservation historique invalide pour {stage_key}")
        if item["dependency_run_ids"] != [child_ids[key] for key in dependencies]:
            raise ValueError(f"Dépendances incompatibles pour l'étape {stage_key}")
    temporal = None
    if manifest.get("temporal_validation_enabled", False):
        temporal = stages[len(scientific_stages)]
        if (
            temporal.get("expected_job_type") != JobType.END_TO_END.value
            or not temporal.get("child_run_id")
            or temporal.get("dependency_run_ids")
            != [child_ids["threshold_calibration"]]
            or not isinstance(temporal.get("artifact_digests"), dict)
        ):
            raise ValueError("Réservation de validation temporelle invalide")
    three_pass = (
        manifest.get("temporal_validation_enabled", False)
        and int(manifest.get("pipeline_version", 1)) >= THREE_PASS_PIPELINE_VERSION
    )
    if three_pass:
        forced = _stage(manifest, FORCED_CANDIDATE_VALIDATION_STAGE)
        if (
            forced.get("expected_job_type") not in {
                JobType.END_TO_END.value,
                JobType.FORCED_CANDIDATE_VALIDATION.value,
            }
            or not forced.get("child_run_id")
            or forced.get("dependency_run_ids") != [
                child_ids["threshold_calibration"],
                str(temporal["child_run_id"]),
            ]
            or not isinstance(forced.get("artifact_digests"), dict)
        ):
            raise ValueError("Réservation de revalidation forcée invalide")
    promotion = stages[-1]
    if (
        promotion.get("expected_job_type") is not None
        or promotion.get("child_run_id") is not None
        or not isinstance(promotion.get("dependency_run_ids"), list)
        or not isinstance(promotion.get("artifact_digests"), dict)
    ):
        raise ValueError("Réservation de promotion End-to-end invalide")
    expected_promotion_dependency = (
        str(_stage(manifest, FORCED_CANDIDATE_VALIDATION_STAGE)["child_run_id"])
        if three_pass
        else child_ids[
            "fixed_candidate_evaluation"
            if manifest.get("forced_candidate_validation", False)
            else "threshold_calibration"
        ]
    )
    if promotion["dependency_run_ids"] != [expected_promotion_dependency]:
        raise ValueError("Dépendances de promotion End-to-end incompatibles")


def persist_or_validate_pipeline_manifest(
    repository: RunRepository, run_id: str, spec: ExperimentSpec
) -> dict[str, Any]:
    persisted = load_pipeline_manifest(repository, run_id)
    if persisted is None:
        expected = build_pipeline_manifest(repository, run_id, spec)
        (repository.run_directory(run_id) / "orchestration").mkdir(exist_ok=True)
        repository.write_json(run_id, PIPELINE_MANIFEST, expected)
        return load_pipeline_manifest(repository, run_id) or expected
    policy_version = int(
        persisted.get("child_id_policy_version", CHILD_ID_POLICY_DETERMINISTIC)
    )
    reserved_child_ids = {
        str(item["stage_key"]): str(item["child_run_id"])
        for item in persisted["stages"][:-1]
    }
    expected = build_pipeline_manifest(
        repository,
        run_id,
        spec,
        child_id_policy_version=policy_version,
        reserved_child_ids=reserved_child_ids,
    )
    immutable_keys = (
        "schema_version",
        "child_id_policy_version",
        "pipeline_version",
        "root_run_id",
        "auto_promote_candidates",
        "temporal_validation_enabled",
        "forced_candidate_validation",
    )
    if any(
        (
            persisted.get(key, False)
            if key == "forced_candidate_validation"
            else persisted.get(key)
            if key != "child_id_policy_version"
            else policy_version
        )
        != expected.get(key)
        for key in immutable_keys
    ):
        raise ValueError("Le manifest End-to-end ne correspond plus au snapshot du run")
    for actual, wanted in zip(persisted["stages"], expected["stages"], strict=True):
        for key in (
            "stage_key",
            "expected_job_type",
            "child_run_id",
            "dependency_run_ids",
        ):
            if actual.get(key) != wanted.get(key):
                raise ValueError(
                    f"Réservation End-to-end altérée pour {wanted['stage_key']}"
                )
    return persisted


def _stage(manifest: dict[str, Any], stage_key: str) -> dict[str, Any]:
    matches = [item for item in manifest["stages"] if item["stage_key"] == stage_key]
    if len(matches) != 1:
        raise ValueError(f"Étape End-to-end introuvable ou ambiguë: {stage_key}")
    return matches[0]


def _read_result_json(
    repository: RunRepository, run_id: str, filename: str
) -> dict[str, Any]:
    path = repository.run_directory(run_id) / "results" / filename
    try:
        values = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"Artefact illisible pour {run_id}: {filename}") from error
    if not isinstance(values, dict):
        raise ValueError(f"Artefact invalide pour {run_id}: {filename}")
    return values


def _walk_forward_traceability(
    repository: RunRepository, run_id: str
) -> tuple[str, str]:
    traceability = repository.summary(run_id).get("traceability")
    if not isinstance(traceability, dict):
        raise ValueError("Traçabilité du Walk-forward End-to-end absente")
    cutoff = traceability.get("prepared_market_last_date")
    digest = traceability.get("prepared_dataset_sha256")
    if cutoff is None or digest is None:
        raise ValueError("Dataset figé du Walk-forward End-to-end incomplet")
    return str(cutoff), str(digest)


def _base_child_spec(
    parent: ExperimentSpec,
    *,
    root_run_id: str,
    job_type: JobType,
) -> ExperimentSpec:
    return replace(
        parent,
        job_type=job_type,
        source_experiment_run=root_run_id,
        source_walk_forward_run=None,
        source_xgboost_calibration_run=None,
        frozen_xgboost_parameters=None,
        source_threshold_parameter_calibration_run=None,
        frozen_threshold_calibration_parameters=None,
        source_end_to_end_run=root_run_id,
        source_threshold_calibration_run=None,
        auto_promote_candidates=False,
        temporal_validation_enabled=False,
        historical_data_cutoff=(
            parent.resolved_market_session_cutoff or parent.historical_data_cutoff
        ),
        requested_historical_cutoff=parent.requested_historical_cutoff,
        resolved_market_session_cutoff=parent.resolved_market_session_cutoff,
        source_prepared_dataset_sha256=None,
        prepared_dataset_digest_required=False,
        combination_plan_version=None,
        combination_plan_sha256=None,
        combination_range_start=None,
        combination_range_stop=None,
    )


def build_stage_spec(
    repository: RunRepository,
    root_run_id: str,
    parent: ExperimentSpec,
    stage_key: str,
    manifest: dict[str, Any],
) -> ExperimentSpec:
    stage = _stage(manifest, stage_key)
    job_type = JobType(str(stage["expected_job_type"]))
    child = _base_child_spec(parent, root_run_id=root_run_id, job_type=job_type)
    if stage_key == "walk_forward":
        return child

    walk_forward_id = str(_stage(manifest, "walk_forward")["child_run_id"])
    cutoff, dataset_digest = _walk_forward_traceability(
        repository, walk_forward_id
    )
    child = replace(
        child,
        source_walk_forward_run=walk_forward_id,
        historical_data_cutoff=cutoff,
        source_prepared_dataset_sha256=dataset_digest,
        prepared_dataset_digest_required=True,
    )
    if stage_key == "fixed_candidate_evaluation":
        return replace(
            child,
            source_xgboost_calibration_run=parent.source_xgboost_calibration_run,
            frozen_xgboost_parameters=parent.frozen_xgboost_parameters,
            source_threshold_parameter_calibration_run=(
                parent.source_threshold_parameter_calibration_run
            ),
            frozen_threshold_calibration_parameters=(
                parent.frozen_threshold_calibration_parameters
            ),
            source_threshold_calibration_run=(
                parent.source_threshold_calibration_run
            ),
            frozen_selected_thresholds_by_set=(
                parent.frozen_selected_thresholds_by_set
            ),
            forced_candidate_identities=parent.forced_candidate_identities,
        )
    if stage_key == "xgboost_calibration":
        return child

    xgboost_id = str(_stage(manifest, "xgboost_calibration")["child_run_id"])
    selected = _read_result_json(
        repository, xgboost_id, "selected_configurations.json"
    )
    frozen_xgboost = selected_xgboost_parameters(selected)
    child = replace(
        child,
        source_xgboost_calibration_run=xgboost_id,
        frozen_xgboost_parameters=frozen_xgboost,
    )
    if stage_key == "threshold_parameter_calibration":
        return child

    threshold_parameter_id = str(
        _stage(manifest, "threshold_parameter_calibration")["child_run_id"]
    )
    selection = _read_result_json(
        repository,
        threshold_parameter_id,
        "selected_threshold_calibration_configuration.json",
    )
    parameters = selection.get("parameters")
    if not isinstance(parameters, dict):
        raise ValueError("Paramètres de calibration des seuils absents")
    return replace(
        child,
        source_threshold_parameter_calibration_run=threshold_parameter_id,
        frozen_threshold_calibration_parameters=dict(parameters),
    )


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def artifact_digests(
    repository: RunRepository, run_id: str, stage_key: str
) -> dict[str, str]:
    run_directory = repository.run_directory(run_id)
    values: dict[str, str] = {}
    for filename in REQUIRED_ARTIFACTS[stage_key]:
        path = run_directory / filename
        if not path.is_file():
            raise ValueError(
                f"Artefact requis absent pour {stage_key}/{run_id}: {filename}"
            )
        values[filename] = _sha256(path)
    return values


def _persist_stage_values(
    repository: RunRepository,
    root_run_id: str,
    stage_key: str,
    **values: object,
) -> dict[str, Any]:
    # Never overwrite a potentially newer in-memory manifest. The persisted
    # manifest is the authority for every reconciliation write.
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent pendant la réconciliation")
    stage = _stage(manifest, stage_key)
    stage.update(values)
    repository.write_json(root_run_id, PIPELINE_MANIFEST, manifest)
    return manifest


def _validate_or_persist_artifacts(
    repository: RunRepository,
    root_run_id: str,
    stage_key: str,
) -> dict[str, str]:
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent")
    stage = _stage(manifest, stage_key)
    actual = artifact_digests(
        repository, str(stage["child_run_id"]), stage_key
    )
    persisted = stage.get("artifact_digests") or {}
    if persisted and persisted != actual:
        raise ValueError(
            f"Les artefacts de l'étape {stage_key} ont changé depuis leur validation"
        )
    if not persisted:
        _persist_stage_values(
            repository, root_run_id, stage_key, artifact_digests=actual
        )
    return actual


def _materialize_stage(
    repository: RunRepository,
    root_run_id: str,
    parent: ExperimentSpec,
    stage_key: str,
    stage_index: int,
) -> tuple[str, ExperimentSpec]:
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent")
    specification = build_stage_spec(
        repository, root_run_id, parent, stage_key, manifest
    )
    stage = _stage(manifest, stage_key)
    child_run_id = str(stage["child_run_id"])
    expected = stage.get("expected_fingerprint")
    if expected is None:
        _persist_stage_values(
            repository,
            root_run_id,
            stage_key,
            expected_fingerprint=specification.fingerprint,
        )
    elif expected != specification.fingerprint:
        raise ValueError(
            f"Fingerprint recalculé incompatible pour l'étape {stage_key}"
        )

    directory = repository.run_directory(child_run_id)
    if not directory.exists():
        repository.create(
            specification,
            run_id=child_run_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE,
                visible_in_history=True,
                parent_run_id=root_run_id,
                relation_key=_relation_key(stage_key),
                relation_type="pipeline_stage",
                stage_key=stage_key,
                stage_index=stage_index,
            ),
        )
    else:
        metadata = repository.run_metadata(child_run_id)
        if (
            metadata.parent_run_id != root_run_id
            or metadata.relation_key != _relation_key(stage_key)
            or metadata.stage_key != stage_key
        ):
            raise ValueError(f"Relation enfant incompatible pour {stage_key}")
        status = repository.status(child_run_id)
        if status.get("job_type") != specification.job_type.value:
            raise ValueError(f"JobType enfant incompatible pour {stage_key}")
        if repository.configuration_fingerprint(child_run_id) != specification.fingerprint:
            raise ValueError(f"Fingerprint enfant incompatible pour {stage_key}")
    return child_run_id, specification


def _temporal_validation_spec(parent: ExperimentSpec) -> ExperimentSpec:
    return replace(
        parent,
        config=replace(parent.config, walk_forward_end_offset_sessions=63),
        temporal_validation_enabled=False,
        auto_promote_candidates=False,
        run_description="Validation temporelle offset 63",
    )


def _materialize_temporal_validation(
    repository: RunRepository, root_run_id: str, parent: ExperimentSpec
) -> tuple[str, ExperimentSpec]:
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent")
    stage = _stage(manifest, TEMPORAL_VALIDATION_STAGE)
    child_run_id = str(stage["child_run_id"])
    specification = _temporal_validation_spec(parent)
    expected = stage.get("expected_fingerprint")
    if expected is None:
        _persist_stage_values(
            repository,
            root_run_id,
            TEMPORAL_VALIDATION_STAGE,
            expected_fingerprint=specification.fingerprint,
        )
    elif expected != specification.fingerprint:
        raise ValueError("Fingerprint de validation temporelle incompatible")
    directory = repository.run_directory(child_run_id)
    if not directory.exists():
        repository.create(
            specification,
            run_id=child_run_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_PARENT,
                run_purpose=RunPurpose.TEMPORAL_VALIDATION,
                visible_in_history=True,
                parent_run_id=root_run_id,
                relation_key=TEMPORAL_VALIDATION_STAGE,
                relation_type="temporal_validation_end_to_end",
                stage_key=TEMPORAL_VALIDATION_STAGE,
                stage_index=len(SCIENTIFIC_STAGES),
                reference_run_id=root_run_id,
            ),
        )
    else:
        metadata = repository.run_metadata(child_run_id)
        if (
            metadata.parent_run_id != root_run_id
            or metadata.reference_run_id != root_run_id
            or metadata.run_purpose is not RunPurpose.TEMPORAL_VALIDATION
        ):
            raise ValueError("Relation de validation temporelle incompatible")
        if repository.configuration_fingerprint(child_run_id) != specification.fingerprint:
            raise ValueError("Snapshot de validation temporelle incompatible")
    return child_run_id, specification


def _parse_candidate_set(value: str) -> tuple[str, ...]:
    try:
        parsed = json.loads(value)
    except json.JSONDecodeError:
        if "<-" not in value:
            raise ValueError(f"Identité de candidat invalide: {value}")
        target, predictors = value.split("<-", 1)
        parsed = [target, *predictors.split("+")]
    if not isinstance(parsed, list) or len(parsed) < 2:
        raise ValueError(f"Identité de candidat invalide: {value}")
    symbols = tuple(str(symbol) for symbol in parsed)
    if any(not symbol for symbol in symbols) or len(set(symbols)) != len(symbols):
        raise ValueError(f"Identité de candidat invalide: {value}")
    return symbols


def _forced_candidate_spec(
    repository: RunRepository,
    root_run_id: str,
    parent: ExperimentSpec,
    manifest: dict[str, Any],
    *,
    validation_spec: ExperimentSpec | None = None,
    historical_backfill: bool = False,
) -> ExperimentSpec:
    walk_forward_id = str(_stage(manifest, "walk_forward")["child_run_id"])
    xgboost_id = str(_stage(manifest, "xgboost_calibration")["child_run_id"])
    threshold_parameter_id = str(
        _stage(manifest, "threshold_parameter_calibration")["child_run_id"]
    )
    threshold_id = str(_stage(manifest, "threshold_calibration")["child_run_id"])
    _, guidance, candidate_ids = AutoPromotionRunner(
        repository,
        root_run_id=root_run_id,
        walk_forward_run_id=walk_forward_id,
        xgboost_calibration_run_id=xgboost_id,
        threshold_calibration_run_id=threshold_id,
    ).source_candidates()
    direction_by_set = {
        str(row["Combinaison"]): str(row["Direction"])
        for _, row in guidance.iterrows()
        if str(row.get("Statut promotion")) == "Candidat"
    }
    reference_xgboost = _read_result_json(
        repository, xgboost_id, "selected_configurations.json"
    )
    frozen_xgboost = selected_xgboost_parameters(reference_xgboost)
    threshold_parameter_selection = _read_result_json(
        repository,
        threshold_parameter_id,
        "selected_threshold_calibration_configuration.json",
    )
    frozen_threshold_parameters = threshold_parameter_selection.get("parameters")
    if not isinstance(frozen_threshold_parameters, dict):
        raise ValueError("Paramètres de calibration des seuils de référence absents")
    reference_thresholds = _read_result_json(
        repository, threshold_id, "selected_thresholds_by_set.json"
    )
    forced_sets = tuple(_parse_candidate_set(value) for value in candidate_ids)
    canonical_ids = tuple(
        json.dumps(list(symbol_set), ensure_ascii=False, separators=(",", ":"))
        for symbol_set in forced_sets
    )
    missing_directions = [
        source_id for source_id in candidate_ids if source_id not in direction_by_set
    ]
    if missing_directions:
        raise ValueError(
            "Direction canonique absente pour les candidats de référence: "
            + ", ".join(missing_directions)
        )
    identities = tuple(
        (canonical_id, direction_by_set[source_id])
        for source_id, canonical_id in zip(
            candidate_ids, canonical_ids, strict=True
        )
    )
    frozen_thresholds: dict[str, dict[str, dict[str, object]]] = {}
    for source_id, canonical_id, (_, direction) in zip(
        candidate_ids, canonical_ids, identities, strict=True
    ):
        selection = reference_thresholds.get(source_id)
        if not isinstance(selection, dict):
            raise ValueError(
                f"Seuils de référence absents pour le candidat {source_id}"
            )
        directional = selection.get(direction)
        if (
            not isinstance(directional, dict)
            or directional.get("status") != "selected"
            or directional.get("threshold") is None
        ):
            raise ValueError(
                f"Seuil de référence absent pour {source_id}/{direction}"
            )
        frozen_thresholds[canonical_id] = {
            str(name): dict(value)
            for name, value in selection.items()
            if isinstance(value, dict)
        }
    targets = tuple(dict.fromkeys(symbol_set[0] for symbol_set in forced_sets))
    all_symbols = tuple(
        dict.fromkeys(symbol for symbol_set in forced_sets for symbol in symbol_set)
    )
    context = tuple(symbol for symbol in all_symbols if symbol not in set(targets))
    if not all_symbols:
        all_symbols = parent.symbols
        targets = parent.target_symbols
        context = parent.context_symbols
    return replace(
        parent,
        job_type=JobType.FORCED_CANDIDATE_VALIDATION,
        config=(
            validation_spec.config
            if validation_spec is not None
            else replace(parent.config, walk_forward_end_offset_sessions=63)
        ),
        symbols=all_symbols,
        target_symbols=targets,
        context_symbols=context,
        predictor_symbols=all_symbols,
        forced_symbol_sets=forced_sets,
        forced_candidate_identities=identities,
        source_xgboost_calibration_run=xgboost_id,
        frozen_xgboost_parameters=frozen_xgboost,
        source_threshold_parameter_calibration_run=threshold_parameter_id,
        frozen_threshold_calibration_parameters=dict(frozen_threshold_parameters),
        source_threshold_calibration_run=threshold_id,
        frozen_selected_thresholds_by_set=frozen_thresholds,
        temporal_validation_enabled=False,
        auto_promote_candidates=False,
        pipeline_version=1,
        historical_forced_validation_backfill=historical_backfill,
        run_description="Revalidation des candidats de référence",
    )


def _materialize_forced_candidate_validation(
    repository: RunRepository, root_run_id: str, parent: ExperimentSpec
) -> tuple[str, ExperimentSpec]:
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent")
    temporal_id = str(_stage(manifest, TEMPORAL_VALIDATION_STAGE)["child_run_id"])
    stage = _stage(manifest, FORCED_CANDIDATE_VALIDATION_STAGE)
    child_run_id = str(stage["child_run_id"])
    validation_spec = repository.load_spec(temporal_id)
    specification = _forced_candidate_spec(
        repository, root_run_id, parent, manifest, validation_spec=validation_spec
    )
    if stage.get("expected_job_type") == JobType.END_TO_END.value:
        # Historical V2 manifests keep their original immutable contract.
        specification = replace(specification, job_type=JobType.END_TO_END)
    expected = stage.get("expected_fingerprint")
    if expected is None:
        _persist_stage_values(
            repository,
            root_run_id,
            FORCED_CANDIDATE_VALIDATION_STAGE,
            expected_fingerprint=specification.fingerprint,
        )
    elif expected != specification.fingerprint:
        raise ValueError("Fingerprint de revalidation forcée incompatible")
    directory = repository.run_directory(child_run_id)
    if not directory.exists():
        repository.create(
            specification,
            run_id=child_run_id,
            metadata=RunMetadata(
                run_role=RunRole.FORCED_CANDIDATE_VALIDATION,
                run_purpose=RunPurpose.FORCED_CANDIDATE_VALIDATION,
                visible_in_history=True,
                parent_run_id=root_run_id,
                relation_key=FORCED_CANDIDATE_VALIDATION_STAGE,
                relation_type="forced_candidate_validation",
                stage_key=FORCED_CANDIDATE_VALIDATION_STAGE,
                stage_index=len(SCIENTIFIC_STAGES) + 1,
                reference_run_id=root_run_id,
                validation_run_id=temporal_id,
            ),
        )
    else:
        metadata = repository.run_metadata(child_run_id)
        if (
            metadata.parent_run_id != root_run_id
            or metadata.reference_run_id != root_run_id
            or metadata.validation_run_id != temporal_id
            or metadata.run_purpose is not RunPurpose.FORCED_CANDIDATE_VALIDATION
        ):
            raise ValueError("Relation de revalidation forcée incompatible")
        if repository.configuration_fingerprint(child_run_id) != specification.fingerprint:
            raise ValueError("Snapshot de revalidation forcée incompatible")
    return child_run_id, specification


def historical_forced_validation_state(
    repository: RunRepository, root_run_id: str
) -> dict[str, object]:
    """Return the dedicated V1 backfill reservation without changing old manifests."""

    try:
        value = repository.read_json(root_run_id, HISTORICAL_FORCED_VALIDATION_CHECKPOINT)
    except FileNotFoundError:
        return {"exists": False}
    if not isinstance(value, dict) or value.get("parent_run_id") != root_run_id:
        raise ValueError("Checkpoint de revalidation historique incompatible")
    child_run_id = value.get("active_child_run_id", value.get("child_run_id"))
    if not isinstance(child_run_id, str) or not child_run_id:
        raise ValueError("Checkpoint de revalidation historique incomplet")
    status = (
        repository.status(child_run_id).get("status")
        if repository.run_directory(child_run_id).exists()
        else "reserved"
    )
    legacy_failed = False
    if repository.run_directory(child_run_id).exists():
        legacy_failed = (
            repository.load_spec(child_run_id).job_type is JobType.END_TO_END
            and status == "failed"
        )
    return {
        "exists": not legacy_failed,
        "child_run_id": child_run_id,
        "created_at": value.get("created_at"),
        "status": status,
        "legacy_failed_child_run_id": child_run_id if legacy_failed else None,
    }


def _historical_forced_validation_spec(
    repository: RunRepository, root_run_id: str
) -> tuple[ExperimentSpec, str]:
    """Preflight a historical parent before reserving its immutable pass-three child."""

    if repository.status(root_run_id).get("status") != "completed":
        raise ValueError("Le run End-to-end de référence doit être terminé")
    parent = repository.load_spec(root_run_id)
    if parent.job_type is not JobType.END_TO_END:
        raise ValueError("La revalidation historique exige un parent End-to-end")
    manifest = load_pipeline_manifest(repository, root_run_id)
    if manifest is None:
        raise ValueError("Manifest End-to-end absent")
    temporal = _stage(manifest, TEMPORAL_VALIDATION_STAGE)
    temporal_id = str(temporal.get("child_run_id") or "")
    if not temporal_id or repository.status(temporal_id).get("status") != "completed":
        raise ValueError("Le child validation offset 63 doit être terminé")
    validation_spec = repository.load_spec(temporal_id)
    if validation_spec.config.walk_forward_end_offset_sessions != 63:
        raise ValueError("Le child de validation doit utiliser l'offset 63")
    for stage_key, _job_type, _dependencies in SCIENTIFIC_STAGES:
        stage = _stage(manifest, stage_key)
        child_id = str(stage.get("child_run_id") or "")
        if not child_id or repository.status(child_id).get("status") != "completed":
            raise ValueError(f"Étape de référence incomplète: {stage_key}")
        artifact_digests(repository, child_id, stage_key)
    return (
        _forced_candidate_spec(
            repository,
            root_run_id,
            parent,
            manifest,
            validation_spec=validation_spec,
            historical_backfill=True,
        ),
        temporal_id,
    )


def materialize_historical_forced_candidate_validation(
    repository: RunRepository, root_run_id: str
) -> tuple[str, ExperimentSpec, bool]:
    """Reserve one diagnostic pass-three child for a completed V1/V2 parent."""

    state = historical_forced_validation_state(repository, root_run_id)
    if state["exists"]:
        child_run_id = str(state["child_run_id"])
        if repository.run_directory(child_run_id).exists():
            specification = repository.load_spec(child_run_id)
            if not specification.historical_forced_validation_backfill:
                raise ValueError("Child de revalidation historique incompatible")
            return child_run_id, specification, False
        specification, temporal_id = _historical_forced_validation_spec(
            repository, root_run_id
        )
        checkpoint = repository.read_json(
            root_run_id, HISTORICAL_FORCED_VALIDATION_CHECKPOINT
        )
        if checkpoint.get("expected_fingerprint") != specification.fingerprint:
            raise ValueError("Checkpoint de revalidation historique incompatible")
        repository.create(
            specification,
            run_id=child_run_id,
            metadata=RunMetadata(
                run_role=RunRole.FORCED_CANDIDATE_VALIDATION,
                run_purpose=RunPurpose.FORCED_CANDIDATE_VALIDATION,
                visible_in_history=True,
                parent_run_id=root_run_id,
                relation_key=HISTORICAL_FORCED_VALIDATION_RELATION_KEY,
                relation_type="historical_forced_candidate_validation",
                stage_key=HISTORICAL_FORCED_VALIDATION_RELATION_KEY,
                stage_index=len(SCIENTIFIC_STAGES) + 1,
                reference_run_id=root_run_id,
                validation_run_id=temporal_id,
            ),
        )
        return child_run_id, specification, True
    specification, temporal_id = _historical_forced_validation_spec(
        repository, root_run_id
    )
    child_run_id = repository.generate_run_id()
    previous_child_run_ids = []
    try:
        previous = repository.read_json(
            root_run_id, HISTORICAL_FORCED_VALIDATION_CHECKPOINT
        )
    except FileNotFoundError:
        previous = {}
    previous_id = previous.get("active_child_run_id", previous.get("child_run_id"))
    if isinstance(previous_id, str) and previous_id:
        previous_child_run_ids.append(previous_id)
    previous_child_run_ids.extend(
        str(value)
        for value in previous.get("previous_child_run_ids", [])
        if str(value) not in previous_child_run_ids
    )
    # Reserve atomically before materialization, so retries never create a duplicate.
    (repository.run_directory(root_run_id) / "orchestration").mkdir(exist_ok=True)
    repository.write_json(
        root_run_id,
        HISTORICAL_FORCED_VALIDATION_CHECKPOINT,
        {
            "schema_version": 2,
            "parent_run_id": root_run_id,
            "reference_run_id": root_run_id,
            "validation_run_id": temporal_id,
            "child_run_id": child_run_id,
            "active_child_run_id": child_run_id,
            "previous_child_run_ids": previous_child_run_ids,
            "expected_fingerprint": specification.fingerprint,
            "historical_backfill": True,
            "created_at": utc_now(),
        },
    )
    repository.create(
        specification,
        run_id=child_run_id,
        metadata=RunMetadata(
            run_role=RunRole.FORCED_CANDIDATE_VALIDATION,
            run_purpose=RunPurpose.FORCED_CANDIDATE_VALIDATION,
            visible_in_history=True,
            parent_run_id=root_run_id,
            relation_key=HISTORICAL_FORCED_VALIDATION_RELATION_KEY,
            relation_type="historical_forced_candidate_validation",
            stage_key=HISTORICAL_FORCED_VALIDATION_RELATION_KEY,
            stage_index=len(SCIENTIFIC_STAGES) + 1,
            reference_run_id=root_run_id,
            validation_run_id=temporal_id,
        ),
    )
    return child_run_id, specification, True


def run_end_to_end(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
    *,
    execute_reserved_child: Callable[[RunRepository, str], None],
    phase_callback: Callable[..., None],
) -> dict[str, Any]:
    repository = RunRepository(output.parent.parent)
    root_run_id = output.parent.name
    if spec.forced_symbol_sets == ():
        output.mkdir(parents=True, exist_ok=True)
        return {
            "schema_version": PIPELINE_SCHEMA_VERSION,
            "pipeline_version": spec.pipeline_version,
            "root_run_id": root_run_id,
            "forced_candidate_validation": True,
            "candidate_count": 0,
            "stages": [],
            "promotion": {"requested": False, "executed": False},
        }
    scientific_stages = _scientific_stages_for_spec(spec)
    if spec.temporal_validation_enabled:
        metadata = repository.run_metadata(root_run_id)
        if (
            metadata.run_role is not RunRole.PIPELINE_PARENT
            or metadata.run_purpose is not RunPurpose.REFERENCE
        ):
            raise ValueError(
                "Un End-to-end avec validation temporelle doit utiliser un parent "
                "pipeline_parent/reference."
            )
    manifest = persist_or_validate_pipeline_manifest(repository, root_run_id, spec)
    stage_count = (
        len(scientific_stages)
        + 2 * int(spec.temporal_validation_enabled)
        + int(
            spec.temporal_validation_enabled
            and spec.pipeline_version >= THREE_PASS_PIPELINE_VERSION
        )
        + int(spec.auto_promote_candidates)
    )

    completed: list[dict[str, Any]] = []
    for stage_index, (stage_key, job_type, dependencies) in enumerate(
        scientific_stages
    ):
        check_cancellation(cancellation_check)
        for dependency in dependencies:
            dependency_stage = _stage(manifest, dependency)
            dependency_id = str(dependency_stage["child_run_id"])
            status = repository.status(dependency_id)
            if status.get("status") != "completed":
                raise RuntimeError(
                    f"Dépendance incomplète pour {stage_key}: {dependency_id}"
                )
            _validate_or_persist_artifacts(
                repository, root_run_id, dependency
            )

        child_run_id, child_spec = _materialize_stage(
            repository, root_run_id, spec, stage_key, stage_index
        )
        phase_callback(
            progress_callback,
            stage_key,
            "started",
            child_run_id=child_run_id,
            job_type=job_type.value,
            stage_index=stage_index + 1,
            stage_count=stage_count,
        )
        try:
            execute_reserved_child(repository, child_run_id)
        except RuntimeError:
            check_cancellation(cancellation_check)
            raise
        check_cancellation(cancellation_check)
        digests = _validate_or_persist_artifacts(
            repository, root_run_id, stage_key
        )
        phase_callback(
            progress_callback,
            stage_key,
            "completed",
            child_run_id=child_run_id,
            job_type=child_spec.job_type.value,
            artifact_count=len(digests),
            stage_index=stage_index + 1,
            stage_count=stage_count,
        )
        completed.append(
            {
                "stage_key": stage_key,
                "job_type": job_type.value,
                "child_run_id": child_run_id,
                "artifact_digests": digests,
            }
        )
        manifest = load_pipeline_manifest(repository, root_run_id) or manifest

    # A historical point-in-time run becomes reusable only after its candidates
    # have been frozen.  This belongs to the completed scientific parent, not
    # to any optional forward child.
    forward_snapshot = None
    if spec.historical_data_cutoff is not None:
        forward_snapshot = build_forward_model_snapshot(
            repository, root_run_id, spec, result_directory=output,
            cancellation_check=cancellation_check,
        )

    # Forward evaluation is deliberately best-effort.  It is a separate child:
    # an unavailable future period cannot retroactively fail the completed
    # discovery pipeline or its immutable model snapshot.
    forward_child: dict[str, object] | None = None
    if (
        spec.forward_simulation_enabled
        and forward_snapshot is not None
        and int(forward_snapshot.get("candidate_count", 0)) > 0
    ):
        cutoff = pd.Timestamp(
            forward_snapshot["resolved_market_session_cutoff"]
        ).normalize()
        try:
            if spec.forward_simulation_mode == "custom_end_date":
                if spec.forward_simulation_end_date is None:
                    raise ValueError("Forward custom end date is required")
                end = resolve_market_session_on_or_before(
                    spec.forward_simulation_end_date, spec.calendar
                )
                start = forward_market_sessions(cutoff, spec.calendar, 1)[0]
            else:
                count = 126 if spec.forward_simulation_mode == "126_sessions" else 63
                sessions = forward_market_sessions(cutoff, spec.calendar, count)
                start, end = sessions[0], sessions[-1]
            if end <= cutoff:
                raise ValueError("Forward end must follow the historical cutoff")
            child_spec = replace(
                spec, job_type=JobType.FORWARD_SIMULATION,
                source_end_to_end_run=root_run_id,
                source_forward_model_snapshot_sha256=forward_snapshot.get("snapshot_sha256"),
                forward_simulation_start_date=start.date().isoformat(),
                forward_simulation_end_date=end.date().isoformat(),
                forward_simulation_enabled=False,
                temporal_validation_enabled=False,
                auto_promote_candidates=False,
                run_description="Forward Simulation automatique",
            )
            forward_id = repository.generate_run_id()
            repository.create(child_spec, run_id=forward_id, metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE, parent_run_id=root_run_id,
                relation_key="forward_simulation", relation_type="forward_simulation",
                stage_key="forward_simulation", visible_in_history=True,
            ))
            forward_child = {"child_run_id": forward_id, "status": "pending"}
        except Exception as error:
            forward_child = {"status": "not_started", "error": str(error)}
    elif spec.forward_simulation_enabled and forward_snapshot is not None:
        forward_child = {"status": "skipped_no_models"}

    if spec.temporal_validation_enabled:
        check_cancellation(cancellation_check)
        child_run_id, child_spec = _materialize_temporal_validation(
            repository, root_run_id, spec
        )
        phase_callback(
            progress_callback,
            TEMPORAL_VALIDATION_STAGE,
            "started",
            child_run_id=child_run_id,
            job_type=child_spec.job_type.value,
            stage_index=len(scientific_stages) + 1,
            stage_count=stage_count,
        )
        execute_reserved_child(repository, child_run_id)
        check_cancellation(cancellation_check)
        phase_callback(
            progress_callback,
            TEMPORAL_VALIDATION_STAGE,
            "completed",
            child_run_id=child_run_id,
            job_type=child_spec.job_type.value,
            stage_index=len(scientific_stages) + 1,
            stage_count=stage_count,
        )
        completed.append(
            {
                "stage_key": TEMPORAL_VALIDATION_STAGE,
                "job_type": child_spec.job_type.value,
                "child_run_id": child_run_id,
                "artifact_digests": {},
            }
        )
        manifest = load_pipeline_manifest(repository, root_run_id) or manifest

    temporal_comparison: dict[str, Any] | None = None
    if spec.temporal_validation_enabled:
        check_cancellation(cancellation_check)
        temporal_id = str(_stage(manifest, TEMPORAL_VALIDATION_STAGE)["child_run_id"])
        phase_callback(
            progress_callback,
            "temporal_validation_comparison",
            "started",
            child_run_id=temporal_id,
            stage_index=len(scientific_stages) + 2,
            stage_count=stage_count,
        )
        temporal_comparison = TemporalValidationRunner(
            repository,
            root_run_id=root_run_id,
            validation_run_id=temporal_id,
            result_output=output / "temporal_validation_comparison.json",
        ).execute()
        phase_callback(
            progress_callback,
            "temporal_validation_comparison",
            "completed",
            child_run_id=temporal_id,
            stage_index=len(scientific_stages) + 2,
            stage_count=stage_count,
            final_status=temporal_comparison["final_status"],
        )

    forced_child_id: str | None = None
    if (
        spec.temporal_validation_enabled
        and spec.pipeline_version >= THREE_PASS_PIPELINE_VERSION
    ):
        check_cancellation(cancellation_check)
        forced_child_id, forced_spec = _materialize_forced_candidate_validation(
            repository, root_run_id, spec
        )
        forced_stage_index = len(scientific_stages) + 3
        phase_callback(
            progress_callback,
            FORCED_CANDIDATE_VALIDATION_STAGE,
            "started",
            child_run_id=forced_child_id,
            job_type=forced_spec.job_type.value,
            stage_index=forced_stage_index,
            stage_count=stage_count,
            candidates=len(forced_spec.forced_symbol_sets or ()),
        )
        execute_reserved_child(repository, forced_child_id)
        check_cancellation(cancellation_check)
        forced_summary = repository.run_directory(forced_child_id) / "summary.json"
        forced_digests = {"summary.json": _sha256(forced_summary)}
        persisted = _stage(
            load_pipeline_manifest(repository, root_run_id) or manifest,
            FORCED_CANDIDATE_VALIDATION_STAGE,
        ).get("artifact_digests") or {}
        if persisted and persisted != forced_digests:
            raise ValueError("Les artefacts de revalidation forcée ont changé")
        if not persisted:
            _persist_stage_values(
                repository,
                root_run_id,
                FORCED_CANDIDATE_VALIDATION_STAGE,
                artifact_digests=forced_digests,
            )
        phase_callback(
            progress_callback,
            FORCED_CANDIDATE_VALIDATION_STAGE,
            "completed",
            child_run_id=forced_child_id,
            job_type=forced_spec.job_type.value,
            stage_index=forced_stage_index,
            stage_count=stage_count,
            candidates=len(forced_spec.forced_symbol_sets or ()),
        )
        completed.append(
            {
                "stage_key": FORCED_CANDIDATE_VALIDATION_STAGE,
                "job_type": forced_spec.job_type.value,
                "child_run_id": forced_child_id,
                "artifact_digests": forced_digests,
            }
        )
        manifest = load_pipeline_manifest(repository, root_run_id) or manifest

    promotion_summary: dict[str, Any] = {
        "requested": spec.auto_promote_candidates,
        "executed": False,
    }
    promotion_allowed = (
        not spec.temporal_validation_enabled
        or temporal_comparison is not None
        and temporal_comparison.get("final_status") == "passed"
    )
    if spec.auto_promote_candidates and not promotion_allowed:
        promotion_summary["reason"] = "temporal_validation_not_passed"
        promotion_summary["temporal_validation_status"] = temporal_comparison.get(
            "final_status"
        ) if temporal_comparison is not None else None
    if spec.auto_promote_candidates and promotion_allowed:
        check_cancellation(cancellation_check)
        promotion_manifest = manifest
        if forced_child_id is not None:
            from .forced_candidate_validation import load_forced_validation_manifest

            promotion_manifest = load_forced_validation_manifest(
                repository, forced_child_id
            )
            if promotion_manifest is None:
                raise ValueError("Manifest de revalidation forcée absent")
        walk_forward_id = str(_stage(promotion_manifest, "walk_forward")["child_run_id"])
        if forced_child_id is not None:
            xgboost_id = str(
                _stage(manifest, "xgboost_calibration")["child_run_id"]
            )
            threshold_id = str(
                _stage(
                    promotion_manifest, "fixed_candidate_evaluation"
                )["child_run_id"]
            )
        else:
            xgboost_id = str(
                _stage(promotion_manifest, "xgboost_calibration")["child_run_id"]
            )
            threshold_id = str(
                _stage(promotion_manifest, "threshold_calibration")["child_run_id"]
            )
        runner = AutoPromotionRunner(
            repository,
            root_run_id=root_run_id,
            walk_forward_run_id=walk_forward_id,
            xgboost_calibration_run_id=xgboost_id,
            threshold_calibration_run_id=threshold_id,
            promotion_provenance=(
                {
                    "reference_run_id": root_run_id,
                    "temporal_validation_run_id": str(
                        _stage(manifest, TEMPORAL_VALIDATION_STAGE)["child_run_id"]
                    ),
                    "forced_candidate_validation_run_id": forced_child_id,
                    "promotion_policy_version": 1,
                }
                if forced_child_id is not None
                else None
            ),
        )
        promotion_state = runner.prepare()
        manifest = load_pipeline_manifest(repository, root_run_id) or manifest
        promotion_stage = _stage(manifest, "promotion")
        expected = promotion_stage.get("expected_fingerprint")
        if expected is None:
            _persist_stage_values(
                repository,
                root_run_id,
                "promotion",
                expected_fingerprint=promotion_state["plan_sha256"],
            )
        elif expected != promotion_state["plan_sha256"]:
            raise ValueError("Fingerprint du plan de promotion incompatible")
        persisted_digests = promotion_stage.get("artifact_digests") or {}
        if persisted_digests:
            actual = _sha256(runner.checkpoint_path)
            if persisted_digests != {PROMOTION_CHECKPOINT: actual}:
                raise ValueError("Le checkpoint de promotion a changé")

        phase_callback(
            progress_callback,
            "promotion",
            "started",
            stage_index=stage_count,
            stage_count=stage_count,
            candidates=promotion_state["candidate_count"],
        )
        promotion_state = runner.execute(
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
        promotion_digest = _sha256(runner.checkpoint_path)
        manifest = load_pipeline_manifest(repository, root_run_id) or manifest
        promotion_stage = _stage(manifest, "promotion")
        persisted_digests = promotion_stage.get("artifact_digests") or {}
        actual_digests = {PROMOTION_CHECKPOINT: promotion_digest}
        if persisted_digests and persisted_digests != actual_digests:
            raise ValueError("Le checkpoint de promotion a changé")
        if not persisted_digests:
            _persist_stage_values(
                repository,
                root_run_id,
                "promotion",
                artifact_digests=actual_digests,
            )
        phase_callback(
            progress_callback,
            "promotion",
            "completed",
            stage_index=stage_count,
            stage_count=stage_count,
            candidates=promotion_state["candidate_count"],
            created=promotion_state["created_count"],
            reused=promotion_state["reused_count"],
        )
        promotion_summary = {
            "requested": True,
            "executed": True,
            "plan_sha256": promotion_state["plan_sha256"],
            "candidate_count": promotion_state["candidate_count"],
            "created_count": promotion_state["created_count"],
            "reused_count": promotion_state["reused_count"],
            "models": [
                {
                    "set_name": item["set_name"],
                    "model_id": item["model_id"],
                    "created": item["created"],
                }
                for item in promotion_state["candidates"]
            ],
        }

    output.mkdir(parents=True, exist_ok=True)
    summary = {
        "schema_version": PIPELINE_SCHEMA_VERSION,
        "pipeline_version": spec.pipeline_version,
        "root_run_id": root_run_id,
        "walk_forward_protocol": (
            f"WF glissante {spec.config.walk_forward_train_size} · "
            f"test {spec.config.walk_forward_test_size} · "
            f"step {spec.config.walk_forward_step_size}"
            if spec.config.walk_forward_window_mode == "rolling"
            else
            f"WF expansive · train min {spec.config.walk_forward_min_train_size} · "
            f"test {spec.config.walk_forward_test_size} · "
            f"step {spec.config.walk_forward_step_size}"
        ),
        "auto_promote_candidates": spec.auto_promote_candidates,
        "stages": completed,
        "promotion": promotion_summary,
        "temporal_validation": temporal_comparison,
        "forward_model_snapshot": (
            None if forward_snapshot is None else {
                "candidate_count": forward_snapshot.get("candidate_count", 0),
                "resolved_market_session_cutoff": forward_snapshot.get(
                    "resolved_market_session_cutoff"
                ),
            }
        ),
        "forward_simulation": forward_child,
    }
    if promotion_summary["executed"]:
        (output / "promotion_results.json").write_text(
            json.dumps(promotion_state, indent=2, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
    (output / "pipeline_summary.json").write_text(
        json.dumps(summary, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    return {
        "job_type": JobType.END_TO_END.value,
        "pipeline_version": spec.pipeline_version,
        "stage_run_ids": {
            item["stage_key"]: item["child_run_id"] for item in completed
        },
        "auto_promote_candidates": spec.auto_promote_candidates,
        "promotion": promotion_summary,
        "temporal_validation": temporal_comparison,
        "result_files": [
            "pipeline_summary.json",
            *(["promotion_results.json"] if promotion_summary["executed"] else []),
            *([TEMPORAL_VALIDATION_RESULT] if temporal_comparison is not None else []),
        ],
    }
