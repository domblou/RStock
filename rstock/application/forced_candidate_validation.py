"""Dedicated two-stage orchestration for forced candidate revalidation."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Callable

from rstock.progress import CancellationCheck, ProgressCallback, check_cancellation

from .domain import ExperimentSpec, JobType, RunMetadata, RunRole
from .end_to_end import (
    CHILD_ID_POLICY_RESERVED,
    PIPELINE_MANIFEST,
    artifact_digests,
    build_stage_spec,
)
from .repository import RunRepository


FORCED_VALIDATION_SCHEMA_VERSION = 1
FORCED_VALIDATION_STAGES: tuple[tuple[str, JobType, tuple[str, ...]], ...] = (
    ("walk_forward", JobType.WALK_FORWARD, ()),
    (
        "fixed_candidate_evaluation",
        JobType.FIXED_CANDIDATE_EVALUATION,
        ("walk_forward",),
    ),
)


def _stage(manifest: dict[str, Any], stage_key: str) -> dict[str, Any]:
    matches = [item for item in manifest["stages"] if item["stage_key"] == stage_key]
    if len(matches) != 1:
        raise ValueError(f"Étape de revalidation forcée introuvable: {stage_key}")
    return matches[0]


def _build_manifest(repository: RunRepository, run_id: str) -> dict[str, Any]:
    child_ids = {
        stage_key: repository.generate_run_id()
        for stage_key, _job_type, _dependencies in FORCED_VALIDATION_STAGES
    }
    return {
        "schema_version": FORCED_VALIDATION_SCHEMA_VERSION,
        "workflow_type": JobType.FORCED_CANDIDATE_VALIDATION.value,
        "child_id_policy_version": CHILD_ID_POLICY_RESERVED,
        "root_run_id": run_id,
        "stages": [
            {
                "stage_key": stage_key,
                "expected_job_type": job_type.value,
                "child_run_id": child_ids[stage_key],
                "expected_fingerprint": None,
                "dependency_run_ids": [child_ids[item] for item in dependencies],
                "artifact_digests": {},
            }
            for stage_key, job_type, dependencies in FORCED_VALIDATION_STAGES
        ],
    }


def load_forced_validation_manifest(
    repository: RunRepository, run_id: str
) -> dict[str, Any] | None:
    path = repository.run_directory(run_id) / PIPELINE_MANIFEST
    if not path.is_file():
        return None
    manifest = repository.read_json(run_id, PIPELINE_MANIFEST)
    if (
        manifest.get("schema_version") != FORCED_VALIDATION_SCHEMA_VERSION
        or manifest.get("workflow_type") != JobType.FORCED_CANDIDATE_VALIDATION.value
        or manifest.get("root_run_id") != run_id
        or [item.get("stage_key") for item in manifest.get("stages", [])]
        != [item[0] for item in FORCED_VALIDATION_STAGES]
    ):
        raise ValueError("Manifest de revalidation forcée incompatible")
    return manifest


def _persist_stage(
    repository: RunRepository, run_id: str, stage_key: str, **values: object
) -> dict[str, Any]:
    manifest = load_forced_validation_manifest(repository, run_id)
    if manifest is None:
        raise ValueError("Manifest de revalidation forcée absent")
    _stage(manifest, stage_key).update(values)
    repository.write_json(run_id, PIPELINE_MANIFEST, manifest)
    return manifest


def _materialize_stage(
    repository: RunRepository,
    run_id: str,
    parent: ExperimentSpec,
    stage_key: str,
    stage_index: int,
) -> tuple[str, ExperimentSpec]:
    manifest = load_forced_validation_manifest(repository, run_id)
    if manifest is None:
        raise ValueError("Manifest de revalidation forcée absent")
    specification = build_stage_spec(repository, run_id, parent, stage_key, manifest)
    stage = _stage(manifest, stage_key)
    child_run_id = str(stage["child_run_id"])
    expected = stage.get("expected_fingerprint")
    if expected is None:
        _persist_stage(
            repository,
            run_id,
            stage_key,
            expected_fingerprint=specification.fingerprint,
        )
    elif expected != specification.fingerprint:
        raise ValueError(f"Fingerprint incompatible pour {stage_key}")
    if not repository.run_directory(child_run_id).exists():
        repository.create(
            specification,
            run_id=child_run_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE,
                visible_in_history=True,
                parent_run_id=run_id,
                relation_key=f"forced_candidate_validation:{stage_key}",
                relation_type="forced_candidate_validation_stage",
                stage_key=stage_key,
                stage_index=stage_index,
            ),
        )
    elif repository.configuration_fingerprint(child_run_id) != specification.fingerprint:
        raise ValueError(f"Snapshot enfant incompatible pour {stage_key}")
    return child_run_id, specification


def run_forced_candidate_validation(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
    *,
    execute_reserved_child: Callable[[RunRepository, str], None],
    phase_callback: Callable[..., None],
) -> dict[str, Any]:
    """Run only targeted WF and fixed evaluation; no generic E2E or calibration."""

    if spec.job_type is not JobType.FORCED_CANDIDATE_VALIDATION:
        raise ValueError("JobType de revalidation forcée requis")
    if spec.forced_symbol_sets is None:
        raise ValueError("Candidats forcés absents")
    repository = RunRepository(output.parent.parent)
    run_id = output.parent.name
    if spec.forced_symbol_sets == ():
        output.mkdir(parents=True, exist_ok=True)
        return {
            "schema_version": FORCED_VALIDATION_SCHEMA_VERSION,
            "job_type": spec.job_type.value,
            "root_run_id": run_id,
            "candidate_count": 0,
            "historical_backfill": spec.historical_forced_validation_backfill,
            "stages": [],
            "promotion": {"requested": False, "executed": False},
        }
    manifest = load_forced_validation_manifest(repository, run_id)
    if manifest is None:
        manifest = _build_manifest(repository, run_id)
        (repository.run_directory(run_id) / "orchestration").mkdir(exist_ok=True)
        repository.write_json(run_id, PIPELINE_MANIFEST, manifest)
        manifest = load_forced_validation_manifest(repository, run_id) or manifest
    completed: list[dict[str, object]] = []
    for stage_index, (stage_key, job_type, dependencies) in enumerate(
        FORCED_VALIDATION_STAGES
    ):
        check_cancellation(cancellation_check)
        for dependency in dependencies:
            dependency_id = str(_stage(manifest, dependency)["child_run_id"])
            if repository.status(dependency_id).get("status") != "completed":
                raise RuntimeError(
                    f"Dépendance incomplète pour {stage_key}: {dependency_id}"
                )
        child_run_id, child_spec = _materialize_stage(
            repository, run_id, spec, stage_key, stage_index
        )
        phase_callback(
            progress_callback,
            stage_key,
            "started",
            child_run_id=child_run_id,
            job_type=job_type.value,
            stage_index=stage_index + 1,
            stage_count=len(FORCED_VALIDATION_STAGES),
        )
        execute_reserved_child(repository, child_run_id)
        check_cancellation(cancellation_check)
        digests = artifact_digests(repository, child_run_id, stage_key)
        persisted = _stage(
            load_forced_validation_manifest(repository, run_id) or manifest, stage_key
        ).get("artifact_digests") or {}
        if persisted and persisted != digests:
            raise ValueError(f"Les artefacts de {stage_key} ont changé")
        if not persisted:
            _persist_stage(repository, run_id, stage_key, artifact_digests=digests)
        phase_callback(
            progress_callback,
            stage_key,
            "completed",
            child_run_id=child_run_id,
            job_type=child_spec.job_type.value,
            artifact_count=len(digests),
            stage_index=stage_index + 1,
            stage_count=len(FORCED_VALIDATION_STAGES),
        )
        completed.append(
            {
                "stage_key": stage_key,
                "job_type": child_spec.job_type.value,
                "child_run_id": child_run_id,
                "artifact_digests": digests,
            }
        )
        manifest = load_forced_validation_manifest(repository, run_id) or manifest
    return {
        "schema_version": FORCED_VALIDATION_SCHEMA_VERSION,
        "job_type": spec.job_type.value,
        "root_run_id": run_id,
        "candidate_count": len(spec.forced_symbol_sets),
        "historical_backfill": spec.historical_forced_validation_backfill,
        "stages": completed,
        "promotion": {"requested": False, "executed": False},
    }
