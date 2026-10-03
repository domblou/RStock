"""Owner-only recursive purge across nested scientific pipelines."""

from dataclasses import replace
from pathlib import Path

from rstock.application.batch_purge import preview_batch_purge
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunPurpose, RunRole
from rstock.application.end_to_end import build_pipeline_manifest
from rstock.application.forced_candidate_validation import _build_manifest
from rstock.application.repository import RunRepository
from rstock.application.run_storage import RunStorageService, _ESSENTIAL_FILES
from rstock.application.runner import RunService
from rstock.application.services import ExperimentService
from rstock.config import DEFAULT_CONFIG


def _spec(root: Path, job_type: JobType, **changes: object) -> ExperimentSpec:
    return replace(
        ExperimentSpec(
            job_type=JobType.WALK_FORWARD,
            config=replace(DEFAULT_CONFIG, project_root=root),
            symbols=("AAA", "BBB"),
            combinations_per_target=1,
        ),
        job_type=job_type, **changes,
    )


def _materialize(
    repository: RunRepository, spec: ExperimentSpec, run_id: str,
    *, parent_id: str | None = None, relation_type: str = "pipeline_stage",
) -> None:
    metadata = (RunMetadata(run_role=RunRole.PIPELINE_PARENT,
                            run_purpose=RunPurpose.REFERENCE) if parent_id is None
                else RunMetadata(run_role=RunRole.PIPELINE_STAGE,
                                 parent_run_id=parent_id, relation_key=relation_type,
                                 relation_type=relation_type))
    repository.create(spec, run_id=run_id, metadata=metadata)
    for relative in _ESSENTIAL_FILES.get(spec.job_type, ()):
        path = repository.run_directory(run_id) / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("{}\n", encoding="utf-8")
    repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    repository.transition(run_id, JobStatus.COMPLETED)


def _save_manifest(repository: RunRepository, run_id: str, manifest: dict) -> None:
    (repository.run_directory(run_id) / "orchestration").mkdir(exist_ok=True)
    repository.write_json(run_id, "orchestration/pipeline.json", manifest)


def test_e2e_purge_recurses_through_temporal_and_forced_validation(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root_spec = _spec(tmp_path, JobType.END_TO_END,
                      pipeline_version=3, temporal_validation_enabled=True)
    root_id = repository.generate_run_id()
    _materialize(repository, root_spec, root_id)
    root_manifest = build_pipeline_manifest(repository, root_id, root_spec)
    _save_manifest(repository, root_id, root_manifest)

    source_id = repository.generate_run_id()
    _materialize(repository, _spec(tmp_path, JobType.WALK_FORWARD), source_id)

    expected: set[str] = set()
    temporal_id = forced_id = ""
    for stage in root_manifest["stages"]:
        child_id = stage.get("child_run_id")
        if not child_id:
            continue
        stage_key = stage["stage_key"]
        child_type = JobType(stage["expected_job_type"])
        changes = {}
        if stage_key == "temporal_validation_end_to_end":
            temporal_id = child_id
            changes = {"pipeline_version": 3}
        elif stage_key == "forced_candidate_validation_end_to_end":
            forced_id = child_id
            changes = {
                "forced_symbol_sets": (("AAA", "BBB"),),
                "forced_period_lock": {"schema_version": 1, "temporal_run_id": temporal_id},
            }
        child_spec = _spec(tmp_path, child_type, **changes)
        _materialize(repository, child_spec, child_id, parent_id=root_id,
                     relation_type=stage_key)
        expected.add(child_id)
        if child_type is JobType.END_TO_END:
            temporal_manifest = build_pipeline_manifest(repository, child_id, child_spec)
            _save_manifest(repository, child_id, temporal_manifest)
            for nested in temporal_manifest["stages"]:
                nested_id = nested.get("child_run_id")
                if nested_id:
                    _materialize(repository, _spec(tmp_path, JobType(nested["expected_job_type"])),
                                 nested_id, parent_id=child_id)
                    expected.add(nested_id)
        elif child_type is JobType.FORCED_CANDIDATE_VALIDATION:
            forced_manifest = _build_manifest(repository, child_id, child_spec)
            _save_manifest(repository, child_id, forced_manifest)
            for nested in forced_manifest["stages"]:
                nested_id = nested["child_run_id"]
                nested_spec = _spec(
                    tmp_path, JobType(nested["expected_job_type"]),
                    **({"source_walk_forward_run": source_id}
                       if nested["stage_key"] == "fixed_candidate_evaluation" else {}),
                )
                _materialize(repository, nested_spec,
                             nested_id, parent_id=child_id,
                             relation_type="forced_candidate_validation_stage")
                expected.add(nested_id)

    diagnostic_id = repository.generate_run_id()
    _materialize(repository, _spec(tmp_path, JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC),
                 diagnostic_id, parent_id=forced_id,
                 relation_type="qualification_holdout_diagnostic")
    expected.add(diagnostic_id)
    forward_id = repository.generate_run_id()
    _materialize(repository, _spec(
        tmp_path, JobType.FORWARD_SIMULATION,
        source_end_to_end_run=root_id,
        forward_simulation_start_date="2026-01-05",
        forward_simulation_end_date="2026-01-06",
    ),
                 forward_id, parent_id=root_id, relation_type="forward_simulation")
    expected.add(forward_id)
    unrelated_id = repository.generate_run_id()
    _materialize(repository, _spec(tmp_path, JobType.WALK_FORWARD),
                 unrelated_id, parent_id=root_id, relation_type="reference_only")

    service = RunStorageService(repository)
    forced_child_id = next(
        run_id for run_id in expected
        if repository.run_metadata(run_id).parent_run_id == forced_id
        and repository.status(run_id)["job_type"] == JobType.FIXED_CANDIDATE_EVALUATION.value
    )
    forced_child_status = repository.status(forced_child_id)
    repository.write_json(forced_child_id, "status.json",
                          {**forced_child_status, "status": JobStatus.FAILED.value})
    assert not service.eligibility(root_id).eligible
    repository.write_json(forced_child_id, "status.json", forced_child_status)

    plan = service.preview(root_id)
    assert set(plan.related_run_ids) == expected
    assert source_id not in plan.related_run_ids
    assert unrelated_id not in plan.related_run_ids
    assert len(plan.related_run_ids) == len(expected)
    forced_stage_types = {
        repository.status(run_id)["job_type"]
        for run_id in plan.related_run_ids
        if repository.run_metadata(run_id).parent_run_id == forced_id
    }
    assert forced_stage_types == {
        JobType.WALK_FORWARD.value,
        JobType.FIXED_CANDIDATE_EVALUATION.value,
        JobType.PROMOTION_QUALIFICATION.value,
        JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC.value,
    }
    batch_review = preview_batch_purge(
        ExperimentService(RunService(repository)), [root_id]
    )
    assert set(batch_review.affected_run_ids) == {root_id, *expected}
    assert dict(batch_review.affected_by_type)[JobType.FIXED_CANDIDATE_EVALUATION.value] == 1
    assert dict(batch_review.affected_by_type)[JobType.PROMOTION_QUALIFICATION.value] == 3

    service.purge(root_id)
    assert all(repository.storage(run_id)["state"] == "purged"
               for run_id in {root_id, *expected})
    assert repository.storage(source_id)["state"] == "full"
    assert repository.storage(unrelated_id)["state"] == "full"
