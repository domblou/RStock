"""Permanent deletion remains separate from the completed-run purge."""

from dataclasses import replace
from pathlib import Path

import pytest

from rstock.application.batch_delete import execute_batch_delete, preview_batch_delete
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.end_to_end import build_pipeline_manifest
from rstock.application.forced_candidate_validation import _build_manifest
from rstock.application.repository import RunRepository
from rstock.application.run_delete import RunDeletionService
from rstock.application.runner import RunService
from rstock.application.services import ExperimentService
from rstock.config import DEFAULT_CONFIG


def _spec(root: Path, kind: JobType, **changes: object) -> ExperimentSpec:
    return replace(ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=root),
        symbols=("AAA", "BBB"), combinations_per_target=1,
    ), job_type=kind, **changes)


def _run(
    repository: RunRepository, kind: JobType, status: JobStatus,
    *, parent: str | None = None, relation: str = "pipeline_stage",
    **changes: object,
) -> str:
    metadata = None if parent is None else RunMetadata(
        run_role=RunRole.PIPELINE_STAGE, parent_run_id=parent,
        relation_key=relation, relation_type=relation,
    )
    run_id = repository.create(_spec(repository.root.parent, kind, **changes), metadata=metadata)
    if status is JobStatus.COMPLETED:
        repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    repository.transition(run_id, status)
    (repository.run_directory(run_id) / "run.log").write_text("trace technique", encoding="utf-8")
    return run_id


@pytest.mark.parametrize("status", [JobStatus.FAILED, JobStatus.CANCELLED])
def test_delete_terminal_standalone_removes_every_file_and_history_row(tmp_path, status):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, status)
    directory = repository.run_directory(run_id)
    (directory / "checkpoints").mkdir()
    (directory / "checkpoints" / "state.bin").write_bytes(b"state")
    (directory / "results").mkdir()
    (directory / "results" / "partial.csv").write_text("partial", encoding="utf-8")
    service = ExperimentService(RunService(repository))
    plan = service.delete_preview(run_id)
    assert plan.run_ids == (run_id,)

    service.delete_run(run_id, expected_run_ids=plan.run_ids)

    assert not directory.exists()
    assert run_id not in repository.list_run_ids()
    assert run_id not in [item.status["run_id"] for item in service.history_runs()]


def test_completed_root_cannot_be_deleted(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    assert not service.eligibility(run_id).eligible
    with pytest.raises(ValueError, match="failed ou cancelled"):
        service.preview(run_id)
    assert repository.run_directory(run_id).exists()


def test_failed_pipeline_deletes_completed_owned_temporal_forced_and_scientific_children(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED,
                pipeline_version=3, temporal_validation_enabled=True)
    root_spec = repository.load_spec(root)
    manifest = build_pipeline_manifest(repository, root, root_spec)
    (repository.run_directory(root) / "orchestration").mkdir()
    repository.write_json(root, "orchestration/pipeline.json", manifest)
    expected = {root}
    temporal = forced = ""
    for stage in manifest["stages"]:
        if not stage.get("child_run_id"):
            continue
        kind = JobType(stage["expected_job_type"])
        child = _run(repository, kind, JobStatus.COMPLETED, parent=root,
                     relation="pipeline_stage", **({"pipeline_version": 3} if kind is JobType.END_TO_END else {}))
        reserved = stage["child_run_id"]
        # Preserve the pipeline's reserved identity and ownership metadata.
        repository.run_directory(child).rename(repository.run_directory(reserved))
        expected.add(reserved)
        if kind is JobType.END_TO_END:
            temporal = reserved
            child_spec = repository.load_spec(reserved)
            nested = build_pipeline_manifest(repository, reserved, child_spec)
            (repository.run_directory(reserved) / "orchestration").mkdir()
            repository.write_json(reserved, "orchestration/pipeline.json", nested)
            for item in nested["stages"]:
                nested_id = item.get("child_run_id")
                if nested_id:
                    actual = _run(repository, JobType(item["expected_job_type"]), JobStatus.COMPLETED,
                                  parent=reserved)
                    repository.run_directory(actual).rename(repository.run_directory(nested_id))
                    expected.add(nested_id)
        elif kind is JobType.FORCED_CANDIDATE_VALIDATION:
            forced = reserved
            child_spec = repository.load_spec(reserved)
            nested = _build_manifest(repository, reserved, child_spec)
            (repository.run_directory(reserved) / "orchestration").mkdir()
            repository.write_json(reserved, "orchestration/pipeline.json", nested)
            for item in nested["stages"]:
                nested_id = item["child_run_id"]
                actual = _run(repository, JobType(item["expected_job_type"]), JobStatus.COMPLETED,
                              parent=reserved, relation="forced_candidate_validation_stage")
                repository.run_directory(actual).rename(repository.run_directory(nested_id))
                expected.add(nested_id)
    assert temporal and forced
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    plan = service.preview(root)
    assert set(plan.run_ids) == expected
    assert source not in plan.run_ids
    assert plan.by_type
    selected_child = next(run_id for run_id in plan.run_ids[1:]
                          if repository.status(run_id)["status"] == "completed")
    review = preview_batch_delete(ExperimentService(RunService(repository)),
                                  [selected_child, root])
    assert selected_child in review.affected_run_ids
    assert not review.skipped

    outcome = execute_batch_delete(ExperimentService(RunService(repository)), review)
    assert set(outcome.succeeded) == {root, selected_child}
    assert set(outcome.deleted_run_ids) == expected
    assert all(not repository.run_directory(run_id).exists() for run_id in expected)
    assert repository.run_directory(source).exists()


def test_external_scientific_reference_blocks_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    dependent = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                     source_walk_forward_run=source)
    service = RunDeletionService(repository)
    assert dependent in (service.eligibility(source).reason or "")
    assert repository.run_directory(source).exists()


def test_external_manifest_dependency_list_blocks_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    dependent = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    (repository.run_directory(dependent) / "orchestration").mkdir()
    repository.write_json(dependent, "orchestration/dependencies.json", {
        "dependency_run_ids": [source],
    })
    reason = RunDeletionService(repository).eligibility(source).reason
    assert dependent in (reason or "")


def test_partially_materialized_child_blocks_parent_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    partial = repository.run_directory("partial-child")
    partial.mkdir()
    (partial / "metadata.json").write_text(
        '{"parent_run_id": "' + root + '"}', encoding="utf-8"
    )
    reason = RunDeletionService(repository).eligibility(root).reason
    assert "partial-child" in (reason or "")


def test_failed_pipeline_without_manifest_uses_only_owned_metadata(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    owned = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                 parent=root, relation="pipeline_stage", source_walk_forward_run=source)
    unrelated = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED,
                     source_walk_forward_run=source)
    service = RunDeletionService(repository)
    plan = service.preview(root)
    assert set(plan.run_ids) == {root, owned}

    service.delete(root, expected_run_ids=plan.run_ids)
    assert repository.run_directory(source).exists()
    assert repository.run_directory(unrelated).exists()


def test_production_model_reference_blocks_deletion(tmp_path, monkeypatch):
    from types import SimpleNamespace
    from rstock.application import run_delete

    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    monkeypatch.setattr(run_delete.ProductionRepository, "models", lambda self: [
        SimpleNamespace(model_id="model-1", to_dict=lambda: {"source_walk_forward_run": source})
    ])
    reason = RunDeletionService(repository).eligibility(source).reason
    assert "model-1" in (reason or "")


def test_batch_delete_continues_after_error_and_skips_completed_root(tmp_path, monkeypatch):
    from rstock.application import run_delete

    repository = RunRepository(tmp_path / "runs")
    bad = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    good = _run(repository, JobType.WALK_FORWARD, JobStatus.CANCELLED)
    completed = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [bad, good, completed])
    assert len(review.plans) == 2
    assert len(review.skipped) == 1
    original = run_delete.shutil.rmtree

    def fail_bad(path):
        if Path(path).name == bad:
            raise OSError("disk error")
        return original(path)

    monkeypatch.setattr(run_delete.shutil, "rmtree", fail_bad)
    outcome = execute_batch_delete(service, review)
    assert outcome.succeeded == (good,)
    assert len(outcome.skipped) == 1
    assert outcome.errors == ((bad, "disk error"),)
    assert not repository.run_directory(good).exists()
    assert repository.run_directory(bad).exists()
    assert repository.run_directory(completed).exists()


def test_confirmation_rechecks_status_before_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    service = RunDeletionService(repository)
    plan = service.preview(run_id)
    status = repository.status(run_id)
    status["status"] = JobStatus.COMPLETED.value
    repository.write_json(run_id, "status.json", status)

    with pytest.raises(ValueError, match="failed ou cancelled"):
        service.delete(run_id, expected_run_ids=plan.run_ids)
    assert repository.run_directory(run_id).exists()


def test_interrupted_deletion_can_resume_without_a_tombstone(tmp_path, monkeypatch):
    from rstock.application import run_delete

    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    child = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=root)
    service = RunDeletionService(repository)
    plan = service.preview(root)
    original = run_delete.shutil.rmtree

    def stop_at_parent(path):
        if Path(path).name == root:
            raise OSError("interrupted")
        return original(path)

    monkeypatch.setattr(run_delete.shutil, "rmtree", stop_at_parent)
    with pytest.raises(OSError, match="interrupted"):
        service.delete(root, expected_run_ids=plan.run_ids)
    assert not repository.run_directory(child).exists()
    assert repository.run_directory(root).exists()

    monkeypatch.setattr(run_delete.shutil, "rmtree", original)
    resumed = service.preview(root)
    assert resumed.run_ids == (root,)
    service.delete(root, expected_run_ids=resumed.run_ids)
    assert not repository.run_directory(root).exists()
