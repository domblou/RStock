"""Integrity and recovery of permanent run deletion."""

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


@pytest.mark.parametrize("status", [JobStatus.FAILED, JobStatus.CANCELLED, JobStatus.COMPLETED])
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

    service.delete_run(run_id, expected_run_ids=plan.run_ids, expected_fingerprint=plan.fingerprint)

    assert not directory.exists()
    assert run_id not in repository.list_run_ids()
    assert run_id not in [item.status["run_id"] for item in service.history_runs()]


def test_completed_purged_root_can_be_deleted(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    repository.write_storage(run_id, {"schema_version": 1, "state": "purged"})
    service = RunDeletionService(repository)
    plan = service.preview(run_id)
    service.delete_many(plan)
    assert run_id not in repository.list_run_ids()


@pytest.mark.parametrize("root_status", [JobStatus.FAILED, JobStatus.COMPLETED])
def test_pipeline_deletes_completed_owned_temporal_forced_and_scientific_children(tmp_path, root_status):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, root_status,
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

    service.delete(root, expected_run_ids=plan.run_ids, expected_fingerprint=plan.fingerprint)
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


def test_batch_staging_error_restores_entire_selection(tmp_path, monkeypatch):
    repository = RunRepository(tmp_path / "runs")
    first = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    second = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [first, second])
    original = Path.rename

    def fail_second(path, target):
        if path == repository.run_directory(second):
            raise OSError("disk error")
        return original(path, target)

    monkeypatch.setattr(Path, "rename", fail_second)
    outcome = execute_batch_delete(service, review)
    assert not outcome.succeeded
    assert "disk error" in outcome.errors[0][1]
    assert set(repository.list_run_ids()) == {first, second}
    assert all((repository.run_directory(item) / "run.log").exists() for item in (first, second))


def test_confirmation_rechecks_status_before_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.FAILED)
    service = RunDeletionService(repository)
    plan = service.preview(run_id)
    status = repository.status(run_id)
    status["status"] = JobStatus.COMPLETED.value
    repository.write_json(run_id, "status.json", status)

    with pytest.raises(ValueError, match="chang\u00e9"):
        service.delete(run_id, expected_run_ids=plan.run_ids, expected_fingerprint=plan.fingerprint)
    assert repository.run_directory(run_id).exists()


def test_interrupted_committed_cleanup_resumes_from_journal(tmp_path, monkeypatch):
    from rstock.application import run_delete
    from rstock.application.run_integrity import journals

    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    child = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=root)
    service = RunDeletionService(repository)
    plan = service.preview(root)
    original = run_delete.shutil.rmtree

    def stop_at_child(path):
        if Path(path).name == child:
            raise OSError("interrupted")
        return original(path)

    monkeypatch.setattr(run_delete.shutil, "rmtree", stop_at_child)
    with pytest.raises(OSError, match="interrupted"):
        service.delete_many(plan)
    assert not repository.run_directory(child).exists()
    assert not repository.run_directory(root).exists()
    assert list(journals(repository.root))[0][1]["state"] == "committed"
    monkeypatch.setattr(run_delete.shutil, "rmtree", original)
    RunDeletionService(RunRepository(repository.root)).recover()
    assert repository.list_run_ids() == []
    assert list(journals(repository.root))[0][1]["state"] == "complete"
    with pytest.raises(ValueError, match="supprim\u00e9"):
        repository.write_json(root, "summary.json", {})


@pytest.mark.parametrize("purged", [False, True])
def test_global_source_and_derived_selection_has_no_dangling_references(tmp_path, purged):
    from rstock.application.run_integrity import references
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    derived = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                   source_walk_forward_run=source)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    if purged:
        repository.write_storage(derived, {"schema_version": 1, "state": "purged"})
    service = ExperimentService(RunService(repository))
    blocked = preview_batch_delete(service, [source])
    assert not blocked.plans
    assert derived in blocked.errors[0][1]
    assert "source_walk_forward_run" in blocked.errors[0][1]
    review = preview_batch_delete(service, [source, derived])
    assert set(review.affected_run_ids) == {source, derived}
    outcome = execute_batch_delete(service, review)
    assert not outcome.errors
    assert set(outcome.deleted_run_ids) == {source, derived}
    assert repository.list_run_ids() == [retained]
    for path in repository.run_directory(retained).rglob("*.json"):
        import json
        assert not {target for _, target in references(json.loads(path.read_text()))} & {source, derived}


def test_global_blocker_prevents_deletion_of_any_selected_run(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    derived = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                   source_walk_forward_run=source)
    other = _run(repository, JobType.WALK_FORWARD, JobStatus.CANCELLED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [source, other])
    outcome = execute_batch_delete(service, review)
    assert outcome.errors
    assert not outcome.deleted_run_ids
    assert set(repository.list_run_ids()) == {source, derived, other}


def test_new_reference_after_confirmation_blocks_entire_operation(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    plan = service.preview(source)
    derived = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                   source_walk_forward_run=source)
    with pytest.raises(ValueError, match=derived):
        service.delete_many(plan)
    assert set(repository.list_run_ids()) == {source, derived}
    assert not (repository.root / ".deletions").exists()


def test_owned_child_cannot_be_deleted_without_parent(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    child = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=root)
    repository.write_json(root, "summary.json", {"child_run_id": child})
    service = RunDeletionService(repository)
    with pytest.raises(ValueError, match=root):
        service.preview(child)
    plan = service.preview_many((child, root, child))
    assert set(plan.run_ids) == {root, child}
    service.delete_many(plan)
    assert repository.list_run_ids() == []


def test_incomplete_config_and_corrupt_reference_file_block_deletion(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    partial = repository.run_directory("partial")
    partial.mkdir()
    (partial / "config.json").write_text('{"source_walk_forward_run": "' + source + '"}')
    service = RunDeletionService(repository)
    with pytest.raises(ValueError, match="partial"):
        service.preview(source)
    (partial / "config.json").write_text("broken json")
    with pytest.raises(ValueError, match="illisibles"):
        service.preview(source)
    assert repository.run_directory(source).exists()


@pytest.mark.parametrize("status", [JobStatus.PENDING, JobStatus.RUNNING])
def test_nonterminal_run_is_never_deleted(tmp_path, status):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))
    if status is JobStatus.RUNNING:
        repository.transition(run_id, status, pid=999_999_999)
    assert not RunDeletionService(repository).eligibility(run_id).eligible
    assert repository.run_directory(run_id).exists()


def test_live_worker_blocks_completed_deletion(tmp_path, monkeypatch):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    monkeypatch.setattr(service.storage, "_worker_is_active", lambda *args: True)
    with pytest.raises(ValueError, match="worker"):
        service.preview(run_id)


def test_completed_pipeline_requires_manifest(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.END_TO_END, JobStatus.COMPLETED)
    with pytest.raises(ValueError, match="Manifest"):
        RunDeletionService(repository).preview(run_id)


def test_crashed_staging_is_reconciled_on_next_history_read(tmp_path, monkeypatch):
    from rstock.application.run_integrity import journals
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    child = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=root)
    service = RunDeletionService(repository)
    plan = service.preview(root)
    original = Path.rename
    original_recover = service._recover_locked
    calls = 0

    def crash(path, target):
        if path == repository.run_directory(child):
            raise KeyboardInterrupt("crash")
        return original(path, target)

    def skip_post_crash_recovery(**kwargs):
        nonlocal calls
        calls += 1
        if calls == 1:
            original_recover(**kwargs)

    monkeypatch.setattr(Path, "rename", crash)
    monkeypatch.setattr(service, "_recover_locked", skip_post_crash_recovery)
    with pytest.raises(KeyboardInterrupt):
        service.delete_many(plan)
    assert not repository.run_directory(root).exists()
    assert repository.run_directory(child).exists()
    assert list(journals(repository.root))[0][1]["state"] == "staging"
    monkeypatch.setattr(Path, "rename", original)
    reloaded = RunRepository(repository.root)
    assert set(reloaded.list_run_ids()) == {root, child}
    assert list(journals(repository.root))[0][1]["state"] == "rolled_back"
    assert set(RunDeletionService(reloaded).preview(root).run_ids) == {root, child}


def test_stale_creation_and_production_publication_cannot_reference_deleted_run(tmp_path):
    from types import SimpleNamespace
    from rstock.application.production_repository import ProductionRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    service.delete_many(service.preview(source))
    stale_spec = _spec(tmp_path, JobType.XGBOOST_CALIBRATION, source_walk_forward_run=source)
    with pytest.raises(ValueError, match="supprim\u00e9"):
        repository.create(stale_spec)
    production = ProductionRepository(tmp_path)
    stale_model = SimpleNamespace(to_dict=lambda: {"calibration_source_run": source})
    with pytest.raises(ValueError, match="supprim\u00e9"):
        production._write_models([stale_model])
    assert not production.registry_path.exists()
    assert repository.list_run_ids() == []


def test_journal_reconciliation_preserves_latest_persistent_fields(tmp_path, monkeypatch):
    import json
    from rstock.application.run_integrity import journals
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    original = service._write_journal

    def publish(path, payload):
        original(path, payload)
        if payload["state"] == "staging":
            current = json.loads(path.read_text())
            current["audit_marker"] = "persisted-update"
            original(path, current)

    monkeypatch.setattr(service, "_write_journal", publish)
    service.delete_many(service.preview(run_id))
    journal = list(journals(repository.root))[0][1]
    assert journal["state"] == "complete"
    assert journal["audit_marker"] == "persisted-update"


@pytest.mark.parametrize("record", [
    lambda source: {"derivation": {"source_end_to_end_run_id": source,
                                   "inherited_stages": {"walk_forward": {"source_run_id": source}}}},
    lambda source: {"calibration_source_run": source},
    lambda source: {"run_ids": [source]},
])
def test_all_persisted_reference_forms_block_deletion(tmp_path, record):
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    (repository.run_directory(retained) / "checkpoints").mkdir()
    repository.write_json(retained, "checkpoints/custom.json", record(source))
    with pytest.raises(ValueError, match="checkpoints"):
        RunDeletionService(repository).preview(source)


def test_production_csv_reference_blocks_completed_run_deletion(tmp_path):
    from rstock.application.production_repository import ProductionRepository
    import pandas as pd
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    production = ProductionRepository(tmp_path)
    production.write_table("lineage.csv", pd.DataFrame([{"source_run_id": source}]))
    with pytest.raises(ValueError, match="Production"):
        RunDeletionService(repository).preview(source)


def test_cycle_and_inconsistent_ownership_block_mutation(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    repository.write_json(run_id, "metadata.json", {"parent_run_id": run_id, "root_run_id": run_id, "relation_key": "cycle", "relation_type": "pipeline_stage"})
    with pytest.raises(ValueError, match="Cycle"):
        RunDeletionService(repository).preview(run_id)
    assert repository.run_directory(run_id).exists()


def test_run_and_nested_symlinks_are_rejected(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    outside = tmp_path / "outside"
    outside.mkdir()
    (outside / "keep.txt").write_text("preserved")
    link = repository.run_directory(run_id) / "linked"
    try:
        link.symlink_to(outside, target_is_directory=True)
    except OSError:
        pytest.skip("Symlink creation unavailable")
    with pytest.raises(ValueError, match="Lien"):
        RunDeletionService(repository).preview(run_id)
    assert (outside / "keep.txt").read_text() == "preserved"


def test_stale_production_csv_publication_is_refused(tmp_path):
    from rstock.application.production_repository import ProductionRepository
    import pandas as pd
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    service.delete_many(service.preview(run_id))
    production = ProductionRepository(tmp_path)
    with pytest.raises(ValueError, match="supprim\u00e9"):
        production.write_table("lineage.csv", pd.DataFrame([{"source_run_id": run_id}]))


def test_concurrent_reference_publication_waits_then_rejects_deleted_source(tmp_path, monkeypatch):
    from concurrent.futures import ThreadPoolExecutor
    from threading import Event
    from rstock.application.run_integrity import graph_lock
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    plan = service.preview(source)
    started = Event()

    def stale_writer():
        started.set()
        return RunRepository(repository.root).write_json(
            retained, "results/reference.json", {"source_run_id": source})

    with ThreadPoolExecutor(max_workers=1) as pool:
        with graph_lock(repository.root):
            future = pool.submit(stale_writer)
            assert started.wait(5)
            assert not future.done()
            service.delete_many(plan)
        with pytest.raises(ValueError, match="supprim\u00e9"):
            future.result(timeout=10)
    assert not (repository.run_directory(retained) / "results/reference.json").exists()



def test_saved_simulation_provenance_blocks_deletion(tmp_path):
    import json
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    simulation = tmp_path / "simulations" / "simulation-1"
    simulation.mkdir(parents=True)
    (simulation / "simulation.json").write_text(json.dumps({
        "models": [{"source_walk_forward_run": source}]}))
    with pytest.raises(ValueError, match="simulation-1"):
        RunDeletionService(repository).preview(source)
    assert repository.run_directory(source).exists()


def test_stale_simulation_snapshot_cannot_publish_deleted_provenance(tmp_path):
    from rstock.application.simulation_repository import SimulationRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    service.delete_many(service.preview(source))
    simulations = SimulationRepository(tmp_path)
    with pytest.raises(ValueError, match="supprim\u00e9"):
        simulations.save(None, parameters={}, models=[{"source_walk_forward_run": source}])
    assert not simulations.root.exists()



def test_committed_cleanup_error_reports_logical_deletion_and_keeps_history_readable(tmp_path, monkeypatch):
    from rstock.application import run_delete
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [source])

    def disk_locked(path):
        raise PermissionError("locked quarantine")

    monkeypatch.setattr(run_delete.shutil, "rmtree", disk_locked)
    outcome = execute_batch_delete(service, review)
    assert outcome.succeeded == (source,)
    assert outcome.deleted_run_ids == (source,)
    assert "quarantaine" in outcome.errors[0][1]
    assert repository.list_run_ids() == [retained]


def test_corrupt_or_escaping_journal_blocks_recovery_without_mutation(tmp_path):
    import json
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    operation = repository.root / ".deletions" / "operation"
    operation.mkdir(parents=True)
    journal = operation / "journal.json"
    journal.write_text(json.dumps({"schema_version": 1, "state": "committed", "run_ids": [".."]}))
    with pytest.raises(ValueError, match="Invalid deletion journal"):
        RunDeletionService(repository).recover()
    assert repository.run_directory(run_id).exists()


def test_completed_pipeline_with_missing_materialized_child_is_refused(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    root = _run(repository, JobType.END_TO_END, JobStatus.COMPLETED)
    manifest = build_pipeline_manifest(repository, root, repository.load_spec(root))
    (repository.run_directory(root) / "orchestration").mkdir()
    repository.write_json(root, "orchestration/pipeline.json", manifest)
    with pytest.raises(ValueError, match="Enfant.*absent"):
        RunDeletionService(repository).preview(root)
    assert repository.run_directory(root).exists()



def test_old_individual_batch_confirmation_cannot_silently_delete_subset(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    first = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    second = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [first, second])
    obsolete = replace(review, plans=(service.delete_preview(first), service.delete_preview(second)))
    outcome = execute_batch_delete(service, obsolete)
    assert not outcome.deleted_run_ids
    assert "pr\u00e9visualisez" in outcome.errors[0][1]
    assert set(repository.list_run_ids()) == {first, second}
