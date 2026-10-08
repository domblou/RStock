"""Bounded preparation IO without sacrificing graph validation."""

import json
import os
from collections import Counter
from pathlib import Path

from rstock.application.domain import JobStatus, JobType
from rstock.application.run_delete import RunDeletionService
from test_run_delete import _run


def _fixture(repository):
    parent = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    children = [_run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=parent)
                for _ in range(3)]
    derived = _run(repository, JobType.XGBOOST_CALIBRATION, JobStatus.COMPLETED,
                   source_walk_forward_run=children[0])
    for _ in range(8):
        other = _run(repository, JobType.THRESHOLD_CALIBRATION, JobStatus.COMPLETED)
        results = repository.run_directory(other) / "results"
        results.mkdir()
        (results / "threshold_diagnostics_by_set.json").write_text(
            json.dumps({"diagnostics": [0.5] * 10000}), encoding="utf-8")
        (results / "predictions.csv").write_text("prediction,probability\n1,0.5\n", encoding="utf-8")
    for run_id in (parent, *children, derived):
        batches = repository.run_directory(run_id) / "checkpoints" / "batches" / "development"
        for number in range(20):
            directory = batches / str(number)
            directory.mkdir(parents=True)
            (directory / "metadata.json").write_text('{"payload_sha256": "digest"}', encoding="utf-8")
            (directory / "complete.json").write_text('{"payload_sha256": "digest"}', encoding="utf-8")
    return parent, children, derived


def test_preparation_io_measurement(tmp_path, monkeypatch):
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    parent, children, derived = _fixture(repository)
    opened = Counter()
    walked = Counter()
    enumerated = Counter()
    original_open, original_walk, original_iterdir = Path.open, os.walk, Path.iterdir

    def observe_open(path, mode="r", *args, **kwargs):
        if "r" in mode and path.is_relative_to(tmp_path):
            opened[str(path.relative_to(tmp_path))] += 1
        return original_open(path, mode, *args, **kwargs)

    def observe_walk(path, *args, **kwargs):
        walked[str(path)] += 1
        return original_walk(path, *args, **kwargs)

    def observe_iterdir(path):
        enumerated[str(path)] += 1
        return original_iterdir(path)

    monkeypatch.setattr(Path, "open", observe_open)
    monkeypatch.setattr(os, "walk", observe_walk)
    monkeypatch.setattr(Path, "iterdir", observe_iterdir)
    plan = RunDeletionService(repository).preview_many((children[0], parent, derived))
    assert set(plan.run_ids) == {parent, *children, derived}
    # Same fixture before optimization: 605 reads, 281 distinct files, six
    # root enumerations, and up to twelve reads of an individual control file.
    assert sum(opened.values()) <= 80
    assert max(opened.values()) == 1
    assert enumerated[str(repository.root)] == 1
    assert not any("/batches/" in name.replace("\\", "/") for name in opened)
    assert not any(name.endswith("threshold_diagnostics_by_set.json") for name in opened)
    assert plan.size_bytes == 0
    print(json.dumps({"file_reads": sum(opened.values()), "unique_files": len(opened),
                      "walks": sum(walked.values()), "run_root_enumerations": enumerated[str(repository.root)],
                      "max_reads_per_file": max(opened.values())}))


def test_unknown_result_and_checkpoint_provenance_still_blocks(tmp_path):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    for relative in ("results/custom_lineage.json", "checkpoints/batches/phase/1/dependencies.json"):
        path = repository.run_directory(retained) / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps({"source_run_id": source}), encoding="utf-8")
        with pytest.raises(ValueError, match=retained):
            RunDeletionService(repository).preview(source)
        path.unlink()


def test_unknown_csv_lineage_still_blocks(tmp_path):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    path = repository.run_directory(retained) / "results" / "custom.csv"
    path.parent.mkdir()
    path.write_text(f"source_run_id,value\n{source},1\n", encoding="utf-8")
    with pytest.raises(ValueError, match=retained):
        RunDeletionService(repository).preview(source)


def test_external_csv_stops_at_first_blocker(tmp_path, monkeypatch):
    import pytest
    from rstock.application import run_delete
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    path = repository.run_directory(retained) / "results" / "lineage.csv"
    path.parent.mkdir()
    path.write_text(f"source_run_id\n{source}\n", encoding="utf-8")

    def reference_rows(file):
        assert file == path
        yield "source_run_id", source
        raise AssertionError("CSV body read after the first blocking dependency")

    monkeypatch.setattr(run_delete, "file_references", reference_rows)
    with pytest.raises(ValueError, match=retained):
        RunDeletionService(repository).preview(source)


def test_registry_is_read_once_for_models_and_raw_lineage(tmp_path, monkeypatch):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    path = tmp_path / "production" / "model_registry.json"
    path.parent.mkdir()
    path.write_text(json.dumps({"schema_version": 1, "models": [], "source_run_id": source}), encoding="utf-8")
    original_open = Path.open
    reads = []

    def observe_open(file, mode="r", *args, **kwargs):
        if file == path and "r" in mode:
            reads.append(file)
        return original_open(file, mode, *args, **kwargs)

    monkeypatch.setattr(Path, "open", observe_open)
    with pytest.raises(ValueError, match="Production"):
        RunDeletionService(repository).preview(source)
    assert reads == [path]


def test_reference_free_exports_cannot_publish_hidden_dependencies(tmp_path):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    source = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    retained = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    for name in ("results/threshold_diagnostics.json", "checkpoints/batches/phase/1/metadata.json"):
        with pytest.raises(ValueError, match="provenance"):
            repository.write_json(retained, name, {"source_run_id": source})


def test_fingerprint_ignores_metric_changes_but_rechecks_lineage(tmp_path):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    first = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    second = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    repository.write_json(selected, "summary.json", {"metric": 1, "source_run_id": first})
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    repository.write_json(selected, "summary.json", {"metric": 2, "source_run_id": first})
    assert service.preview(selected).fingerprint == plan.fingerprint
    repository.write_json(selected, "summary.json", {"metric": 2, "source_run_id": second})
    with pytest.raises(ValueError, match="confirmation"):
        service.delete_many(plan)
    assert repository.run_directory(selected).exists()


def test_graph_is_built_once_at_preview_and_once_at_confirmation(tmp_path, monkeypatch):
    from rstock.application import run_delete
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    parent = _run(repository, JobType.END_TO_END, JobStatus.FAILED)
    child = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED, parent=parent)
    graphs = []
    original = run_delete._DeletionGraph.__init__

    def record(graph, repo):
        graphs.append(graph)
        original(graph, repo)

    monkeypatch.setattr(run_delete._DeletionGraph, "__init__", record)
    service = RunDeletionService(repository)
    plan = service.preview_many((child, parent))
    assert len(graphs) == 1
    service.delete_many(plan)
    assert len(graphs) == 2
    assert repository.list_run_ids() == []


def test_orchestration_csv_fingerprints_lineage_without_metric_payload(tmp_path):
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    path = repository.run_directory(selected) / "orchestration" / "lineage.csv"
    path.parent.mkdir()
    path.write_text(f"source_run_id,metric\n{selected},1\n", encoding="utf-8")
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    path.write_text(f"source_run_id,metric\n{selected},2\n", encoding="utf-8")
    assert service.preview(selected).fingerprint == plan.fingerprint
    service.delete_many(plan)
    assert not repository.run_directory(selected).exists()


def test_preparation_only_uses_file_sizes_for_index_identity(tmp_path, monkeypatch):
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    original_stat, original_lstat = Path.stat, Path.lstat

    class SafetyAttributes:
        def __init__(self, attributes):
            self.attributes = attributes

        def __getattr__(self, name):
            if name == "st_size":
                import inspect
                from rstock.application.dependency_index import identity
                assert inspect.currentframe().f_back.f_code is identity.__code__, (
                    "File sizes must not be used for storage estimation during preparation"
                )
            return getattr(self.attributes, name)

    def safety_stat(path, *args, **kwargs):
        return SafetyAttributes(original_stat(path, *args, **kwargs))

    def safety_lstat(path, *args, **kwargs):
        return SafetyAttributes(original_lstat(path, *args, **kwargs))

    monkeypatch.setattr(Path, "stat", safety_stat)
    monkeypatch.setattr(Path, "lstat", safety_lstat)
    plan = RunDeletionService(repository).preview(selected)
    assert plan.run_ids == (selected,)
    assert plan.size_bytes == 0


def test_new_worker_blocks_confirmation(tmp_path, monkeypatch):
    import pytest
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    monkeypatch.setattr(service.storage, "_worker_is_active", lambda *args: True)
    with pytest.raises(ValueError, match="worker"):
        service.delete_many(plan)
    assert repository.run_directory(selected).exists()


def test_old_quarantine_is_not_cleaned_during_new_preparation_or_confirmation(tmp_path, monkeypatch):
    import pytest
    from rstock.application import run_delete
    from rstock.application.repository import RunRepository
    from rstock.application.run_integrity import journals
    repository = RunRepository(tmp_path / "runs")
    old = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    original = run_delete.shutil.rmtree

    def locked(path):
        if Path(path).name == old:
            raise PermissionError("locked old quarantine")
        return original(path)

    monkeypatch.setattr(run_delete.shutil, "rmtree", locked)
    with pytest.raises(run_delete.DeletionCleanupPending):
        service.delete_many(service.preview(old))
    journal_path = next(path for path, item in journals(repository.root) if old in item["run_ids"])
    walked = []
    original_walk = os.walk

    def observe_walk(path, *args, **kwargs):
        walked.append(Path(path))
        return original_walk(path, *args, **kwargs)

    monkeypatch.setattr(os, "walk", observe_walk)
    plan = service.preview(selected)
    assert repository.list_run_ids() == [selected]
    assert not any(".deletions" in path.parts for path in walked)
    service.delete_many(plan)
    assert json.loads(journal_path.read_text(encoding="utf-8"))["state"] == "committed"
    monkeypatch.setattr(run_delete.shutil, "rmtree", original)
    service.recover()
    assert json.loads(journal_path.read_text(encoding="utf-8"))["state"] == "complete"


def test_running_old_cleanup_does_not_hold_up_a_new_preview(tmp_path, monkeypatch):
    import pytest
    from concurrent.futures import ThreadPoolExecutor
    from threading import Event
    from rstock.application import run_delete
    from rstock.application.repository import RunRepository
    repository = RunRepository(tmp_path / "runs")
    old = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    service = RunDeletionService(repository)
    original = run_delete.shutil.rmtree

    def locked(path):
        raise PermissionError("old cleanup unavailable")

    monkeypatch.setattr(run_delete.shutil, "rmtree", locked)
    with pytest.raises(run_delete.DeletionCleanupPending):
        service.delete_many(service.preview(old))
    started, release = Event(), Event()

    def slow_cleaner(path):
        started.set()
        assert release.wait(15)
        return original(path)

    monkeypatch.setattr(run_delete.shutil, "rmtree", slow_cleaner)
    with ThreadPoolExecutor(max_workers=2) as pool:
        cleanup = pool.submit(service.recover)
        try:
            assert started.wait(5)
            preview = pool.submit(RunDeletionService(RunRepository(repository.root)).preview, selected)
            plan = preview.result(timeout=5)
            assert plan.run_ids == (selected,)
            assert not cleanup.done()
        finally:
            release.set()
        cleanup.result(timeout=5)
    assert repository.list_run_ids() == [selected]
