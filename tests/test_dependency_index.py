"""Persistent dependency analyses, external changes and interrupted publication."""
import json
import os
import time
from pathlib import Path

import pytest

from rstock.application.dependency_index import DependencyIndex, clear_dependency_memory
from rstock.application.domain import JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application.run_delete import RunDeletionService, _DeletionGraph
from rstock.application.run_integrity import file_references
from test_run_delete import _run


@pytest.fixture(autouse=True)
def clean_memory():
    clear_dependency_memory()
    yield
    clear_dependency_memory()


def setup_runs(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    selected = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    other = _run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED)
    path = repository.run_directory(other) / "results" / "custom_lineage.json"
    path.parent.mkdir()
    path.write_text('{"metric":1}', encoding="utf-8")
    for file in repository.root.rglob("*"):
        if file.is_file():
            old = time.time() - 10
            os.utime(file, (old, old))
    return repository, selected, other, path


def test_persistent_reload_reuses_unchanged_files_and_one_changed_file_is_reparsed(tmp_path, monkeypatch):
    repository, selected, other, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    clear_dependency_memory()
    parsed = []
    original = DependencyIndex.iter_edges
    def observe(self, source, loader, **kwargs):
        def record():
            parsed.append(source)
            return loader()
        yield from original(self, source, record, **kwargs)
    monkeypatch.setattr(DependencyIndex, "iter_edges", observe)
    assert RunDeletionService(RunRepository(repository.root)).preview(selected) == plan
    assert parsed == []
    path.write_text('{"metric":200}', encoding="utf-8")
    assert service.preview(selected) == plan
    assert parsed == [path]


@pytest.mark.parametrize("location,extension", [
    ("results", ".json"), ("checkpoints/batches/development/1", ".json"),
    ("results", ".csv"),
])
def test_new_external_reference_blocks_confirmation_after_warm_preview(tmp_path, location, extension):
    repository, selected, other, _ = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    path = repository.run_directory(other) / location / ("new" + extension)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps({"source_run_id": selected}) if extension == ".json"
                    else "source_run_id\n" + selected + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match=other):
        service.delete_many(plan)
    assert repository.run_directory(selected).exists()
    path.unlink()
    assert service.preview(selected) == plan


@pytest.mark.parametrize("domain", ["production", "simulations"])
def test_external_domain_changes_are_reconciled_after_warm_preview(tmp_path, domain):
    repository, selected, _, _ = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    path = tmp_path / domain / "item" / "lineage.json"
    path.parent.mkdir(parents=True)
    path.write_text(json.dumps({"source_run_id": selected}), encoding="utf-8")
    with pytest.raises(ValueError, match="Production|simulation"):
        service.delete_many(plan)
    assert repository.run_directory(selected).exists()


def test_changed_existing_reference_is_not_hidden_by_warm_cache(tmp_path):
    repository, selected, other, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    service.preview(selected)
    path.write_text(json.dumps({"source_run_id": selected}), encoding="utf-8")
    with pytest.raises(ValueError, match=other):
        service.preview(selected)


def test_corrupt_or_incompatible_index_reconstructs_from_sources(tmp_path):
    repository, selected, other, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    index = DependencyIndex(repository)
    for contents in ('{"broken":', '{"schema_version":999}',
                     index.path.read_text().replace('"sha256":"', '"sha256":"broken')):
        index.path.write_text(contents, encoding="utf-8")
        clear_dependency_memory()
        assert service.preview(selected) == plan
        assert DependencyIndex(repository)._load()
    path.write_text(json.dumps({"source_run_id": selected}), encoding="utf-8")
    index.path.write_text('{}', encoding="utf-8")
    with pytest.raises(ValueError, match=other):
        service.preview(selected)


def test_interrupted_atomic_publication_keeps_previous_index_and_reconciles(tmp_path, monkeypatch):
    repository, selected, _, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    plan = service.preview(selected)
    index = DependencyIndex(repository)
    before = index.path.read_bytes()
    original = repository._atomic_json_write
    path.write_text('{"metric":3}', encoding="utf-8")
    def crash(destination, payload):
        if destination == index.path:
            raise KeyboardInterrupt("interrupted publication")
        return original(destination, payload)
    monkeypatch.setattr(repository, "_atomic_json_write", crash)
    with pytest.raises(KeyboardInterrupt):
        service.preview(selected)
    assert index.path.read_bytes() == before
    monkeypatch.setattr(repository, "_atomic_json_write", original)
    clear_dependency_memory()
    assert service.preview(selected) == plan
    assert index.path.read_bytes() != before


def test_publish_reloads_persistent_state_before_merging_independent_updates(tmp_path):
    repository, _, _, first = setup_runs(tmp_path)
    second = first.with_name("second.json")
    second.write_text('{"source_run_id":"second"}')
    old = time.time() - 10
    os.utime(second, (old, old))
    a, b = DependencyIndex(repository), DependencyIndex(repository)
    tuple(a.iter_edges(first, lambda: file_references(first)))
    tuple(b.iter_edges(second, lambda: file_references(second)))
    a.publish(complete=False)
    b.publish(complete=False)
    clear_dependency_memory()
    restored = DependencyIndex(repository)
    assert restored._key(first) in restored.entries
    assert restored._key(second) in restored.entries


def test_partial_csv_scan_is_never_published_as_complete(tmp_path):
    repository, selected, _, first = setup_runs(tmp_path)
    path = first.with_suffix(".csv")
    path.write_text("source_run_id\n" + selected + "\nother\n")
    index = DependencyIndex(repository)
    iterator = index.iter_edges(path, lambda: file_references(path))
    assert next(iterator)[1] == selected
    iterator.close()
    index.publish(complete=False)
    assert index._key(path) not in DependencyIndex(repository)._load()


def test_changed_source_during_read_is_rejected_without_publishing(tmp_path):
    repository, _, _, path = setup_runs(tmp_path)
    index = DependencyIndex(repository)
    def changing():
        yield "source_run_id", "before"
        path.write_text('{"source_run_id":"after"}')
    with pytest.raises(RuntimeError, match="modifiée"):
        tuple(index.iter_edges(path, changing))
    assert index.pending == {}


def test_new_directory_during_scan_is_detected_at_validation(tmp_path):
    repository, _, _, path = setup_runs(tmp_path)
    index = DependencyIndex(repository)
    index.observe(path.parent)
    path.with_name("new.json").write_text('{}')
    with pytest.raises(RuntimeError, match="modifiée"):
        index.publish(complete=True)


def test_explicit_rebuild_reconstructs_all_edges_without_mutating_artifacts(tmp_path):
    repository, selected, _, path = setup_runs(tmp_path)
    original = {file: file.read_bytes() for file in repository.root.rglob("*.json")}
    service = RunDeletionService(repository)
    stats = service.rebuild_dependency_index()
    assert stats["parsed"] > 0
    assert all(file.read_bytes() == content for file, content in original.items())
    graph = _DeletionGraph(repository)
    assert service._external_dependency(set(), graph) is None
    assert graph.index.stats["parsed"] == 0
    assert graph.index.stats["hits"] > 0
    assert service.preview(selected).run_ids == (selected,)


def test_stale_index_entries_for_missing_files_are_pruned_after_complete_scan(tmp_path):
    repository, selected, _, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    service.preview(selected)
    index = DependencyIndex(repository)
    assert index._key(path) in index.entries
    path.unlink()
    service.preview(selected)
    assert index._key(path) not in DependencyIndex(repository)._load()


def test_recent_same_size_same_timestamp_edit_is_never_reused(tmp_path):
    repository, selected, other, path = setup_runs(tmp_path)
    service = RunDeletionService(repository)
    path.write_text(json.dumps({"source_run_id": "x" * len(selected)}))
    service.preview(selected)
    stamp = path.stat()
    path.write_text(json.dumps({"source_run_id": selected}))
    os.utime(path, ns=(stamp.st_atime_ns, stamp.st_mtime_ns))
    with pytest.raises(ValueError, match=other):
        service.preview(selected)


def test_recent_analysis_is_not_persisted_then_stable_analysis_is_cached(tmp_path, monkeypatch):
    import importlib
    module = importlib.import_module("rstock.application.dependency_index")
    repository, _, _, path = setup_runs(tmp_path)
    path.write_text('{"source_run_id":"original"}')
    clock = time.time_ns()
    monkeypatch.setattr(module.time, "time_ns", lambda: clock)
    index = DependencyIndex(repository)
    assert tuple(index.iter_edges(path, lambda: file_references(path)))
    index.publish(complete=False)
    assert index._key(path) not in DependencyIndex(repository).entries
    monkeypatch.setattr(module.time, "time_ns", lambda: clock + 3_000_000_000)
    stable = DependencyIndex(repository)
    assert tuple(stable.iter_edges(path, lambda: file_references(path)))
    stable.publish(complete=False)
    assert stable._key(path) in DependencyIndex(repository).entries
