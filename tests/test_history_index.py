"""History IO bounds, live invalidation and historical display compatibility."""
import json
from collections import Counter
from dataclasses import replace

import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.history_index import clear_history_cache, history_index, HistoryDetail
from rstock.application.history_ui import (
    EXPERIMENT_JOB_TYPES, filter_runs, paginate_runs, history_row, reconcile_history_selection,
)
from rstock.application.history_grid import history_grid_row
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.config import DEFAULT_CONFIG


@pytest.fixture(autouse=True)
def empty_cache():
    clear_history_cache()
    yield
    clear_history_cache()


def make_run(root, identifier, *, summary=None, legacy=False, state="completed"):
    directory = root / identifier
    directory.mkdir(parents=True)
    config = ExperimentSpec(job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=root.parent), symbols=("AAA", "BBB")).to_dict()
    if legacy:
        config["rstock_config"].pop("walk_forward_window_mode")
        config["rstock_config"].pop("walk_forward_train_size")
    payloads = {"config.json": config, "summary.json": summary or {},
                "status.json": {"run_id": identifier, "job_type": "walk_forward",
                                "status": state, "created_at": "2026-10-07T12:00:00+00:00",
                                "duration_seconds": 10}}
    for name, payload in payloads.items():
        (directory / name).write_text(json.dumps(payload), encoding="utf-8")
    return directory


def observe_reads(monkeypatch):
    counts = Counter()
    original = RunRepository._read_json_path
    def read(path):
        counts[path.name] += 1
        return original(path)
    monkeypatch.setattr(RunRepository, "_read_json_path", staticmethod(read))
    return counts


def render(records, repository, page=0):
    details = {item.status["run_id"]: item.detail() for item in records}
    filtered = filter_runs([item.status for item in records], allowed_types=EXPERIMENT_JOB_TYPES,
                           detail_loader=details.__getitem__)
    visible, _ = paginate_runs(filtered, page=page, page_size=25)
    return [history_grid_row(history_row(status, details[status["run_id"]], {},
                         related_details=details), details[status["run_id"]],
                         universe_labels={}, related_details=details, runs_root=repository.root)
            for status in visible]


def test_only_visible_summaries_are_loaded_and_cache_survives_service_instances(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    for number in range(60):
        make_run(root, f"run-{number:03}", summary={"heavy_metrics": [0.5] * 10000})
    repository = RunRepository(root)
    counts = observe_reads(monkeypatch)
    records = history_index(RunService(repository))
    assert counts["summary.json"] == 0
    render(records, repository)
    assert counts["summary.json"] == 25
    counts.clear()
    render(history_index(RunService(RunRepository(root))), repository)
    assert sum(counts.values()) == 0
    render(history_index(RunService(RunRepository(root))), repository, page=1)
    assert counts["summary.json"] == 25
    assert "heavy_metrics" not in records[0].detail()["summary"]


def test_targeted_invalidation_after_external_atomic_replace(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    directory = make_run(root, "first", summary={"prepared_dataset_as_of": "2026-10-01"})
    make_run(root, "second")
    repository = RunRepository(root)
    before = render(history_index(RunService(repository)), repository)
    assert next(row for row in before if row["Run ID"] == "first")["Cutoff"] == "2026-10-01"
    replacement = directory / "replacement.json"
    replacement.write_text('{"prepared_dataset_as_of":"2026-10-02"}', encoding="utf-8")
    replacement.replace(directory / "summary.json")
    counts = observe_reads(monkeypatch)
    after = render(history_index(RunService(RunRepository(root))), repository)
    assert next(row for row in after if row["Run ID"] == "first")["Cutoff"] == "2026-10-02"
    assert counts == {"summary.json": 1}


def test_filters_and_missing_legacy_storage_never_read_summary(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    directory = make_run(root, "legacy", legacy=True)
    make_run(root, "purged")
    (root / "purged" / "storage.json").write_text(
        '{"schema_version":1,"state":"purged"}', encoding="utf-8")
    repository = RunRepository(root)
    records = history_index(RunService(repository))
    details = {item.status["run_id"]: item.detail() for item in records}
    counts = observe_reads(monkeypatch)
    filtered = filter_runs([item.status for item in records], allowed_types=EXPERIMENT_JOB_TYPES,
                          storage="Complet", detail_loader=details.__getitem__)
    assert [item["run_id"] for item in filtered] == ["legacy"]
    assert counts["summary.json"] == 0
    original = (directory / "config.json").read_bytes()
    expected = repository.load_spec("legacy").to_dict()
    actual = details["legacy"]["configuration"]
    assert actual["rstock_config"]["walk_forward_window_mode"] == expected["rstock_config"]["walk_forward_window_mode"] == "expanding"
    assert actual["rstock_config"]["walk_forward_train_size"] == 252
    assert (directory / "config.json").read_bytes() == original


def test_legacy_and_explicit_configuration_display_matches_original(tmp_path):
    root = tmp_path / "runs"
    make_run(root, "legacy", legacy=True, summary={"traceability": {"prepared_market_last_date": "2026-10-02"}})
    make_run(root, "explicit")
    config_path = root / "explicit" / "config.json"
    config = json.loads(config_path.read_text())
    config["rstock_config"].update(walk_forward_window_mode="rolling", walk_forward_train_size=504)
    config_path.write_text(json.dumps(config))
    repository = RunRepository(root)
    service = RunService(repository)
    old = service.history_summaries()
    new = history_index(service)
    assert render(old, repository) == render(new, repository)
    old_details = {item.status["run_id"]: item.detail() for item in old}
    for item in new:
        assert history_row(item.status, item.detail(), {}) == history_row(item.status, old_details[item.status["run_id"]], {})


def test_cache_rechecks_live_status_and_keeps_interruption_tracking(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    directory = make_run(root, "active", state="running")
    calls = []
    monkeypatch.setattr(RunService, "_refresh_interrupted", lambda self, run_id:
                        calls.append(run_id) or self.repository.status(run_id))
    for _ in range(2):
        assert history_index(RunService(RunRepository(root)))[0].status["status"] == "running"
    assert calls == ["active", "active"]
    status = json.loads((directory / "status.json").read_text())
    status["status"] = "completed"
    (directory / "status.json").write_text(json.dumps(status))
    assert history_index(RunService(RunRepository(root)))[0].status["status"] == "completed"
    assert calls == ["active", "active"]


def test_changed_read_is_not_published_as_fresh_cache(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    directory = make_run(root, "changing", summary={"predictions": 1})
    repository = RunRepository(root)
    original = repository.summary
    def race(run_id):
        result = original(run_id)
        (directory / "summary.json").write_text('{"predictions": 200}')
        return result
    monkeypatch.setattr(repository, "summary", race)
    assert HistoryDetail(repository, "changing")["summary"]["predictions"] == 1
    monkeypatch.setattr(repository, "summary", original)
    assert HistoryDetail(repository, "changing")["summary"]["predictions"] == 200


def test_selection_survives_pagination_and_prunes_filtered_runs():
    assert reconcile_history_selection(["a", "b"], ["b", "c"], ["c"], ["a", "b", "c"]) == ["a", "c"]
    assert reconcile_history_selection(["a", "c"], ["b"], [], ["b", "c"]) == ["c"]


def test_live_additions_metadata_storage_and_model_filters_invalidate_only_changed_files(tmp_path, monkeypatch):
    root = tmp_path / "runs"
    directory = make_run(root, "first")
    make_run(root, "other")
    repository = RunRepository(root)
    render(history_index(RunService(repository)), repository)
    # Missing historical metadata/storage defaults must not survive publication.
    (directory / "metadata.json").write_text('{"visible_in_history":false}')
    records = history_index(RunService(repository))
    assert [record.status["run_id"] for record in records] == ["other"]
    (directory / "metadata.json").write_text('{"visible_in_history":true}')
    (directory / "storage.json").write_text('{"schema_version":1,"state":"purged"}')
    config_path = directory / "config.json"
    config = json.loads(config_path.read_text())
    config["model_id"] = "selected-model"
    config_path.write_text(json.dumps(config))
    counts = observe_reads(monkeypatch)
    records = history_index(RunService(repository))
    details = {record.status["run_id"]: record.detail() for record in records}
    filtered = filter_runs([record.status for record in records], allowed_types=EXPERIMENT_JOB_TYPES,
        model_id="selected-model", storage="Résumé seulement", detail_loader=details.__getitem__)
    assert [record["run_id"] for record in filtered] == ["first"]
    assert counts["config.json"] == 1
    assert counts["storage.json"] == 1
    assert counts["summary.json"] == 0
    make_run(root, "new")
    assert {record.status["run_id"] for record in history_index(RunService(repository))} == {"first", "other", "new"}


def test_returned_projection_cannot_poison_shared_cache(tmp_path):
    root = tmp_path / "runs"
    make_run(root, "first", summary={"categories": {"no_signal": 4}})
    detail = HistoryDetail(RunRepository(root), "first")
    value = detail["summary"]
    value["categories"]["no_signal"] = 99
    assert detail["summary"]["categories"]["no_signal"] == 4
