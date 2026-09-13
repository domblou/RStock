import importlib
import os
import sys
import threading
import time
from dataclasses import replace
from types import SimpleNamespace

import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
import rstock.application.repository as repository_module
from rstock.application.repository import RunRepository
from rstock.application.runner import ProgressReporter, RunService
from rstock.application.worker import SlotLease, execute_run
from rstock.application.workflows import WorkflowRegistry, _prepared_experiment
from rstock.config import DEFAULT_CONFIG
from rstock.progress import ProgressEvent, check_cancellation


class FakeBackend:
    def __init__(self, *, error=None):
        self.error = error
        self.launches = []

    def launch(self, runs_root, run_id, max_concurrent_jobs):
        if self.error:
            raise self.error
        self.launches.append((runs_root, run_id, max_concurrent_jobs))
        return 4321


def _spec(tmp_path, job_type=JobType.WALK_FORWARD, **config_changes):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, **config_changes)
    return ExperimentSpec(
        job_type=job_type,
        config=config,
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
    )


def test_run_creation_persists_required_files_and_reloadable_configuration(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    spec = _spec(tmp_path, xgb_seed=987)
    run_id = repository.create(spec)
    directory = repository.run_directory(run_id)

    assert {
        "config.json",
        "status.json",
        "progress.json",
        "summary.json",
        "run.log",
    } <= {path.name for path in directory.iterdir()}
    assert repository.status(run_id)["status"] == "pending"
    restored = RunRepository(tmp_path / "runs").load_spec(run_id)
    assert restored == spec
    assert restored.config.xgb_seed == 987


def test_walk_forward_preparation_fetches_predictor_union_but_limits_targets(
    monkeypatch, tmp_path
):
    index = pd.bdate_range("2026-01-05", periods=4)
    prices = pd.DataFrame(index=index)
    for symbol in ("AAA", "BBB", "CONTEXT"):
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.High"] = 102.0
        prices[f"{symbol}.Low"] = 99.0
        prices[f"{symbol}.Close"] = 101.0
    captured_symbols = []

    def fake_load(self, spec, **kwargs):
        captured_symbols.append(spec.symbols)
        return SimpleNamespace(
            prices=prices,
            symbols=list(spec.symbols),
            failed_symbols=[],
        ), {}

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, permutation_depth=1),
        symbols=("AAA", "BBB", "CONTEXT"),
        target_symbols=("AAA", "BBB"),
        context_symbols=("CONTEXT",),
    )

    prepared, generated, _ = _prepared_experiment(spec, None, None)

    assert captured_symbols == [("AAA", "BBB", "CONTEXT")]
    assert "CONTEXT_intraday_J-1" in prepared
    assert set(generated["V0"]) == {"AAA", "BBB"}
    assert "CONTEXT" in set(generated["V1"])


def test_status_transitions_and_duration_are_persisted(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    running = repository.transition(run_id, JobStatus.RUNNING, pid=123)
    completed = repository.transition(run_id, JobStatus.COMPLETED)

    assert running["started_at"] is not None
    assert running["pid"] == 123
    assert completed["finished_at"] is not None
    assert completed["duration_seconds"] >= 0
    with pytest.raises(ValueError, match="Invalid job transition"):
        repository.transition(run_id, JobStatus.RUNNING)


def test_progress_persists_real_percentage_and_observed_eta(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.started -= 1.0

    reporter(ProgressEvent("simulated", "unit", 2, 4, {"direction": "Up"}))
    progress = RunRepository(tmp_path / "runs").progress(run_id)

    assert progress["percent"] == 50.0
    assert progress["completed_units"] == 2
    assert progress["total_units"] == 4
    assert progress["eta_seconds"] is not None
    assert progress["details"] == {"direction": "Up"}


def test_phase_progress_never_reaches_global_100_before_final_publication(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.configure_phases([("walk_forward", 90), ("publishing", 10)])

    reporter.phase_started("walk_forward")
    reporter(ProgressEvent("walk_forward", "last", 10, 10))
    assert repository.progress(run_id)["stage_percent"] == 100.0
    assert repository.progress(run_id)["workflow_percent"] == 90.0
    reporter.phase_completed("walk_forward")
    assert repository.progress(run_id)["workflow_percent"] == 90.0
    with pytest.raises(RuntimeError, match="before publishing"):
        reporter.complete_workflow()

    reporter.phase_started("publishing")
    assert repository.progress(run_id)["workflow_percent"] < 100.0
    reporter.phase_completed("publishing")
    assert repository.progress(run_id)["workflow_percent"] == 99.9
    reporter.complete_workflow()
    assert repository.progress(run_id)["workflow_percent"] == 100.0


def test_unmeasurable_phase_has_no_eta(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.configure_phases([("aggregation", 1)])
    reporter.phase_started("aggregation")

    progress = repository.progress(run_id)
    assert progress["stage_percent"] is None
    assert progress["eta_seconds"] is None


def test_json_write_retries_temporary_windows_permission_error(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    original_replace = os.replace
    calls = 0

    def temporarily_locked(source, destination):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise PermissionError(5, "Access denied", str(destination))
        return original_replace(source, destination)

    monkeypatch.setattr(repository_module.os, "replace", temporarily_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    assert repository.write_json(run_id, "status.json", {"status": "updated"})
    assert calls == 2
    assert repository.read_json(run_id, "status.json") == {"status": "updated"}
    assert not list(repository.run_directory(run_id).glob(".status.json.*.tmp"))


def test_persistent_lock_skips_only_progress_and_allows_later_recovery(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    original_replace = os.replace
    previous_progress = repository.progress(run_id)
    previous_status = repository.status(run_id)

    def persistently_locked(source, destination):
        raise PermissionError(5, "Access denied", str(destination))

    monkeypatch.setattr(repository_module.os, "replace", persistently_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    reporter = ProgressReporter(repository, run_id)
    reporter(ProgressEvent("blocked", "progress", 1, 2))
    assert repository.write_json(run_id, "progress.json", {"stage": "blocked"}) is False
    assert repository.progress(run_id) == previous_progress
    assert "Progress update skipped" in "\n".join(repository.log_tail(run_id))
    assert not list(repository.run_directory(run_id).glob(".progress.json.*.tmp"))
    with pytest.raises(PermissionError):
        repository.write_json(run_id, "status.json", {"status": "blocked"})
    assert repository.status(run_id) == previous_status
    assert not list(repository.run_directory(run_id).glob(".status.json.*.tmp"))

    monkeypatch.setattr(repository_module.os, "replace", original_replace)
    assert repository.write_json(run_id, "progress.json", {"stage": "recovered"})
    assert repository.progress(run_id) == {"stage": "recovered"}


def test_pending_cancellation_is_durable(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    backend = FakeBackend()
    service = RunService(repository, backend=backend)
    submitted = service.submit(_spec(tmp_path))

    cancelled = service.cancel(submitted.run_id)

    assert cancelled["status"] == "cancelled"
    assert repository.cancellation_requested(submitted.run_id)
    assert RunRepository(tmp_path / "runs").status(submitted.run_id)["status"] == "cancelled"


def test_duplicate_active_submission_returns_existing_run(tmp_path):
    backend = FakeBackend()
    service = RunService(RunRepository(tmp_path / "runs"), backend=backend)
    spec = _spec(tmp_path)

    first = service.submit(spec)
    second = RunService(
        RunRepository(tmp_path / "runs"), backend=backend
    ).submit(spec)

    assert first.created is True
    assert second.created is False
    assert second.run_id == first.run_id
    assert len(backend.launches) == 1


def test_worker_failure_marks_failed_and_does_not_publish_results(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def fail(spec, output, progress, cancellation):
        (output / "partial.csv").write_text("partial", encoding="utf-8")
        raise RuntimeError("simulated worker failure")

    registry = WorkflowRegistry({JobType.WALK_FORWARD: fail})
    execute_run(repository, run_id, 1, registry=registry)

    status = repository.status(run_id)
    assert status["status"] == "failed"
    assert status["error"] == "simulated worker failure"
    assert repository.result_files(run_id) == []
    assert "simulated worker failure" in "\n".join(repository.log_tail(run_id))


def test_worker_cooperative_cancellation_keeps_partial_results_unpublished(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def cancel(spec, output, progress, cancellation):
        (output / "partial.csv").write_text("partial", encoding="utf-8")
        repository.request_cancellation(run_id)
        check_cancellation(cancellation)
        return {}

    registry = WorkflowRegistry({JobType.WALK_FORWARD: cancel})
    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == "cancelled"
    assert repository.result_files(run_id) == []
    assert (repository.run_directory(run_id) / "run.log").exists()


def test_completed_workers_publish_results_in_isolated_run_directories(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    first = repository.create(_spec(tmp_path))
    second = repository.create(_spec(tmp_path, job_type=JobType.THRESHOLD_CALIBRATION))

    def succeed(spec, output, progress, cancellation):
        (output / "result.txt").write_text(spec.job_type.value, encoding="utf-8")
        return {"job_type": spec.job_type.value, "metric": 1}

    registry = WorkflowRegistry(
        {
            JobType.WALK_FORWARD: succeed,
            JobType.THRESHOLD_CALIBRATION: succeed,
        }
    )
    execute_run(repository, first, 1, registry=registry)
    execute_run(repository, second, 1, registry=registry)

    assert repository.status(first)["status"] == "completed"
    assert repository.status(second)["status"] == "completed"
    assert repository.summary(first)["job_type"] == "walk_forward"
    assert repository.summary(second)["job_type"] == "threshold_calibration"
    assert (
        repository.run_directory(first) / "results" / "result.txt"
    ).read_text(encoding="utf-8") == "walk_forward"
    assert (
        repository.run_directory(second) / "results" / "result.txt"
    ).read_text(encoding="utf-8") == "threshold_calibration"


def test_worker_persists_phase_logs_and_completes_only_after_publishing(tmp_path, monkeypatch):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    completed_progress = []
    original_transition = repository.transition

    def observed_transition(identifier, target, **kwargs):
        if target is JobStatus.COMPLETED:
            completed_progress.append(repository.progress(identifier).copy())
        return original_transition(identifier, target, **kwargs)

    monkeypatch.setattr(repository, "transition", observed_transition)

    def succeed(spec, output, progress, cancellation):
        for phase in (
            "data_preparation", "combination_generation", "walk_forward",
            "aggregation", "qualification", "final_holdout", "metrics", "result_writing",
        ):
            progress(ProgressEvent(phase, "started", details={"phase_event": "started"}))
            progress(ProgressEvent(phase, "completed", details={"phase_event": "completed"}))
        (output / "result.txt").write_text("ready", encoding="utf-8")
        return {"job_type": spec.job_type.value}

    execute_run(repository, run_id, 1, registry=WorkflowRegistry({JobType.WALK_FORWARD: succeed}))

    assert repository.status(run_id)["status"] == "completed"
    assert completed_progress[0]["workflow_percent"] == 100.0
    assert completed_progress[0]["phase_history"][-1]["name"] == "publishing"
    assert completed_progress[0]["phase_history"][-1]["status"] == "completed"
    log = "\n".join(repository.log_tail(run_id, lines=100))
    for phase in ("data_preparation", "walk_forward", "final_holdout", "result_writing", "publishing"):
        assert f"Phase started: {phase}" in log
        assert f"Phase completed: {phase}" in log


def test_slot_lease_enforces_concurrency_limit(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    repository.root.mkdir(parents=True)
    first = SlotLease(repository, "first", 1, poll_seconds=0.01)
    second = SlotLease(repository, "second", 1, poll_seconds=0.01)
    first.acquire()
    acquired = threading.Event()

    def acquire_second():
        second.acquire()
        acquired.set()

    thread = threading.Thread(target=acquire_second)
    thread.start()
    time.sleep(0.05)
    assert not acquired.is_set()
    first.release()
    assert acquired.wait(timeout=1)
    second.release()
    thread.join(timeout=1)


def test_services_import_without_streamlit(monkeypatch):
    monkeypatch.setitem(sys.modules, "streamlit", None)
    module = importlib.import_module("rstock.application.services")
    importlib.reload(module)
    assert module.ExperimentService is not None
