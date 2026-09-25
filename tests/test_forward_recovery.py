import hashlib
import json
import multiprocessing
import os
import threading
from dataclasses import replace

import pytest

import rstock.application.runner as runner_module
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.forward_simulation import SNAPSHOT_FILENAME
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.config import DEFAULT_CONFIG


class FakeBackend:
    def __init__(self, pid=9090):
        self.pid = pid
        self.launches = []

    def launch(self, runs_root, run_id, max_concurrent_jobs):
        self.launches.append(run_id)
        return self.pid


class _FileBackend:
    def __init__(self, path):
        self.path = path

    def launch(self, _runs_root, run_id, _max_concurrent_jobs):
        with open(self.path, "a", encoding="utf-8") as stream:
            stream.write(f"{run_id}\n")
        return 424242


def _automatic_recovery_process(runs_root, launch_log, barrier, reports):
    runner_module._pid_alive = lambda pid: pid == 424242
    service = RunService(
        RunRepository(runs_root), backend=_FileBackend(launch_log)
    )
    barrier.wait(timeout=10)
    reports.put(service.recover_pending_forward_dispatches())


def _exit_while_holding_submission_mutex(runs_root, ready):
    service = RunService(RunRepository(runs_root), backend=FakeBackend())
    with service._submission_lock():
        ready.set()
        os._exit(0)


def _forward_run(tmp_path, *, with_model=False):
    repository = RunRepository(tmp_path / "runs")
    parent = ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        historical_data_cutoff="2026-06-22",
        requested_historical_cutoff="2026-06-22",
        resolved_market_session_cutoff="2026-06-22",
    )
    parent_id = repository.create(parent)
    repository.transition(parent_id, JobStatus.RUNNING)
    repository.transition(parent_id, JobStatus.COMPLETED)
    result_dir = repository.run_directory(parent_id) / "results"
    result_dir.mkdir()
    models = [{"source_model_id": "model-1"}] if with_model else []
    if with_model:
        model_directory = result_dir / "forward_model_snapshot" / "model-1"
        model_directory.mkdir(parents=True)
        for name in ("up.ubj", "down.ubj", "metadata.json"):
            (model_directory / name).write_text("fixture", encoding="utf-8")
    snapshot = {
        "source_end_to_end_run_id": parent_id,
        "resolved_market_session_cutoff": "2026-06-22",
        "models": models,
    }
    snapshot_path = result_dir / SNAPSHOT_FILENAME
    snapshot_path.write_text(json.dumps(snapshot), encoding="utf-8")
    digest = hashlib.sha256(snapshot_path.read_bytes()).hexdigest()
    forward = replace(
        parent,
        job_type=JobType.FORWARD_SIMULATION,
        source_end_to_end_run=parent_id,
        source_forward_model_snapshot_sha256=digest,
        forward_simulation_start_date="2026-06-23",
        forward_simulation_end_date="2026-09-21",
        forward_simulation_enabled=False,
        run_description="Forward Simulation automatique",
    )
    run_id = repository.create(
        forward,
        metadata=RunMetadata(
            run_role=RunRole.PIPELINE_STAGE,
            parent_run_id=parent_id,
            relation_key="forward_simulation",
            relation_type="forward_simulation",
            stage_key="forward_simulation",
        ),
    )
    return repository, parent_id, run_id


def test_orphaned_pending_forward_is_recovered_with_the_same_id(tmp_path, monkeypatch):
    repository, parent_id, run_id = _forward_run(tmp_path)
    backend = FakeBackend()
    service = RunService(repository, backend=backend)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    diagnosis = service.forward_recovery_diagnosis(run_id)
    recovered = service.resume_forward_simulation(run_id)

    assert diagnosis.state == "pending_orphaned"
    assert recovered.run_id == run_id
    assert backend.launches == [run_id]
    assert repository.load_spec(run_id).source_end_to_end_run == parent_id
    assert repository.status(run_id)["launcher_pid"] == 9090


def test_pending_forward_with_live_worker_lock_is_not_recoverable(tmp_path, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    lock = repository.run_directory(run_id) / ".worker.lock"
    lock.mkdir()
    (lock / "owner.json").write_text(
        json.dumps({"run_id": run_id, "pid": os.getpid()}), encoding="utf-8"
    )
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == os.getpid())
    service = RunService(repository, backend=FakeBackend())

    assert service.forward_recovery_diagnosis(run_id).state == "pending_active"
    with pytest.raises(ValueError, match="worker ou dispatch actif"):
        service.resume_forward_simulation(run_id)


@pytest.mark.parametrize("state", [JobStatus.RUNNING, JobStatus.COMPLETED])
def test_running_or_completed_forward_is_not_recoverable(tmp_path, state):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    if state is JobStatus.RUNNING:
        repository.transition(run_id, state, pid=os.getpid())
    else:
        repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
        repository.transition(run_id, state)
    service = RunService(repository, backend=FakeBackend())

    with pytest.raises(ValueError):
        service.resume_forward_simulation(run_id)


@pytest.mark.parametrize("state", [JobStatus.FAILED, JobStatus.CANCELLED, JobStatus.INTERRUPTED])
def test_terminal_forward_resume_uses_existing_same_run_mechanism(tmp_path, state, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
    repository.transition(run_id, state, error="test")
    backend = FakeBackend()
    service = RunService(repository, backend=backend)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda _pid: False)

    resumed = service.resume_forward_simulation(run_id)

    assert resumed.run_id == run_id
    assert backend.launches == [run_id]
    assert repository.status(run_id)["status"] == JobStatus.PENDING.value


def test_snapshot_hash_or_checkpoint_mismatch_refuses_recovery(tmp_path):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    spec = repository.load_spec(run_id)
    repository.write_json(run_id, "config.json", replace(
        spec, source_forward_model_snapshot_sha256="0" * 64
    ).to_dict())
    service = RunService(repository, backend=FakeBackend())

    with pytest.raises(ValueError, match="forward_snapshot_sha256_mismatch"):
        service.resume_forward_simulation(run_id)


def test_missing_snapshot_refuses_recovery(tmp_path):
    repository, parent_id, run_id = _forward_run(tmp_path)
    (repository.run_directory(parent_id) / "results" / SNAPSHOT_FILENAME).unlink()
    service = RunService(repository, backend=FakeBackend())

    with pytest.raises(ValueError, match="forward_snapshot_missing"):
        service.resume_forward_simulation(run_id)


def test_incompatible_forward_checkpoint_is_never_reused(tmp_path):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    working = repository.run_directory(run_id) / "_working"
    working.mkdir()
    (working / "forward_observations_checkpoint.csv").write_text(
        "forward_simulation_run_id,source_end_to_end_run_id,source_model_id,resolved_source_cutoff\n"
        "another-run,other-parent,other-model,2026-01-01\n",
        encoding="utf-8",
    )
    service = RunService(repository, backend=FakeBackend())

    with pytest.raises(ValueError, match="forward_checkpoint_run_mismatch"):
        service.resume_forward_simulation(run_id)


def test_valid_forward_checkpoint_is_reused_by_same_run_resume(tmp_path, monkeypatch):
    repository, parent_id, run_id = _forward_run(tmp_path, with_model=True)
    working = repository.run_directory(run_id) / "_working"
    working.mkdir()
    (working / "forward_observations_checkpoint.csv").write_text(
        "forward_simulation_run_id,source_end_to_end_run_id,source_model_id,resolved_source_cutoff\n"
        f"{run_id},{parent_id},model-1,2026-06-22\n",
        encoding="utf-8",
    )
    repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
    repository.transition(run_id, JobStatus.FAILED, error="test")
    backend = FakeBackend()
    service = RunService(repository, backend=backend)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda _pid: False)

    resumed = service.resume_forward_simulation(run_id)

    assert resumed.run_id == run_id
    assert backend.launches == [run_id]


def test_concurrent_orphan_recovery_dispatches_once(tmp_path, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    backend = FakeBackend(pid=6060)
    service = RunService(repository, backend=backend)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 6060)
    outcomes = []

    def recover():
        try:
            outcomes.append(service.resume_forward_simulation(run_id).run_id)
        except ValueError as error:
            outcomes.append(str(error))

    threads = [threading.Thread(target=recover), threading.Thread(target=recover)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

    assert backend.launches == [run_id]
    assert outcomes.count(run_id) == 1
    assert len(outcomes) == 2


def test_automatic_scan_recovers_forward_never_dispatched(tmp_path, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    backend = FakeBackend()
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()
    status = repository.status(run_id)

    assert report["dispatched"] == [run_id]
    assert backend.launches == [run_id]
    assert status["status"] == JobStatus.PENDING.value
    assert status["dispatch_state"] == "launched"
    assert status["dispatch_attempt_count"] == 1
    assert status["resume_requested"] is True
    log = "\n".join(repository.log_tail(run_id, lines=20))
    assert "pending without active worker detected" in log
    assert "automatic recovery dispatch launched" in log


def test_automatic_recovery_survives_repository_and_service_restart(tmp_path, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    original_status = repository.status(run_id)
    backend = FakeBackend()
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    restarted = RunService(RunRepository(repository.root), backend=backend)
    report = restarted.recover_pending_forward_dispatches()

    assert report["dispatched"] == [run_id]
    assert repository.status(run_id)["created_at"] == original_status["created_at"]
    assert backend.launches == [run_id]


def test_two_recovery_processes_dispatch_one_forward_once(tmp_path):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    launch_log = tmp_path / "launches.txt"
    context = multiprocessing.get_context("spawn")
    barrier = context.Barrier(3)
    reports = context.Queue()
    processes = [
        context.Process(
            target=_automatic_recovery_process,
            args=(str(repository.root), str(launch_log), barrier, reports),
        )
        for _ in range(2)
    ]
    for process in processes:
        process.start()
    barrier.wait(timeout=15)
    observed = [reports.get(timeout=15), reports.get(timeout=15)]
    for process in processes:
        process.join(timeout=15)

    assert all(process.exitcode == 0 for process in processes)
    assert launch_log.read_text(encoding="utf-8").splitlines() == [run_id]
    assert sum(run_id in report["dispatched"] for report in observed) == 1
    assert sum(run_id in report["active"] for report in observed) == 1


def test_recovery_mutex_is_released_when_dispatch_process_dies(tmp_path, monkeypatch):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    context = multiprocessing.get_context("spawn")
    ready = context.Event()
    process = context.Process(
        target=_exit_while_holding_submission_mutex,
        args=(str(repository.root), ready),
    )
    process.start()
    assert ready.wait(timeout=10)
    process.join(timeout=10)
    assert process.exitcode == 0
    backend = FakeBackend()
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()

    assert report["dispatched"] == [run_id]
    assert backend.launches == [run_id]


def test_automatic_scan_ignores_forward_already_waiting_with_live_run_lease(
    tmp_path, monkeypatch
):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    lock = repository.run_directory(run_id) / ".worker.lock"
    lock.mkdir()
    (lock / "owner.json").write_text(
        json.dumps({"run_id": run_id, "pid": os.getpid()}), encoding="utf-8"
    )
    queue = repository.root / ".slots" / "queue"
    queue.mkdir(parents=True)
    (queue / f"{run_id}.json").write_text(
        json.dumps({
            "run_id": run_id,
            "queued_at": "2026-09-25T00:00:00+00:00",
            "waiter_pid": os.getpid(),
        }),
        encoding="utf-8",
    )
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == os.getpid())
    backend = FakeBackend()

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()

    assert report["active"] == [run_id]
    assert backend.launches == []


@pytest.mark.parametrize(
    "state", [JobStatus.RUNNING, JobStatus.COMPLETED, JobStatus.FAILED, JobStatus.CANCELLED]
)
def test_automatic_scan_never_redispatches_non_pending_forward(tmp_path, state):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    if state is not JobStatus.CANCELLED:
        repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
    if state not in {JobStatus.RUNNING}:
        repository.transition(run_id, state, error="terminal")
    backend = FakeBackend()

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()

    assert report["inspected"] == 0
    assert backend.launches == []


def test_invalid_pending_forward_is_logged_and_not_dispatched(tmp_path):
    repository, parent_id, run_id = _forward_run(tmp_path)
    (repository.run_directory(parent_id) / "results" / SNAPSHOT_FILENAME).unlink()
    backend = FakeBackend()

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()

    assert run_id in report["invalid"]
    assert repository.status(run_id)["status"] == JobStatus.PENDING.value
    assert backend.launches == []
    assert "invalid or incomplete" in "\n".join(repository.log_tail(run_id, lines=10))


def test_pending_forward_with_cancellation_request_is_not_dispatched(tmp_path):
    repository, _parent_id, run_id = _forward_run(tmp_path)
    repository.request_cancellation(run_id)
    backend = FakeBackend()

    report = RunService(repository, backend=backend).recover_pending_forward_dispatches()

    assert report["invalid"][run_id] == "forward_dispatch_cancelled"
    assert backend.launches == []


def test_launch_failure_remains_pending_and_is_retried(tmp_path, monkeypatch):
    class FailOnceBackend(FakeBackend):
        def launch(self, runs_root, run_id, max_concurrent_jobs):
            if not self.launches:
                self.launches.append("failed")
                raise OSError("temporary launch failure")
            return super().launch(runs_root, run_id, max_concurrent_jobs)

    repository, _parent_id, run_id = _forward_run(tmp_path)
    backend = FailOnceBackend()
    service = RunService(repository, backend=backend)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    first = service.recover_pending_forward_dispatches()
    second = service.recover_pending_forward_dispatches()

    assert run_id in first["errors"]
    assert second["dispatched"] == [run_id]
    assert repository.status(run_id)["dispatch_attempt_count"] == 2
    assert repository.status(run_id)["status"] == JobStatus.PENDING.value


def test_three_orphaned_forwards_are_all_automatically_dispatched(tmp_path, monkeypatch):
    runs = [_forward_run(tmp_path)[2] for _ in range(3)]
    repository = RunRepository(tmp_path / "runs")
    backend = FakeBackend()
    monkeypatch.setattr(runner_module, "_pid_alive", lambda pid: pid == 9090)

    report = RunService(
        repository, backend=backend, max_concurrent_heavy_jobs=1
    ).recover_pending_forward_dispatches()

    assert report["dispatched"] == runs
    assert backend.launches == runs
    assert all(
        repository.status(run_id)["dispatch_state"] == "launched"
        for run_id in runs
    )
