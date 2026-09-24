import hashlib
import json
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
