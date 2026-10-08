from __future__ import annotations

import os
import time

import pytest

from rstock.application.processes import process_alive
from rstock.application import processes
from rstock.application.runner import _pid_alive
from rstock.application.worker import _process_alive
from rstock.application.workflows import _worker_pid_alive


def test_liveness_probes_are_safe_for_the_calling_process():
    current_pid = os.getpid()

    assert process_alive(current_pid)
    assert _pid_alive(current_pid)
    assert _process_alive(current_pid)
    assert _worker_pid_alive(current_pid)


def test_liveness_probe_rejects_invalid_pids():
    for value in (None, "invalid", 0, -1):
        assert process_alive(value) is False


def test_creation_probe_is_non_destructive_for_current_process():
    created = processes.process_creation_time(os.getpid())
    if os.name == "nt" or os.path.exists("/proc/self/stat"):
        assert created is not None
        assert created <= time.time()
        assert processes.process_identity_matches(os.getpid(), expected_created_at=created)
        assert not processes.process_identity_matches(os.getpid(), expected_created_at=created - 1)
    assert process_alive(os.getpid())
    for pid in (None, "invalid", 0, -1):
        assert processes.process_creation_time(pid) is None


@pytest.mark.parametrize("expected,boundary,matches", [
    (100.0, None, True), (90.0, None, False),
    (None, "1970-01-01T00:01:30+00:00", False),
    (None, "1970-01-01T00:01:50+00:00", True),
    (None, None, True), (None, "invalid", True),
    (None, "1970-01-01T00:01:30", True),
    (float("nan"), None, True), (float("inf"), None, True),
])
def test_pid_identity_and_historical_completion_boundary(monkeypatch, expected, boundary, matches):
    monkeypatch.setattr(processes, "process_creation_time", lambda _: 100.0)
    assert processes.process_identity_matches(123, expected_created_at=expected, existed_by=boundary) is matches


def test_unknown_os_identity_keeps_worker_protected(monkeypatch):
    monkeypatch.setattr(processes, "process_creation_time", lambda _: None)
    assert processes.process_identity_matches(123, expected_created_at=90.0)


def test_run_lease_reclaims_reused_pid_and_old_release_cannot_erase_new_owner(tmp_path, monkeypatch):
    import json
    from types import SimpleNamespace
    from rstock.application import worker
    directory = tmp_path / "run"
    owner = directory / ".worker.lock/owner.json"
    owner.parent.mkdir(parents=True)
    owner.write_text(json.dumps({"pid": 123, "pid_created_at": 100.0, "run_id": "run", "token": "old"}))
    monkeypatch.setattr(worker, "_process_alive", lambda _: True)
    monkeypatch.setattr(processes, "process_creation_time", lambda _: 200.0)
    monkeypatch.setattr(worker, "process_creation_time", lambda _: 200.0)
    repository = SimpleNamespace(run_directory=lambda _: directory)
    old = worker.RunLease(repository, "run")
    old.acquired, old.token = True, "old"
    new = worker.RunLease(repository, "run")
    new.acquire()
    fresh = json.loads(owner.read_text())
    assert fresh["pid_created_at"] == 200.0
    assert fresh["token"] != "old"
    old.release()
    assert json.loads(owner.read_text()) == fresh
    new.release()
    assert not owner.parent.exists()


def test_run_lease_still_blocks_real_owner(tmp_path, monkeypatch):
    import json
    from types import SimpleNamespace
    from rstock.application import worker
    directory = tmp_path / "run"
    owner = directory / ".worker.lock/owner.json"
    owner.parent.mkdir(parents=True)
    owner.write_text(json.dumps({"pid": 123, "pid_created_at": 100.0, "run_id": "run", "token": "real"}))
    monkeypatch.setattr(worker, "_process_alive", lambda _: True)
    monkeypatch.setattr(processes, "process_creation_time", lambda _: 100.0)
    lease = worker.RunLease(SimpleNamespace(run_directory=lambda _: directory), "run")
    with pytest.raises(RuntimeError):
        lease.acquire()
    assert json.loads(owner.read_text())["token"] == "real"
