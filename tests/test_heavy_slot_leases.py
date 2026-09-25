from __future__ import annotations

import json
import os
import threading
from dataclasses import replace

import pytest

import rstock.application.worker as worker_module
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.application.worker import SlotLease
from rstock.config import DEFAULT_CONFIG


def _run(repository: RunRepository, root, suffix: str) -> str:
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=root),
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
        run_description=suffix,
    )
    return repository.create(spec)


class _ObservedSlotLease(SlotLease):
    def __init__(self, *args, observed: threading.Event, **kwargs):
        super().__init__(*args, **kwargs)
        self.observed = observed

    def _slot_guard_busy(self, slot):
        self.observed.set()
        super()._slot_guard_busy(slot)

    def _queue_guard_busy(self):
        self.observed.set()
        super()._queue_guard_busy()

    def _log_state(self, slot, state, message):
        if state in {"occupied", "initializing"}:
            self.observed.set()
        super()._log_state(slot, state, message)


def test_simultaneous_acquisition_has_one_owner_and_no_file_not_found(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    contended = threading.Event()
    leases = [
        _ObservedSlotLease(
            repository, _run(repository, tmp_path, suffix), 1,
            poll_seconds=0.001, observed=contended,
        )
        for suffix in ("one", "two")
    ]
    start = threading.Barrier(3)
    release_first = threading.Event()
    first_acquired = threading.Event()
    acquired: list[str] = []
    errors: list[BaseException] = []
    state_lock = threading.Lock()

    def compete(lease: SlotLease) -> None:
        try:
            start.wait(timeout=3)
            lease.acquire()
            with state_lock:
                acquired.append(lease.run_id)
                first = len(acquired) == 1
            if first:
                first_acquired.set()
                assert release_first.wait(timeout=3)
            lease.release()
        except BaseException as error:  # surfaced below with full assertion context
            errors.append(error)

    threads = [threading.Thread(target=compete, args=(lease,)) for lease in leases]
    for thread in threads:
        thread.start()
    start.wait(timeout=3)
    assert first_acquired.wait(timeout=3)
    assert contended.wait(timeout=3)
    assert len(acquired) == 1
    owner = json.loads(
        (repository.root / ".slots" / "slot-0" / "owner.json").read_text(encoding="utf-8")
    )
    assert owner["run_id"] == acquired[0]
    release_first.set()
    for thread in threads:
        thread.join(timeout=5)

    assert not errors
    assert all(not thread.is_alive() for thread in threads)
    assert len(acquired) == 2


def test_directory_without_owner_is_protected_while_first_claim_initializes(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    created = threading.Event()
    allow_owner = threading.Event()
    release_first = threading.Event()
    first_acquired = threading.Event()
    second_observed = threading.Event()
    second_acquired = threading.Event()
    first = SlotLease(repository, _run(repository, tmp_path, "first"), 1, poll_seconds=0.001)
    second = _ObservedSlotLease(
        repository, _run(repository, tmp_path, "second"), 1,
        poll_seconds=0.001, observed=second_observed,
    )

    def pause_after_mkdir(_slot):
        created.set()
        assert allow_owner.wait(timeout=3)

    first._after_slot_directory_created = pause_after_mkdir

    def first_worker():
        first.acquire()
        first_acquired.set()
        assert release_first.wait(timeout=3)
        first.release()

    def second_worker():
        second.acquire()
        second_acquired.set()
        second.release()

    first_thread = threading.Thread(target=first_worker)
    first_thread.start()
    assert created.wait(timeout=3)
    slot = repository.root / ".slots" / "slot-0"
    assert slot.is_dir()
    assert not (slot / "owner.json").exists()

    second_thread = threading.Thread(target=second_worker)
    second_thread.start()
    assert second_observed.wait(timeout=3)
    assert slot.is_dir()
    assert not second_acquired.is_set()

    allow_owner.set()
    assert first_acquired.wait(timeout=3)
    assert json.loads((slot / "owner.json").read_text(encoding="utf-8"))["run_id"] == first.run_id
    release_first.set()
    first_thread.join(timeout=5)
    second_thread.join(timeout=5)

    assert not first_thread.is_alive()
    assert not second_thread.is_alive()
    assert second_acquired.is_set()


def test_dead_slot_owner_is_recovered(tmp_path, monkeypatch, caplog):
    repository = RunRepository(tmp_path / "runs")
    run_id = _run(repository, tmp_path, "replacement")
    slot = repository.root / ".slots" / "slot-0"
    slot.mkdir(parents=True)
    (slot / "owner.json").write_text(
        json.dumps({"pid": 999_999_999, "run_id": "dead", "token": "old"}),
        encoding="utf-8",
    )
    monkeypatch.setattr(worker_module, "_process_alive", lambda pid: False)
    lease = SlotLease(repository, run_id, 1, poll_seconds=0.001)

    with caplog.at_level("INFO"):
        lease.acquire()
    owner = json.loads((slot / "owner.json").read_text(encoding="utf-8"))

    assert owner["run_id"] == run_id
    assert owner["token"] == lease.token
    assert "stale owner detected" in caplog.text
    assert "stale owner recovered" in caplog.text
    lease.release()


def test_two_candidates_recover_one_stale_owner_safely(tmp_path, monkeypatch, caplog):
    repository = RunRepository(tmp_path / "runs")
    slot = repository.root / ".slots" / "slot-0"
    slot.mkdir(parents=True)
    (slot / "owner.json").write_text(
        json.dumps({"pid": 999_999_999, "run_id": "dead", "token": "old"}),
        encoding="utf-8",
    )
    monkeypatch.setattr(worker_module, "_process_alive", lambda pid: pid == os.getpid())
    contended = threading.Event()
    leases = [
        _ObservedSlotLease(
            repository, _run(repository, tmp_path, str(index)), 1,
            poll_seconds=0.001, observed=contended,
        )
        for index in range(2)
    ]
    start = threading.Barrier(3)
    first_ready = threading.Event()
    release_first = threading.Event()
    acquired: list[str] = []
    errors: list[BaseException] = []
    state_lock = threading.Lock()

    def recover(lease: SlotLease):
        try:
            start.wait(timeout=3)
            lease.acquire()
            with state_lock:
                acquired.append(lease.run_id)
                first = len(acquired) == 1
            if first:
                first_ready.set()
                assert release_first.wait(timeout=3)
            lease.release()
        except BaseException as error:
            errors.append(error)

    with caplog.at_level("INFO"):
        threads = [threading.Thread(target=recover, args=(lease,)) for lease in leases]
        for thread in threads:
            thread.start()
        start.wait(timeout=3)
        assert first_ready.wait(timeout=3)
        assert contended.wait(timeout=3)
        assert len(acquired) == 1
        release_first.set()
        for thread in threads:
            thread.join(timeout=5)

    assert not errors
    assert all(not thread.is_alive() for thread in threads)
    assert len(acquired) == 2
    assert caplog.text.count("stale owner recovered") == 1


def test_old_releaser_cannot_delete_reassigned_slot(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    first = SlotLease(repository, _run(repository, tmp_path, "old"), 1, poll_seconds=0.001)
    second = SlotLease(repository, _run(repository, tmp_path, "new"), 1, poll_seconds=0.001)
    first.acquire()
    old_path, old_token = first.path, first.token
    first.release()
    second.acquire()

    delayed_old_release = SlotLease(repository, first.run_id, 1, poll_seconds=0.001)
    delayed_old_release.path = old_path
    delayed_old_release.token = old_token
    delayed_old_release.release()
    owner = json.loads((second.path / "owner.json").read_text(encoding="utf-8"))

    assert owner["run_id"] == second.run_id
    assert owner["token"] == second.token
    second.release()


def test_repeated_single_slot_contention_never_overlaps_owners(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    leases = [
        SlotLease(repository, _run(repository, tmp_path, str(index)), 1, poll_seconds=0.001)
        for index in range(6)
    ]
    start = threading.Barrier(len(leases) + 1)
    state_lock = threading.Lock()
    active = 0
    maximum_active = 0
    errors: list[BaseException] = []

    def exercise(lease: SlotLease):
        nonlocal active, maximum_active
        try:
            start.wait(timeout=3)
            for _ in range(20):
                lease.acquire()
                with state_lock:
                    active += 1
                    maximum_active = max(maximum_active, active)
                owner = json.loads((lease.path / "owner.json").read_text(encoding="utf-8"))
                assert owner["token"] == lease.token
                with state_lock:
                    active -= 1
                lease.release()
        except BaseException as error:
            errors.append(error)

    threads = [threading.Thread(target=exercise, args=(lease,)) for lease in leases]
    for thread in threads:
        thread.start()
    start.wait(timeout=3)
    for thread in threads:
        thread.join(timeout=15)

    assert not errors
    assert all(not thread.is_alive() for thread in threads)
    assert maximum_active == 1
