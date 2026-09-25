from __future__ import annotations

import multiprocessing
import threading
from dataclasses import replace
from pathlib import Path

import rstock.application.worker as worker_module
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application.worker import SlotLease
from rstock.config import DEFAULT_CONFIG


def _spec(root: Path, job_type: JobType = JobType.WALK_FORWARD) -> ExperimentSpec:
    values: dict[str, object] = {}
    if job_type is JobType.FORWARD_SIMULATION:
        values.update(
            source_end_to_end_run="source-end-to-end",
            forward_simulation_start_date="2026-01-02",
            forward_simulation_end_date="2026-02-02",
        )
    return ExperimentSpec(
        job_type=job_type,
        config=replace(DEFAULT_CONFIG, project_root=root),
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
        **values,
    )


def _run(
    repository: RunRepository,
    root: Path,
    job_type: JobType = JobType.WALK_FORWARD,
) -> str:
    return repository.create(_spec(root, job_type))


def _enqueue(lease: SlotLease) -> dict[str, object]:
    with worker_module._file_mutex(lease._queue_guard):
        return lease._ensure_queue_ticket_locked()


def _run_threads_in_queue_order(
    holder: SlotLease,
    leases: list[SlotLease],
) -> tuple[list[str], list[BaseException]]:
    start = threading.Barrier(len(leases) + 1)
    order: list[str] = []
    errors: list[BaseException] = []
    order_lock = threading.Lock()

    def execute(lease: SlotLease) -> None:
        try:
            start.wait(timeout=5)
            lease.acquire()
            with order_lock:
                order.append(lease.run_id)
            lease.release()
        except BaseException as error:  # reported by the caller
            errors.append(error)

    threads = [threading.Thread(target=execute, args=(lease,)) for lease in leases]
    for thread in threads:
        thread.start()
    start.wait(timeout=5)
    holder.release()
    for thread in threads:
        thread.join(timeout=10)
    assert all(not thread.is_alive() for thread in threads)
    return order, errors


def _process_waiter(
    runs_root: str,
    run_id: str,
    barrier,
    acquired,
    release,
) -> None:
    repository = RunRepository(Path(runs_root))
    lease = SlotLease(repository, run_id, 1, poll_seconds=0.002)
    _enqueue(lease)
    barrier.wait(timeout=10)
    lease.acquire()
    acquired.put(run_id)
    release.wait(timeout=10)
    lease.release()


def test_fifo_starts_three_pending_jobs_in_persisted_order(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    holder.acquire()
    leases = [
        SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
        for _ in range(3)
    ]
    tickets = [_enqueue(lease) for lease in leases]

    order, errors = _run_threads_in_queue_order(holder, leases)

    assert not errors
    assert [ticket["queue_sequence"] for ticket in tickets] == sorted(
        ticket["queue_sequence"] for ticket in tickets
    )
    assert order == [lease.run_id for lease in leases]


def test_existing_waiter_precedes_jobs_added_while_it_waits(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    holder.acquire()
    waiting = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    _enqueue(waiting)
    newcomers = [
        SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
        for _ in range(5)
    ]
    for lease in newcomers:
        _enqueue(lease)

    order, errors = _run_threads_in_queue_order(holder, [waiting, *newcomers])

    assert not errors
    assert order[0] == waiting.run_id
    assert order[1:] == [lease.run_id for lease in newcomers]


def test_forward_created_after_waiting_end_to_end_does_not_jump_queue(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(
        repository,
        _run(repository, tmp_path, JobType.END_TO_END),
        1,
        poll_seconds=0.001,
    )
    holder.acquire()
    end_to_end = SlotLease(
        repository,
        _run(repository, tmp_path, JobType.END_TO_END),
        1,
        poll_seconds=0.001,
    )
    forward = SlotLease(
        repository,
        _run(repository, tmp_path, JobType.FORWARD_SIMULATION),
        1,
        poll_seconds=0.001,
    )
    _enqueue(end_to_end)
    _enqueue(forward)

    order, errors = _run_threads_in_queue_order(holder, [forward, end_to_end])

    assert not errors
    assert order == [end_to_end.run_id, forward.run_id]


def test_two_processes_select_only_the_persisted_fifo_head(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    holder.acquire()
    run_ids = [_run(repository, tmp_path) for _ in range(2)]
    for run_id in run_ids:
        _enqueue(SlotLease(repository, run_id, 1, poll_seconds=0.001))

    context = multiprocessing.get_context("spawn")
    barrier = context.Barrier(3)
    acquired = context.Queue()
    release = context.Event()
    processes = [
        context.Process(
            target=_process_waiter,
            args=(str(repository.root), run_id, barrier, acquired, release),
        )
        for run_id in reversed(run_ids)
    ]
    for process in processes:
        process.start()
    barrier.wait(timeout=15)
    holder.release()

    first = acquired.get(timeout=15)
    assert first == run_ids[0]
    assert acquired.empty()
    release.set()
    second = acquired.get(timeout=15)
    for process in processes:
        process.join(timeout=15)

    assert second == run_ids[1]
    assert all(process.exitcode == 0 for process in processes)


def test_waiting_order_survives_worker_restart(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    holder.acquire()
    first_run = _run(repository, tmp_path)
    second_run = _run(repository, tmp_path)
    original = SlotLease(repository, first_run, 1, poll_seconds=0.001)
    second = SlotLease(repository, second_run, 1, poll_seconds=0.001)
    original_ticket = _enqueue(original)
    _enqueue(second)

    restarted = SlotLease(
        RunRepository(repository.root), first_run, 1, poll_seconds=0.001
    )
    restarted_ticket = _enqueue(restarted)
    order, errors = _run_threads_in_queue_order(holder, [second, restarted])

    assert not errors
    assert restarted_ticket["queued_at"] == original_ticket["queued_at"]
    assert restarted_ticket["queue_sequence"] == original_ticket["queue_sequence"]
    assert order == [first_run, second_run]


def test_cancelled_and_invalid_pending_jobs_do_not_block_next_waiter(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    holder = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    holder.acquire()
    cancelled_id = _run(repository, tmp_path)
    invalid_id = _run(repository, tmp_path)
    valid = SlotLease(repository, _run(repository, tmp_path), 1, poll_seconds=0.001)
    cancelled = SlotLease(repository, cancelled_id, 1, poll_seconds=0.001)
    invalid = SlotLease(repository, invalid_id, 1, poll_seconds=0.001)
    for lease in (cancelled, invalid, valid):
        _enqueue(lease)
    repository.transition(cancelled_id, JobStatus.CANCELLED)
    invalid_status = repository.status(invalid_id)
    invalid_status["status"] = "invalid"
    repository.write_json(invalid_id, "status.json", invalid_status)

    order, errors = _run_threads_in_queue_order(holder, [valid])

    assert not errors
    assert order == [valid.run_id]
    assert not cancelled._queue_path.exists()
    assert not invalid._queue_path.exists()
