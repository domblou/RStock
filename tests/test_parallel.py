import time

import pytest

from rstock.parallel import (
    iter_combination_batches,
    iter_ordered_process_results,
    process_cancellation_requested,
    run_combination_tasks,
)
from rstock.progress import CancellationRequested


_TEST_CONTEXT: dict[int, float] | None = None
_STALL_MARKER = None


def _set_test_context(context: dict[int, float]) -> None:
    global _TEST_CONTEXT
    _TEST_CONTEXT = context


def _process_task(item: int) -> int:
    assert _TEST_CONTEXT is not None
    time.sleep(_TEST_CONTEXT[item])
    if process_cancellation_requested():
        raise CancellationRequested("cancelled in worker")
    return item * 10


def _serial_task(item: int, context: dict[int, float], cancellation_check) -> int:
    if cancellation_check is not None and cancellation_check():
        raise CancellationRequested("cancelled serially")
    return item * 10


def _stalled_initializer(context) -> None:
    time.sleep(30)


def _set_stall_context(path):
    global _STALL_MARKER
    _STALL_MARKER = path


def _uncooperative_task(item):
    _STALL_MARKER.write_text("started")
    time.sleep(30)
    return item


def test_cancellation_terminates_a_task_that_does_not_poll_the_event(monkeypatch, tmp_path):
    import multiprocessing as mp
    import rstock.parallel as parallel
    before = {process.pid for process in mp.active_children()}
    marker = tmp_path / "task_started"
    monkeypatch.setattr(parallel, "POOL_STOP_SECONDS", .2)
    started = time.monotonic()
    with pytest.raises(CancellationRequested):
        run_combination_tasks(
            [0, 1], combination_workers=2, worker_context=marker,
            context_initializer=_set_stall_context, process_task=_uncooperative_task,
            serial_task=_serial_task, item_label=str, stage="test",
            cancellation_check=marker.exists,
        )
    assert marker.exists()
    assert time.monotonic() - started < 10
    assert {process.pid for process in mp.active_children()} <= before


def test_stalled_initializer_times_out_and_reaps_owned_workers(monkeypatch):
    import multiprocessing as mp
    import rstock.parallel as parallel
    before = {process.pid for process in mp.active_children()}
    monkeypatch.setattr(parallel, "POOL_STARTUP_SECONDS", 0.5)
    started = time.monotonic()
    with pytest.raises(TimeoutError, match="workers initialized"):
        list(iter_ordered_process_results(
            [0], workers=1, initializer=_stalled_initializer,
            context=None, task=_process_task,
        ))
    assert time.monotonic() - started < 10
    assert {process.pid for process in mp.active_children()} <= before


def test_consecutive_spawn_pools_do_not_use_manager_rpc(monkeypatch):
    import rstock.parallel as parallel
    monkeypatch.setattr(parallel.mp, "Manager", lambda: pytest.fail("Manager RPC must not be used"))
    for _ in range(3):
        assert run_combination_tasks(
            [0, 1, 2, 3], combination_workers=4, worker_context={item: .01 for item in range(4)},
            context_initializer=_set_test_context, process_task=_process_task,
            serial_task=_serial_task, item_label=str, stage="test",
        ) == [0, 10, 20, 30]


def test_parallel_combination_results_are_input_ordered_and_progress_is_completion_based():
    progress = []
    results = run_combination_tasks(
        [0, 1],
        combination_workers=2,
        worker_context={0: 0.10, 1: 0.01},
        context_initializer=_set_test_context,
        process_task=_process_task,
        serial_task=_serial_task,
        item_label=str,
        stage="test",
        progress_callback=progress.append,
    )

    assert results == [0, 10]
    assert [event.completed_units for event in progress] == [0, 1, 2]
    assert progress[1].substage == "1"  # task 1 actually completed first


def test_parallel_combination_cancellation_stops_without_partial_result():
    with pytest.raises(CancellationRequested):
        run_combination_tasks(
            [0, 1],
            combination_workers=2,
            worker_context={0: 0.20, 1: 0.20},
            context_initializer=_set_test_context,
            process_task=_process_task,
            serial_task=_serial_task,
            item_label=str,
            stage="test",
            cancellation_check=lambda: True,
        )


def test_combination_batches_preserve_input_order_and_batch_boundaries():
    batches = list(iter_combination_batches(
        [0, 1, 2, 3, 4],
        batch_size=2,
        combination_workers=2,
        worker_context={item: 0.01 for item in range(5)},
        context_initializer=_set_test_context,
        process_task=_process_task,
        serial_task=_serial_task,
        item_label=str,
        stage="test",
    ))

    assert batches == [[0, 10], [20, 30], [40]]


def test_ordered_process_results_keep_input_order_with_bounded_work():
    results = list(iter_ordered_process_results(
        [0, 1, 2, 3], workers=2,
        initializer=_set_test_context,
        context={0: 0.15, 1: 0.01, 2: 0.01, 3: 0.01},
        task=_process_task,
    ))
    assert results == [0, 10, 20, 30]
