import time

import pytest

from rstock.parallel import (
    iter_combination_batches,
    process_cancellation_requested,
    run_combination_tasks,
)
from rstock.progress import CancellationRequested


_TEST_CONTEXT: dict[int, float] | None = None


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
