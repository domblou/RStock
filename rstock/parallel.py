"""Bounded, deterministic process-level execution for independent combinations."""

from __future__ import annotations

import multiprocessing as mp
from collections.abc import Callable, Iterator, Sequence
from concurrent.futures import FIRST_COMPLETED, ProcessPoolExecutor, wait
from dataclasses import dataclass
from time import perf_counter
from typing import Any, Generic, TypeVar

from .progress import (
    CancellationCheck,
    CancellationRequested,
    ProgressCallback,
    check_cancellation,
    report_progress,
)


Item = TypeVar("Item")
Result = TypeVar("Result")
Context = TypeVar("Context")

_CANCELLATION_EVENT: Any = None


@dataclass(slots=True)
class CombinationBatch(Generic[Result]):
    batch_id: int
    first_index: int
    last_index: int
    results: list[Result]
    elapsed_seconds: float = 0.0


def process_cancellation_requested() -> bool:
    """Return the parent cancellation state from inside a process worker."""

    return bool(_CANCELLATION_EVENT is not None and _CANCELLATION_EVENT.is_set())


def _initialize_process_worker(
    cancellation_event: Any,
    context_initializer: Callable[[Any], None],
    context: Any,
) -> None:
    global _CANCELLATION_EVENT
    _CANCELLATION_EVENT = cancellation_event
    context_initializer(context)


def run_combination_tasks(
    items: Sequence[Item],
    *,
    combination_workers: int,
    worker_context: Context,
    context_initializer: Callable[[Context], None],
    process_task: Callable[[Item], Result],
    serial_task: Callable[[Item, Context, CancellationCheck | None], Result],
    item_label: Callable[[Item], str],
    stage: str,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    details: dict[str, object] | None = None,
) -> list[Result]:
    """Run independent combination tasks and return their results in input order.

    Child processes never write aggregate files or progress.  The parent collects
    completed futures, reports true completion order, and returns the results in
    original input order so downstream metrics remain reproducible.
    """

    batches = iter_combination_batches(
        items,
        batch_size=max(len(items), 1),
        combination_workers=combination_workers,
        worker_context=worker_context,
        context_initializer=context_initializer,
        process_task=process_task,
        serial_task=serial_task,
        item_label=item_label,
        stage=stage,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details=details,
    )
    return [result for batch in batches for result in batch]


def iter_combination_batches(
    items: Sequence[Item],
    *,
    batch_size: int,
    combination_workers: int,
    worker_context: Context,
    context_initializer: Callable[[Context], None],
    process_task: Callable[[Item], Result],
    serial_task: Callable[[Item, Context, CancellationCheck | None], Result],
    item_label: Callable[[Item], str],
    stage: str,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    details: dict[str, object] | None = None,
) -> Iterator[list[Result]]:
    """Yield deterministic result batches while keeping one process pool alive."""

    for batch in iter_indexed_combination_batches(
        items,
        batch_size=batch_size,
        combination_workers=combination_workers,
        worker_context=worker_context,
        context_initializer=context_initializer,
        process_task=process_task,
        serial_task=serial_task,
        item_label=item_label,
        stage=stage,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details=details,
    ):
        yield batch.results


def iter_indexed_combination_batches(
    items: Sequence[Item],
    *,
    batch_size: int,
    combination_workers: int,
    worker_context: Context,
    context_initializer: Callable[[Context], None],
    process_task: Callable[[Item], Result],
    serial_task: Callable[[Item, Context, CancellationCheck | None], Result],
    item_label: Callable[[Item], str],
    stage: str,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    details: dict[str, object] | None = None,
    completed_batch_ids: Sequence[int] = (),
) -> Iterator[CombinationBatch[Result]]:
    """Yield indexed batches, omitting checkpointed batch identifiers."""

    if combination_workers < 1:
        raise ValueError("combination_workers must be positive")
    if batch_size < 1:
        raise ValueError("batch_size must be positive")
    total = len(items)
    skipped = set(int(value) for value in completed_batch_ids)
    completed_initial = sum(
        min(batch_size, total - start)
        for batch_id, start in enumerate(range(0, total, batch_size))
        if batch_id in skipped
    )
    report_progress(
        progress_callback,
        stage,
        substage="combinations",
        completed_units=completed_initial,
        total_units=total,
        details=details or {},
    )
    if total == 0:
        return
    total_batch_count = (total + batch_size - 1) // batch_size
    if set(range(total_batch_count)) <= skipped:
        return

    if combination_workers == 1:
        completed = completed_initial
        for batch_start in range(0, total, batch_size):
            batch_id = batch_start // batch_size
            if batch_id in skipped:
                continue
            batch_started_at = perf_counter()
            batch_results: list[Result] = []
            for item in items[batch_start : batch_start + batch_size]:
                check_cancellation(cancellation_check)
                batch_results.append(serial_task(item, worker_context, cancellation_check))
                completed += 1
                report_progress(
                    progress_callback,
                    stage,
                    substage=item_label(item),
                    completed_units=completed,
                    total_units=total,
                    details=details or {},
                )
            yield CombinationBatch(
                batch_id,
                batch_start,
                min(batch_start + batch_size, total) - 1,
                batch_results,
                perf_counter() - batch_started_at,
            )
        return

    # A Manager Event is picklable under the Windows spawn start method, unlike a
    # bare synchronization Event passed through an executor task queue.
    manager = mp.Manager()
    cancellation_event = manager.Event()
    cancelled = False
    executor = ProcessPoolExecutor(
        max_workers=min(combination_workers, total),
        mp_context=mp.get_context("spawn"),
        initializer=_initialize_process_worker,
        initargs=(cancellation_event, context_initializer, worker_context),
    )
    try:
        completed = completed_initial
        for batch_start in range(0, total, batch_size):
            batch_end = min(batch_start + batch_size, total)
            batch_id = batch_start // batch_size
            if batch_id in skipped:
                continue
            batch_started_at = perf_counter()
            next_index = batch_start
            futures: dict[Any, int] = {}
            results_by_index: dict[int, Result] = {}

            def submit_available() -> None:
                nonlocal next_index
                while (
                    not cancelled
                    and next_index < batch_end
                    and len(futures) < combination_workers
                ):
                    future = executor.submit(process_task, items[next_index])
                    futures[future] = next_index
                    next_index += 1

            submit_available()
            while futures:
                if cancellation_check is not None and cancellation_check():
                    cancelled = True
                    cancellation_event.set()
                    for future in futures:
                        future.cancel()
                done, _ = wait(futures, timeout=0.05, return_when=FIRST_COMPLETED)
                for future in done:
                    index = futures.pop(future)
                    if future.cancelled():
                        continue
                    try:
                        results_by_index[index] = future.result()
                    except CancellationRequested:
                        if cancelled:
                            continue
                        raise
                    completed += 1
                    report_progress(
                        progress_callback,
                        stage,
                        substage=item_label(items[index]),
                        completed_units=completed,
                        total_units=total,
                        details=details or {},
                    )
                if not cancelled:
                    submit_available()
            if cancelled:
                raise CancellationRequested("Cancellation requested")
            yield CombinationBatch(
                batch_id,
                batch_start,
                batch_end - 1,
                [results_by_index[index] for index in range(batch_start, batch_end)],
                perf_counter() - batch_started_at,
            )
    finally:
        if cancelled:
            cancellation_event.set()
            executor.shutdown(wait=True, cancel_futures=True)
        else:
            executor.shutdown(wait=True)
        manager.shutdown()
