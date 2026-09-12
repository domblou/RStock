"""Bounded, deterministic process-level execution for independent combinations."""

from __future__ import annotations

import multiprocessing as mp
from collections.abc import Callable, Sequence
from concurrent.futures import FIRST_COMPLETED, ProcessPoolExecutor, wait
from typing import Any, TypeVar

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

    if combination_workers < 1:
        raise ValueError("combination_workers must be positive")
    total = len(items)
    report_progress(
        progress_callback,
        stage,
        substage="combinations",
        completed_units=0,
        total_units=total,
        details=details or {},
    )
    if total == 0:
        return []

    if combination_workers == 1:
        results: list[Result] = []
        for completed, item in enumerate(items, start=1):
            check_cancellation(cancellation_check)
            results.append(serial_task(item, worker_context, cancellation_check))
            report_progress(
                progress_callback,
                stage,
                substage=item_label(item),
                completed_units=completed,
                total_units=total,
                details=details or {},
            )
        return results

    # A Manager Event is picklable under the Windows spawn start method, unlike a
    # bare synchronization Event passed through an executor task queue.
    manager = mp.Manager()
    cancellation_event = manager.Event()
    results_by_index: dict[int, Result] = {}
    cancelled = False
    executor = ProcessPoolExecutor(
        max_workers=min(combination_workers, total),
        mp_context=mp.get_context("spawn"),
        initializer=_initialize_process_worker,
        initargs=(cancellation_event, context_initializer, worker_context),
    )
    try:
        next_index = 0
        futures: dict[Any, int] = {}

        def submit_available() -> None:
            nonlocal next_index
            while not cancelled and next_index < total and len(futures) < combination_workers:
                future = executor.submit(process_task, items[next_index])
                futures[future] = next_index
                next_index += 1

        submit_available()
        completed = 0
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
        return [results_by_index[index] for index in range(total)]
    finally:
        if cancelled:
            cancellation_event.set()
            executor.shutdown(wait=True, cancel_futures=True)
        else:
            executor.shutdown(wait=True)
        manager.shutdown()
