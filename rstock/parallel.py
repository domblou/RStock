"""Bounded, deterministic process-level execution for independent combinations."""

from __future__ import annotations

import multiprocessing as mp
import os
import logging
from contextlib import contextmanager
from queue import Empty
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
POOL_STARTUP_SECONDS = 60.0
POOL_STOP_SECONDS = 5.0
LOGGER = logging.getLogger(__name__)


def _pool_probe() -> None:
    """Trigger spawn; readiness is acknowledged by every initializer."""


@contextmanager
def _process_pool(workers: int, initializer: Callable[[Any], None], context: Any,
                  cancellation_check: CancellationCheck | None = None):
    # Synchronization primitives can be inherited through spawn's initargs.
    # They must not be sent through the executor's *task* queue. A Manager
    # adds a server and synchronous RPC to initialization and cancellation.
    spawn = mp.get_context("spawn")
    cancellation = spawn.Event()
    ready = spawn.Queue()
    start_gate = spawn.Event()
    executor = ProcessPoolExecutor(
        max_workers=workers, mp_context=spawn,
        initializer=_initialize_process_worker,
        initargs=(cancellation, initializer, context, ready, start_gate),
    )
    failed = True
    try:
        check_cancellation(cancellation_check)
        started = perf_counter()
        LOGGER.info("Process pool startup requested workers=%s", workers)
        deadline = perf_counter() + POOL_STARTUP_SECONDS
        probes = [executor.submit(_pool_probe) for _ in range(workers)]
        acknowledged: set[int] = set()
        while len(acknowledged) < workers:
            check_cancellation(cancellation_check)
            for probe in probes:
                if probe.done():
                    probe.result()  # Surface an initializer/process failure.
            remaining = deadline - perf_counter()
            if remaining <= 0:
                raise TimeoutError(
                    f"Process pool startup timed out: {len(acknowledged)}/{workers} "
                    f"workers initialized after {POOL_STARTUP_SECONDS:g}s"
                )
            try:
                acknowledged.add(ready.get(timeout=min(0.1, remaining)))
            except Empty:
                pass
        start_gate.set()
        LOGGER.info("Process pool ready workers=%s startup_seconds=%.3f pids=%s",
                    workers, perf_counter() - started, sorted(acknowledged))
        yield executor, cancellation
        failed = False
    finally:
        if failed:
            cancellation.set()
            # shutdown(wait=True) alone can wait forever for a stalled native
            # initializer/task. Terminate only this executor's owned processes.
            processes = list((executor._processes or {}).values())
            for process in processes:
                if process.is_alive():
                    process.terminate()
            deadline = perf_counter() + POOL_STOP_SECONDS
            for process in processes:
                process.join(timeout=max(0.0, deadline - perf_counter()))
            executor.shutdown(wait=False, cancel_futures=True)
        else:
            executor.shutdown(wait=True)
        ready.close()
        ready.join_thread()


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


def iter_ordered_process_results(
    items: Sequence[Item],
    *,
    workers: int,
    initializer: Callable[[Any], None],
    context: Any,
    task: Callable[[Item], Result],
    cancellation_check: CancellationCheck | None = None,
) -> Iterator[Result]:
    """Bound in-flight process work and yield only in canonical input order.

    Workers never write checkpoints. The caller can persist each yielded result
    before requesting the next one, including after a resumed run.
    """
    if workers < 1:
        raise ValueError("workers must be positive")
    if not items:
        return
    futures: dict[Any, int] = {}
    ready: dict[int, Result] = {}
    next_submit = 0
    next_yield = 0
    with _process_pool(min(workers, len(items)), initializer, context, cancellation_check) as (executor, _):
        while next_yield < len(items):
            check_cancellation(cancellation_check)
            while (
                next_submit < len(items)
                and len(futures) + len(ready) < 2 * workers
            ):
                future = executor.submit(task, items[next_submit])
                futures[future] = next_submit
                next_submit += 1
            while next_yield in ready:
                yield ready.pop(next_yield)
                next_yield += 1
            if next_yield == len(items):
                break
            done, _ = wait(futures, timeout=0.1, return_when=FIRST_COMPLETED)
            for future in done:
                ready[futures.pop(future)] = future.result()


def _initialize_process_worker(
    cancellation_event: Any,
    context_initializer: Callable[[Any], None],
    context: Any,
    ready: Any = None,
    start_gate: Any = None,
) -> None:
    global _CANCELLATION_EVENT
    _CANCELLATION_EVENT = cancellation_event
    context_initializer(context)
    if ready is not None:
        ready.put(os.getpid())
    if start_gate is not None:
        # Keep startup probes busy until all initializers acknowledge. Without
        # this barrier the executor may reuse an idle worker for later probes
        # instead of spawning the full requested pool.
        if not start_gate.wait(timeout=POOL_STARTUP_SECONDS + POOL_STOP_SECONDS):
            raise TimeoutError("Process pool startup barrier was not released")


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

    cancelled = False
    with _process_pool(min(combination_workers, total), context_initializer, worker_context, cancellation_check) as (executor, cancellation_event):
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
            cancellation_deadline = None
            while futures:
                if cancellation_check is not None and cancellation_check():
                    cancelled = True
                    cancellation_event.set()
                    if cancellation_deadline is None:
                        cancellation_deadline = perf_counter() + POOL_STOP_SECONDS
                    for future in futures:
                        future.cancel()
                if cancellation_deadline is not None and perf_counter() >= cancellation_deadline:
                    raise CancellationRequested("Process pool cancellation timed out; stopping owned workers")
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
