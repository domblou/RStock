"""Independent local worker process for a persisted RStock run."""

from __future__ import annotations

import argparse
import errno
import json
import logging
import os
import time
import traceback
import uuid
from contextlib import contextmanager
from dataclasses import replace
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterator

from rstock.progress import CancellationRequested, check_cancellation
from rstock.checkpoints import CheckpointManager

from .domain import JobStatus, JobType
from .orchestration_runtime import child_executor_context
from .processes import process_alive
from .repository import RunRepository
from .runner import LocalProcessBackend, ProgressReporter, RunService
from .workflows import WorkflowRegistry


LOGGER = logging.getLogger(__name__)
SLOT_INITIALIZATION_GRACE_SECONDS = 30.0
HEAVY_QUEUE_DIRECTORY = "queue"
HEAVY_QUEUE_GUARD = "queue.guard"


WORKFLOW_PHASES: dict[JobType, list[tuple[str, float]]] = {
    JobType.WALK_FORWARD: [
        ("data_preparation", 8), ("predictor_prefilter_generation", 2),
        ("predictor_prefilter_walk_forward", 18),
        ("predictor_prefilter_selection", 2), ("combination_generation", 4),
        ("walk_forward", 47), ("aggregation", 7), ("qualification", 5),
        ("final_holdout", 3), ("metrics", 2), ("result_writing", 1),
        ("publishing", 1),
    ],
    JobType.XGBOOST_CALIBRATION: [
        ("data_preparation", 12), ("combination_generation", 6),
        ("walk_forward", 65), ("final_holdout", 8), ("metrics", 4),
        ("result_writing", 3), ("publishing", 2),
    ],
    JobType.WALK_FORWARD_BATCH: [
        ("walk_forward", 98), ("publishing", 2),
    ],
    JobType.THRESHOLD_PARAMETER_CALIBRATION: [
        ("data_preparation", 12), ("combination_generation", 6),
        ("walk_forward", 57), ("metrics", 20),
        ("result_writing", 3), ("publishing", 2),
    ],
    JobType.THRESHOLD_CALIBRATION: [
        ("data_preparation", 12), ("combination_generation", 6),
        ("walk_forward", 65), ("final_holdout", 8), ("metrics", 4),
        ("result_writing", 3), ("publishing", 2),
    ],
    JobType.FIXED_CANDIDATE_EVALUATION: [
        ("data_preparation", 25),
        ("combination_generation", 5),
        ("final_holdout", 60),
        ("result_writing", 8),
        ("publishing", 2),
    ],
    JobType.FORCED_CANDIDATE_VALIDATION: [
        ("walk_forward", 65),
        ("fixed_candidate_evaluation", 33),
        ("publishing", 2),
    ],
    JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC: [
        ("data_preparation", 25),
        ("combination_generation", 5),
        ("final_holdout", 60),
        ("result_writing", 8),
        ("publishing", 2),
    ],
    JobType.PRODUCTION_TRAINING: [
        ("data_preparation", 35), ("production_training", 60), ("publishing", 5),
    ],
    JobType.MARKET_UPDATE: [("market_update", 95), ("publishing", 5)],
    JobType.DAILY_PREDICTION: [
        ("data_preparation", 50), ("daily_prediction", 45), ("publishing", 5),
    ],
    JobType.DAILY_SCREENING: [("screening", 95), ("publishing", 5)],
    JobType.REALIZED_VALIDATION: [("realized_validation", 95), ("publishing", 5)],
    JobType.OPERATIONAL_RUN: [
        ("market_update", 40), ("daily_prediction", 28), ("screening", 10),
        ("realized_validation", 10), ("production_quality", 7), ("publishing", 5),
    ],
    JobType.PRODUCTION_QUALITY_REBUILD: [
        ("production_quality_rebuild", 98), ("publishing", 2),
    ],
    JobType.END_TO_END: [
        ("walk_forward", 22.5),
        ("xgboost_calibration", 22.5),
        ("threshold_parameter_calibration", 22.5),
        ("threshold_calibration", 22.5),
        ("promotion", 9),
        ("publishing", 1),
    ],
    JobType.FORWARD_SIMULATION: [
        ("forward_simulation", 98), ("publishing", 2),
    ],
}


def _process_alive(pid: int) -> bool:
    return process_alive(pid)


@contextmanager
def _try_file_mutex(path: Path) -> Iterator[bool]:
    """Try an OS-backed, cross-process exclusive lock on one stable byte."""

    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_RDWR | os.O_CREAT, 0o600)
    acquired = False
    try:
        if os.fstat(descriptor).st_size == 0:
            os.write(descriptor, b"\0")
            os.fsync(descriptor)
        os.lseek(descriptor, 0, os.SEEK_SET)
        try:
            if os.name == "nt":
                import msvcrt

                msvcrt.locking(descriptor, msvcrt.LK_NBLCK, 1)
            else:
                import fcntl

                fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
            acquired = True
        except OSError as error:
            if error.errno not in {errno.EACCES, errno.EAGAIN, errno.EDEADLK}:
                raise
        yield acquired
    finally:
        if acquired:
            os.lseek(descriptor, 0, os.SEEK_SET)
            if os.name == "nt":
                import msvcrt

                msvcrt.locking(descriptor, msvcrt.LK_UNLCK, 1)
            else:
                import fcntl

                fcntl.flock(descriptor, fcntl.LOCK_UN)
        os.close(descriptor)


@contextmanager
def _file_mutex(path: Path) -> Iterator[None]:
    """Wait for the short metadata critical section used during release."""

    while True:
        with _try_file_mutex(path) as locked:
            if locked:
                yield
                return
        time.sleep(0.01)


def _atomic_owner_write(path: Path, owner: dict[str, object]) -> None:
    """Publish complete lease metadata with one atomic filesystem replace."""

    temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
    try:
        with temporary.open("x", encoding="utf-8", newline="\n") as stream:
            json.dump(owner, stream, ensure_ascii=False, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def _path_age_seconds(path: Path) -> float | None:
    try:
        return max(0.0, time.time() - path.stat().st_mtime)
    except OSError:
        return None


def _read_owner(path: Path) -> tuple[dict[str, object] | None, str]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(value, dict):
            return None, "invalid"
        return value, "valid"
    except FileNotFoundError:
        return None, "missing"
    except PermissionError:
        return None, "initializing"
    except (OSError, json.JSONDecodeError):
        return None, "invalid"


class SlotLease:
    """Cross-process concurrency limit using per-slot OS file mutexes."""

    def __init__(
        self,
        repository: RunRepository,
        run_id: str,
        max_slots: int,
        *,
        poll_seconds: float = 0.25,
    ) -> None:
        self.repository = repository
        self.run_id = run_id
        self.max_slots = max_slots
        self.poll_seconds = poll_seconds
        self.path: Path | None = None
        self.token: str | None = None
        self.queue_token: str | None = None
        self._logged_states: dict[str, str] = {}

    @property
    def _slots_root(self) -> Path:
        return self.repository.root / ".slots"

    @property
    def _queue_root(self) -> Path:
        return self._slots_root / HEAVY_QUEUE_DIRECTORY

    @property
    def _queue_guard(self) -> Path:
        return self._slots_root / HEAVY_QUEUE_GUARD

    @property
    def _queue_path(self) -> Path:
        return self._queue_root / f"{self.run_id}.json"

    @property
    def _queue_sequence_path(self) -> Path:
        return self._queue_root / "_sequence.json"

    def _next_queue_sequence_locked(self) -> int:
        values, state = _read_owner(self._queue_sequence_path)
        try:
            current = int(values["value"]) if state == "valid" else 0
        except (KeyError, TypeError, ValueError):
            current = 0
        sequence = current + 1
        _atomic_owner_write(self._queue_sequence_path, {"value": sequence})
        return sequence

    def _ensure_queue_ticket_locked(self) -> dict[str, object]:
        """Create or adopt this run's durable FIFO position under the queue mutex."""

        self._queue_root.mkdir(parents=True, exist_ok=True)
        existing, state = _read_owner(self._queue_path)
        queued_at = None
        queue_id = None
        queue_sequence = None
        if state == "valid" and existing.get("run_id") == self.run_id:
            queued_at = existing.get("queued_at")
            queue_id = existing.get("queue_id")
            queue_sequence = existing.get("queue_sequence")
        if not queued_at:
            queued_at = datetime.now(timezone.utc).isoformat()
        if not queue_id:
            queue_id = uuid.uuid4().hex
        if queue_sequence is None:
            queue_sequence = self._next_queue_sequence_locked()
        if self.queue_token is None:
            self.queue_token = uuid.uuid4().hex
        ticket: dict[str, object] = {
            "run_id": self.run_id,
            "queued_at": str(queued_at),
            "queue_id": str(queue_id),
            "queue_sequence": int(queue_sequence),
            "waiter_pid": os.getpid(),
            "waiter_token": self.queue_token,
        }
        if state != "valid" or any(existing.get(key) != value for key, value in ticket.items()):
            _atomic_owner_write(self._queue_path, ticket)
            LOGGER.info(
                "Heavy queue entered run_id=%s queued_at=%s",
                self.run_id,
                ticket["queued_at"],
            )
        return ticket

    def _ticket_is_eligible_locked(
        self, path: Path, ticket: dict[str, object]
    ) -> bool:
        run_id = ticket.get("run_id")
        queued_at = ticket.get("queued_at")
        try:
            waiter_pid = int(ticket["waiter_pid"])
        except (KeyError, TypeError, ValueError):
            waiter_pid = 0
        if not isinstance(run_id, str) or not run_id or not queued_at:
            path.unlink(missing_ok=True)
            LOGGER.warning("Heavy queue invalid ticket removed path=%s", path.name)
            return False
        status_path = self.repository.run_directory(run_id) / "status.json"
        if status_path.is_file():
            try:
                status = self.repository.status(run_id)
            except (OSError, ValueError, json.JSONDecodeError):
                LOGGER.warning("Heavy queue unreadable job skipped run_id=%s", run_id)
                return False
            if (
                status.get("status") not in {
                    JobStatus.PENDING.value,
                    JobStatus.RUNNING.value,
                }
                or status.get("cancellation_requested")
                or self.repository.cancellation_requested(run_id)
            ):
                path.unlink(missing_ok=True)
                LOGGER.info("Heavy queue cancelled or terminal job removed run_id=%s", run_id)
                return False
        current_waiter = (
            run_id == self.run_id
            and ticket.get("waiter_token") == self.queue_token
            and waiter_pid == os.getpid()
        )
        if not current_waiter and not _process_alive(waiter_pid):
            LOGGER.info("Heavy queue inactive waiter skipped run_id=%s", run_id)
            return False
        return True

    def _ordered_waiters_locked(self) -> list[dict[str, object]]:
        waiters: list[dict[str, object]] = []
        for path in self._queue_root.glob("*.json"):
            if path == self._queue_sequence_path:
                continue
            ticket, state = _read_owner(path)
            if state != "valid" or not self._ticket_is_eligible_locked(path, ticket):
                if state in {"missing", "invalid"}:
                    path.unlink(missing_ok=True)
                continue
            waiters.append(ticket)
        return sorted(
            waiters,
            key=lambda item: (
                str(item["queued_at"]),
                int(item.get("queue_sequence") or 0),
                str(item.get("queue_id") or ""),
                str(item["run_id"]),
            ),
        )

    def _remove_queue_ticket_locked(self) -> None:
        ticket, state = _read_owner(self._queue_path)
        if (
            state == "valid"
            and ticket.get("run_id") == self.run_id
            and ticket.get("waiter_token") == self.queue_token
        ):
            self._queue_path.unlink(missing_ok=True)

    def _leave_queue(self) -> None:
        if self.queue_token is None:
            return
        with _file_mutex(self._queue_guard):
            self._remove_queue_ticket_locked()
        self.queue_token = None

    def _log_state(self, slot: Path, state: str, message: str) -> None:
        key = str(slot)
        if self._logged_states.get(key) == state:
            return
        self._logged_states[key] = state
        LOGGER.info("Heavy slot %s run_id=%s slot=%s", message, self.run_id, slot.name)

    def _slot_guard_busy(self, slot: Path) -> None:
        self._log_state(slot, "initializing", "in initialization or metadata transition")

    def _queue_guard_busy(self) -> None:
        self._log_state(
            self._slots_root,
            "queue-transition",
            "waiting for FIFO queue metadata transition",
        )

    def _after_slot_directory_created(self, slot: Path) -> None:
        """Test synchronization seam; production acquisition does not override it."""

    def _claim(self, slot: Path) -> bool:
        guard = slot.parent / f"{slot.name}.guard"
        with _try_file_mutex(guard) as locked:
            if not locked:
                self._slot_guard_busy(slot)
                return False
            owner_path = slot / "owner.json"
            if not slot.exists():
                slot.mkdir()
                self._log_state(slot, "initializing", "initialization started")
                self._after_slot_directory_created(slot)
            else:
                owner, state = _read_owner(owner_path)
                if state == "valid":
                    try:
                        alive = _process_alive(int(owner["pid"]))
                    except (TypeError, ValueError, KeyError):
                        alive = False
                    if alive:
                        self._log_state(slot, "occupied", "occupied")
                        return False
                    self._log_state(slot, "stale", "stale owner detected")
                elif state == "initializing":
                    self._log_state(slot, "initializing", "owner metadata initializing")
                    return False
                else:
                    age = _path_age_seconds(owner_path if owner_path.exists() else slot)
                    if age is None or age < SLOT_INITIALIZATION_GRACE_SECONDS:
                        self._log_state(slot, "initializing", "owner metadata initializing")
                        return False
                    self._log_state(slot, "stale", "abandoned initialization detected")
                self._log_state(slot, "recovered", "stale owner recovered")
            claim_token = uuid.uuid4().hex
            _atomic_owner_write(
                owner_path,
                {
                    "pid": os.getpid(),
                    "run_id": self.run_id,
                    "token": claim_token,
                    "acquired_at": datetime.now(timezone.utc).isoformat(),
                },
            )
            self.token = claim_token
            self.path = slot
            self._log_state(slot, "acquired", "acquired")
            return True

    def acquire(self) -> None:
        slots = self._slots_root
        slots.mkdir(parents=True, exist_ok=True)
        LOGGER.info(
            "Heavy slot acquisition attempt run_id=%s max_slots=%s",
            self.run_id,
            self.max_slots,
        )
        try:
            while self.path is None:
                check_cancellation(
                    lambda: self.repository.cancellation_requested(self.run_id)
                )
                with _try_file_mutex(self._queue_guard) as queue_locked:
                    if queue_locked:
                        self._ensure_queue_ticket_locked()
                        waiters = self._ordered_waiters_locked()
                        position = next(
                            (
                                index
                                for index, waiter in enumerate(waiters)
                                if waiter.get("run_id") == self.run_id
                                and waiter.get("waiter_token") == self.queue_token
                            ),
                            None,
                        )
                        if position == 0:
                            for number in range(self.max_slots):
                                slot = slots / f"slot-{number}"
                                if self._claim(slot):
                                    self._remove_queue_ticket_locked()
                                    self.queue_token = None
                                    return
                        elif position is not None:
                            self._log_state(
                                slots,
                                f"queued-{position}",
                                f"waiting in FIFO position {position + 1}",
                            )
                    else:
                        self._queue_guard_busy()
                time.sleep(self.poll_seconds)
        finally:
            if self.path is None:
                self._leave_queue()

    def release(self) -> None:
        if self.path is None:
            return
        slot = self.path
        guard = slot.parent / f"{slot.name}.guard"
        try:
            with _file_mutex(guard):
                owner_path = slot / "owner.json"
                owner, state = _read_owner(owner_path)
                if (
                    state != "valid"
                    or owner.get("run_id") != self.run_id
                    or owner.get("token") != self.token
                ):
                    LOGGER.warning(
                        "Heavy slot release skipped: ownership changed run_id=%s slot=%s",
                        self.run_id,
                        slot.name,
                    )
                    return
                owner_path.unlink()
                try:
                    slot.rmdir()
                except OSError:
                    pass
                LOGGER.info("Heavy slot released run_id=%s slot=%s", self.run_id, slot.name)
        finally:
            self.path = None
            self.token = None


class RunLease:
    """Exclusive per-run lease preventing concurrent resume workers."""

    def __init__(self, repository: RunRepository, run_id: str) -> None:
        self.repository = repository
        self.run_id = run_id
        self.path = repository.run_directory(run_id) / ".worker.lock"
        self.acquired = False
        self.token: str | None = None

    @property
    def _guard(self) -> Path:
        return self.path.with_name(f"{self.path.name}.guard")

    def acquire(self) -> None:
        for _ in range(2):
            with _try_file_mutex(self._guard) as locked:
                if not locked:
                    continue
                owner_path = self.path / "owner.json"
                if self.path.exists():
                    owner, state = _read_owner(owner_path)
                    if state == "valid":
                        try:
                            if _process_alive(int(owner["pid"])):
                                continue
                        except (TypeError, ValueError, KeyError):
                            pass
                    elif state == "initializing":
                        continue
                    else:
                        age = _path_age_seconds(owner_path if owner_path.exists() else self.path)
                        if age is None or age < SLOT_INITIALIZATION_GRACE_SECONDS:
                            continue
                else:
                    self.path.mkdir()
                claim_token = uuid.uuid4().hex
                _atomic_owner_write(owner_path, {
                    "pid": os.getpid(), "run_id": self.run_id,
                    "token": claim_token,
                    "acquired_at": datetime.now(timezone.utc).isoformat(),
                })
                self.token = claim_token
                self.acquired = True
                return
        raise RuntimeError("Une autre exécution détient déjà le verrou de ce run.")

    def release(self) -> None:
        if not self.acquired:
            return
        try:
            with _file_mutex(self._guard):
                owner_path = self.path / "owner.json"
                owner, state = _read_owner(owner_path)
                if (
                    state != "valid"
                    or owner.get("run_id") != self.run_id
                    or owner.get("token") != self.token
                ):
                    return
                owner_path.unlink()
                try:
                    self.path.rmdir()
                except OSError:
                    pass
        finally:
            self.acquired = False
            self.token = None

    def owned_by_current_process(self) -> bool:
        if not self.acquired:
            return False
        try:
            owner = json.loads((self.path / "owner.json").read_text(encoding="utf-8"))
            return (
                owner.get("run_id") == self.run_id
                and owner.get("token") == self.token
                and int(owner["pid"]) == os.getpid()
            )
        except (FileNotFoundError, OSError, ValueError, KeyError, json.JSONDecodeError):
            return False


def _publishing_completed(repository: RunRepository, run_id: str) -> bool:
    progress = repository.progress(run_id)
    return progress.get("stage") == "completed" and any(
        phase.get("name") == "publishing" and phase.get("status") == "completed"
        for phase in progress.get("phase_history", [])
        if isinstance(phase, dict)
    )


def _cancellation_requested(repository: RunRepository, run_id: str) -> bool:
    if repository.cancellation_requested(run_id):
        return True
    metadata = repository.run_metadata(run_id)
    return bool(
        metadata.parent_run_id
        and repository.cancellation_requested(metadata.parent_run_id)
    )


def _complete_owned_run(
    repository: RunRepository, run_id: str, run_lease: RunLease
) -> None:
    current = JobStatus(repository.status(run_id)["status"])
    if current is JobStatus.RUNNING:
        repository.transition(run_id, JobStatus.COMPLETED)
        return
    if (
        current is JobStatus.INTERRUPTED
        and run_lease.owned_by_current_process()
        and _publishing_completed(repository, run_id)
    ):
        repository.recover_interrupted_completion(run_id, worker_pid=os.getpid())
        repository.append_log(
            run_id,
            "Recovered inconsistent interrupted status after successful publication",
        )
        return
    repository.transition(run_id, JobStatus.COMPLETED)


def execute_run(
    repository: RunRepository,
    run_id: str,
    max_concurrent_jobs: int,
    *,
    registry: WorkflowRegistry | None = None,
) -> None:
    """Execute one run; injectable registry keeps tests small and ML-free."""

    status = repository.status(run_id)
    if JobStatus(status["status"]).terminal:
        return
    lease = SlotLease(repository, run_id, max_concurrent_jobs)
    run_lease = RunLease(repository, run_id)
    checkpoint: CheckpointManager | None = None
    log_handler = logging.FileHandler(
        repository.run_directory(run_id) / "run.log", encoding="utf-8"
    )
    log_handler.setFormatter(logging.Formatter("%(asctime)s %(levelname)s %(message)s"))
    root_logger = logging.getLogger()
    root_logger.addHandler(log_handler)
    root_logger.setLevel(logging.INFO)
    try:
        run_lease.acquire()
        lease.acquire()
        check_cancellation(lambda: _cancellation_requested(repository, run_id))
        repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
        repository.append_log(run_id, "Worker started")
        reporter = ProgressReporter(repository, run_id)
        spec = replace(repository.load_spec(run_id), execution_run_id=run_id)
        if spec.job_type in {
            JobType.WALK_FORWARD,
            JobType.WALK_FORWARD_BATCH,
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            JobType.XGBOOST_CALIBRATION,
        }:
            checkpoint = CheckpointManager(
                repository.run_directory(run_id),
                run_id=run_id,
                job_type=spec.job_type.value,
                configuration_fingerprint=repository.configuration_fingerprint(
                    run_id
                ),
                batch_sizes=(
                    {
                        "predictor_prefilter_walk_forward": spec.config.predictor_prefilter_batch_size,
                        "walk_forward": spec.config.walk_forward_batch_size,
                        "final_holdout": spec.config.final_holdout_batch_size,
                    }
                    if spec.job_type in {JobType.WALK_FORWARD, JobType.WALK_FORWARD_BATCH}
                    else (
                        {"xgboost_calibration": spec.config.walk_forward_batch_size}
                        if spec.job_type is JobType.XGBOOST_CALIBRATION
                        else {}
                    )
                ),
            )
            checkpoint.start_attempt(
                resumed=bool(status.get("resume_requested"))
                or int(checkpoint.manifest.get("attempt_count", 0)) > 0
            )
        phases = WORKFLOW_PHASES[spec.job_type]
        if spec.job_type is JobType.WALK_FORWARD and spec.forced_symbol_sets is not None:
            phases = [
                ("data_preparation", 8),
                ("forced_candidate_loading", 2),
                ("walk_forward", 62),
                ("aggregation", 9),
                ("qualification", 7),
                ("final_holdout", 5),
                ("metrics", 3),
                ("result_writing", 2),
                ("publishing", 2),
            ]
        if spec.job_type is JobType.END_TO_END and spec.forced_symbol_sets is not None:
            phases = [
                ("walk_forward", 55),
                ("fixed_candidate_evaluation", 98),
                ("publishing", 2),
            ]
        if spec.job_type is JobType.END_TO_END and not spec.auto_promote_candidates:
            phases = [item for item in phases if item[0] != "promotion"]
        if spec.job_type is JobType.END_TO_END and spec.temporal_validation_enabled:
            phases = [
                *[item for item in phases if item[0] != "publishing"],
                ("temporal_validation_end_to_end", 90),
                ("temporal_validation_comparison", 95),
                *(
                    [("forced_candidate_validation_end_to_end", 100)]
                    if spec.pipeline_version >= 2
                    else []
                ),
                ("publishing", 1),
            ]
        reporter.configure_phases(phases)
        working = repository.run_directory(run_id) / "_working"
        results = repository.run_directory(run_id) / "results"
        if results.exists() and repository.summary(run_id):
            reporter.phase_started("publishing")
            reporter.phase_completed("publishing")
            reporter.complete_workflow()
            _complete_owned_run(repository, run_id, run_lease)
            if checkpoint is not None:
                checkpoint.finish_attempt("completed")
            repository.append_log(run_id, "Worker completed after publication recovery")
            return
        working.mkdir(exist_ok=bool(status.get("resume_requested")))
        active_registry = registry or WorkflowRegistry.production()

        def execute_child(child_run_id: str) -> None:
            # Keep the parent's exclusive RunLease, but never monopolize the
            # heavy slot while a technical child needs it (including max=1).
            lease.release()
            try:
                execute_run(
                    repository,
                    child_run_id,
                    max_concurrent_jobs,
                    registry=active_registry,
                )
            finally:
                lease.acquire()

        with child_executor_context(execute_child):
            summary = active_registry.execute(
                spec,
                working,
                progress_callback=reporter,
                cancellation_check=lambda: _cancellation_requested(
                    repository, run_id
                ),
            )
        if spec.run_description:
            summary = {**summary, "run_description": spec.run_description}
        check_cancellation(lambda: _cancellation_requested(repository, run_id))
        reporter.phase_started("publishing")
        repository.write_json(run_id, "summary.json", summary)
        working.rename(results)
        reporter.phase_completed("publishing")
        reporter.complete_workflow()
        _complete_owned_run(repository, run_id, run_lease)
        # Forward is intentionally dispatched only after the End-to-end parent
        # is terminal: its own failure/cancellation cannot alter parent status.
        forward = (
            summary.get("forward_simulation")
            if spec.job_type is JobType.END_TO_END and isinstance(summary, dict)
            else None
        )
        if isinstance(forward, dict) and forward.get("status") == "pending":
            child_run_id = str(forward["child_run_id"])
            try:
                dispatch = RunService(
                    repository,
                    backend=LocalProcessBackend(),
                    max_concurrent_heavy_jobs=max_concurrent_jobs,
                ).dispatch_pending_forward(child_run_id)
                child_status = repository.status(child_run_id)
                if dispatch.created or child_status.get("dispatch_state") == "launched":
                    forward["status"] = "launched"
                else:
                    forward["status"] = "dispatch_active"
            except Exception as error:
                repository.append_log(
                    child_run_id,
                    f"Initial Forward dispatch deferred for automatic recovery: {error}",
                )
                forward.update(status="pending", dispatch_error=str(error))
            repository.write_json(run_id, "summary.json", summary)
            pipeline_summary_path = results / "pipeline_summary.json"
            if pipeline_summary_path.is_file():
                pipeline_summary = repository.read_json(
                    run_id, "results/pipeline_summary.json"
                )
                persisted_forward = pipeline_summary.get("forward_simulation")
                if (
                    isinstance(persisted_forward, dict)
                    and persisted_forward.get("child_run_id") == child_run_id
                ):
                    persisted_forward.update(forward)
                    repository.write_json(
                        run_id, "results/pipeline_summary.json", pipeline_summary
                    )
        if checkpoint is not None:
            checkpoint.finish_attempt("completed")
        repository.append_log(run_id, "Worker completed")
    except CancellationRequested:
        current = JobStatus(repository.status(run_id)["status"])
        if not current.terminal:
            repository.transition(run_id, JobStatus.CANCELLED)
        if checkpoint is not None:
            checkpoint.finish_attempt("cancelled")
        repository.append_log(run_id, "Worker cancelled at a safe boundary")
    except Exception as error:
        repository.append_log(run_id, traceback.format_exc())
        if not run_lease.acquired:
            return
        current = JobStatus(repository.status(run_id)["status"])
        if not current.terminal:
            repository.transition(run_id, JobStatus.FAILED, error=str(error))
        if checkpoint is not None:
            checkpoint.finish_attempt("failed", str(error))
    finally:
        lease.release()
        run_lease.release()
        root_logger.removeHandler(log_handler)
        log_handler.close()


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--runs-root", type=Path, required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--max-concurrent-heavy-jobs", type=int, default=1)
    return parser


def main() -> None:
    args = _parser().parse_args()
    execute_run(
        RunRepository(args.runs_root),
        args.run_id,
        args.max_concurrent_heavy_jobs,
    )


if __name__ == "__main__":
    main()
