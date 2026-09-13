"""Independent local worker process for a persisted RStock run."""

from __future__ import annotations

import argparse
import json
import logging
import os
import time
import traceback
from pathlib import Path

from rstock.progress import CancellationRequested, check_cancellation

from .domain import JobStatus, JobType
from .repository import RunRepository
from .runner import ProgressReporter
from .workflows import WorkflowRegistry


WORKFLOW_PHASES: dict[JobType, list[tuple[str, float]]] = {
    JobType.WALK_FORWARD: [
        ("data_preparation", 10), ("combination_generation", 5),
        ("walk_forward", 65), ("aggregation", 7), ("qualification", 5),
        ("final_holdout", 4), ("metrics", 2), ("result_writing", 1), ("publishing", 1),
    ],
    JobType.XGBOOST_CALIBRATION: [
        ("data_preparation", 12), ("combination_generation", 6),
        ("walk_forward", 65), ("final_holdout", 8), ("metrics", 4),
        ("result_writing", 3), ("publishing", 2),
    ],
    JobType.THRESHOLD_CALIBRATION: [
        ("data_preparation", 12), ("combination_generation", 6),
        ("walk_forward", 65), ("final_holdout", 8), ("metrics", 4),
        ("result_writing", 3), ("publishing", 2),
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
        ("market_update", 45), ("daily_prediction", 30), ("screening", 10),
        ("realized_validation", 10), ("publishing", 5),
    ],
}


def _process_alive(pid: int) -> bool:
    if pid <= 0:
        return False
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True


class SlotLease:
    """Cross-process local concurrency limit using atomic slot directories."""

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

    def _clear_stale(self, slot: Path) -> None:
        owner_path = slot / "owner.json"
        try:
            owner = json.loads(owner_path.read_text(encoding="utf-8"))
            alive = _process_alive(int(owner["pid"]))
        except (FileNotFoundError, ValueError, KeyError, json.JSONDecodeError):
            alive = False
        if not alive:
            if owner_path.exists():
                owner_path.unlink()
            try:
                slot.rmdir()
            except OSError:
                pass

    def acquire(self) -> None:
        slots = self.repository.root / ".slots"
        slots.mkdir(parents=True, exist_ok=True)
        while self.path is None:
            check_cancellation(lambda: self.repository.cancellation_requested(self.run_id))
            for number in range(self.max_slots):
                slot = slots / f"slot-{number}"
                try:
                    slot.mkdir()
                except FileExistsError:
                    self._clear_stale(slot)
                    continue
                (slot / "owner.json").write_text(
                    json.dumps({"pid": os.getpid(), "run_id": self.run_id}),
                    encoding="utf-8",
                )
                self.path = slot
                return
            time.sleep(self.poll_seconds)

    def release(self) -> None:
        if self.path is None:
            return
        owner = self.path / "owner.json"
        if owner.exists():
            owner.unlink()
        try:
            self.path.rmdir()
        finally:
            self.path = None


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
    log_handler = logging.FileHandler(
        repository.run_directory(run_id) / "run.log", encoding="utf-8"
    )
    log_handler.setFormatter(logging.Formatter("%(asctime)s %(levelname)s %(message)s"))
    root_logger = logging.getLogger()
    root_logger.addHandler(log_handler)
    root_logger.setLevel(logging.INFO)
    try:
        lease.acquire()
        check_cancellation(lambda: repository.cancellation_requested(run_id))
        repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
        repository.append_log(run_id, "Worker started")
        reporter = ProgressReporter(repository, run_id)
        spec = repository.load_spec(run_id)
        reporter.configure_phases(WORKFLOW_PHASES[spec.job_type])
        working = repository.run_directory(run_id) / "_working"
        working.mkdir(exist_ok=False)
        active_registry = registry or WorkflowRegistry.production()
        summary = active_registry.execute(
            spec,
            working,
            progress_callback=reporter,
            cancellation_check=lambda: repository.cancellation_requested(run_id),
        )
        check_cancellation(lambda: repository.cancellation_requested(run_id))
        results = repository.run_directory(run_id) / "results"
        reporter.phase_started("publishing")
        working.rename(results)
        repository.write_json(run_id, "summary.json", summary)
        reporter.phase_completed("publishing")
        reporter.complete_workflow()
        repository.transition(run_id, JobStatus.COMPLETED)
        repository.append_log(run_id, "Worker completed")
    except CancellationRequested:
        current = JobStatus(repository.status(run_id)["status"])
        if not current.terminal:
            repository.transition(run_id, JobStatus.CANCELLED)
        repository.append_log(run_id, "Worker cancelled at a safe boundary")
    except Exception as error:
        repository.append_log(run_id, traceback.format_exc())
        current = JobStatus(repository.status(run_id)["status"])
        if not current.terminal:
            repository.transition(run_id, JobStatus.FAILED, error=str(error))
    finally:
        lease.release()
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
