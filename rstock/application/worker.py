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
from rstock.checkpoints import CheckpointManager

from .domain import JobStatus, JobType
from .orchestration_runtime import child_executor_context
from .processes import process_alive
from .repository import RunRepository
from .runner import ProgressReporter
from .workflows import WorkflowRegistry


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
    JobType.END_TO_END: [
        ("walk_forward", 22.5),
        ("xgboost_calibration", 22.5),
        ("threshold_parameter_calibration", 22.5),
        ("threshold_calibration", 22.5),
        ("promotion", 9),
        ("publishing", 1),
    ],
}


def _process_alive(pid: int) -> bool:
    return process_alive(pid)


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
        except PermissionError:
            # A concurrent releaser or antivirus can briefly lock owner.json
            # on Windows. Treat the slot as active and retry on the next poll.
            return
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


class RunLease:
    """Exclusive per-run lease preventing concurrent resume workers."""

    def __init__(self, repository: RunRepository, run_id: str) -> None:
        self.repository = repository
        self.run_id = run_id
        self.path = repository.run_directory(run_id) / ".worker.lock"
        self.acquired = False

    def _clear_stale(self) -> None:
        owner_path = self.path / "owner.json"
        try:
            owner = json.loads(owner_path.read_text(encoding="utf-8"))
            alive = _process_alive(int(owner["pid"]))
        except (FileNotFoundError, ValueError, KeyError, json.JSONDecodeError):
            alive = False
        if not alive:
            owner_path.unlink(missing_ok=True)
            try:
                self.path.rmdir()
            except OSError:
                pass

    def acquire(self) -> None:
        for _ in range(2):
            try:
                self.path.mkdir()
            except FileExistsError:
                self._clear_stale()
                continue
            (self.path / "owner.json").write_text(
                json.dumps({"pid": os.getpid(), "run_id": self.run_id}),
                encoding="utf-8",
            )
            self.acquired = True
            return
        raise RuntimeError("Une autre exécution détient déjà le verrou de ce run.")

    def release(self) -> None:
        if not self.acquired:
            return
        (self.path / "owner.json").unlink(missing_ok=True)
        try:
            self.path.rmdir()
        finally:
            self.acquired = False

    def owned_by_current_process(self) -> bool:
        if not self.acquired:
            return False
        try:
            owner = json.loads((self.path / "owner.json").read_text(encoding="utf-8"))
            return owner.get("run_id") == self.run_id and int(owner["pid"]) == os.getpid()
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
        spec = repository.load_spec(run_id)
        if spec.job_type in {JobType.WALK_FORWARD, JobType.WALK_FORWARD_BATCH}:
            checkpoint = CheckpointManager(
                repository.run_directory(run_id),
                run_id=run_id,
                job_type=spec.job_type.value,
                configuration_fingerprint=repository.configuration_fingerprint(
                    run_id
                ),
                batch_sizes={
                    "predictor_prefilter_walk_forward": spec.config.predictor_prefilter_batch_size,
                    "walk_forward": spec.config.walk_forward_batch_size,
                    "final_holdout": spec.config.final_holdout_batch_size,
                },
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
