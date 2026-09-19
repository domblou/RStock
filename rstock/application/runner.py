"""Run submission, local process backend and durable progress reporting."""

from __future__ import annotations

import subprocess
import sys
import threading
import time
from contextlib import contextmanager
from dataclasses import dataclass, replace
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Protocol

from rstock.progress import ProgressEvent

from .domain import (
    ExperimentSpec,
    JobStatus,
    JobType,
    RunMetadata,
    RunPurpose,
    RunRole,
)
from .processes import process_alive
from .repository import RunRepository, utc_now
from rstock.checkpoints import (
    CHECKPOINT_IMPLEMENTATION_VERSION,
    CHECKPOINT_SCHEMA_VERSION,
)


ACTIVE_STATUSES = {JobStatus.PENDING.value, JobStatus.RUNNING.value}
INTERRUPTION_GRACE_SECONDS = 15.0
INTERRUPTION_HEARTBEAT_STALE_SECONDS = 30.0
INTERRUPTION_CONFIRMATION_SECONDS = 2.0


@dataclass(frozen=True, slots=True)
class SubmissionResult:
    run_id: str
    created: bool


class JobBackend(Protocol):
    def launch(self, runs_root: Path, run_id: str, max_concurrent_jobs: int) -> int | None: ...


class LocalProcessBackend:
    """Launch one detached Python worker per submission."""

    def launch(self, runs_root: Path, run_id: str, max_concurrent_jobs: int) -> int:
        command = [
            sys.executable,
            "-m",
            "rstock.application.worker",
            "--runs-root",
            str(runs_root),
            "--run-id",
            run_id,
            "--max-concurrent-heavy-jobs",
            str(max_concurrent_jobs),
        ]
        creationflags = getattr(subprocess, "CREATE_NO_WINDOW", 0)
        process = subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            creationflags=creationflags,
            close_fds=True,
        )
        return process.pid


def _pid_alive(pid: object) -> bool:
    return process_alive(pid)


class RunService:
    """Persistent job API used equally by Streamlit and command-line clients."""

    def __init__(
        self,
        repository: RunRepository,
        *,
        backend: JobBackend | None = None,
        max_concurrent_heavy_jobs: int = 1,
    ) -> None:
        if max_concurrent_heavy_jobs < 1:
            raise ValueError("max_concurrent_heavy_jobs must be positive")
        self.repository = repository
        self.backend = backend or LocalProcessBackend()
        self.max_concurrent_heavy_jobs = max_concurrent_heavy_jobs
        self._interruption_observations: dict[str, tuple[object, float]] = {}
        self._interruption_lock = threading.Lock()

    @staticmethod
    def _timestamp_age_seconds(value: object) -> float | None:
        if value is None:
            return None
        try:
            timestamp = datetime.fromisoformat(str(value))
        except ValueError:
            return None
        if timestamp.tzinfo is None:
            timestamp = timestamp.replace(tzinfo=timezone.utc)
        return max(0.0, (datetime.now(timezone.utc) - timestamp).total_seconds())

    def _worker_lease_active(self, run_id: str, expected_pid: object) -> bool:
        owner_path = self.repository.run_directory(run_id) / ".worker.lock" / "owner.json"
        try:
            import json

            owner = json.loads(owner_path.read_text(encoding="utf-8"))
            owner_pid = int(owner["pid"])
        except (FileNotFoundError, OSError, ValueError, KeyError, json.JSONDecodeError):
            return False
        if owner.get("run_id") != run_id or str(owner_pid) != str(expected_pid):
            return False
        return _pid_alive(owner_pid)

    def _forget_interruption_observation(self, run_id: str) -> None:
        with self._interruption_lock:
            self._interruption_observations.pop(run_id, None)

    @contextmanager
    def _submission_lock(self):
        self.repository.root.mkdir(parents=True, exist_ok=True)
        lock = self.repository.root / ".submission.lock"
        deadline = time.monotonic() + 5.0
        while True:
            try:
                lock.mkdir()
                break
            except FileExistsError:
                if time.monotonic() >= deadline:
                    raise TimeoutError("Could not acquire run submission lock")
                time.sleep(0.05)
        try:
            yield
        finally:
            lock.rmdir()

    def submit(self, spec: ExperimentSpec) -> SubmissionResult:
        with self._submission_lock():
            for status in self.repository.list_runs():
                if (
                    status["status"] in ACTIVE_STATUSES
                    and status.get("configuration_fingerprint") == spec.fingerprint
                ):
                    return SubmissionResult(str(status["run_id"]), False)
            metadata = (
                RunMetadata(
                    run_role=RunRole.PIPELINE_PARENT,
                    run_purpose=(
                        RunPurpose.REFERENCE
                        if spec.temporal_validation_enabled
                        else RunPurpose.STANDARD
                    ),
                )
                if spec.job_type is JobType.END_TO_END
                else None
            )
            run_id = self.repository.create(spec, metadata=metadata)
            try:
                pid = self.backend.launch(
                    self.repository.root,
                    run_id,
                    self.max_concurrent_heavy_jobs,
                )
                status = self.repository.status(run_id)
                status["launcher_pid"] = pid
                self.repository.write_json(run_id, "status.json", status)
            except Exception as error:
                self.repository.append_log(run_id, f"Worker launch failed: {error}")
                self.repository.transition(run_id, JobStatus.FAILED, error=str(error))
                raise
            return SubmissionResult(run_id, True)

    def cancel(self, run_id: str) -> dict[str, object]:
        status = self.repository.request_cancellation(run_id)
        for child_run_id in self.repository.list_children(run_id):
            child_status = self.repository.status(child_run_id)
            if child_status["status"] in ACTIVE_STATUSES:
                self.repository.request_cancellation(child_run_id)
        if status["status"] == JobStatus.PENDING.value:
            return self.repository.transition(run_id, JobStatus.CANCELLED)
        return self.repository.status(run_id)

    def resume(self, run_id: str) -> SubmissionResult:
        """Resume the same run id through the normal detached worker backend."""

        from rstock.checkpoints import CheckpointManager

        with self._submission_lock():
            if self.repository.storage(run_id)["state"] != "full":
                raise ValueError("Un run purgé ne peut pas être repris.")
            self._refresh_interrupted(run_id)
            status = self.repository.status(run_id)
            if status["status"] in ACTIVE_STATUSES:
                raise ValueError("Ce run est déjà en cours ou en attente.")
            spec = self.repository.load_spec(run_id)
            resumable_types = {
                JobType.WALK_FORWARD,
                JobType.THRESHOLD_PARAMETER_CALIBRATION,
                JobType.END_TO_END,
            }
            if spec.job_type not in resumable_types:
                raise ValueError(
                    "Seuls les walk-forward avec checkpoint et les calibrations "
                    "de paramètres de seuils sont reprenables."
                )
            if spec.job_type is JobType.WALK_FORWARD and not (
                self.repository.run_directory(run_id) / "checkpoints" / "manifest.json"
            ).exists():
                raise ValueError(
                    "Aucun checkpoint de reprise n’est disponible pour ce run. "
                    "Relancez depuis le début."
                )
            if spec.job_type is JobType.WALK_FORWARD:
                CheckpointManager(
                    self.repository.run_directory(run_id),
                    run_id=run_id,
                    job_type=spec.job_type.value,
                    configuration_fingerprint=(
                        self.repository.configuration_fingerprint(run_id)
                    ),
                    batch_sizes={
                        "predictor_prefilter_walk_forward": spec.config.predictor_prefilter_batch_size,
                        "walk_forward": spec.config.walk_forward_batch_size,
                        "final_holdout": spec.config.final_holdout_batch_size,
                    },
                )
            self.repository.prepare_resume(run_id)
            pid = self.backend.launch(
                self.repository.root, run_id, self.max_concurrent_heavy_jobs
            )
            resumed = self.repository.status(run_id)
            resumed["launcher_pid"] = pid
            resumed["resume_requested"] = True
            self.repository.write_json(run_id, "status.json", resumed)
            return SubmissionResult(run_id, False)

    def restart(self, run_id: str) -> SubmissionResult:
        """Create a fresh run from a historical spec without touching the source."""

        if self.repository.storage(run_id)["state"] != "full":
            raise ValueError("Un run purgé ne peut pas être relancé.")
        spec = self.repository.load_spec(run_id)
        traceability = self.repository.summary(run_id).get("traceability", {})
        replay_values: dict[str, object] = {}
        if spec.job_type is JobType.WALK_FORWARD:
            replay_values["source_walk_forward_run"] = run_id
        if (
            spec.historical_data_cutoff is None
            and isinstance(traceability, dict)
            and traceability.get("prepared_market_last_date") is not None
        ):
            replay_values.update(
                historical_data_cutoff=str(
                    traceability["prepared_market_last_date"]
                ),
                source_prepared_dataset_sha256=(
                    None
                    if traceability.get("prepared_dataset_sha256") is None
                    else str(traceability["prepared_dataset_sha256"])
                ),
            )
        if replay_values:
            spec = replace(spec, **replay_values)
        with self._submission_lock():
            metadata = (
                RunMetadata(
                    run_role=RunRole.PIPELINE_PARENT,
                    run_purpose=(
                        RunPurpose.REFERENCE
                        if spec.temporal_validation_enabled
                        else RunPurpose.STANDARD
                    ),
                )
                if spec.job_type is JobType.END_TO_END
                else None
            )
            new_run_id = self.repository.create(spec, metadata=metadata)
            pid = self.backend.launch(
                self.repository.root, new_run_id, self.max_concurrent_heavy_jobs
            )
            status = self.repository.status(new_run_id)
            status["launcher_pid"] = pid
            status["restarted_from_run"] = run_id
            self.repository.write_json(new_run_id, "status.json", status)
            return SubmissionResult(new_run_id, True)

    def _refresh_interrupted(self, run_id: str) -> dict[str, object]:
        status = self.repository.status(run_id)
        if status.get("status") != JobStatus.RUNNING.value:
            self._forget_interruption_observation(run_id)
            return status

        pid = status.get("pid")
        if _pid_alive(pid):
            self._forget_interruption_observation(run_id)
            return status

        started_age = self._timestamp_age_seconds(
            status.get("started_at") or status.get("created_at")
        )
        if started_age is None or started_age < INTERRUPTION_GRACE_SECONDS:
            self._forget_interruption_observation(run_id)
            return status

        progress = self.repository.progress(run_id)
        heartbeat_age = self._timestamp_age_seconds(progress.get("updated_at"))
        if (
            heartbeat_age is not None
            and heartbeat_age < INTERRUPTION_HEARTBEAT_STALE_SECONDS
        ):
            self._forget_interruption_observation(run_id)
            return status

        if self._worker_lease_active(run_id, pid):
            self._forget_interruption_observation(run_id)
            return status

        now = time.monotonic()
        with self._interruption_lock:
            previous = self._interruption_observations.get(run_id)
            if previous is None or previous[0] != pid:
                self._interruption_observations[run_id] = (pid, now)
                return status
            if now - previous[1] < INTERRUPTION_CONFIRMATION_SECONDS:
                return status
            self._interruption_observations.pop(run_id, None)

        # Re-read after the confirmation window so an UI reader never applies a
        # decision made from a stale RUNNING snapshot.
        current = self.repository.status(run_id)
        if current.get("status") != JobStatus.RUNNING.value or current.get("pid") != pid:
            return current
        try:
            status = self.repository.transition(
                run_id,
                JobStatus.INTERRUPTED,
                error="Le processus worker n’est plus actif.",
            )
        except ValueError:
            # The worker may have completed or cancellation may have won after
            # the final read. A polling read must not surface that benign race.
            status = self.repository.status(run_id)
        return status

    def get(self, run_id: str) -> dict[str, object]:
        self._refresh_interrupted(run_id)
        checkpoint_path = (
            self.repository.run_directory(run_id) / "checkpoints" / "manifest.json"
        )
        checkpoint = None
        checkpoint_error = None
        if checkpoint_path.exists():
            try:
                import json

                checkpoint = json.loads(checkpoint_path.read_text(encoding="utf-8"))
                status = self.repository.status(run_id)
                if (
                    checkpoint.get("checkpoint_schema_version")
                    != CHECKPOINT_SCHEMA_VERSION
                    or checkpoint.get("implementation_version")
                    != CHECKPOINT_IMPLEMENTATION_VERSION
                ):
                    checkpoint_error = "Version de checkpoint incompatible."
                elif checkpoint.get("configuration_fingerprint") != status.get(
                    "configuration_fingerprint"
                ):
                    checkpoint_error = "Configuration différente du checkpoint."
            except (OSError, ValueError):
                checkpoint_error = "Manifest de checkpoint illisible ou corrompu."
        else:
            checkpoint_error = "Aucun checkpoint de reprise disponible pour cet ancien run."
        batches = None
        walk_forward_batch_manifest = None
        try:
            from .walk_forward_batches import load_manifest

            manifest = load_manifest(self.repository, run_id)
            if manifest is not None:
                walk_forward_batch_manifest = manifest
                batches = []
                for item in manifest["batches"]:
                    child_run_id = str(item["child_run_id"])
                    child_directory = self.repository.run_directory(child_run_id)
                    if not child_directory.exists():
                        batches.append(
                            {
                                **item,
                                "status": "reserved",
                                "progress": None,
                                "duration_seconds": None,
                                "created_at": None,
                                "started_at": None,
                                "completed_at": None,
                                "error": None,
                            }
                        )
                        continue
                    child_status = self.repository.status(child_run_id)
                    child_progress = self.repository.progress(child_run_id)
                    batches.append(
                        {
                            **item,
                            "status": child_status["status"],
                            "progress": child_progress.get("workflow_percent"),
                            "duration_seconds": child_status.get("duration_seconds"),
                            "created_at": child_status.get("created_at"),
                            "started_at": child_status.get("started_at"),
                            "completed_at": child_status.get("completed_at"),
                            "error": child_status.get("error"),
                        }
                    )
        except (FileNotFoundError, ValueError, OSError):
            batches = None
            walk_forward_batch_manifest = None
        pipeline_stages = None
        try:
            from .end_to_end import load_pipeline_manifest

            pipeline = load_pipeline_manifest(self.repository, run_id)
            if pipeline is not None:
                pipeline_stages = []
                for item in pipeline["stages"]:
                    child_run_id = item.get("child_run_id")
                    if child_run_id is None:
                        promotion = None
                        promotion_path = (
                            self.repository.run_directory(run_id)
                            / "orchestration"
                            / "promotion.json"
                        )
                        if promotion_path.exists():
                            promotion = self.repository.read_json(
                                run_id, "orchestration/promotion.json"
                            )
                        completed = (
                            None
                            if promotion is None
                            else int(promotion.get("completed_count", 0))
                        )
                        total = (
                            None
                            if promotion is None
                            else int(promotion.get("candidate_count", 0))
                        )
                        progress = (
                            None
                            if completed is None or total is None
                            else 100.0 if total == 0 and promotion.get("status") == "completed"
                            else 0.0 if total == 0
                            else 100.0 * completed / total
                        )
                        pipeline_stages.append(
                            {
                                **item,
                                "status": (
                                    str(promotion.get("status"))
                                    if promotion is not None
                                    else "pending"
                                    if pipeline["auto_promote_candidates"]
                                    else "not_requested"
                                ),
                                "progress": progress,
                                "duration_seconds": None,
                                "error": (
                                    None if promotion is None
                                    else promotion.get("error")
                                ),
                                "promotion": promotion,
                            }
                        )
                        continue
                    child_directory = self.repository.run_directory(str(child_run_id))
                    if not child_directory.exists():
                        pipeline_stages.append(
                            {
                                **item,
                                "status": "reserved",
                                "progress": None,
                                "duration_seconds": None,
                                "error": None,
                            }
                        )
                        continue
                    child_status = self.repository.status(str(child_run_id))
                    child_progress = self.repository.progress(str(child_run_id))
                    pipeline_stages.append(
                        {
                            **item,
                            "status": child_status["status"],
                            "progress": child_progress.get("workflow_percent"),
                            "duration_seconds": child_status.get("duration_seconds"),
                            "error": child_status.get("error"),
                        }
                    )
        except (FileNotFoundError, ValueError, OSError):
            pipeline_stages = None
        return {
            "configuration": self.repository.load_spec(run_id).to_dict(),
            "metadata": self.repository.run_metadata(run_id).to_dict(),
            "storage": self.repository.storage(run_id),
            "status": self.repository.status(run_id),
            "progress": self.repository.progress(run_id),
            "summary": self.repository.summary(run_id),
            "files": self.repository.result_files(run_id),
            "log_tail": self.repository.log_tail(run_id),
            "checkpoint": checkpoint,
            "checkpoint_error": checkpoint_error,
            "walk_forward_batches": batches,
            "walk_forward_batch_manifest": walk_forward_batch_manifest,
            "pipeline_stages": pipeline_stages,
        }

    def list(self) -> list[dict[str, object]]:
        return [
            self._refresh_interrupted(run_id)
            for run_id in self.repository.list_run_ids()
            if self.repository.run_metadata(run_id).visible_in_history
        ]


class ProgressReporter:
    """Persist stage and workflow progress without falsely completing a job."""

    def __init__(self, repository: RunRepository, run_id: str) -> None:
        self.repository = repository
        self.run_id = run_id
        self.started = time.monotonic()
        self._lock = threading.RLock()
        self._phase_plan: list[str] = []
        self._phase_weights: dict[str, float] = {}
        self._phase_history: list[dict[str, Any]] = []
        self._current_phase: str | None = None
        self._phase_started = self.started

    def configure_phases(self, phases: list[tuple[str, float]]) -> None:
        """Configure an ordered, weighted plan used only for global progress."""

        if not phases or any(weight <= 0 for _, weight in phases):
            raise ValueError("Workflow phases must have positive weights")
        with self._lock:
            self._phase_plan = [name for name, _ in phases]
            self._phase_weights = dict(phases)

    def _workflow_percent(self, stage_percent: float | None = None) -> float | None:
        if not self._phase_plan:
            return None
        completed = sum(
            self._phase_weights[item["name"]]
            for item in self._phase_history
            if item["status"] == "completed"
        )
        if (
            self._current_phase is not None
            and stage_percent is not None
            and not any(
                item["name"] == self._current_phase and item["status"] == "completed"
                for item in self._phase_history
            )
        ):
            completed += self._phase_weights.get(self._current_phase, 0.0) * stage_percent / 100.0
        total = sum(self._phase_weights.values())
        percent = 100.0 * completed / total
        # Only a successful publishing completion may expose global 100%.
        return min(99.9, percent) if self._current_phase != "completed" else 100.0

    def _persist(
        self,
        *,
        stage: str,
        substage: str | None,
        completed: int | None,
        total: int | None,
        stage_percent: float | None,
        eta: float | None,
        details: dict[str, object],
    ) -> None:
        elapsed = max(0.0, time.monotonic() - self.started)
        self.repository.write_json(
            self.run_id,
            "progress.json",
            {
                "stage": stage,
                "substage": substage,
                "completed_units": completed,
                "total_units": total,
                "percent": stage_percent,
                "stage_percent": stage_percent,
                "workflow_percent": self._workflow_percent(stage_percent),
                "elapsed_seconds": elapsed,
                "eta_seconds": eta,
                "details": details,
                "phase_history": self._phase_history,
                "updated_at": utc_now(),
            },
        )

    def phase_started(self, name: str, *, details: dict[str, object] | None = None) -> None:
        with self._lock:
            if self._current_phase == name:
                return
            now = utc_now()
            self._current_phase = name
            self._phase_started = time.monotonic()
            self._phase_history.append({"name": name, "status": "started", "started_at": now, "finished_at": None, "duration_seconds": None})
            phase_details = {key: value for key, value in (details or {}).items() if key != "phase_event"}
            suffix = f" details={phase_details}" if phase_details else ""
            self.repository.append_log(self.run_id, f"Phase started: {name}{suffix}")
            self._persist(stage=name, substage="started", completed=None, total=None, stage_percent=None, eta=None, details=phase_details)

    def phase_completed(self, name: str, *, details: dict[str, object] | None = None) -> None:
        with self._lock:
            current = next(
                (item for item in reversed(self._phase_history) if item["name"] == name and item["status"] == "started"),
                None,
            )
            if current is None:
                self.phase_started(name)
                current = self._phase_history[-1]
            current["status"] = "completed"
            current["finished_at"] = utc_now()
            current["duration_seconds"] = max(0.0, time.monotonic() - self._phase_started)
            phase_details = {key: value for key, value in (details or {}).items() if key != "phase_event"}
            suffix = f" details={phase_details}" if phase_details else ""
            self.repository.append_log(self.run_id, f"Phase completed: {name} ({current['duration_seconds']:.2f}s){suffix}")
            self._persist(stage=name, substage="completed", completed=None, total=None, stage_percent=100.0, eta=None, details=phase_details)

    def complete_workflow(self) -> None:
        with self._lock:
            if "publishing" in self._phase_weights and not any(
                item["name"] == "publishing" and item["status"] == "completed"
                for item in self._phase_history
            ):
                raise RuntimeError("Cannot complete workflow before publishing succeeds")
            self._current_phase = "completed"
            self._persist(stage="completed", substage=None, completed=None, total=None, stage_percent=None, eta=None, details={})

    def __call__(self, event: ProgressEvent) -> None:
        with self._lock:
            lifecycle = event.details.get("phase_event")
            if lifecycle == "started":
                self.phase_started(event.stage, details=dict(event.details))
                return
            if lifecycle == "completed":
                self.phase_completed(event.stage, details=dict(event.details))
                return
            phase_start = self._phase_started if self._current_phase is not None else self.started
            elapsed = max(0.0, time.monotonic() - phase_start)
            completed = event.completed_units
            total = event.total_units
            percent = None
            eta = None
            if completed is not None and total is not None and total > 0:
                percent = min(100.0, max(0.0, completed / total * 100.0))
                if completed > 0 and completed < total and elapsed > 0:
                    rate = completed / elapsed
                    eta = (total - completed) / rate if rate > 0 else None
            # A locked progress.json is intentionally best-effort: write_json
            # returns False after its bounded retries, without interrupting ML.
            self._persist(stage=event.stage, substage=event.substage, completed=completed, total=total, stage_percent=percent, eta=eta, details=dict(event.details))


def running_duration(status: dict[str, object]) -> float:
    started = status.get("started_at") or status["created_at"]
    end = status.get("finished_at") or datetime.now(timezone.utc).isoformat()
    return max(
        0.0,
        (datetime.fromisoformat(str(end)) - datetime.fromisoformat(str(started))).total_seconds(),
    )
