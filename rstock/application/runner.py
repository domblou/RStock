"""Run submission, local process backend and durable progress reporting."""

from __future__ import annotations

import os
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

from .domain import ExperimentSpec, JobStatus, JobType
from .repository import RunRepository, utc_now
from rstock.checkpoints import (
    CHECKPOINT_IMPLEMENTATION_VERSION,
    CHECKPOINT_SCHEMA_VERSION,
)


ACTIVE_STATUSES = {JobStatus.PENDING.value, JobStatus.RUNNING.value}


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
    try:
        numeric = int(pid)
        if numeric <= 0:
            return False
        os.kill(numeric, 0)
    except (TypeError, ValueError, ProcessLookupError):
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True


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
            run_id = self.repository.create(spec)
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
        if status["status"] == JobStatus.PENDING.value:
            return self.repository.transition(run_id, JobStatus.CANCELLED)
        return self.repository.status(run_id)

    def resume(self, run_id: str) -> SubmissionResult:
        """Resume the same run id through the normal detached worker backend."""

        from rstock.checkpoints import CheckpointManager

        with self._submission_lock():
            self._refresh_interrupted(run_id)
            status = self.repository.status(run_id)
            if status["status"] in ACTIVE_STATUSES:
                raise ValueError("Ce run est déjà en cours ou en attente.")
            spec = self.repository.load_spec(run_id)
            if spec.job_type.value != "walk_forward":
                raise ValueError("Seuls les walk-forward avec checkpoint sont reprenables.")
            if not (
                self.repository.run_directory(run_id) / "checkpoints" / "manifest.json"
            ).exists():
                raise ValueError(
                    "Aucun checkpoint de reprise n’est disponible pour ce run. "
                    "Relancez depuis le début."
                )
            CheckpointManager(
                self.repository.run_directory(run_id),
                run_id=run_id,
                job_type=spec.job_type.value,
                configuration_fingerprint=spec.fingerprint,
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
            new_run_id = self.repository.create(spec)
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
        if status.get("status") == JobStatus.RUNNING.value and not _pid_alive(
            status.get("pid")
        ):
            status = self.repository.transition(
                run_id,
                JobStatus.INTERRUPTED,
                error="Le processus worker n’est plus actif.",
            )
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
        return {
            "configuration": self.repository.load_spec(run_id).to_dict(),
            "status": self.repository.status(run_id),
            "progress": self.repository.progress(run_id),
            "summary": self.repository.summary(run_id),
            "files": self.repository.result_files(run_id),
            "log_tail": self.repository.log_tail(run_id),
            "checkpoint": checkpoint,
            "checkpoint_error": checkpoint_error,
        }

    def list(self) -> list[dict[str, object]]:
        return [self._refresh_interrupted(run_id) for run_id in self.repository.list_run_ids()]


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
