"""Filesystem persistence for jobs and experiment artifacts."""

from __future__ import annotations

import json
import os
import tempfile
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from rstock.atomic_io import (
    ATOMIC_WRITE_ATTEMPTS,
    ATOMIC_WRITE_BACKOFF_SECONDS,
    is_temporary_file_lock,
)
from .domain import ExperimentSpec, JobStatus


STATUS_TRANSITIONS = {
    JobStatus.PENDING: {JobStatus.RUNNING, JobStatus.FAILED, JobStatus.CANCELLED},
    JobStatus.RUNNING: {
        JobStatus.COMPLETED,
        JobStatus.FAILED,
        JobStatus.CANCELLED,
        JobStatus.INTERRUPTED,
    },
    JobStatus.COMPLETED: set(),
    JobStatus.FAILED: {JobStatus.PENDING},
    JobStatus.CANCELLED: {JobStatus.PENDING},
    JobStatus.INTERRUPTED: {JobStatus.PENDING},
}

JSON_WRITE_ATTEMPTS = ATOMIC_WRITE_ATTEMPTS
JSON_WRITE_BACKOFF_SECONDS = ATOMIC_WRITE_BACKOFF_SECONDS
JSON_READ_ATTEMPTS = 4
JSON_READ_BACKOFF_SECONDS = 0.01


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


class RunRepository:
    """Atomic JSON persistence; no Streamlit session state is required."""

    def __init__(self, root: Path) -> None:
        self.root = Path(root)
        self._write_lock = threading.Lock()
        self._progress_cache: dict[str, dict[str, Any]] = {}

    def run_directory(self, run_id: str) -> Path:
        if not run_id or Path(run_id).name != run_id:
            raise ValueError("Invalid run_id")
        return self.root / run_id

    def create(self, spec: ExperimentSpec) -> str:
        self.root.mkdir(parents=True, exist_ok=True)
        run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S") + "_" + uuid.uuid4().hex[:10]
        directory = self.run_directory(run_id)
        directory.mkdir()
        created_at = utc_now()
        self.write_json(run_id, "config.json", spec.to_dict())
        self.write_json(
            run_id,
            "status.json",
            {
                "run_id": run_id,
                "job_type": spec.job_type.value,
                "configuration_fingerprint": spec.fingerprint,
                "status": JobStatus.PENDING.value,
                "created_at": created_at,
                "started_at": None,
                "finished_at": None,
                "duration_seconds": None,
                "error": None,
                "pid": None,
                "cancellation_requested": False,
            },
        )
        self.write_json(
            run_id,
            "progress.json",
            {
                "stage": "pending",
                "substage": None,
                "completed_units": None,
                "total_units": None,
                "percent": None,
                "stage_percent": None,
                "workflow_percent": 0.0,
                "elapsed_seconds": 0.0,
                "eta_seconds": None,
                "phase_history": [],
                "updated_at": created_at,
            },
            tolerate_progress_failure=False,
        )
        self.write_json(run_id, "summary.json", {})
        (directory / "run.log").touch()
        return run_id

    def read_json(self, run_id: str, name: str) -> dict[str, Any]:
        path = self.run_directory(run_id) / name
        if name != "progress.json":
            return self._read_json_path(path)
        for attempt in range(JSON_READ_ATTEMPTS):
            try:
                values = self._read_json_path(path)
            except OSError as error:
                if not self._temporary_lock(error):
                    raise
                if attempt < JSON_READ_ATTEMPTS - 1:
                    time.sleep(JSON_READ_BACKOFF_SECONDS * (2**attempt))
                    continue
                cached = self._progress_cache.get(run_id)
                if cached is not None:
                    return dict(cached)
                return self._temporarily_unavailable_progress()
            else:
                self._progress_cache[run_id] = dict(values)
                return values
        raise AssertionError("Progress JSON read loop exited unexpectedly")

    @staticmethod
    def _read_json_path(path: Path) -> dict[str, Any]:
        """Decode JSON normally so malformed content remains visible to callers."""

        return json.loads(path.read_text(encoding="utf-8"))

    @staticmethod
    def _temporarily_unavailable_progress() -> dict[str, Any]:
        """Safe display state used only after transient Windows read-lock retries."""

        return {
            "stage": "progress_temporarily_unavailable",
            "substage": None,
            "completed_units": None,
            "total_units": None,
            "percent": None,
            "stage_percent": None,
            "workflow_percent": None,
            "elapsed_seconds": None,
            "eta_seconds": None,
            "phase_history": [],
            "temporarily_unavailable": True,
        }

    @staticmethod
    def _temporary_lock(error: OSError) -> bool:
        """Recognise the common Windows sharing/access-denied lock errors."""
        return is_temporary_file_lock(error)

    def _atomic_json_write(self, destination: Path, payload: str) -> None:
        """Write and close a sibling temporary file before atomically replacing."""

        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
        )
        temporary = Path(temporary_name)
        try:
            # fdopen owns and closes the descriptor before os.replace runs. This
            # matters on Windows, where replacing an open temporary file fails.
            with os.fdopen(descriptor, "w", encoding="utf-8", newline="\n") as stream:
                stream.write(payload)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(temporary, destination)
        finally:
            if temporary.exists():
                temporary.unlink()

    def _log_progress_write_failure(self, run_id: str, error: OSError) -> None:
        try:
            self.append_log(
                run_id,
                "Progress update skipped after temporary file-lock retries: "
                f"{error}",
            )
        except OSError:
            # Progress publication remains best-effort even if an external viewer
            # has independently locked the log file.
            pass

    def write_json(
        self,
        run_id: str,
        name: str,
        values: dict[str, Any],
        *,
        tolerate_progress_failure: bool = True,
    ) -> bool:
        """Atomically persist JSON, retrying transient Windows file locks.

        Returns ``False`` only for a non-critical ``progress.json`` update that
        could not be published after retries. Critical JSON files always raise.
        """

        directory = self.run_directory(run_id)
        directory.mkdir(parents=True, exist_ok=True)
        destination = directory / name
        payload = json.dumps(values, indent=2, ensure_ascii=False, default=str) + "\n"
        progress_update = name == "progress.json" and tolerate_progress_failure
        with self._write_lock:
            for attempt in range(JSON_WRITE_ATTEMPTS):
                try:
                    self._atomic_json_write(destination, payload)
                    if name == "progress.json":
                        self._progress_cache[run_id] = dict(values)
                    return True
                except OSError as error:
                    if not self._temporary_lock(error):
                        raise
                    if attempt == JSON_WRITE_ATTEMPTS - 1:
                        if progress_update:
                            self._log_progress_write_failure(run_id, error)
                            return False
                        raise
                    time.sleep(JSON_WRITE_BACKOFF_SECONDS * (2**attempt))
        raise AssertionError("JSON write loop exited unexpectedly")

    def load_spec(self, run_id: str) -> ExperimentSpec:
        return ExperimentSpec.from_dict(self.read_json(run_id, "config.json"))

    def status(self, run_id: str) -> dict[str, Any]:
        return self.read_json(run_id, "status.json")

    def progress(self, run_id: str) -> dict[str, Any]:
        return self.read_json(run_id, "progress.json")

    def summary(self, run_id: str) -> dict[str, Any]:
        return self.read_json(run_id, "summary.json")

    def transition(
        self,
        run_id: str,
        target: JobStatus,
        *,
        error: str | None = None,
        pid: int | None = None,
    ) -> dict[str, Any]:
        status = self.status(run_id)
        current = JobStatus(status["status"])
        if target == current:
            return status
        if target not in STATUS_TRANSITIONS[current]:
            raise ValueError(f"Invalid job transition: {current.value} -> {target.value}")
        now = utc_now()
        if target == JobStatus.RUNNING:
            status["started_at"] = now
            status["pid"] = pid
            status["finished_at"] = None
            status["error"] = None
            status["cancellation_requested"] = False
        if target == JobStatus.PENDING:
            status["pid"] = None
            status["finished_at"] = None
            status["error"] = None
            status["cancellation_requested"] = False
        if target.terminal:
            status["finished_at"] = now
            status["error"] = error
            started = status.get("started_at") or status["created_at"]
            status["duration_seconds"] = max(
                0.0,
                (datetime.fromisoformat(now) - datetime.fromisoformat(started)).total_seconds(),
            )
        status["status"] = target.value
        self.write_json(run_id, "status.json", status)
        return status

    def prepare_resume(self, run_id: str) -> dict[str, Any]:
        status = self.status(run_id)
        current = JobStatus(status["status"])
        if current not in {
            JobStatus.FAILED,
            JobStatus.CANCELLED,
            JobStatus.INTERRUPTED,
        }:
            raise ValueError(f"Run {run_id} is not resumable from status {current.value}")
        cancellation = self.run_directory(run_id) / "cancel.requested"
        cancellation.unlink(missing_ok=True)
        return self.transition(run_id, JobStatus.PENDING)

    def request_cancellation(self, run_id: str) -> dict[str, Any]:
        status = self.status(run_id)
        current = JobStatus(status["status"])
        if current.terminal:
            return status
        status["cancellation_requested"] = True
        self.write_json(run_id, "status.json", status)
        (self.run_directory(run_id) / "cancel.requested").touch()
        return status

    def cancellation_requested(self, run_id: str) -> bool:
        return (self.run_directory(run_id) / "cancel.requested").exists()

    def append_log(self, run_id: str, message: str) -> None:
        timestamp = utc_now()
        with (self.run_directory(run_id) / "run.log").open("a", encoding="utf-8") as stream:
            stream.write(f"{timestamp} {message.rstrip()}\n")

    def log_tail(self, run_id: str, lines: int = 20) -> list[str]:
        path = self.run_directory(run_id) / "run.log"
        if not path.exists():
            return []
        return path.read_text(encoding="utf-8", errors="replace").splitlines()[-lines:]

    def list_run_ids(self) -> list[str]:
        if not self.root.exists():
            return []
        return sorted(
            (
                path.name
                for path in self.root.iterdir()
                if path.is_dir() and (path / "status.json").exists()
            ),
            reverse=True,
        )

    def list_runs(self) -> list[dict[str, Any]]:
        return [self.status(run_id) for run_id in self.list_run_ids()]

    def result_files(self, run_id: str) -> list[str]:
        directory = self.run_directory(run_id) / "results"
        if not directory.exists():
            return []
        return sorted(str(path.relative_to(directory)) for path in directory.rglob("*") if path.is_file())
