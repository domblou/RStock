"""Atomic, versioned checkpoints for resumable scientific workflows."""

from __future__ import annotations

import hashlib
import json
import os
import pickle
import tempfile
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


CHECKPOINT_SCHEMA_VERSION = 1
CHECKPOINT_IMPLEMENTATION_VERSION = "walk-forward-v1"


class CheckpointError(RuntimeError):
    """Base class for explicit checkpoint failures."""


class CheckpointIncompatibleError(CheckpointError):
    """The checkpoint cannot safely be combined with this execution."""


class CheckpointCorruptError(CheckpointError):
    """A committed checkpoint failed integrity validation."""


def _utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _sha256(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _atomic_bytes(destination: Path, payload: bytes) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)


def _atomic_json(destination: Path, values: dict[str, Any]) -> None:
    _atomic_bytes(
        destination,
        (json.dumps(values, indent=2, ensure_ascii=False, default=str) + "\n").encode(
            "utf-8"
        ),
    )


@dataclass(frozen=True, slots=True)
class BatchDescriptor:
    phase: str
    batch_id: int
    first_index: int
    last_index: int
    combination_count: int
    row_counts: dict[str, int]
    payload_sha256: str
    completed_at: str


class CheckpointManager:
    """Manage one run's internal checkpoints and recovery manifest."""

    def __init__(
        self,
        run_directory: Path,
        *,
        run_id: str,
        job_type: str,
        configuration_fingerprint: str,
        batch_sizes: dict[str, int],
    ) -> None:
        self.run_directory = Path(run_directory)
        self.root = self.run_directory / "checkpoints"
        self.manifest_path = self.root / "manifest.json"
        self.run_id = run_id
        self.job_type = job_type
        self.configuration_fingerprint = configuration_fingerprint
        self.batch_sizes = {name: int(value) for name, value in batch_sizes.items()}
        if any(value < 1 for value in self.batch_sizes.values()):
            raise ValueError("Checkpoint batch sizes must be positive")
        self.root.mkdir(parents=True, exist_ok=True)
        if self.manifest_path.exists():
            self._manifest = self._read_manifest()
            self._validate_identity()
            self.reconcile_batches()
        else:
            now = _utc_now()
            self._manifest: dict[str, Any] = {
                "checkpoint_schema_version": CHECKPOINT_SCHEMA_VERSION,
                "implementation_version": CHECKPOINT_IMPLEMENTATION_VERSION,
                "run_id": run_id,
                "job_type": job_type,
                "configuration_fingerprint": configuration_fingerprint,
                "state": "created",
                "current_phase": None,
                "phases_completed": [],
                "batch_sizes": self.batch_sizes,
                "batches": {},
                "created_at": now,
                "updated_at": now,
                "last_checkpoint": None,
                "attempt_count": 0,
                "resume_count": 0,
                "attempts": [],
                "error": None,
            }
            self._write_manifest()

    @property
    def manifest(self) -> dict[str, Any]:
        return json.loads(json.dumps(self._manifest))

    def _read_manifest(self) -> dict[str, Any]:
        try:
            values = json.loads(self.manifest_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise CheckpointCorruptError(f"Manifest checkpoint illisible : {error}") from error
        if not isinstance(values, dict):
            raise CheckpointCorruptError("Manifest checkpoint invalide")
        return values

    def _validate_identity(self) -> None:
        if self._manifest.get("checkpoint_schema_version") != CHECKPOINT_SCHEMA_VERSION:
            raise CheckpointIncompatibleError(
                "Ce run ne peut pas être repris car la version du checkpoint est "
                "incompatible. Relancez depuis le début."
            )
        if self._manifest.get("implementation_version") != CHECKPOINT_IMPLEMENTATION_VERSION:
            raise CheckpointIncompatibleError(
                "Ce run ne peut pas être repris avec cette version du moteur. "
                "Relancez depuis le début."
            )
        expected = (self.run_id, self.job_type, self.configuration_fingerprint)
        actual = (
            self._manifest.get("run_id"),
            self._manifest.get("job_type"),
            self._manifest.get("configuration_fingerprint"),
        )
        if actual != expected or self._manifest.get("batch_sizes") != self.batch_sizes:
            raise CheckpointIncompatibleError(
                "Ce run ne peut pas être repris car sa configuration ne correspond "
                "plus au checkpoint. Relancez depuis le début."
            )

    def _write_manifest(self) -> None:
        self._manifest["updated_at"] = _utc_now()
        _atomic_json(self.manifest_path, self._manifest)

    def start_attempt(self, *, resumed: bool) -> int:
        now = _utc_now()
        self._manifest["attempt_count"] = int(self._manifest["attempt_count"]) + 1
        if resumed:
            self._manifest["resume_count"] = int(self._manifest["resume_count"]) + 1
        attempt = {
            "attempt": self._manifest["attempt_count"],
            "started_at": now,
            "resumed_at": now if resumed else None,
            "stopped_at": None,
            "status": "running",
            "phase": self._manifest.get("current_phase"),
            "error": None,
        }
        self._manifest["attempts"].append(attempt)
        self._manifest["state"] = "running"
        self._manifest["error"] = None
        self._write_manifest()
        return int(attempt["attempt"])

    def finish_attempt(self, status: str, error: str | None = None) -> None:
        # The worker and the scientific workflow may hold separate manager
        # instances. Refresh before the final write so a stale worker instance
        # cannot erase phases or batches committed by the workflow instance.
        self._manifest = self._read_manifest()
        self._validate_identity()
        self.reconcile_batches()
        now = _utc_now()
        attempts = self._manifest.get("attempts", [])
        if attempts:
            attempts[-1].update(
                {
                    "stopped_at": now,
                    "status": status,
                    "phase": self._manifest.get("current_phase"),
                    "error": error,
                }
            )
        self._manifest["state"] = status
        self._manifest["error"] = error
        self._write_manifest()

    def phase_started(self, phase: str) -> None:
        self._manifest["current_phase"] = phase
        self._manifest["state"] = "running"
        self._manifest["last_checkpoint"] = {
            "phase": phase,
            "event": "started",
            "at": _utc_now(),
        }
        self._write_manifest()

    def phase_completed(self, phase: str) -> None:
        completed = list(self._manifest.get("phases_completed", []))
        if phase not in completed:
            completed.append(phase)
        self._manifest["phases_completed"] = completed
        self._manifest["current_phase"] = phase
        self._manifest["last_checkpoint"] = {
            "phase": phase,
            "event": "completed",
            "at": _utc_now(),
        }
        self._write_manifest()

    def phase_is_completed(self, phase: str) -> bool:
        return phase in self._manifest.get("phases_completed", [])

    def _phase_directory(self, phase: str) -> Path:
        if not phase or Path(phase).name != phase:
            raise ValueError("Invalid checkpoint phase")
        return self.root / "batches" / phase

    def _batch_directory(self, phase: str, batch_id: int) -> Path:
        if batch_id < 0:
            raise ValueError("batch_id must be non-negative")
        return self._phase_directory(phase) / f"batch-{batch_id:06d}"

    def completed_batch_ids(self, phase: str) -> tuple[int, ...]:
        phase_info = self._manifest.get("batches", {}).get(phase, {})
        return tuple(sorted(int(value) for value in phase_info.get("completed", [])))

    def set_total_batches(self, phase: str, total: int) -> None:
        batches = self._manifest.setdefault("batches", {})
        info = batches.setdefault(phase, {"total": total, "completed": []})
        if info.get("total") not in (None, total):
            raise CheckpointIncompatibleError(
                "Le nombre de batchs ne correspond plus au checkpoint. "
                "Relancez depuis le début."
            )
        info["total"] = int(total)
        self._write_manifest()

    def commit_batch(
        self,
        phase: str,
        batch_id: int,
        payload: Any,
        *,
        first_index: int,
        last_index: int,
        combination_count: int,
        row_counts: dict[str, int],
    ) -> BatchDescriptor:
        directory = self._batch_directory(phase, batch_id)
        directory.mkdir(parents=True, exist_ok=True)
        payload_bytes = pickle.dumps(payload, protocol=pickle.HIGHEST_PROTOCOL)
        digest = _sha256(payload_bytes)
        completed_at = _utc_now()
        descriptor = BatchDescriptor(
            phase=phase,
            batch_id=batch_id,
            first_index=first_index,
            last_index=last_index,
            combination_count=combination_count,
            row_counts={name: int(value) for name, value in row_counts.items()},
            payload_sha256=digest,
            completed_at=completed_at,
        )
        metadata = {
            **asdict(descriptor),
            "configuration_fingerprint": self.configuration_fingerprint,
            "checkpoint_schema_version": CHECKPOINT_SCHEMA_VERSION,
        }
        _atomic_bytes(directory / "payload.pkl", payload_bytes)
        _atomic_json(directory / "metadata.json", metadata)
        # This marker is deliberately committed last.
        _atomic_json(directory / "complete.json", metadata)
        batches = self._manifest.setdefault("batches", {})
        phase_info = batches.setdefault(phase, {"total": None, "completed": []})
        completed = {int(value) for value in phase_info.get("completed", [])}
        completed.add(batch_id)
        phase_info["completed"] = sorted(completed)
        self._manifest["last_checkpoint"] = {
            "phase": phase,
            "batch_id": batch_id,
            "at": completed_at,
        }
        self._write_manifest()
        return descriptor

    def _validated_batch_metadata(self, phase: str, batch_id: int) -> dict[str, Any]:
        directory = self._batch_directory(phase, batch_id)
        marker = directory / "complete.json"
        payload_path = directory / "payload.pkl"
        try:
            metadata = json.loads(marker.read_text(encoding="utf-8"))
            payload = payload_path.read_bytes()
        except (OSError, json.JSONDecodeError) as error:
            raise CheckpointCorruptError(
                f"Checkpoint du batch {phase}/{batch_id} illisible : {error}"
            ) from error
        if (
            metadata.get("configuration_fingerprint") != self.configuration_fingerprint
            or metadata.get("phase") != phase
            or int(metadata.get("batch_id", -1)) != batch_id
            or metadata.get("payload_sha256") != _sha256(payload)
        ):
            raise CheckpointCorruptError(
                f"Checkpoint du batch {phase}/{batch_id} corrompu ou incompatible"
            )
        return metadata

    def load_batch(self, phase: str, batch_id: int) -> Any:
        self._validated_batch_metadata(phase, batch_id)
        payload_path = self._batch_directory(phase, batch_id) / "payload.pkl"
        try:
            return pickle.loads(payload_path.read_bytes())
        except (OSError, pickle.UnpicklingError, EOFError, AttributeError) as error:
            raise CheckpointCorruptError(
                f"Payload du batch {phase}/{batch_id} corrompu : {error}"
            ) from error

    def reconcile_batches(self) -> None:
        batches_root = self.root / "batches"
        if not batches_root.exists():
            return
        changed = False
        batches = self._manifest.setdefault("batches", {})
        for phase_directory in sorted(path for path in batches_root.iterdir() if path.is_dir()):
            phase = phase_directory.name
            completed = {
                int(value)
                for value in batches.setdefault(
                    phase, {"total": None, "completed": []}
                ).get("completed", [])
            }
            for directory in sorted(path for path in phase_directory.iterdir() if path.is_dir()):
                marker = directory / "complete.json"
                if not marker.exists():
                    continue
                try:
                    batch_id = int(directory.name.removeprefix("batch-"))
                    self._validated_batch_metadata(phase, batch_id)
                except (ValueError, CheckpointCorruptError):
                    raise CheckpointCorruptError(
                        f"Checkpoint committé invalide : {directory}"
                    )
                if batch_id not in completed:
                    completed.add(batch_id)
                    changed = True
            batches[phase]["completed"] = sorted(completed)
        if changed:
            self._write_manifest()

    def commit_artifact(self, name: str, payload: Any) -> Path:
        if not name or Path(name).name != name:
            raise ValueError("Invalid checkpoint artifact name")
        raw = pickle.dumps(payload, protocol=pickle.HIGHEST_PROTOCOL)
        path = self.root / "artifacts" / f"{name}.pkl"
        _atomic_bytes(path, raw)
        _atomic_json(
            path.with_suffix(".json"),
            {
                "name": name,
                "sha256": _sha256(raw),
                "configuration_fingerprint": self.configuration_fingerprint,
                "written_at": _utc_now(),
            },
        )
        return path

    def load_artifact(self, name: str) -> Any:
        path = self.root / "artifacts" / f"{name}.pkl"
        metadata_path = path.with_suffix(".json")
        try:
            raw = path.read_bytes()
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise CheckpointCorruptError(f"Artefact checkpoint {name} illisible : {error}") from error
        if (
            metadata.get("sha256") != _sha256(raw)
            or metadata.get("configuration_fingerprint") != self.configuration_fingerprint
        ):
            raise CheckpointCorruptError(f"Artefact checkpoint {name} corrompu")
        try:
            return pickle.loads(raw)
        except (pickle.UnpicklingError, EOFError, AttributeError) as error:
            raise CheckpointCorruptError(f"Artefact checkpoint {name} corrompu") from error

    def artifact_exists(self, name: str) -> bool:
        path = self.root / "artifacts" / f"{name}.pkl"
        return path.exists() and path.with_suffix(".json").exists()

    def commit_snapshot(self, prepared: Any, metadata: dict[str, Any]) -> None:
        self.commit_artifact("prepared_snapshot", {"prepared": prepared, "metadata": metadata})

    def load_snapshot(self) -> tuple[Any, dict[str, Any]]:
        payload = self.load_artifact("prepared_snapshot")
        if not isinstance(payload, dict) or "prepared" not in payload:
            raise CheckpointCorruptError("Snapshot des entrées invalide")
        return payload["prepared"], dict(payload.get("metadata", {}))
