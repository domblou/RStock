"""Permanent deletion of failed or cancelled runs and their owned descendants."""

from __future__ import annotations

import json
import os
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping

from .domain import JobStatus, JobType
from .production_repository import ProductionRepository
from .repository import RunRepository
from .run_storage import RunStorageService


_ROOT_STATUSES = {JobStatus.FAILED.value, JobStatus.CANCELLED.value}
_TERMINAL_STATUSES = {status.value for status in JobStatus if status.terminal}


@dataclass(frozen=True, slots=True)
class DeleteEligibility:
    eligible: bool
    reason: str | None = None


@dataclass(frozen=True, slots=True)
class DeletePlan:
    run_id: str
    run_ids: tuple[str, ...]  # Parent first, descendants in traversal order.
    run_types: tuple[str, ...]
    by_type: tuple[tuple[str, int], ...]
    size_bytes: int


def _references(value: Any, run_ids: set[str]) -> bool:
    if isinstance(value, Mapping):
        for key, item in value.items():
            name = str(key)
            if (
                name == "run_id" or name.endswith("_run_id")
                or name.startswith("source_") and name.endswith("_run")
            ) and isinstance(item, str) and item in run_ids:
                return True
            if name.endswith("_run_ids") and isinstance(item, list):
                if any(isinstance(entry, str) and entry in run_ids for entry in item):
                    return True
            if _references(item, run_ids):
                return True
    elif isinstance(value, list):
        return any(_references(item, run_ids) for item in value)
    return False


class RunDeletionService:
    def __init__(self, repository: RunRepository) -> None:
        self.repository = repository
        self.storage = RunStorageService(repository)

    def _directory(self, run_id: str) -> Path:
        directory = self.repository.run_directory(run_id)
        if directory.is_symlink() or directory.resolve().parent != self.repository.root.resolve():
            raise ValueError(f"Chemin du run {run_id} hors du dépôt des runs.")
        return directory

    def _external_dependency(self, run_ids: set[str]) -> str | None:
        # A partially materialized run has no status.json and is absent from
        # list_run_ids(). Its metadata can still point into the deletion set.
        if self.repository.root.exists():
            for directory in self.repository.root.iterdir():
                if (
                    not directory.is_dir() or directory.name in run_ids
                    or (directory / "status.json").is_file()
                    or not (directory / "metadata.json").is_file()
                ):
                    continue
                try:
                    metadata = json.loads((directory / "metadata.json").read_text(encoding="utf-8"))
                except (OSError, ValueError) as error:
                    raise ValueError(f"Métadonnées du run incomplet {directory.name} illisibles.") from error
                if _references(metadata, run_ids):
                    return f"Le run incomplet {directory.name} référence ce périmètre."
        for other_id in self.repository.list_run_ids():
            if other_id in run_ids:
                continue
            directory = self._directory(other_id)
            files = [directory / name for name in ("config.json", "metadata.json", "status.json", "summary.json")]
            orchestration = directory / "orchestration"
            if orchestration.is_dir():
                files.extend(orchestration.rglob("*.json"))
            results = directory / "results"
            if results.is_dir():
                files.extend(results.glob("*.json"))
            checkpoint_manifest = directory / "checkpoints" / "manifest.json"
            if checkpoint_manifest.is_file():
                files.append(checkpoint_manifest)
            for path in files:
                if not path.is_file():
                    continue
                try:
                    payload = json.loads(path.read_text(encoding="utf-8"))
                except (OSError, ValueError) as error:
                    raise ValueError(f"Dépendances illisibles pour {other_id}: {path.name}") from error
                if _references(payload, run_ids):
                    return f"Le run conservé {other_id} référence ce périmètre."

        production = ProductionRepository(self.repository.root.parent)
        for model in production.models():
            if _references(model.to_dict(), run_ids):
                return f"Le modèle Production {model.model_id} référence ce périmètre."
        return None

    def _plan(self, run_id: str) -> DeletePlan:
        directory = self._directory(run_id)
        if not (directory / "status.json").is_file():
            raise ValueError("Le run n’existe pas ou son statut est absent.")
        status = self.repository.status(run_id)
        if status.get("status") not in _ROOT_STATUSES:
            raise ValueError("Seuls les runs failed ou cancelled peuvent être sélectionnés.")
        if self.storage._worker_is_active(run_id, status):
            raise ValueError("Un worker traite encore ce run.")
        job_type = JobType(str(status["job_type"]))
        related, error = self.storage._related_runs(run_id, job_type, for_delete=True)
        if error is not None:
            raise ValueError(error)
        run_ids = (run_id, *related)
        counts: dict[str, int] = {}
        types: list[str] = []
        size = 0
        for candidate in run_ids:
            child_directory = self._directory(candidate)
            if not (child_directory / "status.json").is_file():
                raise ValueError(f"Statut de l’enfant propriétaire {candidate} absent.")
            child_status = self.repository.status(candidate)
            if child_status.get("status") not in _TERMINAL_STATUSES:
                raise ValueError(f"L’enfant propriétaire {candidate} n’est pas terminal.")
            if self.storage._worker_is_active(candidate, child_status):
                raise ValueError(f"Un worker traite encore l’enfant {candidate}.")
            kind = JobType(str(child_status["job_type"])).value
            types.append(kind)
            counts[kind] = counts.get(kind, 0) + 1
            for base, _directories, files in os.walk(child_directory, followlinks=False):
                for name in files:
                    try:
                        size += (Path(base) / name).lstat().st_size
                    except FileNotFoundError:
                        pass
        dependency = self._external_dependency(set(run_ids))
        if dependency:
            raise ValueError(dependency)
        return DeletePlan(run_id, tuple(run_ids), tuple(types), tuple(sorted(counts.items())), size)

    def eligibility(self, run_id: str) -> DeleteEligibility:
        try:
            self._plan(run_id)
        except (OSError, ValueError, KeyError, TypeError) as error:
            return DeleteEligibility(False, str(error))
        return DeleteEligibility(True)

    def preview(self, run_id: str) -> DeletePlan:
        return self._plan(run_id)

    def delete(self, run_id: str, *, expected_run_ids: tuple[str, ...]) -> DeletePlan:
        from .runner import RunService

        with RunService(self.repository)._submission_lock():
            plan = self._plan(run_id)
            if plan.run_ids != expected_run_ids:
                raise ValueError("Le périmètre de suppression a changé depuis la confirmation.")
            for candidate in reversed(plan.run_ids):
                shutil.rmtree(self._directory(candidate))
                self.repository._progress_cache.pop(candidate, None)
            return plan
