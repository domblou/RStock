"""Validated global deletion, with recoverable quarantine and publication guards."""

from __future__ import annotations

import json
import os
import shutil
import hashlib
import uuid
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Any, Iterator

from .domain import JobStatus, JobType
from .production_repository import ProductionRepository
from .repository import RunRepository
from .run_storage import RunStorageService
from .run_integrity import graph_lock, journals, references, file_references


_ROOT_STATUSES = {JobStatus.COMPLETED.value, JobStatus.FAILED.value, JobStatus.CANCELLED.value}
_TERMINAL_STATUSES = {status.value for status in JobStatus if status.terminal}


@dataclass(frozen=True, slots=True)
class DeleteEligibility:
    eligible: bool
    reason: str | None = None


class DeletionCleanupPending(OSError):
    """Logical deletion committed; disk cleanup remains safely resumable."""

    def __init__(self, run_ids: tuple[str, ...], reason: str) -> None:
        self.run_ids = run_ids
        super().__init__(f"Suppression validée pour {len(run_ids)} runs ; nettoyage de la quarantaine à reprendre : {reason}")


@dataclass(frozen=True, slots=True)
class DeletePlan:
    run_id: str
    run_ids: tuple[str, ...]  # Unique roots/descendants in selection traversal order.
    run_types: tuple[str, ...]
    by_type: tuple[tuple[str, int], ...]
    size_bytes: int
    requested_run_ids: tuple[str, ...] = ()
    fingerprint: str = ""


def _references(value: Any, run_ids: set[str]) -> bool:
    return any(target in run_ids for _, target in references(value))


def _walk_error(error: OSError) -> None:
    raise error


def _reference_paths(directory: Path) -> Iterator[Path]:
    """Never silently skip unreadable folders or filesystem references."""
    if directory.is_symlink() or getattr(directory, "is_junction", lambda: False)():
        raise ValueError(f"Lien interdit pendant le contrôle des références : {directory}")
    if not directory.exists():
        return
    for base, directories, files in os.walk(directory, followlinks=False, onerror=_walk_error):
        for name in (*directories, *files):
            path = Path(base) / name
            if path.is_symlink() or getattr(path, "is_junction", lambda: False)():
                raise ValueError(f"Lien interdit pendant le contrôle des références : {path}")
        for name in sorted(files):
            path = Path(base) / name
            if path.suffix in {".json", ".csv"}:
                yield path


class RunDeletionService:
    def __init__(self, repository: RunRepository) -> None:
        self.repository = repository
        self.storage = RunStorageService(repository)

    def _directory(self, run_id: str) -> Path:
        directory = self.repository.run_directory(run_id)
        if (directory.is_symlink() or getattr(directory, "is_junction", lambda: False)()
                or directory.resolve().parent != self.repository.root.resolve()):
            raise ValueError(f"Chemin du run {run_id} hors du dépôt des runs.")
        return directory

    def _external_dependency(self, run_ids: set[str]) -> str | None:
        # Include incomplete runs: config/metadata may precede status publication.
        directories = sorted(self.repository.root.iterdir()) if self.repository.root.exists() else ()
        for directory in directories:
            if not directory.is_dir() or directory.name == ".deletions" or directory.name in run_ids:
                continue
            other_id = directory.name
            directory = self._directory(other_id)
            for path in _reference_paths(directory):
                if not path.is_file():
                    continue
                try:
                    for field, target in file_references(path):
                        if target in run_ids:
                            return (f"Le run conservé {other_id} référence {target} "
                                    f"({path.relative_to(directory)} : {field}).")
                except (OSError, ValueError) as error:
                    raise ValueError(f"Dépendances illisibles pour {other_id}: {path.name}") from error

        production = ProductionRepository(self.repository.root.parent)
        for model in production.models():
            if _references(model.to_dict(), run_ids):
                return f"Le modèle Production {model.model_id} référence ce périmètre."
        for path in _reference_paths(production.root):
            try:
                for field, target in file_references(path):
                    if target in run_ids:
                        return f"Production référence {target} ({path.relative_to(production.root)} : {field})."
            except (OSError, ValueError) as error:
                raise ValueError(f"Dépendances Production illisibles : {path}") from error
        simulations = self.repository.root.parent / "simulations"
        for path in _reference_paths(simulations):
            try:
                for field, target in file_references(path):
                    if target in run_ids:
                        return f"La simulation conservée {path.parent.name} référence {target} ({field})."
            except (OSError, ValueError) as error:
                raise ValueError(f"Dépendances de simulation illisibles : {path}") from error
        return None

    def _plan(self, run_id: str, *, check_dependencies: bool = True) -> DeletePlan:
        directory = self._directory(run_id)
        if not (directory / "status.json").is_file():
            raise ValueError("Le run n’existe pas ou son statut est absent.")
        status = self.repository.status(run_id)
        if status.get("status") not in _ROOT_STATUSES:
            raise ValueError("Seuls les runs completed, failed ou cancelled peuvent être sélectionnés.")
        if self.storage._worker_is_active(run_id, status):
            raise ValueError("Un worker traite encore ce run.")
        job_type = JobType(str(status["job_type"]))
        if (job_type in {JobType.END_TO_END, JobType.FORCED_CANDIDATE_VALIDATION}
                and status.get("status") == JobStatus.COMPLETED.value
                and not (directory / "orchestration/pipeline.json").is_file()):
            raise ValueError(f"Manifest du pipeline completed {run_id} absent.")
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
            if (kind in {JobType.END_TO_END.value, JobType.FORCED_CANDIDATE_VALIDATION.value}
                    and child_status.get("status") == JobStatus.COMPLETED.value
                    and not (child_directory / "orchestration/pipeline.json").is_file()):
                raise ValueError(f"Manifest du pipeline completed {candidate} absent.")
            types.append(kind)
            counts[kind] = counts.get(kind, 0) + 1
            for base, _directories, files in os.walk(child_directory, followlinks=False, onerror=_walk_error):
                for name in (*_directories, *files):
                    path = Path(base) / name
                    if path.is_symlink() or getattr(path, "is_junction", lambda: False)():
                        raise ValueError(f"Lien interdit dans le run {candidate}: {path}")
                for name in files:
                    try:
                        size += (Path(base) / name).lstat().st_size
                    except FileNotFoundError:
                        pass
        dependency = self._external_dependency(set(run_ids)) if check_dependencies else None
        if dependency:
            raise ValueError(dependency)
        return DeletePlan(run_id, tuple(run_ids), tuple(types), tuple(sorted(counts.items())), size)

    def eligibility(self, run_id: str) -> DeleteEligibility:
        try:
            self.preview(run_id)
        except (OSError, ValueError, KeyError, TypeError) as error:
            return DeleteEligibility(False, str(error))
        return DeleteEligibility(True)

    def preview(self, run_id: str) -> DeletePlan:
        return self.preview_many((run_id,))

    def preview_many(self, run_ids: tuple[str, ...]) -> DeletePlan:
        with graph_lock(self.repository.root):
            self._recover_locked()
            return self._global_plan(run_ids)

    def _global_plan(self, requested: tuple[str, ...]) -> DeletePlan:
        requested = tuple(dict.fromkeys(requested))
        if not requested:
            raise ValueError("Sélection de suppression vide.")
        plans = [self._plan(run_id, check_dependencies=False) for run_id in requested]
        ids = tuple(dict.fromkeys(candidate for plan in plans for candidate in plan.run_ids))
        parents = {candidate: self.repository.run_metadata(candidate).parent_run_id for candidate in ids}
        for candidate in ids:
            chain: set[str] = set()
            cursor = candidate
            while cursor in parents:
                if cursor in chain:
                    raise ValueError(f"Cycle de propriété détecté pour {candidate}.")
                chain.add(cursor)
                cursor = parents[cursor]
            metadata = self.repository.run_metadata(candidate)
            if metadata.parent_run_id and metadata.parent_run_id not in ids:
                raise ValueError(f"Le parent conservé {metadata.parent_run_id} possède encore {candidate}. Sélectionnez le parent.")
            if metadata.parent_run_id:
                parent = self.repository.run_metadata(metadata.parent_run_id)
                expected_root = parent.root_run_id or metadata.parent_run_id
                if metadata.root_run_id and metadata.root_run_id != expected_root:
                    raise ValueError(f"Racine de propriété incohérente pour {candidate}.")
        dependency = self._external_dependency(set(ids))
        if dependency:
            raise ValueError(dependency)
        types_by_id = {candidate: kind for plan in plans
                       for candidate, kind in zip(plan.run_ids, plan.run_types)}
        counts: dict[str, int] = {}
        size = 0
        digest = hashlib.sha256()
        for candidate in ids:
            kind = types_by_id[candidate]
            counts[kind] = counts.get(kind, 0) + 1
            directory = self._directory(candidate)
            for path in sorted(directory.rglob("*")):
                if path.is_file():
                    size += path.stat().st_size
                    # Confirm the persisted state, not just membership in the graph.
                    if path.suffix == ".json":
                        json.loads(path.read_text(encoding="utf-8"))
                        digest.update(candidate.encode())
                        digest.update(str(path.relative_to(directory)).encode())
                        digest.update(path.read_bytes())
        return DeletePlan(requested[0], ids, tuple(types_by_id[item] for item in ids),
                          tuple(sorted(counts.items())), size, requested, digest.hexdigest())

    def delete(self, run_id: str, *, expected_run_ids: tuple[str, ...],
               expected_fingerprint: str) -> DeletePlan:
        plan = self.preview(run_id)
        if plan.run_ids != expected_run_ids:
            raise ValueError("Le périmètre de suppression a changé depuis la confirmation.")
        plan = DeletePlan(**{**asdict(plan), "fingerprint": expected_fingerprint})
        return self.delete_many(plan)

    def delete_many(self, expected: DeletePlan) -> DeletePlan:
        from .runner import RunService

        with RunService(self.repository)._submission_lock():
            with graph_lock(self.repository.root):
                self._recover_locked()
                plan = self._global_plan(expected.requested_run_ids or (expected.run_id,))
                if plan.run_ids != expected.run_ids or plan.fingerprint != expected.fingerprint:
                    raise ValueError("Le plan de suppression a changé depuis la confirmation. Prévisualisez à nouveau.")
                operation = self.repository.root / ".deletions" / uuid.uuid4().hex
                operation.mkdir(parents=True)
                (operation / "quarantine").mkdir()
                journal = {"schema_version": 1, "state": "staging", "run_ids": list(plan.run_ids),
                           "plan": asdict(plan)}
                self._write_journal(operation / "journal.json", journal)
                try:
                    for candidate in plan.run_ids:
                        self._directory(candidate).rename(operation / "quarantine" / candidate)
                    # Re-read before publishing the commit; no stale journal overwrite.
                    journal = json.loads((operation / "journal.json").read_text(encoding="utf-8"))
                    journal["state"] = "committed"
                    self._write_journal(operation / "journal.json", journal)
                except BaseException:
                    self._recover_locked()
                    raise
                self._recover_locked()
                for candidate in plan.run_ids:
                    self.repository._progress_cache.pop(candidate, None)
                return plan

    def _write_journal(self, path: Path, payload: dict[str, Any]) -> None:
        self.repository._atomic_json_write(path, json.dumps(payload, ensure_ascii=False, indent=2))

    def recover(self) -> None:
        """Reconcile a crashed staging operation or finish committed cleanup."""
        from .runner import RunService
        with RunService(self.repository)._submission_lock():
            with graph_lock(self.repository.root):
                self._recover_locked()

    def _recover_locked(self) -> None:
        for path, journal in journals(self.repository.root):
            state = journal["state"]
            if state in {"complete", "rolled_back"}:
                continue
            quarantine = path.parent / "quarantine"
            if (quarantine.is_symlink() or getattr(quarantine, "is_junction", lambda: False)()
                    or quarantine.resolve().parent != path.parent.resolve()):
                raise ValueError("Invalid quarantine directory")
            # Preflight the whole operation before attempting any rollback/cleanup.
            for candidate in journal["run_ids"]:
                original = self._directory(candidate)
                held = quarantine / candidate
                if held.is_symlink() or getattr(held, "is_junction", lambda: False)():
                    raise ValueError("Invalid quarantined run")
                if state == "committed" and held.exists():
                    for base, directories, files in os.walk(held, followlinks=False, onerror=_walk_error):
                        for name in (*directories, *files):
                            entry = Path(base) / name
                            if entry.is_symlink() or getattr(entry, "is_junction", lambda: False)():
                                raise ValueError(f"Lien interdit dans la quarantaine : {entry}")
                if state == "staging" and original.exists() == held.exists():
                    raise RuntimeError(f"Conflit de réconciliation pour {candidate}.")
                if state == "committed" and original.exists():
                    raise RuntimeError(f"Run recréé pendant la suppression : {candidate}.")
            for candidate in journal["run_ids"]:
                original = self._directory(candidate)
                held = quarantine / candidate
                if held.is_symlink() or getattr(held, "is_junction", lambda: False)():
                    raise ValueError("Invalid quarantined run")
                if state == "staging":
                    if held.exists():
                        if original.exists():
                            raise RuntimeError(f"Conflit de réconciliation pour {candidate}.")
                        held.rename(original)
                    elif not original.exists():
                        raise RuntimeError(f"Run absent pendant la réconciliation : {candidate}.")
                else:
                    if original.exists():
                        raise RuntimeError(f"Run recréé pendant la suppression : {candidate}.")
                    if held.exists():
                        try:
                            shutil.rmtree(held)
                        except OSError as error:
                            raise DeletionCleanupPending(tuple(journal["run_ids"]), str(error)) from error
                self.repository._progress_cache.pop(candidate, None)
            journal = json.loads(path.read_text(encoding="utf-8"))
            journal["state"] = "rolled_back" if state == "staging" else "complete"
            self._write_journal(path, journal)
