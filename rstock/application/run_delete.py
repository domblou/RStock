"""Validated global deletion, with recoverable quarantine and publication guards."""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import uuid
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Any, Iterator

from .domain import JobStatus, JobType, RunMetadata
from .production_repository import ProductionRepository
from .repository import RunRepository
from .run_storage import RunStorageService
from .run_integrity import (
    graph_lock, journals, references, file_references, reference_free_run_document,
)


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
    size_bytes: int  # Legacy journal/API field; preparation no longer measures size.
    requested_run_ids: tuple[str, ...] = ()
    fingerprint: str = ""


def _references(value: Any, run_ids: set[str]) -> bool:
    return any(target in run_ids for _, target in references(value))


def _walk_error(error: OSError) -> None:
    raise error


def _reference_paths(directory: Path, *, scientific_run: bool = False) -> Iterator[Path]:
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
        relative = Path(base).relative_to(directory)
        for name in sorted(files):
            path = Path(base) / name
            if scientific_run and reference_free_run_document(relative / name):
                # These published schemas cannot carry references. Unknown
                # JSON/checkpoint files and CSV lineage columns remain checked.
                continue
            if path.suffix in {".json", ".csv"}:
                yield path


class _DeletionGraph(RunRepository):
    """One read-only operation snapshot, shared by ownership and reference scans.

    Nothing is cached between preparation and confirmation. Existing manifest
    validators and ownership traversal operate on this repository view unchanged.
    """

    def __init__(self, repository: RunRepository) -> None:
        super().__init__(repository.root)
        self.directories = tuple(sorted(
            path for path in self.root.iterdir() if path.is_dir() and path.name != ".deletions"
        )) if self.root.exists() else ()
        self._ids = tuple(path.name for path in self.directories if (path / "status.json").is_file())
        self.documents: dict[Path, Any] = {}
        self.edges: dict[Path, tuple[tuple[str, str], ...]] = {}
        self._metadata: dict[str, RunMetadata] = {}
        self._children: dict[str, list[str]] | None = None

    def document(self, path: Path) -> Any:
        if path not in self.documents:
            self.documents[path] = json.loads(path.read_bytes().decode("utf-8"))
        return self.documents[path]

    def read_json(self, run_id: str, name: str) -> dict[str, Any]:
        return self.document(self.run_directory(run_id) / name)

    def run_metadata(self, run_id: str) -> RunMetadata:
        if run_id not in self._metadata:
            path = self.run_directory(run_id) / "metadata.json"
            self._metadata[run_id] = RunMetadata.from_dict(self.document(path)) if path.is_file() else RunMetadata()
        return self._metadata[run_id]

    def list_run_ids(self) -> list[str]:
        return list(self._ids)

    def list_children(self, parent_run_id: str) -> list[str]:
        if self._children is None:
            self._children = {}
            for run_id in self._ids:
                parent = self.run_metadata(run_id).parent_run_id
                if parent:
                    self._children.setdefault(parent, []).append(run_id)
        return self._children.get(parent_run_id, [])

    def file_edges(self, path: Path) -> tuple[tuple[str, str], ...]:
        if path not in self.edges:
            if path.suffix == ".json":
                self.edges[path] = tuple(references(self.document(path)))
            else:
                # Repeated rows do not define additional dependencies. Keep
                # only column/target pairs in the confirmation fingerprint.
                self.edges[path] = tuple(sorted({
                    (field.rsplit(" : ", 1)[-1], target) for field, target in file_references(path)
                }))
        return self.edges[path]

    def iter_file_edges(self, path: Path) -> Iterator[tuple[str, str]]:
        if path.suffix == ".json":
            yield from self.file_edges(path)
        else:
            # External CSVs are not fingerprinted; stop at the first blocker
            # instead of materializing a potentially very large table.
            yield from file_references(path)

    def fingerprint(self, run_ids: tuple[str, ...]) -> str:
        digest = hashlib.sha256(b"deletion-graph-v2")
        selected = set(run_ids)
        for path in sorted(self.edges):
            relative = path.relative_to(self.root) if path.is_relative_to(self.root) else None
            if relative is None or relative.parts[0] not in selected:
                continue
            name = Path(*relative.parts[1:])
            # Only ownership/admissibility documents need their complete state.
            # Scientific summaries/CSV tables contribute lineage, not metrics.
            full_state = path.suffix == ".json" and (
                name in {Path("config.json"), Path("metadata.json"), Path("status.json")}
                or name.parts[0] == "orchestration"
            )
            if not full_state and not self.edges[path]:
                continue
            value = self.documents[path] if full_state else self.edges[path]
            digest.update(json.dumps([relative.as_posix(), value], sort_keys=True, default=str,
                                     separators=(",", ":")).encode("utf-8"))
        return digest.hexdigest()


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

    def _external_dependency(self, run_ids: set[str], graph: _DeletionGraph) -> str | None:
        # The same files feed dependency checks and the confirmation fingerprint.
        for directory in graph.directories:
            other_id = directory.name
            self._directory(other_id)
            for path in _reference_paths(directory, scientific_run=True):
                try:
                    if other_id in run_ids:
                        graph.file_edges(path)
                        continue
                    for field, target in graph.iter_file_edges(path):
                        if target in run_ids:
                            return (f"Le run conservé {other_id} référence {target} "
                                    f"({path.relative_to(directory)} : {field}).")
                except (OSError, ValueError) as error:
                    raise ValueError(f"Dépendances illisibles pour {other_id}: {path.name}") from error

        class GraphProductionRepository(ProductionRepository):
            def _registry_models(self):
                if not self.registry_path.exists():
                    return []
                payload = graph.document(self.registry_path)
                if payload.get("schema_version") != 1:
                    raise ValueError("Unsupported production registry schema")
                if not isinstance(payload.get("models"), list):
                    raise ValueError("Invalid production registry models")
                return [dict(item) for item in payload["models"]]

        production = GraphProductionRepository(self.repository.root.parent)
        for model in production.models():
            if _references(model.to_dict(), run_ids):
                return f"Le modèle Production {model.model_id} référence ce périmètre."
        for path in _reference_paths(production.root):
            try:
                for field, target in graph.iter_file_edges(path):
                    if target in run_ids:
                        return f"Production référence {target} ({path.relative_to(production.root)} : {field})."
            except (OSError, ValueError) as error:
                raise ValueError(f"Dépendances Production illisibles : {path}") from error
        simulations = self.repository.root.parent / "simulations"
        for path in _reference_paths(simulations):
            try:
                for field, target in graph.iter_file_edges(path):
                    if target in run_ids:
                        return f"La simulation conservée {path.parent.name} référence {target} ({field})."
            except (OSError, ValueError) as error:
                raise ValueError(f"Dépendances de simulation illisibles : {path}") from error
        return None

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
            self._reconcile_staging_locked()
            return self._global_plan(run_ids)

    def _global_plan(self, requested: tuple[str, ...]) -> DeletePlan:
        requested = tuple(dict.fromkeys(requested))
        if not requested:
            raise ValueError("Sélection de suppression vide.")
        graph = _DeletionGraph(self.repository)
        storage = RunStorageService(graph)
        selected = set(requested)
        existing = set(graph.list_run_ids())
        for run_id in requested:
            self._directory(run_id)
            if run_id not in existing:
                raise ValueError("Le run n'existe pas ou son statut est absent.")
            if graph.status(run_id).get("status") not in _ROOT_STATUSES:
                raise ValueError("Seuls les runs completed, failed ou cancelled peuvent être sélectionnés.")

        def depth(run_id: str) -> int:
            chain: set[str] = set()
            result = 0
            while run_id:
                if run_id in chain:
                    raise ValueError(f"Cycle de propriété détecté pour {run_id}.")
                chain.add(run_id)
                run_id = graph.run_metadata(run_id).parent_run_id
                result += int(run_id in selected)
            return result

        # Expand selected ancestors first so selected children reuse that graph.
        ids: list[str] = []
        included: set[str] = set()
        for run_id in sorted(requested, key=depth):
            if run_id in included:
                continue
            kind = JobType(str(graph.status(run_id)["job_type"]))
            related, error = storage._related_runs(run_id, kind, for_delete=True)
            if error:
                raise ValueError(error)
            for candidate in (run_id, *related):
                if candidate not in included:
                    ids.append(candidate)
                    included.add(candidate)

        counts: dict[str, int] = {}
        types: list[str] = []
        for candidate in ids:
            directory = self._directory(candidate)
            if not (directory / "status.json").is_file():
                raise ValueError(f"Statut de l'enfant propriétaire {candidate} absent.")
            status = graph.status(candidate)
            if status.get("status") not in _TERMINAL_STATUSES:
                raise ValueError(f"L'enfant propriétaire {candidate} n'est pas terminal.")
            if self.storage._worker_is_active(candidate, status):
                raise ValueError(f"Un worker traite encore le run {candidate}.")
            kind = JobType(str(status["job_type"]))
            if (kind in {JobType.END_TO_END, JobType.FORCED_CANDIDATE_VALIDATION}
                    and status.get("status") == JobStatus.COMPLETED.value
                    and not (directory / "orchestration/pipeline.json").is_file()):
                raise ValueError(f"Manifest du pipeline completed {candidate} absent.")
            metadata = graph.run_metadata(candidate)
            if metadata.parent_run_id and metadata.parent_run_id not in included:
                raise ValueError(f"Le parent conservé {metadata.parent_run_id} possède encore {candidate}. Sélectionnez le parent.")
            if metadata.parent_run_id:
                parent = graph.run_metadata(metadata.parent_run_id)
                expected_root = parent.root_run_id or metadata.parent_run_id
                if metadata.root_run_id and metadata.root_run_id != expected_root:
                    raise ValueError(f"Racine de propriété incohérente pour {candidate}.")
            counts[kind.value] = counts.get(kind.value, 0) + 1
            types.append(kind.value)
        dependency = self._external_dependency(included, graph)
        if dependency:
            raise ValueError(dependency)
        return DeletePlan(requested[0], tuple(ids), tuple(types), tuple(sorted(counts.items())),
                          0, requested, graph.fingerprint(tuple(ids)))

    def delete(self, run_id: str, *, expected_run_ids: tuple[str, ...],
               expected_fingerprint: str) -> DeletePlan:
        expected = DeletePlan(run_id, expected_run_ids, (), (), 0, (run_id,), expected_fingerprint)
        return self.delete_many(expected)

    def delete_many(self, expected: DeletePlan) -> DeletePlan:
        from .runner import RunService

        with RunService(self.repository)._submission_lock():
            with graph_lock(self.repository.root):
                self._reconcile_staging_locked()
                plan = self._global_plan(expected.requested_run_ids or (expected.run_id,))
                if plan.run_ids != expected.run_ids or plan.fingerprint != expected.fingerprint:
                    raise ValueError("Le plan de suppression a changé depuis la confirmation. Prévisualisez à nouveau.")
                # Full filesystem safety is checked only at execution, without
                # reading payloads or measuring sizes during preparation.
                for candidate in plan.run_ids:
                    directory = self._directory(candidate)
                    for base, directories, files in os.walk(directory, followlinks=False, onerror=_walk_error):
                        for name in (*directories, *files):
                            entry = Path(base) / name
                            if entry.is_symlink() or getattr(entry, "is_junction", lambda: False)():
                                raise ValueError(f"Lien interdit dans le run {candidate}: {entry}")
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
                    self._recover_locked(operation=operation)
                    raise
            # Tombstones now protect the entire removed graph. Physical cleanup
            # does not need to hold the live-graph lock and delay other previews.
            self._recover_locked(operation=operation, only_committed=True)
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
                self._reconcile_staging_locked()
            # Submission lock serializes cleaners. The live graph remains
            # available during potentially long quarantined-file removal.
            self._recover_locked(only_committed=True)

    def _reconcile_staging_locked(self) -> None:
        """Restore interrupted moves; never clean a committed quarantine here."""
        self._recover_locked(cleanup_committed=False)

    def _recover_locked(self, *, cleanup_committed: bool = True,
                        operation: Path | None = None, only_committed: bool = False) -> None:
        for path, journal in journals(self.repository.root):
            if operation is not None and path.parent != operation:
                continue
            state = journal["state"]
            if only_committed and state != "committed":
                continue
            if state in {"complete", "rolled_back"}:
                continue
            quarantine = path.parent / "quarantine"
            if (quarantine.is_symlink() or getattr(quarantine, "is_junction", lambda: False)()
                    or quarantine.resolve().parent != path.parent.resolve()):
                raise ValueError("Invalid quarantine directory")
            if state == "committed" and not cleanup_committed:
                for candidate in journal["run_ids"]:
                    if self._directory(candidate).exists():
                        raise RuntimeError(f"Run recréé pendant la suppression : {candidate}.")
                continue
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
            with graph_lock(self.repository.root):
                journal = json.loads(path.read_text(encoding="utf-8"))
                if journal["state"] != state:
                    raise RuntimeError("L'état persistant de suppression a changé pendant la réconciliation.")
                journal["state"] = "rolled_back" if state == "staging" else "complete"
                self._write_journal(path, journal)
