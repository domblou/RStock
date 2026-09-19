"""Safe, explicit purging of heavy artifacts from completed scientific runs."""

from __future__ import annotations

import json
import os
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Mapping

from .domain import JobStatus, JobType
from .processes import process_alive
from .repository import RunRepository, utc_now


STORAGE_SCHEMA_VERSION = 1
STORAGE_POLICY_VERSION = "heavy-artifacts-v1"
_COMMON_PURGE_DIRECTORIES = ("checkpoints", "_working")

# Only these produced artifacts are removable. Unknown files in results/ stay.
_PURGE_FILES: dict[JobType, tuple[str, ...]] = {
    JobType.WALK_FORWARD: (
        "results/predictions.csv",
        "results/windows.csv",
        "results/aggregate_by_window.csv",
        "results/aggregate_by_set.csv",
        "results/aggregate_global.csv",
        "results/risk_by_window.csv",
        "results/risk_by_set.csv",
        "results/risk_global.csv",
        "results/final_holdout_predictions.csv",
        "results/predictor_prefilter.csv",
        "results/predictor_prefilter.json",
    ),
    JobType.WALK_FORWARD_BATCH: (
        "results/predictions.csv",
        "results/windows.csv",
        "results/aggregate_by_window.csv",
        "results/aggregate_by_set.csv",
        "results/aggregate_global.csv",
        "results/risk_by_window.csv",
        "results/risk_by_set.csv",
        "results/risk_global.csv",
        "results/final_holdout_predictions.csv",
        "results/predictor_prefilter.csv",
        "results/predictor_prefilter.json",
    ),
    JobType.XGBOOST_CALIBRATION: (
        "results/development_metrics_by_window.csv",
        "results/holdout_predictions.csv",
        "results/tested_parameters.csv",
        "results/sampled_combinations.csv",
        "results/baseline_vs_calibrated_development.csv",
    ),
    JobType.THRESHOLD_PARAMETER_CALIBRATION: (
        "results/development_metrics_by_window.csv",
        "results/development_predictions.csv",
        "results/sampled_combinations.csv",
    ),
    JobType.THRESHOLD_CALIBRATION: (
        "results/development_probabilities.csv",
        "results/holdout_predictions.csv",
        "results/probability_distribution.csv",
        "results/threshold_grid.csv",
        "results/threshold_metrics_by_window.csv",
        "results/threshold_metrics_by_threshold.csv",
        "results/baseline_vs_calibrated_threshold.csv",
        "results/sampled_combinations.csv",
        "results/threshold_diagnostics_by_set.json",
    ),
}

_ESSENTIAL_FILES: dict[JobType, tuple[str, ...]] = {
    JobType.WALK_FORWARD: (
        "results/qualification.csv",
        "results/final_holdout.csv",
        "results/selection_results.csv",
        "results/run_configuration.json",
    ),
    JobType.XGBOOST_CALIBRATION: (
        "results/selected_configurations.json",
        "results/development_metrics_by_configuration.csv",
        "results/holdout_metrics.csv",
        "results/run_configuration.json",
        "results/sampling_manifest.json",
    ),
    JobType.THRESHOLD_PARAMETER_CALIBRATION: (
        "results/selected_threshold_calibration_configuration.json",
        "results/development_metrics_by_configuration.csv",
        "results/tested_threshold_parameter_configurations.csv",
        "results/run_configuration.json",
        "results/sampling_manifest.json",
    ),
    JobType.THRESHOLD_CALIBRATION: (
        "results/selected_thresholds.json",
        "results/selected_thresholds_by_set.json",
        "results/threshold_metrics_by_set.csv",
        "results/run_configuration.json",
        "results/sampling_manifest.json",
    ),
}

_SOURCE_FIELDS = (
    "source_experiment_run",
    "source_walk_forward_run",
    "source_xgboost_calibration_run",
    "source_threshold_parameter_calibration_run",
    "source_threshold_calibration_run",
    "source_end_to_end_run",
)


@dataclass(frozen=True, slots=True)
class PurgeEligibility:
    eligible: bool
    reason: str | None = None


@dataclass(frozen=True, slots=True)
class PurgeArtifact:
    run_id: str
    path: str
    size_bytes: int

    def to_dict(self) -> dict[str, object]:
        return {
            "run_id": self.run_id,
            "path": self.path,
            "size_bytes": self.size_bytes,
        }

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "PurgeArtifact":
        return cls(
            run_id=str(values["run_id"]),
            path=str(values["path"]),
            size_bytes=int(values.get("size_bytes", 0)),
        )


@dataclass(frozen=True, slots=True)
class PurgePlan:
    run_id: str
    artifacts: tuple[PurgeArtifact, ...]
    related_run_ids: tuple[str, ...]
    reclaimable_bytes: int


class RunStorageService:
    """Plan and execute idempotent purges without touching unknown artifacts."""

    def __init__(self, repository: RunRepository) -> None:
        self.repository = repository

    def state(self, run_id: str) -> dict[str, Any]:
        return self.repository.storage(run_id)

    def eligibility(self, run_id: str) -> PurgeEligibility:
        directory = self.repository.run_directory(run_id)
        if not directory.exists():
            return PurgeEligibility(False, "Le run n’existe pas.")
        storage = self.state(run_id)
        if storage["state"] == "purged":
            return PurgeEligibility(False, "Les données lourdes sont déjà purgées.")
        try:
            status = self.repository.status(run_id)
            job_type = JobType(str(status["job_type"]))
        except (FileNotFoundError, KeyError, ValueError):
            return PurgeEligibility(False, "Le type ou le statut du run est invalide.")
        if status.get("status") != JobStatus.COMPLETED.value:
            return PurgeEligibility(
                False, "Seuls les runs terminés peuvent être purgés."
            )
        if job_type not in {*_PURGE_FILES, JobType.END_TO_END}:
            return PurgeEligibility(False, "Ce type de run n’est pas purgeable.")
        if self._worker_is_active(run_id, status):
            return PurgeEligibility(False, "Un worker traite encore ce run.")

        related, error = self._related_runs(run_id, job_type)
        if error is not None:
            return PurgeEligibility(False, error)
        for related_id in related:
            try:
                related_status = self.repository.status(related_id)
            except FileNotFoundError:
                return PurgeEligibility(
                    False, f"Le run dépendant {related_id} n’est pas matérialisé."
                )
            if related_status.get("status") != JobStatus.COMPLETED.value:
                return PurgeEligibility(
                    False, f"Le run dépendant {related_id} n’est pas terminé."
                )
            if self._worker_is_active(related_id, related_status):
                return PurgeEligibility(
                    False, f"Un worker traite encore le run dépendant {related_id}."
                )

        root_id = self.repository.run_metadata(run_id).root_run_id
        if root_id and root_id != run_id:
            try:
                root_status = self.repository.status(root_id)
            except FileNotFoundError:
                return PurgeEligibility(False, "Le parent racine du run est absent.")
            if root_status.get("status") != JobStatus.COMPLETED.value:
                return PurgeEligibility(
                    False, "Le pipeline End-to-end parent n’est pas terminé."
                )

        dependency = self._unfinished_dependent(run_id, {*related})
        if dependency is not None:
            return PurgeEligibility(
                False, f"Le run {dependency} dépend encore de ces artefacts."
            )
        for candidate_id in (run_id, *related):
            missing = self._missing_essential_files(candidate_id)
            if missing:
                return PurgeEligibility(
                    False,
                    "Artefacts essentiels absents pour "
                    f"{candidate_id} : {', '.join(missing)}.",
                )
        return PurgeEligibility(True)

    def preview(self, run_id: str) -> PurgePlan:
        eligibility = self.eligibility(run_id)
        if not eligibility.eligible:
            raise ValueError(eligibility.reason or "Ce run ne peut pas être purgé.")
        storage = self.state(run_id)
        if storage["state"] == "purging":
            artifacts = tuple(
                PurgeArtifact.from_dict(item)
                for item in storage.get("deleted_artifacts", ())
                if isinstance(item, Mapping)
            )
            related = tuple(
                str(value) for value in storage.get("related_run_ids", ())
            )
            return PurgePlan(
                run_id=run_id,
                artifacts=artifacts,
                related_run_ids=related,
                reclaimable_bytes=sum(self._current_size(item) for item in artifacts),
            )

        job_type = JobType(str(self.repository.status(run_id)["job_type"]))
        related, error = self._related_runs(run_id, job_type)
        if error is not None:
            raise ValueError(error)
        artifacts: list[PurgeArtifact] = []
        for target_id in (run_id, *related):
            artifacts.extend(self._inventory_for_run(target_id))
        artifacts.sort(key=lambda item: (item.run_id, item.path))
        return PurgePlan(
            run_id=run_id,
            artifacts=tuple(artifacts),
            related_run_ids=tuple(related),
            reclaimable_bytes=sum(item.size_bytes for item in artifacts),
        )

    def purge(self, run_id: str) -> dict[str, Any]:
        current = self.state(run_id)
        if current["state"] == "purged":
            return current
        plan = self.preview(run_id)
        if current["state"] == "full":
            self.repository.write_storage(run_id, self._purging_manifest(plan))
        for related_id in plan.related_run_ids:
            if self.state(related_id)["state"] == "full":
                related_artifacts = tuple(
                    item for item in plan.artifacts if item.run_id == related_id
                )
                self.repository.write_storage(
                    related_id,
                    self._purging_manifest(
                        PurgePlan(
                            run_id=related_id,
                            artifacts=related_artifacts,
                            related_run_ids=(),
                            reclaimable_bytes=sum(
                                item.size_bytes for item in related_artifacts
                            ),
                        )
                    ),
                )

        for artifact in plan.artifacts:
            self._delete_artifact(artifact)
        self._remove_policy_directories({run_id, *plan.related_run_ids})

        # Re-read persistent state before final publication. This avoids
        # overwriting a manifest changed by another component from stale memory.
        persisted = self.state(run_id)
        if persisted["state"] == "purged":
            return persisted
        if persisted["state"] != "purging":
            raise RuntimeError("L’état de purge persistant a changé.")
        self._assert_essential_files({run_id, *plan.related_run_ids})
        purged_at = utc_now()
        reclaimed = sum(item.size_bytes for item in plan.artifacts)
        final = {
            **persisted,
            "state": "purged",
            "purged_at": purged_at,
            "reclaimed_bytes": reclaimed,
        }
        self.repository.write_storage(run_id, final)
        for related_id in plan.related_run_ids:
            related_state = self.state(related_id)
            if related_state["state"] == "purged":
                continue
            related_reclaimed = sum(
                item.size_bytes
                for item in plan.artifacts
                if item.run_id == related_id
            )
            self.repository.write_storage(
                related_id,
                {
                    **related_state,
                    "state": "purged",
                    "purged_at": purged_at,
                    "reclaimed_bytes": related_reclaimed,
                },
            )
        return final

    def _related_runs(
        self, run_id: str, job_type: JobType
    ) -> tuple[tuple[str, ...], str | None]:
        related: list[str] = []
        visited: set[str] = set()

        def visit_end_to_end(parent_id: str) -> str | None:
            if parent_id in visited:
                return "Cycle de dépendances End-to-end détecté."
            visited.add(parent_id)
            from .end_to_end import load_pipeline_manifest
            try:
                manifest = load_pipeline_manifest(self.repository, parent_id)
            except (OSError, ValueError) as error:
                return f"Manifest End-to-end invalide : {error}"
            if manifest is None:
                return "Le manifest End-to-end est absent."
            for stage in manifest.get("stages", ()):
                if not isinstance(stage, Mapping):
                    continue
                child_id = stage.get("child_run_id")
                if child_id:
                    child_id = str(child_id)
                    related.append(child_id)
                    try:
                        child_type = JobType(
                            str(self.repository.status(child_id)["job_type"])
                        )
                    except (FileNotFoundError, KeyError, ValueError):
                        continue
                    if child_type is JobType.END_TO_END:
                        error = visit_end_to_end(child_id)
                        if error is not None:
                            return error
                    elif child_type is JobType.WALK_FORWARD:
                        related.extend(self._technical_children(child_id))
            return None

        if job_type is JobType.END_TO_END:
            error = visit_end_to_end(run_id)
            if error is not None:
                return (), error
        if job_type is JobType.WALK_FORWARD:
            related.extend(self._technical_children(run_id))
        for child_id in tuple(related):
            try:
                child_type = JobType(
                    str(self.repository.status(child_id)["job_type"])
                )
            except (FileNotFoundError, KeyError, ValueError):
                continue
            if child_type is JobType.WALK_FORWARD:
                related.extend(self._technical_children(child_id))
        return tuple(dict.fromkeys(related)), None

    def _technical_children(self, run_id: str) -> list[str]:
        children: list[str] = []
        for child_id in self.repository.list_children(run_id):
            metadata = self.repository.run_metadata(child_id)
            if metadata.relation_type == "walk_forward_batch":
                children.append(child_id)
        return children

    def _unfinished_dependent(
        self, run_id: str, included_related: set[str]
    ) -> str | None:
        candidate_ids = {run_id, *included_related}
        for other_id in self.repository.list_run_ids():
            if other_id in candidate_ids:
                continue
            try:
                status = self.repository.status(other_id)
            except (FileNotFoundError, ValueError):
                continue
            if status.get("status") == JobStatus.COMPLETED.value:
                continue
            metadata = self.repository.run_metadata(other_id)
            if (
                metadata.parent_run_id in candidate_ids
                or metadata.root_run_id in candidate_ids
            ):
                return other_id
            try:
                snapshot = self.repository.read_json(other_id, "config.json")
            except (FileNotFoundError, ValueError):
                continue
            if any(snapshot.get(field) in candidate_ids for field in _SOURCE_FIELDS):
                return other_id
        return None

    def _missing_essential_files(self, run_id: str) -> list[str]:
        status = self.repository.status(run_id)
        job_type = JobType(str(status["job_type"]))
        if job_type is JobType.WALK_FORWARD_BATCH:
            return []
        if job_type is JobType.END_TO_END:
            path = self.repository.run_directory(run_id) / "orchestration/pipeline.json"
            return [] if path.is_file() else ["orchestration/pipeline.json"]
        directory = self.repository.run_directory(run_id)
        return [
            relative
            for relative in _ESSENTIAL_FILES.get(job_type, ())
            if not (directory / relative).is_file()
        ]

    def _assert_essential_files(self, run_ids: Iterable[str]) -> None:
        for run_id in run_ids:
            missing = self._missing_essential_files(run_id)
            if missing:
                raise RuntimeError(
                    f"Artefacts essentiels supprimés pour {run_id}: "
                    f"{', '.join(missing)}"
                )

    def _inventory_for_run(self, run_id: str) -> list[PurgeArtifact]:
        try:
            job_type = JobType(str(self.repository.status(run_id)["job_type"]))
        except (FileNotFoundError, KeyError, ValueError):
            return []
        directory = self.repository.run_directory(run_id)
        protected = self._manifest_protected_files(run_id)
        artifacts: list[PurgeArtifact] = []
        for relative in _PURGE_FILES.get(job_type, ()):
            if relative in protected:
                continue
            path = directory / relative
            if path.is_file() or path.is_symlink():
                artifacts.append(
                    PurgeArtifact(run_id, relative, self._safe_size(path))
                )
        for relative in _COMMON_PURGE_DIRECTORIES:
            root = directory / relative
            if root.is_symlink():
                artifacts.append(
                    PurgeArtifact(run_id, relative, self._safe_size(root))
                )
                continue
            if not root.is_dir():
                continue
            for path in root.rglob("*"):
                if path.is_file() or path.is_symlink():
                    artifacts.append(
                        PurgeArtifact(
                            run_id,
                            str(path.relative_to(directory)).replace(os.sep, "/"),
                            self._safe_size(path),
                        )
                    )
        return artifacts

    def _manifest_protected_files(self, run_id: str) -> set[str]:
        metadata = self.repository.run_metadata(run_id)
        owner_id = metadata.parent_run_id
        if not owner_id:
            return set()
        manifest_path = (
            self.repository.run_directory(owner_id) / "orchestration" / "pipeline.json"
        )
        if not manifest_path.is_file():
            return set()
        try:
            manifest = self.repository.read_json(
                owner_id, "orchestration/pipeline.json"
            )
        except (OSError, ValueError):
            return set()
        for stage in manifest.get("stages", ()):
            if (
                isinstance(stage, Mapping)
                and str(stage.get("child_run_id") or "") == run_id
                and isinstance(stage.get("artifact_digests"), Mapping)
            ):
                return {
                    str(relative).replace(chr(92), "/")
                    for relative in stage["artifact_digests"]
                }
        return set()

    @staticmethod
    def _safe_size(path: Path) -> int:
        try:
            return int(path.lstat().st_size)
        except OSError:
            return 0

    def _current_size(self, artifact: PurgeArtifact) -> int:
        path = self._artifact_path(artifact)
        return self._safe_size(path) if path.exists() or path.is_symlink() else 0

    def _artifact_path(self, artifact: PurgeArtifact) -> Path:
        if not artifact.path or Path(artifact.path).is_absolute():
            raise ValueError("Chemin d’artefact de purge invalide.")
        parts = Path(artifact.path).parts
        if ".." in parts:
            raise ValueError("Chemin d’artefact de purge hors du run.")
        return self.repository.run_directory(artifact.run_id).joinpath(*parts)

    def _delete_artifact(self, artifact: PurgeArtifact) -> None:
        path = self._artifact_path(artifact)
        if path.is_symlink() or path.is_file():
            path.unlink(missing_ok=True)

    def _remove_policy_directories(self, run_ids: set[str]) -> None:
        for run_id in run_ids:
            directory = self.repository.run_directory(run_id)
            for relative in _COMMON_PURGE_DIRECTORIES:
                path = directory / relative
                if path.is_symlink():
                    path.unlink(missing_ok=True)
                elif path.exists():
                    try:
                        shutil.rmtree(path)
                    except FileNotFoundError:
                        # A concurrent idempotent purge may have removed it
                        # between the existence check and the deletion.
                        pass

    def _worker_is_active(
        self, run_id: str, status: Mapping[str, object]
    ) -> bool:
        for key in ("pid", "launcher_pid"):
            if process_alive(status.get(key)):
                return True
        owner = self.repository.run_directory(run_id) / ".worker.lock" / "owner.json"
        if not owner.is_file():
            return False
        try:
            values = json.loads(owner.read_text(encoding="utf-8"))
            return values.get("run_id") == run_id and process_alive(values.get("pid"))
        except (OSError, ValueError):
            return False

    @staticmethod
    def _purging_manifest(plan: PurgePlan) -> dict[str, object]:
        return {
            "schema_version": STORAGE_SCHEMA_VERSION,
            "state": "purging",
            "policy_version": STORAGE_POLICY_VERSION,
            "purged_at": None,
            "reclaimed_bytes": 0,
            "deleted_artifacts": [item.to_dict() for item in plan.artifacts],
            "related_run_ids": list(plan.related_run_ids),
        }
