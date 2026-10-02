"""Persistent, idempotent automatic promotion for End-to-end runs."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

import pandas as pd

from rstock.progress import (
    CancellationCheck,
    ProgressCallback,
)

from .history_analysis import (
    promotion_policy,
    threshold_calibration_table,
    threshold_promotion_guidance,
)
from .repository import RunRepository


PROMOTION_SCHEMA_VERSION = 1
PROMOTION_POLICY_VERSION = 1
PROMOTION_CHECKPOINT = "orchestration/promotion.json"


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _optional_digest(path: Path) -> str | None:
    return _sha256(path) if path.is_file() else None


def _read_json(path: Path) -> dict[str, Any]:
    try:
        values = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"Artefact de promotion illisible: {path.name}") from error
    if not isinstance(values, dict):
        raise ValueError(f"Artefact de promotion invalide: {path.name}")
    return values


def _read_csv(path: Path) -> pd.DataFrame:
    if not path.is_file():
        return pd.DataFrame()
    try:
        return pd.read_csv(path)
    except (OSError, pd.errors.ParserError, pd.errors.EmptyDataError) as error:
        raise ValueError(f"Artefact de promotion illisible: {path.name}") from error


def _json_records(frame: pd.DataFrame) -> list[dict[str, Any]]:
    if frame.empty:
        return []
    return json.loads(frame.to_json(orient="records", force_ascii=False))


def _promotion_guidance(
    threshold_results: Path,
    selected_by_set: dict[str, Any],
    promotion_config: object | None = None,
    *,
    calibration_results: Path | None = None,
    require_holdout: bool = False,
) -> pd.DataFrame:
    metrics = _read_csv((calibration_results or threshold_results) / "threshold_metrics_by_set.csv")
    holdout = _read_csv(threshold_results / "holdout_metrics.csv")
    if require_holdout and holdout.empty:
        metrics = metrics.iloc[0:0].copy()
    visible = threshold_calibration_table(metrics, holdout, selected_by_set)
    if "Direction" in visible:
        visible = visible[visible["Direction"].astype(str) == "Up"].copy()
    else:
        visible = visible.iloc[0:0].copy()

    represented = (
        set(visible["Combinaison"].astype(str))
        if "Combinaison" in visible
        else set()
    )
    missing_rows: list[dict[str, Any]] = []
    for set_name in sorted(str(item) for item in selected_by_set):
        if set_name in represented:
            continue
        selection = selected_by_set.get(set_name, {})
        up = selection.get("Up", {}) if isinstance(selection, dict) else {}
        missing_rows.append(
            {
                "Combinaison": set_name,
                "Cible": set_name.split("<-", 1)[0],
                "Predictors": (
                    set_name.split("<-", 1)[1].replace("+", " + ")
                    if "<-" in set_name
                    else set_name
                ),
                "Direction": "Up",
                "Seuil calibré": (
                    up.get("threshold") if isinstance(up, dict) else None
                ),
                "Signaux holdout": None,
                "Précision holdout": None,
                "Success rate holdout": None,
                "Recall": None,
                "F1": None,
                "AUC holdout": None,
                "Rendement directionnel moyen": None,
                "Rendement médian": None,
                "MFE moyen": None,
                "MAE moyen": None,
                "Fréquence mouvement opposé": None,
            }
        )
    if missing_rows:
        visible = pd.concat([visible, pd.DataFrame(missing_rows)], ignore_index=True)
    guided = threshold_promotion_guidance(
        visible, selected_by_set, promotion_config=promotion_config
    )
    if guided.empty:
        return guided
    return guided.sort_values(
        ["Combinaison", "Direction"], kind="stable"
    ).reset_index(drop=True)


def _counts(state: dict[str, Any]) -> dict[str, int]:
    candidates = state["candidates"]
    completed = [item for item in candidates if item["status"] == "completed"]
    return {
        "candidate_count": len(candidates),
        "completed_count": len(completed),
        "created_count": sum(item.get("created") is True for item in completed),
        "reused_count": sum(item.get("created") is False for item in completed),
    }


class PromotionCoordinator:
    """Build and execute one immutable promotion plan with per-set checkpoints."""

    def __init__(
        self,
        repository: RunRepository,
        *,
        root_run_id: str,
        walk_forward_run_id: str,
        xgboost_calibration_run_id: str,
        threshold_calibration_run_id: str,
        holdout_evaluation_run_id: str | None = None,
        promotion_qualification_run_id: str | None = None,
        promotion_provenance: dict[str, object] | None = None,
    ) -> None:
        self.repository = repository
        self.root_run_id = root_run_id
        self.walk_forward_run_id = walk_forward_run_id
        self.xgboost_calibration_run_id = xgboost_calibration_run_id
        self.threshold_calibration_run_id = threshold_calibration_run_id
        self.holdout_evaluation_run_id = holdout_evaluation_run_id
        self.promotion_qualification_run_id = promotion_qualification_run_id
        self.promotion_provenance = dict(promotion_provenance or {})

    @property
    def checkpoint_path(self) -> Path:
        return self.repository.run_directory(self.root_run_id) / PROMOTION_CHECKPOINT

    def _source_values(self) -> tuple[dict[str, str | None], pd.DataFrame]:
        if self.promotion_qualification_run_id is not None:
            status = self.repository.status(self.promotion_qualification_run_id)
            if status.get("status") != "completed" or status.get("job_type") != "promotion_qualification":
                raise ValueError("Promotion qualification run is incomplete")
            qualification_path = (
                self.repository.run_directory(self.promotion_qualification_run_id)
                / "results" / "qualification.json"
            )
            qualification = _read_json(qualification_path)
            if qualification.get("protocol") == "forced_candidate_promotion_qualification_v1":
                if (
                    qualification.get("source_walk_forward_run") != self.walk_forward_run_id
                    or qualification.get("source_fixed_candidate_evaluation_run") != self.threshold_calibration_run_id
                    or qualification.get("source_holdout_evaluation_run") != self.holdout_evaluation_run_id
                ):
                    raise ValueError("Forced qualification lineage differs from promotion sources")
                fixed = self.repository.run_directory(self.threshold_calibration_run_id) / "results"
                walk = self.repository.run_directory(self.walk_forward_run_id) / "results"
                sources = {
                    "selected_thresholds_by_set.json": fixed / "selected_thresholds_by_set.json",
                    "holdout_metrics.csv": fixed / "holdout_metrics.csv",
                    "run_configuration.json": fixed / "run_configuration.json",
                    "walk_forward_qualification.csv": walk / "qualification.csv",
                }
                if qualification.get("source_artifact_digests") != {
                    name: _sha256(path) for name, path in sources.items()
                }:
                    raise ValueError("Forced qualification sources have changed")
                if qualification.get("policy_parameters") != promotion_policy(
                    self.repository.load_spec(self.root_run_id).config
                ):
                    raise ValueError("Forced qualification policy differs from reference")
                forced_id = self.promotion_provenance.get("forced_candidate_validation_run_id")
                if not forced_id or qualification.get("period_lock") != self.repository.load_spec(
                    str(forced_id)
                ).forced_period_lock:
                    raise ValueError("Forced qualification period lock differs")
                decisions = pd.DataFrame(qualification["decisions"])
                persisted_candidates = sorted(set(decisions.loc[
                    decisions["candidate"].astype(bool), "Combinaison"
                ].astype(str))) if not decisions.empty else []
                if qualification.get("candidate_sets") != persisted_candidates:
                    raise ValueError("Forced qualification candidate set differs")
                digests = dict(qualification["source_artifact_digests"])
                digests["qualification.json"] = _sha256(qualification_path)
                return digests, decisions
            if (
                qualification.get("source_threshold_calibration_run") != self.threshold_calibration_run_id
                or qualification.get("source_holdout_evaluation_run") != self.holdout_evaluation_run_id
            ):
                raise ValueError("Qualification lineage differs from promotion sources")
            calibration_results = (
                self.repository.run_directory(self.threshold_calibration_run_id) / "results"
            )
            holdout_results = (
                self.repository.run_directory(str(self.holdout_evaluation_run_id)) / "results"
            )
            sources = {
                "selected_thresholds_by_set.json": calibration_results / "selected_thresholds_by_set.json",
                "threshold_metrics_by_set.csv": calibration_results / "threshold_metrics_by_set.csv",
                "holdout_metrics.csv": holdout_results / "holdout_metrics.csv",
            }
            if (holdout_results / "holdout_predictions.csv").is_file():
                sources["holdout_predictions.csv"] = (
                    holdout_results / "holdout_predictions.csv"
                )
            if qualification.get("source_artifact_digests") != {
                name: _sha256(path) for name, path in sources.items()
            }:
                raise ValueError("Qualification source artifacts have changed")
            if qualification.get("policy_parameters") != promotion_policy(
                self.repository.load_spec(self.root_run_id).config
            ):
                raise ValueError("Qualification policy differs from parent snapshot")
            decisions = pd.DataFrame(qualification["decisions"])
            persisted_candidates = (
                sorted(set(decisions.loc[
                    decisions["Statut promotion"] == "Candidat", "Combinaison"
                ].astype(str)))
                if not decisions.empty else []
            )
            if qualification.get("candidate_sets") != persisted_candidates:
                raise ValueError("Qualification decisions and candidate set differ")
            source_digests = dict(qualification["source_artifact_digests"])
            source_digests["qualification.json"] = _sha256(qualification_path)
            return source_digests, decisions
        threshold_results = (
            self.repository.run_directory(self.threshold_calibration_run_id)
            / "results"
        )
        selected_path = threshold_results / "selected_thresholds_by_set.json"
        selected = _read_json(selected_path)
        source_digests = {
            "selected_thresholds_by_set.json": _optional_digest(selected_path),
            "holdout_metrics.csv": _optional_digest(
                threshold_results / "holdout_metrics.csv"
            ),
            "threshold_metrics_by_set.csv": _optional_digest(
                threshold_results / "threshold_metrics_by_set.csv"
            ),
        }
        config = self.repository.load_spec(self.root_run_id).config
        return source_digests, _promotion_guidance(
            threshold_results, selected, promotion_config=config
        )

    def prepare(self) -> dict[str, Any]:
        source_digests, guidance, candidate_sets = self.source_candidates()
        policy = promotion_policy(self.repository.load_spec(self.root_run_id).config)
        identity = {
            "schema_version": PROMOTION_SCHEMA_VERSION,
            "policy_version": PROMOTION_POLICY_VERSION,
            "policy_parameters": {
                "minimum_holdout_signals": policy["promotion_min_holdout_signals"],
                "minimum_holdout_auc": policy["promotion_min_holdout_auc"],
                "minimum_holdout_precision": policy["promotion_min_holdout_precision"],
                "minimum_directional_return_exclusive": policy[
                    "promotion_min_mean_directional_return"
                ],
                "maximum_opposite_movement_frequency": policy[
                    "promotion_max_opposite_movement_frequency"
                ],
                "required_direction": "Up",
            },
            "root_run_id": self.root_run_id,
            "source_walk_forward_run": self.walk_forward_run_id,
            "source_xgboost_calibration_run": self.xgboost_calibration_run_id,
            "source_threshold_calibration_run": self.threshold_calibration_run_id,
            "source_artifact_digests": source_digests,
            "candidate_sets": candidate_sets,
        }
        if self.promotion_qualification_run_id is not None:
            identity["source_holdout_evaluation_run"] = self.holdout_evaluation_run_id
            identity["source_promotion_qualification_run"] = self.promotion_qualification_run_id
        canonical = json.dumps(identity, sort_keys=True, separators=(",", ":"))
        plan_sha256 = hashlib.sha256(canonical.encode()).hexdigest()
        if self.checkpoint_path.exists():
            persisted = self.repository.read_json(
                self.root_run_id, PROMOTION_CHECKPOINT
            )
            if any(persisted.get(key) != value for key, value in identity.items()):
                raise ValueError(
                    "Les sources ou la population de promotion ont changé"
                )
            if persisted.get("plan_sha256") != plan_sha256:
                raise ValueError("Le plan de promotion ne correspond plus au checkpoint")
            return persisted

        state: dict[str, Any] = {
            **identity,
            "plan_sha256": plan_sha256,
            "status": "pending",
            "error": None,
            "diagnostics": _json_records(guidance),
            "candidates": [
                {
                    "set_name": set_name,
                    "direction": "Up",
                    "status": "pending",
                    "model_id": None,
                    "created": None,
                    "error": None,
                }
                for set_name in candidate_sets
            ],
        }
        state.update(_counts(state))
        self.repository.write_json(self.root_run_id, PROMOTION_CHECKPOINT, state)
        return state

    def source_candidates(self) -> tuple[dict[str, str | None], pd.DataFrame, list[str]]:
        """Return the canonical promotion population without persisting a plan."""

        source_digests, guidance = self._source_values()
        candidate_sets = sorted(
            set(
                guidance.loc[
                    guidance["Statut promotion"] == "Candidat", "Combinaison"
                ].astype(str)
            )
            if not guidance.empty
            else set()
        )
        return source_digests, guidance, candidate_sets

    def _load(self) -> dict[str, Any]:
        return self.repository.read_json(self.root_run_id, PROMOTION_CHECKPOINT)

    def _persist_candidate(
        self, set_name: str, *, status: str, **values: object
    ) -> dict[str, Any]:
        # Re-read the checkpoint before every write; a stale in-memory state is
        # never allowed to overwrite a completed promotion.
        state = self._load()
        matches = [
            item for item in state["candidates"] if item["set_name"] == set_name
        ]
        if len(matches) != 1:
            raise ValueError(f"Candidat de promotion introuvable: {set_name}")
        if matches[0]["status"] == "completed" and status != "completed":
            return state
        matches[0].update(status=status, **values)
        state.update(_counts(state))
        self.repository.write_json(self.root_run_id, PROMOTION_CHECKPOINT, state)
        return state

    def _persist_stage(self, status: str, error: str | None = None) -> dict[str, Any]:
        state = self._load()
        if state["status"] == "completed" and status != "completed":
            return state
        state.update(status=status, error=error, **_counts(state))
        self.repository.write_json(self.root_run_id, PROMOTION_CHECKPOINT, state)
        return state



class AutoPromotionRunner(PromotionCoordinator):
    """Compatibility facade for existing callers and historical checkpoints."""

    def execute(
        self, *, progress_callback: ProgressCallback | None,
        cancellation_check: CancellationCheck | None,
    ) -> dict[str, Any]:
        from .promotion_execution import PromotionExecution

        return PromotionExecution(self).execute(
            progress_callback=progress_callback, cancellation_check=cancellation_check,
        )
