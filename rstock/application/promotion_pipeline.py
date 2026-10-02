"""Promotion policy, frozen plan coordination, and operational pipeline entry point."""

from __future__ import annotations

from typing import Any, Callable

from rstock.progress import CancellationCheck, ProgressCallback, check_cancellation

from .auto_promotion import (
    PromotionCoordinator, PROMOTION_CHECKPOINT, _sha256,
)
from .promotion_execution import PromotionExecution
from .domain import ExperimentSpec, JobType
from .repository import RunRepository


PROMOTION_TRIGGER_CHECKPOINT = "orchestration/promotion_trigger.json"


def _stage(manifest: dict[str, Any], key: str) -> dict[str, Any]:
    matches = [item for item in manifest["stages"] if item["stage_key"] == key]
    if len(matches) != 1:
        raise ValueError(f"Promotion source stage is missing: {key}")
    return matches[0]


def _forced_source_manifest(repository: RunRepository, run_id: str) -> dict[str, Any]:
    from .end_to_end import load_pipeline_manifest
    from .forced_candidate_validation import load_forced_validation_manifest

    if repository.status(run_id)["job_type"] == JobType.END_TO_END.value:
        source = load_pipeline_manifest(repository, run_id)
    else:
        source = load_forced_validation_manifest(repository, run_id)
    if source is None:
        raise ValueError("Manifest de revalidation forcée absent")
    return source


class PromotionTriggerPolicy:
    """Persist the authorization and the individual qualification source."""

    @staticmethod
    def validate_request(spec: ExperimentSpec) -> None:
        if spec.derivation is not None and spec.auto_promote_candidates:
            raise ValueError("Derived automatic promotion is deferred")

    def __init__(self, repository: RunRepository, root_run_id: str) -> None:
        self.repository = repository
        self.root_run_id = root_run_id

    def decide(
        self, spec: ExperimentSpec, manifest: dict[str, Any],
        temporal_comparison: dict[str, Any] | None, forced_child_id: str | None,
    ) -> dict[str, Any]:
        self.validate_request(spec)
        source_manifest = manifest
        temporal_status = (
            temporal_comparison.get("final_status")
            if temporal_comparison is not None else None
        )
        authorized = bool(spec.auto_promote_candidates)
        reason = "authorized" if authorized else "disabled"
        if authorized and spec.temporal_validation_enabled and temporal_status != "passed":
            authorized = False
            reason = "temporal_validation_not_passed"
        if authorized and forced_child_id is not None:
            source_manifest = _forced_source_manifest(self.repository, forced_child_id)
        if authorized and forced_child_id is not None and not source_manifest["stages"]:
            authorized = False
            reason = "no_reference_candidates"

        qualification_id = None
        source_reason = "none"
        if forced_child_id is not None:
            if source_manifest.get("schema_version") == 2:
                source_reason = "forced_reference_candidates"
                if source_manifest["stages"]:
                    qualification_id = _stage(source_manifest, "promotion_qualification")["child_run_id"]
            else:
                source_reason = "legacy_forced_composite"
        elif manifest.get("schema_version") in {3, 4}:
            source_reason = "normal_pipeline"
            qualification_id = _stage(manifest, "promotion_qualification")["child_run_id"]
        else:
            source_reason = "legacy_composite"

        decision = {
            "schema_version": 1,
            "root_run_id": self.root_run_id,
            "requested": bool(spec.auto_promote_candidates),
            "authorized": authorized,
            "reason": reason,
            "temporal_validation_status": temporal_status,
            "forced_candidate_validation_run_id": forced_child_id,
            "promotion_qualification_run_id": qualification_id,
            "source_selection_reason": source_reason,
        }
        path = self.repository.run_directory(self.root_run_id) / PROMOTION_TRIGGER_CHECKPOINT
        if path.exists():
            persisted = self.repository.read_json(self.root_run_id, PROMOTION_TRIGGER_CHECKPOINT)
            if persisted != decision:
                raise ValueError("Promotion trigger decision changed on resume")
            return persisted
        path.parent.mkdir(parents=True, exist_ok=True)
        self.repository.write_json(self.root_run_id, PROMOTION_TRIGGER_CHECKPOINT, decision)
        return decision


class PromotionPipeline:
    """Invoke the promotion domain and reconcile its stage manifest."""

    def __init__(self, repository: RunRepository, root_run_id: str) -> None:
        self.repository = repository
        self.root_run_id = root_run_id

    def run(
        self, spec: ExperimentSpec, manifest: dict[str, Any],
        temporal_comparison: dict[str, Any] | None, forced_child_id: str | None,
        *, stage_count: int, progress_callback: ProgressCallback | None,
        cancellation_check: CancellationCheck | None,
        phase_callback: Callable[..., None],
    ) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any] | None]:
        from .end_to_end import (
            TEMPORAL_VALIDATION_STAGE, _persist_stage_values, load_pipeline_manifest,
        )
        decision = PromotionTriggerPolicy(self.repository, self.root_run_id).decide(
            spec, manifest, temporal_comparison, forced_child_id,
        )
        summary: dict[str, Any] = {"requested": decision["requested"], "executed": False}
        if not decision["authorized"]:
            if decision["requested"]:
                summary["reason"] = decision["reason"]
                if decision["reason"] == "temporal_validation_not_passed":
                    summary["temporal_validation_status"] = decision["temporal_validation_status"]
            return summary, manifest, None

        check_cancellation(cancellation_check)
        source = (_forced_source_manifest(self.repository, forced_child_id)
                  if forced_child_id is not None else manifest)
        if source is None:
            raise ValueError("Manifest de revalidation forcée absent")
        walk_id = str(_stage(source, "walk_forward")["child_run_id"])
        xgb_id = str(_stage(manifest, "xgboost_calibration")["child_run_id"])
        threshold_key = ("fixed_candidate_evaluation" if forced_child_id is not None
                         else "threshold_calibration")
        threshold_id = str(_stage(source, threshold_key)["child_run_id"])
        split_forced = forced_child_id is not None and source.get("schema_version") == 2
        split_normal = forced_child_id is None and manifest.get("schema_version") in {3, 4}
        holdout_id = (threshold_id if split_forced else
                      str(_stage(manifest, "holdout_evaluation")["child_run_id"])
                      if split_normal else None)
        provenance = (
            {
                "reference_run_id": self.root_run_id,
                "temporal_validation_run_id": str(_stage(manifest, TEMPORAL_VALIDATION_STAGE)["child_run_id"]),
                "forced_candidate_validation_run_id": forced_child_id,
                "promotion_policy_version": 1,
            }
            if forced_child_id is not None else None
        )
        coordinator = PromotionCoordinator(
            self.repository, root_run_id=self.root_run_id,
            walk_forward_run_id=walk_id, xgboost_calibration_run_id=xgb_id,
            threshold_calibration_run_id=threshold_id,
            holdout_evaluation_run_id=holdout_id,
            promotion_qualification_run_id=decision["promotion_qualification_run_id"],
            promotion_provenance=provenance,
        )
        state = coordinator.prepare()
        if decision["promotion_qualification_run_id"] is not None:
            _persist_stage_values(
                self.repository, self.root_run_id, "promotion",
                promotion_qualification_run_id=decision["promotion_qualification_run_id"],
            )
        manifest = load_pipeline_manifest(self.repository, self.root_run_id) or manifest
        promotion_stage = _stage(manifest, "promotion")
        expected = promotion_stage.get("expected_fingerprint")
        if expected is None:
            _persist_stage_values(
                self.repository, self.root_run_id, "promotion",
                expected_fingerprint=state["plan_sha256"],
            )
        elif expected != state["plan_sha256"]:
            raise ValueError("Fingerprint du plan de promotion incompatible")
        digests = promotion_stage.get("artifact_digests") or {}
        if digests and digests != {PROMOTION_CHECKPOINT: _sha256(coordinator.checkpoint_path)}:
            raise ValueError("Le checkpoint de promotion a changé")

        phase_callback(progress_callback, "promotion", "started", stage_index=stage_count,
                       stage_count=stage_count, candidates=state["candidate_count"])
        state = PromotionExecution(coordinator).execute(
            progress_callback=progress_callback, cancellation_check=cancellation_check,
        )
        actual = {PROMOTION_CHECKPOINT: _sha256(coordinator.checkpoint_path)}
        manifest = load_pipeline_manifest(self.repository, self.root_run_id) or manifest
        persisted = _stage(manifest, "promotion").get("artifact_digests") or {}
        if persisted and persisted != actual:
            raise ValueError("Le checkpoint de promotion a changé")
        if not persisted:
            _persist_stage_values(self.repository, self.root_run_id, "promotion",
                                  artifact_digests=actual)
        phase_callback(progress_callback, "promotion", "completed", stage_index=stage_count,
                       stage_count=stage_count, candidates=state["candidate_count"],
                       created=state["created_count"], reused=state["reused_count"])
        summary = {
            "requested": True, "executed": True,
            "plan_sha256": state["plan_sha256"],
            "candidate_count": state["candidate_count"],
            "created_count": state["created_count"],
            "reused_count": state["reused_count"],
            "models": [
                {"set_name": item["set_name"], "model_id": item["model_id"],
                 "created": item["created"]}
                for item in state["candidates"]
            ],
        }
        return summary, load_pipeline_manifest(self.repository, self.root_run_id) or manifest, state
