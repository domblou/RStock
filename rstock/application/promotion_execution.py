"""Operational promotion with durable per-identity checkpoints."""

from __future__ import annotations

from typing import Any

from rstock.progress import (
    CancellationCheck, CancellationRequested, ProgressCallback,
    check_cancellation, report_progress,
)

from .auto_promotion import PromotionCoordinator
from .production_repository import ProductionRepository
from .production_services import PromotionService


class PromotionExecution:
    """Apply a frozen plan with one durable checkpoint per identity."""

    def __init__(self, coordinator: PromotionCoordinator) -> None:
        self.coordinator = coordinator

    def execute(
        self,
        *,
        progress_callback: ProgressCallback | None,
        cancellation_check: CancellationCheck | None,
    ) -> dict[str, Any]:
        coordinator = self.coordinator
        state = coordinator.prepare()
        if state["status"] == "completed":
            if state["completed_count"] != state["candidate_count"]:
                raise ValueError("Checkpoint de promotion completed mais incomplet")
            return state

        state = coordinator._persist_stage("running")
        if state["status"] == "completed":
            return state
        service = PromotionService(
            coordinator.repository,
            ProductionRepository(
                coordinator.repository.load_spec(coordinator.root_run_id).config.project_root
            ),
        )
        for candidate in coordinator._load()["candidates"]:
            if candidate["status"] == "completed":
                continue
            set_name = str(candidate["set_name"])
            try:
                check_cancellation(cancellation_check)
                state = coordinator._persist_candidate(
                    set_name,
                    status="running",
                    model_id=candidate.get("model_id"),
                    created=candidate.get("created"),
                    error=None,
                )
                if next(item for item in state["candidates"] if item["set_name"] == set_name)["status"] == "completed":
                    continue
                model, created = service.promote(
                    coordinator.walk_forward_run_id,
                    set_name,
                    xgboost_calibration_run=coordinator.xgboost_calibration_run_id,
                    threshold_calibration_run=coordinator.threshold_calibration_run_id,
                    holdout_evaluation_run=(
                        None if coordinator.promotion_provenance.get("forced_candidate_validation_run_id")
                        else coordinator.holdout_evaluation_run_id
                    ),
                    selected_threshold_direction="Up",
                    promotion_provenance=coordinator.promotion_provenance,
                )
                state = coordinator._persist_candidate(
                    set_name,
                    status="completed",
                    model_id=model.model_id,
                    created=created,
                    error=None,
                )
                coordinator.repository.append_log(
                    coordinator.root_run_id,
                    f"Promotion {'created' if created else 'reused'}: "
                    f"{set_name} -> {model.model_id}",
                )
                report_progress(
                    progress_callback,
                    "promotion",
                    substage=set_name,
                    completed_units=state["completed_count"],
                    total_units=state["candidate_count"],
                    details={
                        "set_name": set_name,
                        "model_id": model.model_id,
                        "created": created,
                    },
                )
            except CancellationRequested:
                coordinator._persist_stage("interrupted", "Cancellation requested")
                raise
            except Exception as error:
                coordinator._persist_candidate(
                    set_name,
                    status="failed",
                    error=str(error),
                )
                coordinator._persist_stage("failed", str(error))
                coordinator.repository.append_log(
                    coordinator.root_run_id,
                    f"Promotion failed for {set_name}: {error}",
                )
                raise RuntimeError(
                    f"La promotion de {set_name} a échoué: {error}"
                ) from error
        return coordinator._persist_stage("completed")

