"""Coordinate permanent deletion using one validated global selection."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol

from .run_delete import DeleteEligibility, DeletePlan, DeletionCleanupPending


class DeleteService(Protocol):
    def delete_eligibility(self, run_id: str) -> DeleteEligibility: ...
    def delete_preview(self, run_id: str) -> DeletePlan: ...
    def delete_preview_many(self, run_ids: tuple[str, ...]) -> DeletePlan: ...
    def delete_many(self, plan: DeletePlan) -> DeletePlan: ...


_ERRORS = (OSError, ValueError, RuntimeError, KeyError, TypeError)


@dataclass(frozen=True, slots=True)
class BatchDeleteReview:
    requested_run_ids: tuple[str, ...]
    plans: tuple[DeletePlan, ...]
    affected_run_ids: tuple[str, ...]
    affected_by_type: tuple[tuple[str, int], ...]
    size_bytes: int
    skipped: tuple[tuple[str, str], ...]
    errors: tuple[tuple[str, str], ...]


@dataclass(frozen=True, slots=True)
class BatchDeleteOutcome:
    succeeded: tuple[str, ...]
    skipped: tuple[tuple[str, str], ...]
    errors: tuple[tuple[str, str], ...]
    deleted_run_ids: tuple[str, ...]


def preview_batch_delete(service: DeleteService, run_ids: list[str] | tuple[str, ...]) -> BatchDeleteReview:
    """Validate the entire selection; a blocker prevents any mutation."""
    requested = tuple(dict.fromkeys(str(run_id) for run_id in run_ids))
    try:
        plan = service.delete_preview_many(requested)
    except _ERRORS as error:
        return BatchDeleteReview(requested, (), (), (), 0, (), ((", ".join(requested), str(error)),))
    return BatchDeleteReview(requested, (plan,), plan.run_ids, plan.by_type,
                             plan.size_bytes, (), ())


def execute_batch_delete(service: DeleteService, review: BatchDeleteReview) -> BatchDeleteOutcome:
    if not review.plans or review.errors:
        return BatchDeleteOutcome((), review.skipped, review.errors, ())
    if len(review.plans) != 1:
        return BatchDeleteOutcome((), (), ((", ".join(review.requested_run_ids),
                                           "Ancien plan de suppression : prévisualisez à nouveau."),), ())
    try:
        result = service.delete_many(review.plans[0])
    except DeletionCleanupPending as error:
        return BatchDeleteOutcome(
            tuple(run_id for run_id in review.requested_run_ids if run_id in error.run_ids),
            review.skipped, ((", ".join(review.requested_run_ids), str(error)),), error.run_ids,
        )
    except _ERRORS as error:
        return BatchDeleteOutcome((), review.skipped,
                                  ((", ".join(review.requested_run_ids), str(error)),), ())
    return BatchDeleteOutcome(review.requested_run_ids, review.skipped, (), result.run_ids)
