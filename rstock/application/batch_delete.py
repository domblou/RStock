"""Coordinate permanent deletion using the individual deletion contract."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol

from .run_delete import DeleteEligibility, DeletePlan


class DeleteService(Protocol):
    def delete_eligibility(self, run_id: str) -> DeleteEligibility: ...
    def delete_preview(self, run_id: str) -> DeletePlan: ...
    def delete_run(self, run_id: str, *, expected_run_ids: tuple[str, ...]) -> DeletePlan: ...


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
    requested = tuple(dict.fromkeys(str(run_id) for run_id in run_ids))
    plans: list[DeletePlan] = []
    skipped: list[tuple[str, str]] = []
    errors: list[tuple[str, str]] = []
    for run_id in requested:
        try:
            eligibility = service.delete_eligibility(run_id)
            if not eligibility.eligible:
                skipped.append((run_id, eligibility.reason or "Run non admissible."))
                continue
            plans.append(service.delete_preview(run_id))
        except _ERRORS as error:
            errors.append((run_id, str(error)))
    covered = {child for plan in plans for child in plan.run_ids[1:]}
    plans.sort(key=lambda plan: plan.run_id in covered)
    affected = tuple(dict.fromkeys(
        candidate for plan in plans for candidate in plan.run_ids
    ))
    skipped = [(run_id, reason) for run_id, reason in skipped if run_id not in affected]
    counts: dict[str, int] = {}
    type_by_id = {
        candidate: kind
        for plan in plans
        for candidate, kind in zip(plan.run_ids, plan.run_types)
    }
    for candidate in affected:
        kind = type_by_id[candidate]
        counts[kind] = counts.get(kind, 0) + 1
    size_by_id: dict[str, int] = {}
    for plan in plans:
        # Overlap is only possible when a selected descendant is also owned by
        # another selected run. In that case the parent's total is authoritative.
        if plan.run_id not in covered:
            size_by_id[plan.run_id] = plan.size_bytes
    return BatchDeleteReview(
        requested, tuple(plans), affected, tuple(sorted(counts.items())),
        sum(size_by_id.values()), tuple(skipped), tuple(errors),
    )


def execute_batch_delete(service: DeleteService, review: BatchDeleteReview) -> BatchDeleteOutcome:
    succeeded: list[str] = []
    skipped = list(review.skipped)
    errors = list(review.errors)
    deleted: set[str] = set()
    for plan in review.plans:
        if plan.run_id in deleted:
            succeeded.append(plan.run_id)
            continue
        try:
            eligibility = service.delete_eligibility(plan.run_id)
            if not eligibility.eligible:
                skipped.append((plan.run_id, eligibility.reason or "Run non admissible."))
                continue
            result = service.delete_run(plan.run_id, expected_run_ids=plan.run_ids)
        except _ERRORS as error:
            errors.append((plan.run_id, str(error)))
            continue
        succeeded.append(plan.run_id)
        deleted.update(result.run_ids)
    for requested_id in review.requested_run_ids:
        if requested_id in deleted and requested_id not in succeeded:
            succeeded.append(requested_id)
    return BatchDeleteOutcome(
        tuple(succeeded), tuple(skipped), tuple(errors), tuple(sorted(deleted)),
    )
