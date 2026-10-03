"""Coordinate multiple existing single-run purges without changing purge policy."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol


class PurgeService(Protocol):
    def purge_eligibility(self, run_id: str) -> object: ...
    def purge_preview(self, run_id: str) -> object: ...
    def purge_run_type(self, run_id: str) -> str: ...
    def purge(self, run_id: str) -> dict[str, object]: ...


_PURGE_ERRORS = (OSError, ValueError, RuntimeError, KeyError, TypeError)


@dataclass(frozen=True, slots=True)
class BatchPurgeReview:
    requested_run_ids: tuple[str, ...]
    eligible_run_ids: tuple[str, ...]
    affected_run_ids: tuple[str, ...]
    affected_by_type: tuple[tuple[str, int], ...]
    related_by_run_id: tuple[tuple[str, tuple[str, ...]], ...]
    skipped: tuple[tuple[str, str], ...]
    errors: tuple[tuple[str, str], ...]
    reclaimable_bytes: int


@dataclass(frozen=True, slots=True)
class BatchPurgeOutcome:
    succeeded: tuple[str, ...]
    skipped: tuple[tuple[str, str], ...]
    errors: tuple[tuple[str, str], ...]
    reclaimed_bytes: int


def preview_batch_purge(
    service: PurgeService, run_ids: tuple[str, ...] | list[str]
) -> BatchPurgeReview:
    """Check each selected run with the individual purge eligibility and plan."""

    requested = tuple(dict.fromkeys(str(run_id) for run_id in run_ids))
    eligible: list[str] = []
    skipped: list[tuple[str, str]] = []
    errors: list[tuple[str, str]] = []
    plans: dict[str, object] = {}
    artifacts: dict[tuple[str, str], int] = {}
    for run_id in requested:
        try:
            eligibility = service.purge_eligibility(run_id)
            if not eligibility.eligible:
                skipped.append((run_id, eligibility.reason or "Run non admissible à la purge."))
                continue
            plan = service.purge_preview(run_id)
        except _PURGE_ERRORS as error:
            errors.append((run_id, str(error)))
            continue
        eligible.append(run_id)
        plans[run_id] = plan
        for artifact in plan.artifacts:
            artifacts[(artifact.run_id, artifact.path)] = artifact.size_bytes

    # Parent pipeline purges already include their related child runs.
    related = {
        str(child_id)
        for plan in plans.values()
        for child_id in plan.related_run_ids
    }
    eligible.sort(key=lambda run_id: run_id in related)
    affected = tuple(dict.fromkeys(
        run_id
        for selected_id in eligible
        for run_id in (selected_id, *plans[selected_id].related_run_ids)
    ))
    counts: dict[str, int] = {}
    for affected_id in affected:
        try:
            job_type = service.purge_run_type(affected_id)
        except _PURGE_ERRORS as error:
            errors.append((affected_id, f"Type du run indisponible : {error}"))
            continue
        counts[job_type] = counts.get(job_type, 0) + 1
    return BatchPurgeReview(
        requested, tuple(eligible), affected, tuple(sorted(counts.items())),
        tuple((run_id, tuple(plans[run_id].related_run_ids)) for run_id in eligible),
        tuple(skipped), tuple(errors), sum(artifacts.values()),
    )


def execute_batch_purge(
    service: PurgeService, review: BatchPurgeReview
) -> BatchPurgeOutcome:
    """Recheck each run and invoke the same purge used by the single-run UI."""

    succeeded: list[str] = []
    skipped = list(review.skipped)
    errors = list(review.errors)
    reclaimed_bytes = 0
    covered_by_success: set[str] = set()
    related_by_run_id = dict(review.related_by_run_id)
    for run_id in review.eligible_run_ids:
        if run_id in covered_by_success:
            succeeded.append(run_id)
            continue
        try:
            eligibility = service.purge_eligibility(run_id)
            if not eligibility.eligible:
                skipped.append((run_id, eligibility.reason or "Run non admissible à la purge."))
                continue
            storage = service.purge(run_id)
            if storage.get("state") != "purged":
                raise RuntimeError("La purge du run ne s’est pas terminée.")
        except _PURGE_ERRORS as error:
            errors.append((run_id, str(error)))
            continue
        succeeded.append(run_id)
        covered_by_success.update(related_by_run_id.get(run_id, ()))
        reclaimed_bytes += int(storage.get("reclaimed_bytes", 0))
    return BatchPurgeOutcome(
        tuple(succeeded), tuple(skipped), tuple(errors), reclaimed_bytes
    )
