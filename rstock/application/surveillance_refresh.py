"""Polling decisions for the active Surveillance page."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Mapping, Sequence


OPERATIONAL_JOB_TYPES = frozenset({
    "production_training",
    "market_update",
    "daily_prediction",
    "daily_screening",
    "realized_validation",
    "operational_run",
})
ACTIVE_JOB_STATUSES = frozenset({"pending", "running"})


@dataclass(frozen=True, slots=True)
class SurveillanceRefreshDecision:
    poll: bool
    final_rerun: bool


def surveillance_refresh_decision(
    runs: Sequence[Mapping[str, object]], *, polling: bool
) -> SurveillanceRefreshDecision:
    """Decide whether the active page should keep polling or finalize itself."""

    active = any(
        str(run.get("job_type")) in OPERATIONAL_JOB_TYPES
        and str(run.get("status")) in ACTIVE_JOB_STATUSES
        for run in runs
    )
    return SurveillanceRefreshDecision(
        poll=active,
        final_rerun=polling and not active,
    )
