"""Framework-neutral progress and cooperative-cancellation hooks."""

from __future__ import annotations

from collections.abc import Callable, Mapping
from dataclasses import dataclass, field


@dataclass(frozen=True, slots=True)
class ProgressEvent:
    stage: str
    substage: str | None = None
    completed_units: int | None = None
    total_units: int | None = None
    details: Mapping[str, object] = field(default_factory=dict)


ProgressCallback = Callable[[ProgressEvent], None]
CancellationCheck = Callable[[], bool]


class CancellationRequested(RuntimeError):
    """Raised at a safe boundary when a caller requests cancellation."""


def report_progress(
    callback: ProgressCallback | None,
    stage: str,
    *,
    substage: str | None = None,
    completed_units: int | None = None,
    total_units: int | None = None,
    details: Mapping[str, object] | None = None,
) -> None:
    if callback is not None:
        callback(
            ProgressEvent(
                stage=stage,
                substage=substage,
                completed_units=completed_units,
                total_units=total_units,
                details=details or {},
            )
        )


def check_cancellation(callback: CancellationCheck | None) -> None:
    if callback is not None and callback():
        raise CancellationRequested("Cancellation requested")
