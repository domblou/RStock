"""Pure display helpers for the production-model catalogue."""

from __future__ import annotations

from collections.abc import Iterable, Sequence
from typing import Any


MODEL_STATUS_ORDER = ("active", "candidate", "trained", "inactive", "retired")
DEFAULT_MODEL_STATUSES = frozenset(
    status for status in MODEL_STATUS_ORDER if status != "retired"
)
JOB_DOMAIN_TYPES = {
    "production": frozenset({
        "market_update",
        "daily_prediction",
        "daily_screening",
        "realized_validation",
        "operational_run",
    }),
    "experiment": frozenset({
        "walk_forward",
        "xgboost_calibration",
        "threshold_parameter_calibration",
        "threshold_calibration",
    }),
    "model": frozenset({"full_training", "production_training"}),
}
JOB_DOMAIN_TITLES = {
    "production": "Jobs actifs — Production",
    "experiment": "Jobs actifs — Expériences",
    "model": "Jobs actifs — Modèles",
}


def _status(model: Any) -> str:
    value = getattr(model, "status", "")
    return str(getattr(value, "value", value)).casefold()


def job_domain(job_type: object) -> str | None:
    """Classify a persisted job for display without changing execution."""

    value = str(getattr(job_type, "value", job_type))
    for domain, types in JOB_DOMAIN_TYPES.items():
        if value in types:
            return domain
    return None


def job_domain_title(domain: str) -> str | None:
    return JOB_DOMAIN_TITLES.get(domain)


def model_filter_options(models: Iterable[Any]) -> list[str]:
    """Return stable status options, including statuses present in old registries."""

    statuses = {_status(model) for model in models}
    ordered = [status for status in MODEL_STATUS_ORDER if status in statuses]
    return ordered + sorted(statuses.difference(MODEL_STATUS_ORDER))


def filter_models(
    models: Sequence[Any] | Iterable[Any],
    *,
    statuses: Iterable[str] | None = None,
    targets: Iterable[str] | None = None,
    predictor_query: str = "",
) -> list[Any]:
    """Filter only the display population; model records are never modified."""

    selected_statuses = {
        str(status).casefold()
        for status in (DEFAULT_MODEL_STATUSES if statuses is None else statuses)
    }
    selected_targets = {str(target) for target in (targets or ())}
    query = str(predictor_query).strip().casefold()
    visible: list[Any] = []
    for model in models:
        if _status(model) not in selected_statuses:
            continue
        if selected_targets and str(getattr(model, "target", "")) not in selected_targets:
            continue
        predictors = tuple(str(item) for item in getattr(model, "predictors", ()))
        if query and not any(query in predictor.casefold() for predictor in predictors):
            continue
        visible.append(model)
    return visible


def reconcile_selected_model_id(
    selected_id: str | None, visible_models: Sequence[Any]
) -> str | None:
    """Keep a visible selection, otherwise choose the first visible model."""

    visible_ids = [str(getattr(model, "model_id")) for model in visible_models]
    if selected_id is not None and str(selected_id) in visible_ids:
        return str(selected_id)
    return visible_ids[0] if visible_ids else None
