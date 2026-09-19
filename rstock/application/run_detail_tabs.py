"""Pure tab registry and lightweight view models for run details."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Mapping, Sequence

from .domain import JobType


@dataclass(frozen=True, slots=True)
class RunTabDefinition:
    """One stable run-detail tab shared by standalone and pipeline views."""

    key: str
    label: str
    job_types: frozenset[JobType]
    renderer_key: str
    minimum_data: str


_SCIENTIFIC_RESULT_TYPES = frozenset(
    {
        JobType.XGBOOST_CALIBRATION,
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.THRESHOLD_CALIBRATION,
    }
)

TAB_REGISTRY: tuple[RunTabDefinition, ...] = (
    RunTabDefinition(
        "summary",
        "Résumé",
        frozenset({JobType.WALK_FORWARD, JobType.END_TO_END}),
        "summary",
        "summary or pipeline manifest",
    ),
    RunTabDefinition(
        "analysis", "Analyse", frozenset({JobType.WALK_FORWARD}), "analysis", "WF artifacts"
    ),
    RunTabDefinition(
        "combinations",
        "Combinaisons",
        frozenset({JobType.WALK_FORWARD}),
        "combinations",
        "WF qualification artifacts",
    ),
    RunTabDefinition(
        "validation",
        "Validation",
        frozenset({JobType.WALK_FORWARD}),
        "validation",
        "selected WF combination",
    ),
    RunTabDefinition(
        "walk_forward_batches",
        "Batchs WF",
        frozenset({JobType.WALK_FORWARD}),
        "walk_forward_batches",
        "WF batch manifest and child statuses",
    ),
    RunTabDefinition(
        "results", "Résultats", _SCIENTIFIC_RESULT_TYPES, "results", "job artifacts"
    ),
    RunTabDefinition(
        "configuration",
        "Configuration",
        _SCIENTIFIC_RESULT_TYPES,
        "configuration",
        "run snapshot",
    ),
    RunTabDefinition(
        "files", "Fichiers", _SCIENTIFIC_RESULT_TYPES, "files", "result file names"
    ),
    RunTabDefinition(
        "logs", "Logs", _SCIENTIFIC_RESULT_TYPES, "logs", "current run log tail"
    ),
    RunTabDefinition(
        "walk_forward",
        "Walk-forward",
        frozenset({JobType.END_TO_END}),
        "child_walk_forward",
        "walk_forward child id",
    ),
    RunTabDefinition(
        "xgboost",
        "XGBoost",
        frozenset({JobType.END_TO_END}),
        "child_xgboost",
        "xgboost child id",
    ),
    RunTabDefinition(
        "threshold_parameters",
        "Paramètres seuils",
        frozenset({JobType.END_TO_END}),
        "child_threshold_parameters",
        "threshold-parameter child id",
    ),
    RunTabDefinition(
        "thresholds",
        "Seuils",
        frozenset({JobType.END_TO_END}),
        "child_thresholds",
        "threshold child id",
    ),
    RunTabDefinition(
        "temporal_validation",
        "Validation temporelle",
        frozenset({JobType.END_TO_END}),
        "temporal_validation",
        "temporal validation child",
    ),
    RunTabDefinition(
        "promotion",
        "Promotion",
        frozenset({JobType.END_TO_END}),
        "promotion",
        "promotion checkpoint",
    ),
    RunTabDefinition(
        "technical",
        "Technique",
        frozenset({JobType.WALK_FORWARD, JobType.END_TO_END}),
        "technical",
        "manifests and run metadata",
    ),
)

_ORDER: dict[JobType, tuple[str, ...]] = {
    JobType.WALK_FORWARD: (
        "summary",
        "analysis",
        "combinations",
        "validation",
        "walk_forward_batches",
        "technical",
    ),
    JobType.XGBOOST_CALIBRATION: ("results", "configuration", "files", "logs"),
    JobType.THRESHOLD_PARAMETER_CALIBRATION: (
        "results",
        "configuration",
        "files",
        "logs",
    ),
    JobType.THRESHOLD_CALIBRATION: ("results", "configuration", "files", "logs"),
    JobType.END_TO_END: (
        "summary",
        "walk_forward",
        "xgboost",
        "threshold_parameters",
        "thresholds",
        "temporal_validation",
        "promotion",
        "technical",
    ),
}


def tabs_for_job(
    job_type: JobType, *, has_walk_forward_batches: bool = False
) -> tuple[RunTabDefinition, ...]:
    """Return applicable tabs in their stable visual order."""

    keys = _ORDER.get(job_type, ("results", "configuration", "files", "logs"))
    definitions = {item.key: item for item in TAB_REGISTRY}
    return tuple(
        definitions[key]
        for key in keys
        if key != "walk_forward_batches" or has_walk_forward_batches
    )


def render_lazy_tabs(
    streamlit_api: Any,
    tabs: Sequence[RunTabDefinition],
    renderers: Mapping[str, Callable[[], None]],
    *,
    key: str,
) -> str | None:
    """Render only the active Streamlit tab and return its stable key."""

    containers = streamlit_api.tabs(
        [item.label for item in tabs], key=key, on_change="rerun"
    )
    for definition, container in zip(tabs, containers, strict=True):
        if container.open:
            with container:
                renderers[definition.renderer_key]()
            return definition.key
    return None


PIPELINE_STAGE_LABELS = {
    "walk_forward": "Walk-forward",
    "xgboost_calibration": "Calibration XGBoost",
    "threshold_parameter_calibration": "Paramètres seuils",
    "threshold_calibration": "Seuils",
    "temporal_validation_end_to_end": "Validation temporelle",
    "promotion": "Promotion",
}

PIPELINE_STAGE_LABEL_COLUMN = "Étape"

PIPELINE_CHILD_TABS = {
    "child_walk_forward": "walk_forward",
    "child_xgboost": "xgboost_calibration",
    "child_threshold_parameters": "threshold_parameter_calibration",
    "child_thresholds": "threshold_calibration",
}


def pipeline_stage_by_key(
    stages: object, stage_key: str
) -> Mapping[str, object] | None:
    if not isinstance(stages, Sequence) or isinstance(stages, (str, bytes)):
        return None
    return next(
        (
            item
            for item in stages
            if isinstance(item, Mapping) and item.get("stage_key") == stage_key
        ),
        None,
    )


def pipeline_stage_rows(stages: object) -> list[dict[str, object]]:
    if not isinstance(stages, Sequence) or isinstance(stages, (str, bytes)):
        return []
    rows: list[dict[str, object]] = []
    for item in stages:
        if not isinstance(item, Mapping):
            continue
        stage_key = str(item.get("stage_key", ""))
        status = str(item.get("status") or "pending")
        rows.append(
            {
                PIPELINE_STAGE_LABEL_COLUMN: PIPELINE_STAGE_LABELS.get(
                    stage_key, stage_key
                ),
                "Statut": "disabled" if status == "not_requested" else status,
                "Progression": item.get("progress"),
                "Durée (s)": item.get("duration_seconds"),
                "Run ID enfant": item.get("child_run_id"),
                "Erreur": item.get("error"),
            }
        )
    return rows


def walk_forward_batch_rows(batches: object) -> list[dict[str, object]]:
    if not isinstance(batches, Sequence) or isinstance(batches, (str, bytes)):
        return []
    rows: list[dict[str, object]] = []
    for item in batches:
        if not isinstance(item, Mapping):
            continue
        rows.append(
            {
                "Batch": item.get("batch_id", item.get("batch_index")),
                "Range start": item.get("range_start"),
                "Range stop": item.get("range_stop"),
                "Combinaisons": item.get("combination_count"),
                "Run ID": item.get("child_run_id"),
                "Statut": item.get("status"),
                "Progression": item.get("progress"),
                "Durée (s)": item.get("duration_seconds"),
                "Début": item.get("started_at"),
                "Fin": item.get("completed_at"),
                "Erreur": item.get("error"),
            }
        )
    return rows


def batch_status_counts(batches: object) -> dict[str, int]:
    rows = walk_forward_batch_rows(batches)
    counts = {
        status: sum(row["Statut"] == status for row in rows)
        for status in ("completed", "running", "failed", "pending")
    }
    counts["pending"] += sum(row["Statut"] == "reserved" for row in rows)
    return counts
