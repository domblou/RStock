"""Immutable run snapshots adapted into editable experiment duplication drafts."""

from __future__ import annotations

import logging
from copy import deepcopy
from dataclasses import asdict, fields
from math import comb
from pathlib import Path
from typing import Any, Mapping

from rstock.config import (
    DEFAULT_CONFIG,
    HISTORICAL_MISSING_CONFIG_DEFAULTS,
    RStockConfig,
)

from .domain import ExperimentSpec, JobType
from .universes import UniverseSelection


LOGGER = logging.getLogger(__name__)

DUPLICATION_JOB_TYPES = (
    JobType.WALK_FORWARD,
    JobType.XGBOOST_CALIBRATION,
    JobType.THRESHOLD_CALIBRATION,
)

JOB_TYPE_LABELS = {
    JobType.WALK_FORWARD: "Walk-forward",
    JobType.XGBOOST_CALIBRATION: "Calibration XGBoost",
    JobType.THRESHOLD_CALIBRATION: "Calibration des seuils",
}
JOB_TYPE_BY_LABEL = {label: job_type for job_type, label in JOB_TYPE_LABELS.items()}


def normalize_duplication_job_type(value: object) -> JobType | None:
    """Normalize enum, serialized and legacy job-type forms for the duplication UI."""

    if isinstance(value, JobType):
        return value
    if isinstance(value, str):
        raw = value.strip()
        normalized = raw.casefold().replace("-", "_").replace(" ", "_")
        for job_type, label in JOB_TYPE_LABELS.items():
            if normalized in {
                job_type.value.casefold(),
                job_type.name.casefold(),
                f"jobtype.{job_type.name}".casefold(),
                label.casefold().replace("-", "_").replace(" ", "_"),
            }:
                return job_type
    LOGGER.warning("Unknown duplication job type: %r", value)
    return None


def duplication_job_label(value: object) -> str:
    """Return a safe UI label, falling back to walk-forward for legacy drafts."""

    job_type = normalize_duplication_job_type(value)
    return JOB_TYPE_LABELS.get(job_type, JOB_TYPE_LABELS[JobType.WALK_FORWARD])


def _description_suffix(value: object) -> str | None:
    """Keep a stored description while allowing the copied job label to change."""

    if not isinstance(value, str) or not (description := value.strip()):
        return None
    for label in JOB_TYPE_LABELS.values():
        for separator in (" — ", " - "):
            prefix = f"{label}{separator}"
            if description.casefold().startswith(prefix.casefold()):
                return description[len(prefix):].strip() or None
    return description


def _source_run_description(
    configuration: Mapping[str, object], detail: Mapping[str, object]
) -> str | None:
    """Read a persisted source label without inventing one for legacy runs."""

    summary = detail.get("summary")
    sources = (configuration, summary if isinstance(summary, Mapping) else {})
    for source in sources:
        for field in (
            "run_description",
            "experiment_description",
            "description",
            "display_name",
            "name",
            "title",
        ):
            description = _description_suffix(source.get(field))
            if description:
                return description
    return None


def walk_forward_duplication_draft(
    run_id: str, detail: Mapping[str, object]
) -> dict[str, object]:
    """Extract the editable inputs of one historical walk-forward run."""

    configuration = detail.get("configuration", {})
    if not isinstance(configuration, Mapping):
        raise ValueError("Run configuration is unavailable")
    if normalize_duplication_job_type(configuration.get("job_type")) != JobType.WALK_FORWARD:
        raise ValueError("Only walk-forward runs can be duplicated")

    symbols = tuple(str(item) for item in configuration.get("symbols", ()) if item)
    return {
        "source_run_id": str(run_id),
        "job_type": JobType.WALK_FORWARD.value,
        "primary_universe_id": configuration.get("primary_universe_id"),
        "context_universe_ids": list(configuration.get("context_universe_ids", ())),
        "context_sample_size": configuration.get("context_sample_size"),
        "context_selection_method": configuration.get("context_selection_method"),
        "context_seed": configuration.get("context_seed"),
        "universe_selection": deepcopy(configuration.get("universe_selection") or {}),
        "target_symbols": list(configuration.get("target_symbols") or symbols),
        "context_symbols": list(configuration.get("context_symbols", ())),
        "predictor_symbols": list(configuration.get("predictor_symbols") or symbols),
        "calendar": str(configuration.get("calendar", "XNYS")),
        "combinations_per_target": int(configuration.get("combinations_per_target", 3)),
        "evaluate_final_holdout": bool(configuration.get("evaluate_final_holdout", True)),
        "rstock_config": deepcopy(configuration.get("rstock_config") or {}),
        "run_description": _source_run_description(configuration, detail),
    }


def config_from_historical_snapshot(
    snapshot: Mapping[str, object] | None, *, current_project_root: Path
) -> RStockConfig:
    """Restore a partial historical config with defaults and the current root."""

    source = snapshot or {}
    allowed = {field.name for field in fields(RStockConfig)} - {"project_root"}
    values: dict[str, Any] = asdict(DEFAULT_CONFIG)
    values.update(HISTORICAL_MISSING_CONFIG_DEFAULTS)
    values.update({name: deepcopy(value) for name, value in source.items() if name in allowed})
    values["project_root"] = Path(current_project_root)
    for name in ("selected_symbols", "threshold_calibration_quantiles"):
        if values.get(name) is not None:
            values[name] = tuple(values[name])
    return RStockConfig(**values)


def duplication_combination_count(
    draft: Mapping[str, object], config: RStockConfig
) -> int:
    """Count combinations from the frozen population without materialising them."""

    predictor_count = len(tuple(draft.get("predictor_symbols", ())))
    target_count = len(tuple(draft.get("target_symbols", ())))
    depth = config.permutation_depth
    if predictor_count < 2 or depth < 1 or depth >= predictor_count:
        raise ValueError("Invalid frozen population or permutation depth")
    return target_count * sum(
        comb(predictor_count - 1, predictor_depth)
        for predictor_depth in range(1, depth + 1)
    )


def duplication_submission_values(
    draft: Mapping[str, object], *, current_config: RStockConfig, use_run_config: bool
) -> dict[str, object]:
    """Choose config source while preserving the duplicated universe inputs."""

    if use_run_config:
        config = config_from_historical_snapshot(
            draft.get("rstock_config")
            if isinstance(draft.get("rstock_config"), Mapping)
            else None,
            current_project_root=current_config.project_root,
        )
    else:
        # Deliberately use the session configuration passed at submission time;
        # it must not be taken from the historical draft.
        config = current_config
    return {**deepcopy(dict(draft)), "config": config}


def _selected_duplication_job_type(job_type: JobType | str | None, draft: Mapping[str, object]) -> JobType:
    """Resolve the selected type and reject jobs outside the locked workflow set."""

    raw = draft.get("job_type") if job_type is None else job_type
    selected = normalize_duplication_job_type(raw)
    if selected is None:
        raise ValueError(f"Unknown duplication job type: {raw!r}")
    if selected not in DUPLICATION_JOB_TYPES:
        raise ValueError(
            "This duplication supports only walk-forward, XGBoost calibration, "
            "or threshold calibration"
        )
    return selected


def validate_duplication_job(
    draft: Mapping[str, object], job_type: JobType | str | None = None
) -> JobType:
    """Validate that a selected experimental job can use the frozen population."""

    selected = _selected_duplication_job_type(job_type, draft)
    targets = tuple(str(item) for item in draft.get("target_symbols", ()) if item)
    context = tuple(str(item) for item in draft.get("context_symbols", ()) if item)
    predictors = tuple(str(item) for item in draft.get("predictor_symbols", ()) if item)
    frozen_predictors = tuple(dict.fromkeys((*targets, *context)))
    if len(targets) < 1:
        raise ValueError("The source run has no frozen target symbols")
    if len(predictors) < 2:
        raise ValueError("The source run needs at least two frozen predictor symbols")
    if set(targets) & set(context):
        raise ValueError("The frozen context overlaps the frozen target symbols")
    if predictors != frozen_predictors:
        raise ValueError(
            "The frozen predictor symbols are inconsistent with targets and context"
        )
    if int(draft.get("combinations_per_target", 0)) < 1:
        raise ValueError("The source run has an invalid combinations-per-target value")
    return selected


def experiment_spec_from_duplication(
    draft: Mapping[str, object],
    *,
    current_config: RStockConfig,
    use_run_config: bool,
    job_type: JobType | str | None = None,
) -> ExperimentSpec:
    """Build a new immutable spec from the frozen run inputs only."""

    selected_job_type = validate_duplication_job(draft, job_type)
    values = duplication_submission_values(
        draft, current_config=current_config, use_run_config=use_run_config
    )
    return ExperimentSpec(
        job_type=selected_job_type,
        config=values["config"],
        symbols=tuple(str(item) for item in values["predictor_symbols"]),
        calendar=str(values["calendar"]),
        combinations_per_target=int(values["combinations_per_target"]),
        evaluate_final_holdout=bool(values["evaluate_final_holdout"]),
        universe_selection=UniverseSelection.from_dict(values["universe_selection"]),
        primary_universe_id=(
            None
            if values["primary_universe_id"] is None
            else str(values["primary_universe_id"])
        ),
        context_universe_ids=tuple(str(item) for item in values["context_universe_ids"]),
        context_sample_size=(
            None
            if values.get("context_sample_size") is None
            else int(values["context_sample_size"])
        ),
        context_selection_method=(
            None
            if values.get("context_selection_method") is None
            else str(values["context_selection_method"])
        ),
        context_seed=(
            None if values.get("context_seed") is None else int(values["context_seed"])
        ),
        target_symbols=tuple(str(item) for item in values["target_symbols"]),
        context_symbols=tuple(str(item) for item in values["context_symbols"]),
        predictor_symbols=tuple(str(item) for item in values["predictor_symbols"]),
        # A calibration created from this locked walk-forward snapshot can be
        # promoted without guessing or re-resolving its source population.
        source_walk_forward_run=(
            str(draft["source_run_id"])
            if selected_job_type in {
                JobType.XGBOOST_CALIBRATION, JobType.THRESHOLD_CALIBRATION
            }
            and draft.get("source_run_id")
            else None
        ),
        run_description=(
            None if values.get("run_description") is None
            else str(values["run_description"])
        ),
    )
