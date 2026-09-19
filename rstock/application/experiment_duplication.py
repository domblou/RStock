"""Immutable run snapshots adapted into editable experiment duplication drafts."""

from __future__ import annotations

import logging
import json
from copy import deepcopy
from dataclasses import asdict, fields, replace
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
    JobType.THRESHOLD_PARAMETER_CALIBRATION,
    JobType.THRESHOLD_CALIBRATION,
)

THRESHOLD_CALIBRATION_CONFIG_FIELDS = (
    "threshold_calibration_min_signals_per_window",
    "threshold_calibration_min_robust_signals",
    "threshold_calibration_min_window_fraction",
    "threshold_calibration_precision_tolerance",
    "threshold_calibration_quantiles",
    "threshold_calibration_grid_decimals",
)

JOB_TYPE_LABELS = {
    JobType.WALK_FORWARD: "Walk-forward",
    JobType.XGBOOST_CALIBRATION: "Calibration XGBoost",
    JobType.THRESHOLD_PARAMETER_CALIBRATION: (
        "Calibration des paramètres de seuils"
    ),
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


def _source_traceability(detail: Mapping[str, object]) -> Mapping[str, object]:
    """Return persisted run provenance, or an empty mapping for legacy runs."""

    summary = detail.get("summary")
    if not isinstance(summary, Mapping):
        return {}
    traceability = summary.get("traceability")
    return traceability if isinstance(traceability, Mapping) else {}


def walk_forward_duplication_draft(
    run_id: str, detail: Mapping[str, object], *, project_root: Path | None = None
) -> dict[str, object]:
    """Extract frozen inputs from a walk-forward or XGBoost calibration run."""

    configuration = detail.get("configuration", {})
    if not isinstance(configuration, Mapping):
        raise ValueError("Run configuration is unavailable")
    source_job_type = normalize_duplication_job_type(configuration.get("job_type"))
    if source_job_type not in DUPLICATION_JOB_TYPES:
        raise ValueError(
            "Only walk-forward, XGBoost calibration, threshold parameter "
            "calibration, and threshold calibration runs can be duplicated"
        )

    source_xgboost_run = configuration.get("source_xgboost_calibration_run")
    frozen_xgboost = deepcopy(configuration.get("frozen_xgboost_parameters"))
    source_threshold_parameter_run = configuration.get(
        "source_threshold_parameter_calibration_run"
    )
    frozen_threshold_parameters = deepcopy(
        configuration.get("frozen_threshold_calibration_parameters")
    )
    if source_job_type is JobType.XGBOOST_CALIBRATION:
        source_xgboost_run = str(run_id)
        root = project_root
        if root is None:
            rstock_config = configuration.get("rstock_config", {})
            if isinstance(rstock_config, Mapping) and rstock_config.get("project_root"):
                root = Path(str(rstock_config["project_root"]))
        selected_path = (
            None if root is None else Path(root) / "runs" / str(run_id) / "results"
            / "selected_configurations.json"
        )
        if selected_path is None or not selected_path.exists():
            raise ValueError("XGBoost calibration selections are unavailable")
        try:
            selected = json.loads(selected_path.read_text(encoding="utf-8"))
            frozen_xgboost = {
                direction: dict(selected[direction]["parameters"])
                for direction in ("Up", "Down")
            }
        except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError) as error:
            raise ValueError("XGBoost calibration selections are invalid") from error
    elif source_job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION:
        source_threshold_parameter_run = str(run_id)
        root = project_root
        if root is None:
            rstock_config = configuration.get("rstock_config", {})
            if isinstance(rstock_config, Mapping) and rstock_config.get("project_root"):
                root = Path(str(rstock_config["project_root"]))
        selected_path = (
            None
            if root is None
            else Path(root)
            / "runs"
            / str(run_id)
            / "results"
            / "selected_threshold_calibration_configuration.json"
        )
        if selected_path is None or not selected_path.exists():
            raise ValueError("Threshold parameter calibration selection is unavailable")
        try:
            selected = json.loads(selected_path.read_text(encoding="utf-8"))
            frozen_threshold_parameters = dict(selected["parameters"])
        except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError) as error:
            raise ValueError("Threshold parameter calibration selection is invalid") from error

    symbols = tuple(str(item) for item in configuration.get("symbols", ()) if item)
    traceability = _source_traceability(detail)
    return {
        "source_run_id": str(run_id),
        "job_type": source_job_type.value,
        "primary_universe_id": configuration.get("primary_universe_id"),
        "market_benchmark_symbol": configuration.get("market_benchmark_symbol"),
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
        "historical_data_cutoff": traceability.get("prepared_market_last_date"),
        "source_prepared_dataset_sha256": traceability.get(
            "prepared_dataset_sha256"
        ),
        "source_walk_forward_run": (
            str(run_id)
            if source_job_type is JobType.WALK_FORWARD
            else configuration.get("source_walk_forward_run")
        ),
        "source_xgboost_calibration_run": source_xgboost_run,
        "frozen_xgboost_parameters": frozen_xgboost,
        "source_threshold_parameter_calibration_run": source_threshold_parameter_run,
        "source_end_to_end_run": configuration.get("source_end_to_end_run"),
        "source_threshold_calibration_run": configuration.get(
            "source_threshold_calibration_run"
        ),
        "frozen_threshold_calibration_parameters": frozen_threshold_parameters,
        "auto_promote_candidates": bool(
            configuration.get("auto_promote_candidates", False)
        ),
        "pipeline_version": int(configuration.get("pipeline_version", 0)),
        "calibration_sampling_policy_version": int(
            configuration.get("calibration_sampling_policy_version", 1)
        ),
        "combination_plan_version": configuration.get("combination_plan_version"),
        "combination_plan_sha256": configuration.get("combination_plan_sha256"),
        "combination_range_start": configuration.get("combination_range_start"),
        "combination_range_stop": configuration.get("combination_range_stop"),
        "xgboost_resolution_version": int(
            configuration.get("xgboost_resolution_version", 0)
        ),
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


def resolve_stage_configuration(
    draft: Mapping[str, object],
    *,
    current_config: RStockConfig,
    use_run_config: bool,
    target_job_type: JobType,
) -> RStockConfig:
    """Resolve only the settings owned by the stage about to be executed."""

    historical = config_from_historical_snapshot(
        draft.get("rstock_config")
        if isinstance(draft.get("rstock_config"), Mapping)
        else None,
        current_project_root=current_config.project_root,
    )
    if use_run_config:
        return historical
    if target_job_type is JobType.WALK_FORWARD:
        # A cutoff is already the effective end date of the source lineage.
        # Current target-stage settings may change the walk-forward protocol,
        # but must not make the persisted offset contradict that frozen period.
        if draft.get("historical_data_cutoff") is not None:
            return replace(
                current_config,
                walk_forward_end_offset_sessions=(
                    historical.walk_forward_end_offset_sessions
                ),
            )
        return current_config
    if target_job_type is JobType.XGBOOST_CALIBRATION:
        # The calibration grid and selection protocol are fixed in calibration.py.
        # No RStockConfig field currently belongs exclusively to this stage.
        return historical
    if (
        target_job_type is JobType.THRESHOLD_CALIBRATION
        and (
            draft.get("source_threshold_parameter_calibration_run")
            or draft.get("frozen_threshold_calibration_parameters")
        )
    ):
        return historical
    return replace(
        historical,
        **{
            name: getattr(current_config, name)
            for name in THRESHOLD_CALIBRATION_CONFIG_FIELDS
        },
    )


def duplication_submission_values(
    draft: Mapping[str, object],
    *,
    current_config: RStockConfig,
    use_run_config: bool,
    job_type: JobType | str | None = None,
    current_combinations_per_target: int | None = None,
) -> dict[str, object]:
    """Resolve stage-owned settings while preserving immutable upstream inputs."""

    selected_job_type = _selected_duplication_job_type(job_type, draft)
    config = resolve_stage_configuration(
        draft,
        current_config=current_config,
        use_run_config=use_run_config,
        target_job_type=selected_job_type,
    )
    values = {**deepcopy(dict(draft)), "config": config}
    source_job_type = JobType(str(draft["job_type"]))
    if (
        selected_job_type is not source_job_type
        and selected_job_type
        in {
            JobType.XGBOOST_CALIBRATION,
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            JobType.THRESHOLD_CALIBRATION,
        }
        and draft.get("source_walk_forward_run")
    ):
        # A newly created downstream calibration uses the modern policy when
        # it has a qualified WF provenance. Same-stage historical replays keep
        # the policy persisted in their original snapshot.
        values["calibration_sampling_policy_version"] = 2
    if (
        not use_run_config
        and selected_job_type in {
            JobType.XGBOOST_CALIBRATION,
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            JobType.THRESHOLD_CALIBRATION,
        }
        and current_combinations_per_target is not None
    ):
        if current_combinations_per_target < 1:
            raise ValueError("current_combinations_per_target must be positive")
        values["combinations_per_target"] = int(current_combinations_per_target)
    return values


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
    current_combinations_per_target: int | None = None,
) -> ExperimentSpec:
    """Build a new immutable spec from the frozen run inputs only."""

    selected_job_type = validate_duplication_job(draft, job_type)
    values = duplication_submission_values(
        draft,
        current_config=current_config,
        use_run_config=use_run_config,
        job_type=selected_job_type,
        current_combinations_per_target=current_combinations_per_target,
    )
    source_job_type = normalize_duplication_job_type(draft.get("job_type"))
    preserve_xgboost = (
        use_run_config or selected_job_type is not JobType.WALK_FORWARD
    )
    preserve_threshold_parameters = selected_job_type in {
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.THRESHOLD_CALIBRATION,
    }
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
        market_benchmark_symbol=(
            None
            if values.get("market_benchmark_symbol") is None
            else str(values["market_benchmark_symbol"])
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
        source_experiment_run=str(values["source_run_id"]),
        source_walk_forward_run=(
            str(values["source_walk_forward_run"])
            if values.get("source_walk_forward_run")
            else None
        ),
        source_xgboost_calibration_run=(
            str(values["source_xgboost_calibration_run"])
            if preserve_xgboost and values.get("source_xgboost_calibration_run")
            else None
        ),
        frozen_xgboost_parameters=(
            deepcopy(values.get("frozen_xgboost_parameters"))
            if preserve_xgboost
            and isinstance(values.get("frozen_xgboost_parameters"), Mapping)
            else None
        ),
        source_threshold_parameter_calibration_run=(
            str(values["source_threshold_parameter_calibration_run"])
            if preserve_threshold_parameters
            and values.get("source_threshold_parameter_calibration_run")
            else None
        ),
        frozen_threshold_calibration_parameters=(
            deepcopy(values.get("frozen_threshold_calibration_parameters"))
            if preserve_threshold_parameters
            and isinstance(
                values.get("frozen_threshold_calibration_parameters"), Mapping
            )
            else None
        ),
        xgboost_resolution_version=(
            int(values.get("xgboost_resolution_version", 0))
            if source_job_type is selected_job_type
            and (
                use_run_config
                or selected_job_type
                in {
                    JobType.THRESHOLD_PARAMETER_CALIBRATION,
                    JobType.THRESHOLD_CALIBRATION,
                }
            )
            else 1
        ),
        run_description=(
            None if values.get("run_description") is None
            else str(values["run_description"])
        ),
        historical_data_cutoff=(
            None
            if values.get("historical_data_cutoff") is None
            else str(values["historical_data_cutoff"])
        ),
        source_prepared_dataset_sha256=(
            None
            if values.get("source_prepared_dataset_sha256") is None
            else str(values["source_prepared_dataset_sha256"])
        ),
        source_end_to_end_run=(
            None
            if values.get("source_end_to_end_run") is None
            else str(values["source_end_to_end_run"])
        ),
        source_threshold_calibration_run=(
            None
            if values.get("source_threshold_calibration_run") is None
            else str(values["source_threshold_calibration_run"])
        ),
        auto_promote_candidates=bool(values.get("auto_promote_candidates", False)),
        pipeline_version=int(values.get("pipeline_version", 0)),
        calibration_sampling_policy_version=int(
            values.get("calibration_sampling_policy_version", 1)
        ),
        combination_plan_version=(
            None
            if values.get("combination_plan_version") is None
            else int(values["combination_plan_version"])
        ),
        combination_plan_sha256=(
            None
            if values.get("combination_plan_sha256") is None
            else str(values["combination_plan_sha256"])
        ),
        combination_range_start=(
            None
            if values.get("combination_range_start") is None
            else int(values["combination_range_start"])
        ),
        combination_range_stop=(
            None
            if values.get("combination_range_stop") is None
            else int(values["combination_range_stop"])
        ),
    )
