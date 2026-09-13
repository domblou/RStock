"""Immutable run snapshots adapted into editable experiment duplication drafts."""

from __future__ import annotations

from copy import deepcopy
from dataclasses import asdict, fields
from math import comb
from pathlib import Path
from typing import Any, Mapping

from rstock.config import DEFAULT_CONFIG, RStockConfig

from .domain import ExperimentSpec, JobType
from .universes import UniverseSelection


def walk_forward_duplication_draft(
    run_id: str, detail: Mapping[str, object]
) -> dict[str, object]:
    """Extract the editable inputs of one historical walk-forward run."""

    configuration = detail.get("configuration", {})
    if not isinstance(configuration, Mapping):
        raise ValueError("Run configuration is unavailable")
    if configuration.get("job_type") != JobType.WALK_FORWARD.value:
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
    }


def config_from_historical_snapshot(
    snapshot: Mapping[str, object] | None, *, current_project_root: Path
) -> RStockConfig:
    """Restore a partial historical config with defaults and the current root."""

    source = snapshot or {}
    allowed = {field.name for field in fields(RStockConfig)} - {"project_root"}
    values: dict[str, Any] = asdict(DEFAULT_CONFIG)
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

    config = (
        config_from_historical_snapshot(
            draft.get("rstock_config") if isinstance(draft.get("rstock_config"), Mapping) else None,
            current_project_root=current_config.project_root,
        )
        if use_run_config
        else current_config
    )
    return {"config": config, **deepcopy(dict(draft))}


def experiment_spec_from_duplication(
    draft: Mapping[str, object], *, current_config: RStockConfig, use_run_config: bool
) -> ExperimentSpec:
    """Build a new immutable spec from the frozen run inputs only."""

    values = duplication_submission_values(
        draft, current_config=current_config, use_run_config=use_run_config
    )
    return ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
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
    )
