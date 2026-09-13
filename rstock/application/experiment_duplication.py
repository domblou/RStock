"""Immutable run snapshots adapted into editable experiment duplication drafts."""

from __future__ import annotations

from copy import deepcopy
from dataclasses import asdict, fields
from pathlib import Path
from typing import Any, Mapping

from rstock.config import DEFAULT_CONFIG, RStockConfig

from .domain import JobType


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
