"""Read-only presentation of persisted promotion decisions and upstream evidence."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Mapping

import pandas as pd


DEFAULT_PROMOTION_SORT = "Statut promotion puis précision holdout"


def sort_promotion_decisions(rows: pd.DataFrame) -> pd.DataFrame:
    """Sort the display copy without changing persisted decision order."""

    if rows.empty:
        return rows.copy().reset_index(drop=True)
    table = rows.copy()
    columns: list[str] = []
    ascending: list[bool] = []
    if "Statut promotion" in table:
        columns.append("Statut promotion")
        ascending.append(True)
    if "Précision holdout" in table:
        table["_sort_holdout_precision"] = pd.to_numeric(
            table["Précision holdout"], errors="coerce"
        )
        columns.append("_sort_holdout_precision")
        ascending.append(False)
    if "Combinaison" in table:
        columns.append("Combinaison")
        ascending.append(True)
    if columns:
        table = table.sort_values(columns, ascending=ascending, na_position="last", kind="stable")
    return table.drop(columns="_sort_holdout_precision", errors="ignore").reset_index(drop=True)


def _read_json(path: Path) -> dict[str, Any]:
    if not path.is_file():
        return {}
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return value if isinstance(value, dict) else {}


def _one_row(path: Path, *, set_name: str, direction: str | None = None) -> Mapping[str, Any]:
    if not path.is_file():
        return {}
    try:
        frame = pd.read_csv(path)
    except (OSError, pd.errors.ParserError, pd.errors.EmptyDataError):
        return {}
    if "Set" not in frame:
        return {}
    rows = frame[frame["Set"].astype(str) == set_name]
    if direction is not None:
        if "Direction" not in rows:
            return {}
        rows = rows[rows["Direction"].astype(str) == direction]
    if "Selected" in rows:
        rows = rows[rows["Selected"].astype(str).str.casefold().isin(("true", "1"))]
    return rows.iloc[0].to_dict() if len(rows) == 1 else {}


def _signal_counts(value: object) -> list[int] | None:
    if isinstance(value, str):
        try:
            value = json.loads(value)
        except ValueError:
            return None
    if not isinstance(value, list):
        return None
    try:
        return [int(item) for item in value]
    except (TypeError, ValueError):
        return None


def upstream_diagnostic(
    project_root: Path, decision: Mapping[str, Any], *,
    threshold_run_id: str | None, walk_forward_run_id: str | None = None,
) -> dict[str, Any]:
    """Resolve only exact run references and frozen metrics for one selected set."""

    runs = Path(project_root) / "runs"
    threshold_results = runs / str(threshold_run_id) / "results" if threshold_run_id else None
    threshold_config = (
        _read_json(runs / str(threshold_run_id) / "config.json")
        if threshold_run_id else {}
    )
    wf_id = walk_forward_run_id or threshold_config.get("source_walk_forward_run")
    set_name = str(decision.get("Combinaison") or "")
    direction = str(decision.get("Direction") or "Up")
    wf = (
        _one_row(runs / str(wf_id) / "results" / "qualification.csv", set_name=set_name)
        if wf_id and set_name else {}
    )
    selected = (
        _read_json(threshold_results / "selected_thresholds_by_set.json")
        if threshold_results else {}
    )
    choice = selected.get(set_name, {}).get(direction, {})
    choice = choice if isinstance(choice, dict) else {}
    calibration = choice.get("calibration_metrics", {})
    calibration = calibration if isinstance(calibration, dict) else {}
    metrics_run = threshold_run_id
    if threshold_results and not (threshold_results / "threshold_metrics_by_set.csv").is_file():
        metrics_run = threshold_config.get("source_threshold_calibration_run")
    metrics = (
        _one_row(
            runs / str(metrics_run) / "results" / "threshold_metrics_by_set.csv",
            set_name=set_name, direction=direction,
        ) if metrics_run and set_name else {}
    )
    return {
        "wf_median_auc": wf.get("ROCAUCMedian"),
        "wf_worst_auc": wf.get("ROCAUCWorst"),
        "wf_auc_std": wf.get("ROCAUCStd"),
        "signal_counts_by_window": _signal_counts(metrics.get("SignalCountsByWindow")),
        "calibration_total_signals": choice.get("total_signals", calibration.get("total_signals")),
        "calibration_precision": calibration.get("precision"),
        "calibration_directional_return_mean": calibration.get("directional_return_mean"),
    }
