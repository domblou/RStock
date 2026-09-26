"""Small, read-only End-to-End projections for the existing History comparison."""

from __future__ import annotations

import csv
import json
from dataclasses import dataclass
from pathlib import Path
from statistics import median
from typing import Any, Mapping, Sequence


def _json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return value if isinstance(value, dict) else {}


def _number(value: object) -> float | None:
    try:
        result = float(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return None
    return result if result == result and abs(result) != float("inf") else None


def _median(values: Sequence[float]) -> float | None:
    return median(values) if values else None


@dataclass(frozen=True, slots=True)
class EndToEndComparison:
    run_id: str
    cutoff: str | None
    status: str
    raw: int | None
    evaluated: int | None
    qualified: int | None
    confirmed: int | None
    up_evaluable: int | None
    candidates: int | None
    targets: int | None
    holdout_auc: float | None
    holdout_precision: float | None
    holdout_return: float | None
    holdout_signals: int | None
    forward_status: str
    forward_models: int | None
    forward_signals: int | None
    forward_precision: float | None
    forward_return: float | None
    forward_sessions: int | None
    forward_first: str | None
    forward_last: str | None

    @property
    def qualification_rate(self) -> float | None:
        return self.qualified / self.evaluated if self.evaluated and self.qualified is not None else None

    @property
    def confirmation_rate(self) -> float | None:
        return self.confirmed / self.qualified if self.qualified and self.confirmed is not None else None

    @property
    def candidate_yield(self) -> float | None:
        return self.candidates / self.evaluated if self.evaluated and self.candidates is not None else None


def _stage_ids(parent: Path, pipeline: Mapping[str, Any]) -> dict[str, str]:
    stages = pipeline.get("stages")
    if not isinstance(stages, list):
        stages = _json(parent / "orchestration" / "pipeline.json").get("stages", [])
    return {
        str(item["stage_key"]): str(item["child_run_id"])
        for item in stages
        if isinstance(item, dict) and item.get("stage_key") and item.get("child_run_id")
    }


def _candidate_quality(path: Path, sets: set[str]) -> tuple[float | None, float | None, float | None, int | None]:
    if not sets or not path.is_file():
        return None, None, None, None
    values: dict[str, list[float]] = {name: [] for name in ("ROCAUC", "Precision", "DirectionalReturnMean")}
    signals: list[float] = []
    found: set[str] = set()
    with path.open(encoding="utf-8", newline="") as stream:
        for row in csv.DictReader(stream):
            set_name = row.get("Set")
            if set_name not in sets or row.get("Direction") != "Up":
                continue
            if set_name in found:
                # A model must have exactly one selected holdout row.
                return None, None, None, None
            found.add(set_name)
            for name in values:
                number = _number(row.get(name))
                if number is not None:
                    values[name].append(number)
            count = _number(row.get("SignalCount"))
            if count is not None:
                signals.append(count)
    if found != sets:
        return None, None, None, None
    return (
        _median(values["ROCAUC"]) if len(values["ROCAUC"]) == len(sets) else None,
        _median(values["Precision"]) if len(values["Precision"]) == len(sets) else None,
        _median(values["DirectionalReturnMean"]) if len(values["DirectionalReturnMean"]) == len(sets) else None,
        int(sum(signals)) if len(signals) == len(sets) else None,
    )


def _forward_state(
    runs: Path, enabled: bool, pipeline: Mapping[str, Any], root_summary: Mapping[str, Any]
) -> tuple[str, dict[str, Any]]:
    recorded = pipeline.get("forward_simulation") or root_summary.get("forward_simulation") or {}
    recorded = recorded if isinstance(recorded, dict) else {}
    child_id = recorded.get("child_run_id")
    if child_id:
        child = runs / str(child_id)
        state = str(_json(child / "status.json").get("status") or "pending")
        summary = _json(child / "summary.json")
        if state == "completed":
            if summary.get("result") == "skipped_no_models":
                return "completed · skipped_no_models", summary
            signals = summary.get("total_signals")
            return ("terminée avec signaux" if _number(signals) and float(signals) > 0 else
                    "terminée sans signaux" if signals == 0 else "terminée, résultat indisponible"), summary
        return {
            "pending": "en attente", "running": "en cours", "failed": "échouée",
            "cancelled": "annulée", "interrupted": "interrompue",
        }.get(state, state), {}
    if not enabled:
        return "désactivée", {}
    return str(recorded.get("status") or "non lancée"), {}


def load_end_to_end_comparison(project_root: Path, run_id: str) -> EndToEndComparison:
    runs = Path(project_root) / "runs"
    parent = runs / run_id
    config = _json(parent / "config.json")
    status = _json(parent / "status.json")
    root_summary = _json(parent / "summary.json")
    pipeline = _json(parent / "results" / "pipeline_summary.json")
    stages = _stage_ids(parent, pipeline)
    wf = _json(runs / stages["walk_forward"] / "summary.json") if "walk_forward" in stages else {}
    threshold = (_json(runs / stages["threshold_calibration"] / "summary.json")
                 if "threshold_calibration" in stages else {})
    up = threshold.get("holdout_combination_counts", {}).get("Up", {})
    snapshot = _json(parent / "results" / "forward_model_snapshot.json")
    models = snapshot.get("models")
    models = models if isinstance(models, list) else None
    candidates = len(models) if models is not None else None
    sets = {str(item["set"]) for item in models or [] if isinstance(item, dict) and item.get("set")}
    targets = {str(item["target"]) for item in models or [] if isinstance(item, dict) and item.get("target")}
    quality = _candidate_quality(
        runs / stages["threshold_calibration"] / "results" / "holdout_metrics.csv", sets
    ) if "threshold_calibration" in stages else (None, None, None, None)
    forward_status, forward = _forward_state(
        runs, config.get("forward_simulation_enabled") is True, pipeline, root_summary
    )
    confirmed = wf.get("metrics", {}).get("FinalConfirmedSets")
    return EndToEndComparison(
        run_id=run_id,
        cutoff=config.get("resolved_market_session_cutoff") or snapshot.get("resolved_market_session_cutoff"),
        status=str(status.get("status") or "—"),
        raw=wf.get("raw_combination_count"), evaluated=wf.get("total_combinations"),
        qualified=wf.get("eligible_combinations"),
        confirmed=int(confirmed) if _number(confirmed) is not None else None,
        up_evaluable=up.get("evaluated_combinations") if isinstance(up, dict) else None,
        candidates=candidates, targets=len(targets) if models is not None else None,
        holdout_auc=quality[0], holdout_precision=quality[1],
        holdout_return=quality[2], holdout_signals=quality[3],
        forward_status=forward_status,
        forward_models=candidates if candidates is not None else forward.get("source_model_count"),
        forward_signals=forward.get("total_signals"),
        forward_precision=forward.get("precision"),
        forward_return=forward.get("directional_return_mean"),
        forward_sessions=forward.get("sessions"),
        forward_first=forward.get("first_session"), forward_last=forward.get("last_session"),
    )


def comparison_types(types: Sequence[str]) -> str | None:
    """Return the homogeneous comparison mode for two to five runs."""
    if not 2 <= len(types) <= 5:
        return None
    kind = set(types)
    return next(iter(kind)) if len(kind) == 1 and kind <= {"walk_forward", "end_to_end"} else None
