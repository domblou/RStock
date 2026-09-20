"""Read-only provenance for candidates lost during temporal validation."""

from __future__ import annotations

import json
from collections.abc import Mapping
from pathlib import Path

import pandas as pd

from .auto_promotion import _promotion_guidance


_MISSING = {"last_stage": "—", "elimination_reason": "—"}


def _read_json(path: Path) -> dict[str, object] | None:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    return value if isinstance(value, dict) else None


def _read_csv(path: Path) -> pd.DataFrame | None:
    try:
        return pd.read_csv(path)
    except (OSError, pd.errors.ParserError, pd.errors.EmptyDataError):
        return None


def _as_bool(value: object) -> bool:
    return str(value).strip().lower() in {"true", "1"}


def _reasons(value: object) -> list[str]:
    try:
        parsed = json.loads(str(value))
    except (TypeError, json.JSONDecodeError):
        return []
    return [str(item) for item in parsed] if isinstance(parsed, list) else []


def _prefilter_rows(prefilter: pd.DataFrame) -> tuple[dict[tuple[str, str], Mapping[str, object]], dict[tuple[str, str], int]]:
    required = {"Observation", "Predictor", "Eligible", "PrefilterStatus"}
    if not required.issubset(prefilter):
        return {}, {}
    rows: dict[tuple[str, str], Mapping[str, object]] = {}
    ranks: dict[tuple[str, str], int] = {}
    for target, group in prefilter.groupby("Observation", sort=False):
        eligible = group[group["Eligible"].map(_as_bool)]
        for rank, (_, row) in enumerate(eligible.iterrows(), start=1):
            ranks[(str(target), str(row["Predictor"]))] = rank
        for _, row in group.iterrows():
            rows[(str(target), str(row["Predictor"]))] = row
    return rows, ranks


def _number(value: object) -> float | None:
    numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    return float(numeric) if pd.notna(numeric) else None


def _reason_text(reason: str, row: Mapping[str, object], config: Mapping[str, object], *, prefilter: bool) -> str:
    """Describe one persisted reason without re-evaluating it."""
    threshold_keys = {
        "worst_window_auc": "predictor_prefilter_min_worst_auc" if prefilter else "qualification_min_worst_window_auc",
        "median_auc": "predictor_prefilter_min_median_auc" if prefilter else "qualification_min_median_auc",
        "windows_above_random": "predictor_prefilter_min_pct_above_random" if prefilter else "qualification_min_pct_windows_above_random",
        "auc_std": "predictor_prefilter_max_auc_std" if prefilter else "qualification_max_auc_std",
        "insufficient_windows": "qualification_min_windows",
        "insufficient_auc_windows": "qualification_min_windows",
        "positive_observations": "qualification_min_positive_observations",
    }
    metrics = {
        "worst_window_auc": "ROCAUCWorst", "median_auc": "ROCAUCMedian",
        "windows_above_random": "PctWindowsAboveRandom", "auc_std": "ROCAUCStd",
        "insufficient_windows": "WindowsEvaluated", "insufficient_auc_windows": "AUCWindows",
        "positive_observations": "PositiveObservations",
    }
    key, metric = threshold_keys.get(reason), metrics.get(reason)
    if key is None or metric is None:
        return reason
    value, threshold = _number(row.get(metric)), _number(config.get(key))
    if value is None or threshold is None:
        return reason
    if reason == "worst_window_auc":
        return f"Worst AUC {value:.3f} < {threshold:.2f}"
    if reason == "median_auc":
        return f"AUC médiane {value:.3f} < {threshold:.2f}"
    if reason == "windows_above_random":
        return (
            f"Fenêtres > 0.50 : {value * 100:.1f} % < {threshold * 100:.1f} %"
        )
    if reason == "auc_std":
        return f"Écart-type AUC {value:.3f} > {threshold:.2f}"
    if reason == "insufficient_windows":
        return f"Fenêtres valides {value:.0f} < {threshold:.0f}"
    if reason == "insufficient_auc_windows":
        return f"Fenêtres AUC {value:.0f} < {threshold:.0f}"
    return f"Observations positives {value:.0f} < {threshold:.0f}"


def _ineligibility_reason(row: Mapping[str, object], config: Mapping[str, object], *, prefilter: bool) -> str:
    reasons = _reasons(row.get("IneligibilityReasons"))
    if not reasons:
        return "—"
    return " · ".join(_reason_text(reason, row, config, prefilter=prefilter) for reason in reasons)


def resolve_lost_candidate_traces(
    lost_candidates: object,
    *,
    prefilter: pd.DataFrame | None,
    qualification: pd.DataFrame | None,
    final_holdout: pd.DataFrame | None,
    config: Mapping[str, object] | None,
    promotion_lookup: Mapping[tuple[str, str], Mapping[str, object]] | None = None,
) -> dict[tuple[str, str], dict[str, str]]:
    """Resolve display-only first-stop provenance from already persisted rows."""

    if not isinstance(lost_candidates, list) or prefilter is None or qualification is None:
        return {}
    prefilter_rows, ranks = _prefilter_rows(prefilter)
    if not prefilter_rows:
        return {}
    settings = config or {}
    top_n = settings.get("predictor_prefilter_top_n")
    qualified = {
        str(row["Set"]): row
        for _, row in qualification.iterrows()
        if "Set" in row
    }
    holdout = (
        {str(row["Set"]): row for _, row in final_holdout.iterrows() if "Set" in row}
        if final_holdout is not None and "Set" in final_holdout
        else {}
    )
    promotion = promotion_lookup or {}
    resolved: dict[tuple[str, str], dict[str, str]] = {}
    for item in lost_candidates:
        if not isinstance(item, Mapping):
            continue
        set_id = str(item.get("symbol_set_id", ""))
        direction = str(item.get("direction", "Up"))
        target = str(item.get("target", ""))
        predictors = item.get("predictors")
        if not isinstance(predictors, list):
            resolved[(set_id, direction)] = dict(_MISSING)
            continue
        rejected: list[str] = []
        top_n_rejected: list[tuple[int, str]] = []
        incomplete = False
        for predictor in predictors:
            name = str(predictor)
            row = prefilter_rows.get((target, name))
            if row is None:
                incomplete = True
                break
            status = str(row.get("PrefilterStatus", ""))
            if status == "rejected_top_n":
                rank = ranks.get((target, name))
                if rank is None or top_n is None:
                    incomplete = True
                    break
                top_n_rejected.append(
                    (rank, f"{name} rang {rank} > Top {int(top_n)}")
                )
            elif status == "removed_redundancy":
                redundant_with = row.get("RedundantWith")
                rejected.append(
                    f"{name} redondant avec {redundant_with}"
                    if pd.notna(redundant_with) else f"{name} redondant"
                )
            elif status != "retained":
                rejected.append(_ineligibility_reason(row, settings, prefilter=True))
        if incomplete:
            resolved[(set_id, direction)] = dict(_MISSING)
        elif rejected or top_n_rejected:
            resolved[(set_id, direction)] = {
                "last_stage": "Préfiltre",
                "elimination_reason": " · ".join(
                    rejected + [text for _, text in sorted(top_n_rejected)]
                ),
            }
        else:
            row = qualified.get(set_id)
            if row is None:
                resolved[(set_id, direction)] = dict(_MISSING)
            elif not _as_bool(row.get("Eligible")):
                resolved[(set_id, direction)] = {
                    "last_stage": "Qualification WF",
                    "elimination_reason": _ineligibility_reason(row, settings, prefilter=False),
                }
            elif set_id not in holdout:
                resolved[(set_id, direction)] = dict(_MISSING)
            elif not _as_bool(holdout[set_id].get("FinalConfirmed")):
                resolved[(set_id, direction)] = {
                    "last_stage": "Holdout final",
                    "elimination_reason": "Holdout non confirmé",
                }
            else:
                guidance = promotion.get((set_id, direction))
                if guidance and str(guidance.get("promotion_status")) == "Non candidat":
                    resolved[(set_id, direction)] = {
                        "last_stage": "Calibration seuils",
                        "elimination_reason": str(guidance.get("promotion_reason") or "—"),
                    }
                elif guidance:
                    resolved[(set_id, direction)] = {
                        "last_stage": "Candidat",
                        "elimination_reason": "—",
                    }
                else:
                    resolved[(set_id, direction)] = dict(_MISSING)
    return resolved


def lost_candidate_trace_lookup(
    stability: object, validation_run_directory: Path
) -> dict[tuple[str, str], dict[str, str]]:
    """Load validation artifacts once and return trace rows for lost candidates."""

    if not isinstance(stability, Mapping):
        return {}
    manifest = _read_json(validation_run_directory / "orchestration" / "pipeline.json")
    config = _read_json(validation_run_directory / "config.json")
    if manifest is None or config is None:
        return {}
    stages = {str(stage.get("stage_key")): str(stage.get("child_run_id")) for stage in manifest.get("stages", []) if isinstance(stage, Mapping) and stage.get("child_run_id")}
    walk_forward_id = stages.get("walk_forward")
    if walk_forward_id is None:
        return {}
    root = validation_run_directory.parent
    results = root / walk_forward_id / "results"
    promotion: dict[tuple[str, str], Mapping[str, object]] = {}
    threshold_id = stages.get("threshold_calibration")
    if threshold_id is not None:
        threshold_results = root / threshold_id / "results"
        selected = _read_json(threshold_results / "selected_thresholds_by_set.json")
        if selected is not None:
            try:
                guidance = _promotion_guidance(threshold_results, selected)
            except ValueError:
                guidance = pd.DataFrame()
            for _, row in guidance.iterrows():
                promotion[(str(row.get("Combinaison", "")), str(row.get("Direction", "Up")))] = {
                    "promotion_status": row.get("Statut promotion"),
                    "promotion_reason": row.get("Raison"),
                }
    rstock = config.get("rstock_config", {})
    return resolve_lost_candidate_traces(
        stability.get("lost_candidates"),
        prefilter=_read_csv(results / "predictor_prefilter.csv"),
        qualification=_read_csv(results / "qualification.csv"),
        final_holdout=_read_csv(results / "final_holdout.csv"),
        config=rstock if isinstance(rstock, Mapping) else {},
        promotion_lookup=promotion,
    )


def forced_candidate_trace_lookup(
    stability: object, forced_run_directory: Path
) -> dict[tuple[str, str], dict[str, str]]:
    """Resolve forced-validation statuses from its canonical child artifacts."""

    if not isinstance(stability, Mapping):
        return {}
    manifest = _read_json(forced_run_directory / "orchestration" / "pipeline.json")
    config = _read_json(forced_run_directory / "config.json")
    if manifest is None or config is None:
        return {}
    stages = {
        str(stage.get("stage_key")): str(stage.get("child_run_id"))
        for stage in manifest.get("stages", [])
        if isinstance(stage, Mapping) and stage.get("child_run_id")
    }
    walk_id = stages.get("walk_forward")
    threshold_id = stages.get(
        "fixed_candidate_evaluation", stages.get("threshold_calibration")
    )
    if walk_id is None:
        return {}
    root = forced_run_directory.parent
    qualification = _read_csv(root / walk_id / "results" / "qualification.csv")
    holdout = _read_csv(root / walk_id / "results" / "final_holdout.csv")
    if qualification is None or "Set" not in qualification:
        return {}
    qualified = {str(row["Set"]): row for _, row in qualification.iterrows()}
    holdout_rows = (
        {str(row["Set"]): row for _, row in holdout.iterrows()}
        if holdout is not None and "Set" in holdout else {}
    )
    promotion: dict[tuple[str, str], Mapping[str, object]] = {}
    if threshold_id is not None:
        results = root / threshold_id / "results"
        selected = _read_json(results / "selected_thresholds_by_set.json")
        if selected is not None:
            try:
                guidance = _promotion_guidance(results, selected)
            except ValueError:
                guidance = pd.DataFrame()
            for _, row in guidance.iterrows():
                promotion[(str(row.get("Combinaison", "")), str(row.get("Direction", "Up")))] = {
                    "status": row.get("Statut promotion"), "reason": row.get("Raison")
                }
    rstock = config.get("rstock_config", {})
    settings = rstock if isinstance(rstock, Mapping) else {}
    candidates = [
        item
        for name in ("common_candidates", "lost_candidates")
        for item in stability.get(name, [])
        if isinstance(item, Mapping)
    ]
    resolved: dict[tuple[str, str], dict[str, str]] = {}
    for item in candidates:
        set_id = str(item.get("symbol_set_id", ""))
        direction = str(item.get("direction", "Up"))
        row = qualified.get(set_id)
        if row is None:
            resolved[(set_id, direction)] = {
                "status": "Non évaluable", "last_stage": "—", "reason": "—"
            }
        elif not _as_bool(row.get("Eligible")):
            resolved[(set_id, direction)] = {
                "status": "Échec qualification WF",
                "last_stage": "Qualification WF",
                "reason": _ineligibility_reason(row, settings, prefilter=False),
            }
        elif set_id not in holdout_rows or not _as_bool(holdout_rows[set_id].get("FinalConfirmed")):
            resolved[(set_id, direction)] = {
                "status": "Holdout non confirmé", "last_stage": "Holdout final",
                "reason": "Holdout non confirmé",
            }
        else:
            guidance = promotion.get((set_id, direction))
            if guidance is None:
                resolved[(set_id, direction)] = {
                    "status": "Aucun seuil sélectionné", "last_stage": "Calibration seuils",
                    "reason": "Aucun seuil sélectionné",
                }
            elif str(guidance.get("status")) == "Candidat":
                resolved[(set_id, direction)] = {
                    "status": "Candidat confirmé", "last_stage": "Candidat",
                    "reason": "—",
                }
            else:
                resolved[(set_id, direction)] = {
                    "status": "Non candidat", "last_stage": "Calibration seuils",
                    "reason": str(guidance.get("reason") or "—"),
                }
    return resolved
