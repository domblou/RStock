"""Small, read-only End-to-End projections for the existing History comparison."""

from __future__ import annotations

import csv
import json
from dataclasses import dataclass, field
from pathlib import Path
from statistics import median
from typing import Any, Mapping, Sequence
from unicodedata import combining, normalize

from .auto_promotion import _promotion_guidance
from .end_to_end import effective_stage_run_id


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
    wf_final_holdout_evaluated: bool | None = None
    calibration_entries: int | None = None
    qualification_passed: int | None = None
    temporal_entering: int | None = None
    temporal_passed: int | None = None
    temporal_status: str | None = None
    temporal_requested: bool = False
    final_candidates: int | None = None
    holdout_population: dict[str, float | int | None] | None = None
    candidate_population: dict[str, float | int | None] | None = None
    rejection_counts: dict[str, int] | None = None
    rejection_analysis: QualificationRejectionAnalysis | None = None
    rejection_qualification_source: str | None = None
    scientific_profile: dict[str, object] | None = None
    dataset_digest: str | None = None
    scientific_commit: str | None = None
    comparability_complete: bool = False

    @property
    def qualification_rate(self) -> float | None:
        return self.qualified / self.evaluated if self.evaluated and self.qualified is not None else None

    @property
    def confirmation_rate(self) -> float | None:
        return self.confirmed / self.qualified if self.qualified and self.confirmed is not None else None

    @property
    def candidate_yield(self) -> float | None:
        return self.candidates / self.evaluated if self.evaluated and self.candidates is not None else None


@dataclass(frozen=True, slots=True)
class QualificationRejectionAnalysis:
    rejected: int
    one_criterion: int
    two_criteria: int
    three_or_more_criteria: int
    single: dict[str, int]
    involved: dict[str, int]
    proximity: dict[str, RejectionProximity] = field(default_factory=dict)


@dataclass(frozen=True, slots=True)
class RejectionProximity:
    labels: tuple[str, str, str]
    counts: tuple[int, int, int]
    unavailable: int


def _stage_ids(parent: Path, pipeline: Mapping[str, Any]) -> dict[str, str]:
    manifest_path = parent / "orchestration" / "pipeline.json"
    if manifest_path.is_file():
        manifest = _json(manifest_path)
        stages = manifest.get("stages", [])
        resolved: dict[str, str] = {}
        for item in stages if isinstance(stages, list) else []:
            if not isinstance(item, dict) or not item.get("stage_key"):
                continue
            key = str(item["stage_key"])
            try:
                run_id = effective_stage_run_id(manifest, key)
            except (KeyError, ValueError):
                continue
            if run_id is not None:
                resolved[key] = run_id
        return resolved
    stages = pipeline.get("stages")
    if not isinstance(stages, list):
        stages = []
    return {
        str(item["stage_key"]): str(item["child_run_id"])
        for item in stages
        if isinstance(item, dict) and item.get("stage_key") and item.get("child_run_id")
    }


def _final_candidate_sets(
    parent: Path, threshold_results: Path | None, config: Mapping[str, Any],
) -> tuple[int | None, set[str], set[str]]:
    snapshot_path = parent / "results" / "forward_model_snapshot.json"
    if snapshot_path.is_file():
        models = _json(snapshot_path).get("models")
        if not isinstance(models, list):
            return None, set(), set()
        return (
            len(models),
            {str(item["set"]) for item in models if isinstance(item, dict) and item.get("set")},
            {str(item["target"]) for item in models if isinstance(item, dict) and item.get("target")},
        )
    if threshold_results is None or not all(
        (threshold_results / name).is_file()
        for name in (
            "selected_thresholds_by_set.json", "threshold_metrics_by_set.csv",
            "holdout_metrics.csv",
        )
    ):
        return None, set(), set()
    try:
        selected = json.loads(
            (threshold_results / "selected_thresholds_by_set.json").read_text(encoding="utf-8")
        )
        if not isinstance(selected, dict):
            return None, set(), set()
        guidance = _promotion_guidance(
            threshold_results, selected, config.get("rstock_config")
        )
    except (OSError, ValueError, TypeError, KeyError):
        return None, set(), set()
    if "Statut promotion" not in guidance:
        return None, set(), set()
    candidates = guidance[guidance["Statut promotion"].eq("Candidat")]
    return (
        len(candidates),
        set(candidates["Combinaison"].astype(str)),
        set(candidates["Cible"].astype(str)),
    )


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
    wf_path = runs / stages["walk_forward"] if "walk_forward" in stages else None
    wf = _json(wf_path / "summary.json") if wf_path else {}
    wf_config = _json(wf_path / "config.json") if wf_path else {}
    threshold = (_json(runs / stages["threshold_calibration"] / "summary.json")
                 if "threshold_calibration" in stages else {})
    holdout = (_json(runs / stages["holdout_evaluation"] / "summary.json")
               if "holdout_evaluation" in stages else threshold)
    up = holdout.get("run_configuration", {}).get("holdout_combination_counts", {}).get("Up", {})
    if not up:
        up = threshold.get("holdout_combination_counts", {}).get("Up", {})
    snapshot = _json(parent / "results" / "forward_model_snapshot.json")
    threshold_results = (
        runs / stages["threshold_calibration"] / "results"
        if "threshold_calibration" in stages else None
    )


    qualification: dict[str, Any] = {}
    if "promotion_qualification" in stages:
        qualification = _json(runs / stages["promotion_qualification"] / "results" / "qualification.json")
        decisions = qualification.get("decisions", [])
        accepted = [row for row in decisions if isinstance(row, dict) and row.get("candidate")]
        sets = {str(row["Combinaison"]) for row in accepted if row.get("Combinaison")}
        targets = {str(row["Cible"]) for row in accepted if row.get("Cible")}
        candidate_sets = qualification.get("candidate_sets")
        candidates = (len(candidate_sets) if isinstance(candidate_sets, list)
                      else len(accepted)) if qualification else None
        if isinstance(candidate_sets, list):
            sets = set(map(str, candidate_sets))
    else:
        candidates, sets, targets = _final_candidate_sets(parent, threshold_results, config)
    holdout_results = (runs / stages["holdout_evaluation"] / "results"
                       if "holdout_evaluation" in stages else threshold_results)
    quality = _candidate_quality(
        holdout_results / "holdout_metrics.csv", sets
    ) if holdout_results is not None else (None, None, None, None)
    holdout_path = holdout_results / "holdout_metrics.csv" if holdout_results else None
    population = _holdout_population(holdout_path) if holdout_path else None
    candidate_population = _holdout_population(holdout_path, sets) if holdout_path and candidates else None
    forced_id = stages.get("forced_candidate_validation_end_to_end")
    forced_stages = _stage_ids(runs / forced_id, {}) if forced_id else {}
    forced_qualification = (_json(runs / forced_stages["promotion_qualification"] / "results" / "qualification.json")
                            if "promotion_qualification" in forced_stages else {})
    rejection_qualification = forced_qualification if forced_id else qualification
    forced_sets = forced_qualification.get("candidate_sets")
    if forced_id:
        fixed_id = forced_stages.get("fixed_candidate_evaluation")
        fixed_path = runs / fixed_id / "results" / "holdout_metrics.csv" if fixed_id else None
        candidate_population = (_holdout_population(fixed_path, set(map(str, forced_sets)))
                                if fixed_path and isinstance(forced_sets, list) and forced_sets else None)
    temporal = _json(parent / "results" / "temporal_validation_comparison.json")
    final_candidates = (
        len(forced_sets) if isinstance(forced_sets, list) else None
    ) if forced_id else (None if config.get("temporal_validation_enabled") is True else candidates)
    temporal_passed = len(forced_sets) if isinstance(forced_sets, list) else None
    wf_run_configuration = _json(wf_path / "results" / "run_configuration.json") if wf_path else {}
    final_holdout_evaluated = wf_run_configuration.get("final_holdout_evaluated")
    if not isinstance(final_holdout_evaluated, bool):
        final_holdout_evaluated = wf_config.get("evaluate_final_holdout")
    if not isinstance(final_holdout_evaluated, bool):
        final_holdout_evaluated = None
    if final_holdout_evaluated is False:
        confirmed = None
    else:
        confirmed = wf.get("metrics", {}).get("FinalConfirmedSets")
    calibration_entries = threshold.get("source_qualified_combinations")
    if calibration_entries is None:
        calibration_entries = wf.get("eligible_combinations") if "threshold_calibration" in stages else None
    forward_status, forward = _forward_state(
        runs, config.get("forward_simulation_enabled") is True, pipeline, root_summary
    )
    return EndToEndComparison(
        run_id=run_id,
        cutoff=config.get("resolved_market_session_cutoff") or snapshot.get("resolved_market_session_cutoff"),
        status=str(status.get("status") or "—"),
        raw=wf.get("raw_combination_count"), evaluated=wf.get("total_combinations"),
        qualified=wf.get("eligible_combinations"),
        confirmed=int(confirmed) if _number(confirmed) is not None else None,
        up_evaluable=up.get("evaluated_combinations") if isinstance(up, dict) else None,
        candidates=candidates, targets=len(targets) if candidates is not None else None,
        holdout_auc=quality[0], holdout_precision=quality[1],
        holdout_return=quality[2], holdout_signals=quality[3],
        forward_status=forward_status,
        forward_models=candidates if candidates is not None else forward.get("source_model_count"),
        forward_signals=forward.get("total_signals"),
        forward_precision=forward.get("precision"),
        forward_return=forward.get("directional_return_mean"),
        forward_sessions=forward.get("sessions"),
        forward_first=forward.get("first_session"), forward_last=forward.get("last_session"),
        wf_final_holdout_evaluated=final_holdout_evaluated,
        calibration_entries=calibration_entries,
        qualification_passed=candidates,
        temporal_entering=candidates if forced_id else None,
        temporal_passed=temporal_passed,
        temporal_status=str(temporal.get("final_status") or temporal.get("status")) if temporal else None,
        temporal_requested=bool(config.get("temporal_validation_enabled") or forced_id
                                or stages.get("temporal_validation_end_to_end")),
        final_candidates=final_candidates,
        holdout_population=population,
        candidate_population=candidate_population,
        rejection_counts=_rejection_counts(qualification.get("decisions")) if qualification else None,
        rejection_analysis=_qualification_rejection_analysis(rejection_qualification)
        if rejection_qualification else None,
        rejection_qualification_source=("Forcée" if forced_id else "Normale")
        if rejection_qualification else None,
        scientific_profile=_scientific_profile(config, final_holdout_evaluated),
        dataset_digest=wf.get("traceability", {}).get("prepared_dataset_sha256"),
        scientific_commit=wf.get("traceability", {}).get("git_commit"),
        comparability_complete=isinstance(config.get("rstock_config"), dict)
        and bool(config.get("target_symbols")) and final_holdout_evaluated is not None,
    )


def _holdout_population(path: Path, sets: set[str] | None = None) -> dict[str, float | int | None] | None:
    if not path.is_file():
        return None
    rows: list[dict[str, str]] = []
    with path.open(encoding="utf-8", newline="") as stream:
        for row in csv.DictReader(stream):
            if row.get("Direction") == "Up" and (sets is None or row.get("Set") in sets):
                rows.append(row)
    if sets is not None and {row.get("Set") for row in rows} != sets:
        return None
    if len({row.get("Set") for row in rows}) != len(rows):
        return None

    def metric(name: str, *, average: bool = False) -> float | None:
        values = [number for row in rows if (number := _number(row.get(name))) is not None]
        if not values:
            return None
        return sum(values) / len(values) if average else median(values)

    return {
        "count": len(rows),
        "auc_median": metric("ROCAUC"),
        "precision_median": metric("Precision"),
        "return_mean": metric("DirectionalReturnMean", average=True),
        "return_median": metric("DirectionalReturnMean"),
        "return_models": sum(_number(row.get("DirectionalReturnMean")) is not None for row in rows),
        "signals_median": metric("SignalCount"),
        "signals_total": int(sum(float(_number(row.get("SignalCount"))) for row in rows))
        if rows and all(_number(row.get("SignalCount")) is not None for row in rows) else None,
    }


def _rejection_counts(decisions: object) -> dict[str, int] | None:
    if not isinstance(decisions, list):
        return None
    counts: dict[str, int] = {}
    categories = (
        ("Signaux insuffisants", ("signaux", "signal")),
        ("Précision insuffisante", ("précision", "precision")),
        ("AUC insuffisante", ("auc",)),
        ("Rendement insuffisant", ("rendement", "return")),
        ("Mouvement opposé", ("opposé", "opposite")),
    )
    for decision in decisions:
        if not isinstance(decision, dict) or decision.get("candidate") is True:
            continue
        reasons = decision.get("reasons")
        if not isinstance(reasons, list):
            reasons = [decision.get("Raison", "")]
        matched: set[str] = set()
        for reason in reasons:
            lowered = str(reason).lower()
            matched.add(next((name for name, keys in categories if any(key in lowered for key in keys)), "Autre"))
        for name in matched:
            counts[name] = counts.get(name, 0) + 1
    return counts


def _reason_category(reason: str) -> str:
    plain = "".join(char for char in normalize("NFKD", reason.lower()) if not combining(char))
    if "precision" in plain:
        return "Précision"
    if "auc" in plain:
        return "AUC"
    if "signal" in plain or "signaux" in plain:
        return "Signaux"
    if "rendement" in plain or "return" in plain:
        return "Rendement"
    if "oppose" in plain or "opposite" in plain:
        return "Mouvement opposé"
    if "seuil" in plain or "threshold" in plain:
        return "Seuil"
    if "walk-forward" in plain:
        return "Walk-forward forcé"
    if "evaluation forcee" in plain or "holdout force" in plain:
        return "Évaluation forcée"
    if "direction" in plain:
        return "Direction"
    return "Autre"


_PROXIMITY_RULES = {
    # Holdout proportions and returns are persisted as fractions, not percentages.
    "Précision": ("Précision holdout", "promotion_min_holdout_precision", "min",
                  (0.01, 0.02, 0.05), ("≤1 pt", "≤2 pts", "≤5 pts")),
    "AUC": ("AUC holdout", "promotion_min_holdout_auc", "min",
            (0.01, 0.02, 0.05), ("≤0,01", "≤0,02", "≤0,05")),
    "Signaux": ("Signaux holdout", "promotion_min_holdout_signals", "min",
                (1.0, 2.0, 5.0), ("manque de 1", "≤2", "≤5")),
    "Rendement": ("Rendement directionnel moyen", "promotion_min_mean_directional_return", "min",
                  (0.001, 0.0025, 0.005), ("≤10 pb", "≤25 pb", "≤50 pb")),
    "Mouvement opposé": ("Fréquence mouvement opposé", "promotion_max_opposite_movement_frequency", "max",
                          (0.01, 0.02, 0.05), ("≤1 pt", "≤2 pts", "≤5 pts")),
}


def _single_rejection_gap(
    category: str, row: Mapping[str, Any], policy: Mapping[str, Any],
) -> float | None:
    metric_key, policy_key, bound, _, _ = _PROXIMITY_RULES[category]
    value = _number(row.get(metric_key))
    limit = _number(policy.get(policy_key))
    if value is None or limit is None:
        return None
    if (bound == "max" and value <= limit) or (
        bound == "min" and (value > limit if category == "Rendement" else value >= limit)
    ):
        return None
    gap = limit - value if bound == "min" else value - limit
    return gap


def _qualification_rejection_analysis(
    qualification: Mapping[str, Any],
) -> QualificationRejectionAnalysis | None:
    decisions = qualification.get("decisions")
    if not isinstance(decisions, list):
        return None
    candidate_sets = qualification.get("candidate_sets")
    if isinstance(candidate_sets, list) and candidate_sets and not decisions:
        return None
    policy = qualification.get("policy_parameters")
    policy = policy if isinstance(policy, dict) else {}
    by_identity: dict[tuple[str, str, str], tuple[bool, set[str], dict[str, Any]]] = {}
    for row in decisions:
        if not isinstance(row, dict) or not isinstance(row.get("candidate"), bool):
            return None
        identity = tuple(str(row.get(key) or "") for key in ("Cible", "Combinaison", "Direction"))
        if not all(identity):
            return None
        reasons = row.get("reasons")
        if not isinstance(reasons, list) or any(not isinstance(reason, str) for reason in reasons):
            return None
        reason_set = {reason.strip() for reason in reasons if reason.strip()}
        if not row["candidate"] and not reason_set:
            return None
        previous = by_identity.get(identity)
        if previous is not None and previous[0] != row["candidate"]:
            return None
        if previous is not None and any(
            previous[2].get(metric_key) != row.get(metric_key)
            for metric_key, _, _, _, _ in _PROXIMITY_RULES.values()
        ):
            return None
        by_identity[identity] = (
            row["candidate"], (previous[1] if previous else set()) | reason_set, row,
        )

    rejected = 0
    one = two = three_or_more = 0
    single: dict[str, int] = {}
    involved: dict[str, int] = {}
    proximity_counts = {name: [0, 0, 0] for name in _PROXIMITY_RULES}
    proximity_unavailable = {name: 0 for name in _PROXIMITY_RULES}
    for candidate, reasons, row in by_identity.values():
        if candidate:
            continue
        rejected += 1
        category_set = {_reason_category(reason) for reason in reasons}
        if len(reasons) == 1:
            one += 1
            category = next(iter(category_set))
            single[category] = single.get(category, 0) + 1
            if category in _PROXIMITY_RULES:
                gap = _single_rejection_gap(category, row, policy)
                if gap is None:
                    proximity_unavailable[category] += 1
                else:
                    for index, upper_bound in enumerate(_PROXIMITY_RULES[category][3]):
                        if gap <= upper_bound + 1e-12:
                            proximity_counts[category][index] += 1
        elif len(reasons) == 2:
            two += 1
        else:
            three_or_more += 1
        for category in category_set:
            involved[category] = involved.get(category, 0) + 1
    proximity = {
        name: RejectionProximity(rule[4], tuple(proximity_counts[name]), proximity_unavailable[name])
        for name, rule in _PROXIMITY_RULES.items() if _number(policy.get(rule[1])) is not None
    }
    return QualificationRejectionAnalysis(rejected, one, two, three_or_more, single, involved, proximity)


_SCIENTIFIC_PREFIXES = (
    "qualification_", "promotion_", "threshold_calibration_", "predictor_prefilter_",
    "temporal_", "model_selection_", "xgb_",
)
_SCIENTIFIC_FIELDS = {
    "intraday_target_threshold", "intraday_down_threshold", "lag_depth", "permutation_depth",
    "keep_predictor_under", "max_generated_sets", "train_fraction", "prediction_threshold",
    "walk_forward_window_mode", "walk_forward_min_train_size", "walk_forward_train_size",
    "walk_forward_test_size", "walk_forward_step_size", "walk_forward_max_symbols",
    "walk_forward_end_offset_sessions", "final_holdout_size", "final_confirmation_min_auc",
    "xgboost_global_max_qualified_combinations", "threshold_parameter_calibration_max_models",
    "xgb_seed",
}
_TECHNICAL_FIELDS = {
    "xgb_nthread", "market_cache_workers", "combination_workers",
    "predictor_prefilter_batch_size", "walk_forward_batch_size", "final_holdout_batch_size",
}


def _scientific_profile(config: Mapping[str, Any], final_holdout_evaluated: bool | None) -> dict[str, object]:
    settings = config.get("rstock_config")
    settings = settings if isinstance(settings, dict) else {}
    scientific = {name: value for name, value in settings.items()
                  if (name in _SCIENTIFIC_FIELDS or name.startswith(_SCIENTIFIC_PREFIXES))
                  and name not in _TECHNICAL_FIELDS}
    return {
        "Univers": {key: config.get(key) for key in ("primary_universe_id", "target_symbols", "predictor_symbols", "context_symbols", "combinations_per_target", "calendar")},
        "Profondeur": settings.get("permutation_depth"),
        "WF scientifique": {key: value for key, value in scientific.items()
                            if key.startswith(("walk_forward_", "predictor_prefilter_", "qualification_", "model_selection_"))
                            or key in {"final_holdout_size", "final_confirmation_min_auc", "train_fraction", "prediction_threshold",
                                       "intraday_target_threshold", "intraday_down_threshold", "lag_depth", "permutation_depth",
                                       "keep_predictor_under", "max_generated_sets"}},
        "Qualification": {key: value for key, value in scientific.items()
                          if key.startswith(("promotion_", "temporal_"))},
        "Holdout": {"final_holdout_size": settings.get("final_holdout_size"),
                    "evaluate_final_holdout": final_holdout_evaluated},
        "Calibration et seuils": {key: value for key, value in scientific.items()
                                  if key.startswith(("threshold_calibration_", "xgb_"))
                                  or key in {"threshold_parameter_calibration_max_models", "xgboost_global_max_qualified_combinations"}},
        "Version scientifique": {key: config.get(key) for key in ("pipeline_version", "calibration_sampling_policy_version")},
    }
def comparison_types(types: Sequence[str]) -> str | None:
    """Return the homogeneous comparison mode for two to six runs."""
    if not 2 <= len(types) <= 6:
        return None
    kind = set(types)
    return next(iter(kind)) if len(kind) == 1 and kind <= {
        "walk_forward", "end_to_end", "predictor_prefilter", "forward_simulation",
    } else None
