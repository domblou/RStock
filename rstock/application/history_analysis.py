"""Read-only analytical projections for walk-forward history views."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence

import pandas as pd


COMBINATION_COLUMNS = (
    "Combinaison", "Cible", "Predictors", "Depth", "AUC dev", "AUC holdout",
    "Delta dev→holdout", "Worst AUC", "Dispersion", "Fenêtres valides",
    "Observations positives", "Statut", "Eligible", "Holdout confirmé",
)

MODEL_SELECTION_COLUMNS = {
    "calibrated_signal_threshold": "Seuil calibré",
    "model_selection_score": "Score",
    "model_selection_rank": "Rang",
    "predictive_quality_score": "Score qualité prédictive",
    "stability_score": "Score stabilité",
    "holdout_score": "Score holdout",
    "signal_quality_score": "Score qualité signal",
    "sample_adequacy_score": "Score adéquation échantillon",
}


THRESHOLD_CALIBRATION_COLUMNS = (
    "Combinaison", "Cible", "Predictors", "Direction", "Seuil calibré",
    "Signaux holdout", "Précision holdout", "Success rate holdout", "Recall",
    "F1", "AUC holdout", "Rendement directionnel moyen",
    "Rendement médian", "MFE moyen", "MAE moyen",
    "Fréquence mouvement opposé",
)


def run_universe_summary(configuration: Mapping[str, Any]) -> dict[str, object]:
    """Summarize frozen universe roles for current and legacy run configurations."""

    selection = configuration.get("universe_selection", {})
    symbols = tuple(str(item) for item in configuration.get("symbols", ()))
    targets = tuple(
        str(item) for item in configuration.get("target_symbols", symbols)
    )
    predictors = tuple(
        str(item) for item in configuration.get("predictor_symbols", symbols)
    )
    context_ids = tuple(
        str(item) for item in configuration.get("context_universe_ids", ())
    )
    primary = configuration.get("primary_universe_id")
    if primary is None and isinstance(selection, Mapping):
        primary = selection.get("universe")
    return {
        "primary_universe_id": "—" if primary is None else str(primary),
        "target_count": len(targets),
        "context_universe_ids": context_ids,
        "predictor_count": len(predictors),
    }


def predictor_prefilter_summary(summary: Mapping[str, Any]) -> pd.DataFrame:
    """Return a compact, legacy-safe diagnostic table for predictor filtering."""

    records = summary.get("predictor_prefilter", ())
    if not isinstance(records, list):
        return pd.DataFrame()
    return pd.DataFrame([
        {
            "Cible": item.get("target", "—"),
            "Candidats initiaux": item.get("initial_candidates", 0),
            "Rejet AUC médiane": item.get("rejected_median_auc", "—"),
            "Rejet fenêtres > 0,50": item.get("rejected_pct_above_random", "—"),
            "Rejet Worst AUC": item.get("rejected_worst_auc", "—"),
            "Rejet dispersion": item.get("rejected_auc_std", "—"),
            "Après qualification": item.get("after_qualification", 0),
            "Après Top N": item.get("after_top_n", 0),
            "Après redondance": item.get("after_redundancy", 0),
            "Retenus": len(item.get("retained_predictors", ())),
            "Combinaisons": (
                f"{item.get('combinations_tested', 0)} au lieu de "
                f"{item.get('combinations_before_filtering', 0)}"
            ),
        }
        for item in records
        if isinstance(item, Mapping)
    ])


def selected_run_action(run_ids: Sequence[str]) -> str | None:
    """Return the only valid analytical route for a history-grid selection."""

    if len(run_ids) == 1:
        return "detail"
    if 2 <= len(run_ids) <= 4:
        return "comparison"
    return None


def _number(frame: pd.DataFrame, name: str) -> pd.Series:
    return pd.to_numeric(frame.get(name, pd.Series(pd.NA, index=frame.index)), errors="coerce")


def _boolean(series: pd.Series) -> pd.Series:
    return series.map(
        lambda value: value is True or str(value).strip().lower() in {"true", "1", "yes"}
    )


def _predictors(value: object) -> str:
    try:
        parsed = json.loads(str(value))
    except (json.JSONDecodeError, TypeError):
        return str(value)
    return " + ".join(str(item) for item in parsed) if isinstance(parsed, list) else str(value)


def load_walk_forward_artifacts(project_root: Path, run_id: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Load published artifacts only; absent/legacy files are represented by empties."""

    results = Path(project_root) / "runs" / run_id / "results"
    qualification_path = results / "qualification.csv"
    holdout_path = results / "final_holdout.csv"
    qualification = pd.read_csv(qualification_path) if qualification_path.exists() else pd.DataFrame()
    holdout = pd.read_csv(holdout_path) if holdout_path.exists() else pd.DataFrame()
    return qualification, holdout


def load_model_selection_artifact(project_root: Path, run_id: str) -> pd.DataFrame:
    """Load final scores when available; legacy runs return an empty frame."""

    path = Path(project_root) / "runs" / run_id / "results" / "selection_results.csv"
    return pd.read_csv(path) if path.exists() else pd.DataFrame()


def load_threshold_calibration_artifacts(
    project_root: Path, run_id: str
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, Any]]:
    """Load published threshold-calibration artifacts without recomputation."""

    results = Path(project_root) / "runs" / run_id / "results"
    metrics_path = results / "threshold_metrics_by_set.csv"
    holdout_path = results / "holdout_metrics.csv"
    selected_path = results / "selected_thresholds_by_set.json"
    metrics = pd.read_csv(metrics_path) if metrics_path.exists() else pd.DataFrame()
    holdout = pd.read_csv(holdout_path) if holdout_path.exists() else pd.DataFrame()
    try:
        selected = json.loads(selected_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        selected = {}
    return metrics, holdout, selected if isinstance(selected, dict) else {}


def _set_parts(set_name: object) -> tuple[str, str]:
    """Return a readable target/predictor pair from a persisted set identifier."""

    value = str(set_name)
    if "<-" not in value:
        return "—", value
    target, predictors = value.split("<-", 1)
    return target, predictors.replace("+", " + ")


def _selection_for(
    selected_by_set: Mapping[str, Any], set_name: object, direction: object
) -> Mapping[str, Any]:
    item = selected_by_set.get(str(set_name), {})
    selected = item.get(str(direction), {}) if isinstance(item, Mapping) else {}
    return selected if isinstance(selected, Mapping) else {}


def threshold_calibration_table(
    metrics_by_set: pd.DataFrame,
    holdout_metrics: pd.DataFrame,
    selected_by_set: Mapping[str, Any] | None = None,
) -> pd.DataFrame:
    """Project existing selected thresholds and holdout metrics into a UI table.

    Holdout rows are preferred.  When a historical run has no holdout artifact,
    the selected calibration rows remain inspectable with missing holdout values.
    """

    selected = selected_by_set or {}
    source = holdout_metrics.copy()
    if source.empty:
        source = metrics_by_set.copy()
        if not source.empty and "Selected" in source:
            source = source[_boolean(source["Selected"])]
    if source.empty or "Set" not in source:
        return pd.DataFrame(columns=THRESHOLD_CALIBRATION_COLUMNS)

    rows: list[dict[str, Any]] = []
    for _, row in source.iterrows():
        set_name = str(row.get("Set", "—"))
        direction = str(row.get("Direction", "—"))
        target, predictors = _set_parts(set_name)
        selection = _selection_for(selected, set_name, direction)
        threshold = row.get("Threshold")
        if pd.isna(threshold):
            threshold = selection.get("threshold")
        metric = selection.get("calibration_metrics", {})
        metric = metric if isinstance(metric, Mapping) else {}
        rows.append({
            "Combinaison": set_name,
            "Cible": str(row.get("Observation", target)),
            "Predictors": predictors,
            "Direction": direction,
            "Seuil calibré": threshold,
            "Signaux holdout": row.get("SignalCount", row.get("TotalSignals")),
            "Précision holdout": row.get("Precision", row.get("HitRate", metric.get("success_rate"))),
            "Success rate holdout": row.get("FavorableMoveFrequency", row.get("HitRate", metric.get("success_rate"))),
            "Recall": row.get("Recall"),
            "F1": row.get("F1"),
            "AUC holdout": row.get("ROCAUC"),
            "Rendement directionnel moyen": row.get("DirectionalReturnMean", row.get("AverageReturn", metric.get("mean_return"))),
            "Rendement médian": row.get("IntradayReturnMedian", row.get("MedianReturn", metric.get("median_return"))),
            "MFE moyen": row.get("MFEMean", row.get("MFE", metric.get("mfe_mean"))),
            "MAE moyen": row.get("MAEMean", row.get("MAE", metric.get("mae_mean"))),
            "Fréquence mouvement opposé": row.get("OppositeMoveFrequency"),
            "Score": row.get("model_selection_score", row.get("Score")),
        })
    columns = list(THRESHOLD_CALIBRATION_COLUMNS)
    if "model_selection_score" in source or "Score" in source:
        columns.append("Score")
    return pd.DataFrame(rows).loc[:, columns]


def filter_threshold_calibration_results(
    results: pd.DataFrame,
    *,
    direction: str = "Up",
    min_signals: int = 0,
    sort_by: str = "Précision holdout",
) -> pd.DataFrame:
    """Apply display-only threshold result filters with deterministic sorting."""

    table = results.copy()
    if direction in {"Up", "Down"} and "Direction" in table:
        table = table[table["Direction"].astype(str) == direction]
    if "Signaux holdout" in table:
        table = table[
            pd.to_numeric(table["Signaux holdout"], errors="coerce").fillna(0)
            >= min_signals
        ]
    if sort_by not in table:
        sort_by = "Précision holdout"
    return table.sort_values(
        [sort_by, "Combinaison"], ascending=[False, True],
        na_position="last", kind="stable",
    ).reset_index(drop=True)


def combination_table(
    qualification: pd.DataFrame,
    holdout: pd.DataFrame,
    *,
    depth: object = None,
    selection_results: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """Join existing development and holdout metrics into a UI-safe table."""

    if qualification.empty or "Set" not in qualification:
        return pd.DataFrame(columns=COMBINATION_COLUMNS)
    work = qualification.copy()
    work["Eligible"] = _boolean(work.get("Eligible", pd.Series(False, index=work.index)))
    if not holdout.empty and "Set" in holdout:
        names = [name for name in ("Set", "FinalUpROCAUC", "FinalConfirmed") if name in holdout]
        work = work.merge(holdout.loc[:, names], on="Set", how="left", validate="one_to_one")
    scored = selection_results if selection_results is not None else pd.DataFrame()
    if not scored.empty and "Set" in scored:
        names = ["Set", *[name for name in MODEL_SELECTION_COLUMNS if name in scored]]
        work = work.merge(scored.loc[:, names], on="Set", how="left", validate="one_to_one")
    holdout_confirmed = _boolean(
        work.get("FinalConfirmed", pd.Series(False, index=work.index))
    )
    dev_auc = _number(work, "ROCAUCMedian")
    holdout_auc = _number(work, "FinalUpROCAUC")
    result = pd.DataFrame({
        "Combinaison": work["Set"].astype(str),
        "Cible": work.get("Observation", pd.Series("—", index=work.index)).astype(str),
        "Predictors": work.get("Predictors", pd.Series("—", index=work.index)).map(_predictors),
        "Depth": depth if depth is not None else pd.NA,
        "AUC dev": dev_auc,
        "AUC holdout": holdout_auc,
        "Delta dev→holdout": holdout_auc - dev_auc,
        "Worst AUC": _number(work, "ROCAUCWorst"),
        "Dispersion": _number(work, "ROCAUCStd"),
        "Fenêtres valides": _number(work, "WindowsEvaluated"),
        "Observations positives": _number(work, "PositiveObservations"),
        "Eligible": work["Eligible"],
        "Holdout confirmé": holdout_confirmed,
    })
    result["Statut"] = "Non qualifiée"
    result.loc[result["Eligible"], "Statut"] = "Qualifiée développement"
    result.loc[result["Holdout confirmé"], "Statut"] = "Holdout confirmé"
    optional_columns: list[str] = []
    for source, display in MODEL_SELECTION_COLUMNS.items():
        if source in work:
            result[display] = _number(work, source)
            optional_columns.append(display)
    sort_columns = (
        ["Score", "AUC holdout", "AUC dev", "Combinaison"]
        if "Score" in result
        else ["AUC holdout", "AUC dev", "Combinaison"]
    )
    return result.loc[:, [*COMBINATION_COLUMNS, *optional_columns]].sort_values(
        sort_columns,
        ascending=[False] * (len(sort_columns) - 1) + [True],
        na_position="last", kind="stable",
    ).reset_index(drop=True)


def filter_combinations(
    combinations: pd.DataFrame,
    *,
    target: str = "Toutes",
    depth: object = "Toutes",
    min_dev_auc: float | None = None,
    min_holdout_auc: float | None = None,
    min_worst_auc: float | None = None,
    max_dispersion: float | None = None,
    min_positive_observations: int | None = None,
    confirmed_only: bool = False,
    eligible_only: bool = True,
) -> pd.DataFrame:
    """Apply deterministic, display-only filters without changing qualification."""

    result = combinations.copy()
    if eligible_only and "Eligible" in result:
        result = result[result["Eligible"]]
    if target != "Toutes":
        result = result[result["Cible"] == target]
    if depth != "Toutes":
        result = result[result["Depth"].astype(str) == str(depth)]
    criteria = (
        ("AUC dev", min_dev_auc, lambda value, threshold: value >= threshold),
        ("AUC holdout", min_holdout_auc, lambda value, threshold: value >= threshold),
        ("Worst AUC", min_worst_auc, lambda value, threshold: value >= threshold),
        ("Dispersion", max_dispersion, lambda value, threshold: value <= threshold),
        ("Observations positives", min_positive_observations, lambda value, threshold: value >= threshold),
    )
    for column, threshold, predicate in criteria:
        if threshold is not None and column in result:
            result = result[predicate(pd.to_numeric(result[column], errors="coerce"), threshold)]
    if confirmed_only and "Holdout confirmé" in result:
        result = result[result["Holdout confirmé"]]
    return result.reset_index(drop=True)


def _median(frame: pd.DataFrame, column: str) -> float | None:
    values = pd.to_numeric(frame.get(column, pd.Series(dtype=float)), errors="coerce").dropna()
    return None if values.empty else float(values.median())


@dataclass(frozen=True, slots=True)
class RunAnalytics:
    run_id: str
    symbols: tuple[str, ...]
    depth: object
    status: str
    duration_seconds: float | None
    combinations: pd.DataFrame
    tested_count: int | None

    @property
    def qualified_count(self) -> int:
        return int(self.combinations["Eligible"].sum()) if not self.combinations.empty else 0

    @property
    def confirmed_count(self) -> int:
        return int(self.combinations["Holdout confirmé"].sum()) if not self.combinations.empty else 0

    @property
    def dev_auc_median(self) -> float | None:
        return _median(self.combinations[self.combinations["Eligible"]], "AUC dev")

    @property
    def holdout_auc_median(self) -> float | None:
        return _median(self.combinations[self.combinations["Holdout confirmé"]], "AUC holdout")

    @property
    def delta_median(self) -> float | None:
        return _median(self.combinations[self.combinations["Holdout confirmé"]], "Delta dev→holdout")

    @property
    def qualification_rate(self) -> float | None:
        return None if not self.tested_count else self.qualified_count / self.tested_count

    @property
    def confirmation_rate(self) -> float | None:
        return None if not self.qualified_count else self.confirmed_count / self.qualified_count


def analyze_run(
    status: Mapping[str, Any], detail: Mapping[str, Any],
    qualification: pd.DataFrame, holdout: pd.DataFrame,
    selection_results: pd.DataFrame | None = None,
) -> RunAnalytics:
    configuration = detail.get("configuration", {})
    rstock = configuration.get("rstock_config", {})
    summary = detail.get("summary", {})
    combinations = combination_table(
        qualification, holdout, depth=rstock.get("permutation_depth"),
        selection_results=selection_results,
    )
    tested = summary.get("total_combinations") or summary.get("combinations")
    if tested is None and not qualification.empty:
        tested = len(qualification)
    duration = status.get("duration_seconds")
    return RunAnalytics(
        run_id=str(status["run_id"]),
        symbols=tuple(str(item) for item in configuration.get("symbols", ())),
        depth=rstock.get("permutation_depth", "—"),
        status=str(status.get("status", "—")),
        duration_seconds=None if duration is None else float(duration),
        combinations=combinations,
        tested_count=None if tested is None else int(tested),
    )


def comparison_table(analyses: Sequence[RunAnalytics], labels: Mapping[str, str]) -> pd.DataFrame:
    """Produce the summary matrix for 2–4 run comparison, including missing values."""

    rows = {
        "Profondeur": [analysis.depth for analysis in analyses],
        "Symboles": [len(analysis.symbols) for analysis in analyses],
        "Combinaisons testées": [analysis.tested_count for analysis in analyses],
        "Combinaisons qualifiées": [analysis.qualified_count for analysis in analyses],
        "% qualifiées": [analysis.qualification_rate for analysis in analyses],
        "Confirmées holdout": [analysis.confirmed_count for analysis in analyses],
        "% confirmées": [analysis.confirmation_rate for analysis in analyses],
        "AUC dev médiane": [analysis.dev_auc_median for analysis in analyses],
        "AUC holdout médiane": [analysis.holdout_auc_median for analysis in analyses],
        "Delta dev→holdout": [analysis.delta_median for analysis in analyses],
        "Durée (s)": [analysis.duration_seconds for analysis in analyses],
        "Statut": [analysis.status for analysis in analyses],
    }
    return pd.DataFrame({"Indicateur": list(rows), **{
        labels[analysis.run_id]: [rows[name][index] for name in rows]
        for index, analysis in enumerate(analyses)
    }})


def _duration_text(seconds: float | None) -> str:
    if seconds is None or pd.isna(seconds):
        return "—"
    value = max(0, int(seconds))
    if value < 3600:
        return f"{value // 60:02d}:{value % 60:02d}"
    return f"{value // 3600:02d}:{value % 3600 // 60:02d}:{value % 60:02d}"


def comparison_chart_frames(
    analyses: Sequence[RunAnalytics], timestamps: Mapping[str, str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Prepare separate, human-labelled quality and duration chart datasets."""

    bases = [f"Profondeur {analysis.depth}" for analysis in analyses]
    occurrences = {base: bases.count(base) for base in bases}
    labels = [
        base if occurrences[base] == 1 else f"{base} · Run {index + 1}"
        for index, base in enumerate(bases)
    ]
    quality = pd.DataFrame({
        "Run": labels,
        "AUC dev médiane": [analysis.dev_auc_median for analysis in analyses],
        "AUC holdout médiane": [analysis.holdout_auc_median for analysis in analyses],
        "Delta dev→holdout": [analysis.delta_median for analysis in analyses],
        "Date / heure": [timestamps[analysis.run_id] for analysis in analyses],
    })
    durations = pd.DataFrame({
        "Run": labels,
        "Durée (s)": [analysis.duration_seconds for analysis in analyses],
        "Durée": [_duration_text(analysis.duration_seconds) for analysis in analyses],
        "Date / heure": [timestamps[analysis.run_id] for analysis in analyses],
    })
    return quality, durations


def configuration_differences(
    configurations: Sequence[Mapping[str, Any]], labels: Sequence[str],
) -> pd.DataFrame:
    """Show only meaningful experiment settings that differ across compared runs."""

    fields = (
        "permutation_depth", "lag_depth", "model_history_days",
        "xgb_max_depth", "xgb_eta", "xgb_rounds", "xgb_min_child_weight",
        "xgb_subsample", "xgb_colsample_bytree", "xgb_gamma", "xgb_reg_alpha",
        "xgb_reg_lambda", "qualification_min_windows", "qualification_min_median_auc",
        "qualification_min_worst_window_auc", "final_holdout_size",
        "model_selection_predictive_quality_weight",
        "model_selection_stability_weight", "model_selection_holdout_weight",
        "model_selection_signal_quality_weight",
        "model_selection_sample_adequacy_weight",
    )
    values: dict[str, list[object]] = {}
    for field in fields:
        field_values = [config.get("rstock_config", {}).get(field, "—") for config in configurations]
        if len({str(value) for value in field_values}) > 1:
            values[field] = field_values
    symbol_values = [", ".join(str(item) for item in config.get("symbols", ())) for config in configurations]
    if len(set(symbol_values)) > 1:
        values["univers"] = symbol_values
    return pd.DataFrame({"Paramètre": list(values), **{
        label: [values[name][index] for name in values]
        for index, label in enumerate(labels)
    }})
