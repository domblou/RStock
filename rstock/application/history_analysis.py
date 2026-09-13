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


def combination_table(
    qualification: pd.DataFrame,
    holdout: pd.DataFrame,
    *,
    depth: object = None,
) -> pd.DataFrame:
    """Join existing development and holdout metrics into a UI-safe table."""

    if qualification.empty or "Set" not in qualification:
        return pd.DataFrame(columns=COMBINATION_COLUMNS)
    work = qualification.copy()
    work["Eligible"] = _boolean(work.get("Eligible", pd.Series(False, index=work.index)))
    if not holdout.empty and "Set" in holdout:
        names = [name for name in ("Set", "FinalUpROCAUC", "FinalConfirmed") if name in holdout]
        work = work.merge(holdout.loc[:, names], on="Set", how="left", validate="one_to_one")
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
    return result.loc[:, COMBINATION_COLUMNS].sort_values(
        ["AUC holdout", "AUC dev", "Combinaison"],
        ascending=[False, False, True], na_position="last", kind="stable",
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
) -> RunAnalytics:
    configuration = detail.get("configuration", {})
    rstock = configuration.get("rstock_config", {})
    summary = detail.get("summary", {})
    combinations = combination_table(
        qualification, holdout, depth=rstock.get("permutation_depth")
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
