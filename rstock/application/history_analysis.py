"""Read-only analytical projections for walk-forward history views."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence

import numpy as np
import pandas as pd

from rstock.evaluation import classification_metrics


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
PROMOTION_GUIDANCE_COLUMNS = (
    "Combinaison", "Cible", "Predictors", "Direction", "Statut promotion",
    "Score promotion",
)

DEFAULT_SENSITIVITY_THRESHOLD_MIN = 0.10
DEFAULT_SENSITIVITY_THRESHOLD_MAX = 0.60
DEFAULT_SENSITIVITY_THRESHOLD_STEP = 0.025
THRESHOLD_SENSITIVITY_BASE_COLUMNS = (
    "Seuil", "Nombre de signaux", "Précision", "Recall", "F1",
    "Rendement directionnel moyen", "Rendement médian",
    "Fréquence mouvement opposé", "MFE moyen", "MAE moyen",
    "Seuil calibré actuel",
)
THRESHOLD_SENSITIVITY_SUMMARY_COLUMNS = (
    "Cible", "Combinaison", "Direction", "Seuil calibré",
    "Meilleur seuil robuste", "Delta seuil",
    "Signaux au seuil calibré", "Signaux au meilleur seuil robuste",
    "Précision au seuil calibré", "Précision au meilleur seuil robuste",
    "Delta précision",
    "Rendement directionnel moyen au seuil calibré",
    "Rendement directionnel moyen au meilleur seuil robuste",
    "Delta rendement",
    "Fréquence mouvement opposé au seuil calibré",
    "Fréquence mouvement opposé au meilleur seuil robuste",
    "Diagnostic",
)
CALIBRATION_SELECTION_SUMMARY_COLUMNS = (
    "Combinaison", "Direction", "Raison sélection calibration",
    "Rang du seuil calibré", "Nombre de candidats admissibles",
)
CALIBRATION_CHOICE_DIAGNOSTIC_COLUMNS = (
    "Seuil", "Sélectionné", "Admissible", "Nombre total de signaux",
    "Fraction de fenêtres admissibles", "Précision calibration",
    "Stabilité précision", "Rendement directionnel moyen",
    "Stabilité rendement", "Fréquence mouvement opposé", "F1",
    "Dans tolérance précision", "Raison de rejet / sélection",
)


def altair_serializable_distribution(distribution: pd.Series) -> pd.Series:
    """Return a display-only copy whose categories are accepted by Altair."""

    serializable = distribution.copy()
    serializable.index = serializable.index.astype(str)
    return serializable


def threshold_sensitivity_best_column(minimum_robust_signals: int) -> str:
    """Return the display-only robust-sample marker column name."""

    return f"Meilleure précision robuste (≥ {minimum_robust_signals} signaux)"


def threshold_sensitivity_columns(minimum_robust_signals: int) -> tuple[str, ...]:
    return (
        *THRESHOLD_SENSITIVITY_BASE_COLUMNS,
        threshold_sensitivity_best_column(minimum_robust_signals),
    )


def threshold_sensitivity_grid(
    minimum: float = DEFAULT_SENSITIVITY_THRESHOLD_MIN,
    maximum: float = DEFAULT_SENSITIVITY_THRESHOLD_MAX,
    step: float = DEFAULT_SENSITIVITY_THRESHOLD_STEP,
) -> tuple[float, ...]:
    """Build the display-only holdout sensitivity grid, including both bounds."""

    minimum, maximum, step = float(minimum), float(maximum), float(step)
    if not 0 <= minimum <= maximum <= 1:
        raise ValueError("Sensitivity thresholds must be between zero and one")
    if step <= 0:
        raise ValueError("Sensitivity threshold step must be positive")
    count = int(np.floor((maximum - minimum) / step))
    values = [round(minimum + index * step, 10) for index in range(count + 1)]
    if not np.isclose(values[-1], maximum):
        values.append(round(maximum, 10))
    return tuple(values)


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


def load_threshold_holdout_predictions(project_root: Path, run_id: str) -> pd.DataFrame:
    """Load immutable holdout probabilities when a run published them."""

    path = Path(project_root) / "runs" / run_id / "results" / "holdout_predictions.csv"
    return pd.read_csv(path) if path.exists() else pd.DataFrame()


def threshold_sensitivity_table(
    holdout_predictions: pd.DataFrame,
    *,
    set_name: str,
    direction: str,
    calibrated_threshold: object,
    thresholds: Sequence[float] | None = None,
    sensitivity_threshold_min: float = DEFAULT_SENSITIVITY_THRESHOLD_MIN,
    sensitivity_threshold_max: float = DEFAULT_SENSITIVITY_THRESHOLD_MAX,
    sensitivity_threshold_step: float = DEFAULT_SENSITIVITY_THRESHOLD_STEP,
    up_target_threshold: float = 0.01,
    down_target_threshold: float = 0.01,
    minimum_robust_signals: int,
) -> pd.DataFrame:
    """Project existing holdout probabilities across display-only thresholds.

    This deliberately does not call calibration or model-fitting code.  It is
    a local projection of the stored probability rows for one combination.
    """

    required = {
        "Set", "Direction", "Probability", "Target", "IntradayReturn",
    }
    columns = threshold_sensitivity_columns(minimum_robust_signals)
    if not required.issubset(holdout_predictions.columns):
        return pd.DataFrame(columns=columns)
    work = holdout_predictions[
        (holdout_predictions["Set"].astype(str) == str(set_name))
        & (holdout_predictions["Direction"].astype(str) == str(direction))
    ].copy()
    if work.empty:
        return pd.DataFrame(columns=columns)
    probability = pd.to_numeric(work["Probability"], errors="coerce")
    target = pd.to_numeric(work["Target"], errors="coerce")
    returns = pd.to_numeric(work["IntradayReturn"], errors="coerce")
    valid = probability.notna() & target.isin([0, 1])
    work = work.loc[valid].copy()
    probability = probability.loc[valid]
    target = target.loc[valid].astype(int)
    returns = returns.loc[valid]
    if work.empty:
        return pd.DataFrame(columns=columns)

    try:
        current = float(calibrated_threshold)
    except (TypeError, ValueError):
        current = None
    values = thresholds if thresholds is not None else threshold_sensitivity_grid(
        sensitivity_threshold_min,
        sensitivity_threshold_max,
        sensitivity_threshold_step,
    )
    grid = {round(float(value), 10) for value in values if 0 <= float(value) <= 1}
    if current is not None and 0 <= current <= 1:
        grid.add(round(current, 10))
    rows: list[dict[str, object]] = []
    for threshold in sorted(grid):
        predicted = (probability >= threshold).astype(int)
        metrics = classification_metrics(target, predicted, probability)
        selected = work.loc[predicted.astype(bool)].copy()
        selected_returns = returns.loc[selected.index].dropna()
        directional_returns = (
            selected_returns if direction == "Up" else -selected_returns
        )
        if direction == "Up":
            opposite = selected_returns <= -down_target_threshold
        else:
            opposite = selected_returns >= up_target_threshold
        mfe = pd.to_numeric(selected.get("MFE", pd.Series(dtype=float)), errors="coerce").dropna()
        mae = pd.to_numeric(selected.get("MAE", pd.Series(dtype=float)), errors="coerce").dropna()
        rows.append({
            "Seuil": threshold,
            "Nombre de signaux": int(predicted.sum()),
            "Précision": metrics.precision,
            "Recall": metrics.recall,
            "F1": metrics.f1,
            "Rendement directionnel moyen": (
                float(directional_returns.mean()) if not directional_returns.empty else np.nan
            ),
            "Rendement médian": (
                float(selected_returns.median()) if not selected_returns.empty else np.nan
            ),
            "Fréquence mouvement opposé": (
                float(opposite.mean()) if not selected_returns.empty else np.nan
            ),
            "MFE moyen": float(mfe.mean()) if not mfe.empty else np.nan,
            "MAE moyen": float(mae.mean()) if not mae.empty else np.nan,
            "Seuil calibré actuel": "✓" if current is not None and np.isclose(threshold, current) else "",
            threshold_sensitivity_best_column(minimum_robust_signals): "",
        })
    result = pd.DataFrame(rows, columns=columns)
    best_column = threshold_sensitivity_best_column(minimum_robust_signals)
    eligible = result[result["Nombre de signaux"] >= minimum_robust_signals]
    if not eligible.empty:
        best = eligible.sort_values(
            ["Précision", "Nombre de signaux", "Seuil"],
            ascending=[False, False, True],
            kind="stable",
        ).index[0]
        result.loc[best, best_column] = "✓"
    return result


def _threshold_sensitivity_summary_values(
    holdout_predictions: pd.DataFrame,
    *,
    set_name: str,
    direction: str,
    calibrated_threshold: object,
    up_target_threshold: float,
    down_target_threshold: float,
    minimum_robust_signals: int,
    sensitivity_threshold_min: float,
    sensitivity_threshold_max: float,
    sensitivity_threshold_step: float,
) -> dict[str, object]:
    """Compute only the sensitivity values used by the aggregate view.

    The detailed table still uses :func:`threshold_sensitivity_table`.  This
    compact projection avoids constructing a full DataFrame and classification
    metric object for every threshold of every visible combination.
    """

    missing = {
        "Seuil calibré": np.nan,
        "Meilleur seuil robuste": np.nan,
        "Delta seuil": np.nan,
        "Signaux au seuil calibré": np.nan,
        "Signaux au meilleur seuil robuste": np.nan,
        "Précision au seuil calibré": np.nan,
        "Précision au meilleur seuil robuste": np.nan,
        "Delta précision": np.nan,
        "Rendement directionnel moyen au seuil calibré": np.nan,
        "Rendement directionnel moyen au meilleur seuil robuste": np.nan,
        "Delta rendement": np.nan,
        "Fréquence mouvement opposé au seuil calibré": np.nan,
        "Fréquence mouvement opposé au meilleur seuil robuste": np.nan,
        "Diagnostic": "unstable",
    }
    required = {"Set", "Direction", "Probability", "Target", "IntradayReturn"}
    if not required.issubset(holdout_predictions.columns):
        return missing
    work = holdout_predictions[
        (holdout_predictions["Set"].astype(str) == set_name)
        & (holdout_predictions["Direction"].astype(str) == direction)
    ]
    probability = pd.to_numeric(work["Probability"], errors="coerce")
    target = pd.to_numeric(work["Target"], errors="coerce")
    returns = pd.to_numeric(work["IntradayReturn"], errors="coerce")
    valid = probability.notna() & target.isin([0, 1])
    if not valid.any():
        return missing
    probability_values = probability.loc[valid].to_numpy(dtype=float)
    target_values = target.loc[valid].to_numpy(dtype=int)
    return_values = returns.loc[valid].to_numpy(dtype=float)
    try:
        current = float(calibrated_threshold)
    except (TypeError, ValueError):
        current = None
    grid = {
        round(float(value), 10)
        for value in threshold_sensitivity_grid(
            sensitivity_threshold_min,
            sensitivity_threshold_max,
            sensitivity_threshold_step,
        )
        if 0 <= float(value) <= 1
    }
    if current is not None and 0 <= current <= 1:
        grid.add(round(current, 10))
    thresholds = np.asarray(sorted(grid), dtype=float)
    selected = probability_values[None, :] >= thresholds[:, None]
    signal_counts = selected.sum(axis=1)
    true_positives = (selected & (target_values[None, :] == 1)).sum(axis=1)
    precision = np.divide(
        true_positives,
        signal_counts,
        out=np.zeros(len(thresholds), dtype=float),
        where=signal_counts != 0,
    )
    valid_returns = ~np.isnan(return_values)
    selected_returns = selected & valid_returns[None, :]
    return_counts = selected_returns.sum(axis=1)
    directional_values = return_values if direction == "Up" else -return_values
    directional_mean = np.divide(
        np.where(selected_returns, directional_values[None, :], 0.0).sum(axis=1),
        return_counts,
        out=np.full(len(thresholds), np.nan),
        where=return_counts != 0,
    )
    if direction == "Up":
        opposite_values = return_values <= -down_target_threshold
    else:
        opposite_values = return_values >= up_target_threshold
    opposite_frequency = np.divide(
        (selected_returns & opposite_values[None, :]).sum(axis=1),
        return_counts,
        out=np.full(len(thresholds), np.nan),
        where=return_counts != 0,
    )
    calibrated_indices = (
        np.flatnonzero(np.isclose(thresholds, current)) if current is not None else []
    )
    calibrated_index = int(calibrated_indices[0]) if len(calibrated_indices) else None
    eligible = np.flatnonzero(signal_counts >= minimum_robust_signals)
    robust_index = (
        min(eligible, key=lambda index: (-precision[index], -signal_counts[index], thresholds[index]))
        if len(eligible) else None
    )
    if calibrated_index is None and robust_index is None:
        return missing

    def indexed(values: np.ndarray, index: int | None) -> float:
        return float(values[index]) if index is not None else np.nan

    calibrated_threshold_value = indexed(thresholds, calibrated_index)
    robust_threshold_value = indexed(thresholds, robust_index)
    delta_threshold = robust_threshold_value - calibrated_threshold_value
    if robust_index is None or signal_counts[robust_index] < minimum_robust_signals:
        diagnostic = "unstable"
    elif abs(delta_threshold) <= sensitivity_threshold_step / 2:
        diagnostic = "near_optimal"
    elif delta_threshold > 0:
        diagnostic = "higher_threshold_better"
    else:
        diagnostic = "lower_threshold_better"
    calibrated_precision = indexed(precision, calibrated_index)
    robust_precision = indexed(precision, robust_index)
    calibrated_return = indexed(directional_mean, calibrated_index)
    robust_return = indexed(directional_mean, robust_index)
    return {
        "Seuil calibré": calibrated_threshold_value,
        "Meilleur seuil robuste": robust_threshold_value,
        "Delta seuil": delta_threshold,
        "Signaux au seuil calibré": indexed(signal_counts, calibrated_index),
        "Signaux au meilleur seuil robuste": indexed(signal_counts, robust_index),
        "Précision au seuil calibré": calibrated_precision,
        "Précision au meilleur seuil robuste": robust_precision,
        "Delta précision": robust_precision - calibrated_precision,
        "Rendement directionnel moyen au seuil calibré": calibrated_return,
        "Rendement directionnel moyen au meilleur seuil robuste": robust_return,
        "Delta rendement": robust_return - calibrated_return,
        "Fréquence mouvement opposé au seuil calibré": indexed(opposite_frequency, calibrated_index),
        "Fréquence mouvement opposé au meilleur seuil robuste": indexed(opposite_frequency, robust_index),
        "Diagnostic": diagnostic,
    }


def threshold_sensitivity_summary(
    visible_results: pd.DataFrame,
    holdout_predictions: pd.DataFrame,
    *,
    up_target_threshold: float = 0.01,
    down_target_threshold: float = 0.01,
    minimum_robust_signals: int,
    sensitivity_threshold_min: float = DEFAULT_SENSITIVITY_THRESHOLD_MIN,
    sensitivity_threshold_max: float = DEFAULT_SENSITIVITY_THRESHOLD_MAX,
    sensitivity_threshold_step: float = DEFAULT_SENSITIVITY_THRESHOLD_STEP,
) -> pd.DataFrame:
    """Summarize the read-only sensitivity projection for visible result rows."""

    required = {"Cible", "Combinaison", "Direction", "Seuil calibré"}
    if visible_results.empty or not required.issubset(visible_results.columns):
        return pd.DataFrame(columns=THRESHOLD_SENSITIVITY_SUMMARY_COLUMNS)
    rows: list[dict[str, object]] = []
    visible = visible_results.drop_duplicates(["Combinaison", "Direction"])
    prediction_groups: dict[tuple[str, str], pd.DataFrame] = {}
    if {"Set", "Direction"}.issubset(holdout_predictions.columns):
        grouped = holdout_predictions.assign(
            _sensitivity_set=holdout_predictions["Set"].astype(str),
            _sensitivity_direction=holdout_predictions["Direction"].astype(str),
        ).groupby(["_sensitivity_set", "_sensitivity_direction"], sort=False)
        prediction_groups = {
            (set_name, direction): group
            for (set_name, direction), group in grouped
        }
    empty_predictions = holdout_predictions.iloc[0:0]
    for _, visible_row in visible.iterrows():
        set_name = str(visible_row["Combinaison"])
        direction = str(visible_row["Direction"])
        values = _threshold_sensitivity_summary_values(
            prediction_groups.get((set_name, direction), empty_predictions),
            set_name=set_name,
            direction=direction,
            calibrated_threshold=visible_row["Seuil calibré"],
            up_target_threshold=up_target_threshold,
            down_target_threshold=down_target_threshold,
            minimum_robust_signals=minimum_robust_signals,
            sensitivity_threshold_min=sensitivity_threshold_min,
            sensitivity_threshold_max=sensitivity_threshold_max,
            sensitivity_threshold_step=sensitivity_threshold_step,
        )
        rows.append({
            "Cible": visible_row["Cible"],
            "Combinaison": visible_row["Combinaison"],
            "Direction": visible_row["Direction"],
            **values,
        })
    return pd.DataFrame(rows, columns=THRESHOLD_SENSITIVITY_SUMMARY_COLUMNS)


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


def _promotion_scale(value: object, minimum: float, maximum: float) -> float:
    """Normalize one existing metric onto [0, 1] with explicit clipping."""

    numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    if pd.isna(numeric):
        return 0.0
    return float(np.clip((float(numeric) - minimum) / (maximum - minimum), 0.0, 1.0))


def _promotion_opposite_scale(value: object) -> float:
    """Map 10% opposite moves to 1 and 30% to 0, with clipping."""

    numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    if pd.isna(numeric):
        return 0.0
    return float(np.clip((0.30 - float(numeric)) / 0.20, 0.0, 1.0))


def _promotion_signal_scale(value: object) -> float:
    """Map 20 holdout signals to 0.5 and 50 signals to 1.0."""

    numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    if pd.isna(numeric):
        return 0.0
    return float(np.clip(0.5 + (float(numeric) - 20.0) / 60.0, 0.0, 1.0))


def _promotion_sensitivity_scale(diagnostic: object) -> float:
    return {
        "near_optimal": 1.0,
        "higher_threshold_better": 0.65,
        "lower_threshold_better": 0.65,
        "unstable": 0.20,
    }.get(str(diagnostic), 0.50)


def _promotion_blockers(
    row: pd.Series, selected_by_set: Mapping[str, Any]
) -> list[str]:
    set_name = str(row.get("Combinaison", ""))
    direction = str(row.get("Direction", ""))
    selection = _selection_for(selected_by_set, set_name, direction)
    threshold = pd.to_numeric(pd.Series([row.get("Seuil calibré")]), errors="coerce").iloc[0]
    valid_threshold = not pd.isna(threshold) and (
        not selection or str(selection.get("status", "")).lower() == "selected"
    )
    blockers: list[str] = []
    if not valid_threshold:
        blockers.append("Seuil calibré non admissible")
    checks = (
        ("Signaux holdout", 20, "Trop peu de signaux"),
        ("AUC holdout", 0.55, "AUC insuffisante"),
        ("Rendement directionnel moyen", 0.0, "Rendement négatif"),
    )
    for column, limit, reason in checks:
        value = pd.to_numeric(pd.Series([row.get(column)]), errors="coerce").iloc[0]
        if pd.isna(value):
            blockers.append("Métriques holdout incomplètes")
        elif (column == "Rendement directionnel moyen" and value <= limit) or (
            column != "Rendement directionnel moyen" and value < limit
        ):
            blockers.append(reason)
    opposite = pd.to_numeric(
        pd.Series([row.get("Fréquence mouvement opposé")]), errors="coerce"
    ).iloc[0]
    if pd.isna(opposite):
        blockers.append("Métriques holdout incomplètes")
    elif opposite > 0.30:
        blockers.append("Mouvement opposé trop fréquent")
    return list(dict.fromkeys(blockers))


def threshold_promotion_guidance(
    results: pd.DataFrame,
    sensitivity_summary: pd.DataFrame,
    selected_by_set: Mapping[str, Any] | None = None,
) -> pd.DataFrame:
    """Add read-only, manual-promotion guidance from holdout and sensitivity data."""

    if results.empty:
        return results.copy()
    selected = selected_by_set or {}
    sensitivity = sensitivity_summary.copy()
    if {"Combinaison", "Direction", "Diagnostic"}.issubset(sensitivity.columns):
        sensitivity = sensitivity.loc[:, ["Combinaison", "Direction", "Diagnostic"]]
        table = results.merge(sensitivity, on=["Combinaison", "Direction"], how="left")
    else:
        table = results.copy()
        table["Diagnostic"] = pd.NA

    statuses: list[str] = []
    scores: list[float | None] = []
    reasons: list[str] = []
    for _, row in table.iterrows():
        blockers = _promotion_blockers(row, selected)
        if blockers:
            statuses.append("Non candidat")
            scores.append(None)
            reasons.append(" · ".join(blockers[:2]))
            continue
        auc = _promotion_scale(row.get("AUC holdout"), 0.55, 0.70)
        precision = _promotion_scale(row.get("Précision holdout"), 0.40, 0.65)
        directional_return = _promotion_scale(
            row.get("Rendement directionnel moyen"), 0.0, 0.015
        )
        opposite = _promotion_opposite_scale(row.get("Fréquence mouvement opposé"))
        signals = _promotion_signal_scale(row.get("Signaux holdout"))
        sensitivity_score = _promotion_sensitivity_scale(row.get("Diagnostic"))
        score = round(100 * (
            0.25 * auc + 0.20 * precision + 0.20 * directional_return
            + 0.15 * opposite + 0.10 * signals + 0.10 * sensitivity_score
        ), 1)
        scores.append(score)
        status = "Candidat fort" if score >= 75 else "À examiner" if score >= 60 else "Non candidat"
        statuses.append(status)
        reason_parts = [
            "AUC forte" if auc >= 0.50 else "AUC limite",
            "rendement positif",
            "seuil stable" if str(row.get("Diagnostic")) == "near_optimal" else "sensibilité à examiner",
        ]
        if signals < 0.50:
            reason_parts.append("volume limité")
        reasons.append(" · ".join(reason_parts))

    table["Statut promotion"] = statuses
    table["Score promotion"] = scores
    table["Raison promotion"] = reasons
    ordered = [
        *PROMOTION_GUIDANCE_COLUMNS,
        *[column for column in table.columns if column not in {
            *PROMOTION_GUIDANCE_COLUMNS, "Diagnostic", "Raison promotion",
        }],
        "Raison promotion",
    ]
    return table.loc[:, ordered]


def threshold_calibration_selection_summary(
    visible_results: pd.DataFrame, metrics_by_set: pd.DataFrame
) -> pd.DataFrame:
    """Summarize persisted calibration-selection diagnostics for visible rows."""

    required = {"Combinaison", "Direction"}
    if visible_results.empty or not required.issubset(visible_results.columns):
        return pd.DataFrame(columns=CALIBRATION_SELECTION_SUMMARY_COLUMNS)
    if metrics_by_set.empty or not {"Set", "Direction"}.issubset(metrics_by_set):
        return pd.DataFrame(columns=CALIBRATION_SELECTION_SUMMARY_COLUMNS)
    rows: list[dict[str, object]] = []
    for _, visible in visible_results.drop_duplicates(["Combinaison", "Direction"]).iterrows():
        candidates = metrics_by_set[
            (metrics_by_set["Set"].astype(str) == str(visible["Combinaison"]))
            & (metrics_by_set["Direction"].astype(str) == str(visible["Direction"]))
        ]
        selected = candidates[_boolean(candidates.get(
            "Selected", pd.Series(False, index=candidates.index)
        ))]
        selected_row = selected.iloc[0] if not selected.empty else None
        reason = None if selected_row is None else selected_row.get("SelectionReason")
        if pd.isna(reason):
            reason = "—"
        rank = None if selected_row is None else selected_row.get("SelectionRank")
        rows.append({
            "Combinaison": visible["Combinaison"],
            "Direction": visible["Direction"],
            "Raison sélection calibration": reason,
            "Rang du seuil calibré": rank,
            "Nombre de candidats admissibles": int(_boolean(candidates.get(
                "Eligible", pd.Series(False, index=candidates.index)
            )).sum()),
        })
    return pd.DataFrame(rows, columns=CALIBRATION_SELECTION_SUMMARY_COLUMNS)


def threshold_calibration_choice_diagnostic_table(
    metrics_by_set: pd.DataFrame, *, set_name: str, direction: str
) -> pd.DataFrame:
    """Project persisted candidate diagnostics for one calibration choice."""

    if metrics_by_set.empty or not {"Set", "Direction"}.issubset(metrics_by_set):
        return pd.DataFrame(columns=CALIBRATION_CHOICE_DIAGNOSTIC_COLUMNS)
    candidates = metrics_by_set[
        (metrics_by_set["Set"].astype(str) == str(set_name))
        & (metrics_by_set["Direction"].astype(str) == str(direction))
    ].copy()
    if candidates.empty:
        return pd.DataFrame(columns=CALIBRATION_CHOICE_DIAGNOSTIC_COLUMNS)
    selected = _boolean(candidates.get("Selected", pd.Series(False, index=candidates.index)))
    reason = candidates.get("SelectionReason", pd.Series(pd.NA, index=candidates.index))
    reason = reason.where(reason.notna(), candidates.get(
        "RejectionReason", pd.Series(pd.NA, index=candidates.index)
    )).fillna("—")
    table = pd.DataFrame({
        "Seuil": candidates.get("Threshold"),
        "Sélectionné": selected,
        "Admissible": _boolean(candidates.get("Eligible", pd.Series(False, index=candidates.index))),
        "Nombre total de signaux": candidates.get("TotalSignals"),
        "Fraction de fenêtres admissibles": candidates.get("EligibleWindowFraction"),
        "Précision calibration": candidates.get("Precision"),
        "Stabilité précision": candidates.get("PrecisionStd"),
        "Rendement directionnel moyen": candidates.get("DirectionalReturnMean"),
        "Stabilité rendement": candidates.get("DirectionalReturnMeanStd"),
        "Fréquence mouvement opposé": candidates.get("OppositeMoveFrequency"),
        "F1": candidates.get("F1Median"),
        "Dans tolérance précision": _boolean(candidates.get(
            "WithinPrecisionTolerance", pd.Series(False, index=candidates.index)
        )),
        "Raison de rejet / sélection": reason,
    })
    return table.sort_values("Seuil", kind="stable").reset_index(drop=True)


def filter_threshold_calibration_results(
    results: pd.DataFrame,
    *,
    direction: str = "Up",
    min_signals: int = 0,
    min_precision: float | None = None,
    min_holdout_auc: float | None = None,
    max_opposite_move_frequency: float | None = None,
    min_directional_return: float | None = None,
    promotion_status: str = "Tous",
    sort_by: str = "Précision holdout",
) -> pd.DataFrame:
    """Apply display-only threshold result filters with deterministic sorting."""

    table = results.copy()
    if direction in {"Up", "Down"} and "Direction" in table:
        table = table[table["Direction"].astype(str) == direction]
    if promotion_status in {"Candidat fort", "À examiner", "Non candidat"}:
        table = table[table.get("Statut promotion", pd.Series("", index=table.index)) == promotion_status]
    if "Signaux holdout" in table:
        table = table[
            pd.to_numeric(table["Signaux holdout"], errors="coerce").fillna(0)
            >= min_signals
        ]
    criteria = (
        ("Précision holdout", min_precision, lambda values, limit: values >= limit),
        ("AUC holdout", min_holdout_auc, lambda values, limit: values >= limit),
        (
            "Fréquence mouvement opposé", max_opposite_move_frequency,
            lambda values, limit: values <= limit,
        ),
        (
            "Rendement directionnel moyen", min_directional_return,
            lambda values, limit: values >= limit,
        ),
    )
    for column, limit, predicate in criteria:
        if limit is not None and column in table:
            table = table[predicate(pd.to_numeric(table[column], errors="coerce"), limit)]
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


def comparison_display_table(table: pd.DataFrame) -> pd.DataFrame:
    """Return an Arrow-safe, human-readable copy of the comparison matrix."""

    def display_value(value: object) -> str:
        if value is None or pd.isna(value):
            return "—"
        return str(value)

    return table.map(display_value).astype("string")


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
