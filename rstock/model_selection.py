"""Explainable final ranking for already-qualified walk-forward models."""

from __future__ import annotations

import json
from collections.abc import Mapping

import numpy as np
import pandas as pd

from .config import RStockConfig


SCORE_COLUMNS = (
    "predictive_quality_score",
    "stability_score",
    "holdout_score",
    "signal_quality_score",
    "sample_adequacy_score",
    "model_selection_score",
    "model_selection_rank",
)


def model_selection_parameters(config: RStockConfig) -> dict[str, object]:
    """Return the transparent scoring recipe persisted with each run."""

    weights = {
        "predictive_quality_score": config.model_selection_predictive_quality_weight,
        "stability_score": config.model_selection_stability_weight,
        "holdout_score": config.model_selection_holdout_weight,
        "signal_quality_score": config.model_selection_signal_quality_weight,
        "sample_adequacy_score": config.model_selection_sample_adequacy_weight,
    }
    if any(value < 0 for value in weights.values()):
        raise ValueError("Model-selection weights cannot be negative")
    if not any(value > 0 for value in weights.values()):
        raise ValueError("At least one model-selection weight must be positive")
    return {
        "scale": "0-100",
        "weights": weights,
        "missing_component_policy": "exclude_and_renormalize_available_weights",
        "components": {
            "predictive_quality_score": ["ROCAUCMedian"],
            "stability_score": [
                "ROCAUCWorst", "ROCAUCStd", "PctWindowsAboveRandom"
            ],
            "holdout_score": [
                "FinalUpROCAUC", "abs(ROCAUCMedian - FinalUpROCAUC)"
            ],
            "signal_quality_score": [
                "success_rate", "window_coverage", "total_signals",
                "precision_stability", "return_stability", "mean_return",
            ],
            "sample_adequacy_score": [
                "WindowsEvaluated", "AUCWindows / WindowsEvaluated",
                "PositiveObservations",
            ],
        },
        "auc_normalization": "clip((auc - 0.50) / 0.50)",
        "dispersion_normalization": (
            "clip(1 - auc_std / qualification_max_auc_std)"
        ),
        "holdout_gap_normalization": (
            "clip(1 - abs(dev_auc - holdout_auc) / qualification_max_auc_std)"
        ),
        "sample_target": "2 x qualification minimum",
    }


def _number(row: pd.Series, name: str) -> float:
    value = pd.to_numeric(pd.Series([row.get(name)]), errors="coerce").iloc[0]
    return float(value) if pd.notna(value) else np.nan


def _clip_score(value: float) -> float:
    return float(np.clip(value, 0.0, 1.0) * 100.0)


def _auc_score(value: float) -> float:
    return np.nan if not np.isfinite(value) else _clip_score((value - 0.5) / 0.5)


def _mean_available(values: list[float]) -> float:
    finite = [value for value in values if np.isfinite(value)]
    return float(np.mean(finite)) if finite else np.nan


def _calibration_metrics(row: pd.Series) -> Mapping[str, object]:
    value = row.get("calibration_metrics")
    if isinstance(value, Mapping):
        return value
    if isinstance(value, str) and value.strip():
        try:
            parsed = json.loads(value)
        except json.JSONDecodeError:
            return {}
        return parsed if isinstance(parsed, Mapping) else {}
    return {}


def _signal_value(row: pd.Series, *names: str) -> float:
    metrics = _calibration_metrics(row)
    for name in names:
        direct = _number(row, name)
        if np.isfinite(direct):
            return direct
        nested = pd.to_numeric(pd.Series([metrics.get(name)]), errors="coerce").iloc[0]
        if pd.notna(nested):
            return float(nested)
    return np.nan


def _component_scores(row: pd.Series, config: RStockConfig) -> dict[str, float]:
    median_auc = _number(row, "ROCAUCMedian")
    worst_auc = _number(row, "ROCAUCWorst")
    auc_std = _number(row, "ROCAUCStd")
    pct_above = _number(row, "PctWindowsAboveRandom")
    holdout_auc = _number(row, "FinalUpROCAUC")

    predictive = _auc_score(median_auc)
    dispersion_limit = max(float(config.qualification_max_auc_std), np.finfo(float).eps)
    stability = _mean_available([
        _auc_score(worst_auc),
        np.nan if not np.isfinite(auc_std) else _clip_score(1.0 - auc_std / dispersion_limit),
        np.nan if not np.isfinite(pct_above) else _clip_score(pct_above),
    ])
    holdout = _mean_available([
        _auc_score(holdout_auc),
        (
            np.nan
            if not np.isfinite(median_auc) or not np.isfinite(holdout_auc)
            else _clip_score(1.0 - abs(median_auc - holdout_auc) / dispersion_limit)
        ),
    ])

    windows = _number(row, "WindowsEvaluated")
    auc_windows = _number(row, "AUCWindows")
    positives = _number(row, "PositiveObservations")
    window_target = max(2 * config.qualification_min_windows, 1)
    positive_target = max(2 * config.qualification_min_positive_observations, 1)
    sample = _mean_available([
        np.nan if not np.isfinite(windows) else _clip_score(windows / window_target),
        (
            np.nan
            if not np.isfinite(windows) or windows <= 0 or not np.isfinite(auc_windows)
            else _clip_score(auc_windows / windows)
        ),
        np.nan if not np.isfinite(positives) else _clip_score(positives / positive_target),
    ])

    success = _signal_value(row, "signal_success_rate", "success_rate")
    coverage = _signal_value(row, "signal_window_coverage", "window_coverage")
    total_signals = _signal_value(row, "signal_total_signals", "total_signals")
    precision_std = _signal_value(row, "signal_precision_stability", "precision_stability")
    return_std = _signal_value(row, "signal_return_stability", "return_stability")
    mean_return = _signal_value(row, "signal_mean_return", "mean_return")
    signal_target = max(
        config.threshold_calibration_min_signals_per_window
        * config.qualification_min_windows,
        1,
    )
    signal_parts = [
        np.nan if not np.isfinite(success) else _clip_score((success - 0.5) / 0.5),
        np.nan if not np.isfinite(coverage) else _clip_score(coverage),
        np.nan if not np.isfinite(total_signals) else _clip_score(total_signals / signal_target),
        np.nan if not np.isfinite(precision_std) else _clip_score(1.0 - 2.0 * precision_std),
        (
            np.nan
            if not np.isfinite(return_std)
            else _clip_score(1.0 - abs(return_std) / max(config.intraday_target_threshold, np.finfo(float).eps))
        ),
        (
            np.nan
            if not np.isfinite(mean_return)
            else _clip_score(mean_return / max(config.intraday_target_threshold, np.finfo(float).eps))
        ),
    ]
    return {
        "predictive_quality_score": predictive,
        "stability_score": stability,
        "holdout_score": holdout,
        "signal_quality_score": _mean_available(signal_parts),
        "sample_adequacy_score": sample,
    }


def score_qualified_models(
    selection_results: pd.DataFrame, config: RStockConfig
) -> pd.DataFrame:
    """Score and rank eligible rows without changing their qualification verdict."""

    parameters = model_selection_parameters(config)
    weights = parameters["weights"]
    result = selection_results.copy()
    for name in SCORE_COLUMNS:
        result[name] = np.nan
    if result.empty:
        result["model_selection_rank"] = pd.Series(dtype="Int64")
        return result

    eligible = result.get("Eligible", pd.Series(False, index=result.index)).map(
        lambda value: value is True or str(value).strip().lower() in {"true", "1", "yes"}
    )
    for index in result.index[eligible]:
        components = _component_scores(result.loc[index], config)
        for name, value in components.items():
            result.at[index, name] = value
        available = {
            name: value
            for name, value in components.items()
            if np.isfinite(value) and weights[name] > 0
        }
        available_weight = sum(weights[name] for name in available)
        result.at[index, "model_selection_score"] = (
            sum(available[name] * weights[name] for name in available) / available_weight
            if available_weight
            else np.nan
        )

    result["model_selection_rank"] = pd.Series(pd.NA, index=result.index, dtype="Int64")
    ranked = result.loc[eligible & result["model_selection_score"].notna()].sort_values(
        ["model_selection_score", "Set"],
        ascending=[False, True],
        kind="stable",
    )
    result.loc[ranked.index, "model_selection_rank"] = np.arange(1, len(ranked) + 1)
    return result
