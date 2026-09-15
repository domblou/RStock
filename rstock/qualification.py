"""Stable, pre-holdout qualification of walk-forward model combinations."""

from __future__ import annotations

import json

import numpy as np
import pandas as pd

from .config import RStockConfig
from .evaluation import classification_metrics


def _validate_criteria(config: RStockConfig) -> None:
    if config.qualification_min_windows < 1:
        raise ValueError("qualification_min_windows must be positive")
    if config.qualification_min_positive_observations < 0:
        raise ValueError("qualification_min_positive_observations cannot be negative")
    bounded = {
        "qualification_min_median_auc": config.qualification_min_median_auc,
        "qualification_min_pct_windows_above_random": (
            config.qualification_min_pct_windows_above_random
        ),
        "qualification_min_worst_window_auc": (
            config.qualification_min_worst_window_auc
        ),
        "final_confirmation_min_auc": config.final_confirmation_min_auc,
    }
    for name, value in bounded.items():
        if not 0.0 <= value <= 1.0:
            raise ValueError(f"{name} must be between zero and one")
    if config.qualification_max_auc_std < 0:
        raise ValueError("qualification_max_auc_std cannot be negative")


def qualification_parameters(config: RStockConfig) -> dict[str, int | float]:
    """Return the predeclared calibration criteria in a serializable form."""

    _validate_criteria(config)
    return {
        "min_windows": config.qualification_min_windows,
        "min_median_auc": config.qualification_min_median_auc,
        "min_pct_windows_above_random": (
            config.qualification_min_pct_windows_above_random
        ),
        "min_worst_window_auc": config.qualification_min_worst_window_auc,
        "min_positive_observations": (
            config.qualification_min_positive_observations
        ),
        "max_auc_std": config.qualification_max_auc_std,
        "final_confirmation_min_auc": config.final_confirmation_min_auc,
    }


def qualify_combinations(
    windows: pd.DataFrame,
    predictions: pd.DataFrame,
    config: RStockConfig,
) -> pd.DataFrame:
    """Aggregate temporal stability and rank only combinations that qualify."""

    _validate_criteria(config)
    records: list[dict[str, object]] = []
    for set_name, window_group in windows.groupby("Set", sort=False):
        prediction_group = predictions[predictions["Set"] == set_name]
        auc = pd.to_numeric(window_group["UpROCAUC"], errors="coerce")
        pr_auc = pd.to_numeric(window_group["UpPRAUC"], errors="coerce")
        valid_auc = auc.dropna()
        aggregate = classification_metrics(
            prediction_group["UpTarget"],
            prediction_group["UpPrediction"],
            prediction_group["UpProbability"],
        )
        windows_evaluated = int(window_group["Window"].nunique())
        auc_windows = int(valid_auc.size)
        median_auc = float(valid_auc.median()) if auc_windows else np.nan
        mean_auc = float(valid_auc.mean()) if auc_windows else np.nan
        auc_std = float(valid_auc.std(ddof=0)) if auc_windows else np.nan
        worst_auc = float(valid_auc.min()) if auc_windows else np.nan
        pct_above_random = float((auc > 0.5).sum() / windows_evaluated)
        positive_observations = int(prediction_group["UpTarget"].sum())

        failures: list[str] = []
        if windows_evaluated < config.qualification_min_windows:
            failures.append("insufficient_windows")
        if auc_windows < config.qualification_min_windows:
            failures.append("insufficient_auc_windows")
        if not np.isfinite(median_auc) or median_auc < config.qualification_min_median_auc:
            failures.append("median_auc")
        if pct_above_random < config.qualification_min_pct_windows_above_random:
            failures.append("windows_above_random")
        if not np.isfinite(worst_auc) or worst_auc < config.qualification_min_worst_window_auc:
            failures.append("worst_window_auc")
        if positive_observations < config.qualification_min_positive_observations:
            failures.append("positive_observations")
        if not np.isfinite(auc_std) or auc_std > config.qualification_max_auc_std:
            failures.append("auc_std")

        records.append(
            {
                "Set": set_name,
                "Observation": window_group.iloc[0]["Observation"],
                "Predictors": window_group.iloc[0]["Predictors"],
                "WindowsEvaluated": windows_evaluated,
                "AUCWindows": auc_windows,
                "ROCAUCMedian": median_auc,
                "ROCAUCMean": mean_auc,
                "ROCAUCStd": auc_std,
                "ROCAUCWorst": worst_auc,
                "PctWindowsAboveRandom": pct_above_random,
                "PRAUCMedian": float(pr_auc.median()) if pr_auc.notna().any() else np.nan,
                "MeanPrevalence": float(window_group["UpPrevalence"].mean()),
                "TotalObservations": len(prediction_group),
                "PositiveObservations": positive_observations,
                "RowsLostToLags": int(
                    window_group.iloc[0].get("RowsLostToLags", 0)
                ),
                "AggregatePrecision": aggregate.precision,
                "AggregateRecall": aggregate.recall,
                "AggregateF1": aggregate.f1,
                "Eligible": not failures,
                "IneligibilityReasons": json.dumps(failures, separators=(",", ":")),
            }
        )

    return rank_qualified_combinations(pd.DataFrame(records))


def rank_qualified_combinations(qualified: pd.DataFrame) -> pd.DataFrame:
    """Apply the historical deterministic rank to precomputed qualification rows."""

    qualified = qualified.copy()
    qualified["EligibleRank"] = pd.Series(pd.NA, index=qualified.index, dtype="Int64")
    eligible = qualified[qualified["Eligible"]].sort_values(
        [
            "PctWindowsAboveRandom",
            "ROCAUCMedian",
            "ROCAUCWorst",
            "ROCAUCStd",
            "PRAUCMedian",
            "Set",
        ],
        ascending=[False, False, False, True, False, True],
        kind="stable",
    )
    qualified.loc[eligible.index, "EligibleRank"] = np.arange(1, len(eligible) + 1)
    return qualified.sort_values(
        ["Eligible", "EligibleRank", "Set"],
        ascending=[False, True, True],
        na_position="last",
        kind="stable",
    ).reset_index(drop=True)
