"""Transparent selection of stable, non-redundant univariate predictors."""

from __future__ import annotations

import json
from dataclasses import dataclass
from math import comb

import numpy as np
import pandas as pd

from .config import RStockConfig
from .features import intraday_lag_column


PREFILTER_SCORE_FORMULA = (
    "ROCAUCMedian + PctWindowsAboveRandom - ROCAUCStd "
    "- max(0, 0.50 - ROCAUCWorst)"
)


@dataclass(frozen=True, slots=True)
class PredictorPrefilterResult:
    predictors_by_target: dict[str, tuple[str, ...]]
    metrics: pd.DataFrame
    diagnostics: list[dict[str, object]]


def _predictor(value: object) -> str:
    parsed = json.loads(str(value))
    if not isinstance(parsed, list) or len(parsed) != 1:
        raise ValueError("Predictor prefilter requires univariate sets")
    return str(parsed[0])


def _score(row: pd.Series) -> float:
    median = float(row["ROCAUCMedian"])
    pct = float(row["PctWindowsAboveRandom"])
    std = float(row["ROCAUCStd"])
    worst = float(row["ROCAUCWorst"])
    return median + pct - std - max(0.0, 0.50 - worst)


def _feature_correlation(
    prepared: pd.DataFrame,
    left: str,
    right: str,
    lag_depth: int,
) -> float | None:
    left_names = [intraday_lag_column(left, lag) for lag in range(1, lag_depth + 1)]
    right_names = [intraday_lag_column(right, lag) for lag in range(1, lag_depth + 1)]
    if not set((*left_names, *right_names)) <= set(prepared.columns):
        return None
    paired = prepared[[*left_names, *right_names]].dropna()
    if len(paired) < 2:
        return None
    left_values = paired[left_names].to_numpy(dtype=float).ravel()
    right_values = paired[right_names].to_numpy(dtype=float).ravel()
    if np.std(left_values) == 0 or np.std(right_values) == 0:
        return None
    correlation = float(np.corrcoef(left_values, right_values)[0, 1])
    return correlation if np.isfinite(correlation) else None


def _combination_count(candidate_count: int, depth: int) -> int:
    return sum(comb(candidate_count, size) for size in range(1, min(
        candidate_count, depth
    ) + 1))


def _threshold_rejection_counts(
    candidates: pd.DataFrame, config: RStockConfig
) -> dict[str, int]:
    """Count each univariate threshold failure without making them exclusive."""

    median = pd.to_numeric(candidates["ROCAUCMedian"], errors="coerce")
    pct_above_random = pd.to_numeric(
        candidates["PctWindowsAboveRandom"], errors="coerce"
    )
    worst = pd.to_numeric(candidates["ROCAUCWorst"], errors="coerce")
    auc_std = pd.to_numeric(candidates["ROCAUCStd"], errors="coerce")
    return {
        "rejected_median_auc": int(
            ((~np.isfinite(median)) | (median < config.predictor_prefilter_min_median_auc)).sum()
        ),
        "rejected_pct_above_random": int(
            (
                (~np.isfinite(pct_above_random))
                | (pct_above_random < config.predictor_prefilter_min_pct_above_random)
            ).sum()
        ),
        "rejected_worst_auc": int(
            ((~np.isfinite(worst)) | (worst < config.predictor_prefilter_min_worst_auc)).sum()
        ),
        "rejected_auc_std": int(
            ((~np.isfinite(auc_std)) | (auc_std > config.predictor_prefilter_max_auc_std)).sum()
        ),
    }


def select_predictors(
    qualification: pd.DataFrame,
    prepared_development: pd.DataFrame,
    *,
    targets: list[str],
    candidate_symbols: list[str],
    config: RStockConfig,
) -> PredictorPrefilterResult:
    """Rank qualified univariate predictors, cap them, then remove redundancy."""

    if config.predictor_prefilter_top_n < 1:
        raise ValueError("predictor_prefilter_top_n must be positive")
    threshold = config.predictor_prefilter_correlation_threshold
    if not 0.0 <= threshold <= 1.0:
        raise ValueError("predictor_prefilter_correlation_threshold must be between zero and one")

    metrics = qualification.copy()
    metrics["Predictor"] = metrics["Predictors"].map(_predictor)
    metrics["PrefilterScore"] = metrics.apply(_score, axis=1)
    metrics["PrefilterStatus"] = "rejected_threshold"
    metrics["RedundantWith"] = pd.NA
    metrics["RedundancyCorrelation"] = np.nan
    retained_by_target: dict[str, tuple[str, ...]] = {}
    diagnostics: list[dict[str, object]] = []

    for target in targets:
        initial = [symbol for symbol in candidate_symbols if symbol != target]
        target_rows = metrics[
            (metrics["Observation"] == target)
            & metrics["Predictor"].isin(initial)
        ]
        qualified = target_rows[target_rows["Eligible"]].sort_values(
            [
                "PrefilterScore", "PctWindowsAboveRandom", "ROCAUCMedian",
                "ROCAUCWorst", "ROCAUCStd", "Predictor",
            ],
            ascending=[False, False, False, False, True, True],
            kind="stable",
        )
        top = qualified.head(config.predictor_prefilter_top_n)
        metrics.loc[qualified.index, "PrefilterStatus"] = "rejected_top_n"
        metrics.loc[top.index, "PrefilterStatus"] = "retained"
        retained: list[str] = []
        for index, row in top.iterrows():
            predictor = str(row["Predictor"])
            redundant_with = None
            redundancy_correlation = None
            for kept in retained:
                correlation = _feature_correlation(
                    prepared_development, predictor, kept, config.lag_depth
                )
                if correlation is not None and abs(correlation) >= threshold:
                    redundant_with = kept
                    redundancy_correlation = correlation
                    break
            if redundant_with is None:
                retained.append(predictor)
            else:
                metrics.at[index, "PrefilterStatus"] = "removed_redundancy"
                metrics.at[index, "RedundantWith"] = redundant_with
                metrics.at[index, "RedundancyCorrelation"] = redundancy_correlation
        retained_by_target[target] = tuple(retained)
        diagnostics.append({
            "target": target,
            "initial_candidates": len(initial),
            **_threshold_rejection_counts(target_rows, config),
            "after_qualification": len(qualified),
            "after_top_n": len(top),
            "removed_for_redundancy": len(top) - len(retained),
            "after_redundancy": len(retained),
            "retained_predictors": retained,
            "combinations_before_filtering": _combination_count(
                len(initial), config.permutation_depth
            ),
            "combinations_tested": _combination_count(
                len(retained), config.permutation_depth
            ),
        })

    return PredictorPrefilterResult(
        retained_by_target,
        metrics.sort_values(["Observation", "PrefilterScore", "Predictor"],
                            ascending=[True, False, True], kind="stable").reset_index(drop=True),
        diagnostics,
    )
