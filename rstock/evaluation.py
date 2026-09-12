"""Model evaluation, including the deliberately preserved legacy error."""

from __future__ import annotations

from collections.abc import Sequence

import numpy as np
import pandas as pd

from .config import ErrorMetric


def binary_predictions(probabilities: Sequence[float], threshold: float = 0.5) -> np.ndarray:
    """Apply the R workflow's strict greater-than threshold."""

    return (np.asarray(probabilities, dtype=float) > threshold).astype(int)


def legacy_predictor_error(predictions: Sequence[int], predictors: pd.DataFrame) -> float:
    """Reproduce the erroneous R comparison against every predictor value.

    R recycles the prediction vector down every matrix column. NumPy's row-wise
    broadcasting below is algebraically equivalent.
    """

    predicted = np.asarray(predictions).reshape(-1, 1)
    values = predictors.to_numpy()
    if len(predicted) != len(values):
        raise ValueError("predictions and predictors must have the same row count")
    return float(np.mean(predicted != values))


def outcome_error(predictions: Sequence[int], outcome: Sequence[int]) -> float:
    """Calculate classification error against the actual outcome (future mode)."""

    predicted = np.asarray(predictions)
    actual = np.asarray(outcome)
    if predicted.shape != actual.shape:
        raise ValueError("predictions and outcome must have the same shape")
    return float(np.mean(predicted != actual))


def calculate_error(
    predictions: Sequence[int],
    predictors: pd.DataFrame,
    outcome: Sequence[int],
    mode: ErrorMetric = "legacy_predictors",
) -> float:
    if mode == "legacy_predictors":
        return legacy_predictor_error(predictions, predictors)
    if mode == "outcome":
        return outcome_error(predictions, outcome)
    raise ValueError(f"Unknown error metric: {mode}")

