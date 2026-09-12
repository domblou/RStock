"""Correct binary-classification evaluation against observed outcomes."""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import asdict, dataclass
from typing import Any

import numpy as np
import pandas as pd


@dataclass(frozen=True, slots=True)
class BinaryClassificationMetrics:
    """Standard metrics calculated against the actual binary outcome."""

    true_negative: int
    false_positive: int
    false_negative: int
    true_positive: int
    accuracy: float
    precision: float
    recall: float
    f1: float
    roc_auc: float | None

    def as_dict(self) -> dict[str, Any]:
        return asdict(self)

    def as_columns(self) -> dict[str, int | float | None]:
        return {
            "TN": self.true_negative,
            "FP": self.false_positive,
            "FN": self.false_negative,
            "TP": self.true_positive,
            "Accuracy": self.accuracy,
            "Precision": self.precision,
            "Recall": self.recall,
            "F1": self.f1,
            "ROCAUC": self.roc_auc,
        }


def binary_predictions(probabilities: Sequence[float], threshold: float = 0.5) -> np.ndarray:
    """Apply the R workflow's strict greater-than threshold."""

    return (np.asarray(probabilities, dtype=float) > threshold).astype(int)


def outcome_error(predictions: Sequence[int], outcome: Sequence[int]) -> float:
    """Calculate classification error against the actual outcome."""

    predicted = np.asarray(predictions)
    actual = np.asarray(outcome)
    if predicted.shape != actual.shape:
        raise ValueError("predictions and outcome must have the same shape")
    return float(np.mean(predicted != actual))


def _roc_auc(outcome: np.ndarray, probabilities: np.ndarray) -> float | None:
    positives = outcome == 1
    positive_count = int(positives.sum())
    negative_count = len(outcome) - positive_count
    if positive_count == 0 or negative_count == 0:
        return None

    # Average ranks reproduce the conventional 0.5 credit for tied scores.
    ranks = pd.Series(probabilities).rank(method="average").to_numpy()
    positive_rank_sum = float(ranks[positives].sum())
    auc = (
        positive_rank_sum - positive_count * (positive_count + 1) / 2
    ) / (positive_count * negative_count)
    return float(auc)


def classification_metrics(
    outcome: Sequence[int],
    predictions: Sequence[int],
    probabilities: Sequence[float],
) -> BinaryClassificationMetrics:
    """Return confusion counts and standard binary-classification metrics.

    Precision, recall and F1 use zero when their denominator is zero. ROC-AUC is
    ``None`` when the test set contains only one outcome class.
    """

    actual = np.asarray(outcome, dtype=int).reshape(-1)
    predicted = np.asarray(predictions, dtype=int).reshape(-1)
    scores = np.asarray(probabilities, dtype=float).reshape(-1)
    if not (actual.shape == predicted.shape == scores.shape):
        raise ValueError("outcome, predictions and probabilities must have the same shape")
    if not np.isin(actual, [0, 1]).all() or not np.isin(predicted, [0, 1]).all():
        raise ValueError("outcome and predictions must contain only binary values")

    true_negative = int(((actual == 0) & (predicted == 0)).sum())
    false_positive = int(((actual == 0) & (predicted == 1)).sum())
    false_negative = int(((actual == 1) & (predicted == 0)).sum())
    true_positive = int(((actual == 1) & (predicted == 1)).sum())
    accuracy = float((actual == predicted).mean()) if len(actual) else 0.0
    precision_denominator = true_positive + false_positive
    recall_denominator = true_positive + false_negative
    precision = true_positive / precision_denominator if precision_denominator else 0.0
    recall = true_positive / recall_denominator if recall_denominator else 0.0
    f1 = 2 * precision * recall / (precision + recall) if precision + recall else 0.0

    return BinaryClassificationMetrics(
        true_negative=true_negative,
        false_positive=false_positive,
        false_negative=false_negative,
        true_positive=true_positive,
        accuracy=accuracy,
        precision=float(precision),
        recall=float(recall),
        f1=float(f1),
        roc_auc=_roc_auc(actual, scores),
    )
