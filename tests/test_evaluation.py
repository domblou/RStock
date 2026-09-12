import numpy as np
import pandas as pd

from rstock.config import DEFAULT_CONFIG
from rstock.evaluation import binary_predictions, calculate_error, classification_metrics


def test_binary_threshold_is_strictly_greater_than_half():
    assert binary_predictions([0.49, 0.5, 0.51]).tolist() == [0, 0, 1]


def test_legacy_error_is_explicitly_distinct_from_correct_outcome_error():
    predicted = np.array([1, 0])
    predictors = pd.DataFrame({"x1": [1, 0], "x2": [1, 1]})
    outcome = np.array([0, 0])

    assert calculate_error(predicted, predictors, outcome, "legacy_predictors") == 0.25
    assert calculate_error(predicted, predictors, outcome, "outcome") == 0.5


def test_correct_outcome_error_is_the_default():
    predicted = np.array([1, 0])
    predictors = pd.DataFrame({"x1": [1, 0], "x2": [1, 1]})
    outcome = np.array([0, 0])

    assert DEFAULT_CONFIG.error_metric == "outcome"
    assert calculate_error(predicted, predictors, outcome) == 0.5


def test_classification_metrics_include_confusion_matrix_and_auc():
    metrics = classification_metrics(
        outcome=[0, 0, 1, 1],
        predictions=[0, 1, 0, 1],
        probabilities=[0.1, 0.8, 0.4, 0.9],
    )

    assert metrics.as_columns() == {
        "TN": 1,
        "FP": 1,
        "FN": 1,
        "TP": 1,
        "Accuracy": 0.5,
        "Precision": 0.5,
        "Recall": 0.5,
        "F1": 0.5,
        "ROCAUC": 0.75,
    }


def test_undefined_rates_are_zero_and_single_class_auc_is_none():
    metrics = classification_metrics(
        outcome=[0, 0], predictions=[0, 0], probabilities=[0.1, 0.2]
    )

    assert metrics.accuracy == 1.0
    assert metrics.precision == 0.0
    assert metrics.recall == 0.0
    assert metrics.f1 == 0.0
    assert metrics.roc_auc is None
