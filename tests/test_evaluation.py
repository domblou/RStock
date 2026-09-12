import numpy as np

from rstock.evaluation import binary_predictions, classification_metrics, outcome_error


def test_binary_threshold_is_strictly_greater_than_half():
    assert binary_predictions([0.49, 0.5, 0.51]).tolist() == [0, 0, 1]


def test_error_is_calculated_only_against_the_outcome():
    predicted = np.array([1, 0])
    outcome = np.array([0, 0])

    assert outcome_error(predicted, outcome) == 0.5


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
        "PRAUC": 0.8333333333333333,
        "Prevalence": 0.5,
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
    assert metrics.pr_auc is None
    assert metrics.prevalence == 0.0
