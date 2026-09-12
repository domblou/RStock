import numpy as np
import pandas as pd

from rstock.evaluation import binary_predictions, calculate_error


def test_binary_threshold_is_strictly_greater_than_half():
    assert binary_predictions([0.49, 0.5, 0.51]).tolist() == [0, 0, 1]


def test_legacy_error_is_explicitly_distinct_from_correct_outcome_error():
    predicted = np.array([1, 0])
    predictors = pd.DataFrame({"x1": [1, 0], "x2": [1, 1]})
    outcome = np.array([0, 0])

    assert calculate_error(predicted, predictors, outcome, "legacy_predictors") == 0.25
    assert calculate_error(predicted, predictors, outcome, "outcome") == 0.5

