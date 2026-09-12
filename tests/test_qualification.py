from dataclasses import replace

import pandas as pd

from rstock.config import DEFAULT_CONFIG
from rstock.qualification import qualify_combinations


def _frames():
    window_rows = []
    prediction_rows = []
    definitions = {
        "stable": [0.60, 0.61, 0.59],
        "unstable": [0.80, 0.51, 0.30],
        "random": [0.51, 0.49, 0.50],
    }
    for set_name, aucs in definitions.items():
        for number, auc in enumerate(aucs, 1):
            window_rows.append(
                {
                    "Set": set_name,
                    "Observation": "AAA",
                    "Predictors": '["BBB"]',
                    "Window": number,
                    "UpROCAUC": auc,
                    "UpPRAUC": 0.4 + auc / 10,
                    "UpPrevalence": 0.4,
                }
            )
            for offset in range(10):
                outcome = offset % 2
                prediction_rows.append(
                    {
                        "Set": set_name,
                        "Window": number,
                        "UpTarget": outcome,
                        "UpPrediction": outcome if set_name == "stable" else 0,
                        "UpProbability": 0.8 if outcome else 0.2,
                    }
                )
    return pd.DataFrame(window_rows), pd.DataFrame(prediction_rows)


def test_qualification_requires_repeated_stability_and_ranks_only_eligible():
    windows, predictions = _frames()
    config = replace(
        DEFAULT_CONFIG,
        qualification_min_windows=3,
        qualification_min_median_auc=0.55,
        qualification_min_pct_windows_above_random=2 / 3,
        qualification_min_worst_window_auc=0.50,
        qualification_min_positive_observations=10,
        qualification_max_auc_std=0.05,
    )

    result = qualify_combinations(windows, predictions, config).set_index("Set")

    assert bool(result.loc["stable", "Eligible"])
    assert result.loc["stable", "EligibleRank"] == 1
    assert not bool(result.loc["unstable", "Eligible"])
    assert pd.isna(result.loc["unstable", "EligibleRank"])
    assert result.loc["stable", "WindowsEvaluated"] == 3
    assert result.loc["stable", "ROCAUCMedian"] == 0.60
    assert result.loc["stable", "ROCAUCWorst"] == 0.59
    assert result.loc["stable", "PositiveObservations"] == 15


def test_undefined_auc_window_cannot_satisfy_minimum_auc_window_count():
    windows, predictions = _frames()
    windows.loc[
        (windows["Set"] == "stable") & (windows["Window"] == 3), "UpROCAUC"
    ] = None
    config = replace(
        DEFAULT_CONFIG,
        qualification_min_windows=3,
        qualification_min_median_auc=0.55,
        qualification_min_pct_windows_above_random=0.5,
        qualification_min_worst_window_auc=0.5,
        qualification_min_positive_observations=10,
        qualification_max_auc_std=0.2,
    )

    result = qualify_combinations(windows, predictions, config).set_index("Set")

    assert not bool(result.loc["stable", "Eligible"])
    assert "insufficient_auc_windows" in result.loc["stable", "IneligibilityReasons"]
