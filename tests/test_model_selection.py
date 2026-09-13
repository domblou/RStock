from dataclasses import replace
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from rstock.config import RStockConfig
from rstock.model_selection import SCORE_COLUMNS, score_qualified_models


def _config(**changes) -> RStockConfig:
    return replace(
        RStockConfig(project_root=Path(".")),
        qualification_min_windows=3,
        qualification_min_positive_observations=20,
        qualification_max_auc_std=0.10,
        **changes,
    )


def _row(name: str, **changes) -> dict[str, object]:
    values: dict[str, object] = {
        "Set": name,
        "Eligible": True,
        "ROCAUCMedian": 0.70,
        "ROCAUCWorst": 0.62,
        "ROCAUCStd": 0.02,
        "PctWindowsAboveRandom": 0.90,
        "WindowsEvaluated": 8,
        "AUCWindows": 8,
        "PositiveObservations": 80,
        "FinalUpROCAUC": 0.68,
        "FinalConfirmed": True,
    }
    values.update(changes)
    return values


def test_stable_model_ranks_above_higher_but_unstable_model():
    rows = pd.DataFrame([
        _row(
            "high-unstable", ROCAUCMedian=0.82, ROCAUCWorst=0.50,
            ROCAUCStd=0.10, PctWindowsAboveRandom=0.55, FinalUpROCAUC=0.61,
        ),
        _row("stable", ROCAUCMedian=0.72, ROCAUCWorst=0.68, ROCAUCStd=0.01, FinalUpROCAUC=0.71),
    ])

    scored = score_qualified_models(rows, _config()).set_index("Set")

    assert scored.at["stable", "model_selection_rank"] == 1
    assert scored.at["stable", "model_selection_score"] > scored.at["high-unstable", "model_selection_score"]


def test_large_development_holdout_delta_is_penalized():
    rows = pd.DataFrame([
        _row("close", FinalUpROCAUC=0.69),
        _row("far", FinalUpROCAUC=0.52),
    ])

    scored = score_qualified_models(rows, _config()).set_index("Set")

    assert scored.at["close", "holdout_score"] > scored.at["far", "holdout_score"]
    assert scored.at["close", "model_selection_score"] > scored.at["far", "model_selection_score"]


def test_low_window_count_is_penalized():
    rows = pd.DataFrame([
        _row("few", WindowsEvaluated=3, AUCWindows=3),
        _row("many", WindowsEvaluated=8, AUCWindows=8),
    ])

    scored = score_qualified_models(rows, _config()).set_index("Set")

    assert scored.at["many", "sample_adequacy_score"] > scored.at["few", "sample_adequacy_score"]


def test_configurable_weights_change_ranking_without_changing_eligibility():
    rows = pd.DataFrame([
        _row("predictive", ROCAUCMedian=0.85, ROCAUCWorst=0.51, ROCAUCStd=0.09, PctWindowsAboveRandom=0.55),
        _row("stable", ROCAUCMedian=0.65, ROCAUCWorst=0.64, ROCAUCStd=0.01, PctWindowsAboveRandom=1.0),
    ])
    predictive_config = _config(
        model_selection_predictive_quality_weight=1.0,
        model_selection_stability_weight=0.0,
        model_selection_holdout_weight=0.0,
        model_selection_signal_quality_weight=0.0,
        model_selection_sample_adequacy_weight=0.0,
    )
    stability_config = replace(
        predictive_config,
        model_selection_predictive_quality_weight=0.0,
        model_selection_stability_weight=1.0,
    )

    predictive = score_qualified_models(rows, predictive_config).set_index("Set")
    stable = score_qualified_models(rows, stability_config).set_index("Set")

    assert predictive.at["predictive", "model_selection_rank"] == 1
    assert stable.at["stable", "model_selection_rank"] == 1
    assert score_qualified_models(rows, stability_config).equals(
        score_qualified_models(rows, stability_config)
    )
    assert score_qualified_models(rows, stability_config)["Eligible"].tolist() == rows["Eligible"].tolist()


def test_missing_signal_metrics_are_neutral_and_ineligible_models_are_not_scored():
    rows = pd.DataFrame([
        _row("eligible"),
        _row("ineligible", Eligible=False, FinalConfirmed=False),
    ])

    scored = score_qualified_models(rows, _config()).set_index("Set")

    assert np.isnan(scored.at["eligible", "signal_quality_score"])
    assert np.isfinite(scored.at["eligible", "model_selection_score"])
    assert all(pd.isna(scored.at["ineligible", name]) for name in SCORE_COLUMNS)


def test_signal_quality_uses_persisted_calibration_metrics_and_penalizes_tiny_sample():
    rows = pd.DataFrame([
        _row("enough", calibration_metrics={
            "success_rate": 0.70, "window_coverage": 1.0, "total_signals": 100,
            "precision_stability": 0.02, "mean_return": 0.012,
        }),
        _row("tiny", calibration_metrics={
            "success_rate": 0.70, "window_coverage": 1.0, "total_signals": 2,
            "precision_stability": 0.02, "mean_return": 0.012,
        }),
    ])

    scored = score_qualified_models(rows, _config()).set_index("Set")

    assert scored.at["enough", "signal_quality_score"] > scored.at["tiny", "signal_quality_score"]
    assert scored.at["enough", "model_selection_score"] > scored.at["tiny", "model_selection_score"]


def test_invalid_weights_are_rejected():
    rows = pd.DataFrame([_row("model")])
    with pytest.raises(ValueError, match="cannot be negative"):
        score_qualified_models(
            rows, _config(model_selection_stability_weight=-1.0)
        )
