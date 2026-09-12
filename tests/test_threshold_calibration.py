from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG
from rstock.combinations import generate_symbol_sets
from rstock.features import prepare_dataset
from rstock.modeling import XGBoostParameters
from rstock.threshold_calibration import (
    EXPERIMENTAL_XGBOOST_PARAMETERS,
    adaptive_threshold_grid,
    apply_frozen_thresholds,
    calibrate_thresholds,
    evaluate_threshold_grid,
    generate_development_probabilities,
    probability_distribution,
    run_controlled_threshold_calibration,
    summarize_and_select_thresholds,
    validate_threshold_calibration_config,
)


def _predictions() -> pd.DataFrame:
    rows = []
    dates = pd.bdate_range("2025-01-01", periods=8)
    probabilities = {
        "Up": ([0.10, 0.30, 0.55, 0.80], [0.20, 0.45, 0.65, 0.90]),
        "Down": ([0.15, 0.35, 0.60, 0.85], [0.25, 0.40, 0.70, 0.95]),
    }
    returns = [0.02, -0.02, 0.015, -0.015]
    for direction, windows in probabilities.items():
        for window, scores in enumerate(windows, start=1):
            for position, (date, score, realised) in enumerate(
                zip(dates[(window - 1) * 4 : window * 4], scores, returns, strict=True)
            ):
                target = realised >= 0.01 if direction == "Up" else realised <= -0.01
                rows.append(
                    {
                        "Direction": direction,
                        "Window": window,
                        "Date": date,
                        "Probability": score,
                        "Target": int(target),
                        "IntradayReturn": realised,
                        "MFE": 0.025 + position / 1000,
                        "MAE": -0.020 - position / 1000,
                    }
                )
    return pd.DataFrame(rows)


def _config(**changes):
    values = {
        "threshold_calibration_min_signals_per_window": 1,
        "threshold_calibration_min_window_fraction": 1.0,
        "threshold_calibration_quantiles": (0.5, 0.75, 0.9),
        **changes,
    }
    return replace(DEFAULT_CONFIG, **values)


def test_experimental_xgboost_parameters_are_frozen_as_requested():
    assert EXPERIMENTAL_XGBOOST_PARAMETERS.as_dict() == {
        "max_depth": 1,
        "eta": 0.05,
        "num_boost_round": 120,
        "min_child_weight": 1,
        "subsample": 0.8,
        "colsample_bytree": 0.8,
        "gamma": 0.0,
        "reg_alpha": 0.0,
        "reg_lambda": 1.0,
    }


def test_probability_distribution_and_grid_are_directional_and_adaptive():
    predictions = _predictions()
    distribution = probability_distribution(predictions)
    grid = adaptive_threshold_grid(predictions, _config())

    assert set(distribution["Direction"]) == {"Up", "Down"}
    assert set(distribution["Count"]) == {8}
    assert (grid.groupby("Direction")["ReferenceThreshold"].sum() == 1).all()
    assert set(grid.loc[grid["ReferenceThreshold"], "Threshold"]) == {0.5}
    assert grid.groupby("Direction")["Threshold"].nunique().min() >= 3


def test_threshold_metrics_include_classification_returns_and_directional_risk():
    predictions = _predictions()
    grid = pd.DataFrame(
        {
            "Direction": ["Up", "Down"],
            "Threshold": [0.5, 0.5],
            "Sources": ["reference", "reference"],
            "ReferenceThreshold": [True, True],
        }
    )
    result = evaluate_threshold_grid(predictions, grid, _config())

    assert len(result) == 4
    assert {
        "SignalCount",
        "SignalProportion",
        "Precision",
        "Recall",
        "F1",
        "TP",
        "FP",
        "TN",
        "FN",
        "IntradayReturnMean",
        "IntradayReturnMedian",
        "FavorableMoveFrequency",
        "OppositeMoveFrequency",
        "MFEMean",
        "MAEMean",
    } <= set(result.columns)
    first_up = result[(result["Direction"] == "Up") & (result["Window"] == 1)].iloc[0]
    assert first_up["SignalCount"] == 2
    assert first_up["SignalProportion"] == 0.5
    assert first_up["TP"] == 1
    assert first_up["FP"] == 1
    assert first_up["FavorableMoveFrequency"] == 0.5
    assert first_up["OppositeMoveFrequency"] == 0.5


def test_selection_is_separate_reproducible_and_enforces_minimum_signals():
    predictions = _predictions()
    config = _config(threshold_calibration_min_signals_per_window=2)
    grid = pd.DataFrame(
        {
            "Direction": ["Up", "Up", "Down", "Down"],
            "Threshold": [0.5, 0.89, 0.5, 0.94],
            "Sources": ["reference", "tail", "reference", "tail"],
            "ReferenceThreshold": [True, False, True, False],
        }
    )
    metrics = evaluate_threshold_grid(predictions, grid, config)
    first_summary, first_selected = summarize_and_select_thresholds(metrics, config)
    second_summary, second_selected = summarize_and_select_thresholds(metrics, config)

    pd.testing.assert_frame_equal(first_summary, second_summary)
    assert first_selected == second_selected
    assert set(first_selected) == {"Up", "Down"}
    assert first_selected["Up"]["threshold"] == 0.5
    assert first_selected["Down"]["threshold"] == 0.5
    tail = first_summary[first_summary["Threshold"].isin([0.89, 0.94])]
    assert not tail["Eligible"].any()


def test_calibration_compares_reference_and_frozen_threshold_application():
    predictions = _predictions()
    calibration = calibrate_thresholds(predictions, _config())

    assert set(calibration.selected_thresholds) == {"Up", "Down"}
    assert set(calibration.baseline_comparison["ThresholdRole"]) == {
        "Reference0.5",
        "Calibrated",
    }
    applied = apply_frozen_thresholds(
        predictions, calibration.selected_thresholds
    )
    altered_outcomes = predictions.copy()
    altered_outcomes["Target"] = 1 - altered_outcomes["Target"]
    altered_applied = apply_frozen_thresholds(
        altered_outcomes, calibration.selected_thresholds
    )
    pd.testing.assert_series_equal(applied["Threshold"], altered_applied["Threshold"])
    pd.testing.assert_series_equal(applied["Prediction"], altered_applied["Prediction"])


def test_controlled_runner_freezes_selection_before_optional_holdout(monkeypatch):
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    events = []
    development_predictions = _predictions()

    def fake_development(development, *args, **kwargs):
        events.append("development")
        assert development.index.max() == index[-4]
        assert index[-3] not in development.index
        return development_predictions

    real_calibrate = calibrate_thresholds

    def fake_calibrate(predictions, config):
        events.append("selection_frozen")
        return real_calibrate(predictions, config)

    def fake_holdout(development, holdout, *args, **kwargs):
        events.append("holdout")
        assert events == ["development", "selection_frozen", "holdout"]
        assert development.index.max() < holdout.index.min()
        assert list(holdout.index) == list(index[-3:])
        return development_predictions

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        fake_development,
    )
    monkeypatch.setattr("rstock.threshold_calibration.calibrate_thresholds", fake_calibrate)
    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_holdout_probabilities", fake_holdout
    )

    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        _config(),
        combinations_per_target=1,
        min_train_size=2,
        test_size=2,
        step_size=2,
        final_holdout_size=3,
        evaluate_final_holdout=True,
    )

    assert events == ["development", "selection_frozen", "holdout"]
    assert result.run_configuration["holdout_used_for_selection"] is False
    assert result.run_configuration["holdout_evaluated"] is True


@pytest.mark.parametrize(
    "changes",
    [
        {"threshold_calibration_min_signals_per_window": 0},
        {"threshold_calibration_min_window_fraction": 0.0},
        {"threshold_calibration_min_window_fraction": 1.1},
        {"threshold_calibration_quantiles": (0.0, 0.5)},
    ],
)
def test_threshold_calibration_configuration_is_validated(changes):
    with pytest.raises(ValueError):
        validate_threshold_calibration_config(replace(DEFAULT_CONFIG, **changes))


def test_threshold_probability_workers_preserve_ordered_results(tmp_path):
    index = pd.bdate_range("2025-01-01", periods=35)
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB", "CCC")):
        intraday = np.where((np.arange(len(index)) + offset) % 3 == 0, 0.02, -0.015)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = 100.0 * (1.0 + intraday)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 0.5
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 0.5
    prepared = prepare_dataset(stock, ["AAA", "BBB", "CCC"])
    sampled = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    parameters = XGBoostParameters(1, 0.1, 1)
    arguments = dict(min_train_size=10, test_size=5, step_size=5, parameters=parameters)
    serial = generate_development_probabilities(
        prepared, sampled, replace(_config(project_root=tmp_path), combination_workers=1, xgb_nthread=1),
        **arguments,
    )
    parallel = generate_development_probabilities(
        prepared, sampled, replace(_config(project_root=tmp_path), combination_workers=2, xgb_nthread=1),
        **arguments,
    )
    pd.testing.assert_frame_equal(serial, parallel)
