from dataclasses import replace
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.combinations import generate_symbol_sets
from rstock.calibration_sampling import GLOBAL_STRATIFIED_V2
from rstock.features import prepare_dataset
from rstock.modeling import XGBoostParameters
from rstock.threshold_calibration import (
    EXPERIMENTAL_XGBOOST_PARAMETERS,
    adaptive_threshold_grid,
    apply_frozen_thresholds,
    apply_frozen_thresholds_by_set,
    calibrate_thresholds,
    calibrate_thresholds_by_set,
    evaluate_threshold_grid,
    generate_development_probabilities,
    generate_holdout_probabilities,
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


def _economically_viable_predictions() -> pd.DataFrame:
    """Calibration fixture whose high scores align with directional returns."""

    predictions = _predictions().copy()
    up_scores = [0.80, 0.10, 0.70, 0.20, 0.75, 0.15, 0.65, 0.25]
    down_scores = [0.10, 0.80, 0.20, 0.70, 0.15, 0.75, 0.25, 0.65]
    predictions.loc[predictions["Direction"] == "Up", "Probability"] = up_scores
    predictions.loc[predictions["Direction"] == "Down", "Probability"] = down_scores
    return predictions


def _config(**changes):
    values = {
        "threshold_calibration_min_signals_per_window": 1,
        "threshold_calibration_min_window_fraction": 1.0,
        "threshold_calibration_quantiles": (0.5, 0.75, 0.9),
        **changes,
    }
    return replace(DEFAULT_CONFIG, **values)


def _threshold_candidate(
    threshold, signal_counts, *, precision, directional_return, opposite,
    precision_by_window=None,
):
    precision_values = precision_by_window or [precision] * len(signal_counts)
    return pd.DataFrame([
        {
            "Direction": "Up", "Threshold": threshold, "Window": window,
            "Observations": 50, "SignalCount": count,
            "SignalProportion": count / 50,
            "Precision": precision_value, "Recall": 0.5, "F1": 0.55,
            "IntradayReturnMean": directional_return,
            "IntradayReturnMedian": directional_return,
            "DirectionalReturnMean": directional_return,
            "FavorableMoveFrequency": precision_value,
            "OppositeMoveFrequency": opposite,
            "MFEMean": 0.02, "MAEMean": -0.01,
        }
        for window, (count, precision_value) in enumerate(
            zip(signal_counts, precision_values, strict=True), start=1
        )
    ])


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


def test_development_and_holdout_use_directional_xgboost_parameters(monkeypatch):
    index = pd.bdate_range("2025-01-01", periods=24)
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB")):
        intraday = np.where((np.arange(len(index)) + offset) % 2 == 0, 0.02, -0.02)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = 100.0 * (1.0 + intraday)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 0.5
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 0.5
    prepared = prepare_dataset(stock, ["AAA", "BBB"])
    sampled = generate_symbol_sets(["AAA", "BBB"], 1).iloc[:1]
    directional = {
        "Up": XGBoostParameters(2, 0.05, 10),
        "Down": XGBoostParameters(4, 0.1, 20),
    }
    calls = []

    def fake_fit(frame, names, outcome, config, *, parameters):
        calls.append((outcome, parameters))
        return object()

    monkeypatch.setattr("rstock.threshold_calibration.fit_booster", fake_fit)
    monkeypatch.setattr(
        "rstock.threshold_calibration.predict_probabilities",
        lambda booster, frame, names: np.full(len(frame), 0.5),
    )
    config = replace(_config(), combination_workers=1)
    generate_development_probabilities(
        prepared.iloc[:18], sampled, config,
        min_train_size=8, test_size=4, step_size=4,
        parameters_by_direction=directional,
    )
    generate_holdout_probabilities(
        prepared.iloc[:18], prepared.iloc[18:], sampled, config,
        parameters_by_direction=directional,
    )

    assert calls
    assert all(parameters is directional["Up"] for _, parameters in calls[::2])
    assert all(parameters is directional["Down"] for _, parameters in calls[1::2])


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
    predictions = _economically_viable_predictions()
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


def test_configurable_robust_sample_minimum_prefers_adequate_sample():
    metrics = pd.concat([
        _threshold_candidate(0.70, [3, 2], precision=0.70, directional_return=0.02, opposite=0.10),
        _threshold_candidate(0.55, [10, 10], precision=0.60, directional_return=0.015, opposite=0.15),
    ], ignore_index=True)

    summary, selected = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_min_robust_signals=10)
    )

    assert selected["Up"]["threshold"] == 0.55
    assert selected["Up"]["selection_reason"] == "robust_sample_preferred"
    assert summary.loc[summary["Threshold"] == 0.70, "RobustSample"].iloc[0] == False

    _, lowered_minimum = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_min_robust_signals=5)
    )
    assert lowered_minimum["Up"]["threshold"] == 0.70


@pytest.mark.parametrize(
    ("directional_return", "opposite", "reason"),
    [
        (-0.001, 0.10, "non_positive_directional_return"),
        (0.010, 0.31, "opposite_move_frequency_above_0.30"),
    ],
)
def test_economic_constraints_reject_negative_return_or_excess_opposite_moves(
    directional_return, opposite, reason
):
    metrics = _threshold_candidate(
        0.60, [10, 10], precision=0.80,
        directional_return=directional_return, opposite=opposite,
    )

    summary, selected = summarize_and_select_thresholds(metrics, _config())

    assert selected["Up"]["status"] == "no_eligible_threshold"
    assert summary.iloc[0]["Eligible"] == False
    assert reason in summary.iloc[0]["RejectionReason"]


def test_higher_precision_wins_between_robust_candidates_before_return():
    metrics = pd.concat([
        _threshold_candidate(0.50, [10, 10], precision=0.62, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.60, [8, 7], precision=0.66, directional_return=0.008, opposite=0.25),
    ], ignore_index=True)

    _, selected = summarize_and_select_thresholds(metrics, _config())

    assert selected["Up"]["threshold"] == 0.60
    assert selected["Up"]["calibration_metrics"]["precision"] == pytest.approx(0.66)


def test_zero_precision_tolerance_keeps_the_highest_precision_up_candidate():
    metrics = pd.concat([
        _threshold_candidate(0.50, [10, 10], precision=0.70, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.60, [10, 10], precision=0.698, directional_return=0.04, opposite=0.10),
    ], ignore_index=True)

    summary, selected = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_precision_tolerance=0.0)
    )

    assert selected["Up"]["threshold"] == 0.50
    assert summary.loc[summary["Threshold"] == 0.50, "WithinPrecisionTolerance"].iloc[0]
    assert not summary.loc[summary["Threshold"] == 0.60, "WithinPrecisionTolerance"].iloc[0]


def test_precision_tolerance_allows_economic_up_tie_breaker():
    metrics = pd.concat([
        _threshold_candidate(0.50, [10, 10], precision=0.70, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.60, [10, 10], precision=0.698, directional_return=0.04, opposite=0.10),
    ], ignore_index=True)

    summary, selected = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_precision_tolerance=0.01)
    )

    assert selected["Up"]["threshold"] == 0.60
    assert selected["Up"]["selection_reason"] == "precision_tolerance_economic_selection"
    assert summary["WithinPrecisionTolerance"].all()
    assert summary["BestPrecision"].tolist() == pytest.approx([0.70, 0.70])


def test_up_candidate_outside_precision_tolerance_cannot_win_economic_tie_breaker():
    metrics = pd.concat([
        _threshold_candidate(0.50, [10, 10], precision=0.70, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.60, [10, 10], precision=0.689, directional_return=0.04, opposite=0.10),
    ], ignore_index=True)

    summary, selected = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_precision_tolerance=0.01)
    )

    assert selected["Up"]["threshold"] == 0.50
    assert not summary.loc[summary["Threshold"] == 0.60, "WithinPrecisionTolerance"].iloc[0]


def test_down_selection_is_unchanged_by_up_precision_tolerance():
    metrics = pd.concat([
        _threshold_candidate(0.50, [10, 10], precision=0.60, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.60, [10, 10], precision=0.70, directional_return=0.01, opposite=0.20),
    ], ignore_index=True)
    metrics["Direction"] = "Down"

    _, selected = summarize_and_select_thresholds(
        metrics, _config(threshold_calibration_precision_tolerance=0.50)
    )

    assert selected["Down"]["threshold"] == 0.60
    assert selected["Down"]["selection_reason"] == "legacy_down_stability_order"


def test_below_configured_robust_minimum_is_a_deterministic_fallback():
    metrics = pd.concat([
        _threshold_candidate(0.65, [4, 4], precision=0.64, directional_return=0.01, opposite=0.20),
        _threshold_candidate(0.70, [3, 2], precision=0.68, directional_return=0.01, opposite=0.20),
    ], ignore_index=True)

    config = _config(threshold_calibration_min_robust_signals=9)
    first_summary, first_selected = summarize_and_select_thresholds(metrics, config)
    second_summary, second_selected = summarize_and_select_thresholds(metrics, config)

    assert first_selected["Up"]["threshold"] == 0.70
    assert first_selected["Up"]["selection_reason"] == "fallback_below_9_total_signals"
    assert first_selected == second_selected
    pd.testing.assert_frame_equal(first_summary, second_summary)


def test_calibration_compares_reference_and_frozen_threshold_application():
    predictions = _economically_viable_predictions()
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


def test_calibration_is_independent_per_model_set_and_keeps_signal_diagnostics():
    predictions = _economically_viable_predictions()
    first = predictions.copy()
    first["Set"] = "AAA<-BBB"
    second = predictions.copy()
    second["Set"] = "CCC<-DDD"
    second["Probability"] = 1.0 - second["Probability"]

    calibrated = calibrate_thresholds_by_set(
        pd.concat([first, second], ignore_index=True), _config()
    )

    assert set(calibrated) == {"AAA<-BBB", "CCC<-DDD"}
    selected = calibrated["AAA<-BBB"].selected_thresholds["Up"]
    assert selected["status"] == "selected"
    assert selected["calibration_sample_size"] == 8
    assert {
        "total_signals", "success_rate", "mean_return", "median_return",
        "mfe_mean", "mae_mean", "return_stability", "precision_stability",
    } <= set(selected["calibration_metrics"])


def test_holdout_application_uses_each_set_frozen_threshold_without_reselection():
    predictions = _predictions()
    first = predictions.copy()
    first["Set"] = "AAA<-BBB"
    second = predictions.copy()
    second["Set"] = "CCC<-DDD"
    frozen = {
        "AAA<-BBB": {
            "Up": {"status": "selected", "threshold": 0.50},
            "Down": {"status": "selected", "threshold": 0.50},
        },
        "CCC<-DDD": {
            "Up": {"status": "selected", "threshold": 0.90},
            "Down": {"status": "selected", "threshold": 0.90},
        },
    }

    applied = apply_frozen_thresholds_by_set(
        pd.concat([first, second], ignore_index=True), frozen
    )

    assert set(applied[applied["Set"] == "AAA<-BBB"]["Threshold"]) == {0.5}
    assert set(applied[applied["Set"] == "CCC<-DDD"]["Threshold"]) == {0.9}


def test_controlled_runner_freezes_selection_before_optional_holdout(monkeypatch):
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    events = []
    development_predictions = _economically_viable_predictions()

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


def test_controlled_runner_persists_effective_xgboost_provenance(monkeypatch):
    directional = {
        "Up": XGBoostParameters(2, 0.04, 60),
        "Down": XGBoostParameters(5, 0.09, 100),
    }
    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        lambda *args, **kwargs: _economically_viable_predictions(),
    )
    prepared = pd.DataFrame(
        {"placeholder": np.arange(12)},
        index=pd.bdate_range("2025-01-01", periods=12),
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generate_symbol_sets(["AAA", "BBB"], 1),
        _config(),
        combinations_per_target=1,
        min_train_size=2,
        test_size=2,
        step_size=2,
        final_holdout_size=3,
        xgboost_parameters_by_direction=directional,
        xgboost_parameter_source="frozen_snapshot",
        source_xgboost_calibration_run="xgb-parent",
        frozen_xgboost_parameters_sha256="abc123",
    )

    configuration = result.run_configuration
    assert configuration["xgboost_parameter_source"] == "frozen_snapshot"
    assert configuration["xgboost_parameters_by_direction"] == {
        direction: parameters.as_dict()
        for direction, parameters in directional.items()
    }
    assert configuration["source_xgboost_calibration_run"] == "xgb-parent"
    assert configuration["frozen_xgboost_parameters_sha256"] == "abc123"


def _controlled_result_with_predictions(monkeypatch, predictions, config):
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    holdout_called = False

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        lambda *args, **kwargs: predictions,
    )

    def fake_holdout(*args, **kwargs):
        nonlocal holdout_called
        holdout_called = True
        return predictions.copy()

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_holdout_probabilities",
        fake_holdout,
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        config,
        combinations_per_target=1,
        min_train_size=2,
        test_size=2,
        step_size=2,
        final_holdout_size=3,
        evaluate_final_holdout=True,
    )
    return result, holdout_called


def _per_set_predictions(missing_by_set):
    frames = []
    for set_name, missing_directions in missing_by_set.items():
        predictions = _economically_viable_predictions().copy()
        predictions["Set"] = set_name
        for direction in missing_directions:
            predictions.loc[
                predictions["Direction"] == direction, "Probability"
            ] = 0.10
        frames.append(predictions)
    return pd.concat(frames, ignore_index=True)


def _partial_holdout_result(monkeypatch, missing_by_set):
    predictions = _per_set_predictions(missing_by_set)
    result, holdout_called = _controlled_result_with_predictions(
        monkeypatch,
        predictions,
        _config(threshold_calibration_min_signals_per_window=2),
    )
    return result, holdout_called


@pytest.mark.parametrize("missing_direction", ["Up", "Down"])
def test_no_eligible_direction_runs_partial_frozen_holdout_without_crashing(
    monkeypatch, missing_direction
):
    predictions = _economically_viable_predictions().copy()
    predictions["Set"] = "AAA<-BBB"
    predictions.loc[predictions["Direction"] == missing_direction, "Probability"] = 0.10

    result, holdout_called = _controlled_result_with_predictions(
        monkeypatch,
        predictions,
        _config(threshold_calibration_min_signals_per_window=2),
    )

    available_direction = "Down" if missing_direction == "Up" else "Up"
    assert holdout_called is True
    assert set(result.holdout_predictions["Direction"]) == {available_direction}
    assert set(result.holdout_metrics["Direction"]) == {available_direction}
    assert result.run_configuration["outcome"] == "completed_partial_holdout"
    assert result.run_configuration["holdout_evaluated"] is True
    assert result.run_configuration["holdout_skipped_reason"] is None
    assert any(
        item["direction"] == missing_direction
        for item in result.run_configuration["missing_frozen_thresholds"]
    )
    counts = result.run_configuration["holdout_combination_counts"]
    assert counts[missing_direction]["evaluated_combinations"] == 0
    assert counts[available_direction]["evaluated_combinations"] == 1


@pytest.mark.parametrize(
    ("missing_by_set", "expected_outcome", "expected_up", "expected_down"),
    [
        ({"set-a": set(), "set-b": set()}, "completed", 2, 2),
        ({"set-a": {"Up"}, "set-b": set()}, "completed_partial_holdout", 1, 2),
        ({"set-a": {"Down"}, "set-b": set()}, "completed_partial_holdout", 2, 1),
        (
            {"set-a": {"Up", "Down"}, "set-b": set()},
            "completed_partial_holdout", 1, 1,
        ),
        (
            {"set-a": {"Up"}, "set-b": {"Up"}},
            "completed_partial_holdout", 0, 2,
        ),
    ],
)
def test_holdout_filters_each_set_and_direction_independently(
    monkeypatch, missing_by_set, expected_outcome, expected_up, expected_down
):
    result, holdout_called = _partial_holdout_result(monkeypatch, missing_by_set)

    assert holdout_called is True
    assert result.run_configuration["outcome"] == expected_outcome
    counts = result.run_configuration["holdout_combination_counts"]
    assert counts["Up"]["evaluated_combinations"] == expected_up
    assert counts["Down"]["evaluated_combinations"] == expected_down
    assert counts["Up"]["total_combinations"] == len(missing_by_set)
    assert counts["Down"]["total_combinations"] == len(missing_by_set)
    expected_missing_up = sum("Up" in missing for missing in missing_by_set.values())
    expected_missing_down = sum(
        "Down" in missing for missing in missing_by_set.values()
    )
    assert result.run_configuration["missing_threshold_count_up"] == expected_missing_up
    assert (
        result.run_configuration["missing_threshold_count_down"]
        == expected_missing_down
    )
    assert counts["Up"]["exclusion_reasons"].get("no_eligible_threshold", 0) == (
        expected_missing_up
    )
    assert counts["Down"]["exclusion_reasons"].get("no_eligible_threshold", 0) == (
        expected_missing_down
    )
    actual_pairs = set(
        result.holdout_predictions[["Set", "Direction"]]
        .drop_duplicates()
        .itertuples(index=False, name=None)
    )
    metric_pairs = set(
        result.holdout_metrics[["Set", "Direction"]]
        .drop_duplicates()
        .itertuples(index=False, name=None)
    )
    expected_pairs = {
        (set_name, direction)
        for set_name, missing in missing_by_set.items()
        for direction in ("Up", "Down")
        if direction not in missing
    }
    assert actual_pairs == expected_pairs
    assert metric_pairs == expected_pairs


def test_no_eligible_set_direction_skips_the_entire_holdout(monkeypatch):
    predictions = _per_set_predictions({
        "set-a": {"Up", "Down"},
        "set-b": {"Up", "Down"},
    })
    holdout_called = False
    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        lambda *args, **kwargs: predictions,
    )

    def unexpected_holdout(*args, **kwargs):
        nonlocal holdout_called
        holdout_called = True
        raise AssertionError("Holdout must not run without any frozen threshold")

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_holdout_probabilities",
        unexpected_holdout,
    )
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    result = run_controlled_threshold_calibration(
        prepared,
        generate_symbol_sets(["AAA", "BBB"], 1),
        _config(threshold_calibration_min_signals_per_window=2),
        combinations_per_target=1,
        min_train_size=2,
        test_size=2,
        step_size=2,
        final_holdout_size=3,
        evaluate_final_holdout=True,
    )

    assert holdout_called is False
    assert result.run_configuration["outcome"] == "completed_no_eligible_threshold"
    assert result.run_configuration["holdout_evaluated"] is False
    assert result.run_configuration["holdout_skipped_reason"] == "no_eligible_frozen_threshold"
    assert result.holdout_predictions.empty
    assert result.holdout_metrics.empty


def test_partial_holdout_artifacts_exclude_missing_pairs_without_global_fallback(
    monkeypatch, tmp_path
):
    from rstock.threshold_calibration import write_threshold_calibration_results

    result, _ = _partial_holdout_result(
        monkeypatch, {"set-a": {"Up"}, "set-b": set()}
    )
    assert result.calibration.selected_thresholds["Up"]["status"] == "selected"
    assert not (
        (result.holdout_predictions["Set"] == "set-a")
        & (result.holdout_predictions["Direction"] == "Up")
    ).any()

    write_threshold_calibration_results(result, tmp_path)
    persisted_predictions = pd.read_csv(tmp_path / "holdout_predictions.csv")
    persisted_metrics = pd.read_csv(tmp_path / "holdout_metrics.csv")
    expected_pairs = {("set-a", "Down"), ("set-b", "Up"), ("set-b", "Down")}
    assert set(
        persisted_predictions[["Set", "Direction"]]
        .drop_duplicates()
        .itertuples(index=False, name=None)
    ) == expected_pairs
    assert set(
        persisted_metrics[["Set", "Direction"]]
        .drop_duplicates()
        .itertuples(index=False, name=None)
    ) == expected_pairs


def test_threshold_diagnostics_persist_candidate_rejection_details(monkeypatch, tmp_path):
    predictions = _predictions().copy()
    predictions["Set"] = "AAA<-BBB"
    predictions.loc[predictions["Direction"] == "Up", "Probability"] = 0.10
    config = _config(threshold_calibration_min_signals_per_window=2)

    result, _ = _controlled_result_with_predictions(monkeypatch, predictions, config)
    diagnostics = result.run_configuration["threshold_diagnostics"]["Up"]

    assert diagnostics["candidate_threshold_count"] > 0
    assert diagnostics["eligible_threshold_count"] == 0
    assert diagnostics["min_signals_per_window"] == 2
    assert diagnostics["min_window_fraction"] == 1.0
    assert diagnostics["best_rejected_threshold"] is not None
    assert diagnostics["candidates"]
    assert {
        "threshold", "total_signals", "min_signals_in_any_window",
        "signal_counts_by_window", "eligible_window_fraction", "hit_rate",
        "average_return", "median_return", "mfe", "mae", "stability",
        "precision", "directional_return_mean", "opposite_move_frequency",
        "precision_stability", "return_stability", "robust_sample",
        "best_precision", "precision_tolerance", "within_precision_tolerance",
        "eligible", "rejection_reason", "selected", "selection_reason",
    } <= set(diagnostics["candidates"][0])
    assert {
        "RejectionReason", "SignalCountsByWindow", "MinSignalsInAnyWindow",
        "EligibleWindowFraction", "HitRate", "AverageReturn", "MedianReturn",
        "MFE", "MAE", "Stability", "Precision", "DirectionalReturnMean",
        "OppositeMoveFrequency", "RobustSample", "SelectionReason",
    } <= set(result.calibration.metrics_by_threshold)

    from rstock.threshold_calibration import write_threshold_calibration_results

    write_threshold_calibration_results(result, tmp_path)
    assert (tmp_path / "threshold_diagnostics.json").exists()
    assert (tmp_path / "threshold_diagnostics_by_set.json").exists()


def test_both_eligible_directions_still_apply_frozen_thresholds(monkeypatch):
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    predictions = _economically_viable_predictions().copy()
    predictions["Set"] = "AAA<-BBB"
    calls = []
    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        lambda *args, **kwargs: predictions,
    )
    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_holdout_probabilities",
        lambda *args, **kwargs: calls.append("holdout") or predictions,
    )

    result = run_controlled_threshold_calibration(
        prepared, generated, _config(), combinations_per_target=1,
        min_train_size=2, test_size=2, step_size=2, final_holdout_size=3,
        evaluate_final_holdout=True,
    )

    assert calls == ["holdout"]
    assert result.run_configuration["outcome"] == "completed"
    assert result.run_configuration["holdout_evaluated"] is True
    assert not result.holdout_metrics.empty


def test_final_threshold_v2_uses_every_qualified_set_without_resampling(monkeypatch):
    prepared = pd.DataFrame(
        {"placeholder": np.arange(20)},
        index=pd.bdate_range("2025-01-01", periods=20),
    )
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    sampled_sizes = []

    def fake_probabilities(development, sampled, *args, **kwargs):
        sampled_sizes.append(len(sampled))
        return _predictions()

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        fake_probabilities,
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        _config(final_holdout_size=3),
        combinations_per_target=1,
        sampling_policy=GLOBAL_STRATIFIED_V2,
        evaluate_final_holdout=False,
    )

    assert sampled_sizes == [len(generated)]
    assert result.run_configuration["sampling_manifest"]["sample_size"] == len(
        generated
    )


def test_threshold_workflow_summary_reports_no_eligible_threshold(monkeypatch, tmp_path):
    from rstock.application import workflows

    controlled = SimpleNamespace(
        calibration=SimpleNamespace(selected_thresholds={
            "Up": {"status": "no_eligible_threshold", "threshold": None},
            "Down": {"status": "selected", "threshold": 0.6},
        }),
        holdout_metrics=pd.DataFrame(),
        run_configuration={
            "outcome": "completed_no_eligible_threshold",
            "threshold_diagnostics": {"Up": {"eligible_threshold_count": 0}},
            "missing_frozen_thresholds": [{"set": "AAA<-BBB", "direction": "Up"}],
            "holdout_skipped_reason": "no_eligible_frozen_threshold",
            "holdout_combination_counts": {
                "Up": {
                    "total_combinations": 1,
                    "eligible_combinations": 0,
                    "evaluated_combinations": 0,
                    "skipped_combinations": 1,
                    "exclusion_reasons": {"no_eligible_threshold": 1},
                },
                "Down": {
                    "total_combinations": 1,
                    "eligible_combinations": 1,
                    "evaluated_combinations": 0,
                    "skipped_combinations": 1,
                    "exclusion_reasons": {"no_holdout_predictions": 1},
                },
            },
            "missing_threshold_count_up": 1,
            "missing_threshold_count_down": 0,
        },
    )
    monkeypatch.setattr(
        workflows, "_prepared_experiment",
        lambda *args, **kwargs: (pd.DataFrame(), pd.DataFrame(), {}),
    )
    monkeypatch.setattr(
        workflows, "run_controlled_threshold_calibration",
        lambda *args, **kwargs: controlled,
    )
    monkeypatch.setattr(workflows, "write_threshold_calibration_results", lambda *args: None)
    spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
    )

    summary = workflows._threshold_calibration(spec, tmp_path, None, None)

    assert summary["outcome"] == "completed_no_eligible_threshold"
    assert summary["holdout_skipped_reason"] == "no_eligible_frozen_threshold"
    assert summary["missing_frozen_thresholds"] == [
        {"set": "AAA<-BBB", "direction": "Up"}
    ]
    assert summary["missing_threshold_count_up"] == 1
    assert summary["missing_threshold_count_down"] == 0
    assert summary["holdout_combination_counts"]["Up"]["skipped_combinations"] == 1


def test_threshold_calibration_reuses_qualified_sets_from_its_walk_forward_source(tmp_path):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository

    runs = RunRepository(tmp_path / "runs")
    source_spec = ExperimentSpec(
        JobType.WALK_FORWARD,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB", "CCC"),
        target_symbols=("AAA",),
        context_symbols=("BBB", "CCC"),
    )
    source_run = runs.create(source_spec)
    results = runs.run_directory(source_run) / "results"
    results.mkdir()
    pd.DataFrame([
        {"Set": '["AAA","BBB"]', "Eligible": True},
        {"Set": '["AAA","CCC"]', "Eligible": False},
    ]).to_csv(results / "qualification.csv", index=False)
    runs.transition(source_run, JobStatus.RUNNING)
    runs.transition(source_run, JobStatus.COMPLETED)
    calibration_spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB", "CCC"),
        target_symbols=("AAA",),
        context_symbols=("BBB", "CCC"),
        source_walk_forward_run=source_run,
    )

    generated = workflows._qualified_sets_from_walk_forward_source(calibration_spec)

    assert generated.to_dict("records") == [{"V0": "AAA", "V1": "BBB"}]


def test_threshold_xgboost_resolution_uses_frozen_snapshot_before_reference(tmp_path):
    from rstock.application import workflows

    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
        source_xgboost_calibration_run="missing-source-is-not-read",
        frozen_xgboost_parameters={
            "Up": XGBoostParameters(2, 0.03, 70).as_dict(),
            "Down": XGBoostParameters(4, 0.08, 90).as_dict(),
        },
    )

    resolved = workflows._resolve_threshold_xgboost_parameters(spec)

    assert resolved.source == "frozen_snapshot"
    assert resolved.up.max_depth == 2
    assert resolved.down.max_depth == 4


def test_threshold_xgboost_resolution_loads_referenced_calibration(tmp_path):
    import json
    from rstock.application import workflows
    from rstock.application.repository import RunRepository

    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    runs = RunRepository(tmp_path / "runs")
    source = runs.create(ExperimentSpec(
        JobType.XGBOOST_CALIBRATION, config, symbols=("AAA", "BBB")
    ))
    results = runs.run_directory(source) / "results"
    results.mkdir()
    (results / "selected_configurations.json").write_text(json.dumps({
        "Up": {"parameters": XGBoostParameters(3, 0.04, 60).as_dict()},
        "Down": {"parameters": XGBoostParameters(5, 0.09, 100).as_dict()},
    }), encoding="utf-8")
    runs.transition(source, JobStatus.RUNNING)
    runs.transition(source, JobStatus.COMPLETED)
    spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
        source_xgboost_calibration_run=source,
    )

    resolved = workflows._resolve_threshold_xgboost_parameters(spec)

    assert resolved.source == "referenced_calibration"
    assert resolved.up.max_depth == 3
    assert resolved.down.max_depth == 5


def test_threshold_xgboost_resolution_uses_current_config_for_new_run(tmp_path):
    from rstock.application import workflows

    config = replace(
        DEFAULT_CONFIG, project_root=tmp_path,
        xgb_max_depth=6, xgb_eta=0.07, xgb_rounds=77,
    )
    resolved = workflows._resolve_threshold_xgboost_parameters(ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION, config, symbols=("AAA", "BBB")
    ))

    assert resolved.source == "rstock_config"
    assert resolved.up.as_dict() == resolved.down.as_dict()
    assert resolved.up.max_depth == 6
    assert resolved.up.eta == 0.07
    assert resolved.up.num_boost_round == 77


def test_historical_threshold_spec_uses_documented_legacy_fallback(tmp_path):
    from rstock.application import workflows

    snapshot = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_max_depth=7),
        symbols=("AAA", "BBB"),
    ).to_dict()
    for name in (
        "source_xgboost_calibration_run",
        "frozen_xgboost_parameters",
        "frozen_xgboost_parameters_sha256",
        "xgboost_resolution_version",
    ):
        snapshot.pop(name)

    resolved = workflows._resolve_threshold_xgboost_parameters(
        ExperimentSpec.from_dict(snapshot)
    )

    assert resolved.source == "legacy_fallback"
    assert resolved.up == EXPERIMENTAL_XGBOOST_PARAMETERS
    assert resolved.down == EXPERIMENTAL_XGBOOST_PARAMETERS


@pytest.mark.parametrize(
    "changes",
    [
        {"threshold_calibration_min_signals_per_window": 0},
        {"threshold_calibration_min_robust_signals": 0},
        {"threshold_calibration_min_window_fraction": 0.0},
        {"threshold_calibration_min_window_fraction": 1.1},
        {"threshold_calibration_precision_tolerance": -0.01},
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
