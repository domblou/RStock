from dataclasses import replace
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.combinations import generate_symbol_sets
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


def _controlled_result_with_predictions(monkeypatch, predictions, config):
    index = pd.bdate_range("2025-01-01", periods=12)
    prepared = pd.DataFrame({"placeholder": np.arange(12)}, index=index)
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    holdout_called = False

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_development_probabilities",
        lambda *args, **kwargs: predictions,
    )

    def unexpected_holdout(*args, **kwargs):
        nonlocal holdout_called
        holdout_called = True
        raise AssertionError("Holdout must not run without both frozen directions")

    monkeypatch.setattr(
        "rstock.threshold_calibration.generate_holdout_probabilities",
        unexpected_holdout,
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


@pytest.mark.parametrize("missing_direction", ["Up", "Down"])
def test_no_eligible_direction_skips_frozen_holdout_without_crashing(
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

    assert holdout_called is False
    assert result.holdout_predictions.empty
    assert result.holdout_metrics.empty
    assert result.run_configuration["outcome"] == "completed_no_eligible_threshold"
    assert result.run_configuration["holdout_evaluated"] is False
    assert result.run_configuration["holdout_skipped_reason"] == "no_eligible_frozen_threshold"
    assert any(
        item["direction"] == missing_direction
        for item in result.run_configuration["missing_frozen_thresholds"]
    )


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


@pytest.mark.parametrize(
    "changes",
    [
        {"threshold_calibration_min_signals_per_window": 0},
        {"threshold_calibration_min_robust_signals": 0},
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
