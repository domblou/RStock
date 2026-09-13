from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.walk_forward import (
    evaluate_walk_forward,
    expanding_windows,
    write_walk_forward_results,
)


def test_expanding_windows_are_chronological_and_include_final_partial_window():
    windows = expanding_windows(11, min_train_size=4, test_size=3, step_size=3)

    assert [(window.train_slice, window.test_slice) for window in windows] == [
        (slice(0, 4), slice(4, 7)),
        (slice(0, 7), slice(7, 10)),
        (slice(0, 10), slice(10, 11)),
    ]


def test_walk_forward_rejects_insufficient_history():
    with pytest.raises(ValueError, match="Not enough observations"):
        expanding_windows(10, min_train_size=10, test_size=2, step_size=2)


def test_walk_forward_reports_windows_predictions_and_recomputed_aggregates(tmp_path):
    index = pd.bdate_range("2024-01-01", periods=30)
    signal = np.arange(len(index)) % 2
    stock = pd.DataFrame(
        {
            "AAA.Open": 100.0,
            "AAA.Close": np.where(signal, 102.0, 100.0),
            "BBB.Open": 100.0,
            "BBB.Close": np.where(np.roll(signal, 1), 102.0, 100.0),
        },
        index=index,
    )
    for symbol in ("AAA", "BBB"):
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 1.0
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 1.0
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        xgb_rounds=1,
        xgb_nthread=1,
        qualification_min_windows=3,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    prepared = prepare_dataset(stock, ["AAA", "BBB"])
    generated = generate_symbol_sets(["AAA", "BBB"], 1)

    progress_events = []
    result = evaluate_walk_forward(
        prepared,
        generated,
        config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        min_train_size=10,
        test_size=5,
        step_size=5,
        final_holdout_size=5,
        progress_callback=progress_events.append,
    )

    assert len(result.windows) == 6
    assert (result.windows["TrainEnd"] < result.windows["TestStart"]).all()
    assert set(result.windows["TrainObservations"]) == {10, 15, 20}
    assert set(result.windows["MarketCalendar"]) == {"XNYS"}
    assert set(result.windows["TestObservations"]) == {2, 5}
    assert (result.windows["TestObservations"] == result.windows["Predictions"]).all()
    assert len(result.predictions) == 24
    assert result.aggregate_global.iloc[0]["Predictions"] == 24
    assert result.aggregate_global.iloc[0]["WindowEvaluations"] == 6
    assert len(result.aggregate_by_window) == 3
    assert set(result.aggregate_by_window["Sets"]) == {2}
    assert set(result.aggregate_by_set["Windows"]) == {3}
    assert {
        "OvernightReturn", "IntradayReturn", "CloseToCloseReturn",
        "IntradayTarget", "UpTarget", "DownTarget", "MFE", "MAE",
        "UpProbability", "UpPrediction", "DownProbability", "DownPrediction",
    } <= set(result.predictions.columns)
    assert (result.predictions["UpTarget"] == result.predictions["IntradayTarget"]).all()
    assert set(result.qualification["RowsLostToLags"]) == {3}
    assert {
        "UpAccuracy", "UpPrecision", "UpRecall", "UpF1", "UpROCAUC",
        "UpPRAUC", "UpPrevalence", "DownAccuracy", "DownROCAUC",
    } <= set(result.windows.columns)
    assert result.run_configuration["final_holdout_size"] == 5
    assert result.run_configuration["development_end"] < result.run_configuration["final_holdout_start"]
    assert len(result.qualification) == 2
    assert set(result.selection_results["Set"]) == set(result.qualification["Set"])
    assert len(result.final_holdout) == 2
    assert (result.final_holdout["FinalTrainEnd"] < result.final_holdout["FinalTestStart"]).all()
    assert (result.windows["TestEnd"] < result.final_holdout["FinalTestStart"].min()).all()
    assert result.final_holdout["FinalTestStart"].min() == index[-5]
    assert set(result.final_holdout_predictions["Date"]) == set(index[-5:])
    aggregation_complete = next(
        event
        for event in progress_events
        if event.stage == "aggregation" and event.substage == "completed"
    )
    timings = aggregation_complete.details["timings_seconds"]
    assert set(timings) == {
        "dataframe_creation",
        "aggregate_predictions",
        "aggregate_windows",
        "aggregate_risk",
    }
    assert all(isinstance(duration, float) and duration >= 0 for duration in timings.values())

    output = tmp_path / "walk-forward-output"
    write_walk_forward_results(result, output)
    assert {
        "windows.csv",
        "predictions.csv",
        "aggregate_by_window.csv",
        "aggregate_by_set.csv",
        "aggregate_global.csv",
        "qualification.csv",
        "final_holdout.csv",
        "final_holdout_predictions.csv",
        "selection_results.csv",
        "risk_by_window.csv",
        "risk_by_set.csv",
        "risk_global.csv",
        "final_holdout_risk.csv",
        "run_configuration.json",
    } == {path.name for path in output.iterdir()}

    altered_holdout = prepared.copy()
    outcome_columns = [
        name for name in altered_holdout if name.endswith(".intraday_target")
    ]
    altered_holdout.loc[index[-5:], outcome_columns] = (
        1 - altered_holdout.loc[index[-5:], outcome_columns]
    )
    altered_result = evaluate_walk_forward(
        altered_holdout,
        generated,
        config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        min_train_size=10,
        test_size=5,
        step_size=5,
        final_holdout_size=5,
    )
    pd.testing.assert_frame_equal(result.qualification, altered_result.qualification)


def test_combination_process_workers_preserve_walk_forward_results_and_seed(tmp_path):
    index = pd.bdate_range("2024-01-01", periods=30)
    signal = np.arange(len(index)) % 2
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB", "CCC")):
        shifted = np.roll(signal, offset)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 1.0
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 1.0
    base = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_rounds=1,
        xgb_nthread=1,
        xgb_seed=17,
        qualification_min_windows=1,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    prepared = prepare_dataset(stock, ["AAA", "BBB", "CCC"])
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    arguments = dict(min_train_size=10, test_size=5, step_size=5, final_holdout_size=5)
    serial = evaluate_walk_forward(
        prepared, generated, replace(base, combination_workers=1), **arguments
    )
    parallel = evaluate_walk_forward(
        prepared, generated, replace(base, combination_workers=2), **arguments
    )
    for name in ("windows", "predictions", "qualification", "final_holdout"):
        pd.testing.assert_frame_equal(getattr(serial, name), getattr(parallel, name))
