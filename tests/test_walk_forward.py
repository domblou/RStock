import logging
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.combinations import generate_symbol_sets
from rstock.checkpoints import CheckpointManager
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.streaming_walk_forward import run_streamed_walk_forward
import rstock.walk_forward as walk_forward
from rstock.walk_forward import (
    evaluate_prefilter_walk_forward,
    evaluate_walk_forward,
    expanding_windows,
    rolling_windows,
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


def test_rolling_windows_keep_a_fixed_train_and_shift_both_boundaries():
    windows = rolling_windows(504, train_size=252, test_size=63, step_size=63)

    assert [(window.train_slice, window.test_slice) for window in windows[:4]] == [
        (slice(0, 252), slice(252, 315)),
        (slice(63, 315), slice(315, 378)),
        (slice(126, 378), slice(378, 441)),
        (slice(189, 441), slice(441, 504)),
    ]
    assert {window.train_slice.stop - window.train_slice.start for window in windows} == {252}


def test_rolling_windows_reject_configuration_without_a_test_window():
    with pytest.raises(ValueError, match="rolling walk-forward test window"):
        rolling_windows(252, train_size=252, test_size=63, step_size=63)


def test_prefilter_skips_insufficient_pair_and_keeps_valid_pair(caplog, tmp_path):
    index = pd.bdate_range("2025-01-01", periods=40)
    signal = np.arange(len(index)) % 2
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB")):
        shifted = np.roll(signal, offset)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 1.0
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 1.0
    prepared = prepare_dataset(stock, ["AAA", "BBB"])
    for lag in range(1, 4):
        prepared[f"EA_intraday_J-{lag}"] = np.nan
    generated = pd.DataFrame([
        {"V0": "AAA", "V1": "BBB"},
        {"V0": "AAA", "V1": "EA"},
    ])
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        combination_workers=1,
        predictor_prefilter_batch_size=2,
        xgb_rounds=1,
        xgb_nthread=1,
        qualification_min_windows=1,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=0,
        qualification_max_auc_std=1.0,
    )

    with caplog.at_level(logging.WARNING, logger="rstock.walk_forward"):
        result = evaluate_prefilter_walk_forward(
            prepared,
            generated,
            config,
            min_train_size=10,
            test_size=5,
            step_size=5,
            final_holdout_size=5,
        )

    skipped = result.qualification.set_index("Set").loc['["AAA","EA"]']
    assert len(result.qualification) == 2
    assert not skipped["Eligible"]
    assert skipped["IneligibilityReasons"] == (
        '["insufficient_walk_forward_observations"]'
    )
    assert skipped["RawObservations"] == 40
    assert skipped["ModelObservations"] == skipped["DevelopmentObservations"] == 0
    assert skipped["MinimumRequiredObservations"] == 11
    assert result.telemetry["pairs_attempted"] == 2
    assert result.telemetry["pairs_admissible"] == 1
    assert result.telemetry["pairs_skipped"] == 1
    assert result.exploitable_targets == ("AAA",)
    assert result.excluded_targets == {}
    assert "target=AAA" in caplog.text
    assert "development=0" in caplog.text


def test_prefilter_does_not_hide_unrelated_value_errors(monkeypatch, tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    prepared = pd.DataFrame(index=pd.bdate_range("2025-01-01", periods=2))
    context = (prepared, config, prepared.index[-1], {}, 1, 1, 1)

    def fail(*args, **kwargs):
        raise ValueError("structural failure")

    monkeypatch.setattr(walk_forward, "_walk_forward_combination", fail)
    with pytest.raises(ValueError, match="structural failure"):
        walk_forward._prefilter_combination({"V0": "AAA", "V1": "BBB"}, context, None)


def test_prefilter_continues_150_target_population_with_missing_ea(monkeypatch, tmp_path):
    symbols = [f"S{index:03d}" for index in range(149)] + ["EA"]
    generated = generate_symbol_sets(symbols, 1)
    prepared = pd.DataFrame(index=pd.bdate_range("2025-01-01", periods=2))
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        combination_workers=1,
        predictor_prefilter_batch_size=1_000,
        final_holdout_size=1,
    )

    def fake_prefilter(values, *args):
        target, predictor = values["V0"], values["V1"]
        skipped = "EA" in (target, predictor)
        return {
            "Set": f"{target}<-{predictor}",
            "Observation": target,
            "Predictors": f'["{predictor}"]',
            "Eligible": False,
            "PctWindowsAboveRandom": np.nan,
            "ROCAUCMedian": np.nan,
            "ROCAUCWorst": np.nan,
            "ROCAUCStd": np.nan,
            "PRAUCMedian": np.nan,
            "PrefilterSkipReason": (
                "insufficient_walk_forward_observations" if skipped else None
            ),
        }

    monkeypatch.setattr(walk_forward, "_prefilter_combination", fake_prefilter)
    result = evaluate_prefilter_walk_forward(
        prepared,
        generated,
        config,
        min_train_size=1,
        test_size=1,
        step_size=1,
        final_holdout_size=1,
    )

    assert len(result.qualification) == 22_350
    assert result.telemetry["pairs_attempted"] == 22_350
    assert result.telemetry["pairs_admissible"] == 22_052
    assert result.telemetry["pairs_skipped"] == 298
    assert result.telemetry["pair_coverage_percent"] == pytest.approx(98.6666666667)
    assert result.telemetry["targets_requested"] == 150
    assert result.telemetry["targets_exploitable"] == 149
    assert result.excluded_targets == {"EA": "insufficient_walk_forward_observations"}
    assert "EA" not in result.exploitable_targets


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
    assert {
        "predictive_quality_score", "stability_score", "holdout_score",
        "signal_quality_score", "sample_adequacy_score",
        "model_selection_score", "model_selection_rank",
    } <= set(result.selection_results)
    assert "model_selection" in result.run_configuration
    assert result.run_configuration["model_selection"]["scale"] == "0-100"
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

    prefilter_result = evaluate_prefilter_walk_forward(
        prepared,
        generated,
        config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        min_train_size=10,
        test_size=5,
        step_size=5,
        final_holdout_size=5,
    )
    pd.testing.assert_frame_equal(result.qualification, prefilter_result.qualification)
    assert prefilter_result.telemetry["qualification_rows"] == len(result.qualification)

    rolling_config = replace(
        config,
        walk_forward_window_mode="rolling",
        walk_forward_train_size=10,
    )
    rolling = evaluate_walk_forward(
        prepared,
        generated,
        rolling_config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        test_size=5,
        step_size=5,
        final_holdout_size=5,
    )
    assert set(rolling.windows["TrainObservations"]) == {10}
    assert rolling.windows.groupby("Set")["TrainStart"].nunique().eq(3).all()
    assert (rolling.windows["TestEnd"] < rolling.final_holdout["FinalTestStart"].min()).all()
    rolling_prefilter = evaluate_prefilter_walk_forward(
        prepared,
        generated,
        rolling_config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        test_size=5,
        step_size=5,
        final_holdout_size=5,
    )
    pd.testing.assert_frame_equal(
        rolling.qualification,
        rolling_prefilter.qualification,
    )


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
        prepared,
        generated,
        replace(base, combination_workers=1, walk_forward_batch_size=1),
        **arguments,
    )
    parallel = evaluate_walk_forward(
        prepared,
        generated,
        replace(base, combination_workers=2, walk_forward_batch_size=2),
        **arguments,
    )
    large_batch = evaluate_walk_forward(
        prepared,
        generated,
        replace(base, combination_workers=1, walk_forward_batch_size=100),
        **arguments,
    )
    for name in ("windows", "predictions", "qualification", "final_holdout"):
        pd.testing.assert_frame_equal(getattr(serial, name), getattr(parallel, name))
        pd.testing.assert_frame_equal(getattr(serial, name), getattr(large_batch, name))


def test_streamed_walk_forward_matches_reference_artifacts(tmp_path):
    index = pd.bdate_range("2024-01-01", periods=30)
    signal = np.arange(len(index)) % 2
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB")):
        shifted = np.roll(signal, offset)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 1.0
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 1.0
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        combination_workers=1,
        walk_forward_batch_size=1,
        final_holdout_batch_size=1,
        xgb_rounds=1,
        xgb_nthread=1,
        walk_forward_min_train_size=10,
        walk_forward_test_size=5,
        walk_forward_step_size=5,
        final_holdout_size=5,
        qualification_min_windows=3,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    prepared = prepare_dataset(stock, ["AAA", "BBB"])
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    reference = evaluate_walk_forward(prepared, generated, config)
    reference_path = tmp_path / "reference"
    write_walk_forward_results(reference, reference_path)
    run_path = tmp_path / "run"
    checkpoint = CheckpointManager(
        run_path,
        run_id="run",
        job_type="walk_forward",
        configuration_fingerprint="fixture",
        batch_sizes={
            "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
            "walk_forward": config.walk_forward_batch_size,
            "final_holdout": config.final_holdout_batch_size,
        },
    )
    streamed_path = run_path / "_working"

    run_streamed_walk_forward(
        prepared,
        generated,
        config,
        checkpoint,
        streamed_path,
    )

    for name in (
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
    ):
        expected = pd.read_csv(reference_path / name)
        actual = pd.read_csv(streamed_path / name)
        pd.testing.assert_frame_equal(actual, expected, check_dtype=False, atol=1e-12)

    large_config = replace(
        config,
        combination_workers=2,
        walk_forward_batch_size=100,
        final_holdout_batch_size=100,
    )
    large_run = tmp_path / "large-run"
    large_checkpoint = CheckpointManager(
        large_run,
        run_id="large-run",
        job_type="walk_forward",
        configuration_fingerprint="large-fixture",
        batch_sizes={
            "predictor_prefilter_walk_forward": large_config.predictor_prefilter_batch_size,
            "walk_forward": large_config.walk_forward_batch_size,
            "final_holdout": large_config.final_holdout_batch_size,
        },
    )
    large_path = large_run / "_working"
    run_streamed_walk_forward(
        prepared, generated, large_config, large_checkpoint, large_path
    )
    for name in (
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
    ):
        small = pd.read_csv(streamed_path / name)
        large = pd.read_csv(large_path / name)
        pd.testing.assert_frame_equal(small, large, check_dtype=False, atol=1e-12)
