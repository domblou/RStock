from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.calibration import (
    _calibration_combination,
    _configuration_lookup,
    _window_metric_rows,
    deterministic_combination_sample,
    run_controlled_calibration,
    write_calibration_results,
)
from rstock.checkpoints import CheckpointManager
from rstock.combinations import generate_symbol_sets, symbols_from_set
from rstock.config import DEFAULT_CONFIG
from rstock.features import (
    intraday_down_target_column,
    intraday_target_column,
    predictor_columns,
    prepare_dataset,
)
from rstock.modeling import XGBoostParameters, fit_booster, predict_probabilities
from rstock.walk_forward import walk_forward_windows


def _prepared_history(periods: int = 65) -> pd.DataFrame:
    index = pd.bdate_range("2024-01-01", periods=periods)
    stock = pd.DataFrame(index=index)
    base = np.arange(periods)
    for offset, symbol in enumerate(("AAA", "BBB", "CCC")):
        intraday = np.where((base + offset) % 5 == 0, 0.02, -0.012)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = 100.0 * (1.0 + intraday)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 0.5
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 0.5
    return prepare_dataset(stock, ["AAA", "BBB", "CCC"])


def _test_config(tmp_path):
    return replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_nthread=1,
        xgb_rounds=2,
        xgb_max_depth=2,
        xgb_seed=17,
        combination_workers=1,
    )


def _candidates(config):
    return [
        XGBoostParameters(config.xgb_max_depth, config.xgb_eta, config.xgb_rounds),
        XGBoostParameters(1, 0.1, 6, 2, 0.8, 0.8, 0.1, 0.1, 2.0),
    ]


@pytest.mark.parametrize(
    ("kwargs", "message"),
    [
        ({"max_depth": 0, "eta": 0.1, "num_boost_round": 2}, "max_depth"),
        ({"max_depth": 1, "eta": 0, "num_boost_round": 2}, "eta"),
        ({"max_depth": 1, "eta": 0.1, "num_boost_round": 0}, "num_boost_round"),
        (
            {"max_depth": 1, "eta": 0.1, "num_boost_round": 2, "subsample": 1.1},
            "subsample",
        ),
        (
            {"max_depth": 1, "eta": 0.1, "num_boost_round": 2, "reg_alpha": -1},
            "reg_alpha",
        ),
    ],
)
def test_xgboost_parameters_reject_invalid_values(kwargs, message):
    with pytest.raises(ValueError, match=message):
        XGBoostParameters(**kwargs)


def test_fit_booster_forwards_all_controlled_parameters(monkeypatch, tmp_path):
    captured = {}

    class FakeXGBoost:
        @staticmethod
        def DMatrix(data, label=None, feature_names=None):
            return (data, label, feature_names)

        @staticmethod
        def train(parameters, matrix, num_boost_round, verbose_eval):
            captured.update(parameters)
            captured["num_boost_round"] = num_boost_round
            return object()

    monkeypatch.setattr("rstock.modeling.xgboost_module", lambda: FakeXGBoost)
    parameters = XGBoostParameters(3, 0.05, 9, 4, 0.7, 0.6, 0.2, 0.3, 2.5)
    frame = pd.DataFrame({"feature": [0.0, 1.0], "target": [0, 1]})
    fit_booster(
        frame,
        ["feature"],
        "target",
        _test_config(tmp_path),
        parameters=parameters,
    )

    assert captured == {
        "objective": "binary:logistic",
        "max_depth": 3,
        "eta": 0.05,
        "min_child_weight": 4,
        "subsample": 0.7,
        "colsample_bytree": 0.6,
        "gamma": 0.2,
        "reg_alpha": 0.3,
        "reg_lambda": 2.5,
        "nthread": 1,
        "seed": 17,
        "num_boost_round": 9,
    }


def test_combination_sampling_is_balanced_and_reproducible():
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    first = deterministic_combination_sample(generated, per_target=1, seed=1234)
    shuffled = generated.sample(frac=1, random_state=99).reset_index(drop=True)
    second = deterministic_combination_sample(shuffled, per_target=1, seed=1234)

    pd.testing.assert_frame_equal(first, second)
    assert first["V0"].value_counts().to_dict() == {"AAA": 1, "BBB": 1, "CCC": 1}


def test_calibration_is_reproducible_temporal_and_holdout_isolated(tmp_path):
    prepared = _prepared_history()
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    config = _test_config(tmp_path)
    arguments = dict(
        candidates=_candidates(config),
        combinations_per_target=1,
        min_train_size=20,
        test_size=10,
        step_size=10,
        final_holdout_size=5,
    )

    first = run_controlled_calibration(prepared, generated, config, **arguments)
    repeated = run_controlled_calibration(prepared, generated, config, **arguments)
    pd.testing.assert_frame_equal(
        first.development_by_configuration, repeated.development_by_configuration
    )
    assert first.selected_configurations == repeated.selected_configurations

    holdout_start = pd.Timestamp(first.run_configuration["final_holdout_start"])
    assert (first.development_by_window["TrainEnd"] < first.development_by_window["TestStart"]).all()
    assert (first.development_by_window["TestEnd"] < holdout_start).all()
    assert first.run_configuration["holdout_used_for_selection"] is False
    assert first.run_configuration["holdout_evaluation_count"] == 1

    altered = prepared.copy()
    target_columns = [name for name in altered if name.endswith("target")]
    altered.loc[altered.index >= holdout_start, target_columns] = (
        1 - altered.loc[altered.index >= holdout_start, target_columns]
    )
    changed_holdout = run_controlled_calibration(altered, generated, config, **arguments)
    pd.testing.assert_frame_equal(
        first.development_by_configuration,
        changed_holdout.development_by_configuration,
    )
    assert first.selected_configurations == changed_holdout.selected_configurations
    assert not first.holdout_metrics.equals(changed_holdout.holdout_metrics)

    output = tmp_path / "calibration"
    write_calibration_results(first, output)
    assert {
        "tested_parameters.csv",
        "development_metrics_by_configuration.csv",
        "development_metrics_by_window.csv",
        "selected_configurations.json",
        "baseline_vs_calibrated_development.csv",
        "holdout_metrics.csv",
        "holdout_predictions.csv",
            "sampled_combinations.csv",
            "sampling_manifest.json",
            "run_configuration.json",
    } == {path.name for path in output.iterdir()}


def test_calibration_combination_workers_preserve_development_metrics(tmp_path):
    prepared = _prepared_history()
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    base = _test_config(tmp_path)
    arguments = dict(
        candidates=_candidates(base), combinations_per_target=1,
        min_train_size=20, test_size=10, step_size=10, final_holdout_size=5,
    )
    serial = run_controlled_calibration(
        prepared, generated, replace(base, combination_workers=1), **arguments
    )
    parallel = run_controlled_calibration(
        prepared, generated, replace(base, combination_workers=2), **arguments
    )
    pd.testing.assert_frame_equal(
        serial.development_by_configuration, parallel.development_by_configuration
    )
    pd.testing.assert_frame_equal(serial.development_by_window, parallel.development_by_window)
    assert serial.selected_configurations == parallel.selected_configurations


def test_calibration_development_checkpoint_reuses_completed_batches(tmp_path):
    prepared = _prepared_history()
    generated = generate_symbol_sets(["AAA", "BBB", "CCC"], 1)
    config = _test_config(tmp_path)
    arguments = dict(
        candidates=_candidates(config), combinations_per_target=1,
        min_train_size=20, test_size=10, step_size=10, final_holdout_size=5,
    )
    checkpoint = CheckpointManager(
        tmp_path / "run",
        run_id="run",
        job_type="xgboost_calibration",
        configuration_fingerprint="test-fingerprint",
        batch_sizes={"xgboost_calibration": config.walk_forward_batch_size},
    )
    first = run_controlled_calibration(
        prepared, generated, config, checkpoint_manager=checkpoint, **arguments
    )
    repeated = run_controlled_calibration(
        prepared, generated, config, checkpoint_manager=checkpoint, **arguments
    )

    pd.testing.assert_frame_equal(
        first.development_by_configuration, repeated.development_by_configuration
    )
    pd.testing.assert_frame_equal(first.development_by_window, repeated.development_by_window)
    assert repeated.run_configuration["performance_telemetry"]["batches_reused"] == 1
    assert repeated.run_configuration["performance_telemetry"]["batches_calculated"] == 0


def test_reused_dmatrix_matches_legacy_per_configuration_training(tmp_path):
    development = _prepared_history(50)
    config = _test_config(tmp_path)
    candidates = _candidates(config)
    row = generate_symbol_sets(["AAA", "BBB", "CCC"], 1).iloc[0]
    observation, feature_symbols = symbols_from_set(row)
    names = predictor_columns(
        development, feature_symbols, config.lag_depth, config.date_feature_regex
    )
    up, down = intraday_target_column(observation), intraday_down_target_column(observation)
    model_data = development[[*names, up, down]].dropna()
    legacy: dict[tuple[str, str, int], dict[str, object]] = {}
    for window in walk_forward_windows(
        len(model_data), config, min_train_size=20, test_size=10, step_size=10
    ):
        train, test = model_data.iloc[window.train_slice], model_data.iloc[window.test_slice]
        for configuration, parameters in _configuration_lookup(candidates, config).items():
            for direction, outcome in (("Up", up), ("Down", down)):
                probabilities = predict_probabilities(
                    fit_booster(train, names, outcome, config, parameters=parameters), test, names
                )
                legacy[(configuration, direction, window.number)] = {
                    "actual": [test[outcome].astype(int).to_numpy()],
                    "predicted": [(probabilities >= config.prediction_threshold).astype(int)],
                    "probabilities": [probabilities],
                    "train_start": train.index.min(), "train_end": train.index.max(),
                    "test_start": test.index.min(), "test_end": test.index.max(), "sets": 1,
                }
    current = _calibration_combination(
        row.to_dict(),
        (development, config, tuple(candidates), _configuration_lookup(candidates, config), 20, 10, 10),
        None,
    )
    pd.testing.assert_frame_equal(
        _window_metric_rows(legacy), _window_metric_rows(current.buckets)
    )
