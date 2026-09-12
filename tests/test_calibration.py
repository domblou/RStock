from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.calibration import (
    deterministic_combination_sample,
    run_controlled_calibration,
    write_calibration_results,
)
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.modeling import XGBoostParameters, fit_booster


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
