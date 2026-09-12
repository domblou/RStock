from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.walk_forward import evaluate_walk_forward, expanding_windows


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
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        xgb_rounds=1,
        xgb_nthread=1,
    )
    prepared = prepare_dataset(stock, ["AAA", "BBB"])
    generated = generate_symbol_sets(["AAA", "BBB"], 1)

    result = evaluate_walk_forward(
        prepared,
        generated,
        config,
        market_calendars={"AAA": "XNYS", "BBB": "XNYS"},
        min_train_size=10,
        test_size=5,
        step_size=5,
    )

    assert len(result.windows) == 8
    assert (result.windows["TrainEnd"] < result.windows["TestStart"]).all()
    assert set(result.windows["TrainObservations"]) == {10, 15, 20, 25}
    assert set(result.windows["MarketCalendar"]) == {"XNYS"}
    assert set(result.windows["TestObservations"]) == {4, 5}
    assert (result.windows["TestObservations"] == result.windows["Predictions"]).all()
    assert len(result.predictions) == 38
    assert result.aggregate_global.iloc[0]["Predictions"] == 38
    assert result.aggregate_global.iloc[0]["WindowEvaluations"] == 8
    assert len(result.aggregate_by_window) == 4
    assert set(result.aggregate_by_window["Sets"]) == {2}
    assert set(result.aggregate_by_set["Windows"]) == {4}
    assert {
        "Accuracy", "Precision", "Recall", "F1", "ROCAUC"
    } <= set(result.windows.columns)
