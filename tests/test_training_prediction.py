from dataclasses import replace

import numpy as np
import pandas as pd

from rstock.combinations import generate_symbol_sets
from rstock.calendars import next_market_session
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.prediction import predict_saved_models
from rstock.training import train_models


def test_train_persist_reload_and_predict_end_to_end(tmp_path):
    index = pd.bdate_range("2024-01-01", periods=60)
    alternating = np.arange(len(index)) % 2
    stock = pd.DataFrame(index=index)
    for offset, symbol in enumerate(["AAA", "BBB"]):
        signal = np.roll(alternating, offset)
        stock[f"{symbol}.Open"] = 100.0
        stock[f"{symbol}.Close"] = np.where(signal, 102.0, 100.0)
        stock[f"{symbol}.High"] = np.maximum(stock[f"{symbol}.Close"], 100.0) + 1.0
        stock[f"{symbol}.Low"] = np.minimum(stock[f"{symbol}.Close"], 100.0) - 1.0

    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        xgb_rounds=1,
        xgb_max_depth=2,
        xgb_nthread=1,
        keep_predictor_under=1.1,
    )
    prepared = prepare_dataset(
        stock,
        ["AAA", "BBB"],
        config.intraday_target_threshold,
        config.lag_depth,
    )
    generated = generate_symbol_sets(["AAA", "BBB"], config.permutation_depth)

    trained = train_models(
        prepared, generated, config, {"AAA": "XNYS", "BBB": "XNYS"}
    )
    predicted = predict_saved_models(prepared, trained.survey_sets, config)

    assert len(trained.survey_sets) == 2
    assert {
        "TN", "FP", "FN", "TP", "Accuracy", "Precision", "Recall", "F1", "ROCAUC"
    } <= set(trained.evaluated_sets.columns)
    assert trained.evaluated_sets["Err"].equals(
        1.0 - trained.evaluated_sets["Accuracy"]
    )
    assert (
        trained.evaluated_sets["TrainEnd"]
        < trained.evaluated_sets["TestStart"]
    ).all()
    assert set(trained.evaluated_sets["TrainRows"]) == {39}
    assert set(trained.evaluated_sets["TestRows"]) == {18}
    assert len(list(config.models_path.glob("*.ubj"))) == 2
    assert len(list(config.models_path.glob("*.metadata.json"))) == 2
    assert len(predicted) == 2
    assert set(predicted["BinaryPrediction"]) <= {0, 1}
    assert set(predicted["BinaryResult"]) == {-1}
    assert set(predicted["Feature1"]) == {"AAA", "BBB"}
    assert set(predicted["MarketCalendar"]) == {"XNYS"}
    assert set(predicted["TargetThreshold"]) == {0.01}
    assert set(predicted["LagDepth"]) == {3}
    assert set(predicted["TargetDefinition"]) == {"(Close_J / Open_J) - 1"}
    assert predicted.iloc[0]["Date"] == next_market_session(index[-1], "XNYS")


def test_training_rejects_non_date_index(tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, permutation_depth=1)
    prepared = pd.DataFrame(
        {
            "AAA.intraday_target": [0, 1],
            "BBB_intraday_J-1": [1, 0],
            "BBB_intraday_J-2": [1, 0],
            "BBB_intraday_J-3": [1, 0],
        }
    )
    generated = generate_symbol_sets(["AAA", "BBB"], 1)

    try:
        train_models(prepared, generated, config, {"AAA": "XNYS", "BBB": "XNYS"})
    except TypeError as error:
        assert "DatetimeIndex" in str(error)
    else:
        raise AssertionError("training should reject a non-date index")
