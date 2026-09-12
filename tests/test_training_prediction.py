from dataclasses import replace

import numpy as np
import pandas as pd

from rstock.combinations import generate_symbol_sets
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

    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        xgb_rounds=1,
        xgb_max_depth=2,
        xgb_nthread=1,
        keep_predictor_under=1.1,
        shuffle_seed=99,
    )
    prepared = prepare_dataset(stock, ["AAA", "BBB"], config.up_down_threshold)
    generated = generate_symbol_sets(["AAA", "BBB"], config.permutation_depth)

    trained = train_models(prepared, generated, config)
    predicted = predict_saved_models(prepared, trained.survey_sets, config)

    assert len(trained.survey_sets) == 2
    assert len(list(config.models_path.glob("*.ubj"))) == 2
    assert len(list(config.models_path.glob("*.metadata.json"))) == 2
    assert len(predicted) == 2
    assert set(predicted["BinaryPrediction"]) <= {0, 1}
    assert set(predicted["BinaryResult"]) == {-1}
    assert set(predicted["Feature1"]) == {"AAA", "BBB"}
    assert predicted.iloc[0]["Date"] == (index[-1] + pd.Timedelta(days=1)).date().isoformat()
