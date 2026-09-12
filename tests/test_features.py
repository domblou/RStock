import pandas as pd

from rstock.features import prepare_dataset, prepare_prediction_row, predictor_columns


def _stock_frame():
    return pd.DataFrame(
        {
            "AAA.Open": [100.0, 100.0, 100.0],
            "AAA.Close": [102.0, 100.5, 104.0],
            "BBB.Open": [50.0, 50.0, 50.0],
            "BBB.Close": [50.0, 51.0, 49.0],
        },
        index=pd.to_datetime(["2024-01-02", "2024-01-03", "2024-01-04"]),
    )


def test_prepare_dataset_preserves_threshold_lag_order_and_r_date_numbering():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01)

    assert prepared.index.tolist() == list(pd.to_datetime(["2024-01-04", "2024-01-03"]))
    assert prepared.loc["2024-01-04", "AAA.UPDW"] == 1
    assert prepared.loc["2024-01-04", "AAA.DAY_MINUS_1_UPDW"] == 0
    assert prepared.loc["2024-01-03", "AAA.DAY_MINUS_1_UPDW"] == 1
    assert prepared.loc["2024-01-04", ["wday", "yday", "mon"]].tolist() == [4, 3, 0]


def test_prediction_row_uses_latest_current_values_as_lagged_features():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01)
    row = prepare_prediction_row(prepared)

    assert row.index[0] == pd.Timestamp("2024-01-04")
    assert "AAA.UPDW" not in row
    assert row.iloc[0]["AAA.DAY_MINUS_1_UPDW"] == 1
    assert row.iloc[0]["BBB.DAY_MINUS_1_UPDW"] == 0


def test_predictor_columns_uses_explicit_symbols_and_optional_date_regex():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01)
    assert predictor_columns(prepared, ["BBB"]) == ["BBB.DAY_MINUS_1_UPDW"]
    assert predictor_columns(prepared, ["BBB"], r"^(wday|mon)$") == [
        "BBB.DAY_MINUS_1_UPDW", "wday", "mon"
    ]

