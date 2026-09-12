import numpy as np
import pandas as pd

from rstock.features import prepare_dataset, prepare_prediction_row, predictor_columns


def _stock_frame():
    # Friday to Monday and a larger earlier gap prove that lags count observations,
    # not civil days.
    return pd.DataFrame(
        {
            "AAA.Open": [100.0, 110.0, 105.0, 120.0],
            "AAA.High": [112.0, 111.0, 116.0, 121.0],
            "AAA.Low": [99.0, 104.0, 104.0, 118.0],
            "AAA.Close": [110.0, 105.0, 115.0, 120.0],
            "BBB.Open": [50.0, 50.0, 52.0, 51.0],
            "BBB.High": [51.0, 53.0, 53.0, 53.0],
            "BBB.Low": [49.0, 49.0, 50.0, 50.0],
            "BBB.Close": [50.0, 52.0, 51.0, 52.0],
        },
        index=pd.to_datetime(
            ["2024-01-02", "2024-01-05", "2024-01-08", "2024-01-09"]
        ),
    )


def test_three_returns_and_intraday_target_are_calculated_exactly():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01, 3)

    assert np.isclose(prepared.loc["2024-01-02", "AAA.intraday_return"], 0.10)
    assert pd.isna(prepared.loc["2024-01-02", "AAA.overnight_return"])
    assert pd.isna(prepared.loc["2024-01-02", "AAA.close_to_close_return"])
    assert np.isclose(prepared.loc["2024-01-05", "AAA.overnight_return"], 0.0)
    assert np.isclose(
        prepared.loc["2024-01-05", "AAA.intraday_return"], 105 / 110 - 1
    )
    assert np.isclose(
        prepared.loc["2024-01-05", "AAA.close_to_close_return"], 105 / 110 - 1
    )
    assert prepared["AAA.intraday_target"].tolist() == [1.0, 0.0, 1.0, 0.0]
    assert prepared["AAA.intraday_down_target"].tolist() == [0.0, 1.0, 0.0, 0.0]
    assert np.isclose(prepared.loc["2024-01-05", "AAA.mfe"], 111 / 110 - 1)
    assert np.isclose(prepared.loc["2024-01-05", "AAA.mae"], 104 / 110 - 1)


def test_lags_follow_observed_sessions_across_weekends_and_holidays():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01, 3)

    assert pd.isna(prepared.loc["2024-01-02", "AAA_intraday_J-1"])
    assert pd.isna(prepared.loc["2024-01-05", "AAA_intraday_J-2"])
    assert np.isclose(prepared.loc["2024-01-09", "AAA_intraday_J-1"], 115 / 105 - 1)
    assert np.isclose(prepared.loc["2024-01-09", "AAA_intraday_J-2"], 105 / 110 - 1)
    assert np.isclose(prepared.loc["2024-01-09", "AAA_intraday_J-3"], 0.10)


def test_target_session_values_never_enter_its_predictors():
    stock = _stock_frame()
    before = prepare_dataset(stock, ["AAA", "BBB"], 0.01, 3)
    changed = stock.copy()
    changed.loc["2024-01-09", "AAA.Close"] = 240.0
    after = prepare_dataset(changed, ["AAA", "BBB"], 0.01, 3)

    lag_columns = [f"AAA_intraday_J-{lag}" for lag in range(1, 4)]
    pd.testing.assert_series_equal(
        before.loc["2024-01-09", lag_columns],
        after.loc["2024-01-09", lag_columns],
    )
    assert before.loc["2024-01-09", "AAA.intraday_target"] != after.loc[
        "2024-01-09", "AAA.intraday_target"
    ]


def test_prediction_row_uses_latest_three_completed_intraday_sessions():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01, 3)
    row = prepare_prediction_row(
        prepared, target_date="2024-01-10", lag_depth=3
    )

    assert row.index[0] == pd.Timestamp("2024-01-10")
    assert not any(name.endswith(".intraday_target") for name in row)
    assert np.isclose(row.iloc[0]["AAA_intraday_J-1"], 0.0)
    assert np.isclose(row.iloc[0]["AAA_intraday_J-2"], 115 / 105 - 1)
    assert np.isclose(row.iloc[0]["AAA_intraday_J-3"], 105 / 110 - 1)


def test_symbol_specific_gap_uses_last_real_observations_without_imputation():
    stock = _stock_frame().copy()
    stock.loc[
        "2024-01-08", ["AAA.Open", "AAA.High", "AAA.Low", "AAA.Close"]
    ] = float("nan")

    prepared = prepare_dataset(stock, ["AAA", "BBB"], 0.01, 3)

    assert pd.isna(prepared.loc["2024-01-08", "AAA.intraday_target"])
    assert np.isclose(
        prepared.loc["2024-01-09", "AAA_intraday_J-1"], 105 / 110 - 1
    )
    assert np.isclose(prepared.loc["2024-01-09", "AAA_intraday_J-2"], 0.10)
    assert pd.isna(prepared.loc["2024-01-09", "AAA_intraday_J-3"])


def test_zero_open_produces_missing_intraday_return_and_target():
    stock = _stock_frame().copy()
    stock.loc["2024-01-05", "AAA.Open"] = 0.0

    prepared = prepare_dataset(stock, ["AAA", "BBB"], 0.01, 3)

    assert pd.isna(prepared.loc["2024-01-05", "AAA.intraday_return"])
    assert pd.isna(prepared.loc["2024-01-05", "AAA.intraday_target"])


def test_down_target_includes_return_exactly_at_negative_threshold():
    stock = pd.DataFrame(
        {
            "AAA.Open": [100.0],
            "AAA.High": [101.0],
            "AAA.Low": [98.0],
            "AAA.Close": [99.0],
        },
        index=pd.to_datetime(["2024-01-02"]),
    )

    prepared = prepare_dataset(stock, ["AAA"], 0.01, 3, 0.01)

    assert prepared.iloc[0]["AAA.intraday_target"] == 0
    assert prepared.iloc[0]["AAA.intraday_down_target"] == 1


def test_invalid_or_duplicate_dates_are_rejected():
    duplicate = _stock_frame()
    duplicate.index = pd.to_datetime(
        ["2024-01-02", "2024-01-02", "2024-01-08", "2024-01-09"]
    )

    try:
        prepare_dataset(duplicate, ["AAA", "BBB"])
    except ValueError as error:
        assert "one row per date" in str(error)
    else:
        raise AssertionError("duplicate dates should be rejected")


def test_predictor_columns_include_all_lags_but_never_current_returns():
    prepared = prepare_dataset(_stock_frame(), ["AAA", "BBB"], 0.01, 3)

    assert predictor_columns(prepared, ["BBB"], 3) == [
        "BBB_intraday_J-1",
        "BBB_intraday_J-2",
        "BBB_intraday_J-3",
    ]
    assert predictor_columns(prepared, ["BBB"], 3, r".*") == [
        "BBB_intraday_J-1",
        "BBB_intraday_J-2",
        "BBB_intraday_J-3",
        "wday",
        "yday",
        "mon",
    ]
