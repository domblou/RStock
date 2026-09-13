import numpy as np
import pandas as pd
import pytest

import rstock.risk as risk
from rstock.risk import conditional_signal_metrics, intraday_risk_metrics


def _risk_frame():
    return pd.DataFrame(
        {
            "IntradayReturn": [-0.02, 0.01, 0.03, 0.0],
            "MFE": [0.01, 0.02, 0.04, 0.005],
            "MAE": [-0.03, -0.01, -0.005, -0.002],
            "UpPrediction": [1, 1, 0, 0],
            "DownPrediction": [1, 0, 0, 1],
        }
    )


def test_intraday_risk_metrics_cover_distribution_asymmetry_and_excursions():
    frame = _risk_frame()
    result = intraday_risk_metrics(frame, 0.01, 0.01)

    assert result["Observations"] == 4
    assert np.isclose(result["IntradayReturnMean"], 0.005)
    assert np.isclose(result["IntradayReturnMedian"], 0.005)
    assert np.isclose(result["IntradayReturnStd"], np.std(frame["IntradayReturn"]))
    assert np.isclose(result["IntradayReturnP10"], frame["IntradayReturn"].quantile(0.10))
    assert result["IntradayReturnBest"] == 0.03
    assert result["IntradayReturnWorst"] == -0.02
    assert result["UpFrequency"] == 0.5
    assert result["DownFrequency"] == 0.25
    assert np.isclose(result["MeanPositiveGain"], 0.02)
    assert result["MeanNegativeLoss"] == -0.02
    assert np.isclose(result["MeanGainLossRatio"], 1.0)
    assert result["MFEAboveThresholdFrequency"] == 0.75
    assert result["MAEBelowThresholdFrequency"] == 0.5


def test_conditional_metrics_use_only_positive_model_signals():
    result = conditional_signal_metrics(
        _risk_frame(), "UpPrediction", "UpSignal", 0.01, 0.01
    )

    assert result["UpSignalCount"] == 2
    assert np.isclose(result["UpSignalIntradayReturnMean"], -0.005)
    assert result["UpSignalUpFrequency"] == 0.5
    assert result["UpSignalDownFrequency"] == 0.5
    assert np.isclose(result["UpSignalMAEMean"], -0.02)
    assert np.isclose(result["UpSignalMFEMean"], 0.015)


def _legacy_value(series, operation, *args):
    clean = pd.to_numeric(series, errors="coerce").dropna()
    if clean.empty:
        return np.nan
    if operation == "quantile":
        return float(clean.quantile(args[0]))
    if operation == "std":
        return float(clean.std(ddof=0))
    return float(getattr(clean, operation)())


def _legacy_intraday_risk_metrics(frame, up_threshold, down_threshold):
    returns = pd.to_numeric(frame["IntradayReturn"], errors="coerce").dropna()
    mfe = pd.to_numeric(frame["MFE"], errors="coerce").dropna()
    mae = pd.to_numeric(frame["MAE"], errors="coerce").dropna()
    gains = returns[returns > 0]
    losses = returns[returns < 0]
    mean_gain = float(gains.mean()) if not gains.empty else np.nan
    mean_loss = float(losses.mean()) if not losses.empty else np.nan
    gain_loss_ratio = (
        mean_gain / abs(mean_loss)
        if np.isfinite(mean_gain) and np.isfinite(mean_loss) and mean_loss != 0
        else np.nan
    )
    return {
        "Observations": int(len(returns)),
        "IntradayReturnMean": _legacy_value(returns, "mean"),
        "IntradayReturnMedian": _legacy_value(returns, "median"),
        "IntradayReturnStd": _legacy_value(returns, "std"),
        "IntradayReturnP10": _legacy_value(returns, "quantile", 0.10),
        "IntradayReturnP25": _legacy_value(returns, "quantile", 0.25),
        "IntradayReturnP75": _legacy_value(returns, "quantile", 0.75),
        "IntradayReturnP90": _legacy_value(returns, "quantile", 0.90),
        "IntradayReturnBest": _legacy_value(returns, "max"),
        "IntradayReturnWorst": _legacy_value(returns, "min"),
        "UpFrequency": float((returns >= up_threshold).mean()) if len(returns) else np.nan,
        "DownFrequency": float((returns <= -down_threshold).mean()) if len(returns) else np.nan,
        "MeanPositiveGain": mean_gain,
        "MeanNegativeLoss": mean_loss,
        "MeanGainLossRatio": float(gain_loss_ratio),
        "ExpectedIntradayReturn": _legacy_value(returns, "mean"),
        "MFEMean": _legacy_value(mfe, "mean"),
        "MFEMedian": _legacy_value(mfe, "median"),
        "MFEP10": _legacy_value(mfe, "quantile", 0.10),
        "MFEP90": _legacy_value(mfe, "quantile", 0.90),
        "MAEMean": _legacy_value(mae, "mean"),
        "MAEMedian": _legacy_value(mae, "median"),
        "MAEP10": _legacy_value(mae, "quantile", 0.10),
        "MAEP90": _legacy_value(mae, "quantile", 0.90),
        "MAEBelowThresholdFrequency": float((mae <= -down_threshold).mean()) if len(mae) else np.nan,
        "MFEAboveThresholdFrequency": float((mfe >= up_threshold).mean()) if len(mfe) else np.nan,
    }


def _assert_same_metrics(actual, expected):
    assert actual.keys() == expected.keys()
    for name, expected_value in expected.items():
        actual_value = actual[name]
        if isinstance(expected_value, (int, np.integer)):
            assert actual_value == expected_value, name
        else:
            assert actual_value == pytest.approx(
                expected_value, rel=1e-12, abs=1e-15, nan_ok=True
            ), name


@pytest.mark.parametrize(
    "frame",
    [
        _risk_frame(),
        pd.DataFrame(
            {
                "IntradayReturn": ["0.01", None, "invalid", -0.02],
                "MFE": [0.03, None, "invalid", 0.01],
                "MAE": [-0.02, None, "invalid", -0.03],
                "UpPrediction": [1, 1, 0, 0],
                "DownPrediction": [0, 1, 1, 0],
            }
        ),
        pd.DataFrame(
            {
                "IntradayReturn": [0.0, -0.01, -0.02],
                "MFE": [0.0, 0.0, 0.0],
                "MAE": [-0.01, -0.02, -0.03],
                "UpPrediction": [0, 0, 0],
                "DownPrediction": [0, 0, 0],
            }
        ),
        pd.DataFrame(
            {
                "IntradayReturn": [0.0, 0.01, 0.02],
                "MFE": [0.01, 0.02, 0.03],
                "MAE": [0.0, 0.0, 0.0],
                "UpPrediction": [1, 1, 1],
                "DownPrediction": [1, 1, 1],
            }
        ),
        pd.DataFrame(
            {
                "IntradayReturn": [np.nan],
                "MFE": [np.nan],
                "MAE": [np.nan],
                "UpPrediction": [1],
                "DownPrediction": [0],
            }
        ),
    ],
    ids=["normal", "missing", "no_gain", "no_loss_only_signals", "short"],
)
def test_intraday_risk_metrics_match_the_pre_optimization_reference(frame):
    _assert_same_metrics(
        intraday_risk_metrics(frame, 0.01, 0.01),
        _legacy_intraday_risk_metrics(frame, 0.01, 0.01),
    )


def test_conditional_metrics_match_reference_without_full_risk_calculation(monkeypatch):
    frame = _risk_frame()
    expected = _legacy_intraday_risk_metrics(frame[frame["UpPrediction"] == 1], 0.01, 0.01)

    def fail_if_called(*args, **kwargs):
        raise AssertionError("conditional metrics must not calculate the full risk record")

    monkeypatch.setattr(risk, "intraday_risk_metrics", fail_if_called)
    actual = conditional_signal_metrics(frame, "UpPrediction", "UpSignal", 0.01, 0.01)

    expected_subset = {
        "UpSignalCount": expected["Observations"],
        "UpSignalIntradayReturnMean": expected["IntradayReturnMean"],
        "UpSignalIntradayReturnMedian": expected["IntradayReturnMedian"],
        "UpSignalUpFrequency": expected["UpFrequency"],
        "UpSignalDownFrequency": expected["DownFrequency"],
        "UpSignalMAEMean": expected["MAEMean"],
        "UpSignalMFEMean": expected["MFEMean"],
        "UpSignalMeanPositiveGain": expected["MeanPositiveGain"],
        "UpSignalMeanNegativeLoss": expected["MeanNegativeLoss"],
    }
    _assert_same_metrics(actual, expected_subset)


def test_conditional_metrics_preserve_empty_signal_nan_behavior():
    frame = _risk_frame().assign(UpPrediction=0)
    actual = conditional_signal_metrics(frame, "UpPrediction", "UpSignal", 0.01, 0.01)

    assert actual["UpSignalCount"] == 0
    for name, value in actual.items():
        if name != "UpSignalCount":
            assert np.isnan(value), name
