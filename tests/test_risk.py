import numpy as np
import pandas as pd

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
