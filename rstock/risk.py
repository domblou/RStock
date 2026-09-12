"""Descriptive intraday return and excursion risk statistics."""

from __future__ import annotations

import numpy as np
import pandas as pd


def _value(series: pd.Series, operation: str, *args: float) -> float:
    clean = pd.to_numeric(series, errors="coerce").dropna()
    if clean.empty:
        return np.nan
    if operation == "quantile":
        return float(clean.quantile(args[0]))
    if operation == "std":
        return float(clean.std(ddof=0))
    return float(getattr(clean, operation)())


def intraday_risk_metrics(
    frame: pd.DataFrame,
    up_threshold: float,
    down_threshold: float,
) -> dict[str, int | float]:
    """Describe signed returns and Open-to-High/Low excursions."""

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
        "IntradayReturnMean": _value(returns, "mean"),
        "IntradayReturnMedian": _value(returns, "median"),
        "IntradayReturnStd": _value(returns, "std"),
        "IntradayReturnP10": _value(returns, "quantile", 0.10),
        "IntradayReturnP25": _value(returns, "quantile", 0.25),
        "IntradayReturnP75": _value(returns, "quantile", 0.75),
        "IntradayReturnP90": _value(returns, "quantile", 0.90),
        "IntradayReturnBest": _value(returns, "max"),
        "IntradayReturnWorst": _value(returns, "min"),
        "UpFrequency": float((returns >= up_threshold).mean()) if len(returns) else np.nan,
        "DownFrequency": float((returns <= -down_threshold).mean()) if len(returns) else np.nan,
        "MeanPositiveGain": mean_gain,
        "MeanNegativeLoss": mean_loss,
        "MeanGainLossRatio": float(gain_loss_ratio),
        "ExpectedIntradayReturn": _value(returns, "mean"),
        "MFEMean": _value(mfe, "mean"),
        "MFEMedian": _value(mfe, "median"),
        "MFEP10": _value(mfe, "quantile", 0.10),
        "MFEP90": _value(mfe, "quantile", 0.90),
        "MAEMean": _value(mae, "mean"),
        "MAEMedian": _value(mae, "median"),
        "MAEP10": _value(mae, "quantile", 0.10),
        "MAEP90": _value(mae, "quantile", 0.90),
        "MAEBelowThresholdFrequency": (
            float((mae <= -down_threshold).mean()) if len(mae) else np.nan
        ),
        "MFEAboveThresholdFrequency": (
            float((mfe >= up_threshold).mean()) if len(mfe) else np.nan
        ),
    }


def conditional_signal_metrics(
    frame: pd.DataFrame,
    signal_column: str,
    prefix: str,
    up_threshold: float,
    down_threshold: float,
) -> dict[str, int | float]:
    """Describe realised risk only when a model emits a positive signal."""

    selected = frame[frame[signal_column] == 1]
    metrics = intraday_risk_metrics(selected, up_threshold, down_threshold)
    wanted = {
        "Observations": "Count",
        "IntradayReturnMean": "IntradayReturnMean",
        "IntradayReturnMedian": "IntradayReturnMedian",
        "UpFrequency": "UpFrequency",
        "DownFrequency": "DownFrequency",
        "MAEMean": "MAEMean",
        "MFEMean": "MFEMean",
        "MeanPositiveGain": "MeanPositiveGain",
        "MeanNegativeLoss": "MeanNegativeLoss",
    }
    return {f"{prefix}{target}": metrics[source] for source, target in wanted.items()}
