"""Descriptive intraday return and excursion risk statistics."""

from __future__ import annotations

import numpy as np
import pandas as pd


def _numeric_series(frame: pd.DataFrame, column: str) -> pd.Series:
    """Return one cleaned numeric series for a risk input column."""

    return pd.to_numeric(frame[column], errors="coerce").dropna()


def _statistic(series: pd.Series, operation: str, *args: float) -> float:
    """Calculate a scalar statistic from an already-cleaned numeric series."""

    if series.empty:
        return np.nan
    if operation == "quantile":
        return float(series.quantile(args[0]))
    if operation == "std":
        return float(series.std(ddof=0))
    return float(getattr(series, operation)())


def intraday_risk_metrics(
    frame: pd.DataFrame,
    up_threshold: float,
    down_threshold: float,
) -> dict[str, int | float]:
    """Describe signed returns and Open-to-High/Low excursions."""

    returns = _numeric_series(frame, "IntradayReturn")
    mfe = _numeric_series(frame, "MFE")
    mae = _numeric_series(frame, "MAE")
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
        "IntradayReturnMean": _statistic(returns, "mean"),
        "IntradayReturnMedian": _statistic(returns, "median"),
        "IntradayReturnStd": _statistic(returns, "std"),
        "IntradayReturnP10": _statistic(returns, "quantile", 0.10),
        "IntradayReturnP25": _statistic(returns, "quantile", 0.25),
        "IntradayReturnP75": _statistic(returns, "quantile", 0.75),
        "IntradayReturnP90": _statistic(returns, "quantile", 0.90),
        "IntradayReturnBest": _statistic(returns, "max"),
        "IntradayReturnWorst": _statistic(returns, "min"),
        "UpFrequency": float((returns >= up_threshold).mean()) if len(returns) else np.nan,
        "DownFrequency": float((returns <= -down_threshold).mean()) if len(returns) else np.nan,
        "MeanPositiveGain": mean_gain,
        "MeanNegativeLoss": mean_loss,
        "MeanGainLossRatio": float(gain_loss_ratio),
        "ExpectedIntradayReturn": _statistic(returns, "mean"),
        "MFEMean": _statistic(mfe, "mean"),
        "MFEMedian": _statistic(mfe, "median"),
        "MFEP10": _statistic(mfe, "quantile", 0.10),
        "MFEP90": _statistic(mfe, "quantile", 0.90),
        "MAEMean": _statistic(mae, "mean"),
        "MAEMedian": _statistic(mae, "median"),
        "MAEP10": _statistic(mae, "quantile", 0.10),
        "MAEP90": _statistic(mae, "quantile", 0.90),
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
    returns = _numeric_series(selected, "IntradayReturn")
    mfe = _numeric_series(selected, "MFE")
    mae = _numeric_series(selected, "MAE")
    gains = returns[returns > 0]
    losses = returns[returns < 0]
    return {
        f"{prefix}Count": int(len(returns)),
        f"{prefix}IntradayReturnMean": _statistic(returns, "mean"),
        f"{prefix}IntradayReturnMedian": _statistic(returns, "median"),
        f"{prefix}UpFrequency": (
            float((returns >= up_threshold).mean()) if len(returns) else np.nan
        ),
        f"{prefix}DownFrequency": (
            float((returns <= -down_threshold).mean()) if len(returns) else np.nan
        ),
        f"{prefix}MAEMean": _statistic(mae, "mean"),
        f"{prefix}MFEMean": _statistic(mfe, "mean"),
        f"{prefix}MeanPositiveGain": _statistic(gains, "mean"),
        f"{prefix}MeanNegativeLoss": _statistic(losses, "mean"),
    }
