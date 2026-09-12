"""Deferred validation of daily predictions against realised prices."""

from __future__ import annotations

import warnings

import pandas as pd


def validate_pending_predictions(
    predictions: pd.DataFrame,
    history: pd.DataFrame,
) -> pd.DataFrame:
    """Validate predictions against the next session's exploitable Open-to-Close return."""

    result = predictions.copy()
    result["Date"] = pd.to_datetime(result["Date"], errors="raise").dt.normalize()
    result["AsOfDate"] = pd.to_datetime(result["AsOfDate"], errors="raise").dt.normalize()
    if "TargetThreshold" not in result:
        raise ValueError("Predictions are missing their intraday TargetThreshold")
    result["TargetThreshold"] = pd.to_numeric(
        result["TargetThreshold"], errors="raise"
    )
    market_history = history.copy()
    market_history["Date"] = pd.to_datetime(
        market_history["Date"], errors="raise"
    ).dt.normalize()
    market_history["Value"] = pd.to_numeric(market_history["Value"], errors="coerce")
    for index, prediction in result.iterrows():
        if int(prediction["BinaryResult"]) != -1:
            continue
        matches = market_history[
            (market_history["Date"] > prediction["AsOfDate"])
            & (market_history["Symbol"] == str(prediction["Observation"]))
            & (market_history["Field"] == "IntradayReturn")
        ].sort_values("Date")
        if len(matches):
            matches = matches[matches["Date"] == matches.iloc[0]["Date"]]
        if len(matches) == 1:
            value = matches.iloc[0]["Value"]
            if pd.isna(value):
                continue
            result.at[index, "Date"] = matches.iloc[0]["Date"]
            binary_result = int(float(value) >= float(prediction["TargetThreshold"]))
            binary_prediction = int(prediction["BinaryPrediction"])
            result.at[index, "BinaryResult"] = binary_result
            result.at[index, "SuccessfulPrediction"] = int(
                binary_result == binary_prediction
            )
        elif len(matches) > 1:
            warnings.warn(
                "More than one result found while setting BinaryResult",
                RuntimeWarning,
                stacklevel=2,
            )
    return result
