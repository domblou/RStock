"""Deferred validation of daily predictions against realised prices."""

from __future__ import annotations

import warnings

import pandas as pd


def validate_pending_predictions(
    predictions: pd.DataFrame,
    history: pd.DataFrame,
    up_down_threshold: float = 0.01,
    *,
    legacy_character_comparison: bool = True,
) -> pd.DataFrame:
    """Populate legacy BinaryResult and true-positive-only success fields.

    The R CSV is loaded with character columns. R then coerces the numeric threshold
    to text for comparison, so phase 1 uses a lexical comparison by default.
    """

    result = predictions.copy()
    for index, prediction in result.iterrows():
        if int(prediction["BinaryResult"]) != -1:
            continue
        matches = history[
            (history["Date"].astype(str) == str(prediction["Date"]))
            & (history["Symbol"].astype(str) == str(prediction["Observation"]))
            & (history["Field"].astype(str) == "OpCl")
        ]
        if len(matches) == 1:
            raw_value = matches.iloc[0]["Value"]
            if legacy_character_comparison:
                binary_result = int(str(raw_value) > str(up_down_threshold))
            else:
                value = pd.to_numeric(raw_value, errors="coerce")
                if pd.isna(value):
                    continue
                binary_result = int(float(value) > up_down_threshold)
            binary_prediction = int(prediction["BinaryPrediction"])
            result.at[index, "BinaryResult"] = binary_result
            result.at[index, "SuccessfulPrediction"] = int(
                binary_result == binary_prediction and binary_prediction == 1
            )
        elif len(matches) > 1:
            warnings.warn(
                "More than one result found while setting BinaryResult",
                RuntimeWarning,
                stacklevel=2,
            )
    return result
