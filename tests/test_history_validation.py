import pandas as pd
import pytest

from rstock.history import append_symbol_history, market_data_to_history
from rstock.validation import validate_pending_predictions


def _prediction(symbol: str, binary: int) -> pd.DataFrame:
    return pd.DataFrame(
        [["2024-01-03", "2024-01-02", "XNYS", "set", symbol, binary, -1, 0]],
        columns=[
            "Date",
            "AsOfDate",
            "MarketCalendar",
            "Set",
            "Observation",
            "BinaryPrediction",
            "BinaryResult",
            "SuccessfulPrediction",
        ],
    )


def test_history_preserves_punctuated_ticker_and_typed_date():
    stock = pd.DataFrame(
        {"BRK.B.Open": [100.0], "BRK.B.Close": [102.0]},
        index=pd.to_datetime(["2024-01-02"]),
    )

    history = market_data_to_history(stock)

    assert set(history["Symbol"]) == {"BRK.B"}
    assert "OpCl" in set(history["Field"])
    assert pd.api.types.is_datetime64_any_dtype(history["Date"])


def test_history_merge_fills_old_gaps_and_refreshes_existing_values():
    existing = pd.DataFrame(
        [["2024-01-02", "AAA", "Open", 9.0]],
        columns=["Date", "Symbol", "Field", "Value"],
    )
    stock = pd.DataFrame(
        {"AAA.Open": [10.0, 11.0], "AAA.Close": [10.0, 12.0]},
        index=pd.to_datetime(["2024-01-02", "2024-01-03"]),
    )

    result = append_symbol_history(stock, existing)

    assert len(result[result["Date"] == pd.Timestamp("2024-01-02")]) == 3
    refreshed = result[
        (result["Date"] == pd.Timestamp("2024-01-02"))
        & (result["Field"] == "Open")
    ]
    assert refreshed.iloc[0]["Value"] == 10.0


def test_validation_uses_first_actual_session_after_as_of_and_counts_true_negative():
    predictions = _prediction("AAA", binary=0)
    history = pd.DataFrame(
        [["2024-01-04", "AAA", "OpCl", -0.02]],
        columns=["Date", "Symbol", "Field", "Value"],
    )

    result = validate_pending_predictions(predictions, history, 0.01)

    assert result.iloc[0]["Date"] == pd.Timestamp("2024-01-04")
    assert result.iloc[0]["BinaryResult"] == 0
    assert result.iloc[0]["SuccessfulPrediction"] == 1


def test_numeric_threshold_comparison_is_not_lexicographic():
    predictions = _prediction("AAA", binary=0)
    history = pd.DataFrame(
        [["2024-01-03", "AAA", "OpCl", "1e-3"]],
        columns=["Date", "Symbol", "Field", "Value"],
    )

    result = validate_pending_predictions(predictions, history, 0.01)

    assert result.iloc[0]["BinaryResult"] == 0


def test_duplicate_actual_result_is_left_pending():
    predictions = _prediction("AAA", binary=1)
    history = pd.DataFrame(
        [["2024-01-03", "AAA", "OpCl", 0.02]] * 2,
        columns=["Date", "Symbol", "Field", "Value"],
    )

    with pytest.warns(RuntimeWarning):
        result = validate_pending_predictions(predictions, history)
    assert result.iloc[0]["BinaryResult"] == -1
