import pandas as pd
import pytest

from rstock.history import append_symbol_history, market_data_to_history
from rstock.validation import validate_pending_predictions


def test_history_adds_opcl_and_preserves_first_dot_split_bug_explicitly():
    stock = pd.DataFrame(
        {"BRK.B.Open": [100.0], "BRK.B.Close": [102.0]},
        index=pd.to_datetime(["2024-01-02"]),
    )
    legacy = market_data_to_history(stock)
    corrected = market_data_to_history(stock, legacy_field_split=False)

    assert set(legacy["Symbol"]) == {"BRK"}
    assert "B.OpCl" in set(legacy["Field"])
    assert set(corrected["Symbol"]) == {"BRK.B"}
    assert "OpCl" in set(corrected["Field"])


def test_history_only_appends_dates_after_existing_maximum():
    existing = pd.DataFrame(
        [["2024-01-02", "AAA", "Open", "10"]],
        columns=["Date", "Symbol", "Field", "Value"],
    )
    stock = pd.DataFrame(
        {"AAA.Open": [10.0, 11.0], "AAA.Close": [10.0, 12.0]},
        index=pd.to_datetime(["2024-01-02", "2024-01-03"]),
    )
    result = append_symbol_history(stock, existing)

    assert len(result[result["Date"] == "2024-01-02"]) == 1
    assert len(result[result["Date"] == "2024-01-03"]) == 3


def test_validation_preserves_true_positive_only_success_semantics():
    predictions = pd.DataFrame(
        [
            ["2024-01-03", "A-B", "AAA", 1, -1, 0],
            ["2024-01-03", "A-C", "BBB", 0, -1, 0],
        ],
        columns=[
            "Date", "Set", "Observation", "BinaryPrediction", "BinaryResult",
            "SuccessfulPrediction",
        ],
    )
    history = pd.DataFrame(
        [
            ["2024-01-03", "AAA", "OpCl", "0.02"],
            ["2024-01-03", "BBB", "OpCl", "-0.02"],
        ],
        columns=["Date", "Symbol", "Field", "Value"],
    )

    result = validate_pending_predictions(predictions, history, 0.01)

    assert result["BinaryResult"].tolist() == [1, 0]
    assert result["SuccessfulPrediction"].tolist() == [1, 0]


def test_duplicate_history_result_is_left_pending():
    predictions = pd.DataFrame(
        [["2024-01-03", "A-B", "AAA", 1, -1, 0]],
        columns=[
            "Date", "Set", "Observation", "BinaryPrediction", "BinaryResult",
            "SuccessfulPrediction",
        ],
    )
    history = pd.DataFrame(
        [["2024-01-03", "AAA", "OpCl", "0.02"]] * 2,
        columns=["Date", "Symbol", "Field", "Value"],
    )

    with pytest.warns(RuntimeWarning):
        result = validate_pending_predictions(predictions, history)
    assert result.iloc[0]["BinaryResult"] == -1


def test_history_threshold_comparison_exposes_legacy_and_numeric_modes():
    predictions = pd.DataFrame(
        [["2024-01-03", "A-B", "AAA", 0, -1, 0]],
        columns=[
            "Date", "Set", "Observation", "BinaryPrediction", "BinaryResult",
            "SuccessfulPrediction",
        ],
    )
    history = pd.DataFrame(
        [["2024-01-03", "AAA", "OpCl", "1e-3"]],
        columns=["Date", "Symbol", "Field", "Value"],
    )

    legacy = validate_pending_predictions(predictions, history, 0.01)
    numeric = validate_pending_predictions(
        predictions, history, 0.01, legacy_character_comparison=False
    )

    assert legacy.iloc[0]["BinaryResult"] == 1
    assert numeric.iloc[0]["BinaryResult"] == 0
