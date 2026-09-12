import pandas as pd
import pytest

from rstock.combinations import generate_symbol_sets, legacy_set_name, symbols_from_set


def test_generate_sets_preserves_legacy_pair_and_combination_semantics():
    generated = generate_symbol_sets(["A", "B", "C"], permutation_depth=2)

    assert list(generated.columns) == ["V0", "V1", "V2"]
    assert len(generated) == 9
    assert generated.iloc[:6][["V0", "V1"]].values.tolist() == [
        ["A", "B"], ["A", "C"], ["B", "A"],
        ["B", "C"], ["C", "A"], ["C", "B"],
    ]
    assert generated.iloc[6:].values.tolist() == [
        ["A", "B", "C"], ["B", "A", "C"], ["C", "A", "B"]
    ]
    assert legacy_set_name(generated.iloc[0]) == "A-B-NA"
    assert symbols_from_set(generated.iloc[6]) == ("A", ["B", "C"])


def test_generate_sets_rejects_depth_equal_to_symbol_count():
    with pytest.raises(ValueError):
        generate_symbol_sets(["A", "B"], permutation_depth=2)

