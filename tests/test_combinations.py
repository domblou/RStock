import pytest

from rstock.combinations import (
    count_symbol_sets,
    generate_symbol_sets,
    symbol_set_id,
    symbols_from_set,
)


def test_generate_sets_builds_expected_pairs_and_combinations():
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
    assert symbol_set_id(generated.iloc[0]) == '["A","B"]'
    assert symbols_from_set(generated.iloc[6]) == ("A", ["B", "C"])


def test_generate_sets_rejects_depth_equal_to_symbol_count():
    with pytest.raises(ValueError):
        generate_symbol_sets(["A", "B"], permutation_depth=2)


def test_set_id_is_unambiguous_for_punctuated_symbols():
    generated = generate_symbol_sets(["BRK-B", "RDS.A"], permutation_depth=1)
    assert symbol_set_id(generated.iloc[0]) == '["BRK-B","RDS.A"]'


def test_generation_stops_before_combinatorial_materialisation():
    assert count_symbol_sets(10, 3) == 1290
    with pytest.raises(ValueError, match="exceeds max_generated_sets"):
        generate_symbol_sets([f"S{i}" for i in range(10)], 3, max_sets=1_000)


def test_context_symbols_are_available_as_predictors_but_never_targets():
    generated = generate_symbol_sets(
        ["AAA", "BBB", "CONTEXT"],
        permutation_depth=2,
        target_symbols=["AAA", "BBB"],
    )

    assert set(generated["V0"]) == {"AAA", "BBB"}
    assert "CONTEXT" not in set(generated["V0"])
    assert any(
        "CONTEXT" in {row.V1, row.V2}
        for row in generated.itertuples(index=False)
    )
    assert len(generated) == 6
