import pandas as pd
import pytest

from rstock.combination_planning import (
    COMBINATION_PLAN_VERSION,
    CombinationPlan,
    build_combination_plan,
    build_combination_preview,
)
from rstock.combinations import (
    generate_symbol_sets,
    generate_target_symbol_sets,
    symbol_set_id,
)


@pytest.mark.parametrize(
    ("symbols", "targets", "depth"),
    [
        (["A", "B", "C"], None, 1),
        (["A", "B", "C"], None, 2),
        (["A", "B", "C", "D"], ["C", "A"], 3),
        (["BRK-B", "RDS.A", "SPY"], ["BRK-B", "RDS.A"], 2),
    ],
)
def test_raw_plan_is_exhaustively_equivalent_to_historical_generator(
    symbols, targets, depth
):
    historical = generate_symbol_sets(
        symbols,
        depth,
        target_symbols=targets,
        max_sets=1_000_000,
    )
    plan = CombinationPlan(symbols, depth, target_symbols=targets)

    planned = plan.slice(0, plan.count())

    pd.testing.assert_frame_equal(planned, historical)
    assert [symbol_set_id(row) for _, row in planned.iterrows()] == [
        symbol_set_id(row) for _, row in historical.iterrows()
    ]


def test_concatenated_plan_slices_preserve_exact_historical_order_and_ids():
    symbols = ["A", "B", "C", "D", "E"]
    historical = generate_symbol_sets(
        symbols, 3, target_symbols=["D", "B", "A"], max_sets=1_000_000
    )
    plan = build_combination_plan(
        target_symbols=["D", "B", "A"],
        predictor_symbols=symbols,
        permutation_depth=3,
    )
    boundaries = (0, 1, 7, 19, 35, plan.count())

    sliced = pd.concat(
        [
            plan.slice(start, stop)
            for start, stop in zip(boundaries, boundaries[1:])
        ],
        ignore_index=True,
    )

    pd.testing.assert_frame_equal(sliced, historical)
    assert plan.row_at(0) == ("D", "A", None, None)
    assert plan[-1] == tuple(historical.iloc[-1])
    assert list(plan.iter_range(7, 19)) == [
        tuple(None if pd.isna(value) else value for value in row)
        for row in historical.iloc[7:19].itertuples(index=False, name=None)
    ]


def test_prefiltered_plan_matches_historical_target_specific_generator():
    predictors_by_target = {
        "T2": ("A", "B", "A", "C"),
        "T1": ("C", "B"),
        "T3": (),
    }
    historical = generate_target_symbol_sets(
        predictors_by_target, 3, max_sets=1_000_000
    )
    plan = CombinationPlan.from_target_predictors(predictors_by_target, 3)

    pd.testing.assert_frame_equal(plan.slice(0, plan.count()), historical)
    assert plan.population_kind == "prefiltered"


def test_large_plan_counts_and_slices_without_max_generated_sets_materialization():
    targets = [f"T{index:03d}" for index in range(150)]
    contexts = [f"C{index:02d}" for index in range(15)]
    predictors = [*targets, *contexts]
    plan = build_combination_plan(
        target_symbols=targets,
        predictor_symbols=predictors,
        permutation_depth=3,
    )

    assert plan.count() == 110_294_100
    assert len(plan.slice(0, 2)) == 2
    assert len(plan.slice(plan.count() - 2, plan.count())) == 2
    assert plan.row_at(plan.count() - 1)[0] == targets[-1]


def test_preview_uses_raw_exact_count_and_separate_effective_count():
    raw = CombinationPlan(
        ["A", "B", "C", "CONTEXT"],
        2,
        target_symbols=["A", "B"],
    )
    effective = CombinationPlan.from_target_predictors(
        {"A": ("B", "CONTEXT"), "B": ("A", "C")}, 2
    )

    preview = build_combination_preview(
        raw,
        context_symbols=["CONTEXT"],
        max_combinations_per_batch=5,
        effective_plan=effective,
    )

    assert preview.raw_combination_count == 12
    assert preview.preview_batch_count == 3
    assert preview.prefiltered_combination_count == 6
    assert preview.effective_combination_count == 6
    assert preview.effective_batch_count == 2
    assert preview.target_count == 2
    assert preview.context_count == 1
    assert preview.predictor_count == 4
    assert preview.to_dict()["raw_combination_count"] == 12


def test_preview_real_150_target_example_requires_51_batches():
    targets = [f"T{index:03d}" for index in range(150)]
    contexts = [f"C{index:02d}" for index in range(15)]
    plan = CombinationPlan([*targets, *contexts], 3, target_symbols=targets)

    preview = build_combination_preview(
        plan,
        context_symbols=contexts,
        max_combinations_per_batch=2_200_000,
    )

    assert preview.raw_combination_count == 110_294_100
    assert preview.preview_batch_count == 51
    assert preview.prefiltered_combination_count is None
    assert preview.effective_batch_count is None


def test_null_historical_capacity_means_one_monolithic_preview_batch():
    plan = CombinationPlan(["A", "B", "C"], 2)

    preview = build_combination_preview(
        plan,
        max_combinations_per_batch=None,
    )

    assert preview.preview_batch_count == 1


def test_plan_hash_is_reproducible_versioned_and_order_sensitive():
    first = CombinationPlan(["A", "B", "C"], 2, target_symbols=["A", "B"])
    repeated = CombinationPlan(["A", "B", "C"], 2, target_symbols=["A", "B"])
    reordered = CombinationPlan(["B", "A", "C"], 2, target_symbols=["A", "B"])

    assert first.plan_version == COMBINATION_PLAN_VERSION == 2
    assert first.plan_sha256 == repeated.plan_sha256
    assert first.plan_sha256 == (
        "1a83779394680750efd772a7fb6c99ee6b2cd4ef627d3807a8b1b81f20edd4b5"
    )
    assert first.plan_sha256 != reordered.plan_sha256
    assert len(first.plan_sha256) == 64


def test_plan_validates_populations_and_slice_bounds():
    with pytest.raises(ValueError, match="unique"):
        CombinationPlan(["A", "A", "B"], 1)
    with pytest.raises(ValueError, match="included"):
        CombinationPlan(["A", "B"], 1, target_symbols=["C"])
    with pytest.raises(ValueError, match="target cannot"):
        CombinationPlan.from_target_predictors({"A": ("A", "B")}, 1)

    plan = CombinationPlan(["A", "B", "C"], 1)
    with pytest.raises(IndexError, match="outside"):
        plan.slice(0, plan.count() + 1)
    with pytest.raises(ValueError, match="step"):
        _ = plan[::2]


def test_frozen_plan_round_trip_preserves_identity_and_rows():
    raw = CombinationPlan(
        ("AAA", "BBB", "CCC"), 2, target_symbols=("BBB", "AAA")
    )
    effective = CombinationPlan.from_target_predictors(
        {"BBB": ("AAA", "CCC"), "AAA": ("CCC", "BBB")}, 2
    )

    for plan in (raw, effective):
        restored = CombinationPlan.from_dict(plan.to_dict())
        assert restored.plan_sha256 == plan.plan_sha256
        assert restored.count() == plan.count()
        pd.testing.assert_frame_equal(
            restored.slice(0, restored.count()), plan.slice(0, plan.count())
        )
