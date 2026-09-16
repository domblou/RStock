from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.combinations import generate_target_symbol_sets, symbols_from_set
from rstock.config import DEFAULT_CONFIG
from rstock.features import intraday_lag_column
from rstock.predictor_prefilter import select_predictors


def _qualification() -> pd.DataFrame:
    rows = []
    definitions = (
        ("A", True, 0.65, 1.00, 0.02, 0.56),
        ("B", True, 0.64, 0.98, 0.02, 0.55),
        ("C", True, 0.63, 0.95, 0.03, 0.54),
        ("D", True, 0.62, 0.90, 0.04, 0.53),
        ("WEAK", False, 0.49, 0.25, 0.20, 0.30),
    )
    for predictor, eligible, median, pct, std, worst in definitions:
        rows.append({
            "Set": f'T<-{predictor}',
            "Observation": "T",
            "Predictors": f'["{predictor}"]',
            "WindowsEvaluated": 4,
            "AUCWindows": 4,
            "ROCAUCMedian": median,
            "ROCAUCMean": median,
            "ROCAUCStd": std,
            "ROCAUCWorst": worst,
            "PctWindowsAboveRandom": pct,
            "PositiveObservations": 40,
            "Eligible": eligible,
            "IneligibilityReasons": "[]" if eligible else '["median_auc"]',
        })
    return pd.DataFrame(rows)


def _prepared() -> pd.DataFrame:
    rng = np.random.default_rng(42)
    index = pd.bdate_range("2025-01-01", periods=40)
    values = rng.normal(size=(len(index), 3))
    independent = rng.normal(size=(len(index), 3))
    other = rng.normal(size=(len(index), 3))
    columns = {}
    for lag in range(1, 4):
        columns[intraday_lag_column("A", lag)] = values[:, lag - 1]
        columns[intraday_lag_column("B", lag)] = values[:, lag - 1]
        columns[intraday_lag_column("C", lag)] = independent[:, lag - 1]
        columns[intraday_lag_column("D", lag)] = other[:, lag - 1]
        columns[intraday_lag_column("WEAK", lag)] = rng.normal(size=len(index))
    return pd.DataFrame(columns, index=index)


def test_prefilter_applies_threshold_top_n_and_actual_feature_redundancy():
    config = replace(
        DEFAULT_CONFIG,
        predictor_prefilter_enabled=True,
        predictor_prefilter_top_n=3,
        predictor_prefilter_correlation_threshold=0.90,
        permutation_depth=3,
    )

    result = select_predictors(
        _qualification(),
        _prepared(),
        targets=["T"],
        candidate_symbols=["T", "A", "B", "C", "D", "WEAK"],
        config=config,
    )
    statuses = result.metrics.set_index("Predictor")["PrefilterStatus"].to_dict()

    assert result.predictors_by_target == {"T": ("A", "C")}
    assert statuses["WEAK"] == "rejected_threshold"
    assert statuses["D"] == "rejected_top_n"
    assert statuses["B"] == "removed_redundancy"
    assert statuses["A"] == statuses["C"] == "retained"
    redundant = result.metrics.set_index("Predictor").loc["B"]
    assert redundant["RedundantWith"] == "A"
    assert redundant["RedundancyCorrelation"] == 1.0

    diagnostic = result.diagnostics[0]
    assert diagnostic == {
        "target": "T",
        "initial_candidates": 5,
        "pairs_attempted": 5,
        "pairs_admissible": 5,
        "pairs_skipped": 0,
        "target_excluded": False,
        "exclusion_reason": None,
        "rejected_median_auc": 1,
        "rejected_pct_above_random": 1,
        "rejected_worst_auc": 1,
        "rejected_auc_std": 1,
        "after_qualification": 4,
        "after_top_n": 3,
        "removed_for_redundancy": 1,
        "after_redundancy": 2,
        "retained_predictors": ["A", "C"],
        "combinations_before_filtering": 25,
        "combinations_tested": 3,
    }


def test_prefilter_is_reproducible_and_generated_sets_use_only_retained_candidates():
    config = replace(
        DEFAULT_CONFIG,
        predictor_prefilter_enabled=True,
        predictor_prefilter_top_n=3,
        predictor_prefilter_correlation_threshold=0.90,
        permutation_depth=3,
    )
    arguments = dict(
        targets=["T"],
        candidate_symbols=["T", "A", "B", "C", "D", "WEAK"],
        config=config,
    )

    first = select_predictors(_qualification(), _prepared(), **arguments)
    second = select_predictors(_qualification(), _prepared(), **arguments)
    pd.testing.assert_frame_equal(first.metrics, second.metrics)
    assert first.diagnostics == second.diagnostics

    generated = generate_target_symbol_sets(
        first.predictors_by_target, config.permutation_depth
    )
    parsed = [symbols_from_set(row) for _, row in generated.iterrows()]
    assert len(parsed) == 3
    assert {predictor for _, predictors in parsed for predictor in predictors} == {"A", "C"}
    assert max(len(predictors) for _, predictors in parsed) == 2


def test_prefilter_selection_preserves_skip_reason_and_excludes_empty_target():
    qualification = _qualification()
    skipped = qualification["Predictors"] == '["WEAK"]'
    qualification.loc[skipped, "PrefilterSkipReason"] = (
        "insufficient_walk_forward_observations"
    )
    config = replace(
        DEFAULT_CONFIG,
        predictor_prefilter_enabled=True,
        predictor_prefilter_top_n=3,
        predictor_prefilter_correlation_threshold=0.90,
        permutation_depth=2,
    )

    result = select_predictors(
        qualification,
        _prepared(),
        targets=["T", "EA"],
        candidate_symbols=["T", "A", "B", "C", "D", "WEAK", "EA"],
        config=config,
        excluded_targets={"EA": "insufficient_walk_forward_observations"},
    )

    assert "EA" not in result.predictors_by_target
    assert (
        result.metrics.set_index("Predictor").at["WEAK", "PrefilterStatus"]
        == "skipped_insufficient_observations"
    )
    excluded = next(item for item in result.diagnostics if item["target"] == "EA")
    assert excluded["target_excluded"]
    assert excluded["exclusion_reason"] == "insufficient_walk_forward_observations"
    assert excluded["pairs_admissible"] == 0


@pytest.mark.parametrize(
    ("median", "pct", "std", "worst", "expected"),
    [
        (0.49, 0.80, 0.02, 0.50, {"rejected_median_auc": 1}),
        (0.60, 0.49, 0.02, 0.50, {"rejected_pct_above_random": 1}),
        (0.60, 0.80, 0.02, 0.39, {"rejected_worst_auc": 1}),
        (0.60, 0.80, 0.16, 0.50, {"rejected_auc_std": 1}),
        (
            0.49,
            0.49,
            0.16,
            0.39,
            {
                "rejected_median_auc": 1,
                "rejected_pct_above_random": 1,
                "rejected_worst_auc": 1,
                "rejected_auc_std": 1,
            },
        ),
    ],
    ids=["median", "pct_above_random", "worst", "dispersion", "multiple"],
)
def test_prefilter_counts_each_threshold_rejection_without_changing_selection(
    median, pct, std, worst, expected
):
    qualification = _qualification()
    weak = qualification["Predictors"] == '["WEAK"]'
    qualification.loc[weak, "ROCAUCMedian"] = median
    qualification.loc[weak, "PctWindowsAboveRandom"] = pct
    qualification.loc[weak, "ROCAUCStd"] = std
    qualification.loc[weak, "ROCAUCWorst"] = worst
    qualification.loc[weak, "Eligible"] = False
    config = replace(
        DEFAULT_CONFIG,
        predictor_prefilter_enabled=True,
        predictor_prefilter_top_n=10,
        predictor_prefilter_correlation_threshold=0.90,
    )

    result = select_predictors(
        qualification,
        _prepared(),
        targets=["T"],
        candidate_symbols=["T", "A", "B", "C", "D", "WEAK"],
        config=config,
    )
    diagnostic = result.diagnostics[0]

    for name in (
        "rejected_median_auc",
        "rejected_pct_above_random",
        "rejected_worst_auc",
        "rejected_auc_std",
    ):
        assert diagnostic[name] == expected.get(name, 0)
    assert result.predictors_by_target["T"] == ("A", "C", "D")
