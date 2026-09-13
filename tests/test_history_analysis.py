from dataclasses import replace

import pandas as pd
import pytest

from rstock.application.history_analysis import (
    analyze_run,
    combination_table,
    comparison_table,
    comparison_chart_frames,
    configuration_differences,
    filter_combinations,
    run_universe_summary,
    selected_run_action,
)


def _qualification():
    return pd.DataFrame([
        {
            "Set": "DIS<-PFE+WMT", "Observation": "DIS",
            "Predictors": '["PFE", "WMT"]', "Eligible": True,
            "ROCAUCMedian": 0.76, "ROCAUCWorst": 0.55, "ROCAUCStd": 0.09,
            "WindowsEvaluated": 7, "PositiveObservations": 284,
        },
        {
            "Set": "AAA<-BBB", "Observation": "AAA", "Predictors": '["BBB"]',
            "Eligible": False, "ROCAUCMedian": 0.80, "ROCAUCWorst": 0.40,
            "ROCAUCStd": 0.18, "WindowsEvaluated": 4, "PositiveObservations": 19,
        },
    ])


def _holdout():
    return pd.DataFrame([
        {"Set": "DIS<-PFE+WMT", "FinalUpROCAUC": 0.74, "FinalConfirmed": True},
        {"Set": "AAA<-BBB", "FinalUpROCAUC": 0.52, "FinalConfirmed": False},
    ])


def _analysis(identifier="run-1", duration=100.0):
    return analyze_run(
        {"run_id": identifier, "status": "completed", "duration_seconds": duration},
        {"configuration": {"symbols": ["DIS", "PFE", "WMT"], "rstock_config": {"permutation_depth": 3}}, "summary": {"total_combinations": 20}},
        _qualification(), _holdout(),
    )


def test_combination_table_calculates_delta_and_preserves_existing_metrics():
    table = combination_table(_qualification(), _holdout(), depth=3)
    row = table[table["Combinaison"] == "DIS<-PFE+WMT"].iloc[0]

    assert row["Predictors"] == "PFE + WMT"
    assert row["Delta dev→holdout"] == pytest.approx(-0.02)
    assert row["Worst AUC"] == 0.55
    assert row["Dispersion"] == 0.09
    assert bool(row["Holdout confirmé"]) is True
    assert row["Statut"] == "Holdout confirmé"


def test_combination_filters_keep_qualification_separate_from_confirmation():
    table = combination_table(_qualification(), _holdout(), depth=3)

    qualified = filter_combinations(table, min_worst_auc=0.5, min_positive_observations=100)
    confirmed = filter_combinations(table, confirmed_only=True)

    assert qualified["Combinaison"].tolist() == ["DIS<-PFE+WMT"]
    assert confirmed["Combinaison"].tolist() == ["DIS<-PFE+WMT"]


def test_legacy_run_without_artifacts_is_analysable_with_missing_metrics():
    analytics = analyze_run(
        {"run_id": "legacy", "status": "completed", "duration_seconds": None},
        {"configuration": {"symbols": ["AAA"], "rstock_config": {}}, "summary": {}},
        pd.DataFrame(), pd.DataFrame(),
    )

    assert analytics.combinations.empty
    assert analytics.qualified_count == 0
    assert analytics.holdout_auc_median is None
    assert analytics.tested_count is None


def test_comparison_keeps_configuration_and_missing_values_distinct():
    first = _analysis("run-a", 120.0)
    second = _analysis("run-b", 60.0)
    matrix = comparison_table(
        [first, second], {"run-a": "Run A", "run-b": "Run B"}
    )

    assert matrix.columns.tolist() == ["Indicateur", "Run A", "Run B"]
    assert matrix.loc[matrix["Indicateur"] == "Durée (s)", "Run B"].iloc[0] == 60.0
    assert matrix.loc[matrix["Indicateur"] == "AUC holdout médiane", "Run A"].iloc[0] == 0.74


def test_configuration_comparison_exposes_only_differences():
    differences = configuration_differences(
        [
            {"symbols": ["AAA", "BBB"], "rstock_config": {"permutation_depth": 2, "lag_depth": 3}},
            {"symbols": ["AAA", "CCC"], "rstock_config": {"permutation_depth": 3, "lag_depth": 3}},
        ],
        ["Run A", "Run B"],
    )

    assert differences["Paramètre"].tolist() == ["permutation_depth", "univers"]
    assert differences["Run B"].tolist() == [3, "AAA, CCC"]


def test_selection_routes_one_run_to_detail_and_two_to_four_to_comparison():
    assert selected_run_action(["run-1"]) == "detail"
    assert selected_run_action(["run-1", "run-2"]) == "comparison"
    assert selected_run_action(["a", "b", "c", "d"]) == "comparison"
    assert selected_run_action([]) is None
    assert selected_run_action(["a", "b", "c", "d", "e"]) is None


def test_comparison_charts_keep_quality_and_duration_on_separate_human_axes():
    first = _analysis("run-a", 372.0)
    second = _analysis("run-b", 1609.0)
    first = replace(first, depth=2)
    second = replace(second, depth=3)

    quality, durations = comparison_chart_frames(
        [first, second], {"run-a": "2026-09-12 22:53", "run-b": "2026-09-12 22:09"}
    )

    assert quality.columns.tolist() == ["Run", "AUC dev médiane", "AUC holdout médiane", "Delta dev→holdout", "Date / heure"]
    assert quality["Run"].tolist() == ["Profondeur 2", "Profondeur 3"]
    assert durations[["Durée (s)", "Durée"]].to_dict("records") == [
        {"Durée (s)": 372.0, "Durée": "06:12"},
        {"Durée (s)": 1609.0, "Durée": "26:49"},
    ]


def test_run_universe_summary_supports_context_and_legacy_configurations():
    contextual = run_universe_summary({
        "primary_universe_id": "US_CORE_50",
        "context_universe_ids": ["MARKET_CONTEXT", "SECTOR_ETFS"],
        "target_symbols": ["AAA", "BBB"],
        "predictor_symbols": ["AAA", "BBB", "SPY"],
    })
    legacy = run_universe_summary({
        "symbols": ["AAA", "BBB"],
        "universe_selection": {"universe": "LEGACY"},
    })

    assert contextual == {
        "primary_universe_id": "US_CORE_50",
        "target_count": 2,
        "context_universe_ids": ("MARKET_CONTEXT", "SECTOR_ETFS"),
        "predictor_count": 3,
    }
    assert legacy == {
        "primary_universe_id": "LEGACY",
        "target_count": 2,
        "context_universe_ids": (),
        "predictor_count": 2,
    }
