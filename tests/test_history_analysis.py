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
    filter_threshold_calibration_results,
    run_universe_summary,
    predictor_prefilter_summary,
    selected_run_action,
    threshold_calibration_table,
    threshold_sensitivity_best_column,
    threshold_sensitivity_grid,
    threshold_sensitivity_table,
)
from rstock.config import DEFAULT_CONFIG
from rstock.threshold_calibration import adaptive_threshold_grid, calibrate_thresholds


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


def test_combination_table_exposes_persisted_score_and_components():
    scores = pd.DataFrame([{
        "Set": "DIS<-PFE+WMT",
        "model_selection_score": 78.25,
        "model_selection_rank": 1,
        "predictive_quality_score": 52.0,
        "stability_score": 84.0,
        "holdout_score": 76.0,
        "signal_quality_score": pd.NA,
        "sample_adequacy_score": 100.0,
    }])

    table = combination_table(
        _qualification(), _holdout(), depth=3, selection_results=scores
    )
    row = table[table["Combinaison"] == "DIS<-PFE+WMT"].iloc[0]

    assert row["Score"] == 78.25
    assert row["Rang"] == 1
    assert row["Score stabilité"] == 84.0
    assert pd.isna(row["Score qualité signal"])


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


def test_predictor_prefilter_summary_is_compact_and_legacy_safe():
    table = predictor_prefilter_summary({
        "predictor_prefilter": [{
            "target": "DIS",
            "initial_candidates": 49,
            "after_qualification": 18,
            "after_top_n": 10,
            "after_redundancy": 8,
            "retained_predictors": ["A", "B", "C", "D", "E", "F", "G", "H"],
            "combinations_before_filtering": 19649,
            "combinations_tested": 92,
        }]
    })

    assert table.iloc[0].to_dict() == {
        "Cible": "DIS",
        "Candidats initiaux": 49,
        "Rejet AUC médiane": "—",
        "Rejet fenêtres > 0,50": "—",
        "Rejet Worst AUC": "—",
        "Rejet dispersion": "—",
        "Après qualification": 18,
        "Après Top N": 10,
        "Après redondance": 8,
        "Retenus": 8,
        "Combinaisons": "92 au lieu de 19649",
    }
    assert predictor_prefilter_summary({}).empty


def test_predictor_prefilter_summary_exposes_threshold_rejection_counts():
    table = predictor_prefilter_summary({
        "predictor_prefilter": [{
            "target": "DIS",
            "initial_candidates": 5,
            "rejected_median_auc": 2,
            "rejected_pct_above_random": 1,
            "rejected_worst_auc": 3,
            "rejected_auc_std": 1,
            "after_qualification": 2,
            "after_top_n": 2,
            "after_redundancy": 2,
            "retained_predictors": ["AMZN", "NVDA"],
            "combinations_before_filtering": 15,
            "combinations_tested": 3,
        }]
    })

    assert table.iloc[0]["Rejet AUC médiane"] == 2
    assert table.iloc[0]["Rejet fenêtres > 0,50"] == 1
    assert table.iloc[0]["Rejet Worst AUC"] == 3
    assert table.iloc[0]["Rejet dispersion"] == 1


def test_threshold_calibration_table_uses_per_set_thresholds_and_holdout_metrics():
    selected = {
        "DIS<-AMZN+NVDA": {
            "Up": {"status": "selected", "threshold": 0.71},
            "Down": {"status": "selected", "threshold": 0.33},
        }
    }
    holdout = pd.DataFrame([
        {
            "Set": "DIS<-AMZN+NVDA", "Observation": "DIS", "Direction": "Up",
            "Threshold": 0.71, "SignalCount": 12, "Precision": 0.75,
            "Recall": 0.5, "F1": 0.6, "ROCAUC": 0.66,
            "DirectionalReturnMean": 0.012, "IntradayReturnMedian": 0.01,
            "MFEMean": 0.02, "MAEMean": -0.01,
            "OppositeMoveFrequency": 0.25,
        },
        {
            "Set": "DIS<-AMZN+NVDA", "Observation": "DIS", "Direction": "Down",
            "Threshold": 0.33, "SignalCount": 8, "Precision": 0.5,
        },
    ])

    table = threshold_calibration_table(pd.DataFrame(), holdout, selected)
    up = table[table["Direction"] == "Up"].iloc[0]

    assert up["Cible"] == "DIS"
    assert up["Predictors"] == "AMZN + NVDA"
    assert up["Seuil calibré"] == 0.71
    assert up["Précision holdout"] == 0.75
    assert up["Fréquence mouvement opposé"] == 0.25


def test_threshold_calibration_filters_direction_and_missing_legacy_metrics():
    selected = {"AAA<-BBB": {"Up": {"status": "selected", "threshold": 0.6}}}
    legacy = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Threshold": 0.6, "Selected": True},
        {"Set": "AAA<-BBB", "Direction": "Down", "Threshold": 0.4, "Selected": True},
    ])

    table = threshold_calibration_table(legacy, pd.DataFrame(), selected)
    filtered = filter_threshold_calibration_results(table, direction="Up", min_signals=0)
    down = filter_threshold_calibration_results(table, direction="Down", min_signals=0)

    assert filtered["Direction"].tolist() == ["Up"]
    assert down["Direction"].tolist() == ["Down"]
    assert pd.isna(filtered.iloc[0]["AUC holdout"])


def test_threshold_calibration_quality_filters_combine_with_direction_and_signals():
    table = pd.DataFrame([
        {
            "Combinaison": "AAA<-BBB", "Direction": "Up", "Signaux holdout": 12,
            "Précision holdout": 0.72, "AUC holdout": 0.67,
            "Fréquence mouvement opposé": 0.20,
            "Rendement directionnel moyen": 0.012,
        },
        {
            "Combinaison": "CCC<-DDD", "Direction": "Up", "Signaux holdout": 12,
            "Précision holdout": 0.72, "AUC holdout": 0.67,
            "Fréquence mouvement opposé": 0.45,
            "Rendement directionnel moyen": 0.012,
        },
        {
            "Combinaison": "EEE<-FFF", "Direction": "Down", "Signaux holdout": 20,
            "Précision holdout": 0.95, "AUC holdout": 0.90,
            "Fréquence mouvement opposé": 0.05,
            "Rendement directionnel moyen": 0.03,
        },
    ])

    filtered = filter_threshold_calibration_results(
        table,
        direction="Up",
        min_signals=10,
        min_precision=0.70,
        min_holdout_auc=0.65,
        max_opposite_move_frequency=0.25,
        min_directional_return=0.01,
    )

    assert filtered["Combinaison"].tolist() == ["AAA<-BBB"]


def test_threshold_sensitivity_reprojects_holdout_probabilities_without_mutation():
    holdout = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.20, "Target": 0, "IntradayReturn": -0.02, "MFE": 0.01, "MAE": -0.03},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.30, "Target": 1, "IntradayReturn": 0.01, "MFE": 0.02, "MAE": -0.01},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.50, "Target": 1, "IntradayReturn": 0.02, "MFE": 0.03, "MAE": -0.01},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.70, "Target": 0, "IntradayReturn": -0.03, "MFE": 0.01, "MAE": -0.04},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.80, "Target": 1, "IntradayReturn": 0.04, "MFE": 0.05, "MAE": -0.01},
    ])
    original = holdout.copy(deep=True)

    sensitivity = threshold_sensitivity_table(
        holdout,
        set_name="AAA<-BBB",
        direction="Up",
        calibrated_threshold=0.73,
        thresholds=(0.30, 0.50, 0.95),
        minimum_robust_signals=5,
    )
    at_half = sensitivity.loc[sensitivity["Seuil"] == 0.50].iloc[0]
    at_high = sensitivity.loc[sensitivity["Seuil"] == 0.95].iloc[0]

    assert sensitivity["Seuil"].tolist() == sorted(sensitivity["Seuil"].tolist())
    assert 0.73 in sensitivity["Seuil"].tolist()
    assert sensitivity.loc[
        sensitivity["Seuil"] == 0.73, "Seuil calibré actuel"
    ].iloc[0] == "✓"
    assert at_half["Nombre de signaux"] == 3
    assert at_half["Précision"] == pytest.approx(2 / 3)
    assert at_half["Rendement directionnel moyen"] == pytest.approx(0.01)
    assert at_half["Rendement médian"] == pytest.approx(0.02)
    assert at_half["Fréquence mouvement opposé"] == pytest.approx(1 / 3)
    assert at_high["Nombre de signaux"] == 0
    assert at_high["Précision"] == 0.0
    assert sensitivity[threshold_sensitivity_best_column(5)].eq("✓").sum() == 0
    assert holdout.equals(original)


def test_threshold_sensitivity_grid_uses_configured_minimum_maximum_and_step():
    assert threshold_sensitivity_grid(0.10, 0.20, 0.025) == (
        0.10, 0.125, 0.15, 0.175, 0.20,
    )


def test_threshold_sensitivity_adds_the_exact_calibrated_threshold_to_custom_grid():
    holdout = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.20, "Target": 0, "IntradayReturn": -0.02},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.40, "Target": 1, "IntradayReturn": 0.02},
    ])

    sensitivity = threshold_sensitivity_table(
        holdout,
        set_name="AAA<-BBB",
        direction="Up",
        calibrated_threshold=0.137,
        sensitivity_threshold_min=0.10,
        sensitivity_threshold_max=0.20,
        sensitivity_threshold_step=0.025,
        minimum_robust_signals=1,
    )

    assert sensitivity["Seuil"].tolist() == [0.10, 0.125, 0.137, 0.15, 0.175, 0.20]
    assert sensitivity.loc[
        sensitivity["Seuil"] == 0.137, "Seuil calibré actuel"
    ].iloc[0] == "✓"


def test_sensitivity_settings_do_not_change_calibration_grid_or_selection():
    rows = []
    for direction in ("Up", "Down"):
        for probability, target, intraday_return in (
            (0.20, 0, -0.02), (0.40, 1, 0.02),
            (0.60, 1 if direction == "Up" else 0, 0.03),
        ):
            rows.append({
                "Direction": direction, "Window": 1, "Date": "2025-01-01",
                "Probability": probability, "Target": target,
                "IntradayReturn": intraday_return, "MFE": 0.03, "MAE": -0.02,
            })
    predictions = pd.DataFrame(rows)
    config = replace(
        DEFAULT_CONFIG,
        threshold_calibration_min_signals_per_window=1,
        threshold_calibration_quantiles=(0.5,),
    )
    grid_before = adaptive_threshold_grid(predictions, config)
    selected_before = calibrate_thresholds(predictions, config).selected_thresholds

    threshold_sensitivity_table(
        predictions.assign(Set="AAA<-BBB"),
        set_name="AAA<-BBB",
        direction="Up",
        calibrated_threshold=0.137,
        sensitivity_threshold_min=0.10,
        sensitivity_threshold_max=0.60,
        sensitivity_threshold_step=0.025,
        minimum_robust_signals=1,
    )

    pd.testing.assert_frame_equal(grid_before, adaptive_threshold_grid(predictions, config))
    assert calibrate_thresholds(predictions, config).selected_thresholds == selected_before


def test_threshold_sensitivity_is_unavailable_without_persisted_probabilities():
    sensitivity = threshold_sensitivity_table(
        pd.DataFrame({"Set": ["AAA<-BBB"]}),
        set_name="AAA<-BBB",
        direction="Up",
        calibrated_threshold=0.5,
        minimum_robust_signals=10,
    )

    assert sensitivity.empty


def test_threshold_sensitivity_uses_the_configured_robust_signal_minimum():
    holdout = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.30, "Target": 1, "IntradayReturn": 0.01},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.40, "Target": 1, "IntradayReturn": 0.02},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.50, "Target": 0, "IntradayReturn": -0.01},
    ])

    relaxed = threshold_sensitivity_table(
        holdout, set_name="AAA<-BBB", direction="Up", calibrated_threshold=0.5,
        thresholds=(0.5,), minimum_robust_signals=1,
    )
    strict = threshold_sensitivity_table(
        holdout, set_name="AAA<-BBB", direction="Up", calibrated_threshold=0.5,
        thresholds=(0.5,), minimum_robust_signals=2,
    )

    assert threshold_sensitivity_best_column(1) in relaxed.columns
    assert relaxed[threshold_sensitivity_best_column(1)].tolist() == ["✓"]
    assert strict[threshold_sensitivity_best_column(2)].tolist() == [""]
