from dataclasses import replace

import pandas as pd
import pyarrow as pa
import pytest

from rstock.application import history_analysis
from rstock.application.history_analysis import (
    altair_serializable_distribution,
    analyze_run,
    comparison_display_table,
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
    threshold_calibration_choice_diagnostic_table,
    threshold_calibration_selection_summary,
    threshold_promotion_guidance,
    threshold_sensitivity_best_column,
    threshold_sensitivity_grid,
    threshold_sensitivity_summary,
    threshold_sensitivity_table,
    xgboost_calibration_selection_display_table,
    xgboost_calibration_selection_table,
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


def test_comparison_display_table_normalizes_mixed_values_for_arrow():
    display = comparison_display_table(
        pd.DataFrame(
            {
                "Indicateur": ["AUC", "Statut", "Valeur absente"],
                "2026-09-15 17:23": [0.74, "completed", None],
            }
        )
    )

    assert display["2026-09-15 17:23"].tolist() == ["0.74", "completed", "—"]
    assert str(display["2026-09-15 17:23"].dtype) == "string"
    pa.Table.from_pandas(display, preserve_index=False)


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


def test_xgboost_calibration_selection_table_projects_selected_configs_and_holdout():
    selected = {
        "Up": {
            "configuration": "candidate_up",
            "selection_score": 0.61234,
            "parameters": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120},
        },
        "Down": {
            "configuration": "candidate_down",
            "selection_score": 0.52345,
            "parameters": {"max_depth": 3, "eta": 0.1, "num_boost_round": 80},
        },
    }
    development = pd.DataFrame([
        {"Direction": "Up", "Configuration": "candidate_up", "SelectionScore": 0.61234, "ROCAUCMedian": 0.63, "PRAUCMedian": 0.41, "ROCAUCStd": 0.03},
        {"Direction": "Up", "Configuration": "runner_up", "SelectionScore": 0.59},
        {"Direction": "Down", "Configuration": "candidate_down", "SelectionScore": 0.52345, "ROCAUCMedian": 0.58, "PRAUCMedian": 0.36, "ROCAUCStd": 0.04},
        {"Direction": "Down", "Configuration": "runner_up", "SelectionScore": 0.50},
    ])
    holdout = pd.DataFrame([
        {"Direction": "Up", "Configuration": "candidate_up", "ROCAUC": 0.55, "PRAUC": 0.31, "Precision": 0.0, "Recall": 0.0, "F1": 0.0, "PositivePredictionRate": 0.02, "Prevalence": 0.30, "TP": 0, "FP": 3, "Sets": 4, "Observations": 120},
        {"Direction": "Up", "Configuration": "wrong_configuration", "ROCAUC": 0.99},
        {"Direction": "Down", "Configuration": "candidate_down", "ROCAUC": 0.57, "PRAUC": 0.35, "Precision": 0.4, "Recall": 0.2, "F1": 0.27, "PositivePredictionRate": 0.1, "Prevalence": 0.2, "TP": 2, "FP": 3, "Sets": 4, "Observations": 120},
    ])

    table = xgboost_calibration_selection_table(selected, development, holdout)
    up = table[table["Direction"] == "Up"].iloc[0]
    down = table[table["Direction"] == "Down"].iloc[0]

    assert up["Configuration sélectionnée"] == "candidate_up"
    assert up["Écart avec le 2e meilleur candidat"] == pytest.approx(0.02234)
    assert down["Écart avec le 2e meilleur candidat"] == pytest.approx(0.02345)
    assert up["ROC-AUC holdout"] == 0.55
    assert up["TP"] == 0
    assert up["Rappel holdout"] == 0.0
    assert up["Paramètres clés"] == "d2 / η0.05 / 120"
    assert down["Paramètres clés"] == "d3 / η0.1 / 80"

    display = xgboost_calibration_selection_display_table(table)
    up_display = display[display["Direction"] == "Up"].iloc[0]
    assert up_display["ROC-AUC holdout"] == "0.550"
    assert up_display["TP"] == "0"
    assert up_display["Rappel holdout"] == "0.000"


def test_xgboost_calibration_selection_table_displays_missing_development_metrics():
    selected = {
        "Up": {"configuration": "baseline", "parameters": {}},
        "Down": {"configuration": "baseline", "parameters": {}},
    }

    table = xgboost_calibration_selection_display_table(
        xgboost_calibration_selection_table(selected, pd.DataFrame(), pd.DataFrame())
    )

    assert table["ROC-AUC développement"].tolist() == ["—", "—"]
    assert table["Écart avec le 2e meilleur candidat"].tolist() == ["—", "—"]
    assert table["Paramètres clés"].tolist() == ["—", "—"]


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


def test_threshold_promotion_guidance_blocks_ineligible_holdout_rows_without_mutation():
    results = pd.DataFrame([{
        "Combinaison": "AAA<-BBB", "Cible": "AAA", "Predictors": "BBB",
        "Direction": "Up", "Seuil calibré": 0.60, "Signaux holdout": 19,
        "AUC holdout": 0.70, "Précision holdout": 0.75,
        "Rendement directionnel moyen": 0.02,
        "Fréquence mouvement opposé": 0.10,
    }])
    original = results.copy(deep=True)

    guided = threshold_promotion_guidance(
        results,
        pd.DataFrame([{
            "Combinaison": "AAA<-BBB", "Direction": "Up", "Diagnostic": "near_optimal"
        }]),
        {"AAA<-BBB": {"Up": {"status": "selected", "threshold": 0.60}}},
    )

    assert guided.iloc[0]["Statut promotion"] == "Non candidat"
    assert pd.isna(guided.iloc[0]["Score promotion"])
    assert "Trop peu de signaux" in guided.iloc[0]["Raison promotion"]
    assert guided.columns[:6].tolist() == [
        "Combinaison", "Cible", "Predictors", "Direction", "Statut promotion", "Score promotion"
    ]
    pd.testing.assert_frame_equal(results, original)


def test_threshold_promotion_score_is_normalized_and_maps_statuses_and_filters():
    rows = [
        ("STRONG<-BBB", 0.70, 0.75, 0.015, 0.15, 60, "near_optimal"),
        ("REVIEW<-BBB", 0.65, 0.60, 0.010, 0.15, 50, "lower_threshold_better"),
        ("LOW<-BBB", 0.56, 0.51, 0.001, 0.29, 21, "unstable"),
    ]
    results = pd.DataFrame([{
        "Combinaison": set_name, "Cible": set_name.split("<-")[0], "Predictors": "BBB",
        "Direction": "Up", "Seuil calibré": 0.60, "AUC holdout": auc,
        "Précision holdout": precision, "Rendement directionnel moyen": directional_return,
        "Fréquence mouvement opposé": opposite, "Signaux holdout": signals,
    } for set_name, auc, precision, directional_return, opposite, signals, _ in rows])
    sensitivity = pd.DataFrame([{
        "Combinaison": set_name, "Direction": "Up", "Diagnostic": diagnostic
    } for set_name, *_, diagnostic in rows])
    selected = {
        set_name: {"Up": {"status": "selected", "threshold": 0.60}}
        for set_name, *_ in rows
    }

    guided = threshold_promotion_guidance(results, sensitivity, selected)
    statuses = dict(zip(guided["Combinaison"], guided["Statut promotion"], strict=True))
    scores = dict(zip(guided["Combinaison"], guided["Score promotion"], strict=True))

    assert statuses == {
        "STRONG<-BBB": "Candidat fort",
        "REVIEW<-BBB": "À examiner",
        "LOW<-BBB": "Non candidat",
    }
    assert scores["STRONG<-BBB"] == pytest.approx(96.2)
    assert scores["REVIEW<-BBB"] == pytest.approx(73.8)
    assert all(0 <= score <= 100 for score in scores.values())
    assert filter_threshold_calibration_results(
        guided, direction="Toutes", min_signals=0, promotion_status="Candidat fort"
    )["Combinaison"].tolist() == ["STRONG<-BBB"]


def test_threshold_promotion_uses_the_revised_normalization_bounds():
    assert history_analysis._promotion_scale(0.55, 0.55, 0.70) == 0.0
    assert history_analysis._promotion_scale(0.70, 0.55, 0.70) == 1.0
    assert history_analysis._promotion_scale(0.40, 0.40, 0.65) == 0.0
    assert history_analysis._promotion_scale(0.65, 0.40, 0.65) == 1.0
    assert history_analysis._promotion_scale(0.00, 0.00, 0.015) == 0.0
    assert history_analysis._promotion_scale(0.015, 0.00, 0.015) == 1.0
    assert history_analysis._promotion_signal_scale(20) == 0.5
    assert history_analysis._promotion_signal_scale(50) == 1.0
    assert history_analysis._promotion_signal_scale(80) == 1.0

    results = pd.DataFrame([{
        "Combinaison": "AAA<-BBB", "Cible": "AAA", "Predictors": "BBB",
        "Direction": "Up", "Seuil calibré": 0.60, "AUC holdout": 0.716,
        "Précision holdout": 0.55, "Rendement directionnel moyen": 0.0077,
        "Fréquence mouvement opposé": 0.10, "Signaux holdout": 20,
    }])
    sensitivity = pd.DataFrame([{
        "Combinaison": "AAA<-BBB", "Direction": "Up", "Diagnostic": "near_optimal",
    }])
    selected = {"AAA<-BBB": {"Up": {"status": "selected", "threshold": 0.60}}}

    guided = threshold_promotion_guidance(results, sensitivity, selected)

    assert guided.iloc[0]["Score promotion"] == pytest.approx(77.3)
    assert guided.iloc[0]["Statut promotion"] == "Candidat fort"


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


def test_threshold_sensitivity_summary_uses_visible_rows_and_computes_deltas():
    holdout = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.10, "Target": 0, "IntradayReturn": -0.02},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.20, "Target": 0, "IntradayReturn": -0.01},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.30, "Target": 1, "IntradayReturn": 0.02},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.40, "Target": 1, "IntradayReturn": 0.03},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.50, "Target": 1, "IntradayReturn": 0.04},
        {"Set": "CCC<-DDD", "Direction": "Up", "Probability": 0.10, "Target": 0, "IntradayReturn": -0.02},
        {"Set": "CCC<-DDD", "Direction": "Up", "Probability": 0.20, "Target": 1, "IntradayReturn": 0.02},
        {"Set": "CCC<-DDD", "Direction": "Up", "Probability": 0.30, "Target": 0, "IntradayReturn": -0.01},
        {"Set": "CCC<-DDD", "Direction": "Up", "Probability": 0.40, "Target": 1, "IntradayReturn": 0.03},
        {"Set": "CCC<-DDD", "Direction": "Up", "Probability": 0.50, "Target": 1, "IntradayReturn": 0.04},
    ])
    visible = pd.DataFrame([
        {"Cible": "AAA", "Combinaison": "AAA<-BBB", "Direction": "Up", "Seuil calibré": 0.20},
        {"Cible": "CCC", "Combinaison": "CCC<-DDD", "Direction": "Up", "Seuil calibré": 0.30},
    ])
    original_holdout = holdout.copy(deep=True)
    original_visible = visible.copy(deep=True)

    summary = threshold_sensitivity_summary(
        visible,
        holdout,
        minimum_robust_signals=2,
        sensitivity_threshold_min=0.10,
        sensitivity_threshold_max=0.50,
        sensitivity_threshold_step=0.10,
    )

    assert summary["Combinaison"].tolist() == ["AAA<-BBB", "CCC<-DDD"]
    first = summary.iloc[0]
    assert first["Meilleur seuil robuste"] == 0.30
    assert first["Delta seuil"] == pytest.approx(0.10)
    assert first["Signaux au seuil calibré"] == 4
    assert first["Signaux au meilleur seuil robuste"] == 3
    assert first["Précision au seuil calibré"] == pytest.approx(0.75)
    assert first["Précision au meilleur seuil robuste"] == pytest.approx(1.0)
    assert first["Delta précision"] == pytest.approx(0.25)
    assert first["Delta rendement"] == pytest.approx(0.01)
    assert first["Diagnostic"] == "higher_threshold_better"
    assert holdout.equals(original_holdout)
    assert visible.equals(original_visible)

    filtered_summary = threshold_sensitivity_summary(
        visible.iloc[[1]],
        holdout,
        minimum_robust_signals=2,
        sensitivity_threshold_min=0.10,
        sensitivity_threshold_max=0.50,
        sensitivity_threshold_step=0.10,
    )
    assert filtered_summary["Combinaison"].tolist() == ["CCC<-DDD"]


def test_threshold_sensitivity_summary_does_not_build_detailed_tables(monkeypatch):
    visible = pd.DataFrame([{
        "Cible": "AAA", "Combinaison": "AAA<-BBB", "Direction": "Up",
        "Seuil calibré": 0.30,
    }])
    holdout = pd.DataFrame([
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.20, "Target": 0, "IntradayReturn": -0.01},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.30, "Target": 1, "IntradayReturn": 0.02},
        {"Set": "AAA<-BBB", "Direction": "Up", "Probability": 0.40, "Target": 1, "IntradayReturn": 0.03},
    ])

    def detailed_table_must_not_be_called(*_args, **_kwargs):
        raise AssertionError("The aggregate view must not build detailed tables")

    monkeypatch.setattr(
        history_analysis, "threshold_sensitivity_table", detailed_table_must_not_be_called
    )

    summary = threshold_sensitivity_summary(
        visible, holdout, minimum_robust_signals=1,
        sensitivity_threshold_min=0.10,
        sensitivity_threshold_max=0.50,
        sensitivity_threshold_step=0.10,
    )

    assert summary.iloc[0]["Diagnostic"] == "near_optimal"


def test_calibration_choice_diagnostics_use_persisted_candidates_for_visible_rows():
    metrics = pd.DataFrame([
        {
            "Set": "AAA<-BBB", "Direction": "Up", "Threshold": 0.30,
            "Selected": False, "Eligible": True, "SelectionRank": 2,
            "SelectionReason": None, "RejectionReason": "not_selected_by_economic_order",
            "TotalSignals": 24, "EligibleWindowFraction": 1.0,
            "Precision": 0.70, "PrecisionStd": 0.03,
            "DirectionalReturnMean": 0.02, "DirectionalReturnMeanStd": 0.01,
            "OppositeMoveFrequency": 0.15, "F1Median": 0.65,
        },
        {
            "Set": "AAA<-BBB", "Direction": "Up", "Threshold": 0.40,
            "Selected": True, "Eligible": True, "SelectionRank": 1,
            "SelectionReason": "robust_sample_preferred", "RejectionReason": None,
            "TotalSignals": 18, "EligibleWindowFraction": 1.0,
            "Precision": 0.75, "PrecisionStd": 0.02,
            "DirectionalReturnMean": 0.03, "DirectionalReturnMeanStd": 0.01,
            "OppositeMoveFrequency": 0.10, "F1Median": 0.70,
        },
        {
            "Set": "CCC<-DDD", "Direction": "Up", "Threshold": 0.40,
            "Selected": True, "Eligible": True, "SelectionRank": 1,
            "SelectionReason": "robust_sample_preferred", "RejectionReason": None,
            "TotalSignals": 20, "EligibleWindowFraction": 1.0,
            "Precision": 0.80, "PrecisionStd": 0.02,
            "DirectionalReturnMean": 0.04, "DirectionalReturnMeanStd": 0.01,
            "OppositeMoveFrequency": 0.08, "F1Median": 0.75,
        },
    ])
    visible = pd.DataFrame([
        {"Combinaison": "AAA<-BBB", "Direction": "Up"},
    ])
    original = metrics.copy(deep=True)

    compact = threshold_calibration_selection_summary(visible, metrics)
    detailed = threshold_calibration_choice_diagnostic_table(
        metrics, set_name="AAA<-BBB", direction="Up"
    )

    assert compact.to_dict("records") == [{
        "Combinaison": "AAA<-BBB", "Direction": "Up",
        "Raison sélection calibration": "robust_sample_preferred",
        "Rang du seuil calibré": 1,
        "Nombre de candidats admissibles": 2,
    }]
    assert detailed["Seuil"].tolist() == [0.30, 0.40]
    assert detailed.loc[detailed["Sélectionné"], "Raison de rejet / sélection"].iloc[0] == "robust_sample_preferred"
    assert detailed.loc[~detailed["Sélectionné"], "Raison de rejet / sélection"].iloc[0] == "not_selected_by_economic_order"
    assert metrics.equals(original)


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


def test_altair_serializable_distribution_converts_interval_categories_only():
    distribution = pd.Series(
        [3, 5],
        index=pd.IntervalIndex.from_breaks([0.014, 0.0141, 0.0142]),
        name="Nombre de prédictions",
    )

    serializable = altair_serializable_distribution(distribution)

    assert serializable.tolist() == [3, 5]
    assert serializable.index.tolist() == ["(0.014, 0.0141]", "(0.0141, 0.0142]"]
    assert all(isinstance(category, str) for category in serializable.index)
    assert isinstance(distribution.index[0], pd.Interval)
