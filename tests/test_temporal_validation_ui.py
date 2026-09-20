import pyarrow as pa

from rstock.application.temporal_validation_ui import (
    candidate_identity_tables,
    forced_candidate_revalidation_table,
    lost_candidate_display_table,
    temporal_validation_gate_table,
)


def test_forced_candidate_revalidation_table_keeps_single_and_two_predictor_sets():
    stability = {"common_candidates": [], "lost_candidates": [
        {"symbol_set_id": '["DDOG","REGN"]', "target": "DDOG", "predictors": ["REGN"], "direction": "Up", "holdout_auc": 0.61},
        {"symbol_set_id": '["VLO","TMO","MMM"]', "target": "VLO", "predictors": ["TMO", "MMM"], "direction": "Up", "holdout_auc": 0.64},
    ]}
    traces = {
        ('["DDOG","REGN"]', "Up"): {"status": "Échec qualification WF", "last_stage": "Qualification WF", "reason": "Worst AUC 0.402 < 0.45"},
        ('["VLO","TMO","MMM"]', "Up"): {"status": "Candidat confirmé", "last_stage": "Candidat", "reason": "—"},
    }
    forced = {('["VLO","TMO","MMM"]', "Up"): {"holdout_auc": 0.62}}
    table = forced_candidate_revalidation_table(stability, forced, traces, {'["VLO","TMO","MMM"]'})
    assert table[["Cible", "Predictors", "Statut", "Promotion"]].to_dict("records") == [
        {"Cible": "DDOG", "Predictors": "REGN", "Statut": "Échec qualification WF", "Promotion": "Non promu"},
        {"Cible": "VLO", "Predictors": "TMO + MMM", "Statut": "Candidat confirmé", "Promotion": "Promu"},
    ]


def test_temporal_validation_gate_table_preserves_large_bootstrap_seed_as_text():
    too_large_for_arrow = 11706298947129035838
    table = temporal_validation_gate_table(
        {
            "precision_edge": {
                "status": "passed",
                "reason": "ok",
                "metrics": {
                    "precision_edge": 0.12,
                    "signal_count": 416,
                    "bootstrap": {
                        "seed": too_large_for_arrow,
                        "replications": 2000,
                    },
                },
            }
        }
    )

    metrics = table.loc[0, "Metrics"]
    assert metrics["bootstrap"]["seed"] == str(too_large_for_arrow)
    assert metrics["bootstrap"]["replications"] == 2000
    assert metrics["signal_count"] == 416
    assert metrics["precision_edge"] == 0.12

    arrow_table = pa.Table.from_pandas(table, preserve_index=False)
    assert arrow_table.column("Metrics").type.field("bootstrap").type.field("seed").type == pa.string()


def test_candidate_identity_tables_render_empty_categories_without_error():
    tables = candidate_identity_tables({
        "common_candidates": [],
        "lost_candidates": [],
        "new_candidates": [
            {
                "target": "AAA",
                "predictors": ["BBB", "CCC"],
                "direction": "Up",
                "holdout_precision": 0.6,
                "holdout_auc": 0.7,
                "directional_return_mean": 0.01,
                "holdout_signal_count": 20,
            }
        ],
    })

    assert tables["common"].empty
    assert tables["lost"].empty
    assert tables["new"].iloc[0].to_dict() == {
        "Cible": "AAA",
        "Predictors": "BBB + CCC",
        "Direction": "Up",
        "Précision val.": 0.6,
        "AUC val.": 0.7,
        "Rendement val.": 0.01,
        "Signaux val.": 20,
    }


def test_historical_artifact_without_identity_section_is_displayable():
    tables = candidate_identity_tables(None)

    assert all(table.empty for table in tables.values())


def test_lost_candidates_include_existing_validation_metrics_and_reason():
    stability = {
        "lost_candidates": [
            {
                "target": "AAA",
                "predictors": ["BBB"],
                "direction": "Up",
                "symbol_set_id": '["AAA","BBB"]',
                "holdout_precision": 0.69,
                "holdout_auc": 0.71,
                "directional_return_mean": 0.021,
                "holdout_signal_count": 31,
            }
        ],
        "common_candidates": [],
        "new_candidates": [],
    }
    validation_lookup = {
        ('["AAA","BBB"]', "Up"): {
            "holdout_precision": 0.35,
            "holdout_auc": 0.58,
            "directional_return_mean": -0.004,
            "holdout_signal_count": 18,
            "opposite_move_frequency": 0.34,
            "promotion_reason": "18 signaux < 20 ; AUC 0,58 < 0,60",
        }
    }

    tables = candidate_identity_tables(stability, validation_lookup)

    assert list(tables["lost"].columns) == [
        "Cible", "Predictors", "Direction", "Précision réf.", "Précision val.",
        "AUC réf.", "AUC val.", "Rendement réf.", "Rendement val.",
        "Signaux réf.", "Signaux val.", "Mouv. opposé val.",
        "Critère(s) échoué(s)", "Dernière étape atteinte", "Raison d’élimination",
    ]
    assert tables["lost"].iloc[0].to_dict() == {
        "Cible": "AAA",
        "Predictors": "BBB",
        "Direction": "Up",
        "Précision réf.": 0.69,
        "Précision val.": 0.35,
        "AUC réf.": 0.71,
        "AUC val.": 0.58,
        "Rendement réf.": 0.021,
        "Rendement val.": -0.004,
        "Signaux réf.": 31,
        "Signaux val.": 18,
        "Mouv. opposé val.": 0.34,
        "Critère(s) échoué(s)": "18 signaux < 20 ; AUC 0,58 < 0,60",
        "Dernière étape atteinte": "—",
        "Raison d’élimination": "—",
    }
    assert lost_candidate_display_table(tables["lost"]).iloc[0].to_dict()[
        "Critère(s) échoué(s)"
    ] == "18 signaux < 20 · AUC 0,58 < 0,60"


def test_lost_candidate_missing_validation_values_display_as_dash():
    tables = candidate_identity_tables({
        "lost_candidates": [
            {
                "target": "AAA",
                "predictors": [],
                "direction": "Up",
                "symbol_set_id": '["AAA"]',
                "holdout_precision": None,
                "holdout_auc": None,
                "directional_return_mean": None,
                "holdout_signal_count": None,
            }
        ]
    })

    row = lost_candidate_display_table(tables["lost"]).iloc[0].to_dict()
    assert all(
        row[column] == "—"
        for column in (
            "Précision réf.", "Précision val.", "AUC réf.", "AUC val.",
            "Rendement réf.", "Rendement val.", "Signaux réf.", "Signaux val.",
            "Mouv. opposé val.", "Critère(s) échoué(s)",
        )
    )
