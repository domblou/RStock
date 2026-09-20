import pyarrow as pa

from rstock.application.temporal_validation_ui import (
    candidate_identity_tables,
    temporal_validation_gate_table,
)


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
