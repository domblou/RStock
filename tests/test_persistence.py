import json

from rstock.persistence import iter_model_metadata


def test_phase_one_metadata_without_standard_metrics_remains_readable(tmp_path):
    payload = {
        "schema_version": 1,
        "set_name": "AAA-BBB",
        "observation": "AAA",
        "features": ["BBB"],
        "predictor_columns": ["BBB.DAY_MINUS_1_UPDW"],
        "error": 0.1,
        "error_metric": "legacy_predictors",
        "model_file": "model_000000.ubj",
    }
    (tmp_path / "model_000000.metadata.json").write_text(
        json.dumps(payload), encoding="utf-8"
    )

    bundles = iter_model_metadata(tmp_path)

    assert len(bundles) == 1
    assert bundles[0][1].classification_metrics == {}
