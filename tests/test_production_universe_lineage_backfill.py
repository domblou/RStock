from __future__ import annotations

import json

from rstock.application.production_domain import ProductionModel
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_universe_lineage_backfill import (
    backfill_primary_universe_names,
)
from rstock.application.universes import UniverseService


def _model(model_id: str) -> ProductionModel:
    return ProductionModel(
        model_id=model_id,
        target="AAA",
        predictors=("BBB",),
        lag_depth=1,
        target_definition="Up >= 1%",
        up_target_threshold=0.01,
        down_target_threshold=0.01,
        xgboost_parameters={"max_depth": 1},
        up_threshold=0.6,
        down_threshold=0.4,
        qualification_rules={},
        source_walk_forward_run="wf",
        source_xgboost_calibration_run=None,
        source_threshold_calibration_run=None,
        development_metrics={},
        holdout_metrics={},
        created_at="2026-09-20T12:00:00+00:00",
    )


def _universe(root, *, updated_at="2026-09-19T12:00:00+00:00") -> str:
    service = UniverseService(root=root)
    record = service.create("Univers historique", ("AAA", "BBB"))
    metadata = root / "data" / "universes" / "universes.json"
    payload = json.loads(metadata.read_text(encoding="utf-8"))
    payload["universes"][0]["updated_at"] = updated_at
    metadata.write_text(json.dumps(payload), encoding="utf-8")
    return record.universe_id


def _lineage(model_id: str, *, name=None, universe_id="PRIMARY", promotion="2026-09-20T12:00:00+00:00"):
    return {
        "model_id": model_id,
        "primary_universe_id": universe_id,
        "primary_universe_name_at_promotion": name,
        "promotion_date": promotion,
    }


def test_backfill_fills_blank_name_from_registry_that_precedes_promotion(tmp_path):
    model = _model("model_blank")
    ProductionRepository(tmp_path).add(model)
    universe_id = _universe(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, universe_id=universe_id))

    report = backfill_primary_universe_names(tmp_path)

    assert report["counts"] == {"corrected": 1, "already_populated": 0, "unresolved": 0, "errors": 0}
    assert quality.load_lineage(model.model_id)["primary_universe_name_at_promotion"] == "Univers historique"
    assert report["models_corrected"][0]["source"] == "persisted_universe_registry_preceding_promotion"


def test_backfill_never_overwrites_an_existing_name(tmp_path):
    model = _model("model_frozen")
    ProductionRepository(tmp_path).add(model)
    universe_id = _universe(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, name="Nom figé", universe_id=universe_id))

    report = backfill_primary_universe_names(tmp_path)

    assert report["counts"] == {"corrected": 0, "already_populated": 1, "unresolved": 0, "errors": 0}
    assert quality.load_lineage(model.model_id)["primary_universe_name_at_promotion"] == "Nom figé"


def test_backfill_leaves_name_empty_when_historical_registry_record_is_unavailable(tmp_path):
    model = _model("model_unknown")
    ProductionRepository(tmp_path).add(model)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, universe_id="MISSING"))

    report = backfill_primary_universe_names(tmp_path)

    assert report["counts"] == {"corrected": 0, "already_populated": 0, "unresolved": 1, "errors": 0}
    assert report["models_unresolved"][0]["reason"] == "universe_record_missing"
    assert quality.load_lineage(model.model_id)["primary_universe_name_at_promotion"] is None


def test_backfill_is_idempotent(tmp_path):
    model = _model("model_idempotent")
    ProductionRepository(tmp_path).add(model)
    universe_id = _universe(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, universe_id=universe_id))

    first = backfill_primary_universe_names(tmp_path)
    second = backfill_primary_universe_names(tmp_path)

    assert first["counts"]["corrected"] == 1
    assert second["counts"] == {"corrected": 0, "already_populated": 1, "unresolved": 0, "errors": 0}


def test_backfill_propagates_only_the_universe_name_to_the_master_snapshot(tmp_path):
    model = _model("model_master")
    ProductionRepository(tmp_path).add(model)
    universe_id = _universe(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, universe_id=universe_id))
    quality.upsert_master_snapshot({
        "model_id": model.model_id,
        "universe_name": None,
        "metric_that_must_not_change": 17.5,
        "status": "active",
    })
    before = quality.load_master_snapshot().copy()

    first = backfill_primary_universe_names(tmp_path)
    after = quality.load_master_snapshot()
    second = backfill_primary_universe_names(tmp_path)

    assert quality.load_lineage(model.model_id)["primary_universe_name_at_promotion"] == "Univers historique"
    assert after.loc[0, "universe_name"] == "Univers historique"
    assert after.drop(columns="universe_name").equals(before.drop(columns="universe_name"))
    assert first["master_snapshot"]["names_updated"] == [{
        "model_id": model.model_id, "old_value": None, "new_value": "Univers historique",
    }]
    assert second["master_snapshot"]["names_updated"] == []
    assert second["master_snapshot"]["models_already_coherent"] == [model.model_id]


def test_backfill_does_not_overwrite_a_different_master_snapshot_name(tmp_path):
    model = _model("model_discrepancy")
    ProductionRepository(tmp_path).add(model)
    universe_id = _universe(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    quality.write_lineage(model.model_id, _lineage(model.model_id, name="Univers historique", universe_id=universe_id))
    quality.upsert_master_snapshot({"model_id": model.model_id, "universe_name": "Nom différent"})

    report = backfill_primary_universe_names(tmp_path)

    assert quality.load_master_snapshot().loc[0, "universe_name"] == "Nom différent"
    assert report["master_snapshot"]["discrepancies"] == [{
        "model_id": model.model_id,
        "snapshot_value": "Nom différent",
        "lineage_value": "Univers historique",
    }]
