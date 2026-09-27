"""Source-backed repair of the historical quality-origin downgrade."""

from dataclasses import replace

import pandas as pd
import pytest

from rstock.application.production_quality import build_quality_observations, compute_model_quality
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_repository import ProductionRepository
from scripts.repair_production_quality_origins import repair

from test_production_quality import _model, _prediction


def test_source_backed_repair_restores_vlo_equivalent_without_changing_sources(tmp_path):
    production = ProductionRepository(tmp_path)
    quality = ProductionQualityRepository(tmp_path)
    production.add(replace(
        _model(), target="VLO", artifact_version=1,
        training_metadata={"train_end": "2026-09-18"},
    ))
    predictions = []
    for identifier, date, as_of, created in (
        ("vlo-22", "2026-09-22", "2026-09-21", "2026-09-21T20:45:16Z"),
        ("vlo-24", "2026-09-24", "2026-09-23", "2026-09-23T20:12:51Z"),
    ):
        item = _prediction(identifier, prediction_date=date)
        item.update(target="VLO", as_of_date=as_of, created_at=created)
        item.pop("prediction_origin")
        predictions.append(item)
    predictions = pd.DataFrame(predictions)
    signals = pd.DataFrame([
        {"signal_id": identifier, "prediction_id": identifier,
         "model_id": "model_A", "category": "bullish_signal"}
        for identifier in ("vlo-22", "vlo-24")
    ])
    results = pd.DataFrame([
        {"result_id": identifier, "prediction_id": identifier, "model_id": "model_A",
         "open": 100, "high": 103, "low": 96, "close": 100 * (1 + value),
         "intraday_return": value, "mfe": 0.03, "mae": -0.04,
         "recorded_at": "2026-09-25T21:00:00Z"}
        for identifier, value in (("vlo-22", -0.020161), ("vlo-24", 0.010025))
    ])
    production.append_tables({
        "predictions": (predictions, "prediction_id"),
        "signals": (signals, "signal_id"),
        "realized_results": (results, "result_id"),
    })
    source_bytes = (production.history_root / "predictions.csv").read_bytes()
    degraded = build_quality_observations(predictions, signals, results, pd.DataFrame())
    assert set(degraded["prediction_origin"]) == {"legacy_unknown"}
    quality.upsert_observations("model_A", degraded)
    quality.write_lineage("model_A", {
        "model_id": "model_A", "model_version": 1,
        "promotion_date": "2026-09-21", "target": "VLO",
    })
    snapshot, series = compute_model_quality(
        degraded, None, quality.load_lineage("model_A"), "2026-09-25"
    )
    initial = quality.publish_model_quality_batch(
        {"model_A": (snapshot, series)},
        expected_observation_generations=quality.load_manifest()["observation_generations"],
    )
    quality.mark_model_clean("model_A")
    initial_master = quality.load_master_snapshot().copy()
    preview = repair(tmp_path)
    assert preview["applied"] is False
    assert preview["repaired_observations"] == 2
    assert preview["models"]["model_A"]["before"]["signals_63"] == 0
    assert preview["models"]["model_A"]["after"]["signals_63"] == 2
    assert preview["models"]["model_A"]["after"]["pnl"] == pytest.approx(-101.36)
    assert quality._load_json(quality.current_generation_path)["generation"] == initial

    result = repair(tmp_path, apply=True)
    assert result["applied"] is True
    assert result["publication"]["models_remaining"] == 0
    assert (production.history_root / "predictions.csv").read_bytes() == source_bytes
    assert quality.generation_path(initial).joinpath("snapshots/models.parquet").exists()
    assert initial_master.iloc[0]["signal_count_63"] == 0
    assert quality.load_master_snapshot().iloc[0]["signal_count_63"] == 2
    assert quality.load_model_snapshot("model_A")["since_promotion"]["pnl"] == pytest.approx(-101.36)
    assert set(quality.load_observations("model_A")["prediction_origin"]) == {
        "legacy_inferred_live"
    }
