"""Current-version quality must not relabel or mix historical observations."""
from dataclasses import replace

import pandas as pd
import pytest

from rstock.application.production_domain import ProductionModelStatus
from rstock.application.production_quality import compute_model_quality, ProductionQualityMetricsService
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_quality_ui import load_models_master, models_grid
from rstock.application.production_repository import ProductionRepository
from test_production_quality import _canonical_observations, _model, _prediction
from rstock.application.production_quality_runtime import synchronize_production_quality


def _sync(tmp_path, predictions):
    return synchronize_production_quality(
        tmp_path, candidate_predictions=predictions, new_results=pd.DataFrame(),
        evaluations=pd.DataFrame(), as_of_session="2026-09-02",
    )


def _historical_store(tmp_path, old_version=1):
    production = ProductionRepository(tmp_path)
    production.add(replace(_model(), artifact_version=2,
                           status=ProductionModelStatus.WATCHING))
    quality = ProductionQualityRepository(tmp_path)
    historical = {"model_id": "model_A", "model_version": old_version,
                  "artifact_version": old_version, "status": "active",
                  "promotion_date": "2026-08-28"}
    quality.write_lineage("model_A", historical)
    observations = _canonical_observations(
        _prediction("old", model_version=1),
        _prediction("current", model_version=2),
    )
    observations.loc[observations["prediction_id"] == "old", "intraday_return"] = -0.05
    quality.upsert_observations("model_A", observations)
    # Reproduce an already-published, clean historical snapshot.
    snapshot, series = compute_model_quality(observations, None, historical, "2026-09-02")
    quality.publish_model_quality_batch(
        {"model_A": (snapshot, series)},
        expected_observation_generations=quality.load_manifest()["observation_generations"],
    )
    quality.mark_model_clean("model_A")
    return quality


@pytest.mark.parametrize("old_version", [1, None])
def test_clean_stale_snapshot_is_reconciled_without_new_events(tmp_path, old_version):
    quality = _historical_store(tmp_path, old_version)
    history = quality.lineage_path("model_A").read_bytes()
    observations = quality.observation_path("model_A").read_bytes()

    summary = _sync(tmp_path, pd.DataFrame())

    assert summary["models_processed"] == ["model_A"]
    snapshot = quality.load_model_snapshot("model_A")
    assert snapshot["identity"]["model_version"] == 2
    assert snapshot["identity"]["status"] == "watching"
    assert snapshot["window_20"]["signal_count"] == 1
    assert snapshot["window_20"]["pnl"] == pytest.approx(200)
    assert quality.lineage_path("model_A").read_bytes() == history
    assert quality.observation_path("model_A").read_bytes() == observations
    grid = models_grid(load_models_master(tmp_path), window=20)
    assert grid.iloc[0]["Signaux"] == "1"
    assert grid.iloc[0]["Rendement moyen"] == "2,00 %"
    pointer = quality.current_generation_path.read_bytes()
    assert _sync(tmp_path, pd.DataFrame())["models_rebuilt"] == 0
    assert quality.current_generation_path.read_bytes() == pointer


def test_version_reconciliation_resumes_after_interrupted_publication(tmp_path, monkeypatch):
    quality = _historical_store(tmp_path)
    pointer = quality.current_generation_path.read_bytes()
    original = ProductionQualityRepository.publish_model_quality_batch
    def interrupted(*args, **kwargs):
        raise OSError("interrupted before publication")
    monkeypatch.setattr(ProductionQualityRepository, "publish_model_quality_batch", interrupted)
    with pytest.raises(OSError, match="interrupted"):
        _sync(tmp_path, pd.DataFrame())
    assert quality.current_generation_path.read_bytes() == pointer
    monkeypatch.setattr(ProductionQualityRepository, "publish_model_quality_batch", original)
    assert _sync(tmp_path, pd.DataFrame())["models_processed"] == ["model_A"]
    assert quality.load_model_snapshot("model_A")["identity"]["model_version"] == 2


def test_explicit_rebuild_uses_registry_version(tmp_path):
    quality = _historical_store(tmp_path)
    snapshot = ProductionQualityMetricsService(quality).rebuild_model("model_A", "2026-09-02")
    assert snapshot["identity"]["model_version"] == 2
    assert snapshot["window_20"]["signal_count"] == 1


def test_legacy_unspecified_version_keeps_aggregate_and_explicit_version_scopes_baseline():
    observations = _canonical_observations(
        _prediction("old", model_version=1), _prediction("new", model_version=2),
        _prediction("unknown", model_version=None),
    )
    legacy, _ = compute_model_quality(observations, None, {"model_id": "model_A"}, "2026-09-02")
    assert legacy["window_20"]["signal_count"] == 3
    baseline = {"availability_status": "available", "model_version": 1, "metrics": {}}
    current, series = compute_model_quality(
        observations, baseline, {"model_id": "model_A", "model_version": 2}, "2026-09-02",
    )
    assert current["window_20"]["signal_count"] == 1
    assert current["baseline_comparison"]["baseline_status"] == "unavailable"
    assert series["signal_count"].sum() == 1
    assert baseline["availability_status"] == "available"


def test_current_version_without_signals_has_zero_count_and_scoped_detail(tmp_path):
    from rstock.application.production_quality_ui import load_model_quality_detail
    quality = _historical_store(tmp_path)
    observations = quality.load_observations("model_A")
    current = observations["model_version"].eq(2)
    observations.loc[current, "is_bullish_signal"] = False
    observations.loc[current, "is_winner"] = pd.NA
    observations.loc[current, "signal_category"] = "no_signal"
    quality.upsert_observations("model_A", observations)
    quality.write_baseline("model_A", {"model_id": "model_A", "model_version": 1,
                                       "availability_status": "available"})
    _sync(tmp_path, pd.DataFrame())
    grid = models_grid(load_models_master(tmp_path), window=20)
    assert grid.iloc[0]["Signaux"] == "0"
    assert grid.iloc[0]["Rendement moyen"] == "—"
    detail = load_model_quality_detail(tmp_path, "model_A", model_version=2)
    assert detail.observations["model_version"].tolist() == [2]
    assert detail.lineage["model_version"] == 2
    assert detail.baseline["availability_status"] == "unavailable_version_mismatch"
    assert len(quality.load_observations("model_A")) == 2


@pytest.mark.parametrize("change_version", [False, True])
def test_interrupted_rebuild_resumes_only_for_same_registry_version(tmp_path, monkeypatch, change_version):
    from rstock.application.domain import ExperimentSpec, JobType
    from rstock.application.repository import RunRepository
    from rstock.application.production_quality_rebuild import ProductionQualityRebuildRunner
    from rstock.config import DEFAULT_CONFIG
    quality = _historical_store(tmp_path)
    pointer = quality.current_generation_path.read_bytes()
    runs = RunRepository(tmp_path / "runs")
    run_id = runs.create(ExperimentSpec(
        JobType.PRODUCTION_QUALITY_REBUILD,
        replace(DEFAULT_CONFIG, project_root=tmp_path), symbols=("AAPL", "MSFT"),
    ))
    original = ProductionQualityRepository.publish_generation
    def interrupt(*args, **kwargs):
        raise OSError("interrupted before pointer")
    monkeypatch.setattr(ProductionQualityRepository, "publish_generation", interrupt)
    with pytest.raises(OSError, match="interrupted"):
        ProductionQualityRebuildRunner(runs, run_id, tmp_path).execute("2026-09-02")
    assert quality.current_generation_path.read_bytes() == pointer
    monkeypatch.setattr(ProductionQualityRepository, "publish_generation", original)
    if change_version:
        production = ProductionRepository(tmp_path)
        def retrain(model):
            model.artifact_version = 3
            return model
        production.mutate_model("model_A", retrain)
        with pytest.raises(ValueError, match="incompatible"):
            ProductionQualityRebuildRunner(runs, run_id, tmp_path).execute("2026-09-02")
        assert quality.current_generation_path.read_bytes() == pointer
    else:
        ProductionQualityRebuildRunner(runs, run_id, tmp_path).execute("2026-09-02")
        assert quality.load_model_snapshot("model_A")["identity"]["model_version"] == 2
