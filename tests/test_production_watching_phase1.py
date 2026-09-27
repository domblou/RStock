"""Registry transitions and immutable event provenance for observation models."""

import json
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_quality import (
    build_quality_observations,
    normalize_quality_observations,
    resolve_prediction_model_status,
)
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import (
    DailyPredictionService,
    ProductionLifecycleService,
    ProductionSignalService,
    ProductionTrainingService,
)
from rstock.config import DEFAULT_CONFIG


def _model(*, status=ProductionModelStatus.CANDIDATE, version=None):
    return ProductionModel(
        model_id="watched_model", target="AAA", predictors=("BBB",), lag_depth=1,
        target_definition="Up", up_target_threshold=0.01, down_target_threshold=0.01,
        xgboost_parameters={"max_depth": 1, "eta": 0.1, "num_boost_round": 1},
        up_threshold=0.6, down_threshold=0.4, qualification_rules={},
        source_walk_forward_run="wf", source_xgboost_calibration_run=None,
        source_threshold_calibration_run=None, development_metrics={}, holdout_metrics={},
        created_at="2026-09-01T12:00:00+00:00", status=status, artifact_version=version,
    )


def _artifacts(repository, model):
    directory = repository.artifact_directory(model.model_id)
    directory.mkdir(parents=True)
    for name in ("up.ubj", "down.ubj"):
        (directory / name).write_bytes(b"booster")
    (directory / "production.metadata.json").write_text(json.dumps({
        "model_id": model.model_id, "artifact_version": model.artifact_version,
        "feature_version": model.feature_version,
        "predictor_columns": ["BBB_intraday_J-1"],
    }), encoding="utf-8")


def test_historical_registry_and_watching_round_trip():
    legacy = _model().to_dict()
    for name in ("watching_started_at", "activated_at", "deactivated_at", "status_history"):
        legacy.pop(name)
    restored = ProductionModel.from_dict(legacy)
    assert restored.status == ProductionModelStatus.CANDIDATE
    assert restored.watching_started_at is None
    assert restored.activated_at is None
    assert restored.deactivated_at is None
    assert restored.status_history == []
    watched = ProductionModel.from_dict(replace(restored, status=ProductionModelStatus.WATCHING).to_dict())
    assert watched.status == ProductionModelStatus.WATCHING
    assert watched.status.display_label == "En observation"


def test_watching_lifecycle_preserves_identity_version_and_dates(tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model(status=ProductionModelStatus.TRAINED, version=2))
    lifecycle = ProductionLifecycleService(repository)
    with pytest.raises(ValueError, match="artifacts"):
        lifecycle.watch(model.model_id)
    assert repository.get(model.model_id).status == ProductionModelStatus.TRAINED
    _artifacts(repository, model)
    watched = lifecycle.watch(model.model_id)
    assert watched.model_id == model.model_id
    assert watched.artifact_version == 2
    assert watched.created_at == model.created_at
    assert watched.watching_started_at is not None
    assert watched.activated_at is None
    assert repository.active_models() == []
    with pytest.raises(ValueError, match="trained"):
        lifecycle.watch(model.model_id)
    with pytest.raises(ValueError, match="Stop"):
        ProductionTrainingService(repository).train(model.model_id, pd.DataFrame(), DEFAULT_CONFIG)
    with pytest.raises(ValueError, match="Deactivate"):
        lifecycle.retire(model.model_id)

    active = lifecycle.activate(model.model_id)
    assert active.model_id == model.model_id
    assert active.artifact_version == 2
    assert active.watching_started_at == watched.watching_started_at
    assert active.activated_at is not None
    assert [event["to"] for event in active.status_history] == ["watching", "active"]
    with pytest.raises(ValueError, match="trained"):
        lifecycle.watch(model.model_id)
    inactive = lifecycle.deactivate(model.model_id)
    assert inactive.deactivated_at is not None
    assert [event["to"] for event in inactive.status_history] == ["watching", "active", "inactive"]
    assert lifecycle.activate(model.model_id).activated_at == active.activated_at


def test_watching_can_stop_without_activation_and_direct_activation_still_works(tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model(status=ProductionModelStatus.TRAINED, version=1))
    _artifacts(repository, model)
    lifecycle = ProductionLifecycleService(repository)
    assert lifecycle.activate(model.model_id).status == ProductionModelStatus.ACTIVE
    lifecycle.deactivate(model.model_id)
    assert lifecycle.activate(model.model_id).status == ProductionModelStatus.ACTIVE

    other = replace(model, model_id="other")
    repository.add(other)
    _artifacts(repository, other)
    lifecycle.watch(other.model_id)
    stopped = lifecycle.deactivate(other.model_id)
    assert stopped.status == ProductionModelStatus.INACTIVE
    assert stopped.activated_at is None
    assert stopped.watching_started_at is not None


def test_prediction_signal_and_quality_keep_watching_context_after_activation(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model(status=ProductionModelStatus.TRAINED, version=1))
    _artifacts(repository, model)
    lifecycle = ProductionLifecycleService(repository)
    watched = lifecycle.watch(model.model_id)
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    dates = pd.bdate_range("2026-09-21", periods=4)
    prepared = pd.DataFrame({
        "AAA.intraday_return": [0.01, 0.02, -0.01, 0.03],
        "BBB.intraday_return": [0.02, 0.01, 0.0, 0.02],
    }, index=dates)
    service = DailyPredictionService(repository)
    watching_prediction = service._predict_model(watched, prepared)
    assert watching_prediction["model_status_at_prediction"] == "watching"
    repository.append_table("predictions", pd.DataFrame([watching_prediction]), key="prediction_id")
    signal = ProductionSignalService(repository).screen(
        pd.DataFrame([watching_prediction]), persist=False, restrict_to_active_models=False
    )
    assert signal.iloc[0]["model_status_at_prediction"] == "watching"
    repository.append_table("signals", signal, key="signal_id")

    active = lifecycle.activate(model.model_id)
    active_prediction = service._predict_model(active, prepared)
    assert active_prediction["model_status_at_prediction"] == "active"
    assert active_prediction["prediction_id"] == watching_prediction["prediction_id"]
    repository.append_table("predictions", pd.DataFrame([active_prediction]), key="prediction_id")
    repository.append_table("signals", signal.assign(model_status_at_prediction="active"), key="signal_id")
    stored = repository.read_table("predictions").iloc[0]
    assert stored["model_status_at_prediction"] == "watching"
    assert repository.read_table("signals").iloc[0]["model_status_at_prediction"] == "watching"
    observations = build_quality_observations(
        repository.read_table("predictions"), repository.read_table("signals"),
        pd.DataFrame(), pd.DataFrame(),
    )
    assert observations.iloc[0]["model_status_at_prediction"] == "watching"


def test_legacy_event_context_is_unknown_and_cannot_be_relabelled(tmp_path):
    repository = ProductionRepository(tmp_path)
    legacy = {"prediction_id": "old", "model_id": "old-model", "status": "predicted"}
    assert resolve_prediction_model_status(legacy) == "legacy_unknown"
    repository.append_table("predictions", pd.DataFrame([legacy]), key="prediction_id")
    repository.append_table("predictions", pd.DataFrame([
        {**legacy, "model_status_at_prediction": "active"}
    ]), key="prediction_id")
    assert repository.read_table("predictions").iloc[0]["model_status_at_prediction"] == "legacy_unknown"
    normalized = normalize_quality_observations(pd.DataFrame([{
        "observation_schema_version": 1, "model_id": "old-model", "prediction_id": "old",
    }]))
    assert normalized.iloc[0]["model_status_at_prediction"] == "legacy_unknown"


def test_failed_registry_publication_keeps_previous_status_and_retry_recovers(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model(status=ProductionModelStatus.TRAINED, version=1))
    _artifacts(repository, model)
    lifecycle = ProductionLifecycleService(repository)
    original_write = repository._write_models

    def fail_once(_models):
        raise OSError("interrupted registry publication")

    monkeypatch.setattr(repository, "_write_models", fail_once)
    with pytest.raises(OSError, match="interrupted"):
        lifecycle.watch(model.model_id)
    assert repository.get(model.model_id).status == ProductionModelStatus.TRAINED
    assert repository.get(model.model_id).status_history == []
    monkeypatch.setattr(repository, "_write_models", original_write)
    assert lifecycle.watch(model.model_id).status == ProductionModelStatus.WATCHING
    assert len(repository.get(model.model_id).status_history) == 1
