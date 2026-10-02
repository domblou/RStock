"""Tracked daily operation without promoting observation events to production."""

import json
import sys
from dataclasses import replace
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_quality_ui import global_quality_kpis, load_models_master
from rstock.application.production_quality_ui import load_model_quality_detail, model_phase_metrics
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import (
    DailyPredictionService,
    OperationalUniverseService,
    ProductionLifecycleService,
    ProductionSignalService,
)
from rstock.application.simulation import SimulationService
from rstock.application.surveillance import surveillance_model_history_table
from rstock.application.workflows import _daily_screening, _operational_run
import rstock.application.workflows as workflows_module
from rstock.config import DEFAULT_CONFIG


def _model(model_id, target, status, *, watching_started_at=None):
    return ProductionModel(
        model_id=model_id, target=target, predictors=("BBB",), lag_depth=1,
        target_definition="Up", up_target_threshold=0.01, down_target_threshold=0.01,
        xgboost_parameters={"max_depth": 1, "eta": 0.1, "num_boost_round": 1},
        up_threshold=0.6, down_threshold=0.4, qualification_rules={},
        source_walk_forward_run="wf", source_xgboost_calibration_run=None,
        source_threshold_calibration_run=None, development_metrics={}, holdout_metrics={},
        created_at="2026-09-01T12:00:00+00:00", status=status, artifact_version=1,
        watching_started_at=watching_started_at,
    )


def _artifacts(repository, model):
    directory = repository.artifact_directory(model.model_id)
    directory.mkdir(parents=True)
    for name in ("up.ubj", "down.ubj"):
        (directory / name).write_bytes(b"booster")
    (directory / "production.metadata.json").write_text(json.dumps({
        "model_id": model.model_id, "artifact_version": 1,
        "feature_version": model.feature_version,
        "predictor_columns": ["BBB_intraday_J-1"],
    }), encoding="utf-8")


def _prepared(dates):
    return pd.DataFrame({
        "AAA.intraday_return": [0.02] * len(dates),
        "CCC.intraday_return": [0.01] * len(dates),
        "BBB.intraday_return": [0.01] * len(dates),
    }, index=dates)


def test_tracked_universe_and_prediction_services_include_both_statuses(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    watching = _model("watch", "AAA", ProductionModelStatus.WATCHING,
                      watching_started_at="2026-09-20T12:00:00+00:00")
    active = _model("active", "CCC", ProductionModelStatus.ACTIVE)
    for model in (watching, active):
        repository.add(model)
        _artifacts(repository, model)
    assert {model.model_id for model in repository.tracked_models()} == {"watch", "active"}
    assert {model.model_id for model in repository.active_models()} == {"active"}
    assert OperationalUniverseService(repository).tracked().symbols == ("AAA", "BBB", "CCC")
    assert OperationalUniverseService(repository).current().symbols == ("BBB", "CCC")

    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    prepared = _prepared(pd.bdate_range("2026-09-21", periods=5))
    predictions = DailyPredictionService(repository).generate(
        prepared, replace(DEFAULT_CONFIG, project_root=tmp_path), persist=True
    )
    assert set(predictions["model_status_at_prediction"]) == {"watching", "active"}
    assert len(predictions) == 2
    tracked_signals = ProductionSignalService(repository).screen(
        predictions, persist=True, restrict_to_active_models=False
    )
    assert set(tracked_signals["model_status_at_prediction"]) == {"watching", "active"}
    assert ProductionSignalService(repository).screen(predictions, persist=False)[
        "model_id"
    ].tolist() == ["active"]
    assert repository.read_active_model_table("signals")["model_id"].tolist() == ["active"]
    screening_output = tmp_path / "screening_output"
    screening_output.mkdir()
    screening = _daily_screening(
        ExperimentSpec(
            JobType.DAILY_SCREENING,
            replace(DEFAULT_CONFIG, project_root=tmp_path),
            symbols=("AAA", "BBB", "CCC"),
        ),
        screening_output, None, None,
    )
    assert screening["categories"] == {"bullish_signal": 1}
    assert screening["watching_categories"] == {"bullish_signal": 1}

    from scripts import laboratory_cli

    cli_output = tmp_path / "operational.json"
    monkeypatch.setattr(sys, "argv", [
        "laboratory_cli.py", "--project-root", str(tmp_path),
        "create-config", "--job-type", "operational_run", "--output", str(cli_output),
    ])
    laboratory_cli.main()
    assert ExperimentSpec.from_dict(json.loads(cli_output.read_text(encoding="utf-8"))).symbols == (
        "AAA", "BBB", "CCC"
    )


def test_watching_only_run_evaluates_and_resume_keeps_scope(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    watching = _model("watch", "AAA", ProductionModelStatus.WATCHING,
                      watching_started_at="2026-09-20T12:00:00+00:00")
    repository.add(watching)
    _artifacts(repository, watching)
    first_dates = pd.bdate_range("2026-09-21", periods=5)
    dates = first_dates
    price_dates = pd.bdate_range("2026-09-21", periods=8)
    target_prices = pd.DataFrame({
        "Open": [100.0] * len(price_dates),
        "High": [103.0] * len(price_dates),
        "Low": [99.0] * len(price_dates),
        "Close": [102.0] * len(price_dates),
    }, index=price_dates)

    def prepared_download(*_args, **_kwargs):
        prices = pd.DataFrame(index=dates)
        for symbol in ("AAA", "BBB"):
            for column in ("Open", "High", "Low", "Close"):
                prices[f"{symbol}.{column}"] = target_prices.loc[dates, column].to_numpy()
        return _prepared(dates), SimpleNamespace(symbols=("AAA", "BBB"), prices=prices)

    monkeypatch.setattr("rstock.application.workflows._operational_prepared", prepared_download)
    monkeypatch.setattr("rstock.application.workflows._refresh_simulation_benchmark", lambda *_: None)
    monkeypatch.setattr(
        "rstock.application.workflows.market_data_service",
        lambda _config: SimpleNamespace(store=SimpleNamespace(read=lambda _symbol: target_prices)),
    )
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    spec = ExperimentSpec(
        JobType.OPERATIONAL_RUN, replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
    )
    output = tmp_path / "output"
    output.mkdir()
    real_synchronize = workflows_module.synchronize_production_quality

    def interrupted_quality(*_args, **_kwargs):
        raise RuntimeError("interrupted after operational history publication")

    monkeypatch.setattr(workflows_module, "synchronize_production_quality", interrupted_quality)
    with pytest.raises(RuntimeError, match="interrupted after"):
        _operational_run(spec, output, None, None)
    interrupted_predictions = repository.read_table("predictions")
    assert not interrupted_predictions.empty
    assert ProductionQualityRepository(tmp_path).load_observations("watch").empty
    monkeypatch.setattr(workflows_module, "synchronize_production_quality", real_synchronize)
    first = _operational_run(spec, output, None, None)
    assert len(repository.read_table("predictions")) == len(interrupted_predictions)
    assert first["signals"] == 0
    assert first["watching_signals"] >= 1
    assert repository.read_table("realized_results").shape[0] >= 1
    observations = ProductionQualityRepository(tmp_path).load_observations("watch")
    assert set(observations["model_status_at_prediction"]) == {"watching"}
    assert observations["mfe"].notna().any()
    assert observations["mae"].notna().any()
    assert set(repository.read_watching_model_table("predictions")[
        "model_status_at_prediction"
    ]) == {"watching"}

    # The same persisted prediction is reused after activation; the following
    # market session receives a new ACTIVE event with the same model version.
    ProductionLifecycleService(repository).activate("watch")
    dates = pd.bdate_range("2026-09-21", periods=6)
    second = _operational_run(spec, output, None, None)
    assert second["signals"] >= 1
    persisted = repository.read_table("predictions")
    scheduled = persisted[persisted["prediction_origin"].eq("scheduled_live")]
    assert scheduled.set_index("prediction_date").loc[
        "2026-09-28", "model_status_at_prediction"
    ] == "watching"
    assert scheduled.set_index("prediction_date").loc[
        "2026-09-29", "model_status_at_prediction"
    ] == "active"
    assert repository.read_active_model_table("predictions")[
        "model_status_at_prediction"
    ].eq("active").all()
    assert repository.read_active_model_table("signals")[
        "model_status_at_prediction"
    ].eq("active").all()
    assert repository.read_watching_model_table("predictions").empty
    assert repository.read_active_model_table("realized_results")[
        "prediction_id"
    ].isin(repository.read_active_model_table("predictions")["prediction_id"]).all()
    snapshot = ProductionQualityRepository(tmp_path).load_model_snapshot("watch")
    assert snapshot["window_63"]["signal_count"] >= 1
    assert snapshot["watching_since_start"]["signal_count"] >= 1
    assert snapshot["production_window_63"]["signal_count"] == 0
    assert global_quality_kpis(load_models_master(tmp_path))["signals"] == 0

    # A rerun sees existing identities, preserves their provenance and repairs
    # quality after an interrupted publication without duplicating trades.
    before = len(persisted)
    _operational_run(spec, output, None, None)
    assert len(repository.read_table("predictions")) == before
    assert repository.read_table("predictions").set_index("prediction_date").loc[
        "2026-09-28", "model_status_at_prediction"
    ] == "watching"
    result = SimulationService(repository, lambda _symbol: target_prices).run(
        "2026-09-28", "2026-09-29"
    )
    assert result.metrics.signals_found == 1
    assert result.trades["Date trade"].tolist() == ["2026-09-29"]

    # Stopping observation/production removes the model from the daily
    # population without deleting its frozen observation history.
    ProductionLifecycleService(repository).deactivate("watch")
    retained = repository.read_table("predictions")
    assert set(retained["model_status_at_prediction"]) == {"watching", "active"}
    assert repository.read_active_model_table("predictions").empty
    assert repository.read_watching_model_table("predictions").empty
    assert DailyPredictionService(repository).generate(
        _prepared(pd.bdate_range("2026-09-21", periods=7)),
        replace(DEFAULT_CONFIG, project_root=tmp_path),
    ).empty
    assert repository.read_table("predictions").equals(retained)
    assert set(ProductionQualityRepository(tmp_path).load_observations("watch")[
        "model_status_at_prediction"
    ]) == {"watching", "active"}


def test_global_production_kpis_exclude_watching_and_pre_activation_history():
    frame = pd.DataFrame([
        {
            "status": "active", "health_label": "Données insuffisantes",
            "signal_count_63": 8, "mean_return_63": 0.04, "win_rate_63": 0.75,
            "pnl_since_promotion": 800.0,
            "production_signal_count_63": 2, "production_mean_return_63": 0.01,
            "production_win_rate_63": 0.5, "production_pnl_since_activation": 100.0,
        },
        {
            "status": "watching", "health_label": "Données insuffisantes",
            "signal_count_63": 10, "mean_return_63": 0.09, "win_rate_63": 0.9,
            "pnl_since_promotion": 1000.0,
            "production_signal_count_63": 0, "production_mean_return_63": None,
            "production_win_rate_63": None, "production_pnl_since_activation": 0.0,
        },
    ])
    assert global_quality_kpis(frame) == {
        "active_models": 1, "data_insufficient": 1, "mean_return": 0.01,
        "pnl": 100.0, "win_rate": 0.5, "signals": 2,
    }
    assert global_quality_kpis(frame[frame["status"].eq("watching")])["signals"] == 0


def test_lifecycle_change_during_prediction_rejects_atomic_publication(tmp_path):
    repository = ProductionRepository(tmp_path)
    watching = _model("watch", "AAA", ProductionModelStatus.WATCHING,
                      watching_started_at="2026-09-20T12:00:00+00:00")
    repository.add(watching)
    _artifacts(repository, watching)
    selected = tuple(repository.tracked_models())
    ProductionLifecycleService(repository).activate("watch")
    with pytest.raises(ValueError, match="lifecycle changed"):
        repository.append_table(
            "predictions",
            pd.DataFrame([{
                "prediction_id": "pending", "model_id": "watch",
                "model_status_at_prediction": "watching",
            }]),
            key="prediction_id", expected_models=selected,
        )
    assert repository.read_table("predictions").empty


def test_active_to_watching_preserves_identity_artifacts_history_and_quality(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model("cycle", "AAA", ProductionModelStatus.ACTIVE))
    _artifacts(repository, model)
    artifact_bytes = {
        path.name: path.read_bytes()
        for path in repository.artifact_directory(model.model_id).iterdir()
    }
    repository.append_table("predictions", pd.DataFrame([{
        "prediction_id": "old", "prediction_date": "2026-09-22", "model_id": "cycle",
        "target": "AAA", "model_status_at_prediction": "active",
    }]), key="prediction_id")
    repository.append_table("signals", pd.DataFrame([{
        "signal_id": "old", "prediction_id": "old", "model_id": "cycle",
        "category": "bullish_signal", "model_status_at_prediction": "active",
    }]), key="signal_id")
    repository.append_table("realized_results", pd.DataFrame([{
        "result_id": "old", "prediction_id": "old", "model_id": "cycle",
        "intraday_return": 0.02,
    }]), key="result_id")
    quality = ProductionQualityRepository(tmp_path)
    quality.write_model_snapshot("cycle", {
        "model_id": "cycle", "identity": {"model_version": 1},
    })
    quality.write_model_series("cycle", pd.DataFrame({"session_date": ["2026-09-22"]}))
    quality.upsert_observations("cycle", pd.DataFrame([{
        "observation_schema_version": 1,
        "prediction_id": "old", "model_id": "cycle", "model_version": 1,
        "session_date": "2026-09-22", "prediction_origin": "scheduled_live",
        "model_status_at_prediction": "active", "evaluation_status": "evaluated",
        "is_bullish_signal": True, "intraday_return": 0.02,
    }]))
    monkeypatch.setattr(
        "rstock.application.production_services.utc_now",
        lambda: "2026-09-23T12:00:00+00:00",
    )
    monkeypatch.setattr(
        "rstock.application.production_services.ProductionTrainingService.train",
        lambda *args, **kwargs: (_ for _ in ()).throw(AssertionError("unexpected training")),
    )

    watched = ProductionLifecycleService(repository).watch("cycle")

    assert watched.model_id == model.model_id
    assert watched.artifact_version == model.artifact_version
    assert watched.source_walk_forward_run == model.source_walk_forward_run
    assert watched.training_metadata == model.training_metadata
    assert watched.status_history == [{
        "from": "active", "to": "watching", "at": "2026-09-23T12:00:00+00:00",
        "artifact_version": 1,
    }]
    assert {path.name: path.read_bytes() for path in repository.artifact_directory("cycle").iterdir()} == artifact_bytes
    assert repository.read_table("predictions").iloc[0]["model_status_at_prediction"] == "active"
    assert repository.read_watching_model_table("predictions").empty
    history = surveillance_model_history_table(
        repository.read_tracked_model_table("predictions"),
        repository.read_tracked_model_table("signals"),
        repository.read_tracked_model_table("realized_results"),
    )
    assert history.iloc[0]["Période"] == "Production active"
    assert history.iloc[0]["P&L"] == "200,00 $"
    detail = load_model_quality_detail(tmp_path, "cycle", model_version=watched.artifact_version)
    assert detail.quality_state == "current"
    phases = model_phase_metrics(detail.observations, detail.series)
    assert phases["Production active"]["pnl"] == pytest.approx(200)
    assert phases["Historique complet"]["pnl"] == pytest.approx(200)
    assert global_quality_kpis(load_models_master(tmp_path))["active_models"] == 0
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    fresh = DailyPredictionService(repository).generate(
        _prepared(pd.bdate_range("2026-09-21", periods=4)),
        replace(DEFAULT_CONFIG, project_root=tmp_path),
    )
    assert fresh.iloc[0]["model_status_at_prediction"] == "watching"
    assert set(repository.read_table("predictions")["model_status_at_prediction"]) == {
        "active", "watching"
    }
    assert repository.read_watching_model_table("predictions")["prediction_id"].tolist() == [
        fresh.iloc[0]["prediction_id"]
    ]


def test_backfill_uses_each_status_interval_across_multiple_cycles(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    model = repository.add(_model("cycle", "AAA", ProductionModelStatus.ACTIVE))
    _artifacts(repository, model)
    clock = iter([
        "2026-09-23T12:00:00+00:00",  # ACTIVE -> WATCHING before the open
        "2026-09-25T12:00:00+00:00",  # WATCHING -> ACTIVE
        "2026-09-29T12:00:00+00:00",  # ACTIVE -> WATCHING
    ])
    monkeypatch.setattr("rstock.application.production_services.utc_now", lambda: next(clock))
    lifecycle = ProductionLifecycleService(repository)
    lifecycle.watch("cycle")
    lifecycle.activate("cycle")
    lifecycle.watch("cycle")
    assert repository.get("cycle").watching_started_at == "2026-09-23T12:00:00+00:00"
    monkeypatch.setattr(
        "rstock.application.production_services.utc_now",
        lambda: "2026-09-30T20:00:00+00:00",
    )
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    dates = pd.bdate_range("2026-09-21", "2026-09-30")
    backfilled = DailyPredictionService(repository).backfill(_prepared(dates), max_days=20)
    contexts = backfilled.set_index("prediction_date")["model_status_at_prediction"]
    assert contexts.loc["2026-09-22"] == "active"
    assert contexts.loc["2026-09-23"] == "watching"
    assert contexts.loc["2026-09-24"] == "watching"
    assert contexts.loc["2026-09-25"] == "active"
    assert contexts.loc["2026-09-28"] == "active"
    assert contexts.loc["2026-09-29"] == "watching"
    assert repository.read_table("predictions").set_index("prediction_date").loc[
        "2026-09-28", "model_status_at_prediction"
    ] == "active"
    before = repository.read_table("predictions")
    assert DailyPredictionService(repository).backfill(_prepared(dates), max_days=20).empty
    assert repository.read_table("predictions").equals(before)
