import json
import os
import sys
from dataclasses import replace
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_repository import ProductionRepository
import rstock.application.production_repository as production_repository_module
from rstock.application.production_services import (
    DailyPredictionService,
    OperationalUniverseService,
    ProductionLifecycleService,
    ProductionSignalService,
    ProductionTrainingService,
    PromotionService,
    RealizedResultService,
)
from rstock.application.repository import RunRepository, utc_now
from rstock.application.surveillance import build_evaluated_predictions_view
from rstock.application.worker import execute_run
from rstock.application.workflows import WorkflowRegistry, _market_update, _operational_run
from rstock.config import DEFAULT_CONFIG
from rstock.progress import CancellationRequested


def _model(model_id="model_test", *, target="AAA", predictors=("BBB",)):
    return ProductionModel(
        model_id=model_id, target=target, predictors=predictors, lag_depth=1,
        target_definition="Up >= 1%; Down <= -1%", up_target_threshold=0.01,
        down_target_threshold=0.01,
        xgboost_parameters={
            "max_depth": 1, "eta": 0.1, "num_boost_round": 1,
            "min_child_weight": 1.0, "subsample": 1.0, "colsample_bytree": 1.0,
            "gamma": 0.0, "reg_alpha": 0.0, "reg_lambda": 1.0,
        },
        up_threshold=0.6, down_threshold=0.4,
        qualification_rules={"min_windows": 3}, source_walk_forward_run="wf_run",
        source_xgboost_calibration_run="xgb_run",
        source_threshold_calibration_run="threshold_run",
        development_metrics={"ROCAUCMedian": 0.61},
        holdout_metrics={"FinalUpROCAUC": 0.58}, created_at=utc_now(),
        training_metadata={"calendar": "XNYS"},
    )


def _promotion_run(tmp_path):
    runs = RunRepository(tmp_path / "runs")
    spec = ExperimentSpec(
        JobType.WALK_FORWARD, replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        primary_universe_id="PRIMARY",
        context_universe_ids=("CONTEXT",),
        target_symbols=("AAA",),
        context_symbols=("BBB",),
        predictor_symbols=("AAA", "BBB"),
    )
    run_id = runs.create(spec)
    results = runs.run_directory(run_id) / "results"
    results.mkdir()
    pd.DataFrame([{
        "Set": "AAA<-BBB", "Observation": "AAA", "Predictors": '["BBB"]',
        "Eligible": True, "ROCAUCMedian": 0.6,
    }]).to_csv(results / "qualification.csv", index=False)
    pd.DataFrame([{"Set": "AAA<-BBB", "FinalUpROCAUC": 0.57}]).to_csv(
        results / "final_holdout.csv", index=False
    )
    pd.DataFrame([{
        "Set": "AAA<-BBB", "model_selection_score": 74.5,
        "model_selection_rank": 1, "stability_score": 82.0,
    }]).to_csv(results / "selection_results.csv", index=False)
    runs.transition(run_id, JobStatus.RUNNING)
    runs.transition(run_id, JobStatus.COMPLETED)
    return runs, run_id


def test_promotion_is_idempotent_and_preserves_run_traceability(tmp_path):
    runs, run_id = _promotion_run(tmp_path)
    calibration_spec = ExperimentSpec(
        JobType.XGBOOST_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
    )
    xgb_run = runs.create(calibration_spec)
    xgb_results = runs.run_directory(xgb_run) / "results"
    xgb_results.mkdir()
    up_parameters = dict(_model().xgboost_parameters)
    down_parameters = {**up_parameters, "max_depth": 2}
    (xgb_results / "selected_configurations.json").write_text(
        json.dumps({
            "Up": {"parameters": up_parameters},
            "Down": {"parameters": down_parameters},
        }),
        encoding="utf-8",
    )
    runs.transition(xgb_run, JobStatus.RUNNING)
    runs.transition(xgb_run, JobStatus.COMPLETED)
    threshold_run = runs.create(replace(calibration_spec, job_type=JobType.THRESHOLD_CALIBRATION))
    threshold_results = runs.run_directory(threshold_run) / "results"
    threshold_results.mkdir()
    (threshold_results / "selected_thresholds.json").write_text(
        json.dumps({"Up": {"threshold": 0.63}, "Down": {"threshold": 0.37}}),
        encoding="utf-8",
    )

    (threshold_results / "selected_thresholds_by_set.json").write_text(
        json.dumps({
            "AAA<-BBB": {
                "Up": {
                    "status": "selected", "threshold": 0.63,
                    "calibration_sample_size": 42,
                    "calibration_metrics": {
                        "total_signals": 18, "success_rate": 0.67,
                        "mean_return": 0.012, "median_return": 0.01,
                        "mfe_mean": 0.02, "mae_mean": -0.01,
                        "return_stability": 0.03,
                    },
                },
                "Down": {"status": "selected", "threshold": 0.37},
            }
        }),
        encoding="utf-8",
    )
    pd.DataFrame([{
        "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up",
        "Threshold": 0.63, "IntradayReturnMean": 0.009,
    }]).to_csv(threshold_results / "holdout_metrics.csv", index=False)
    runs.transition(threshold_run, JobStatus.RUNNING)
    runs.transition(threshold_run, JobStatus.COMPLETED)
    repository = ProductionRepository(tmp_path)
    service = PromotionService(runs, repository)

    first, created = service.promote(
        run_id,
        "AAA<-BBB",
        xgboost_calibration_run=xgb_run,
        threshold_calibration_run=threshold_run,
    )
    repeated, created_again = service.promote(
        run_id,
        "AAA<-BBB",
        xgboost_calibration_run=xgb_run,
        threshold_calibration_run=threshold_run,
    )

    assert created is True
    assert created_again is False
    assert repeated.model_id == first.model_id
    assert first.source_walk_forward_run == run_id
    assert first.source_xgboost_calibration_run == xgb_run
    assert first.source_threshold_calibration_run == threshold_run
    assert first.down_xgboost_parameters["max_depth"] == 2
    assert first.up_threshold == 0.63
    assert first.down_threshold == 0.37
    assert first.calibrated_signal_threshold == 0.63
    assert first.signal_threshold == 0.63
    assert first.calibration_source_run == threshold_run
    assert first.calibration_sample_size == 42
    assert first.calibration_metrics["total_signals"] == 18
    assert first.holdout_signal_metrics["IntradayReturnMean"] == 0.009
    assert first.source_configuration["job_type"] == "walk_forward"
    assert first.training_metadata["universe_roles"] == {
        "primary_universe_id": "PRIMARY",
        "context_universe_ids": ["CONTEXT"],
        "target_symbols": ["AAA"],
        "context_symbols": ["BBB"],
        "predictor_symbols": ["AAA", "BBB"],
    }
    assert first.development_metrics["ROCAUCMedian"] == 0.6
    assert first.development_metrics["model_selection_score"] == 74.5
    assert first.development_metrics["model_selection_rank"] == 1
    assert first.development_metrics["stability_score"] == 82.0
    assert first.holdout_metrics["FinalUpROCAUC"] == 0.57


def test_promotion_inherits_frozen_xgboost_provenance_from_derived_run(tmp_path):
    runs, walk_forward_run = _promotion_run(tmp_path)
    calibration_spec = ExperimentSpec(
        JobType.XGBOOST_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
    )
    xgb_run = runs.create(calibration_spec)
    runs.transition(xgb_run, JobStatus.RUNNING)
    runs.transition(xgb_run, JobStatus.COMPLETED)
    up_parameters = dict(_model().xgboost_parameters)
    down_parameters = {**up_parameters, "max_depth": 2}
    threshold_spec = replace(
        calibration_spec,
        job_type=JobType.THRESHOLD_CALIBRATION,
        source_xgboost_calibration_run=xgb_run,
        frozen_xgboost_parameters={"Up": up_parameters, "Down": down_parameters},
    )
    threshold_run = runs.create(threshold_spec)
    threshold_results = runs.run_directory(threshold_run) / "results"
    threshold_results.mkdir()
    (threshold_results / "selected_thresholds_by_set.json").write_text(
        json.dumps({
            "AAA<-BBB": {
                "Up": {"status": "selected", "threshold": 0.63},
                "Down": {"status": "selected", "threshold": 0.37},
            }
        }),
        encoding="utf-8",
    )
    runs.transition(threshold_run, JobStatus.RUNNING)
    runs.transition(threshold_run, JobStatus.COMPLETED)

    model, created = PromotionService(
        runs, ProductionRepository(tmp_path)
    ).promote(
        walk_forward_run,
        "AAA<-BBB",
        threshold_calibration_run=threshold_run,
    )

    assert created is True
    assert model.source_xgboost_calibration_run == xgb_run
    assert model.xgboost_parameters == up_parameters
    assert model.down_xgboost_parameters == down_parameters


def test_model_signal_threshold_falls_back_to_its_global_prediction_threshold():
    model = replace(
        _model(),
        source_configuration={"rstock_config": {"prediction_threshold": 0.71}},
    )

    assert model.calibrated_signal_threshold is None
    assert model.signal_threshold == 0.71


def test_threshold_run_promotion_uses_its_per_set_threshold_and_provenance(tmp_path):
    runs, walk_forward_run = _promotion_run(tmp_path)
    threshold_spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        source_walk_forward_run=walk_forward_run,
    )
    threshold_run = runs.create(threshold_spec)
    results = runs.run_directory(threshold_run) / "results"
    results.mkdir()
    (results / "selected_thresholds_by_set.json").write_text(
        json.dumps({
            "AAA<-BBB": {
                "Up": {"status": "selected", "threshold": 0.69},
                "Down": {"status": "selected", "threshold": 0.31},
            }
        }), encoding="utf-8",
    )
    pd.DataFrame([{
        "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Down",
        "Threshold": 0.31, "SignalCount": 7, "Precision": 0.71,
    }]).to_csv(results / "holdout_metrics.csv", index=False)
    runs.transition(threshold_run, JobStatus.RUNNING)
    runs.transition(threshold_run, JobStatus.COMPLETED)

    model, created = PromotionService(
        runs, ProductionRepository(tmp_path)
    ).promote(
        walk_forward_run, "AAA<-BBB", threshold_calibration_run=threshold_run,
        selected_threshold_direction="Down",
    )

    assert created is True
    assert model.up_threshold == 0.69
    assert model.down_threshold == 0.31
    assert model.source_walk_forward_run == walk_forward_run
    assert model.source_threshold_calibration_run == threshold_run
    assert model.training_metadata["selected_threshold_direction"] == "Down"


def test_legacy_threshold_run_resolves_only_a_matching_qualified_walk_forward(tmp_path):
    runs, walk_forward_run = _promotion_run(tmp_path)
    threshold = runs.create(ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        primary_universe_id="PRIMARY",
        context_universe_ids=("CONTEXT",),
        target_symbols=("AAA",),
        context_symbols=("BBB",),
        predictor_symbols=("AAA", "BBB"),
    ))
    runs.transition(threshold, JobStatus.RUNNING)
    runs.transition(threshold, JobStatus.COMPLETED)
    service = PromotionService(runs, ProductionRepository(tmp_path))

    assert service.resolve_walk_forward_source(threshold, "AAA<-BBB") == walk_forward_run
    assert service.resolve_walk_forward_source(threshold, "AAA<-CCC") is None


def test_training_publishes_two_new_full_history_artifacts(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    repository.add(_model())
    index = pd.bdate_range("2026-01-01", periods=6)
    prepared = pd.DataFrame({
        "BBB_intraday_J-1": np.arange(6, dtype=float),
        "AAA.intraday_target": [0, 1, 0, 1, 0, 1],
        "AAA.intraday_down_target": [1, 0, 1, 0, 1, 0],
    }, index=index)

    class FakeBooster:
        def save_model(self, path):
            path.write_bytes(b"booster")

    monkeypatch.setattr("rstock.application.production_services.fit_booster", lambda *a, **k: FakeBooster())
    trained = ProductionTrainingService(repository).train(
        "model_test", prepared, replace(DEFAULT_CONFIG, project_root=tmp_path)
    )

    directory = repository.artifact_directory("model_test")
    assert trained.status == ProductionModelStatus.TRAINED
    assert trained.training_metadata["observations"] == 6
    assert trained.training_metadata["train_start"] == index.min().isoformat()
    assert (directory / "up.ubj").read_bytes() == b"booster"
    assert (directory / "down.ubj").read_bytes() == b"booster"


def test_activation_requires_artifacts_and_recalculates_operational_universe(tmp_path):
    repository = ProductionRepository(tmp_path)
    first = repository.add(_model())
    lifecycle = ProductionLifecycleService(repository)
    with pytest.raises(ValueError, match="trained"):
        lifecycle.activate(first.model_id)

    for model in (
        replace(first, status=ProductionModelStatus.TRAINED, artifact_version=1),
        replace(_model("model_two", target="CCC", predictors=("BBB", "DDD")), status=ProductionModelStatus.TRAINED, artifact_version=1),
    ):
        if model.model_id == first.model_id:
            repository.update(model)
        else:
            repository.add(model)
        directory = repository.artifact_directory(model.model_id)
        directory.mkdir(parents=True)
        for name in ("up.ubj", "down.ubj"):
            (directory / name).write_text("booster", encoding="utf-8")
        predictor_names = [
            f"{predictor}_intraday_J-1" for predictor in model.predictors
        ]
        (directory / "production.metadata.json").write_text(
            json.dumps({
                "model_id": model.model_id, "artifact_version": 1,
                "feature_version": model.feature_version,
                "predictor_columns": predictor_names,
            }),
            encoding="utf-8",
        )
        lifecycle.activate(model.model_id)

    universe = OperationalUniverseService(repository).current()
    assert universe.symbols == ("AAA", "BBB", "CCC", "DDD")
    assert universe.used_by["BBB"] == ("model_test", "model_two")
    lifecycle.deactivate("model_two")
    assert OperationalUniverseService(repository).current().symbols == ("AAA", "BBB")
    lifecycle.activate("model_two")
    assert OperationalUniverseService(repository).current().symbols == (
        "AAA", "BBB", "CCC", "DDD"
    )
    lifecycle.deactivate("model_two")
    lifecycle.retire("model_two")
    assert OperationalUniverseService(repository).current().symbols == ("AAA", "BBB")


def test_active_model_source_excludes_inactive_and_retired_from_surveillance(tmp_path):
    repository = ProductionRepository(tmp_path)
    active = replace(
        _model("model_active"), status=ProductionModelStatus.ACTIVE, artifact_version=1
    )
    inactive = replace(
        _model("model_inactive", target="CCC"),
        status=ProductionModelStatus.INACTIVE,
        artifact_version=1,
    )
    retired = replace(
        _model("model_retired", target="DDD"),
        status=ProductionModelStatus.RETIRED,
        artifact_version=1,
    )
    for model in (active, inactive, retired):
        repository.add(model)
    predictions = pd.DataFrame([
        {
            "prediction_id": f"prediction-{model.model_id}",
            "model_id": model.model_id,
            "target": model.target,
            "status": "predicted",
            "up_probability": 0.8,
            "down_probability": 0.2,
            "up_threshold": 0.6,
            "down_threshold": 0.4,
        }
        for model in (active, inactive, retired)
    ])
    repository.append_table("predictions", predictions, key="prediction_id")

    signals = ProductionSignalService(repository).screen(predictions)

    assert [model.model_id for model in repository.active_models()] == ["model_active"]
    assert repository.read_active_model_table("predictions")["model_id"].tolist() == [
        "model_active"
    ]
    assert signals["model_id"].tolist() == ["model_active"]

    repository.update(replace(active, status=ProductionModelStatus.INACTIVE))
    assert repository.active_models() == []
    assert repository.read_active_model_table("predictions").empty
    assert ProductionSignalService(repository).screen(predictions, persist=False).empty
    assert DailyPredictionService(repository).generate(
        pd.DataFrame(), replace(DEFAULT_CONFIG, project_root=tmp_path), persist=False
    ).empty

    # Lifecycle changes never remove already-persisted audit history.
    assert len(repository.read_table("predictions")) == 3
    assert repository.read_table("signals")["model_id"].tolist() == ["model_active"]


def test_market_update_uses_only_frozen_operational_symbols(monkeypatch, tmp_path):
    symbols = ("AAA", "BBB")
    ProductionRepository(tmp_path).add(
        replace(_model(), status=ProductionModelStatus.ACTIVE, artifact_version=1)
    )
    index = pd.bdate_range("2026-01-01", periods=4)
    prices = pd.DataFrame(index=index)
    for symbol in symbols:
        for field, value in (("Open", 100.0), ("High", 102.0), ("Low", 99.0), ("Close", 101.0)):
            prices[f"{symbol}.{field}"] = value
    captured = []

    def fake_load(self, spec, **kwargs):
        captured.append(spec.symbols)
        return SimpleNamespace(prices=prices, symbols=list(symbols), failed_symbols=[]), {}

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    spec = ExperimentSpec(
        JobType.MARKET_UPDATE, replace(DEFAULT_CONFIG, project_root=tmp_path), symbols=symbols
    )
    output = tmp_path / "output"
    output.mkdir()
    summary = _market_update(spec, output, None, None)
    assert captured == [symbols]
    assert summary["requested_symbols"] == list(symbols)


def test_daily_prediction_threshold_screening_and_realized_result_are_separate(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    model = replace(
        _model(),
        status=ProductionModelStatus.ACTIVE,
        artifact_version=1,
        calibrated_signal_threshold=0.85,
    )
    repository.add(model)
    directory = repository.artifact_directory(model.model_id)
    directory.mkdir(parents=True)
    (directory / "production.metadata.json").write_text(
        json.dumps({
            "model_id": model.model_id,
            "artifact_version": 1,
            "feature_version": model.feature_version,
            "predictor_columns": ["BBB_intraday_J-1"],
        }),
        encoding="utf-8",
    )
    for name in ("up.ubj", "down.ubj"):
        (directory / name).write_bytes(b"x")
    index = pd.bdate_range("2026-01-05", periods=4)
    prepared = pd.DataFrame({
        "AAA.intraday_return": [0.0, 0.01, -0.01, 0.02],
        "BBB.intraday_return": [0.01, 0.02, -0.01, 0.03],
    }, index=index)
    market_data = pd.DataFrame({
        "BBB.Open": [100.0, 100.0, 100.0, 100.0],
        "BBB.High": [102.0, 103.0, 101.0, 104.0],
        "BBB.Low": [99.0, 99.0, 98.0, 99.0],
        "BBB.Close": [101.0, 102.0, 99.0, 103.0],
    }, index=index)
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda booster, *args: np.array([0.8 if booster == "up" else 0.2]),
    )
    predictions = DailyPredictionService(repository).generate(
        prepared,
        replace(DEFAULT_CONFIG, project_root=tmp_path, lag_depth=1),
        market_data=market_data,
    )
    signals = ProductionSignalService(repository).screen(predictions)
    assert len(predictions) == 1
    assert predictions.iloc[0]["up_probability"] == 0.8
    assert predictions.iloc[0]["signal_status"] == "no_signal"
    assert predictions.iloc[0]["up_threshold"] == 0.85
    snapshot_fields = {
        "feature_names", "features", "source_observations",
        "up_probability", "down_probability", "model_id", "model_version",
        "prediction_id", "prediction_date",
    }
    assert snapshot_fields <= set(predictions.columns)
    assert json.loads(predictions.iloc[0]["feature_names"]) == ["BBB_intraday_J-1"]
    assert json.loads(predictions.iloc[0]["features"]) == {
        "BBB_intraday_J-1": 0.03
    }
    assert json.loads(predictions.iloc[0]["source_observations"]) == {
        "BBB": [{
            "date": index[-1].date().isoformat(),
            "intraday_return": 0.03,
            "open": 100.0,
            "high": 104.0,
            "low": 99.0,
            "close": 103.0,
        }]
    }
    persisted_prediction = repository.read_table("predictions").iloc[0]
    assert snapshot_fields <= set(repository.read_table("predictions").columns)
    assert json.loads(persisted_prediction["features"]) == {
        "BBB_intraday_J-1": 0.03
    }
    assert json.loads(persisted_prediction["source_observations"])["BBB"][0]["close"] == 103.0
    assert signals.iloc[0]["category"] == "no_signal"

    prediction_date = pd.Timestamp(predictions.iloc[0]["prediction_date"])
    prices = pd.DataFrame(
        {"Open": [100.0], "High": [103.0], "Low": [98.0], "Close": [102.0]},
        index=[prediction_date],
    )
    realized = RealizedResultService(repository).update(lambda _: prices)
    repeated = RealizedResultService(repository).update(lambda _: prices)
    assert realized.iloc[0]["intraday_return"] == pytest.approx(0.02)
    assert realized.iloc[0]["open"] == 100.0
    assert realized.iloc[0]["high"] == 103.0
    assert realized.iloc[0]["low"] == 98.0
    assert realized.iloc[0]["close"] == 102.0
    assert realized.iloc[0]["up_target"] == 1
    assert repeated.empty
    assert not repository.read_table("predictions").empty
    assert not repository.read_table("signals").empty
    assert not repository.read_table("realized_results").empty
    assert len(repository.read_table("realized_results")) == 1
    assert repository.read_table("predictions").columns.tolist() != repository.read_table("realized_results").columns.tolist()
    displayed = build_evaluated_predictions_view(
        repository.read_table("predictions"),
        repository.read_table("signals"),
        repository.read_table("realized_results"),
    ).table.iloc[0]
    assert displayed["intraday_return"] == "2.00%"
    assert displayed["up_target_hit"] == "Oui"
    assert displayed["down_target_hit"] == "Non"


def test_training_cancellation_between_directions_leaves_no_partial_artifacts(
    monkeypatch, tmp_path
):
    repository = ProductionRepository(tmp_path)
    repository.add(_model())
    index = pd.bdate_range("2026-01-01", periods=5)
    prepared = pd.DataFrame({
        "BBB_intraday_J-1": np.arange(5, dtype=float),
        "AAA.intraday_target": [0, 1, 0, 1, 0],
        "AAA.intraday_down_target": [1, 0, 1, 0, 1],
    }, index=index)
    fitted = 0

    class FakeBooster:
        def save_model(self, path):
            path.write_bytes(b"partial")

    def fake_fit(*args, **kwargs):
        nonlocal fitted
        fitted += 1
        return FakeBooster()

    monkeypatch.setattr("rstock.application.production_services.fit_booster", fake_fit)
    with pytest.raises(CancellationRequested):
        ProductionTrainingService(repository).train(
            "model_test",
            prepared,
            replace(DEFAULT_CONFIG, project_root=tmp_path),
            cancellation_check=lambda: fitted == 1,
        )

    assert fitted == 1
    assert not repository.artifact_directory("model_test").exists()
    assert repository.get("model_test").status == ProductionModelStatus.CANDIDATE


def test_realized_result_is_attached_to_no_signal_prediction(tmp_path):
    repository = ProductionRepository(tmp_path)
    repository.add(_model())
    prediction_date = pd.Timestamp("2026-09-14")
    prediction = pd.DataFrame([{
        "prediction_id": "no-signal-prediction",
        "prediction_date": "2026-09-14",
        "as_of_date": "2026-09-11",
        "target": "AAA",
        "predictors": '["BBB"]',
        "model_id": "model_test",
        "model_version": 1,
        "up_probability": 0.4,
        "down_probability": 0.2,
        "up_threshold": 0.6,
        "down_threshold": 0.4,
        "signal_status": "no_signal",
        "status": "predicted",
        "error": None,
        "created_at": utc_now(),
    }])
    repository.append_table("predictions", prediction, key="prediction_id")
    repository.append_table(
        "signals",
        pd.DataFrame([{
            "signal_id": "no-signal-prediction",
            "prediction_id": "no-signal-prediction",
            "prediction_date": "2026-09-14",
            "model_id": "model_test",
            "target": "AAA",
            "category": "no_signal",
        }]),
        key="signal_id",
    )
    prices = pd.DataFrame(
        {"Open": [100.0], "High": [101.0], "Low": [98.0], "Close": [99.0]},
        index=[prediction_date],
    )

    realized = RealizedResultService(repository).update(lambda _: prices)

    assert realized["prediction_id"].tolist() == ["no-signal-prediction"]
    assert len(repository.read_table("realized_results")) == 1


def test_prediction_error_is_isolated_per_active_model(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    valid = replace(_model(), status=ProductionModelStatus.ACTIVE, artifact_version=1)
    broken = replace(
        _model("model_broken", target="CCC", predictors=("BBB",)),
        status=ProductionModelStatus.ACTIVE,
        artifact_version=1,
    )
    repository.add(valid)
    repository.add(broken)
    directory = repository.artifact_directory(valid.model_id)
    directory.mkdir(parents=True)
    (directory / "production.metadata.json").write_text(
        json.dumps({
            "model_id": valid.model_id,
            "artifact_version": 1,
            "feature_version": valid.feature_version,
            "predictor_columns": ["BBB_intraday_J-1"],
        }),
        encoding="utf-8",
    )
    for name in ("up.ubj", "down.ubj"):
        (directory / name).write_bytes(b"x")
    prepared = pd.DataFrame({
        "AAA.intraday_return": [0.01, 0.02],
        "BBB.intraday_return": [0.02, 0.03],
    }, index=pd.bdate_range("2026-01-05", periods=2))
    monkeypatch.setattr("rstock.application.production_services.load_booster", lambda path: path.stem)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities",
        lambda *args: np.array([0.7]),
    )

    predictions = DailyPredictionService(repository).generate(
        prepared, replace(DEFAULT_CONFIG, project_root=tmp_path)
    )

    statuses = predictions.set_index("model_id")["status"].to_dict()
    assert statuses == {"model_test": "predicted", "model_broken": "error"}
    assert "FileNotFoundError" in predictions.set_index("model_id").loc["model_broken", "error"]


def test_production_training_runs_through_async_worker_with_frozen_configuration(
    monkeypatch, tmp_path
):
    production = ProductionRepository(tmp_path)
    candidate = replace(
        _model(),
        lag_depth=2,
        up_target_threshold=0.02,
        down_target_threshold=0.03,
        xgboost_seed=987,
        xgboost_threads=1,
        source_configuration={
            "rstock_config": {
                "model_history_days": 777,
                "date_feature_regex": "",
            }
        },
    )
    production.add(candidate)
    prices = pd.DataFrame(index=pd.bdate_range("2026-01-01", periods=8))
    for symbol in candidate.symbols:
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.High"] = 104.0
        prices[f"{symbol}.Low"] = 96.0
        prices[f"{symbol}.Close"] = [101.0, 98.0] * 4
    observed_configs = []

    def fake_load(self, spec, **kwargs):
        observed_configs.append(spec.config)
        return SimpleNamespace(
            prices=prices,
            symbols=list(candidate.symbols),
            failed_symbols=[],
        ), {}

    class FakeBooster:
        def save_model(self, path):
            path.write_bytes(b"booster")

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    monkeypatch.setattr(
        "rstock.application.production_services.fit_booster",
        lambda *args, **kwargs: FakeBooster(),
    )
    runs = RunRepository(tmp_path / "runs")
    spec = ExperimentSpec(
        JobType.PRODUCTION_TRAINING,
        replace(
            DEFAULT_CONFIG,
            project_root=tmp_path,
            lag_depth=1,
            intraday_target_threshold=0.01,
            intraday_down_threshold=0.01,
            xgb_seed=1234,
            xgb_nthread=2,
        ),
        symbols=candidate.symbols,
        model_id=candidate.model_id,
    )
    run_id = runs.create(spec)

    execute_run(runs, run_id, 1)

    assert runs.status(run_id)["status"] == "completed"
    trained = production.get(candidate.model_id)
    assert trained.status == ProductionModelStatus.TRAINED
    assert observed_configs[0].lag_depth == 2
    assert observed_configs[0].intraday_target_threshold == 0.02
    assert observed_configs[0].intraday_down_threshold == 0.03
    assert observed_configs[0].xgb_seed == 987
    assert observed_configs[0].xgb_nthread == 1
    assert observed_configs[0].model_history_days == 777
    assert runs.progress(run_id)["workflow_percent"] == 100.0


def test_cancelled_operational_chain_publishes_no_partial_history(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    repository.add(
        replace(_model(), status=ProductionModelStatus.ACTIVE, artifact_version=1)
    )
    predicted = {"done": False}
    prediction = pd.DataFrame([{
        "prediction_id": "prediction-1", "status": "predicted",
        "model_id": "model_test", "target": "AAA",
    }])

    monkeypatch.setattr(
        "rstock.application.workflows._operational_prepared",
        lambda *args, **kwargs: (pd.DataFrame(), SimpleNamespace(symbols=["AAA", "BBB"])),
    )

    def fake_generate(*args, **kwargs):
        predicted["done"] = True
        return prediction

    monkeypatch.setattr(
        "rstock.application.workflows.DailyPredictionService.generate", fake_generate
    )
    spec = ExperimentSpec(
        JobType.OPERATIONAL_RUN,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
    )
    output = tmp_path / "output"
    output.mkdir()

    with pytest.raises(CancellationRequested):
        _operational_run(
            spec, output, None, lambda: predicted["done"]
        )

    assert repository.read_table("predictions").empty
    assert repository.read_table("signals").empty
    assert repository.read_table("realized_results").empty


def test_operational_history_directory_swap_rolls_back_on_publish_failure(
    monkeypatch, tmp_path
):
    repository = ProductionRepository(tmp_path)
    initial = pd.DataFrame([{"prediction_id": "old", "value": 1}])
    repository.append_table("predictions", initial, key="prediction_id")
    original_replace = os.replace

    def fail_staging_publish(source, destination):
        if ".history.staging-" in str(source):
            raise PermissionError(5, "Access denied", str(destination))
        return original_replace(source, destination)

    monkeypatch.setattr(production_repository_module.os, "replace", fail_staging_publish)
    with pytest.raises(PermissionError):
        repository.append_tables({
            "predictions": (
                pd.DataFrame([{"prediction_id": "new", "value": 2}]),
                "prediction_id",
            ),
            "signals": (
                pd.DataFrame([{"signal_id": "new", "category": "bullish_signal"}]),
                "signal_id",
            ),
        })

    persisted = repository.read_table("predictions")
    assert persisted.to_dict("records") == initial.to_dict("records")
    assert repository.read_table("signals").empty


def test_all_operational_workflows_are_registered():
    handlers = WorkflowRegistry.production().handlers
    assert {
        JobType.PRODUCTION_TRAINING,
        JobType.MARKET_UPDATE,
        JobType.DAILY_PREDICTION,
        JobType.DAILY_SCREENING,
        JobType.REALIZED_VALIDATION,
        JobType.OPERATIONAL_RUN,
    } <= set(handlers)


def test_cli_derives_production_training_symbols_from_candidate(
    monkeypatch, tmp_path, capsys
):
    from scripts import laboratory_cli

    ProductionRepository(tmp_path).add(_model())
    output = tmp_path / "production-training.json"
    monkeypatch.setattr(sys, "argv", [
        "laboratory_cli.py",
        "--project-root", str(tmp_path),
        "create-config",
        "--job-type", "production_training",
        "--model-id", "model_test",
        "--output", str(output),
    ])

    laboratory_cli.main()

    configured = ExperimentSpec.from_dict(json.loads(output.read_text(encoding="utf-8")))
    assert configured.model_id == "model_test"
    assert configured.symbols == ("AAA", "BBB")
    assert str(output) in capsys.readouterr().out


def test_old_experiment_config_without_operational_fields_still_loads(tmp_path):
    spec = ExperimentSpec(
        JobType.WALK_FORWARD, replace(DEFAULT_CONFIG, project_root=tmp_path), symbols=("AAA", "BBB")
    )
    payload = spec.to_dict()
    for field in (
        "model_id", "primary_universe_id", "context_universe_ids",
        "target_symbols", "context_symbols", "predictor_symbols",
    ):
        payload.pop(field)
    restored = ExperimentSpec.from_dict(payload)
    assert restored.model_id is None
    assert restored.symbols == spec.symbols
    assert restored.target_symbols == spec.symbols
    assert restored.context_symbols == ()
    assert restored.predictor_symbols == spec.symbols
