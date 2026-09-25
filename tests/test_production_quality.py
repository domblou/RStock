import json
import os
from dataclasses import replace

import pandas as pd
import pytest

from rstock.application.production_domain import ProductionModel
from rstock.application.production_quality import (
    BASELINE_COMPARISON_VERSION,
    EvaluationStatus,
    ORIGIN_INFERENCE_VERSION,
    OBSERVATION_COLUMNS,
    PredictionOrigin,
    ProductionQualityMetricsService,
    build_quality_observations,
    compute_model_quality,
    compute_quality_metrics,
    infer_legacy_prediction_origin,
)
from rstock.application.production_quality_repository import (
    ProductionQualityRepository,
    reconcile_quality_observations,
)
import rstock.application.production_quality_repository as quality_repository_module
from rstock.application.production_quality_baseline import (
    AVAILABLE,
    UNAVAILABLE_MISSING_ARTIFACT,
    PromotionQualityService,
    build_model_lineage,
    build_promotion_baseline,
)
from rstock.application.production_quality_rebuild import (
    CHECKPOINT_NAME,
    ProductionQualityRebuildRunner,
)
from rstock.application.production_quality_runtime import synchronize_production_quality
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import RealizedResultService
from rstock.application.repository import RunRepository, utc_now
from rstock.application.domain import ExperimentSpec, JobType
from rstock.config import DEFAULT_CONFIG


def _model(model_id="model_A"):
    return ProductionModel(
        model_id=model_id,
        target="AAPL",
        predictors=("MSFT",),
        lag_depth=1,
        target_definition="Up >= 1%; Down <= -1%",
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
        created_at=utc_now(),
    )


def _prediction(
    prediction_id="prediction-1",
    *,
    model_id="model_A",
    model_version=1,
    prediction_date="2026-09-01",
    origin="scheduled_live",
    status="predicted",
):
    return {
        "prediction_id": prediction_id,
        "prediction_date": prediction_date,
        "as_of_date": "2026-08-31",
        "target": "AAPL",
        "predictors": '["MSFT"]',
        "model_id": model_id,
        "model_version": model_version,
        "up_probability": 0.72,
        "down_probability": 0.12,
        "up_threshold": 0.60,
        "down_threshold": 0.40,
        "signal_status": "bullish_signal",
        "prediction_origin": origin,
        "status": status,
        "created_at": "2026-08-31T20:00:00+00:00",
    }


def _canonical_observations(*predictions, ingested_at="2026-09-02T12:00:00Z"):
    prediction_frame = pd.DataFrame(predictions)
    signals = pd.DataFrame([
        {
            "prediction_id": row["prediction_id"],
            "category": row["signal_status"],
        }
        for row in predictions
    ])
    results = pd.DataFrame([
        {
            "result_id": row["prediction_id"],
            "prediction_id": row["prediction_id"],
            "open": 100.0,
            "high": 103.0,
            "low": 99.0,
            "close": 102.0,
            "intraday_return": 0.02,
            "mfe": 0.03,
            "mae": -0.01,
            "up_target": 1,
            "down_target": 0,
            "recorded_at": "2026-09-01T21:00:00Z",
        }
        for row in predictions
    ])
    evaluations = pd.DataFrame([
        {
            "prediction_id": row["prediction_id"],
            "evaluation_status": "evaluated",
            "exclusion_reason": None,
        }
        for row in predictions
    ])
    return build_quality_observations(
        prediction_frame,
        signals,
        results,
        evaluations,
        train_end_by_model={row["model_id"]: "2026-08-28" for row in predictions},
        ingested_at=ingested_at,
    )


def test_legacy_origin_inference_is_versioned_and_conservative():
    inferred = infer_legacy_prediction_origin(
        created_at="2026-08-31T20:00:00Z",
        prediction_date="2026-09-01",
        train_end="2026-08-28",
        as_of_date="2026-08-31",
    )
    assert inferred.origin is PredictionOrigin.LEGACY_INFERRED_LIVE
    assert inferred.rule_version == ORIGIN_INFERENCE_VERSION

    ambiguous = infer_legacy_prediction_origin(
        created_at="2026-09-01T12:00:00Z",
        prediction_date="2026-09-01",
        train_end="2026-08-28",
        as_of_date="2026-08-31",
    )
    assert ambiguous.origin is PredictionOrigin.LEGACY_UNKNOWN

    proven_backfill = infer_legacy_prediction_origin(
        created_at="2026-09-10T12:00:00Z",
        prediction_date="2026-09-01",
        train_end="2026-09-05",
        as_of_date="2026-08-31",
    )
    assert proven_backfill.origin is PredictionOrigin.OPERATIONAL_BACKFILL


def test_realized_evaluation_exposes_evaluated_pending_and_excluded(tmp_path):
    production = ProductionRepository(tmp_path)
    production.add(_model())
    predictions = pd.DataFrame([
        _prediction("complete", prediction_date="2026-09-01"),
        _prediction("bad-ohlc", prediction_date="2026-09-02"),
        _prediction("future", prediction_date="2026-09-03"),
        _prediction("invalid", prediction_date="2026-09-01", status="error"),
    ])
    production.append_table("predictions", predictions, key="prediction_id")
    prices = pd.DataFrame(
        {
            "Open": [100.0, None],
            "High": [103.0, 102.0],
            "Low": [99.0, 98.0],
            "Close": [102.0, 101.0],
        },
        index=pd.to_datetime(["2026-09-01", "2026-09-02"]),
    )

    batch = RealizedResultService(production).evaluate(lambda _symbol: prices)
    statuses = batch.evaluations.set_index("prediction_id")

    assert batch.results["prediction_id"].tolist() == ["complete"]
    assert statuses.loc["complete", "evaluation_status"] == "evaluated"
    assert statuses.loc["future", "evaluation_status"] == "pending"
    assert statuses.loc["bad-ohlc", "evaluation_status"] == "excluded"
    assert statuses.loc["bad-ohlc", "exclusion_reason"] == "missing_target_ohlc"
    assert statuses.loc["invalid", "evaluation_status"] == "excluded"
    assert statuses.loc["invalid", "exclusion_reason"] == "invalid_prediction"


def test_canonical_winner_exists_only_for_evaluated_bullish_signal():
    bullish = _prediction("bullish")
    no_signal = {**_prediction("no-signal"), "signal_status": "no_signal"}
    observations = _canonical_observations(bullish, no_signal)

    by_id = observations.set_index("prediction_id")
    assert bool(by_id.loc["bullish", "is_winner"]) is True
    assert pd.isna(by_id.loc["no-signal", "is_winner"])
    assert tuple(observations.columns) == OBSERVATION_COLUMNS


def test_quality_repository_upserts_by_model_version_and_prediction_id(tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    original = _canonical_observations(_prediction())

    first = repository.upsert_observations("model_A", original)
    repeated = repository.upsert_observations(
        "model_A",
        _canonical_observations(
            _prediction(), ingested_at="2026-09-03T12:00:00Z"
        ),
    )
    corrected = original.copy()
    corrected.loc[0, "close"] = 103.0
    corrected.loc[0, "intraday_return"] = 0.03
    correction = repository.upsert_observations("model_A", corrected)

    assert (first.added, first.updated, first.total) == (1, 0, 1)
    assert repeated.changed is False
    assert correction.updated == 1
    stored = repository.load_observations("model_A")
    assert len(stored) == 1
    assert stored.iloc[0]["intraday_return"] == pytest.approx(0.03)
    assert repository.load_manifest()["dirty_model_ids"] == ["model_A"]


def test_quality_repository_reingests_nullable_float_observation(tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    observation = _canonical_observations(_prediction())
    observation.loc[0, "open"] = pd.NA
    observation.loc[0, "intraday_return"] = pd.NA

    repository.upsert_observations("model_A", observation)
    repeated = repository.upsert_observations("model_A", observation)

    assert repeated.changed is False


def test_same_ticker_and_date_from_two_models_remain_distinct(tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    observations = _canonical_observations(
        _prediction("same-prediction", model_id="model_A"),
        _prediction("same-prediction", model_id="model_B"),
    )

    results = reconcile_quality_observations(repository, observations)

    assert set(results) == {"model_A", "model_B"}
    assert len(repository.load_observations("model_A")) == 1
    assert len(repository.load_observations("model_B")) == 1
    assert (
        repository.load_observations("model_A").iloc[0]["observation_id"]
        != repository.load_observations("model_B").iloc[0]["observation_id"]
    )


def test_parquet_round_trip_preserves_nullable_typed_contract(tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    observation = _canonical_observations(_prediction())
    observation.loc[0, "exclusion_reason"] = pd.NA
    observation.loc[0, "down_target"] = pd.NA
    repository.upsert_observations("model_A", observation)

    restored = repository.load_observations("model_A")

    assert str(restored["session_date"].dtype) == "datetime64[ns]"
    assert str(restored["is_bullish_signal"].dtype) == "boolean"
    assert str(restored["is_winner"].dtype) == "boolean"
    assert str(restored["down_target"].dtype) == "Int64"
    assert pd.isna(restored.iloc[0]["exclusion_reason"])
    assert restored.iloc[0]["intraday_return"] == pytest.approx(0.02, abs=1e-15)


def test_unchanged_model_is_not_marked_dirty(tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    repository.upsert_observations(
        "model_A", _canonical_observations(_prediction(model_id="model_A"))
    )
    repository.mark_model_clean("model_A")
    repository.upsert_observations(
        "model_A", _canonical_observations(_prediction(model_id="model_A"))
    )

    assert repository.load_manifest()["dirty_model_ids"] == []


def test_atomic_parquet_failure_keeps_previous_partition_readable(monkeypatch, tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    original = _canonical_observations(_prediction())
    repository.upsert_observations("model_A", original)
    corrected = original.copy()
    corrected.loc[0, "intraday_return"] = 0.05
    real_replace = os.replace

    def fail_parquet_publish(source, destination):
        if str(destination).endswith("observations.parquet"):
            raise PermissionError("simulated publication failure")
        return real_replace(source, destination)

    monkeypatch.setattr(quality_repository_module.os, "replace", fail_parquet_publish)
    with pytest.raises(PermissionError):
        repository.upsert_observations("model_A", corrected)

    stored = repository.load_observations("model_A")
    assert stored.iloc[0]["intraday_return"] == pytest.approx(0.02)


def test_resume_reconciles_parquet_published_before_manifest(monkeypatch, tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    original = _canonical_observations(_prediction())
    repository.upsert_observations("model_A", original)
    repository.mark_model_clean("model_A")
    corrected = original.copy()
    corrected.loc[0, "intraday_return"] = 0.04
    original_write_manifest = repository._write_manifest

    def fail_manifest(_manifest):
        raise PermissionError("simulated manifest interruption")

    monkeypatch.setattr(repository, "_write_manifest", fail_manifest)
    with pytest.raises(PermissionError):
        repository.upsert_observations("model_A", corrected)
    monkeypatch.setattr(repository, "_write_manifest", original_write_manifest)

    manifest = repository.reconcile_manifest()

    assert manifest["dirty_model_ids"] == ["model_A"]
    assert repository.load_observations("model_A").iloc[0][
        "intraday_return"
    ] == pytest.approx(0.04)
    assert repository.upsert_observations("model_A", corrected).changed is False


def test_quality_paths_are_prepared_without_creating_phase_three_artifacts(tmp_path):
    repository = ProductionQualityRepository(tmp_path)

    assert repository.snapshot_path().as_posix().endswith(
        "production/quality/snapshots/models.parquet"
    )
    assert repository.series_path("model_A").as_posix().endswith(
        "production/quality/series/model_A.parquet"
    )
    assert repository.baseline_path("model_A").as_posix().endswith(
        "production/quality/baselines/model_A.json"
    )
    assert repository.lineage_path("model_A").as_posix().endswith(
        "production/quality/lineage/model_A.json"
    )
    assert not repository.root.exists()


def _holdout_predictions(path, *, set_id='["AAPL","MSFT"]'):
    pd.DataFrame([
        {"Set": set_id, "Direction": "Up", "Window": 0, "Date": "2026-06-01", "Probability": 0.7, "IntradayReturn": 0.02, "MFE": 0.03, "MAE": -0.01},
        {"Set": set_id, "Direction": "Down", "Window": 0, "Date": "2026-06-01", "Probability": 0.2, "IntradayReturn": 0.02, "MFE": 0.03, "MAE": -0.01},
        {"Set": set_id, "Direction": "Up", "Window": 0, "Date": "2026-06-02", "Probability": 0.8, "IntradayReturn": -0.01, "MFE": 0.01, "MAE": -0.02},
        {"Set": set_id, "Direction": "Down", "Window": 0, "Date": "2026-06-02", "Probability": 0.1, "IntradayReturn": -0.01, "MFE": 0.01, "MAE": -0.02},
        {"Set": set_id, "Direction": "Up", "Window": 0, "Date": "2026-06-03", "Probability": 0.4, "IntradayReturn": 0.10, "MFE": 0.11, "MAE": -0.01},
        {"Set": set_id, "Direction": "Down", "Window": 0, "Date": "2026-06-03", "Probability": 0.1, "IntradayReturn": 0.10, "MFE": 0.11, "MAE": -0.01},
    ]).to_csv(path, index=False)


def test_baseline_pairs_up_down_and_applies_combined_rule(tmp_path):
    runs = RunRepository(tmp_path / "runs")
    run_id = "threshold-source"
    results = runs.run_directory(run_id) / "results"
    results.mkdir(parents=True)
    source = results / "holdout_predictions.csv"
    _holdout_predictions(source)
    model = replace(_model(), source_threshold_calibration_run=run_id)

    baseline = build_promotion_baseline(model, runs)

    assert baseline["availability_status"] == AVAILABLE
    assert baseline["source_holdout_sha256"]
    assert baseline["holdout_start"] == "2026-06-01"
    assert baseline["holdout_end"] == "2026-06-03"
    metrics = baseline["metrics"]
    assert metrics["holdout_sessions"] == 3
    assert metrics["evaluable_observations"] == 3
    assert metrics["signal_count"] == 2
    assert metrics["signal_rate"] == pytest.approx(2 / 3)
    assert metrics["positive_trade_count"] == 1
    assert metrics["win_rate_strict_gt_0"] == pytest.approx(0.5)
    assert metrics["mean_intraday_return"] == pytest.approx(0.005)
    assert metrics["median_intraday_return"] == pytest.approx(0.005)
    assert metrics["cumulative_return_sum"] == pytest.approx(0.01)
    assert metrics["mean_mfe"] == pytest.approx(0.02)
    assert metrics["mean_mae"] == pytest.approx(-0.015)


def test_missing_holdout_persists_unavailable_baseline_and_is_not_overwritten(tmp_path):
    runs = RunRepository(tmp_path / "runs")
    model = replace(_model(), source_threshold_calibration_run="missing")
    production = ProductionRepository(tmp_path)
    production.add(model)
    service = PromotionQualityService(tmp_path, runs, production)

    service.materialize_after_promotion(model)
    first = service.quality.load_baseline(model.model_id)
    assert first["availability_status"] == UNAVAILABLE_MISSING_ARTIFACT

    changed = service.quality.write_baseline(model.model_id, {**first, "availability_status": AVAILABLE})
    assert changed is False
    assert service.quality.load_baseline(model.model_id)["availability_status"] == UNAVAILABLE_MISSING_ARTIFACT


def test_explicit_rebuild_preserves_frozen_universe_name(tmp_path):
    runs = RunRepository(tmp_path / "runs")
    run_id = "threshold-source"
    source = runs.run_directory(run_id) / "results"
    source.mkdir(parents=True)
    _holdout_predictions(source / "holdout_predictions.csv")
    model = replace(
        _model(), source_threshold_calibration_run=run_id,
        source_configuration={"primary_universe_id": "gone", "temporal_validation_enabled": True},
        training_metadata={"universe_roles": {"primary_universe_id": "gone"}},
    )
    production = ProductionRepository(tmp_path)
    production.add(model)
    service = PromotionQualityService(tmp_path, runs, production)
    service.quality.write_lineage(model.model_id, {
        **build_model_lineage(model, runs),
        "primary_universe_name_at_promotion": "Nom figé",
    })

    rebuilt = service.rebuild_lineage_for_model(model.model_id)
    baseline = service.rebuild_baseline_for_model(model.model_id)

    assert rebuilt["primary_universe_name_at_promotion"] == "Nom figé"
    assert rebuilt["temporal_validation_status"] == "not_available"
    assert baseline["availability_status"] == AVAILABLE


def test_lineage_reads_temporal_decision_from_end_to_end_parent(tmp_path):
    runs = RunRepository(tmp_path / "runs")
    root_id = "end-to-end-root"
    results = runs.run_directory(root_id) / "results"
    results.mkdir(parents=True)
    (results / "temporal_validation_comparison.json").write_text(
        json.dumps({"final_status": "passed", "gates": {}}), encoding="utf-8"
    )
    model = replace(
        _model(),
        source_configuration={"source_end_to_end_run": root_id, "temporal_validation_enabled": True},
        training_metadata={"validation_provenance": {"temporal_validation_run_id": "validation-child"}},
    )

    lineage = build_model_lineage(model, runs)

    assert lineage["temporal_validation_status"] == "passed"
    assert lineage["temporal_validation_run_id"] == "validation-child"


def _metric_observations(returns, *, origins=None, statuses=None, bullish=None):
    dates = pd.bdate_range("2026-01-05", periods=len(returns))
    values = []
    for index, (date, value) in enumerate(zip(dates, returns)):
        row = _canonical_observations(_prediction(
            f"metric-{index}", prediction_date=date.date().isoformat(),
            origin=(origins or ["scheduled_live"] * len(returns))[index],
            status="predicted",
        )).iloc[0].copy()
        row["evaluation_status"] = (statuses or ["evaluated"] * len(returns))[index]
        row["is_bullish_signal"] = (bullish or [True] * len(returns))[index]
        row["signal_category"] = "bullish_signal" if row["is_bullish_signal"] else "no_signal"
        row["intraday_return"] = value
        row["mfe"] = value + 0.01
        row["mae"] = value - 0.01
        values.append(row)
    from rstock.application.production_quality import normalize_quality_observations
    return normalize_quality_observations(pd.DataFrame(values))


def test_metrics_respect_origins_statuses_strict_wins_and_notional_drawdown():
    frame = _metric_observations(
        [0.01, 0.02, -0.005, -0.03, 0.01, 0.0, 0.4, 0.5],
        origins=["scheduled_live", "legacy_inferred_live", "scheduled_live", "scheduled_live", "scheduled_live", "scheduled_live", "operational_backfill", "legacy_unknown"],
        statuses=["evaluated", "evaluated", "evaluated", "evaluated", "evaluated", "evaluated", "evaluated", "evaluated"],
        bullish=[True, True, True, True, True, True, True, True],
    )
    sessions = pd.DatetimeIndex(frame["session_date"].iloc[:6])

    metrics = compute_quality_metrics(frame, sessions)

    assert metrics["live_predictions"] == 6
    assert metrics["signal_count"] == 6
    assert metrics["positive_trade_count"] == 3
    assert metrics["flat_trade_count"] == 1
    assert metrics["win_rate"] == pytest.approx(0.5)
    assert metrics["pnl"] == pytest.approx(50.0)
    assert metrics["max_drawdown_dollars"] == pytest.approx(-350.0)
    assert metrics["current_drawdown_dollars"] == pytest.approx(-250.0)
    assert metrics["max_drawdown_return_points"] == pytest.approx(-0.035)


def test_pending_and_excluded_are_separate_and_do_not_become_zero_return():
    frame = _metric_observations(
        [0.02, 0.50, 0.50], statuses=["evaluated", "pending", "excluded"],
        bullish=[True, True, True],
    )
    metrics = compute_quality_metrics(frame, pd.DatetimeIndex(frame["session_date"]))

    assert metrics["evaluated_observations"] == 1
    assert metrics["pending_observations"] == 1
    assert metrics["excluded_observations"] == 1
    assert metrics["evaluability_rate"] == pytest.approx(0.5)
    assert metrics["signal_count"] == 1
    assert metrics["pnl"] == pytest.approx(200.0)


def test_model_quality_uses_market_session_windows_and_baseline_deltas():
    frame = _metric_observations([0.01, -0.01, 0.02])
    baseline = {"availability_status": "available", "metrics": {
        "mean_intraday_return": 0.01, "median_intraday_return": 0.01,
        "win_rate_strict_gt_0": 0.5, "signal_rate": 0.25,
        "mean_mfe": 0.02, "mean_mae": -0.01,
    }}
    lineage = {"model_id": "model_A", "promotion_date": "2026-01-01", "target": "AAPL"}

    snapshot, series = compute_model_quality(
        frame, baseline, lineage, "2026-01-09"
    )

    assert snapshot["window_20"]["market_sessions"] == 20
    assert snapshot["window_63"]["market_sessions"] == 63
    assert snapshot["baseline_comparison"]["version"] == BASELINE_COMPARISON_VERSION
    assert snapshot["baseline_comparison"]["delta_mean_return"] == pytest.approx(
        (0.01 - 0.01 + 0.02) / 3 - 0.01
    )
    assert len(series) > 3
    assert (series["signal_count"] == 0).any()
    assert snapshot["health_status"] == "not_evaluated"


def test_model_quality_accepts_timezone_aware_as_of_and_promotion_dates():
    frame = _metric_observations([0.01])
    lineage = {
        "model_id": "model_A",
        "promotion_date": "2026-01-01T15:30:00+00:00",
    }

    snapshot, series = compute_model_quality(
        frame, None, lineage, pd.Timestamp("2026-01-09T00:00:00+00:00")
    )

    assert snapshot["as_of_session"] == "2026-01-09"
    assert not series.empty


def test_rebuild_publishes_all_derived_artifacts_before_cleaning(monkeypatch, tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    frame = _metric_observations([0.01, -0.01])
    repository.upsert_observations("model_A", frame)
    repository.write_lineage("model_A", {"model_id": "model_A", "promotion_date": "2026-01-01"})
    service = ProductionQualityMetricsService(repository)
    original = repository.upsert_master_snapshot

    def fail_master(_row):
        raise OSError("simulated master publication failure")

    monkeypatch.setattr(repository, "upsert_master_snapshot", fail_master)
    with pytest.raises(OSError):
        service.rebuild_model("model_A", "2026-01-09")
    assert repository.load_manifest()["dirty_model_ids"] == ["model_A"]

    monkeypatch.setattr(repository, "upsert_master_snapshot", original)
    snapshot = service.rebuild_model("model_A", "2026-01-09")
    assert repository.load_model_series("model_A").shape[0] > 0
    assert repository.load_model_snapshot("model_A")["as_of_session"] == snapshot["as_of_session"]
    assert repository.load_master_snapshot()["model_id"].tolist() == ["model_A"]
    assert repository.load_manifest()["dirty_model_ids"] == []


def test_quality_calculation_is_idempotent_and_models_do_not_share_notional():
    frame = _metric_observations([0.01])
    lineage = {"model_id": "model_A", "promotion_date": "2026-01-01"}
    first, series_first = compute_model_quality(frame, None, lineage, "2026-01-09")
    second, series_second = compute_model_quality(frame, None, lineage, "2026-01-09")

    assert first == second
    pd.testing.assert_frame_equal(series_first, series_second)
    other_frame = frame.copy()
    other_frame["model_id"] = "model_B"
    other, _ = compute_model_quality(other_frame, None, {**lineage, "model_id": "model_B"}, "2026-01-09")
    assert first["since_promotion"]["pnl"] + other["since_promotion"]["pnl"] == pytest.approx(200.0)


def test_phase_five_sync_is_incremental_detects_late_correction_and_noops(tmp_path):
    production = ProductionRepository(tmp_path)
    prediction = _prediction("late", model_id="model_A", prediction_date="2026-01-05")
    signals = pd.DataFrame([{"signal_id": "late", "prediction_id": "late", "category": "bullish_signal"}])
    result = pd.DataFrame([{
        "result_id": "late", "prediction_id": "late", "open": 100.0, "high": 103.0,
        "low": 99.0, "close": 102.0, "intraday_return": 0.02, "mfe": 0.03,
        "mae": -0.01, "up_target": 1, "down_target": 0, "recorded_at": "2026-01-05T21:00:00Z",
    }])
    production.append_tables({
        "predictions": (pd.DataFrame([prediction]), "prediction_id"),
        "signals": (signals, "signal_id"),
        "realized_results": (result, "result_id"),
    })
    evaluations = pd.DataFrame([{"prediction_id": "late", "evaluation_status": "evaluated", "exclusion_reason": None}])

    first = synchronize_production_quality(
        tmp_path, candidate_predictions=pd.DataFrame([prediction]), new_results=result,
        evaluations=evaluations, as_of_session="2026-01-09",
    )
    second = synchronize_production_quality(
        tmp_path, candidate_predictions=pd.DataFrame(), new_results=pd.DataFrame(),
        evaluations=pd.DataFrame(), as_of_session="2026-01-09",
    )
    corrected = result.copy()
    corrected.loc[0, "intraday_return"] = 0.03
    production.append_table("realized_results", corrected, key="result_id")
    third = synchronize_production_quality(
        tmp_path, candidate_predictions=pd.DataFrame(), new_results=corrected,
        evaluations=evaluations, as_of_session="2026-01-09",
    )

    quality = ProductionQualityRepository(tmp_path)
    assert first["models_processed"] == ["model_A"]
    assert second["dirty_detected"] == 0
    assert third["models_processed"] == ["model_A"]
    assert len(quality.load_observations("model_A")) == 1
    assert quality.load_observations("model_A").iloc[0]["intraday_return"] == pytest.approx(0.03)
    assert quality.load_manifest()["dirty_model_ids"] == []


def test_phase_six_rebuild_stages_then_atomically_publishes_generation(tmp_path):
    quality = ProductionQualityRepository(tmp_path)
    quality.upsert_observations("model_A", _metric_observations([0.01]))
    second = _metric_observations([0.02])
    second["model_id"] = "model_B"
    quality.upsert_observations("model_B", second)
    quality.write_lineage("model_A", {"model_id": "model_A", "promotion_date": "2026-01-01"})
    quality.write_lineage("model_B", {"model_id": "model_B", "promotion_date": "2026-01-01"})
    runs = RunRepository(tmp_path / "runs")
    run_id = runs.create(ExperimentSpec(
        JobType.PRODUCTION_QUALITY_REBUILD,
        replace(DEFAULT_CONFIG, project_root=tmp_path), symbols=("AAA", "BBB"),
    ))

    result = ProductionQualityRebuildRunner(runs, run_id, tmp_path).execute("2026-01-09")

    assert result["completed_models"] == 2
    assert quality.current_generation_path.exists()
    assert quality.load_master_snapshot()["model_id"].tolist() == ["model_A", "model_B"]
    checkpoint = runs.read_json(run_id, CHECKPOINT_NAME)
    assert checkpoint["status"] == "completed"
    assert checkpoint["completed_model_ids"] == ["model_A", "model_B"]
