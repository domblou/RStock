"""Incremental operational quality reconciliation and publication."""

from dataclasses import replace

import pandas as pd
import pytest

from rstock.application.production_quality import build_quality_observations, compute_model_quality
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_quality_runtime import synchronize_production_quality
from rstock.application.production_repository import ProductionRepository

from test_production_quality import _model, _prediction


def _events(*models):
    predictions = pd.DataFrame([
        _prediction(f"prediction-{model_id}", model_id=model_id)
        for model_id in models
    ])
    signals = pd.DataFrame([
        {"signal_id": f"signal-{model_id}", "prediction_id": f"prediction-{model_id}",
         "model_id": model_id, "category": "bullish_signal"}
        for model_id in models
    ])
    results = pd.DataFrame([
        {"result_id": f"result-{model_id}", "prediction_id": f"prediction-{model_id}",
         "model_id": model_id, "open": 100.0, "high": 103.0, "low": 99.0,
         "close": 102.0, "intraday_return": 0.02, "mfe": 0.03, "mae": -0.01,
         "recorded_at": "2026-09-01T21:00:00Z"}
        for model_id in models
    ])
    return predictions, signals, results


def _setup(tmp_path, *model_ids):
    production = ProductionRepository(tmp_path)
    for model_id in model_ids:
        production.add(replace(_model(model_id), training_metadata={"train_end": "2026-08-28"}))
    predictions, signals, results = _events(*model_ids)
    production.append_tables({
        "predictions": (predictions, "prediction_id"),
        "signals": (signals, "signal_id"),
        "realized_results": (results, "result_id"),
    })
    return production, predictions, signals, results


def _sync(tmp_path, predictions, signals=None, results=None):
    evaluations = pd.DataFrame([
        {"prediction_id": prediction_id, "evaluation_status": "evaluated"}
        for prediction_id in ProductionRepository(tmp_path).read_table("predictions")["prediction_id"]
    ])
    return synchronize_production_quality(
        tmp_path, candidate_predictions=predictions,
        candidate_signals=signals, new_results=results if results is not None else pd.DataFrame(),
        evaluations=evaluations, as_of_session="2026-09-02",
    )


def test_unchanged_window_skips_all_identities_and_models(tmp_path):
    _, predictions, signals, results = _setup(tmp_path, "model_A", "model_B")
    first = _sync(tmp_path, predictions, signals, results)
    quality = ProductionQualityRepository(tmp_path)
    pointer = quality.current_generation_path.read_bytes()
    master = quality.snapshot_path().read_bytes()

    repeated = _sync(tmp_path, predictions)

    assert first["identities_new"] == 2
    assert first["models_rebuilt"] == 2
    assert repeated["events_read"] == 2
    assert repeated["identities_unchanged"] == 2
    assert repeated["identities_new"] == 0
    assert repeated["identities_modified"] == 0
    assert repeated["models_rebuilt"] == 0
    assert repeated["published_generation"] is None
    assert quality.current_generation_path.read_bytes() == pointer
    assert quality.snapshot_path().read_bytes() == master


def test_new_predictions_rebuild_only_affected_models(tmp_path):
    production, predictions, signals, results = _setup(tmp_path, "model_A", "model_B", "model_C")
    _sync(tmp_path, predictions, signals, results)
    added = pd.DataFrame([_prediction("prediction-A-new", model_id="model_A", prediction_date="2026-09-02")])
    production.append_table("predictions", added, key="prediction_id")

    summary = _sync(tmp_path, pd.concat([predictions, added], ignore_index=True))

    assert summary["identities_new"] == 1
    assert summary["identities_unchanged"] == 3
    assert summary["models_rebuilt"] == 1
    assert summary["models_processed"] == ["model_A"]


def test_multiple_new_events_rebuild_only_their_two_models(tmp_path):
    production, predictions, signals, results = _setup(tmp_path, "model_A", "model_B", "model_C")
    _sync(tmp_path, predictions, signals, results)
    added = pd.DataFrame([
        _prediction(f"new-{model_id}", model_id=model_id, prediction_date="2026-09-02")
        for model_id in ("model_A", "model_C")
    ])
    production.append_table("predictions", added, key="prediction_id")

    summary = _sync(tmp_path, pd.concat([predictions, added], ignore_index=True))

    assert summary["identities_new"] == 2
    assert summary["identities_unchanged"] == 3
    assert summary["models_rebuilt"] == 2
    assert summary["models_processed"] == ["model_A", "model_C"]


def test_relevant_source_changes_and_new_signals_are_selected(tmp_path):
    production, predictions, signals, results = _setup(tmp_path, "model_A")
    _sync(tmp_path, predictions, signals, results)
    irrelevant = predictions.copy()
    irrelevant["debug_note"] = "new trace text"
    production.append_table("predictions", irrelevant, key="prediction_id")
    unchanged = _sync(tmp_path, irrelevant)
    assert unchanged["identities_unchanged"] == 1
    assert unchanged["models_rebuilt"] == 0

    corrected = irrelevant.copy()
    corrected["up_probability"] = 0.81
    production.append_table("predictions", corrected, key="prediction_id")
    changed = _sync(tmp_path, corrected)
    assert changed["identities_modified"] == 1
    assert changed["models_processed"] == ["model_A"]
    assert ProductionQualityRepository(tmp_path).load_observations("model_A").iloc[0][
        "up_probability"
    ] == pytest.approx(0.81)

    corrected_signal = signals.copy()
    corrected_signal["category"] = "neutral"
    production.append_table("signals", corrected_signal, key="signal_id")
    signal_summary = _sync(tmp_path, pd.DataFrame(), corrected_signal)
    assert signal_summary["identities_modified"] == 1
    assert signal_summary["models_processed"] == ["model_A"]


def test_new_signal_for_existing_prediction_is_detected(tmp_path):
    production, predictions, _, results = _setup(tmp_path, "model_A")
    # The source prediction already exists; the later signal is a separate event.
    production.write_table("signals", pd.DataFrame())
    _sync(tmp_path, predictions, results=results)
    signal = pd.DataFrame([{
        "signal_id": "late-signal", "prediction_id": "prediction-model_A",
        "model_id": "model_A", "category": "neutral",
    }])
    production.append_table("signals", signal, key="signal_id")

    summary = _sync(tmp_path, pd.DataFrame(), signal)

    assert summary["identities_modified"] == 1
    assert summary["models_processed"] == ["model_A"]


def test_interrupted_batch_reconciles_only_unindexed_identity(monkeypatch, tmp_path):
    _, predictions, signals, results = _setup(tmp_path, "model_A", "model_B")
    original = ProductionQualityRepository.record_source_fingerprints
    interrupted = False

    def fail_second(self, model_id, fingerprints, *, expected_observation_generation):
        nonlocal interrupted
        if model_id == "model_B" and not interrupted:
            interrupted = True
            raise OSError("interrupted after observation publication")
        return original(
            self, model_id, fingerprints,
            expected_observation_generation=expected_observation_generation,
        )

    monkeypatch.setattr(ProductionQualityRepository, "record_source_fingerprints", fail_second)
    with pytest.raises(OSError, match="interrupted"):
        _sync(tmp_path, predictions, signals, results)
    monkeypatch.setattr(ProductionQualityRepository, "record_source_fingerprints", original)

    resumed = _sync(tmp_path, predictions)
    assert resumed["identities_unchanged"] == 1
    assert resumed["identities_unindexed"] == 1
    assert resumed["identities_new"] == 0
    assert resumed["models_rebuilt"] == 2
    assert resumed["models_remaining"] == 0


def test_published_batch_recovers_from_interrupted_clean_marker(monkeypatch, tmp_path):
    _, predictions, signals, results = _setup(tmp_path, "model_A")
    original = ProductionQualityRepository.mark_model_clean
    monkeypatch.setattr(
        ProductionQualityRepository, "mark_model_clean",
        lambda *args, **kwargs: (_ for _ in ()).throw(OSError("interrupted after pointer")),
    )
    with pytest.raises(OSError, match="interrupted"):
        _sync(tmp_path, predictions, signals, results)
    quality = ProductionQualityRepository(tmp_path)
    pointer = quality.current_generation_path.read_bytes()
    monkeypatch.setattr(ProductionQualityRepository, "mark_model_clean", original)

    resumed = _sync(tmp_path, predictions)
    assert resumed["identities_unchanged"] == 1
    assert resumed["models_rebuilt"] == 0
    assert resumed["models_remaining"] == 0
    assert quality.current_generation_path.read_bytes() == pointer


def test_observation_arriving_after_pointer_remains_dirty(monkeypatch, tmp_path):
    _, predictions, signals, results = _setup(tmp_path, "model_A")
    original = ProductionQualityRepository.publish_model_quality_batch

    def late_observation(self, updates, *, expected_observation_generations):
        name = original(
            self, updates,
            expected_observation_generations=expected_observation_generations,
        )
        current = self.load_observations("model_A")
        later = current.copy()
        later["prediction_id"] = "late-prediction"
        later["observation_id"] = "late-observation"
        self.upsert_observations("model_A", later)
        return name

    monkeypatch.setattr(ProductionQualityRepository, "publish_model_quality_batch", late_observation)
    first = _sync(tmp_path, predictions, signals, results)
    assert first["models_remaining"] == 1
    monkeypatch.setattr(ProductionQualityRepository, "publish_model_quality_batch", original)

    resumed = _sync(tmp_path, predictions)
    assert resumed["models_rebuilt"] == 1
    assert resumed["models_remaining"] == 0


def test_incremental_metrics_match_full_reconstruction(tmp_path):
    production, predictions, signals, results = _setup(tmp_path, "model_A", "model_B")
    _sync(tmp_path, predictions, signals, results)
    corrected = results.copy()
    corrected.loc[corrected["model_id"] == "model_A", ["close", "intraday_return"]] = [98.0, -0.02]
    production.append_table("realized_results", corrected, key="result_id")
    summary = _sync(tmp_path, predictions, results=corrected)
    assert summary["models_rebuilt"] == 1

    quality = ProductionQualityRepository(tmp_path)
    source_predictions = production.read_table("predictions")
    source_signals = production.read_table("signals")
    source_results = production.read_table("realized_results")
    evaluations = pd.DataFrame([
        {"prediction_id": value, "evaluation_status": "evaluated"}
        for value in source_predictions["prediction_id"]
    ])
    reference = build_quality_observations(
        source_predictions, source_signals, source_results, evaluations,
        train_end_by_model={(model.model_id, str(model.artifact_version)):
                            model.training_metadata["train_end"] for model in production.models()},
    )
    for model_id in ("model_A", "model_B"):
        expected_observations = reference[reference["model_id"] == model_id].reset_index(drop=True)
        actual_observations = quality.load_observations(model_id).reset_index(drop=True)
        columns = [
            "prediction_origin", "model_status_at_prediction", "signal_category",
            "open", "high", "low", "close", "intraday_return", "mfe", "mae",
            "is_winner", "evaluation_status",
        ]
        pd.testing.assert_frame_equal(
            actual_observations[columns], expected_observations[columns]
        )
        expected, expected_series = compute_model_quality(
            reference[reference["model_id"] == model_id],
            None, quality.load_current_lineage(model_id), "2026-09-02",
        )
        actual = quality.load_model_snapshot(model_id)
        for field in ("window_20", "window_63", "window_126", "since_promotion"):
            assert actual[field] == expected[field]
        pd.testing.assert_frame_equal(
            quality.load_model_series(model_id).reset_index(drop=True),
            expected_series.reset_index(drop=True),
        )
