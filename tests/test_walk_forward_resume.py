from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

import rstock.streaming_walk_forward as streaming
import rstock.walk_forward as walk_forward
import rstock.application.workflows as workflows
from rstock.checkpoints import CheckpointIncompatibleError, CheckpointManager
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.progress import CancellationRequested


def _fixture(tmp_path):
    index = pd.bdate_range("2024-01-01", periods=24)
    signal = np.arange(len(index)) % 2
    prices = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB")):
        shifted = np.roll(signal, offset)
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        prices[f"{symbol}.High"] = np.maximum(prices[f"{symbol}.Close"], 100.0) + 1.0
        prices[f"{symbol}.Low"] = np.minimum(prices[f"{symbol}.Close"], 100.0) - 1.0
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        combination_workers=1,
        predictor_prefilter_batch_size=1,
        walk_forward_batch_size=1,
        final_holdout_batch_size=1,
        xgb_rounds=1,
        xgb_nthread=1,
        walk_forward_min_train_size=8,
        walk_forward_test_size=4,
        walk_forward_step_size=4,
        final_holdout_size=4,
        qualification_min_windows=2,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    prepared = prepare_dataset(prices, ["AAA", "BBB"])
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    run = tmp_path / "run"
    manager = CheckpointManager(
        run,
        run_id="run",
        job_type="walk_forward",
        configuration_fingerprint="resume-fixture",
        batch_sizes={
            "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
            "walk_forward": config.walk_forward_batch_size,
            "final_holdout": config.final_holdout_batch_size,
        },
    )
    return prepared, generated, config, manager, run / "_working"


def _run(values):
    prepared, generated, config, manager, output = values
    return streaming.run_streamed_walk_forward(
        prepared, generated, config, manager, output
    )


def _forbid_walk_forward(monkeypatch):
    def forbidden(*args, **kwargs):
        raise AssertionError("completed walk-forward batch was recalculated")

    monkeypatch.setattr(streaming, "_walk_forward_combination", forbidden)


def test_resume_after_walk_forward_crash_replays_only_incomplete_batch(
    monkeypatch, tmp_path
):
    values = _fixture(tmp_path)
    original = streaming._walk_forward_combination
    calls = 0

    def fail_second(*args, **kwargs):
        nonlocal calls
        calls += 1
        if calls == 2:
            raise RuntimeError("walk-forward crash")
        return original(*args, **kwargs)

    monkeypatch.setattr(streaming, "_walk_forward_combination", fail_second)
    with pytest.raises(RuntimeError, match="walk-forward crash"):
        _run(values)
    assert values[3].completed_batch_ids("walk_forward") == (0,)

    resumed_calls = 0

    def count_remaining(*args, **kwargs):
        nonlocal resumed_calls
        resumed_calls += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(streaming, "_walk_forward_combination", count_remaining)
    _run(values)

    assert resumed_calls == 1
    assert values[3].completed_batch_ids("walk_forward") == (0, 1)


def test_resume_after_aggregation_failure_does_not_rerun_walk_forward(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    original = streaming._insert_predictions
    monkeypatch.setattr(
        streaming,
        "_insert_predictions",
        lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("aggregation crash")),
    )
    with pytest.raises(RuntimeError, match="aggregation crash"):
        _run(values)
    assert values[3].completed_batch_ids("walk_forward") == (0, 1)

    monkeypatch.setattr(streaming, "_insert_predictions", original)
    _forbid_walk_forward(monkeypatch)
    result = _run(values)

    assert len(result.qualification) == 2


def test_parent_aggregation_of_child_batches_matches_monolithic_run(tmp_path):
    monolithic = _fixture(tmp_path / "monolithic")
    _run(monolithic)

    prepared, generated, config, parent, output = _fixture(tmp_path / "batched")
    parent.set_total_batches("walk_forward", len(generated))
    for batch_id in range(len(generated)):
        child = CheckpointManager(
            tmp_path / "batched" / f"child-{batch_id}",
            run_id=f"child-{batch_id}",
            job_type="walk_forward_batch",
            configuration_fingerprint=f"child-{batch_id}",
            batch_sizes={
                "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
                "walk_forward": config.walk_forward_batch_size,
                "final_holdout": config.final_holdout_batch_size,
            },
        )
        streaming.run_streamed_walk_forward_batch(
            prepared, generated.iloc[[batch_id]], config, child
        )
        payload = child.load_batch("walk_forward", 0)
        parent.commit_batch(
            "walk_forward",
            batch_id,
            payload,
            first_index=batch_id,
            last_index=batch_id,
            combination_count=1,
            row_counts={
                "windows": len(payload["windows"]),
                "predictions": len(payload["predictions"]),
            },
        )
    parent.phase_completed("walk_forward")

    streaming.run_streamed_walk_forward(
        prepared,
        None,
        config,
        parent,
        output,
        precomputed_walk_forward_batches=len(generated),
        precomputed_combination_count=len(generated),
    )

    for name in (
        "aggregate_global.csv",
        "qualification.csv",
        "final_holdout.csv",
        "selection_results.csv",
        "risk_global.csv",
    ):
        expected = pd.read_csv(monolithic[4] / name)
        actual = pd.read_csv(output / name)
        pd.testing.assert_frame_equal(actual, expected, check_dtype=False, atol=1e-12)


def test_resume_after_qualification_failure_reuses_aggregation(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    original = streaming.qualify_combinations
    monkeypatch.setattr(
        streaming,
        "qualify_combinations",
        lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("qualification crash")),
    )
    with pytest.raises(RuntimeError, match="qualification crash"):
        _run(values)
    assert values[3].artifact_exists("aggregation")

    monkeypatch.setattr(streaming, "qualify_combinations", original)
    _forbid_walk_forward(monkeypatch)
    assert len(_run(values).qualification) == 2


def test_resume_final_holdout_only_replays_incomplete_batch(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    original = streaming._evaluate_final_holdout
    calls = 0

    def fail_second(*args, **kwargs):
        nonlocal calls
        calls += 1
        if calls == 2:
            raise RuntimeError("holdout crash")
        return original(*args, **kwargs)

    monkeypatch.setattr(streaming, "_evaluate_final_holdout", fail_second)
    with pytest.raises(RuntimeError, match="holdout crash"):
        _run(values)
    assert values[3].completed_batch_ids("final_holdout") == (0,)

    resumed_calls = 0

    def count_resume(*args, **kwargs):
        nonlocal resumed_calls
        resumed_calls += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(streaming, "_evaluate_final_holdout", count_resume)
    _forbid_walk_forward(monkeypatch)
    _run(values)

    assert resumed_calls == 1
    assert values[3].completed_batch_ids("final_holdout") == (0, 1)


def test_resume_after_metrics_failure_only_restarts_metrics(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    original = streaming.score_qualified_models
    monkeypatch.setattr(
        streaming,
        "score_qualified_models",
        lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("metrics crash")),
    )
    with pytest.raises(RuntimeError, match="metrics crash"):
        _run(values)

    monkeypatch.setattr(streaming, "score_qualified_models", original)
    _forbid_walk_forward(monkeypatch)
    result = _run(values)

    assert "model_selection" in result.run_configuration


def test_resume_after_result_writing_does_not_recalculate_science(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    original = streaming._atomic_csv_frames
    monkeypatch.setattr(
        streaming,
        "_atomic_csv_frames",
        lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("write crash")),
    )
    with pytest.raises(RuntimeError, match="write crash"):
        _run(values)
    assert values[3].artifact_exists("metrics")

    monkeypatch.setattr(streaming, "_atomic_csv_frames", original)
    _forbid_walk_forward(monkeypatch)
    monkeypatch.setattr(
        streaming,
        "_aggregate_final_risk",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            AssertionError("completed holdout risk was recalculated")
        ),
    )
    _run(values)

    assert (values[4] / "predictions.csv").exists()


def test_cancellation_keeps_completed_batch_and_resume_starts_at_next(monkeypatch, tmp_path):
    values = _fixture(tmp_path)
    cancelled = False

    def progress(event):
        nonlocal cancelled
        if event.details.get("checkpoint_written"):
            cancelled = True

    prepared, generated, config, manager, output = values
    with pytest.raises(CancellationRequested):
        streaming.run_streamed_walk_forward(
            prepared,
            generated,
            config,
            manager,
            output,
            progress_callback=progress,
            cancellation_check=lambda: cancelled,
        )
    assert manager.completed_batch_ids("walk_forward") == (0,)

    calls = 0
    original = streaming._walk_forward_combination

    def count_remaining(*args, **kwargs):
        nonlocal calls
        calls += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(streaming, "_walk_forward_combination", count_remaining)
    _run(values)

    assert calls == 1


def test_prefilter_resume_reuses_completed_qualification_batch(monkeypatch, tmp_path):
    prepared, generated, config, manager, _ = _fixture(tmp_path)
    cancelled = False

    def progress(event):
        nonlocal cancelled
        if event.details.get("checkpoint_written"):
            cancelled = True

    with pytest.raises(CancellationRequested):
        walk_forward.evaluate_prefilter_walk_forward(
            prepared,
            generated,
            config,
            progress_callback=progress,
            cancellation_check=lambda: cancelled,
            checkpoint_manager=manager,
        )
    assert manager.completed_batch_ids("predictor_prefilter_walk_forward") == (0,)

    calls = 0
    original = walk_forward._prefilter_combination

    def count_remaining(*args, **kwargs):
        nonlocal calls
        calls += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(walk_forward, "_prefilter_combination", count_remaining)
    result = walk_forward.evaluate_prefilter_walk_forward(
        prepared, generated, config, checkpoint_manager=manager
    )

    assert calls == 1
    assert len(result.qualification) == 2


def test_prefilter_protocol_reuses_new_checkpoint_and_rejects_legacy(tmp_path):
    fresh = _fixture(tmp_path / "fresh")[3]
    workflows._ensure_prefilter_checkpoint_protocol(fresh)
    workflows._ensure_prefilter_checkpoint_protocol(fresh)
    assert fresh.load_artifact("prefilter_execution_protocol") == (
        workflows.PREFILTER_POLICY_VERSION
    )

    legacy = _fixture(tmp_path / "legacy")[3]
    legacy.commit_batch(
        "predictor_prefilter_walk_forward", 0,
        {"qualification": pd.DataFrame([{"Set": "legacy"}])},
        first_index=0, last_index=0, combination_count=1,
        row_counts={"qualification": 1},
    )
    with pytest.raises(CheckpointIncompatibleError, match="Up\\+Down"):
        workflows._ensure_prefilter_checkpoint_protocol(legacy)


def test_streamed_walk_forward_reports_final_batch_telemetry(tmp_path):
    prepared, generated, config, manager, output = _fixture(tmp_path)
    events = []

    streaming.run_streamed_walk_forward(
        prepared,
        generated,
        config,
        manager,
        output,
        progress_callback=events.append,
    )

    completed = next(
        event
        for event in events
        if event.stage == "walk_forward" and event.substage == "completed"
    )
    assert completed.details["combinations_requested"] == 2
    assert completed.details["combinations_processed"] == 2
    assert completed.details["unique_combinations_processed"] == 2
    assert completed.details["expected_batches"] == 2
    assert completed.details["completed_batches"] == 2
    assert completed.details["peak_batch_prediction_rows"] > 0
    assert completed.details["phase_seconds"] >= 0


@pytest.mark.parametrize(
    "bad_index",
    [
        pd.DatetimeIndex(["2024-01-01", "2024-01-01"]),
        pd.DatetimeIndex(["2024-01-01", pd.NaT]),
    ],
    ids=["duplicate", "missing"],
)
def test_streamed_walk_forward_rejects_invalid_prepared_index(tmp_path, bad_index):
    prepared, generated, config, manager, output = _fixture(tmp_path)
    invalid = prepared.iloc[:2].copy()
    invalid.index = bad_index

    with pytest.raises(ValueError, match="complete and unique"):
        streaming.run_streamed_walk_forward(
            invalid, generated, config, manager, output
        )
