import json

import pandas as pd
import pytest

from rstock.checkpoints import (
    CheckpointCorruptError,
    CheckpointIncompatibleError,
    CheckpointManager,
)


def _manager(tmp_path, fingerprint="fingerprint"):
    return CheckpointManager(
        tmp_path / "run-1",
        run_id="run-1",
        job_type="walk_forward",
        configuration_fingerprint=fingerprint,
        batch_sizes={"walk_forward": 2},
    )


def test_checkpoint_batch_is_visible_only_after_completion_marker(tmp_path):
    manager = _manager(tmp_path)
    incomplete = manager.root / "batches" / "walk_forward" / "batch-000000"
    incomplete.mkdir(parents=True)
    (incomplete / "payload.pkl").write_bytes(b"partial")

    reloaded = _manager(tmp_path)

    assert reloaded.completed_batch_ids("walk_forward") == ()


def test_committed_batch_is_idempotent_and_reconciles_stale_manifest(tmp_path):
    manager = _manager(tmp_path)
    payload = {"windows": pd.DataFrame({"Set": ["A"]})}
    manager.set_total_batches("walk_forward", 1)
    manager.commit_batch(
        "walk_forward",
        0,
        payload,
        first_index=0,
        last_index=0,
        combination_count=1,
        row_counts={"windows": 1},
    )
    manifest = json.loads(manager.manifest_path.read_text(encoding="utf-8"))
    manifest["batches"]["walk_forward"]["completed"] = []
    manager.manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    reloaded = _manager(tmp_path)

    assert reloaded.completed_batch_ids("walk_forward") == (0,)
    pd.testing.assert_frame_equal(
        reloaded.load_batch("walk_forward", 0)["windows"], payload["windows"]
    )


def test_stale_manager_finish_preserves_workflow_phases_and_batches(tmp_path):
    worker_manager = _manager(tmp_path)
    worker_manager.start_attempt(resumed=False)
    workflow_manager = _manager(tmp_path)
    workflow_manager.phase_started("walk_forward")
    workflow_manager.set_total_batches("walk_forward", 1)
    workflow_manager.commit_batch(
        "walk_forward",
        0,
        {"windows": pd.DataFrame({"Set": ["A"]})},
        first_index=0,
        last_index=0,
        combination_count=1,
        row_counts={"windows": 1},
    )
    workflow_manager.phase_completed("walk_forward")

    worker_manager.finish_attempt("completed")

    reloaded = _manager(tmp_path)
    assert reloaded.completed_batch_ids("walk_forward") == (0,)
    assert reloaded.manifest["batches"]["walk_forward"] == {
        "total": 1,
        "completed": [0],
    }
    assert reloaded.manifest["phases_completed"] == ["walk_forward"]
    assert reloaded.manifest["attempts"][-1]["status"] == "completed"


def test_stale_manager_failure_keeps_reconciled_state_for_resume(tmp_path):
    worker_manager = _manager(tmp_path)
    worker_manager.start_attempt(resumed=False)
    workflow_manager = _manager(tmp_path)
    workflow_manager.set_total_batches("walk_forward", 2)
    workflow_manager.commit_batch(
        "walk_forward",
        0,
        {"windows": pd.DataFrame({"Set": ["A"]})},
        first_index=0,
        last_index=1,
        combination_count=2,
        row_counts={"windows": 1},
    )

    worker_manager.finish_attempt("failed", "interrupted")

    resumed = _manager(tmp_path)
    assert resumed.completed_batch_ids("walk_forward") == (0,)
    assert resumed.manifest["batches"]["walk_forward"]["total"] == 2
    assert resumed.manifest["state"] == "failed"


def test_checkpoint_refuses_a_different_configuration(tmp_path):
    _manager(tmp_path)

    with pytest.raises(CheckpointIncompatibleError, match="configuration"):
        _manager(tmp_path, fingerprint="changed")


def test_checkpoint_refuses_an_incompatible_schema_version(tmp_path):
    manager = _manager(tmp_path)
    manifest = json.loads(manager.manifest_path.read_text(encoding="utf-8"))
    manifest["checkpoint_schema_version"] = 999
    manager.manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    with pytest.raises(CheckpointIncompatibleError, match="version du checkpoint"):
        _manager(tmp_path)


def test_corrupt_committed_batch_is_never_loaded_silently(tmp_path):
    manager = _manager(tmp_path)
    manager.commit_batch(
        "walk_forward",
        0,
        {"value": 1},
        first_index=0,
        last_index=0,
        combination_count=1,
        row_counts={},
    )
    payload = manager.root / "batches" / "walk_forward" / "batch-000000" / "payload.pkl"
    payload.write_bytes(b"corrupt")

    with pytest.raises(CheckpointCorruptError, match="corrompu"):
        manager.load_batch("walk_forward", 0)


def test_snapshot_preserves_datetime_index_dtypes_and_attrs(tmp_path):
    manager = _manager(tmp_path)
    prepared = pd.DataFrame(
        {"value": pd.Series([1.5, 2.5], dtype="float64").to_numpy()},
        index=pd.date_range("2026-01-01", periods=2),
    )
    prepared.attrs["effective_end_date"] = "2026-01-02T00:00:00"
    manager.commit_snapshot(prepared, {"calendar": "XNYS"})

    restored, metadata = manager.load_snapshot()

    pd.testing.assert_frame_equal(restored, prepared)
    assert restored.attrs == prepared.attrs
    assert metadata == {"calendar": "XNYS"}
