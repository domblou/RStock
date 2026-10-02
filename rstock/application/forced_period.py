"""Immutable temporal dataset and session contract for modern forced validation."""

from __future__ import annotations

import hashlib
import json
import pickle
from pathlib import Path
from typing import Any

import pandas as pd
import exchange_calendars as xcals

from rstock.traceability import prepared_dataset_hash

from .domain import JobType, RunPurpose
from .repository import RunRepository


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _sessions(prepared: pd.DataFrame) -> list[str]:
    index = prepared.index
    if not isinstance(index, pd.DatetimeIndex) or index.empty or index.has_duplicates or not index.is_monotonic_increasing:
        raise ValueError("Temporal prepared sessions are invalid")
    if index.tz is not None or not (index == index.normalize()).all():
        raise ValueError("Temporal prepared sessions must be timezone-free dates")
    calendar = xcals.get_calendar("XNYS")
    if any(not calendar.is_session(day) for day in index):
        raise ValueError("Temporal prepared dates are not XNYS sessions")
    return [item.date().isoformat() for item in index]


def _session_digest(sessions: list[str]) -> str:
    return hashlib.sha256(json.dumps(sessions, separators=(",", ":")).encode()).hexdigest()


def build_forced_period_lock(
    repository: RunRepository, temporal_run_id: str,
) -> dict[str, Any]:
    """Freeze the completed temporal child's prepared snapshot and holdout geometry."""
    spec = repository.load_spec(temporal_run_id)
    metadata = repository.run_metadata(temporal_run_id)
    if (
        spec.job_type is not JobType.END_TO_END
        or metadata.run_purpose is not RunPurpose.TEMPORAL_VALIDATION
        or repository.status(temporal_run_id)["status"] != "completed"
        or spec.config.walk_forward_end_offset_sessions != 63
        or spec.calendar != "XNYS"
    ):
        raise ValueError("Completed offset-63 temporal End-to-End is required")
    manifest = repository.read_json(temporal_run_id, "orchestration/pipeline.json")
    if manifest.get("schema_version") != 3:
        raise ValueError("Split temporal manifest is required for a period lock")
    stages = {item["stage_key"]: item for item in manifest["stages"]}
    walk_id = str(stages["walk_forward"]["child_run_id"])
    holdout_stage = stages.get("holdout_evaluation") or stages["threshold_calibration"]
    holdout_id = str(holdout_stage["child_run_id"])
    if repository.status(walk_id)["status"] != "completed" or repository.status(holdout_id)["status"] != "completed":
        raise ValueError("Temporal period sources must be completed")
    root = repository.run_directory(walk_id) / "checkpoints" / "artifacts"
    path = root / "prepared_snapshot.pkl"
    sidecar = repository.read_json(walk_id, "checkpoints/artifacts/prepared_snapshot.json")
    snapshot_sha = _digest(path)
    if sidecar.get("sha256") != snapshot_sha or sidecar.get("configuration_fingerprint") != repository.configuration_fingerprint(walk_id):
        raise ValueError("Temporal prepared snapshot digest differs")
    payload = pickle.loads(path.read_bytes())
    prepared = payload.get("prepared") if isinstance(payload, dict) else None
    if not isinstance(prepared, pd.DataFrame):
        raise ValueError("Temporal prepared snapshot is invalid")
    sessions = _sessions(prepared)
    trace = repository.summary(walk_id).get("traceability")
    if not isinstance(trace, dict) or trace.get("prepared_dataset_sha256") != prepared_dataset_hash(prepared):
        raise ValueError("Temporal prepared dataset digest differs")
    if trace.get("prepared_market_last_date") != prepared.index.max().isoformat():
        raise ValueError("Temporal prepared cutoff differs from Walk-forward traceability")
    configuration = repository.read_json(holdout_id, "results/run_configuration.json")
    size = int(
        configuration["final_holdout_size"]
        if "final_holdout_size" in configuration
        else repository.load_spec(holdout_id).config.final_holdout_size
    )
    if size < 1 or size >= len(sessions):
        raise ValueError("Temporal holdout size is invalid")
    holdout_sessions = sessions[-size:]
    development_end = sessions[-size - 1]
    if configuration.get("development_end") and pd.Timestamp(configuration["development_end"]).date().isoformat() != development_end:
        raise ValueError("Temporal development boundary differs")
    if configuration.get("final_holdout_start") and pd.Timestamp(configuration["final_holdout_start"]).date().isoformat() != holdout_sessions[0]:
        raise ValueError("Temporal holdout boundary differs")
    predictions_path = repository.run_directory(holdout_id) / "results" / "holdout_predictions.csv"
    if predictions_path.is_file():
        predictions = pd.read_csv(predictions_path, usecols=["Date"])
        observed = pd.to_datetime(predictions["Date"], errors="raise")
        if not set(observed.dt.date.map(str)).issubset(holdout_sessions):
            raise ValueError("Temporal holdout observations differ from frozen sessions")
    return {
        "schema_version": 1,
        "temporal_run_id": temporal_run_id,
        "temporal_walk_forward_run_id": walk_id,
        "temporal_holdout_run_id": holdout_id,
        "offset_sessions": repository.load_spec(temporal_run_id).config.walk_forward_end_offset_sessions,
        "effective_cutoff": sessions[-1],
        "prepared_first_session": sessions[0],
        "prepared_sessions": sessions,
        "prepared_sessions_sha256": _session_digest(sessions),
        "prepared_dataset_sha256": trace["prepared_dataset_sha256"],
        "prepared_snapshot_sha256": snapshot_sha,
        "development_end": development_end,
        "holdout_first_session": holdout_sessions[0],
        "holdout_last_session": holdout_sessions[-1],
        "holdout_sessions": holdout_sessions,
        "holdout_sessions_sha256": _session_digest(holdout_sessions),
        "final_holdout_size": size,
    }


def validate_forced_period(prepared: pd.DataFrame, lock: dict[str, Any]) -> None:
    sessions = _sessions(prepared)
    size = lock.get("final_holdout_size")
    if (
        lock.get("schema_version") != 1
        or sessions != lock.get("prepared_sessions")
        or _session_digest(sessions) != lock.get("prepared_sessions_sha256")
        or prepared_dataset_hash(prepared) != lock.get("prepared_dataset_sha256")
        or sessions[-1] != lock.get("effective_cutoff")
        or not isinstance(size, int) or size < 1 or size >= len(sessions)
        or sessions[-size:] != lock.get("holdout_sessions")
        or _session_digest(sessions[-size:]) != lock.get("holdout_sessions_sha256")
        or sessions[-size - 1] != lock.get("development_end")
        or sessions[-size] != lock.get("holdout_first_session")
        or sessions[-1] != lock.get("holdout_last_session")
    ):
        raise ValueError("Forced validation period differs from frozen temporal period")
