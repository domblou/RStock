"""Read a source Walk-forward prepared checkpoint without altering that run."""

from __future__ import annotations

import hashlib
import json
import pickle
from pathlib import Path

import pandas as pd

from rstock.traceability import verify_prepared_dataset_digest

from .domain import ExperimentSpec, JobType
from .repository import RunRepository


def load_source_prepared_snapshot(
    repository: RunRepository, spec: ExperimentSpec,
) -> tuple[pd.DataFrame, list[str], list[str], dict[str, str]]:
    source_id = spec.source_walk_forward_run
    if not source_id or not spec.source_prepared_dataset_sha256:
        raise ValueError("Snapshot Walk-forward source or expected digest is missing")
    if repository.storage(source_id)["state"] != "full":
        raise ValueError("Source Walk-forward snapshot has been purged")
    if repository.status(source_id).get("status") != "completed":
        raise ValueError("Source Walk-forward is not completed")
    if repository.load_spec(source_id).job_type is not JobType.WALK_FORWARD:
        raise ValueError("Prepared snapshot source is not a Walk-forward")
    source_config = repository.load_spec(source_id).config
    for field in (
        "model_history_days", "lag_depth", "intraday_target_threshold",
        "intraday_down_threshold", "date_feature_regex",
    ):
        if getattr(spec.config, field) != getattr(source_config, field):
            raise ValueError(f"Derived preparation contract changed: {field}")
    root = repository.run_directory(source_id) / "checkpoints" / "artifacts"
    path = root / "prepared_snapshot.pkl"
    sidecar = root / "prepared_snapshot.json"
    try:
        raw = path.read_bytes()
        metadata = json.loads(sidecar.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("Source Walk-forward prepared snapshot is missing or unreadable") from error
    if (
        metadata.get("name") != "prepared_snapshot"
        or metadata.get("sha256") != hashlib.sha256(raw).hexdigest()
        or metadata.get("configuration_fingerprint")
        != repository.configuration_fingerprint(source_id)
    ):
        raise ValueError("Source Walk-forward prepared snapshot is corrupt")
    try:
        payload = pickle.loads(raw)
    except (pickle.UnpicklingError, EOFError, AttributeError, ImportError) as error:
        raise ValueError("Source Walk-forward prepared snapshot is corrupt") from error
    if not isinstance(payload, dict) or not isinstance(payload.get("prepared"), pd.DataFrame):
        raise ValueError("Source Walk-forward prepared snapshot has an invalid contract")
    prepared = payload["prepared"]
    info = payload.get("metadata")
    if not isinstance(info, dict) or prepared.empty:
        raise ValueError("Source Walk-forward prepared snapshot has an invalid contract")
    predictors = info.get("predictor_symbols")
    targets = info.get("target_symbols")
    calendars = info.get("calendars")
    if (
        not isinstance(predictors, list)
        or not isinstance(targets, list)
        or not isinstance(calendars, dict)
        or not isinstance(prepared.index, pd.DatetimeIndex)
        or prepared.index.has_duplicates
        or not prepared.index.is_monotonic_increasing
        or prepared.columns.has_duplicates
        or not set(predictors).issubset(spec.predictor_symbols)
        or not set(targets).issubset(spec.target_symbols)
        or info.get("effective_end_date") != prepared.attrs.get("effective_end_date")
    ):
        raise ValueError("Source Walk-forward prepared snapshot has an invalid contract")
    source_traceability = repository.summary(source_id).get("traceability")
    if not isinstance(source_traceability, dict):
        raise ValueError("Source Walk-forward traceability is missing")
    if source_traceability.get("prepared_dataset_sha256") != spec.source_prepared_dataset_sha256:
        raise ValueError("Source Walk-forward digest differs from the frozen reference")
    if str(prepared.index.max().isoformat()) != source_traceability.get("prepared_market_last_date"):
        raise ValueError("Source Walk-forward prepared dates differ from traceability")
    verification = verify_prepared_dataset_digest(
        prepared,
        expected_digest=spec.source_prepared_dataset_sha256,
        required=True,
        run_id=spec.execution_run_id,
        source_run_id=source_id,
        cutoff=spec.historical_data_cutoff,
        stage=spec.job_type.value,
    )
    prepared = prepared.copy()
    prepared.attrs["prepared_dataset_digest_verification"] = verification
    return prepared, list(predictors), list(targets), dict(calendars)
