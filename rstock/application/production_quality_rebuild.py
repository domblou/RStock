"""Explicit, generation-based rebuild of Phase-4 derived quality artifacts."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from time import perf_counter
from typing import Any

import pandas as pd

from rstock.progress import CancellationCheck, CancellationRequested, check_cancellation

from .production_quality import (
    QUALITY_METRICS_VERSION,
    compute_model_quality,
    master_snapshot_row,
)
from .production_quality_repository import ProductionQualityRepository
from .repository import RunRepository, utc_now


CHECKPOINT_NAME = "quality_rebuild_checkpoint.json"
REBUILD_SCHEMA_VERSION = 1


def _source_digest(manifest: dict[str, Any]) -> str:
    encoded = json.dumps(
        manifest.get("observation_generations", {}), sort_keys=True, separators=(",", ":")
    ).encode()
    return hashlib.sha256(encoded).hexdigest()


def _validate_snapshot(snapshot: dict[str, Any], model_id: str) -> None:
    if snapshot.get("model_id") != model_id:
        raise ValueError("Incomplete quality snapshot model identity")
    if snapshot.get("quality_metrics_version") != QUALITY_METRICS_VERSION:
        raise ValueError("Incompatible quality metrics version")
    for name in ("window_20", "window_63", "window_126", "since_promotion"):
        values = snapshot.get(name)
        if not isinstance(values, dict):
            raise ValueError(f"Missing quality metric block: {name}")
        for rate in ("win_rate", "evaluability_rate"):
            value = values.get(rate)
            if value is not None and not 0 <= float(value) <= 1:
                raise ValueError(f"Invalid {rate}")
        if values.get("signal_rate") is not None and float(values["signal_rate"]) < 0:
            raise ValueError("Invalid signal_rate")
        for drawdown in ("max_drawdown_dollars", "current_drawdown_dollars", "max_drawdown_return_points", "current_drawdown_return_points"):
            value = values.get(drawdown)
            if value is not None and float(value) > 0:
                raise ValueError("Invalid positive drawdown")


class ProductionQualityRebuildRunner:
    """One-model-at-a-time, resumable rebuild that never publishes partial output."""

    def __init__(self, runs: RunRepository, run_id: str, project_root: Path) -> None:
        self.runs = runs
        self.run_id = run_id
        self.quality = ProductionQualityRepository(project_root)
        self.stage_name = f"generation_{run_id}.tmp"
        self.final_name = f"generation_{run_id}"

    @property
    def stage(self) -> Path:
        return self.quality.generation_path(self.stage_name)

    def _load_or_start(self) -> dict[str, Any]:
        manifest = self.quality.reconcile_manifest()
        models = sorted(manifest.get("observation_generations", {}))
        digest = _source_digest(manifest)
        path = self.runs.run_directory(self.run_id) / CHECKPOINT_NAME
        if path.exists():
            checkpoint = self.runs.read_json(self.run_id, CHECKPOINT_NAME)
            if (
                checkpoint.get("quality_metrics_version") != QUALITY_METRICS_VERSION
                or checkpoint.get("source_observation_digest") != digest
                or checkpoint.get("target_generation") != self.final_name
            ):
                raise ValueError("Quality rebuild checkpoint is incompatible with current observations/version")
            return checkpoint
        self.stage.mkdir(parents=True, exist_ok=False)
        checkpoint = {
            "schema_version": REBUILD_SCHEMA_VERSION,
            "run_id": self.run_id,
            "status": "running",
            "created_at": utc_now(),
            "started_at": utc_now(),
            "completed_at": None,
            "current_model": None,
            "total_models": len(models),
            "completed_model_ids": [],
            "failed_models": [],
            "quality_metrics_version": QUALITY_METRICS_VERSION,
            "target_generation": self.final_name,
            "source_observation_digest": digest,
            "source_observation_generations": manifest.get("observation_generations", {}),
        }
        self.runs.write_json(self.run_id, CHECKPOINT_NAME, checkpoint)
        return checkpoint

    def _persist(self, checkpoint: dict[str, Any]) -> None:
        self.runs.write_json(self.run_id, CHECKPOINT_NAME, checkpoint)

    def execute(self, as_of_session: object, *, cancellation_check: CancellationCheck | None = None) -> dict[str, Any]:
        started = perf_counter()
        checkpoint = self._load_or_start()
        completed = set(str(value) for value in checkpoint.get("completed_model_ids", []))
        model_ids = sorted(str(value) for value in checkpoint.get("source_observation_generations", {}))
        try:
            for model_id in model_ids:
                check_cancellation(cancellation_check)
                if model_id in completed:
                    continue
                checkpoint["current_model"] = model_id
                self._persist(checkpoint)
                item_started = perf_counter()
                observations = self.quality.load_observations(model_id)
                snapshot, series = compute_model_quality(
                    observations,
                    self.quality.load_baseline(model_id),
                    self.quality.load_lineage(model_id),
                    as_of_session,
                )
                snapshot["model_id"] = model_id
                _validate_snapshot(snapshot, model_id)
                self.quality._atomic_parquet(self.stage / "series" / f"{self.quality._model_token(model_id)}.parquet", series)
                self.quality._atomic_text(
                    self.stage / "snapshots" / f"{self.quality._model_token(model_id)}.json",
                    json.dumps(snapshot, indent=2, sort_keys=True, default=str) + "\n",
                )
                completed.add(model_id)
                checkpoint["completed_model_ids"] = sorted(completed)
                checkpoint["current_model"] = None
                checkpoint.setdefault("model_timings_seconds", {})[model_id] = perf_counter() - item_started
                self._persist(checkpoint)
            check_cancellation(cancellation_check)
            # A crash after directory publication but before the atomic pointer
            # update leaves a complete final directory and the old generation
            # active. Resume validates that directory and only flips the pointer.
            workspace = (
                self.quality.generation_path(self.final_name)
                if self.quality.generation_path(self.final_name).exists()
                else self.stage
            )
            snapshots: list[dict[str, Any]] = []
            for model_id in model_ids:
                path = workspace / "snapshots" / f"{self.quality._model_token(model_id)}.json"
                if not path.exists():
                    raise ValueError("Missing staged model snapshot")
                snapshot = json.loads(path.read_text(encoding="utf-8"))
                _validate_snapshot(snapshot, model_id)
                series_path = workspace / "series" / f"{self.quality._model_token(model_id)}.parquet"
                if not series_path.exists():
                    raise ValueError("Missing staged model series")
                snapshots.append(snapshot)
            if len({item["model_id"] for item in snapshots}) != len(snapshots):
                raise ValueError("Duplicate model in quality rebuild")
            rows = [master_snapshot_row(item, pd.read_parquet(workspace / "series" / f"{self.quality._model_token(item['model_id'])}.parquet", engine="pyarrow")) for item in snapshots]
            self.quality._atomic_parquet(workspace / "snapshots" / "models.parquet", pd.DataFrame(rows))
            final = self.quality.generation_path(self.final_name)
            if final.exists() and workspace != final:
                # A terminal prior publication is reusable only if it is exactly
                # this checkpoint's generation; otherwise never overwrite it.
                raise ValueError("Quality rebuild generation already exists")
            if workspace != final:
                os.replace(self.stage, final)
            self.quality.publish_generation(self.final_name)
            # Only the atomically published generation can clear source dirt.
            for model_id in model_ids:
                self.quality.mark_model_clean(model_id)
            checkpoint.update(status="completed", completed_at=utc_now(), current_model=None)
            self._persist(checkpoint)
            return {
                "job_type": "production_quality_rebuild",
                "target_generation": self.final_name,
                "total_models": len(model_ids),
                "completed_models": len(completed),
                "failed_models": checkpoint["failed_models"],
                "elapsed_seconds": perf_counter() - started,
                "quality_metrics_version": QUALITY_METRICS_VERSION,
                "result_files": [CHECKPOINT_NAME],
            }
        except CancellationRequested:
            checkpoint.update(status="cancelled", current_model=None)
            self._persist(checkpoint)
            raise
        except Exception:
            current = checkpoint.get("current_model")
            if current and current not in checkpoint["failed_models"]:
                checkpoint["failed_models"].append(current)
            checkpoint["status"] = "failed"
            self._persist(checkpoint)
            raise
