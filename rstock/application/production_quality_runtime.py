"""Operational phase-5 bridge for the derived Production-quality layer."""

from __future__ import annotations

from pathlib import Path
from time import perf_counter
from typing import Any, Callable

import pandas as pd

from rstock.progress import CancellationCheck, check_cancellation

from .production_quality import (
    ProductionQualityMetricsService,
    build_quality_observations,
)
from .production_quality_repository import ProductionQualityRepository
from .production_repository import ProductionRepository


def synchronize_production_quality(
    project_root: Path,
    *,
    candidate_predictions: pd.DataFrame,
    new_results: pd.DataFrame,
    evaluations: pd.DataFrame,
    as_of_session: object,
    cancellation_check: CancellationCheck | None = None,
) -> dict[str, Any]:
    """Consume only newly published/corrected operational identities.

    Existing historical predictions are intentionally not migrated here. A late
    realized result brings its old prediction identity back into this bounded
    population, so corrections remain detectable without date watermarks.
    """
    started = perf_counter()
    production = ProductionRepository(project_root)
    quality = ProductionQualityRepository(project_root)
    ids = set(candidate_predictions.get("prediction_id", pd.Series(dtype=str)).dropna().astype(str))
    ids.update(new_results.get("prediction_id", pd.Series(dtype=str)).dropna().astype(str))
    if ids:
        predictions = production.read_table("predictions")
        predictions = predictions[predictions.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        signals = production.read_table("signals")
        signals = signals[signals.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        results = production.read_table("realized_results")
        results = results[results.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        selected_evaluations = evaluations[
            evaluations.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)
        ]
        observations = build_quality_observations(
            predictions, signals, results, selected_evaluations
        )
        for model_id, group in observations.groupby("model_id", sort=True, dropna=False):
            check_cancellation(cancellation_check)
            if pd.isna(model_id):
                continue
            quality.upsert_observations(str(model_id), group)
    manifest = quality.load_manifest()
    dirty = list(manifest.get("dirty_model_ids", []))
    metrics = ProductionQualityMetricsService(quality)
    completed: list[str] = []
    for model_id in dirty:
        check_cancellation(cancellation_check)
        metrics.rebuild_model(str(model_id), as_of_session)
        completed.append(str(model_id))
    return {
        "dirty_detected": len(dirty),
        "models_processed": completed,
        "models_remaining": len(quality.load_manifest().get("dirty_model_ids", [])),
        "elapsed_seconds": perf_counter() - started,
        "observation_candidates": len(ids),
    }
