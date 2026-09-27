"""Operational phase-5 bridge for the derived Production-quality layer."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from time import perf_counter
from typing import Any

import pandas as pd

from rstock.progress import CancellationCheck, check_cancellation

from .production_quality import (
    build_quality_observations,
    canonical_observation_id,
    canonical_set_id,
    compute_model_quality,
)
from .production_quality_repository import ProductionQualityRepository
from .production_repository import ProductionRepository


_PREDICTION_FIELDS = (
    "prediction_date", "as_of_date", "target", "status", "signal_status",
    "up_probability", "down_probability", "up_threshold", "down_threshold",
    "created_at",
)
_RESULT_FIELDS = (
    "result_id", "open", "high", "low", "close", "intraday_return",
    "mfe", "mae", "up_target", "down_target", "recorded_at",
)


def _fingerprint_value(value: object) -> str | None:
    if value is None or (not isinstance(value, (list, dict)) and pd.isna(value)):
        return None
    return str(value)


def _source_fingerprint(
    prediction: dict[str, Any], signal: dict[str, Any],
    result: dict[str, Any], evaluation: dict[str, Any],
) -> str:
    """Hash only source fields used by the canonical quality observation."""

    payload = {
        "prediction": {name: _fingerprint_value(prediction.get(name)) for name in _PREDICTION_FIELDS},
        "set_id": canonical_set_id(prediction.get("target"), prediction.get("predictors")),
        "signal": {
            "signal_id": _fingerprint_value(signal.get("signal_id")),
            "category": _fingerprint_value(signal.get("category")),
        },
        "result": {name: _fingerprint_value(result.get(name)) for name in _RESULT_FIELDS},
        "evaluation": {
            name: _fingerprint_value(evaluation.get(name))
            for name in ("evaluation_status", "exclusion_reason")
        },
    }
    return hashlib.sha256(
        json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()


def synchronize_production_quality(
    project_root: Path,
    *,
    candidate_predictions: pd.DataFrame,
    candidate_signals: pd.DataFrame | None = None,
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
    quality.reconcile_manifest()
    train_ends = {
        (model.model_id, str(model.artifact_version)): model.training_metadata.get("train_end")
        for model in production.models()
        if model.artifact_version is not None
    }
    ids = set(candidate_predictions.get("prediction_id", pd.Series(dtype=str)).dropna().astype(str))
    if candidate_signals is not None:
        ids.update(candidate_signals.get("prediction_id", pd.Series(dtype=str)).dropna().astype(str))
    ids.update(new_results.get("prediction_id", pd.Series(dtype=str)).dropna().astype(str))
    counts = {"new": 0, "modified": 0, "unchanged": 0, "unindexed": 0, "resumed": 0}
    source_rows_read = 0
    if ids:
        predictions = production.read_table("predictions")
        signals = production.read_table("signals")
        results = production.read_table("realized_results")
        source_rows_read = len(predictions) + len(signals) + len(results)
        predictions = predictions[predictions.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        signals = signals[signals.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        results = results[results.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)]
        selected_evaluations = evaluations[
            evaluations.get("prediction_id", pd.Series(dtype=str)).astype(str).isin(ids)
        ]
        signal_lookup = {str(row["prediction_id"]): row for row in signals.to_dict("records")}
        result_lookup = {str(row["prediction_id"]): row for row in results.to_dict("records")}
        evaluation_lookup = {
            str(row["prediction_id"]): row for row in selected_evaluations.to_dict("records")
        }
        observation_generations = quality.load_manifest()["observation_generations"]
        for model_id, model_predictions in predictions.groupby("model_id", sort=True, dropna=False):
            check_cancellation(cancellation_check)
            if pd.isna(model_id):
                continue
            model_id = str(model_id)
            known, indexed_generation = quality.load_source_fingerprint_state(model_id)
            reconcile_partition = (
                indexed_generation != observation_generations.get(model_id)
            )
            pending = []
            fingerprints = {}
            missing_keys = {}
            for prediction in model_predictions.to_dict("records"):
                prediction_id = str(prediction["prediction_id"])
                key = canonical_observation_id(
                    model_id, prediction.get("model_version"), prediction_id
                )
                fingerprint = _source_fingerprint(
                    prediction, signal_lookup.get(prediction_id, {}),
                    result_lookup.get(prediction_id, {}),
                    evaluation_lookup.get(prediction_id, {}),
                )
                if known.get(key) == fingerprint and not reconcile_partition:
                    counts["unchanged"] += 1
                    continue
                pending.append(prediction_id)
                fingerprints[key] = fingerprint
                if key not in known:
                    missing_keys[key] = prediction_id
                elif known[key] == fingerprint:
                    counts["resumed"] += 1
                else:
                    counts["modified"] += 1
            if not pending:
                continue
            if missing_keys:
                existing_ids = set(quality.load_observations(model_id)["observation_id"].dropna().astype(str))
                for key in missing_keys:
                    counts["unindexed" if key in existing_ids else "new"] += 1
            chosen = model_predictions[
                model_predictions["prediction_id"].astype(str).isin(pending)
            ]
            observations = build_quality_observations(
                chosen, signals[signals["prediction_id"].astype(str).isin(pending)] if not signals.empty else signals,
                results[results["prediction_id"].astype(str).isin(pending)] if not results.empty else results,
                selected_evaluations[
                    selected_evaluations["prediction_id"].astype(str).isin(pending)
                ] if not selected_evaluations.empty else selected_evaluations,
                train_end_by_model=train_ends,
            )
            quality.upsert_observations(model_id, observations)
            generation = quality.load_manifest()["observation_generations"][model_id]
            quality.record_source_fingerprints(
                model_id, fingerprints, expected_observation_generation=generation,
            )
    manifest = quality.reconcile_published_models()
    dirty = list(manifest.get("dirty_model_ids", []))
    updates: dict[str, tuple[dict[str, Any], pd.DataFrame]] = {}
    for model_id in dirty:
        check_cancellation(cancellation_check)
        snapshot, series = compute_model_quality(
            quality.load_observations(str(model_id)),
            quality.load_baseline(str(model_id)),
            quality.load_lineage(str(model_id)),
            as_of_session,
        )
        snapshot["model_id"] = str(model_id)
        updates[str(model_id)] = (snapshot, series)
    check_cancellation(cancellation_check)
    reconciliation_seconds = perf_counter() - started
    publication_started = perf_counter()
    generation = quality.publish_model_quality_batch(
        updates,
        expected_observation_generations=manifest.get("observation_generations", {}),
    )
    publication_seconds = perf_counter() - publication_started
    for model_id in dirty:
        quality.mark_model_clean(
            str(model_id),
            expected_observation_generation=manifest.get("observation_generations", {}).get(str(model_id)),
        )
    return {
        "dirty_detected": len(dirty),
        "models_processed": [str(model_id) for model_id in dirty],
        "models_remaining": len(quality.load_manifest().get("dirty_model_ids", [])),
        "elapsed_seconds": perf_counter() - started,
        "observation_candidates": len(ids),
        "source_rows_read": source_rows_read,
        "events_read": (
            len(candidate_predictions)
            + (len(candidate_signals) if candidate_signals is not None else 0)
            + len(new_results)
        ),
        "identities_new": counts["new"],
        "identities_modified": counts["modified"],
        "identities_unindexed": counts["unindexed"],
        "identities_resumed": counts["resumed"],
        "identities_unchanged": counts["unchanged"],
        "publication_seconds": publication_seconds,
        "reconciliation_seconds": reconciliation_seconds,
        "models_rebuilt": len(dirty),
        "published_generation": generation,
    }
