"""Repair downgraded legacy origins from immutable operational predictions."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from rstock.application.production_quality import (
    PredictionOrigin,
    compute_model_quality,
    resolve_prediction_origin,
)
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_quality_runtime import synchronize_production_quality
from rstock.application.production_repository import ProductionRepository


def _source_hash(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _version(value: object) -> str:
    return "" if pd.isna(value) else str(int(value))


def repair(project_root: Path, *, apply: bool = False) -> dict[str, object]:
    root = Path(project_root).resolve()
    production = ProductionRepository(root)
    quality = ProductionQualityRepository(root)
    source_path = production.history_root / "predictions.csv"
    source_digest = _source_hash(source_path)
    predictions = production.read_table("predictions")
    models = {model.model_id: model for model in production.models()}
    initial_manifest = quality.load_manifest()
    if initial_manifest.get("dirty_model_ids"):
        raise ValueError("Quality has pending dirty models; complete or reconcile that work first")
    source = {
        (str(row.get("model_id")), _version(row.get("model_version")), str(row.get("prediction_id"))): row
        for row in predictions.to_dict("records")
    }
    changes: dict[str, pd.DataFrame] = {}
    origin_counts: Counter[str] = Counter()
    before_after: dict[str, dict[str, object]] = {}
    skipped = Counter()
    for model_id in sorted(initial_manifest.get("observation_generations", {})):
        frame = quality.load_observations(model_id)
        if frame.empty:
            continue
        repaired = frame.copy()
        indices = []
        model = models.get(model_id)
        for index, row in frame.iterrows():
            if row["prediction_origin"] != PredictionOrigin.LEGACY_UNKNOWN.value:
                continue
            key = (str(model_id), _version(row["model_version"]), str(row["prediction_id"]))
            event = source.get(key)
            if event is None:
                skipped["missing_source_prediction"] += 1
                continue
            if model is None or _version(model.artifact_version) != _version(row["model_version"]):
                skipped["missing_matching_artifact_version"] += 1
                continue
            train_end = model.training_metadata.get("train_end")
            inferred = resolve_prediction_origin(event, train_end=train_end)
            if inferred.origin == PredictionOrigin.LEGACY_UNKNOWN:
                skipped["insufficient_temporal_evidence"] += 1
                continue
            repaired.at[index, "prediction_origin"] = inferred.origin.value
            repaired.at[index, "origin_inference_version"] = inferred.rule_version
            indices.append(index)
            origin_counts[inferred.origin.value] += 1
        if not indices:
            continue
        changes[model_id] = repaired.loc[indices].copy()
        snapshot = quality.load_model_snapshot(model_id) or {}
        as_of = snapshot.get("as_of_session")
        if not as_of:
            raise ValueError(f"Missing quality as-of session for {model_id}")
        prospective, _ = compute_model_quality(
            repaired, quality.load_baseline(model_id), quality.load_lineage(model_id), as_of
        )
        before_after[model_id] = {
            "target": model.target,
            "repaired_observations": len(indices),
            "before": {
                "signals_63": (snapshot.get("window_63") or {}).get("signal_count"),
                "pnl": (snapshot.get("since_promotion") or {}).get("pnl"),
            },
            "after": {
                "signals_63": prospective["window_63"]["signal_count"],
                "pnl": prospective["since_promotion"]["pnl"],
                "return_63": prospective["window_63"]["mean_intraday_return"],
                "win_rate_63": prospective["window_63"]["win_rate"],
                "drawdown": prospective["since_promotion"]["max_drawdown_dollars"],
            },
        }
    report: dict[str, object] = {
        "source_sha256": source_digest,
        "repaired_observations": sum(len(item) for item in changes.values()),
        "affected_models": len(changes),
        "origins_restored": dict(origin_counts),
        "skipped": dict(skipped),
        "models": before_after,
        "applied": False,
    }
    if not apply:
        return report
    if _source_hash(source_path) != source_digest:
        raise ValueError("Operational predictions changed during repair preview")
    if quality.load_manifest() != initial_manifest:
        raise ValueError("Quality manifest changed during repair preview")
    as_of_dates = {
        str((quality.load_model_snapshot(model_id) or {})["as_of_session"])
        for model_id in changes
    }
    if len(as_of_dates) > 1:
        raise ValueError("Affected models have different quality as-of sessions")
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    report_root = root / "reports" / "production_quality_origin_repair" / stamp
    report_root.mkdir(parents=True, exist_ok=False)
    backup = Path(shutil.make_archive(
        str(report_root / "quality_before"), "zip", root_dir=quality.root
    ))
    for model_id, rows in changes.items():
        quality.upsert_observations(model_id, rows, allow_origin_repair=True)
    if changes:
        publication = synchronize_production_quality(
            root, candidate_predictions=pd.DataFrame(), new_results=pd.DataFrame(),
            evaluations=pd.DataFrame(), as_of_session=as_of_dates.pop(),
        )
        report["publication"] = publication
    if _source_hash(source_path) != source_digest:
        raise ValueError("Operational predictions changed during repair")
    report["applied"] = True
    report["backup"] = str(backup)
    report["report_path"] = str(report_root / "result.json")
    (report_root / "result.json").write_text(
        json.dumps(report, indent=2, ensure_ascii=False, sort_keys=True, default=str) + "\n",
        encoding="utf-8",
    )
    return report


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project-root", type=Path, required=True)
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()
    print(json.dumps(repair(args.project_root, apply=args.apply), indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
