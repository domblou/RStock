"""One-shot, auditable Phase-9 migration for Production quality.

The operational CSV sources are read-only.  This module only orchestrates the
contracts introduced by phases 1-8 and writes derived quality artifacts,
reports, and a normal ``PRODUCTION_QUALITY_REBUILD`` run.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import tempfile
import time
import tracemalloc
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

import pandas as pd

from .domain import JobStatus, JobType
from .production_quality import (
    BASELINE_COMPARISON_VERSION,
    DRAW_DOWN_POLICY_VERSION,
    LIVE_PREDICTION_ORIGINS,
    NOTIONAL_POLICY_VERSION,
    OBSERVATION_SCHEMA_VERSION,
    QUALITY_METRICS_VERSION,
    WINDOW_POLICY_VERSION,
    EvaluationStatus,
    PredictionOrigin,
    build_quality_observations,
    canonical_observation_id,
    compute_model_quality,
    resolve_prediction_origin,
    validate_quality_observations,
)
from .production_quality_baseline import (
    BASELINE_METRICS_VERSION,
    BASELINE_SCHEMA_VERSION,
    LINEAGE_SCHEMA_VERSION,
    PromotionQualityService,
)
from .production_quality_rebuild import ProductionQualityRebuildRunner
from .production_quality_repository import (
    ProductionQualityRepository,
    reconcile_quality_observations,
)
from .production_quality_ui import (
    filter_quality_models,
    global_quality_kpis,
    load_model_quality_detail,
    load_models_master,
    models_grid,
)
from .production_repository import ProductionRepository
from .repository import RunRepository
from .services import default_experiment_spec


SOURCE_TABLES = ("predictions", "signals", "realized_results")
PHASE9_SCHEMA_VERSION = 1


def _now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _as_of_session() -> pd.Timestamp:
    """Return a timezone-naive UTC date accepted by exchange_calendars."""
    return pd.Timestamp.now(tz="UTC").tz_localize(None).normalize()


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, ensure_ascii=False, sort_keys=True, default=str)
        + "\n",
        encoding="utf-8",
    )
    os.replace(temporary, path)


def _inventory(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {"exists": False, "files": [], "total_bytes": 0}
    files = []
    for item in sorted(value for value in path.rglob("*") if value.is_file()):
        files.append({
            "path": str(item.relative_to(path)).replace("\\", "/"),
            "size": item.stat().st_size,
            "sha256": _sha256(item),
        })
    return {
        "exists": True,
        "files": files,
        "total_bytes": sum(item["size"] for item in files),
    }


def _source_paths(project_root: Path) -> dict[str, Path]:
    production = Path(project_root) / "production"
    return {name: production / "history" / f"{name}.csv" for name in SOURCE_TABLES}


def _source_digests(project_root: Path) -> dict[str, str]:
    paths = _source_paths(project_root)
    missing = [str(path) for path in paths.values() if not path.exists()]
    if missing:
        raise FileNotFoundError(f"Missing operational sources: {missing}")
    return {name: _sha256(path) for name, path in paths.items()}


def _date_range(frame: pd.DataFrame, column: str) -> dict[str, str | None]:
    if column not in frame or frame.empty:
        return {"minimum": None, "maximum": None}
    values = pd.to_datetime(frame[column], errors="coerce", utc=True)
    return {
        "minimum": None if values.isna().all() else values.min().isoformat(),
        "maximum": None if values.isna().all() else values.max().isoformat(),
    }


def _read_sources(project_root: Path) -> dict[str, pd.DataFrame]:
    repository = ProductionRepository(project_root)
    return {name: repository.read_table(name) for name in SOURCE_TABLES}


def preflight(project_root: Path) -> dict[str, Any]:
    production = ProductionRepository(project_root)
    quality = ProductionQualityRepository(project_root)
    models = production.models()
    sources = _read_sources(project_root)
    return {
        "phase9_schema_version": PHASE9_SCHEMA_VERSION,
        "generated_at": _now(),
        "models": {
            "total": len(models),
            "by_status": dict(sorted(Counter(model.status.value for model in models).items())),
        },
        "operational_sources": {
            name: {
                "rows": len(frame),
                "models": int(frame["model_id"].nunique()) if "model_id" in frame else 0,
                "prediction_dates": _date_range(frame, "prediction_date"),
                "recorded_at": _date_range(frame, "recorded_at"),
                "created_at": _date_range(frame, "created_at"),
                "sha256": _source_digests(project_root)[name],
            }
            for name, frame in sources.items()
        },
        "quality": _inventory(quality.root),
        "quality_components": {
            "observations": _inventory(quality.observations_root),
            "baselines": _inventory(quality.baselines_root),
            "lineage": _inventory(quality.lineage_root),
            "series": _inventory(quality.series_root),
            "snapshots": _inventory(quality.snapshots_root),
            "generations": _inventory(quality.generations_root),
            "current_generation": quality.current_generation_path.exists(),
        },
        "versions": {
            "observation_schema": OBSERVATION_SCHEMA_VERSION,
            "baseline_schema": BASELINE_SCHEMA_VERSION,
            "baseline_metrics": BASELINE_METRICS_VERSION,
            "lineage_schema": LINEAGE_SCHEMA_VERSION,
            "quality_metrics": QUALITY_METRICS_VERSION,
            "window_policy": WINDOW_POLICY_VERSION,
            "notional_policy": NOTIONAL_POLICY_VERSION,
            "drawdown_policy": DRAW_DOWN_POLICY_VERSION,
            "baseline_comparison": BASELINE_COMPARISON_VERSION,
        },
    }


def _train_ends(models: list[Any]) -> dict[str, object]:
    return {
        model.model_id: model.training_metadata.get("train_end")
        for model in models
    }


def dry_run(project_root: Path) -> tuple[dict[str, Any], pd.DataFrame]:
    production = ProductionRepository(project_root)
    models = production.models()
    sources = _read_sources(project_root)
    predictions = sources["predictions"]
    train_ends = _train_ends(models)
    classified = []
    for row in predictions.to_dict("records"):
        model_id = str(row.get("model_id"))
        inference = resolve_prediction_origin(row, train_end=train_ends.get(model_id))
        classified.append({
            "prediction_id": row.get("prediction_id"),
            "model_id": model_id,
            "prediction_date": row.get("prediction_date"),
            "origin": inference.origin.value,
            "reason": inference.reason,
        })
    classification = pd.DataFrame(classified)
    observations = build_quality_observations(
        predictions,
        sources["signals"],
        sources["realized_results"],
        pd.DataFrame(columns=["prediction_id", "evaluation_status", "exclusion_reason"]),
        train_end_by_model=train_ends,
        ingested_at=_now(),
    )
    counts = classification["origin"].value_counts().to_dict()
    allowed = {item.value for item in PredictionOrigin}
    issues: list[str] = []
    if len(classification) != len(predictions):
        issues.append("classification_count_mismatch")
    if not set(counts) <= allowed:
        issues.append("invalid_prediction_origin")
    duplicate_source = predictions.duplicated(
        ["model_id", "model_version", "prediction_id"], keep=False
    )
    if duplicate_source.any():
        issues.append("duplicate_canonical_source_key")
    prediction_ids = set(predictions["prediction_id"].dropna().astype(str))
    for name in ("signals", "realized_results"):
        foreign = set(sources[name]["prediction_id"].dropna().astype(str)) - prediction_ids
        if foreign:
            issues.append(f"{name}_without_prediction:{len(foreign)}")
    report = {
        "generated_at": _now(),
        "total": len(classification),
        "counts": {origin: int(counts.get(origin, 0)) for origin in sorted(allowed)},
        "by_model": classification.groupby(["model_id", "origin"]).size().unstack(fill_value=0).to_dict("index"),
        "by_date": classification.groupby(["prediction_date", "origin"]).size().unstack(fill_value=0).to_dict("index"),
        "examples": {
            origin: classification[classification["origin"] == origin].head(5).to_dict("records")
            for origin in sorted(allowed)
        },
        "reasons": classification["reason"].value_counts().to_dict(),
        "issues": issues,
        "automatic_validation": "passed" if not issues else "failed",
    }
    return report, observations


def backup_quality(project_root: Path, backup_root: Path) -> dict[str, Any]:
    quality = Path(project_root).resolve() / "production" / "quality"
    root = Path(backup_root).resolve()
    if quality == root or quality in root.parents:
        raise ValueError("Backup must be outside production/quality")
    root.mkdir(parents=True, exist_ok=False)
    before = _inventory(quality)
    if quality.exists():
        shutil.copytree(quality, root / "quality")
    else:
        (root / "QUALITY_WAS_ABSENT").write_text("production/quality did not exist\n", encoding="utf-8")
    payload = {"created_at": _now(), "source": str(quality), "backup": str(root), "inventory": before}
    _json(root / "backup_manifest.json", payload)
    return payload


def rollback_quality(project_root: Path, backup_root: Path) -> None:
    quality = Path(project_root).resolve() / "production" / "quality"
    production = Path(project_root).resolve() / "production"
    if quality.parent != production or quality.name != "quality":
        raise ValueError("Unsafe quality rollback target")
    backup = Path(backup_root).resolve()
    if quality.exists():
        shutil.rmtree(quality)
    saved = backup / "quality"
    if saved.exists():
        shutil.copytree(saved, quality)


def _validate_observations(
    observations: pd.DataFrame, model_ids: set[str], quality: ProductionQualityRepository
) -> dict[str, Any]:
    validate_quality_observations(observations)
    key = ["model_id", "model_version", "prediction_id"]
    duplicates = int(observations.duplicated(key, keep=False).sum())
    unknown_models = sorted(set(observations["model_id"].dropna().astype(str)) - model_ids)
    origins = set(observations["prediction_origin"].dropna().astype(str))
    statuses = set(observations["evaluation_status"].dropna().astype(str))
    backfill_live = int(observations[
        observations["prediction_origin"].isin(["operational_backfill", "legacy_unknown"])
        & observations["prediction_origin"].isin(LIVE_PREDICTION_ORIGINS)
    ].shape[0])
    manifest = quality.load_manifest()
    critical = []
    if duplicates:
        critical.append(f"duplicate_canonical_keys:{duplicates}")
    if unknown_models:
        critical.append(f"unknown_models:{len(unknown_models)}")
    if not origins <= {item.value for item in PredictionOrigin}:
        critical.append("invalid_origins")
    if not statuses <= {item.value for item in EvaluationStatus}:
        critical.append("invalid_evaluation_statuses")
    if backfill_live:
        critical.append("non_live_origin_counted_live")
    partition_models = set(manifest.get("observation_generations", {}))
    dirty = set(manifest.get("dirty_model_ids", []))
    if not partition_models <= dirty:
        critical.append("dirty_set_missing_observation_partition")
    return {
        "canonical_duplicates": duplicates,
        "unknown_models": unknown_models,
        "origins": sorted(origins),
        "evaluation_statuses": sorted(statuses),
        "non_live_origins_in_live_population": backfill_live,
        "dirty_models": sorted(dirty),
        "partition_models": sorted(partition_models),
        "critical_issues": critical,
        "status": "passed" if not critical else "failed",
    }


def _lineage_report(items: list[dict[str, Any]]) -> dict[str, Any]:
    def missing(name: str) -> int:
        return sum(item.get(name) in (None, "") for item in items)
    complete_fields = (
        "source_end_to_end_run_id", "primary_universe_id",
        "predictor_prefilter_top_n", "temporal_validation_status",
    )
    complete = sum(all(item.get(name) not in (None, "") for name in complete_fields) for item in items)
    return {
        "total": len(items), "complete": complete, "partial": len(items) - complete,
        "source_end_to_end_unknown": missing("source_end_to_end_run_id"),
        "universe_unknown": missing("primary_universe_id"),
        "top_n_unknown": missing("predictor_prefilter_top_n"),
        "temporal_validation_unavailable": sum(
            item.get("temporal_validation_status") == "not_available" for item in items
        ),
    }


def _model_report(project_root: Path) -> pd.DataFrame:
    production = ProductionRepository(project_root)
    quality = ProductionQualityRepository(project_root)
    rows = []
    for model in production.models():
        observations = quality.load_observations(model.model_id)
        snapshot = quality.load_model_snapshot(model.model_id) or {}
        lineage = quality.load_lineage(model.model_id) or {}
        origin_counts = observations["prediction_origin"].value_counts().to_dict() if not observations.empty else {}
        status_counts = observations["evaluation_status"].value_counts().to_dict() if not observations.empty else {}
        row = {
            "model_id": model.model_id, "status": model.status.value,
            "target": model.target, "predictors": json.dumps(list(model.predictors)),
            "universe": lineage.get("primary_universe_name_at_promotion"),
            "source_end_to_end": lineage.get("source_end_to_end_run_id"),
            "top_n": lineage.get("predictor_prefilter_top_n"),
            "promotion": lineage.get("promotion_date"),
            "observations_live": sum(int(origin_counts.get(name, 0)) for name in LIVE_PREDICTION_ORIGINS),
            "observations_backfill": int(origin_counts.get("operational_backfill", 0)),
            "observations_unknown": int(origin_counts.get("legacy_unknown", 0)),
            "evaluated": int(status_counts.get("evaluated", 0)),
            "pending": int(status_counts.get("pending", 0)),
            "excluded": int(status_counts.get("excluded", 0)),
            "baseline_status": snapshot.get("baseline_status"),
            "last_observation": snapshot.get("last_evaluated_date"),
            "last_signal": snapshot.get("last_signal_date"),
            "ui_status": "Données insuffisantes",
        }
        for width in (20, 63, 126):
            metrics = snapshot.get(f"window_{width}", {})
            row[f"signals_{width}"] = metrics.get("signal_count")
            row[f"mean_return_{width}"] = metrics.get("mean_intraday_return")
            row[f"win_rate_{width}"] = metrics.get("win_rate")
        since = snapshot.get("since_promotion", {})
        row["pnl_since_promotion"] = since.get("pnl")
        row["max_drawdown_dollars"] = since.get("max_drawdown_dollars")
        rows.append(row)
    return pd.DataFrame(rows)


def _timed(function: Callable[[], Any]) -> tuple[float, Any]:
    started = time.perf_counter()
    value = function()
    return time.perf_counter() - started, value


def benchmarks(project_root: Path) -> dict[str, Any]:
    master_read_seconds, master = _timed(lambda: load_models_master(project_root))
    template = master.iloc[0].to_dict() if not master.empty else {}
    volumes: dict[str, Any] = {}
    for count in (30, 150, 500, 5000):
        rows = []
        for index in range(count):
            row = dict(template)
            target = f"T{index % 50}"
            row.update(
                model_id=f"synthetic_{index}", target=target,
                target_search=target.casefold(),
            )
            rows.append(row)
        frame = pd.DataFrame(rows)
        if frame.empty:
            continue
        tracemalloc.start()
        filtering, filtered = _timed(lambda: filter_quality_models(frame, query="t1"))
        columns, grid = _timed(lambda: models_grid(frame, window=63))
        page50, _ = _timed(lambda: grid.iloc[:50].copy())
        page100, _ = _timed(lambda: grid.iloc[:100].copy())
        _, peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()
        volumes[str(count)] = {
            "filter_search_seconds": filtering,
            "column_selection_seconds": columns,
            "pagination_50_seconds": page50,
            "pagination_100_seconds": page100,
            "matched": len(filtered),
            "peak_memory_bytes": peak,
        }
    detail_seconds = None
    small_recalc = long_recalc = None
    synthetic_rebuild = None
    if not master.empty:
        model_id = str(master.iloc[0]["model_id"])
        detail_seconds, detail = _timed(lambda: load_model_quality_detail(project_root, model_id))
        observations = detail.observations
        lineage, baseline = detail.lineage, detail.baseline
        small = observations.tail(min(40, len(observations)))
        small_recalc, _ = _timed(lambda: compute_model_quality(small, baseline, lineage, _as_of_session()))
        long_frame = pd.concat([observations] * 20, ignore_index=True) if not observations.empty else observations
        long_recalc, _ = _timed(lambda: compute_model_quality(long_frame, baseline, lineage, _as_of_session()))
        if not observations.empty:
            with tempfile.TemporaryDirectory(prefix="rstock-quality-benchmark-") as directory:
                synthetic_root = Path(directory)
                synthetic_quality = ProductionQualityRepository(synthetic_root)
                seed = observations.iloc[0].to_dict()
                for index in range(30):
                    synthetic_id = f"synthetic_model_{index}"
                    prediction_id = f"synthetic_prediction_{index}"
                    row = dict(seed)
                    row.update(
                        model_id=synthetic_id,
                        model_version=1,
                        prediction_id=prediction_id,
                        observation_id=canonical_observation_id(
                            synthetic_id, 1, prediction_id
                        ),
                    )
                    synthetic_quality.upsert_observations(
                        synthetic_id, pd.DataFrame([row])
                    )
                    synthetic_quality.write_lineage(
                        synthetic_id,
                        {
                            "model_id": synthetic_id,
                            "model_version": 1,
                            "target": row.get("target"),
                            "predictors": [],
                            "promotion_date": "2026-01-01",
                        },
                    )
                synthetic_runs = RunRepository(synthetic_root / "runs")
                synthetic_spec = default_experiment_spec(
                    JobType.PRODUCTION_QUALITY_REBUILD,
                    ["SPY", "QQQ"],
                    project_root=synthetic_root,
                )
                synthetic_run_id = synthetic_runs.create(synthetic_spec)
                tracemalloc.start()
                rebuild_started = time.perf_counter()
                rebuild_result = ProductionQualityRebuildRunner(
                    synthetic_runs, synthetic_run_id, synthetic_root
                ).execute(_as_of_session())
                rebuild_elapsed = time.perf_counter() - rebuild_started
                _, rebuild_peak = tracemalloc.get_traced_memory()
                tracemalloc.stop()
                synthetic_rebuild = {
                    "models": rebuild_result["completed_models"],
                    "total_seconds": rebuild_elapsed,
                    "seconds_per_model": rebuild_elapsed / 30,
                    "peak_memory_bytes": rebuild_peak,
                }
    return {
        "generated_at": _now(), "master_rows": len(master),
        "master_read_seconds": master_read_seconds, "volumes": volumes,
        "detail_read_seconds": detail_seconds,
        "model_recalculation_small_seconds": small_recalc,
        "model_recalculation_long_seconds": long_recalc,
        "synthetic_rebuild_30_models": synthetic_rebuild,
        "notes": "Synthetic transformations only; no SLA imposed.",
    }


def execute(project_root: Path) -> dict[str, Any]:
    project_root = Path(project_root).resolve()
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    report_root = project_root / "reports" / "production_quality_phase9" / stamp
    backup_root = project_root / "phase9_backups" / f"production_quality_{stamp}"
    report_root.mkdir(parents=True, exist_ok=False)
    before_digests = _source_digests(project_root)
    initial = preflight(project_root)
    _json(report_root / "01_preflight.json", initial)
    dry, observations = dry_run(project_root)
    _json(report_root / "02_dry_run.json", dry)
    if dry["automatic_validation"] != "passed":
        raise ValueError(f"Phase-9 dry-run failed: {dry['issues']}")
    backup = backup_quality(project_root, backup_root)
    _json(report_root / "03_backup.json", backup)
    quality = ProductionQualityRepository(project_root)
    production = ProductionRepository(project_root)
    runs = RunRepository(project_root / "runs")
    models = production.models()
    try:
        upserts = reconcile_quality_observations(quality, observations)
        migration = {
            model_id: {
                "added": value.added, "updated": value.updated,
                "unchanged": value.unchanged, "total": value.total,
            }
            for model_id, value in upserts.items()
        }
        _json(report_root / "04_observation_migration.json", migration)

        promotion = PromotionQualityService(project_root, runs, production)
        lineage_items = [promotion.rebuild_lineage_for_model(model.model_id) for model in models]
        baseline_items = [promotion.rebuild_baseline_for_model(model.model_id) for model in models]
        lineage = _lineage_report(lineage_items)
        baselines = dict(Counter(item.get("availability_status", "unknown") for item in baseline_items))
        _json(report_root / "05_lineage.json", {"summary": lineage, "models": lineage_items})
        _json(report_root / "06_baselines.json", {"summary": baselines, "models": baseline_items})

        validation = _validate_observations(observations, {model.model_id for model in models}, quality)
        _json(report_root / "07_pre_rebuild_validation.json", validation)
        if validation["status"] != "passed":
            raise ValueError(f"Critical pre-rebuild validation failed: {validation['critical_issues']}")
        if _source_digests(project_root) != before_digests:
            raise ValueError("Operational sources changed during Phase-9 migration")

        symbols = sorted({model.target for model in models} | {item for model in models for item in model.predictors})
        spec = default_experiment_spec(JobType.PRODUCTION_QUALITY_REBUILD, symbols, project_root=project_root)
        run_id = runs.create(spec)
        runs.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
        try:
            rebuild = ProductionQualityRebuildRunner(runs, run_id, project_root).execute(
                _as_of_session()
            )
            runs.write_json(run_id, "summary.json", rebuild)
            runs.transition(run_id, JobStatus.COMPLETED)
        except Exception as error:
            runs.transition(run_id, JobStatus.FAILED, error=str(error))
            raise
        _json(report_root / "08_rebuild.json", {"run_id": run_id, **rebuild})

        model_report = _model_report(project_root)
        model_report.to_csv(report_root / "09_models.csv", index=False)
        active = model_report[model_report["status"] == "active"].copy()
        active.to_csv(report_root / "10_active_models.csv", index=False)

        master = load_models_master(project_root)
        ui_validation = {
            "master_rows": len(master),
            "kpis_20": global_quality_kpis(master, window=20),
            "kpis_63": global_quality_kpis(master, window=63),
            "kpis_126": global_quality_kpis(master, window=126),
            "grid_columns": list(models_grid(master.head(50), window=63).columns),
            "search_target_rows": len(filter_quality_models(master, query=str(master.iloc[0]["target"]))) if not master.empty else 0,
            "health_values": sorted(master["health_label"].dropna().unique().tolist()) if not master.empty else [],
            "grid_source": "production/quality/snapshots/models.parquet",
            "scientific_recalculation": False,
        }
        _json(report_root / "11_ui_validation.json", ui_validation)
        performance = benchmarks(project_root)
        _json(report_root / "12_benchmarks.json", performance)

        final_digests = _source_digests(project_root)
        if final_digests != before_digests:
            raise ValueError("Operational sources changed before Phase-9 completion")
        result = {
            "status": "completed", "generated_at": _now(), "report_root": str(report_root),
            "backup_root": str(backup_root), "source_digests_before": before_digests,
            "source_digests_after": final_digests, "preflight": initial,
            "dry_run": dry, "lineage": lineage, "baselines": baselines,
            "validation": validation, "rebuild": {"run_id": run_id, **rebuild},
            "models": {"total": len(model_report), "active": len(active)},
            "ui_validation": ui_validation, "benchmarks": performance,
            "rollback": f"python -m rstock.application.production_quality_phase9 --rollback {backup_root}",
        }
        _json(report_root / "phase9_report.json", result)
        return result
    except Exception:
        rollback_quality(project_root, backup_root)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="RStock Production-quality Phase 9")
    parser.add_argument("--project-root", type=Path, default=Path.cwd())
    parser.add_argument("--rollback", type=Path)
    args = parser.parse_args()
    if args.rollback:
        rollback_quality(args.project_root, args.rollback)
        print(json.dumps({"status": "rolled_back", "backup": str(args.rollback)}))
        return
    result = execute(args.project_root)
    print(json.dumps({
        "status": result["status"], "report_root": result["report_root"],
        "backup_root": result["backup_root"], "run_id": result["rebuild"]["run_id"],
    }, ensure_ascii=False))


if __name__ == "__main__":
    main()
