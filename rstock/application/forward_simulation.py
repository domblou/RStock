"""Immutable point-in-time model snapshots and forward-only evaluation."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from rstock.features import (
    intraday_down_target_column, intraday_return_column, intraday_target_column, mae_column, mfe_column,
    predictor_columns,
    prepare_dataset, prepare_prediction_row,
)
from rstock.modeling import fit_booster, predict_probabilities, selected_xgboost_parameters
from rstock.persistence import load_booster
from rstock.progress import CancellationCheck, check_cancellation
from rstock.traceability import prepared_dataset_hash

from .auto_promotion import _promotion_guidance
from .domain import ExperimentSpec
from .repository import RunRepository
from .services import MarketDataService


SNAPSHOT_FILENAME = "forward_model_snapshot.json"
SNAPSHOT_DIRECTORY = "forward_model_snapshot"
OBSERVATION_CHECKPOINT_FILENAME = "forward_observations_checkpoint.csv"
EXCLUSION_CHECKPOINT_FILENAME = "forward_exclusions_checkpoint.csv"
CHECKPOINT_MANIFEST_FILENAME = "forward_checkpoint.json"
EXCLUSIONS_FILENAME = "forward_exclusions.csv"

OBSERVATION_COLUMNS = (
    "forward_simulation_run_id", "source_end_to_end_run_id", "source_model_id",
    "requested_historical_cutoff", "resolved_source_cutoff", "target", "Set",
    "direction", "session_date", "as_of_date", "prediction_probability",
    "down_probability", "decision_threshold", "signal", "outcome",
    "directional_return", "MFE", "MAE", "correct_direction", "opposite_movement",
)
EXCLUSION_COLUMNS = (
    "forward_simulation_run_id", "source_end_to_end_run_id", "source_model_id",
    "target", "Set", "direction", "session_date", "as_of_date",
    "exclusion_reason", "invalid_fields",
)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _read_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"Invalid JSON artifact: {path.name}")
    return value


def validate_forward_snapshot(
    repository: RunRepository, spec: ExperimentSpec, *, require_expected_hash: bool = True
) -> dict[str, Any]:
    """Validate the immutable source snapshot required by a Forward run."""

    if not spec.source_end_to_end_run:
        raise ValueError("forward_simulation requires source_end_to_end_run")
    expected = spec.source_forward_model_snapshot_sha256
    if require_expected_hash and not expected:
        raise ValueError("forward_snapshot_sha256_missing")
    path = (
        repository.run_directory(spec.source_end_to_end_run)
        / "results"
        / SNAPSHOT_FILENAME
    )
    if not path.is_file():
        raise ValueError("forward_snapshot_missing")
    actual = _sha256(path)
    if expected and actual != expected:
        raise ValueError("forward_snapshot_sha256_mismatch")
    snapshot = _read_json(path)
    if snapshot.get("source_end_to_end_run_id") != spec.source_end_to_end_run:
        raise ValueError("forward_snapshot_source_mismatch")
    for model in snapshot.get("models", []):
        if not isinstance(model, dict) or not model.get("source_model_id"):
            raise ValueError("forward_snapshot_model_invalid")
        directory = path.parent / SNAPSHOT_DIRECTORY / str(model["source_model_id"])
        if not all((directory / name).is_file() for name in ("up.ubj", "down.ubj", "metadata.json")):
            raise ValueError("forward_snapshot_model_artifact_missing")
    return snapshot


def validate_forward_checkpoint(
    checkpoint_path: Path, spec: ExperimentSpec, snapshot: dict[str, Any],
    *, run_id: str,
) -> None:
    """Reject a partial Forward checkpoint from another run or snapshot."""

    if not checkpoint_path.exists():
        return
    try:
        checkpoint = pd.read_csv(checkpoint_path)
    except Exception as error:
        raise ValueError("forward_checkpoint_invalid") from error
    required = {
        "forward_simulation_run_id", "source_end_to_end_run_id",
        "source_model_id", "resolved_source_cutoff",
    }
    if not required.issubset(checkpoint.columns):
        raise ValueError("forward_checkpoint_invalid")
    if checkpoint.empty:
        return
    if not checkpoint["forward_simulation_run_id"].astype(str).eq(run_id).all():
        raise ValueError("forward_checkpoint_run_mismatch")
    if not checkpoint["source_end_to_end_run_id"].astype(str).eq(spec.source_end_to_end_run).all():
        raise ValueError("forward_checkpoint_source_mismatch")
    if not checkpoint["resolved_source_cutoff"].astype(str).eq(
        str(snapshot["resolved_market_session_cutoff"])
    ).all():
        raise ValueError("forward_checkpoint_cutoff_mismatch")
    model_ids = {str(model["source_model_id"]) for model in snapshot.get("models", [])}
    if not checkpoint["source_model_id"].astype(str).isin(model_ids).all():
        raise ValueError("forward_checkpoint_model_mismatch")


def _write_csv_atomic(frame: pd.DataFrame, destination: Path) -> None:
    temporary = destination.with_suffix(destination.suffix + ".tmp")
    frame.to_csv(temporary, index=False)
    temporary.replace(destination)


def _write_json_atomic(value: dict[str, Any], destination: Path) -> None:
    temporary = destination.with_suffix(destination.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")
    temporary.replace(destination)


def _read_checkpoint_rows(path: Path, columns: tuple[str, ...]) -> list[dict[str, Any]]:
    if not path.is_file():
        return []
    try:
        frame = pd.read_csv(path)
    except Exception as error:
        raise ValueError("forward_checkpoint_invalid") from error
    if not set(columns).issubset(frame.columns):
        raise ValueError("forward_checkpoint_invalid")
    return frame.loc[:, list(columns)].to_dict("records")


def _target_evaluation_validity(
    prices: pd.DataFrame, returns: pd.Series, target: str
) -> tuple[str | None, list[str]]:
    """Apply the complete-OHLC convention used by realized production results."""

    fields = ("Open", "High", "Low", "Close")
    ohlc = pd.to_numeric(
        pd.Series({field: prices.get(f"{target}.{field}") for field in fields}),
        errors="coerce",
    )
    invalid_fields = [
        field for field, value in ohlc.items() if pd.isna(value) or not np.isfinite(value)
    ]
    if invalid_fields:
        return "missing_target_ohlc", invalid_fields
    metric_columns = (
        intraday_return_column(target), intraday_target_column(target),
        mfe_column(target), mae_column(target),
    )
    metrics = pd.to_numeric(returns.reindex(metric_columns), errors="coerce")
    if metrics.isna().any() or not np.isfinite(metrics.to_numpy()).all():
        return "non_finite_target_metric", [
            name for name, value in metrics.items() if pd.isna(value) or not np.isfinite(value)
        ]
    return None, []


def validate_forward_checkpoint_bundle(
    output: Path, spec: ExperimentSpec, snapshot: dict[str, Any], *, run_id: str
) -> None:
    """Validate every persisted Forward checkpoint artifact before reuse."""

    validate_forward_checkpoint(
        output / OBSERVATION_CHECKPOINT_FILENAME, spec, snapshot, run_id=run_id
    )
    exclusions = _read_checkpoint_rows(
        output / EXCLUSION_CHECKPOINT_FILENAME, EXCLUSION_COLUMNS
    )
    model_ids = {str(item["source_model_id"]) for item in snapshot.get("models", [])}
    if any(
        str(row["forward_simulation_run_id"]) != run_id
        or str(row["source_end_to_end_run_id"]) != spec.source_end_to_end_run
        or not row.get("exclusion_reason")
        or str(row["source_model_id"]) not in model_ids
        for row in exclusions
    ):
        raise ValueError("forward_checkpoint_source_mismatch")
    manifest_path = output / CHECKPOINT_MANIFEST_FILENAME
    if not manifest_path.is_file():
        return
    manifest = _read_json(manifest_path)
    completed = {str(item) for item in manifest.get("completed_model_ids", [])}
    if (
        manifest.get("forward_simulation_run_id") != run_id
        or manifest.get("source_end_to_end_run_id") != spec.source_end_to_end_run
        or manifest.get("resolved_source_cutoff") != snapshot.get("resolved_market_session_cutoff")
        or not completed.issubset(model_ids)
    ):
        raise ValueError("forward_checkpoint_manifest_mismatch")


def _stage_ids(repository: RunRepository, root_run_id: str) -> dict[str, str]:
    manifest = _read_json(repository.run_directory(root_run_id) / "orchestration" / "pipeline.json")
    return {
        str(item["stage_key"]): str(item["child_run_id"])
        for item in manifest.get("stages", [])
        if isinstance(item, dict) and item.get("child_run_id")
    }


def build_forward_model_snapshot(
    repository: RunRepository, root_run_id: str, spec: ExperimentSpec,
    *, result_directory: Path | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> dict[str, Any]:
    """Persist boosters fitted once on discovery data ending at the cutoff."""

    cutoff_text = spec.resolved_market_session_cutoff or spec.historical_data_cutoff
    if cutoff_text is None:
        raise ValueError("Forward model snapshot requires a resolved historical cutoff")
    cutoff = pd.Timestamp(cutoff_text).normalize()
    root = repository.run_directory(root_run_id)
    result_dir = result_directory or root / "results"
    manifest_path = result_dir / SNAPSHOT_FILENAME
    if manifest_path.is_file():
        snapshot = _read_json(manifest_path)
        if all(
            (result_dir / SNAPSHOT_DIRECTORY / str(model.get("source_model_id")) / name).is_file()
            for model in snapshot.get("models", [])
            for name in ("up.ubj", "down.ubj", "metadata.json")
        ):
            snapshot["snapshot_sha256"] = _sha256(manifest_path)
            return snapshot
        raise ValueError("Incomplete forward model snapshot")
    stages = _stage_ids(repository, root_run_id)
    try:
        xgb_id = stages["xgboost_calibration"]
        threshold_id = stages["threshold_calibration"]
    except KeyError as error:
        raise ValueError("Forward model snapshot requires completed calibration stages") from error
    threshold_results = repository.run_directory(threshold_id) / "results"
    selected_thresholds = _read_json(threshold_results / "selected_thresholds_by_set.json")
    source_config = _read_json(root / "config.json").get("rstock_config", {})
    guidance = _promotion_guidance(threshold_results, selected_thresholds, source_config)
    candidates = guidance[guidance.get("Statut promotion", pd.Series(dtype=str)).eq("Candidat")]
    selected_xgb = selected_xgboost_parameters(
        _read_json(repository.run_directory(xgb_id) / "results" / "selected_configurations.json")
    )
    downloaded, _ = MarketDataService().load(
        spec, as_of=cutoff.date(), cancellation_check=cancellation_check
    )
    prices = downloaded.prices.loc[downloaded.prices.index <= cutoff].copy()
    prepared = prepare_dataset(
        prices, downloaded.symbols, spec.config.intraday_target_threshold,
        spec.config.lag_depth, spec.config.intraday_down_threshold,
    )
    if prepared.empty or prepared.index.max() > cutoff:
        raise ValueError("Discovery dataset exceeds the resolved historical cutoff")
    model_root = result_dir / SNAPSHOT_DIRECTORY
    model_root.mkdir(parents=True, exist_ok=True)
    models: list[dict[str, Any]] = []
    for _, candidate in candidates.iterrows():
        check_cancellation(cancellation_check)
        set_name = str(candidate["Combinaison"])
        direction = str(candidate["Direction"])
        if direction != "Up":
            continue
        symbols = json.loads(set_name)
        target, predictors = str(symbols[0]), tuple(str(item) for item in symbols[1:])
        names = predictor_columns(prepared, predictors, spec.config.lag_depth, spec.config.date_feature_regex)
        up_name, down_name = intraday_target_column(target), intraday_down_target_column(target)
        training = prepared.loc[prepared.index <= cutoff, [*names, up_name, down_name]].dropna()
        if training.empty:
            raise ValueError(f"No complete discovery training data for {set_name}")
        identity = f"{root_run_id}:{set_name}:{direction}"
        model_id = hashlib.sha256(identity.encode()).hexdigest()[:20]
        directory = model_root / model_id
        directory.mkdir(exist_ok=True)
        for label, outcome, parameters in (
            ("up", up_name, selected_xgb["Up"]), ("down", down_name, selected_xgb["Down"]),
        ):
            from rstock.modeling import XGBoostParameters
            booster = fit_booster(training, names, outcome, spec.config, parameters=XGBoostParameters(**parameters))
            booster.save_model(directory / f"{label}.ubj")
        thresholds = selected_thresholds.get(set_name, {})
        up_threshold = thresholds.get("Up", {}).get("threshold")
        down_threshold = thresholds.get("Down", {}).get("threshold")
        if up_threshold is None or down_threshold is None:
            raise ValueError(f"Missing final thresholds for candidate {set_name}")
        entry = {
            "source_model_id": model_id, "set": set_name, "target": target,
            "predictors": list(predictors), "direction": direction,
            "feature_names": list(names), "lag_depth": spec.config.lag_depth,
            "xgboost_parameters": selected_xgb,
            "up_threshold": up_threshold,
            "down_threshold": down_threshold,
            "train_start": training.index.min().date().isoformat(),
            "train_end": training.index.max().date().isoformat(),
            "training_observations": len(training),
            "up_booster_sha256": _sha256(directory / "up.ubj"),
            "down_booster_sha256": _sha256(directory / "down.ubj"),
        }
        (directory / "metadata.json").write_text(json.dumps(entry, indent=2) + "\n", encoding="utf-8")
        models.append(entry)
    snapshot = {
        "schema_version": 1, "source_end_to_end_run_id": root_run_id,
        "pipeline_version": spec.pipeline_version,
        "requested_historical_cutoff": spec.requested_historical_cutoff,
        "resolved_market_session_cutoff": cutoff.date().isoformat(),
        "feature_configuration": {
            "lag_depth": spec.config.lag_depth,
            "intraday_target_threshold": spec.config.intraday_target_threshold,
            "intraday_down_threshold": spec.config.intraday_down_threshold,
            "date_feature_regex": spec.config.date_feature_regex,
        },
        "prepared_dataset_sha256": prepared_dataset_hash(prepared),
        "candidate_count": len(models), "models": models,
    }
    if result_dir == root / "results":
        repository.write_json(root_run_id, f"results/{SNAPSHOT_FILENAME}", snapshot)
    else:
        temporary = manifest_path.with_suffix(manifest_path.suffix + ".tmp")
        temporary.write_text(json.dumps(snapshot, indent=2) + "\n", encoding="utf-8")
        temporary.replace(manifest_path)
    snapshot["snapshot_sha256"] = _sha256(manifest_path)
    return snapshot


def run_forward_simulation(
    spec: ExperimentSpec, output: Path, *, cancellation_check: CancellationCheck | None = None
) -> dict[str, Any]:
    """Evaluate only persisted boosters; this function never fits a model."""

    runs = RunRepository(spec.config.project_root / "runs")
    # Historical Forward snapshots predate the persisted hash.  They remain
    # executable with their frozen spec; manual recovery requires the stronger
    # hash check above.
    snapshot = validate_forward_snapshot(runs, spec, require_expected_hash=False)
    root = runs.run_directory(spec.source_end_to_end_run)
    if not snapshot.get("models"):
        return {"job_type": "forward_simulation", "status": "skipped_no_models", "signals": 0}
    cutoff = pd.Timestamp(snapshot["resolved_market_session_cutoff"]).normalize()
    start = pd.Timestamp(spec.forward_simulation_start_date).normalize()
    end = pd.Timestamp(spec.forward_simulation_end_date).normalize()
    if start <= cutoff or end < start:
        raise ValueError("Forward period must be strictly after the source cutoff")
    downloaded, _ = MarketDataService().load(spec, as_of=end.date(), cancellation_check=cancellation_check)
    prepared = prepare_dataset(downloaded.prices, downloaded.symbols, spec.config.intraday_target_threshold, spec.config.lag_depth, spec.config.intraday_down_threshold)
    if prepared.empty or prepared.index.max() < end:
        raise ValueError("insufficient_forward_market_data")
    checkpoint_path = output / OBSERVATION_CHECKPOINT_FILENAME
    validate_forward_checkpoint_bundle(output, spec, snapshot, run_id=output.parent.name)
    rows = _read_checkpoint_rows(checkpoint_path, OBSERVATION_COLUMNS)
    exclusion_checkpoint_path = output / EXCLUSION_CHECKPOINT_FILENAME
    exclusions = _read_checkpoint_rows(exclusion_checkpoint_path, EXCLUSION_COLUMNS)
    completed_models = {str(row["source_model_id"]) for row in rows} | {
        str(row["source_model_id"]) for row in exclusions
    }
    checkpoint_manifest_path = output / CHECKPOINT_MANIFEST_FILENAME
    if checkpoint_manifest_path.is_file():
        checkpoint_manifest = _read_json(checkpoint_manifest_path)
        snapshot_model_ids = {str(item["source_model_id"]) for item in snapshot["models"]}
        restored_models = {str(item) for item in checkpoint_manifest.get("completed_model_ids", [])}
        if (
            checkpoint_manifest.get("forward_simulation_run_id") != output.parent.name
            or checkpoint_manifest.get("source_end_to_end_run_id") != spec.source_end_to_end_run
            or checkpoint_manifest.get("resolved_source_cutoff") != cutoff.date().isoformat()
            or not restored_models.issubset(snapshot_model_ids)
        ):
            raise ValueError("forward_checkpoint_manifest_mismatch")
        completed_models = restored_models
    for model in snapshot["models"]:
        if str(model["source_model_id"]) in completed_models:
            continue
        names = list(model["feature_names"])
        directory = root / "results" / SNAPSHOT_DIRECTORY / str(model["source_model_id"])
        up, down = load_booster(directory / "up.ubj"), load_booster(directory / "down.ubj")
        target = str(model["target"])
        for session in prepared.index[(prepared.index >= start) & (prepared.index <= end)]:
            check_cancellation(cancellation_check)
            previous = prepared.index[prepared.index < session]
            if previous.empty:
                continue
            current = prepare_prediction_row(prepared, as_of_date=previous.max(), target_date=session, lag_depth=int(model["lag_depth"]))
            if any(name not in current or current[name].isna().any() for name in names):
                continue
            returns = prepared.loc[session]
            exclusion_reason, invalid_fields = _target_evaluation_validity(
                downloaded.prices.loc[session], returns, target
            )
            if exclusion_reason is not None:
                exclusions.append({
                    "forward_simulation_run_id": output.parent.name,
                    "source_end_to_end_run_id": spec.source_end_to_end_run,
                    "source_model_id": model["source_model_id"],
                    "target": target, "Set": model["set"], "direction": model["direction"],
                    "session_date": session.date().isoformat(),
                    "as_of_date": previous.max().date().isoformat(),
                    "exclusion_reason": exclusion_reason,
                    "invalid_fields": ", ".join(invalid_fields),
                })
                continue
            up_probability = float(predict_probabilities(up, current, names)[0])
            down_probability = float(predict_probabilities(down, current, names)[0])
            signal = up_probability >= float(model["up_threshold"]) and down_probability < float(model["down_threshold"])
            value = float(returns[intraday_return_column(target)])
            rows.append({"forward_simulation_run_id": output.parent.name, "source_end_to_end_run_id": spec.source_end_to_end_run, "source_model_id": model["source_model_id"], "requested_historical_cutoff": snapshot.get("requested_historical_cutoff"), "resolved_source_cutoff": cutoff.date().isoformat(), "target": target, "Set": model["set"], "direction": model["direction"], "session_date": session.date().isoformat(), "as_of_date": previous.max().date().isoformat(), "prediction_probability": up_probability, "down_probability": down_probability, "decision_threshold": model["up_threshold"], "signal": signal, "outcome": int(returns[intraday_target_column(target)]), "directional_return": value, "MFE": float(returns[mfe_column(target)]), "MAE": float(returns[mae_column(target)]), "correct_direction": bool(value >= spec.config.intraday_target_threshold), "opposite_movement": bool(value <= -spec.config.intraday_down_threshold)})
        _write_csv_atomic(pd.DataFrame(rows, columns=OBSERVATION_COLUMNS), checkpoint_path)
        _write_csv_atomic(pd.DataFrame(exclusions, columns=EXCLUSION_COLUMNS), exclusion_checkpoint_path)
        completed_models.add(str(model["source_model_id"]))
        _write_json_atomic({
            "schema_version": 1, "forward_simulation_run_id": output.parent.name,
            "source_end_to_end_run_id": spec.source_end_to_end_run,
            "resolved_source_cutoff": cutoff.date().isoformat(),
            "completed_model_ids": sorted(completed_models),
        }, checkpoint_manifest_path)
    frame = pd.DataFrame(rows, columns=OBSERVATION_COLUMNS)
    exclusion_frame = pd.DataFrame(exclusions, columns=EXCLUSION_COLUMNS)
    output.mkdir(parents=True, exist_ok=True)
    _write_csv_atomic(frame, output / "forward_observations.csv")
    _write_csv_atomic(exclusion_frame, output / EXCLUSIONS_FILENAME)
    signals = frame[frame.get("signal", pd.Series(dtype=bool)).astype(bool)] if not frame.empty else frame
    notional_per_signal = 10_000.0
    unique_issues = exclusion_frame.drop_duplicates(
        ["target", "session_date", "exclusion_reason", "invalid_fields"]
    )
    population = len(frame) + len(exclusion_frame)
    summary = {"job_type": "forward_simulation", "source_end_to_end_run_id": spec.source_end_to_end_run, "source_model_count": len(snapshot["models"]), "models_with_signals": int(signals["source_model_id"].nunique()) if not signals.empty else 0, "total_signals": len(signals), "evaluated_observations": len(frame), "skipped_observations": len(exclusion_frame), "evaluability_rate": None if population == 0 else len(frame) / population, "unique_data_quality_issues": len(unique_issues), "skipped_missing_target_ohlc": int(exclusion_frame["exclusion_reason"].eq("missing_target_ohlc").sum()), "precision": float(signals["correct_direction"].mean()) if not signals.empty else None, "directional_return_mean": float(signals["directional_return"].mean()) if not signals.empty else None, "opposite_movement_frequency": float(signals["opposite_movement"].mean()) if not signals.empty else None, "notional_per_signal": notional_per_signal, "cumulative_profit_loss": float((signals["directional_return"] * notional_per_signal).sum()) if not signals.empty else 0.0, "first_session": None if frame.empty else str(frame["session_date"].min()), "last_session": None if frame.empty else str(frame["session_date"].max()), "sessions": int(frame["session_date"].nunique()) if not frame.empty else 0}
    (output / "forward_summary.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    return summary
