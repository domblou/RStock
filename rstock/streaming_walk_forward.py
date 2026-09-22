"""Disk-backed, resumable execution of the walk-forward scientific workflow."""

from __future__ import annotations

import json
import os
import sqlite3
import tempfile
from contextlib import closing
from dataclasses import dataclass
from pathlib import Path
from time import perf_counter
from collections.abc import Iterable
from typing import Any, Mapping

import numpy as np
import pandas as pd

from .checkpoints import CheckpointManager
from .combinations import symbol_set_id
from .config import RStockConfig
from .model_selection import model_selection_parameters, score_qualified_models
from .parallel import iter_indexed_combination_batches
from .progress import CancellationCheck, ProgressCallback, check_cancellation, report_progress
from .qualification import qualification_parameters, qualify_combinations, rank_qualified_combinations
from .risk import conditional_signal_metrics, intraday_risk_metrics
from .telemetry import dataframe_bytes, process_rss_bytes
from .walk_forward import (
    _aggregate_final_risk,
    _aggregate_predictions,
    _aggregate_risk,
    _combine_selection_results,
    _evaluate_final_holdout,
    _set_walk_forward_task_context,
    _validate_prepared_index,
    _walk_forward_combination,
    _walk_forward_process_task,
)


@dataclass(slots=True)
class StreamedWalkForwardResult:
    aggregate_global: pd.DataFrame
    qualification: pd.DataFrame
    run_configuration: dict[str, object]
    telemetry: dict[str, object]


FINAL_ARTIFACT_NAMES = {
    "windows.csv",
    "predictions.csv",
    "aggregate_by_window.csv",
    "aggregate_by_set.csv",
    "aggregate_global.csv",
    "qualification.csv",
    "final_holdout.csv",
    "final_holdout_predictions.csv",
    "selection_results.csv",
    "risk_by_window.csv",
    "risk_by_set.csv",
    "risk_global.csv",
    "final_holdout_risk.csv",
    "run_configuration.json",
}


def validate_final_artifacts(output: Path) -> None:
    """Refuse to mark result writing complete while a public artifact is absent."""

    missing = sorted(name for name in FINAL_ARTIFACT_NAMES if not (output / name).is_file())
    if missing:
        raise RuntimeError(f"Artefacts walk-forward finaux manquants : {', '.join(missing)}")


def _atomic_csv(destination: Path, frame: pd.DataFrame) -> None:
    _atomic_csv_frames(destination, [frame])


def _atomic_csv_frames(destination: Path, frames: Iterable[pd.DataFrame]) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8", newline="") as stream:
            wrote_header = False
            for frame in frames:
                frame.to_csv(stream, index=False, header=not wrote_header)
                wrote_header = True
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)


def _atomic_json(destination: Path, values: dict[str, object]) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8", newline="\n") as stream:
            json.dump(values, stream, indent=2, ensure_ascii=False, default=str)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)


def _sql_scalar(connection: sqlite3.Connection, query: str, parameters=()) -> Any:
    return connection.execute(query, parameters).fetchone()[0]


def _sql_classification_metrics(
    connection: sqlite3.Connection,
    prefix: str,
    where: str = "",
    parameters: tuple[object, ...] = (),
) -> dict[str, object]:
    target = f'"{prefix}Target"'
    prediction = f'"{prefix}Prediction"'
    probability = f'"{prefix}Probability"'
    suffix = f" WHERE {where}" if where else ""
    row = connection.execute(
        f"""
        SELECT COUNT(*),
               SUM(CASE WHEN {target}=0 AND {prediction}=0 THEN 1 ELSE 0 END),
               SUM(CASE WHEN {target}=0 AND {prediction}=1 THEN 1 ELSE 0 END),
               SUM(CASE WHEN {target}=1 AND {prediction}=0 THEN 1 ELSE 0 END),
               SUM(CASE WHEN {target}=1 AND {prediction}=1 THEN 1 ELSE 0 END),
               SUM(CASE WHEN {target}=1 THEN 1 ELSE 0 END)
        FROM predictions{suffix}
        """,
        parameters,
    ).fetchone()
    total, tn, fp, fn, tp, positives = (int(value or 0) for value in row)
    negatives = total - positives
    accuracy = (tn + tp) / total if total else 0.0
    precision = tp / (tp + fp) if tp + fp else 0.0
    recall = tp / (tp + fn) if tp + fn else 0.0
    f1 = 2 * precision * recall / (precision + recall) if precision + recall else 0.0

    auc: float | None = None
    pr_auc: float | None = None
    if positives and negatives:
        rank = 1
        positive_rank_sum = 0.0
        for _, group_count, group_positives in connection.execute(
            f"""
            SELECT {probability}, COUNT(*),
                   SUM(CASE WHEN {target}=1 THEN 1 ELSE 0 END)
            FROM predictions{suffix}
            GROUP BY {probability} ORDER BY {probability} ASC
            """,
            parameters,
        ):
            group_count = int(group_count)
            group_positives = int(group_positives or 0)
            average_rank = (rank + rank + group_count - 1) / 2.0
            positive_rank_sum += group_positives * average_rank
            rank += group_count
        auc = (
            positive_rank_sum - positives * (positives + 1) / 2.0
        ) / (positives * negatives)
    if positives:
        cumulative_positive = 0
        cumulative_total = 0
        area = 0.0
        for _, group_count, group_positives in connection.execute(
            f"""
            SELECT {probability}, COUNT(*),
                   SUM(CASE WHEN {target}=1 THEN 1 ELSE 0 END)
            FROM predictions{suffix}
            GROUP BY {probability} ORDER BY {probability} DESC
            """,
            parameters,
        ):
            group_count = int(group_count)
            group_positives = int(group_positives or 0)
            cumulative_positive += group_positives
            cumulative_total += group_count
            area += (group_positives / positives) * (
                cumulative_positive / cumulative_total
            )
        pr_auc = area
    return {
        "TN": tn,
        "FP": fp,
        "FN": fn,
        "TP": tp,
        "Accuracy": float(accuracy),
        "Precision": float(precision),
        "Recall": float(recall),
        "F1": float(f1),
        "ROCAUC": None if auc is None else float(auc),
        "PRAUC": None if pr_auc is None else float(pr_auc),
        "Prevalence": float(positives / total) if total else 0.0,
    }


def _prefixed_sql_metrics(
    connection: sqlite3.Connection,
    prefix: str,
    where: str = "",
    parameters: tuple[object, ...] = (),
) -> dict[str, object]:
    return {
        f"{prefix}{name}": value
        for name, value in _sql_classification_metrics(
            connection, prefix, where, parameters
        ).items()
    }


def _sql_aggregate_predictions(
    connection: sqlite3.Connection,
    windows: pd.DataFrame,
    window_evaluations: int,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    by_window: list[dict[str, object]] = []
    for window_number in sorted(int(value) for value in windows["Window"].unique()):
        definitions = windows[windows["Window"] == window_number]
        row = connection.execute(
            """
            SELECT COUNT(DISTINCT SetName), COUNT(*), COUNT(DISTINCT Date),
                   SUM(UpTarget), SUM(DownTarget)
            FROM predictions WHERE Window=?
            """,
            (window_number,),
        ).fetchone()
        record: dict[str, object] = {
            "Window": window_number,
            "TrainStart": definitions["TrainStart"].min(),
            "TrainEnd": definitions["TrainEnd"].max(),
            "TestStart": definitions["TestStart"].min(),
            "TestEnd": definitions["TestEnd"].max(),
            "Sets": int(row[0]),
            "TestObservations": int(row[1]),
            "UniqueTestDates": int(row[2]),
            "Predictions": int(row[1]),
            "UpPositiveOutcomes": int(row[3] or 0),
            "DownPositiveOutcomes": int(row[4] or 0),
        }
        record.update(_prefixed_sql_metrics(connection, "Up", "Window=?", (window_number,)))
        record.update(_prefixed_sql_metrics(connection, "Down", "Window=?", (window_number,)))
        by_window.append(record)

    row = connection.execute(
        """
        SELECT COUNT(DISTINCT SetName), COUNT(*), COUNT(DISTINCT Date),
               SUM(UpTarget), SUM(DownTarget)
        FROM predictions
        """
    ).fetchone()
    global_record: dict[str, object] = {
        "Sets": int(row[0]),
        "WindowEvaluations": window_evaluations,
        "TestObservations": int(row[1]),
        "UniqueTestDates": int(row[2]),
        "Predictions": int(row[1]),
        "UpPositiveOutcomes": int(row[3] or 0),
        "DownPositiveOutcomes": int(row[4] or 0),
    }
    global_record.update(_prefixed_sql_metrics(connection, "Up"))
    global_record.update(_prefixed_sql_metrics(connection, "Down"))
    return pd.DataFrame(by_window), pd.DataFrame([global_record])


def _sql_conditional_metrics(
    connection: sqlite3.Connection,
    signal_column: str,
    prefix: str,
    up_threshold: float,
    down_threshold: float,
) -> dict[str, int | float]:
    row = connection.execute(
        f"""
        SELECT COUNT(IntradayReturn), AVG(IntradayReturn),
               CAST(SUM(CASE WHEN IntradayReturn >= ? THEN 1 ELSE 0 END) AS REAL)
                   / NULLIF(COUNT(IntradayReturn), 0),
               CAST(SUM(CASE WHEN IntradayReturn <= ? THEN 1 ELSE 0 END) AS REAL)
                   / NULLIF(COUNT(IntradayReturn), 0),
               AVG(MAE), AVG(MFE),
               AVG(CASE WHEN IntradayReturn > 0 THEN IntradayReturn END),
               AVG(CASE WHEN IntradayReturn < 0 THEN IntradayReturn END)
        FROM predictions WHERE "{signal_column}"=1
        """,
        (up_threshold, -down_threshold),
    ).fetchone()
    numeric = [np.nan if value is None else float(value) for value in row[1:]]
    return {
        f"{prefix}Count": int(row[0]),
        f"{prefix}IntradayReturnMean": numeric[0],
        f"{prefix}IntradayReturnMedian": _sql_signal_median(
            connection, signal_column
        ),
        f"{prefix}UpFrequency": numeric[1],
        f"{prefix}DownFrequency": numeric[2],
        f"{prefix}MAEMean": numeric[3],
        f"{prefix}MFEMean": numeric[4],
        f"{prefix}MeanPositiveGain": numeric[5],
        f"{prefix}MeanNegativeLoss": numeric[6],
    }


def _sql_signal_median(connection: sqlite3.Connection, signal_column: str) -> float:
    count = int(_sql_scalar(
        connection,
        f'SELECT COUNT(IntradayReturn) FROM predictions WHERE "{signal_column}"=1 '
        "AND IntradayReturn IS NOT NULL",
    ))
    if not count:
        return np.nan
    offset = (count - 1) // 2
    limit = 2 if count % 2 == 0 else 1
    values = [
        float(row[0])
        for row in connection.execute(
            f'SELECT IntradayReturn FROM predictions WHERE "{signal_column}"=1 '
            "AND IntradayReturn IS NOT NULL ORDER BY IntradayReturn LIMIT ? OFFSET ?",
            (limit, offset),
        )
    ]
    return float(np.mean(values))


def _sql_global_risk(
    connection: sqlite3.Connection, config: RStockConfig
) -> pd.DataFrame:
    unique = pd.read_sql_query(
        """
        SELECT Observation, Date, MIN(IntradayReturn) AS IntradayReturn,
               MIN(MFE) AS MFE, MIN(MAE) AS MAE
        FROM predictions GROUP BY Observation, Date ORDER BY MIN(RowOrder)
        """,
        connection,
    )
    record: dict[str, object] = {
        "Sets": int(_sql_scalar(connection, "SELECT COUNT(DISTINCT SetName) FROM predictions")),
        "UniqueMarketObservations": len(unique),
    }
    record.update(intraday_risk_metrics(
        unique, config.intraday_target_threshold, config.intraday_down_threshold
    ))
    record.update(_sql_conditional_metrics(
        connection,
        "UpPrediction",
        "UpSignal",
        config.intraday_target_threshold,
        config.intraday_down_threshold,
    ))
    record.update(_sql_conditional_metrics(
        connection,
        "DownPrediction",
        "DownSignal",
        config.intraday_target_threshold,
        config.intraday_down_threshold,
    ))
    return pd.DataFrame([record])


def _insert_predictions(
    connection: sqlite3.Connection, predictions: pd.DataFrame, row_offset: int
) -> int:
    stored = predictions.copy()
    stored.insert(0, "RowOrder", np.arange(row_offset, row_offset + len(stored)))
    stored = stored.rename(columns={"Set": "SetName"})
    stored.to_sql("predictions", connection, if_exists="append", index=False)
    return row_offset + len(stored)


def _workflow_configuration(
    config: RStockConfig,
    *,
    min_train: int,
    test_window: int,
    step: int,
    holdout_size: int,
    development_end: pd.Timestamp,
    holdout_start: pd.Timestamp,
    evaluate_holdout: bool,
) -> dict[str, object]:
    values: dict[str, object] = {
        "target": "intraday_return >= intraday_target_threshold",
        "down_target": "intraday_return <= -intraday_down_threshold",
        "intraday_target_threshold": config.intraday_target_threshold,
        "intraday_down_threshold": config.intraday_down_threshold,
        "conditional_signal": "predicted class == 1 (probability > 0.5)",
        "lag_depth": config.lag_depth,
        "lag_features": [f"intraday_J-{lag}" for lag in range(1, config.lag_depth + 1)],
        "walk_forward_window_mode": config.walk_forward_window_mode,
        "walk_forward_min_train_size": min_train,
        "walk_forward_train_size": config.walk_forward_train_size,
        "walk_forward_test_size": test_window,
        "walk_forward_step_size": step,
        "combination_workers": config.combination_workers,
        "walk_forward_batch_size": config.walk_forward_batch_size,
        "final_holdout_batch_size": config.final_holdout_batch_size,
        "final_holdout_size": holdout_size,
        "development_end": development_end.isoformat(),
        "final_holdout_start": holdout_start.isoformat(),
        "qualification": qualification_parameters(config),
        "model_selection": model_selection_parameters(config),
        "ranking_order": [
            "PctWindowsAboveRandom desc",
            "ROCAUCMedian desc",
            "ROCAUCWorst desc",
            "ROCAUCStd asc",
            "PRAUCMedian desc",
        ],
    }
    if not evaluate_holdout:
        values["final_holdout_evaluated"] = False
    return values


def run_streamed_walk_forward_batch(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    checkpoint: CheckpointManager,
    *,
    market_calendars: Mapping[str, str] | None = None,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> dict[str, int]:
    """Evaluate one technical child range and persist restartable sub-batches."""

    _validate_prepared_index(prepared)
    ordered = prepared.sort_index()
    holdout_size = config.final_holdout_size
    if holdout_size < 1 or holdout_size >= len(ordered):
        raise ValueError("final_holdout_size must leave non-empty development history")
    task_rows = [row.to_dict() for _, row in generated_sets.iterrows()]
    task_context = (
        ordered,
        config,
        ordered.index[-holdout_size],
        market_calendars or {},
        config.walk_forward_min_train_size,
        config.walk_forward_test_size,
        config.walk_forward_step_size,
    )
    phase = "walk_forward"
    batch_size = config.walk_forward_batch_size
    total_batches = (len(task_rows) + batch_size - 1) // batch_size
    checkpoint.phase_started(phase)
    checkpoint.set_total_batches(phase, total_batches)
    completed = checkpoint.completed_batch_ids(phase)
    for batch in iter_indexed_combination_batches(
        task_rows,
        batch_size=batch_size,
        combination_workers=config.combination_workers,
        worker_context=task_context,
        context_initializer=_set_walk_forward_task_context,
        process_task=_walk_forward_process_task,
        serial_task=_walk_forward_combination,
        item_label=lambda values: symbol_set_id(pd.Series(values)),
        stage=phase,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details={"combination_workers": config.combination_workers},
        completed_batch_ids=completed,
    ):
        check_cancellation(cancellation_check)
        windows = pd.DataFrame(
            [record for result in batch.results for record in result.window_records]
        )
        predictions = pd.DataFrame(
            [record for result in batch.results for record in result.prediction_records]
        )
        checkpoint.commit_batch(
            phase,
            batch.batch_id,
            {"windows": windows, "predictions": predictions},
            first_index=batch.first_index,
            last_index=batch.last_index,
            combination_count=len(batch.results),
            row_counts={"windows": len(windows), "predictions": len(predictions)},
        )
    completed = checkpoint.completed_batch_ids(phase)
    if completed != tuple(range(total_batches)):
        raise RuntimeError("Le batch WF enfant n'a pas produit tous ses checkpoints.")
    checkpoint.phase_completed(phase)
    return {
        "combinations": len(task_rows),
        "internal_batches": total_batches,
    }


def _sets_from_qualification(qualification: pd.DataFrame) -> pd.DataFrame:
    rows: list[list[object]] = []
    for raw in qualification.get("Set", pd.Series(dtype=object)):
        values = json.loads(str(raw))
        if not isinstance(values, list) or len(values) < 2:
            raise ValueError("Identifiant de combinaison walk-forward invalide")
        rows.append([str(value) for value in values])
    width = max((len(row) for row in rows), default=2)
    return pd.DataFrame(
        [row + [None] * (width - len(row)) for row in rows],
        columns=[f"V{index}" for index in range(width)],
    )


def run_streamed_walk_forward(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame | None,
    config: RStockConfig,
    checkpoint: CheckpointManager,
    output: Path,
    *,
    market_calendars: Mapping[str, str] | None = None,
    evaluate_holdout: bool = True,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    run_configuration_extras: Mapping[str, object] | None = None,
    precomputed_walk_forward_batches: int | None = None,
    precomputed_combination_count: int | None = None,
) -> StreamedWalkForwardResult:
    """Execute and finalize a walk-forward without retaining all detail in RAM."""

    _validate_prepared_index(prepared)
    ordered = prepared.sort_index()
    holdout_size = config.final_holdout_size
    if holdout_size < 1 or holdout_size >= len(ordered):
        raise ValueError("final_holdout_size must leave non-empty development history")
    holdout_start = ordered.index[-holdout_size]
    development_end = ordered.index[-holdout_size - 1]
    min_train = config.walk_forward_min_train_size
    test_window = config.walk_forward_test_size
    step = config.walk_forward_step_size
    calendars = market_calendars or {}
    task_context = (
        ordered, config, holdout_start, calendars, min_train, test_window, step
    )
    task_rows = (
        [row.to_dict() for _, row in generated_sets.iterrows()]
        if generated_sets is not None
        else []
    )
    combination_count = (
        len(task_rows)
        if precomputed_combination_count is None
        else int(precomputed_combination_count)
    )
    telemetry: dict[str, object] = {
        "prepared_bytes": dataframe_bytes(ordered),
        "parent_rss_start_bytes": process_rss_bytes(),
        "combination_workers": config.combination_workers,
        "xgb_threads_per_worker": config.xgb_nthread,
        "maximum_xgb_threads": config.combination_workers * config.xgb_nthread,
        "peak_batch_prediction_rows": 0,
        "parent_rss_peak_bytes": process_rss_bytes(),
        "phase_seconds": {},
    }
    phase_seconds = telemetry["phase_seconds"]
    assert isinstance(phase_seconds, dict)

    phase = "walk_forward"
    checkpoint.phase_started(phase)
    batch_size = config.walk_forward_batch_size
    total_batches = (
        (len(task_rows) + batch_size - 1) // batch_size
        if precomputed_walk_forward_batches is None
        else int(precomputed_walk_forward_batches)
    )
    if total_batches < 1 or combination_count < 1:
        raise ValueError("Walk-forward requires at least one combination")
    checkpoint.set_total_batches(phase, total_batches)
    phase_started = perf_counter()
    completed_ids = checkpoint.completed_batch_ids(phase)
    report_progress(
        progress_callback,
        phase,
        substage="started",
        details={
            "phase_event": "started",
            "combinations": combination_count,
            "total_batches": total_batches,
            **telemetry,
        },
    )
    if precomputed_walk_forward_batches is None:
        for batch in iter_indexed_combination_batches(
            task_rows,
            batch_size=batch_size,
            combination_workers=config.combination_workers,
            worker_context=task_context,
            context_initializer=_set_walk_forward_task_context,
            process_task=_walk_forward_process_task,
            serial_task=_walk_forward_combination,
            item_label=lambda values: symbol_set_id(pd.Series(values)),
            stage=phase,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
            details={"combination_workers": config.combination_workers},
            completed_batch_ids=completed_ids,
        ):
            check_cancellation(cancellation_check)
            checkpoint_started = perf_counter()
            windows = pd.DataFrame([
                record for result in batch.results for record in result.window_records
            ])
            predictions = pd.DataFrame([
                record for result in batch.results for record in result.prediction_records
            ])
            checkpoint.commit_batch(
                phase,
                batch.batch_id,
                {"windows": windows, "predictions": predictions},
                first_index=batch.first_index,
                last_index=batch.last_index,
                combination_count=len(batch.results),
                row_counts={"windows": len(windows), "predictions": len(predictions)},
            )
            telemetry["peak_batch_prediction_rows"] = max(
                int(telemetry["peak_batch_prediction_rows"]), len(predictions)
            )
            current_rss = process_rss_bytes()
            if current_rss is not None:
                previous_peak = telemetry.get("parent_rss_peak_bytes")
                telemetry["parent_rss_peak_bytes"] = max(
                    int(previous_peak or 0), current_rss
                )
            report_progress(
                progress_callback,
                phase,
                substage=f"batch {batch.batch_id + 1}/{total_batches}",
                completed_units=batch.last_index + 1,
                total_units=len(task_rows),
                details={
                    "batch_id": batch.batch_id,
                    "batch_number": batch.batch_id + 1,
                    "total_batches": total_batches,
                    "combinations": len(batch.results),
                    "rows": len(predictions),
                    "elapsed_seconds": (
                        batch.elapsed_seconds + perf_counter() - checkpoint_started
                    ),
                    "calculation_seconds": batch.elapsed_seconds,
                    "parent_rss_bytes": current_rss,
                    "checkpoint_written": True,
                },
            )
    elif checkpoint.completed_batch_ids(phase) != tuple(range(total_batches)):
        raise RuntimeError("Les résultats des enfants WF sont incomplets.")
    checkpoint.phase_completed(phase)
    telemetry["walk_forward_seconds"] = perf_counter() - phase_started
    phase_seconds[phase] = telemetry["walk_forward_seconds"]
    completed_ids = checkpoint.completed_batch_ids(phase)
    completed_indices = set(range(combination_count))
    processed_set_ids = (
        {symbol_set_id(pd.Series(row)) for row in task_rows}
        if task_rows
        else set()
    )
    report_progress(
        progress_callback,
        phase,
        substage="completed",
        details={
            "phase_event": "completed",
            "combinations_requested": combination_count,
            "combinations_processed": len(completed_indices),
            "unique_combinations_processed": (
                len(processed_set_ids) if processed_set_ids else combination_count
            ),
            "expected_batches": total_batches,
            "completed_batches": len(completed_ids),
            "peak_batch_prediction_rows": telemetry["peak_batch_prediction_rows"],
            "phase_seconds": phase_seconds[phase],
        },
    )

    phase = "aggregation"
    if checkpoint.phase_is_completed(phase) and checkpoint.artifact_exists("aggregation"):
        aggregation = checkpoint.load_artifact("aggregation")
        if "_elapsed_seconds" in aggregation:
            phase_seconds[phase] = aggregation["_elapsed_seconds"]
    else:
        checkpoint.phase_started(phase)
        report_progress(progress_callback, phase, substage="started", details={"phase_event": "started"})
        phase_started = perf_counter()
        database = checkpoint.root / "aggregation.sqlite"
        database.unlink(missing_ok=True)
        window_definition_parts: list[pd.DataFrame] = []
        window_evaluations = 0
        aggregate_set_parts: list[pd.DataFrame] = []
        risk_window_parts: list[pd.DataFrame] = []
        risk_set_parts: list[pd.DataFrame] = []
        row_offset = 0
        with closing(sqlite3.connect(database)) as connection:
            for batch_id in range(total_batches):
                batch_started_at = perf_counter()
                check_cancellation(cancellation_check)
                payload = checkpoint.load_batch("walk_forward", batch_id)
                windows = payload["windows"]
                predictions = payload["predictions"]
                window_evaluations += len(windows)
                window_definition_parts.append(
                    windows.groupby("Window", sort=True, as_index=False).agg(
                        TrainStart=("TrainStart", "min"),
                        TrainEnd=("TrainEnd", "max"),
                        TestStart=("TestStart", "min"),
                        TestEnd=("TestEnd", "max"),
                    )
                )
                aggregate_by_set, _ = _aggregate_predictions(predictions, windows)
                risk_by_window, risk_by_set, _ = _aggregate_risk(predictions, config)
                aggregate_set_parts.append(aggregate_by_set)
                risk_window_parts.append(risk_by_window)
                risk_set_parts.append(risk_by_set)
                row_offset = _insert_predictions(connection, predictions, row_offset)
                report_progress(
                    progress_callback,
                    phase,
                    substage=f"batch {batch_id + 1}/{total_batches}",
                    completed_units=batch_id + 1,
                    total_units=total_batches,
                    details={
                        "batch_id": batch_id,
                        "rows": len(predictions),
                        "elapsed_seconds": perf_counter() - batch_started_at,
                    },
                )
            windows = pd.concat(window_definition_parts, ignore_index=True).groupby(
                "Window", sort=True, as_index=False
            ).agg(
                TrainStart=("TrainStart", "min"),
                TrainEnd=("TrainEnd", "max"),
                TestStart=("TestStart", "min"),
                TestEnd=("TestEnd", "max"),
            )
            aggregate_by_window, aggregate_global = _sql_aggregate_predictions(
                connection, windows, window_evaluations
            )
            risk_global = _sql_global_risk(connection, config)
        aggregation = {
            "aggregate_by_window": aggregate_by_window,
            "aggregate_by_set": pd.concat(aggregate_set_parts, ignore_index=True),
            "aggregate_global": aggregate_global,
            "risk_by_window": pd.concat(risk_window_parts, ignore_index=True),
            "risk_by_set": pd.concat(risk_set_parts, ignore_index=True),
            "risk_global": risk_global,
        }
        aggregation["_elapsed_seconds"] = perf_counter() - phase_started
        checkpoint.commit_artifact("aggregation", aggregation)
        checkpoint.phase_completed(phase)
        telemetry["aggregation_seconds"] = aggregation["_elapsed_seconds"]
        phase_seconds[phase] = aggregation["_elapsed_seconds"]
        report_progress(
            progress_callback,
            phase,
            substage="completed",
            details={
                "phase_event": "completed",
                "elapsed_seconds": aggregation["_elapsed_seconds"],
            },
        )

    phase = "qualification"
    if checkpoint.phase_is_completed(phase) and checkpoint.artifact_exists("qualification"):
        qualification = checkpoint.load_artifact("qualification")
    else:
        checkpoint.phase_started(phase)
        report_progress(progress_callback, phase, substage="started", details={"phase_event": "started"})
        phase_started = perf_counter()
        parts: list[pd.DataFrame] = []
        for batch_id in range(total_batches):
            check_cancellation(cancellation_check)
            payload = checkpoint.load_batch("walk_forward", batch_id)
            part = qualify_combinations(payload["windows"], payload["predictions"], config)
            parts.append(part.drop(columns="EligibleRank"))
        qualification = rank_qualified_combinations(pd.concat(parts, ignore_index=True))
        checkpoint.commit_artifact("qualification", qualification)
        checkpoint.phase_completed(phase)
        phase_seconds[phase] = perf_counter() - phase_started
        report_progress(
            progress_callback,
            phase,
            substage="completed",
            details={
                "phase_event": "completed",
                "eligible_combinations": int(qualification["Eligible"].sum()),
                "elapsed_seconds": phase_seconds[phase],
            },
        )

    eligibility = qualification[["Set", "Eligible", "EligibleRank"]]
    aggregation["risk_by_window"] = aggregation["risk_by_window"].merge(
        eligibility, on="Set", how="left"
    )
    aggregation["risk_by_set"] = aggregation["risk_by_set"].merge(
        eligibility, on="Set", how="left"
    )

    phase = "final_holdout"
    eligible = qualification[qualification["Eligible"]].sort_values("EligibleRank")
    holdout_sets = (
        generated_sets
        if generated_sets is not None
        else _sets_from_qualification(qualification)
    )
    holdout_batch_size = config.final_holdout_batch_size
    total_holdout_batches = (
        (len(eligible) + holdout_batch_size - 1) // holdout_batch_size
        if evaluate_holdout
        else 0
    )
    checkpoint.set_total_batches(phase, total_holdout_batches)
    holdout_phase_executed = not checkpoint.phase_is_completed(phase)
    if holdout_phase_executed:
        checkpoint.phase_started(phase)
        report_progress(progress_callback, phase, substage="started", details={"phase_event": "started"})
        phase_started = perf_counter()
        completed_holdout = set(checkpoint.completed_batch_ids(phase))
        for batch_id, start in enumerate(range(0, len(eligible), holdout_batch_size)):
            if batch_id in completed_holdout:
                continue
            batch_started_at = perf_counter()
            check_cancellation(cancellation_check)
            subset = eligible.iloc[start : start + holdout_batch_size]
            metrics, predictions = _evaluate_final_holdout(
                ordered,
                holdout_sets,
                subset,
                config,
                holdout_start,
                calendars,
                progress_callback=None,
                cancellation_check=cancellation_check,
            )
            checkpoint.commit_batch(
                phase,
                batch_id,
                {"metrics": metrics, "predictions": predictions},
                first_index=start,
                last_index=start + len(subset) - 1,
                combination_count=len(subset),
                row_counts={"metrics": len(metrics), "predictions": len(predictions)},
            )
            report_progress(
                progress_callback,
                phase,
                substage=f"batch {batch_id + 1}/{total_holdout_batches}",
                completed_units=min(start + len(subset), len(eligible)),
                total_units=len(eligible),
                details={
                    "batch_id": batch_id,
                    "rows": len(predictions),
                    "elapsed_seconds": perf_counter() - batch_started_at,
                    "parent_rss_bytes": process_rss_bytes(),
                    "checkpoint_written": True,
                },
            )

    if checkpoint.artifact_exists("final_holdout_aggregate"):
        holdout_aggregate = checkpoint.load_artifact("final_holdout_aggregate")
        final_holdout = holdout_aggregate["final_holdout"]
        final_holdout_risk = holdout_aggregate["final_holdout_risk"]
    else:
        if total_holdout_batches:
            holdout_parts: list[pd.DataFrame] = []
            holdout_risk_parts: list[pd.DataFrame] = []
            for batch_id in range(total_holdout_batches):
                payload = checkpoint.load_batch(phase, batch_id)
                holdout_parts.append(payload["metrics"])
                holdout_risk_parts.append(
                    _aggregate_final_risk(payload["predictions"], config)
                )
            final_holdout = pd.concat(holdout_parts, ignore_index=True)
            nonempty_risk = [frame for frame in holdout_risk_parts if not frame.empty]
            final_holdout_risk = (
                pd.concat(nonempty_risk, ignore_index=True)
                if nonempty_risk
                else pd.DataFrame()
            )
        else:
            final_holdout, empty_predictions = _evaluate_final_holdout(
                ordered,
                holdout_sets,
                qualification.assign(Eligible=False),
                config,
                holdout_start,
                calendars,
            )
            final_holdout_risk = pd.DataFrame()
            checkpoint.commit_artifact("empty_final_predictions", empty_predictions)
        if not final_holdout_risk.empty:
            final_holdout_risk = final_holdout_risk.merge(
                eligibility, on="Set", how="left"
            )
        checkpoint.commit_artifact(
            "final_holdout_aggregate",
            {
                "final_holdout": final_holdout,
                "final_holdout_risk": final_holdout_risk,
            },
        )
    if holdout_phase_executed:
        checkpoint.phase_completed(phase)
        phase_seconds[phase] = perf_counter() - phase_started
        report_progress(
            progress_callback,
            phase,
            substage="completed",
            details={
                "phase_event": "completed",
                "elapsed_seconds": phase_seconds[phase],
            },
        )

    phase = "metrics"
    if checkpoint.phase_is_completed(phase) and checkpoint.artifact_exists("metrics"):
        metrics = checkpoint.load_artifact("metrics")
    else:
        checkpoint.phase_started(phase)
        report_progress(progress_callback, phase, substage="started", details={"phase_event": "started"})
        phase_started = perf_counter()
        selection_results = score_qualified_models(
            _combine_selection_results(qualification, final_holdout), config
        )
        aggregate_global = aggregation["aggregate_global"].copy()
        aggregate_global["EligibleSets"] = int(qualification["Eligible"].sum())
        aggregate_global["EligiblePct"] = float(qualification["Eligible"].mean())
        aggregate_global["FinalConfirmedSets"] = int(final_holdout["FinalConfirmed"].sum())
        run_configuration = _workflow_configuration(
            config,
            min_train=min_train,
            test_window=test_window,
            step=step,
            holdout_size=holdout_size,
            development_end=development_end,
            holdout_start=holdout_start,
            evaluate_holdout=evaluate_holdout,
        )
        run_configuration.update(dict(run_configuration_extras or {}))
        metrics = {
            "aggregate_global": aggregate_global,
            "selection_results": selection_results,
            "run_configuration": run_configuration,
        }
        checkpoint.commit_artifact("metrics", metrics)
        checkpoint.phase_completed(phase)
        phase_seconds[phase] = perf_counter() - phase_started
        report_progress(
            progress_callback,
            phase,
            substage="completed",
            details={
                "phase_event": "completed",
                "elapsed_seconds": phase_seconds[phase],
            },
        )

    phase = "result_writing"
    if not checkpoint.phase_is_completed(phase):
        checkpoint.phase_started(phase)
        report_progress(progress_callback, phase, substage="started", details={"phase_event": "started"})
        phase_started = perf_counter()
        output.mkdir(parents=True, exist_ok=True)
        _atomic_csv_frames(
            output / "windows.csv",
            (
                checkpoint.load_batch("walk_forward", value)["windows"]
                for value in range(total_batches)
            ),
        )
        _atomic_csv_frames(
            output / "predictions.csv",
            (
                checkpoint.load_batch("walk_forward", value)["predictions"]
                for value in range(total_batches)
            ),
        )
        _atomic_csv(output / "aggregate_by_window.csv", aggregation["aggregate_by_window"])
        _atomic_csv(output / "aggregate_by_set.csv", aggregation["aggregate_by_set"])
        _atomic_csv(output / "aggregate_global.csv", metrics["aggregate_global"])
        _atomic_csv(output / "qualification.csv", qualification)
        _atomic_csv(output / "final_holdout.csv", final_holdout)
        if total_holdout_batches:
            _atomic_csv_frames(
                output / "final_holdout_predictions.csv",
                (
                    checkpoint.load_batch("final_holdout", value)["predictions"]
                    for value in range(total_holdout_batches)
                ),
            )
        else:
            _atomic_csv(
                output / "final_holdout_predictions.csv",
                checkpoint.load_artifact("empty_final_predictions"),
            )
        _atomic_csv(output / "selection_results.csv", metrics["selection_results"])
        _atomic_csv(output / "risk_by_window.csv", aggregation["risk_by_window"])
        _atomic_csv(output / "risk_by_set.csv", aggregation["risk_by_set"])
        _atomic_csv(output / "risk_global.csv", aggregation["risk_global"])
        _atomic_csv(output / "final_holdout_risk.csv", final_holdout_risk)
        _atomic_json(output / "run_configuration.json", metrics["run_configuration"])
        validate_final_artifacts(output)
        checkpoint.commit_artifact(
            "result_summary",
            {
                "aggregate_global": metrics["aggregate_global"],
                "qualification": qualification,
                "run_configuration": metrics["run_configuration"],
            },
        )
        checkpoint.phase_completed(phase)
        phase_seconds[phase] = perf_counter() - phase_started
        report_progress(
            progress_callback,
            phase,
            substage="completed",
            details={
                "phase_event": "completed",
                "elapsed_seconds": phase_seconds[phase],
            },
        )

    telemetry["parent_rss_end_bytes"] = process_rss_bytes()
    telemetry["checkpoint_bytes"] = sum(
        path.stat().st_size for path in checkpoint.root.rglob("*") if path.is_file()
    )
    telemetry["walk_forward_batches"] = total_batches
    telemetry["final_holdout_batches"] = total_holdout_batches
    return StreamedWalkForwardResult(
        aggregate_global=metrics["aggregate_global"],
        qualification=qualification,
        run_configuration=metrics["run_configuration"],
        telemetry=telemetry,
    )
