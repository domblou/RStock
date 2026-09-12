"""Workflow adapters shared by the Laboratory worker and future CLI execution."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from rstock.calibration import run_controlled_calibration, write_calibration_results
from rstock.combinations import generate_symbol_sets
from rstock.features import prepare_dataset
from rstock.progress import (
    CancellationCheck,
    ProgressCallback,
    ProgressEvent,
    check_cancellation,
    report_progress,
)
from rstock.threshold_calibration import (
    run_controlled_threshold_calibration,
    write_threshold_calibration_results,
)
from rstock.walk_forward import evaluate_walk_forward, write_walk_forward_results

from .domain import ExperimentSpec, JobType
from .services import MarketDataService


WorkflowHandler = Callable[
    [ExperimentSpec, Path, ProgressCallback | None, CancellationCheck | None],
    dict[str, Any],
]


def _phase(
    callback: ProgressCallback | None, name: str, event: str, **details: object
) -> None:
    report_progress(callback, name, substage=event, details={"phase_event": event, **details})


def _phase_callback(callback: ProgressCallback | None, phase: str) -> ProgressCallback | None:
    if callback is None:
        return None
    return lambda event: callback(
        ProgressEvent(phase, event.substage, event.completed_units, event.total_units, event.details)
    )


def _json_value(value: Any) -> Any:
    if isinstance(value, (np.integer,)):
        return int(value)
    if isinstance(value, (np.floating,)):
        return None if np.isnan(value) else float(value)
    if isinstance(value, pd.Timestamp):
        return value.isoformat()
    if isinstance(value, dict):
        return {str(key): _json_value(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_value(item) for item in value]
    return value


def _prepared_experiment(
    spec: ExperimentSpec,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, str]]:
    _phase(progress_callback, "data_preparation", "started")
    downloaded, calendars = MarketDataService().load(
        spec,
        progress_callback=_phase_callback(progress_callback, "data_preparation"),
        cancellation_check=cancellation_check,
    )
    check_cancellation(cancellation_check)
    report_progress(progress_callback, "data_preparation", substage="prepare_dataset")
    prepared = prepare_dataset(
        downloaded.prices,
        downloaded.symbols,
        spec.config.intraday_target_threshold,
        spec.config.lag_depth,
        spec.config.intraday_down_threshold,
    )
    _phase(progress_callback, "data_preparation", "completed", symbols=len(downloaded.symbols))
    _phase(progress_callback, "combination_generation", "started")
    generated = generate_symbol_sets(
        downloaded.symbols,
        spec.config.permutation_depth,
        max_sets=spec.config.max_generated_sets,
    )
    _phase(progress_callback, "combination_generation", "completed", combinations=len(generated))
    return prepared, generated, calendars


def _walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, calendars = _prepared_experiment(
        spec, progress_callback, cancellation_check
    )
    result = evaluate_walk_forward(
        prepared,
        generated,
        spec.config,
        market_calendars=calendars,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "result_writing", "started")
    write_walk_forward_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "metrics": _json_value(result.aggregate_global.iloc[0].to_dict()),
        "eligible_combinations": int(result.qualification["Eligible"].sum()),
        "result_files": sorted(path.name for path in output.iterdir()),
    }


def _xgboost_calibration(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, _ = _prepared_experiment(
        spec, progress_callback, cancellation_check
    )
    result = run_controlled_calibration(
        prepared,
        generated,
        spec.config,
        combinations_per_target=spec.combinations_per_target,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "result_writing", "started")
    write_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "selected_configurations": _json_value(result.selected_configurations),
        "holdout_metrics": _json_value(result.holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
    }


def _threshold_calibration(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, _ = _prepared_experiment(
        spec, progress_callback, cancellation_check
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        spec.config,
        combinations_per_target=spec.combinations_per_target,
        evaluate_final_holdout=spec.evaluate_final_holdout,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "result_writing", "started")
    write_threshold_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "selected_thresholds": _json_value(result.calibration.selected_thresholds),
        "holdout_metrics": _json_value(result.holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
    }


@dataclass(slots=True)
class WorkflowRegistry:
    handlers: dict[JobType, WorkflowHandler]

    @classmethod
    def production(cls) -> "WorkflowRegistry":
        return cls(
            {
                JobType.WALK_FORWARD: _walk_forward,
                JobType.XGBOOST_CALIBRATION: _xgboost_calibration,
                JobType.THRESHOLD_CALIBRATION: _threshold_calibration,
            }
        )

    def execute(
        self,
        spec: ExperimentSpec,
        output: Path,
        *,
        progress_callback: ProgressCallback | None,
        cancellation_check: CancellationCheck | None,
    ) -> dict[str, Any]:
        handler = self.handlers.get(spec.job_type)
        if handler is None:
            raise ValueError(f"No workflow is registered for {spec.job_type.value}")
        return handler(spec, output, progress_callback, cancellation_check)
