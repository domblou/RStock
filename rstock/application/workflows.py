"""Workflow adapters shared by the Laboratory worker and future CLI execution."""

from __future__ import annotations

import json
from collections.abc import Callable
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from rstock.calibration import run_controlled_calibration, write_calibration_results
from rstock.combinations import generate_symbol_sets
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
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
from .production_repository import ProductionRepository
from .production_services import (
    DailyPredictionService,
    OperationalUniverseService,
    ProductionSignalService,
    ProductionTrainingService,
    RealizedResultService,
)
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


def _operational_prepared(
    spec: ExperimentSpec,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
    *,
    preparation_config: Any | None = None,
    phase_name: str = "data_preparation",
) -> tuple[pd.DataFrame, Any]:
    effective_spec = (
        spec if preparation_config is None else replace(spec, config=preparation_config)
    )
    _phase(progress_callback, phase_name, "started", symbols=len(spec.symbols))
    downloaded, _ = MarketDataService().load(
        effective_spec,
        progress_callback=_phase_callback(progress_callback, phase_name),
        cancellation_check=cancellation_check,
    )
    check_cancellation(cancellation_check)
    prepared = prepare_dataset(
        downloaded.prices,
        downloaded.symbols,
        effective_spec.config.intraday_target_threshold,
        effective_spec.config.lag_depth,
        effective_spec.config.intraday_down_threshold,
    )
    _phase(progress_callback, phase_name, "completed", symbols=len(downloaded.symbols))
    return prepared, downloaded


def _production_training(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    repository = ProductionRepository(spec.config.project_root)
    candidate = repository.get(str(spec.model_id))
    source_rstock = candidate.source_configuration.get("rstock_config", {})
    effective_config = replace(
        spec.config,
        lag_depth=candidate.lag_depth,
        intraday_target_threshold=candidate.up_target_threshold,
        intraday_down_threshold=candidate.down_target_threshold,
        xgb_seed=candidate.xgboost_seed,
        xgb_nthread=candidate.xgboost_threads,
        model_history_days=int(
            source_rstock.get("model_history_days", spec.config.model_history_days)
        ),
        date_feature_regex=str(
            source_rstock.get("date_feature_regex", spec.config.date_feature_regex)
        ),
    )
    prepared, _ = _operational_prepared(
        spec,
        progress_callback,
        cancellation_check,
        preparation_config=effective_config,
    )
    _phase(progress_callback, "production_training", "started", model_id=spec.model_id)
    model = ProductionTrainingService(repository).train(
        str(spec.model_id),
        prepared,
        effective_config,
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "production_training", "completed", model_id=model.model_id)
    (output / "trained_model.json").write_text(
        json.dumps(model.to_dict(), indent=2, ensure_ascii=False, default=str) + "\n",
        encoding="utf-8",
    )
    return {"job_type": spec.job_type.value, "model_id": model.model_id, "status": model.status.value}


def _market_update(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    repository = ProductionRepository(spec.config.project_root)
    operational = OperationalUniverseService(repository).current()
    if tuple(spec.symbols) != operational.symbols:
        raise ValueError(
            "Operational universe changed after submission; submit a new market update"
        )
    _phase(progress_callback, "market_update", "started", symbols=len(spec.symbols))
    downloaded, _ = MarketDataService().load(
        spec,
        progress_callback=_phase_callback(progress_callback, "market_update"),
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "market_update", "completed", symbols=len(downloaded.symbols))
    summary = {
        "job_type": spec.job_type.value,
        "requested_symbols": list(spec.symbols),
        "updated_symbols": list(downloaded.symbols),
        "failed_symbols": list(downloaded.failed_symbols),
    }
    (output / "market_update.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    return summary


def _daily_prediction(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    repository = ProductionRepository(spec.config.project_root)
    active = [
        model
        for model in repository.models()
        if model.status.value == "active"
    ]
    operational = OperationalUniverseService(repository).current()
    if tuple(spec.symbols) != operational.symbols:
        raise ValueError(
            "Operational universe changed after submission; submit a new prediction"
        )
    if not active:
        raise ValueError("No active production model")
    source_history = [
        int(model.source_configuration.get("rstock_config", {}).get("model_history_days", 0))
        for model in active
    ]
    effective_config = replace(
        spec.config,
        lag_depth=max(model.lag_depth for model in active),
        model_history_days=max([spec.config.model_history_days, *source_history]),
    )
    prepared, _ = _operational_prepared(
        spec,
        progress_callback,
        cancellation_check,
        preparation_config=effective_config,
    )
    _phase(progress_callback, "daily_prediction", "started")
    predictions = DailyPredictionService(repository).generate(
        prepared, effective_config, persist=False
    )
    check_cancellation(cancellation_check)
    if not predictions.empty:
        repository.append_table("predictions", predictions, key="prediction_id")
    _phase(progress_callback, "daily_prediction", "completed", predictions=len(predictions))
    predictions.to_csv(output / "predictions.csv", index=False)
    return {"job_type": spec.job_type.value, "predictions": len(predictions), "errors": int((predictions.get("status") == "error").sum()) if not predictions.empty else 0}


def _daily_screening(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    check_cancellation(cancellation_check)
    _phase(progress_callback, "screening", "started")
    repository = ProductionRepository(spec.config.project_root)
    signals = ProductionSignalService(repository).screen(
        cancellation_check=cancellation_check, persist=False
    )
    check_cancellation(cancellation_check)
    if not signals.empty:
        repository.append_table("signals", signals, key="signal_id")
    _phase(progress_callback, "screening", "completed", records=len(signals))
    signals.to_csv(output / "screening.csv", index=False)
    counts = signals["category"].value_counts().to_dict() if not signals.empty else {}
    return {"job_type": spec.job_type.value, "categories": _json_value(counts)}


def _realized_validation(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    check_cancellation(cancellation_check)
    _phase(progress_callback, "realized_validation", "started")
    market_store = market_data_service(spec.config).store
    repository = ProductionRepository(spec.config.project_root)
    results = RealizedResultService(repository).update(
        market_store.read, cancellation_check=cancellation_check, persist=False
    )
    check_cancellation(cancellation_check)
    if not results.empty:
        repository.append_table("realized_results", results, key="result_id")
    _phase(progress_callback, "realized_validation", "completed", results=len(results))
    results.to_csv(output / "realized_results.csv", index=False)
    return {"job_type": spec.job_type.value, "realized_results": len(results)}


def _operational_run(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    repository = ProductionRepository(spec.config.project_root)
    active = [
        model
        for model in repository.models()
        if model.status.value == "active"
    ]
    operational = OperationalUniverseService(repository).current()
    if tuple(spec.symbols) != operational.symbols:
        raise ValueError(
            "Operational universe changed after submission; submit a new operational run"
        )
    if not active:
        raise ValueError("No active production model")
    source_history = [
        int(model.source_configuration.get("rstock_config", {}).get("model_history_days", 0))
        for model in active
    ]
    effective_config = replace(
        spec.config,
        lag_depth=max(model.lag_depth for model in active),
        model_history_days=max([spec.config.model_history_days, *source_history]),
    )
    prepared, downloaded = _operational_prepared(
        spec,
        progress_callback,
        cancellation_check,
        preparation_config=effective_config,
        phase_name="market_update",
    )
    check_cancellation(cancellation_check)
    _phase(progress_callback, "daily_prediction", "started")
    predictions = DailyPredictionService(repository).generate(
        prepared, effective_config, persist=False
    )
    _phase(progress_callback, "daily_prediction", "completed", predictions=len(predictions))
    check_cancellation(cancellation_check)
    _phase(progress_callback, "screening", "started")
    signals = ProductionSignalService(repository).screen(
        predictions, cancellation_check=cancellation_check, persist=False
    )
    _phase(progress_callback, "screening", "completed", records=len(signals))
    check_cancellation(cancellation_check)
    _phase(progress_callback, "realized_validation", "started")
    market_store = market_data_service(spec.config).store
    realized = RealizedResultService(repository).update(
        market_store.read,
        cancellation_check=cancellation_check,
        additional_predictions=predictions,
        additional_signals=signals,
        persist=False,
    )
    _phase(progress_callback, "realized_validation", "completed", results=len(realized))
    check_cancellation(cancellation_check)
    updates = {}
    if not predictions.empty:
        updates["predictions"] = (predictions, "prediction_id")
    if not signals.empty:
        updates["signals"] = (signals, "signal_id")
    if not realized.empty:
        updates["realized_results"] = (realized, "result_id")
    if updates:
        repository.append_tables(updates)
    predictions.to_csv(output / "predictions.csv", index=False)
    signals.to_csv(output / "screening.csv", index=False)
    realized.to_csv(output / "realized_results.csv", index=False)
    return {
        "job_type": spec.job_type.value, "updated_symbols": len(downloaded.symbols),
        "predictions": len(predictions), "signals": int((signals.get("category") == "bullish_signal").sum()) if not signals.empty else 0,
        "realized_results": len(realized),
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
                JobType.PRODUCTION_TRAINING: _production_training,
                JobType.MARKET_UPDATE: _market_update,
                JobType.DAILY_PREDICTION: _daily_prediction,
                JobType.DAILY_SCREENING: _daily_screening,
                JobType.REALIZED_VALIDATION: _realized_validation,
                JobType.OPERATIONAL_RUN: _operational_run,
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
