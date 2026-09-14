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
from rstock.calendars import offset_market_session
from rstock.combinations import generate_symbol_sets, generate_target_symbol_sets
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
from rstock.progress import (
    CancellationCheck,
    ProgressCallback,
    ProgressEvent,
    check_cancellation,
    report_progress,
)
from rstock.predictor_prefilter import PREFILTER_SCORE_FORMULA, select_predictors
from rstock.threshold_calibration import (
    run_controlled_threshold_calibration,
    write_threshold_calibration_results,
)
from rstock.walk_forward import evaluate_walk_forward, write_walk_forward_results

from .domain import ExperimentSpec, JobType
from .production_repository import ProductionRepository
from .repository import RunRepository
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


def _prepared_inputs(
    spec: ExperimentSpec,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> tuple[pd.DataFrame, list[str], list[str], dict[str, str]]:
    _phase(progress_callback, "data_preparation", "started")
    downloaded, calendars = MarketDataService().load(
        spec,
        progress_callback=_phase_callback(progress_callback, "data_preparation"),
        cancellation_check=cancellation_check,
    )
    check_cancellation(cancellation_check)
    offset = spec.config.walk_forward_end_offset_sessions
    if not isinstance(offset, int) or isinstance(offset, bool) or offset < 0:
        raise ValueError(
            "Décalage de fin walk-forward invalide : "
            f"offset demandé={offset!r}; un entier >= 0 est requis."
        )
    effective_end_date: pd.Timestamp | None = None
    if offset > 0:
        available_observations = len(downloaded.prices)
        if downloaded.prices.empty:
            raise ValueError(
                "Décalage de fin walk-forward impossible : "
                f"offset demandé={offset}; date de fin effective=indéterminée; "
                "observations disponibles=0; aucune donnée de marché disponible."
            )
        try:
            effective_end_date = offset_market_session(
                downloaded.prices.index.max(), spec.calendar, offset
            )
        except (ValueError, IndexError, KeyError) as error:
            raise ValueError(
                "Décalage de fin walk-forward impossible : "
                f"offset demandé={offset}; date de fin effective=indéterminée; "
                f"observations disponibles={available_observations}; "
                f"le calendrier {spec.calendar} ne contient pas assez de séances."
            ) from error
        downloaded, calendars = MarketDataService().load(
            spec,
            history_days=spec.config.model_history_days,
            as_of=effective_end_date.date(),
            progress_callback=_phase_callback(progress_callback, "data_preparation"),
            cancellation_check=cancellation_check,
        )
        if downloaded.prices.empty:
            raise ValueError(
                "Décalage de fin walk-forward impossible : "
                f"offset demandé={offset}; "
                f"date de fin effective={effective_end_date.date().isoformat()}; "
                "observations disponibles=0; aucune donnée historique n'est "
                "disponible pour la période décalée."
            )
        downloaded.prices = downloaded.prices.loc[
            downloaded.prices.index <= effective_end_date
        ].copy()
        check_cancellation(cancellation_check)
    report_progress(progress_callback, "data_preparation", substage="prepare_dataset")
    prepared = prepare_dataset(
        downloaded.prices,
        downloaded.symbols,
        spec.config.intraday_target_threshold,
        spec.config.lag_depth,
        spec.config.intraday_down_threshold,
    )
    if effective_end_date is None and not prepared.empty:
        effective_end_date = pd.Timestamp(prepared.index.max()).normalize()
    if effective_end_date is not None:
        prepared.attrs["effective_end_date"] = effective_end_date.isoformat()
    prepared.attrs["walk_forward_end_offset_sessions"] = offset
    if offset > 0:
        minimum_observations = (
            spec.config.walk_forward_min_train_size
            + spec.config.final_holdout_size
            + 1
        )
        if len(prepared) < minimum_observations:
            raise ValueError(
                "Décalage de fin walk-forward impossible : "
                f"offset demandé={offset}; "
                f"date de fin effective={effective_end_date.date().isoformat()}; "
                f"observations disponibles={len(prepared)}; "
                f"au moins {minimum_observations} observations sont requises pour "
                "le train minimal et le holdout final."
            )
    _phase(progress_callback, "data_preparation", "completed", symbols=len(downloaded.symbols))
    available = set(downloaded.symbols)
    predictor_symbols = [
        symbol for symbol in spec.predictor_symbols if symbol in available
    ]
    target_symbols = [symbol for symbol in spec.target_symbols if symbol in available]
    return prepared, predictor_symbols, target_symbols, calendars


def _persist_walk_forward_period(
    run_configuration: dict[str, object], prepared: pd.DataFrame, config: object
) -> dict[str, object]:
    period = {
        "walk_forward_end_offset_sessions": int(
            getattr(config, "walk_forward_end_offset_sessions", 0)
        ),
        "effective_end_date": prepared.attrs.get("effective_end_date"),
    }
    run_configuration.update(period)
    return period


def _prepared_experiment(
    spec: ExperimentSpec,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, str]]:
    prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
        spec, progress_callback, cancellation_check
    )
    _phase(progress_callback, "combination_generation", "started")
    generated = generate_symbol_sets(
        predictor_symbols,
        spec.config.permutation_depth,
        target_symbols=target_symbols,
        max_sets=spec.config.max_generated_sets,
    )
    _phase(progress_callback, "combination_generation", "completed", combinations=len(generated))
    return prepared, generated, calendars


def _qualified_sets_from_walk_forward_source(spec: ExperimentSpec) -> pd.DataFrame | None:
    """Reuse qualified source sets for a duplicated threshold calibration.

    A prefiltered walk-forward run and a calibration rebuilt from the full
    universe do not evaluate the same models.  When duplication records its
    source, the frozen qualified set list is the only valid calibration input.
    """

    source_run = spec.source_walk_forward_run
    if not source_run:
        return None
    runs = RunRepository(spec.config.project_root / "runs")
    status = runs.status(source_run)
    if status.get("job_type") != JobType.WALK_FORWARD.value:
        raise ValueError("Threshold calibration source must be a walk-forward run")
    if status.get("status") != "completed":
        raise ValueError("Threshold calibration source walk-forward is not completed")
    source_spec = runs.load_spec(source_run)
    frozen_fields = (
        "target_symbols", "context_symbols", "predictor_symbols", "calendar",
    )
    if any(getattr(source_spec, name) != getattr(spec, name) for name in frozen_fields):
        raise ValueError("Threshold calibration source has a different frozen population")
    path = runs.run_directory(source_run) / "results" / "qualification.csv"
    if not path.exists():
        raise ValueError("Source walk-forward qualification artifact is unavailable")
    qualification = pd.read_csv(path)
    eligible = qualification[
        qualification.get("Eligible", pd.Series(False, index=qualification.index))
        .map(lambda value: value is True or str(value).strip().lower() in {"true", "1", "yes"})
    ]
    rows: list[list[object]] = []
    for value in eligible.get("Set", pd.Series(dtype=object)):
        try:
            symbols = json.loads(str(value))
        except json.JSONDecodeError as error:
            raise ValueError("Source walk-forward contains an invalid set identifier") from error
        if not isinstance(symbols, list) or len(symbols) < 2:
            raise ValueError("Source walk-forward contains an invalid qualified set")
        rows.append([str(symbol) for symbol in symbols])
    if not rows:
        raise ValueError("Source walk-forward has no qualified combinations")
    width = max(len(row) for row in rows)
    return pd.DataFrame(
        [row + [None] * (width - len(row)) for row in rows],
        columns=[f"V{index}" for index in range(width)],
    )


def _walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prefilter = None
    if spec.config.predictor_prefilter_enabled:
        prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check
        )
        _phase(progress_callback, "combination_generation", "started")
        univariate_sets = generate_symbol_sets(
            predictor_symbols,
            1,
            target_symbols=target_symbols,
            max_sets=spec.config.max_generated_sets,
        )
        prefilter_config = replace(
            spec.config,
            qualification_min_median_auc=spec.config.predictor_prefilter_min_median_auc,
            qualification_min_pct_windows_above_random=(
                spec.config.predictor_prefilter_min_pct_above_random
            ),
            qualification_min_worst_window_auc=(
                spec.config.predictor_prefilter_min_worst_auc
            ),
            qualification_max_auc_std=spec.config.predictor_prefilter_max_auc_std,
        )
        univariate = evaluate_walk_forward(
            prepared,
            univariate_sets,
            prefilter_config,
            market_calendars=calendars,
            evaluate_holdout=False,
            cancellation_check=cancellation_check,
        )
        development = prepared.iloc[:-spec.config.final_holdout_size]
        prefilter = select_predictors(
            univariate.qualification,
            development,
            targets=target_symbols,
            candidate_symbols=predictor_symbols,
            config=spec.config,
        )
        generated = generate_target_symbol_sets(
            prefilter.predictors_by_target,
            spec.config.permutation_depth,
            max_sets=spec.config.max_generated_sets,
        )
        if generated.empty:
            raise ValueError("Predictor prefilter retained no testable combinations")
        _phase(
            progress_callback,
            "combination_generation",
            "completed",
            combinations=len(generated),
        )
    else:
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
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    if prefilter is not None:
        result.run_configuration["predictor_prefilter"] = {
            "enabled": True,
            "score_formula": PREFILTER_SCORE_FORMULA,
            "top_n": spec.config.predictor_prefilter_top_n,
            "min_median_auc": spec.config.predictor_prefilter_min_median_auc,
            "min_pct_above_random": (
                spec.config.predictor_prefilter_min_pct_above_random
            ),
            "min_worst_auc": spec.config.predictor_prefilter_min_worst_auc,
            "max_auc_std": spec.config.predictor_prefilter_max_auc_std,
            "correlation_threshold": (
                spec.config.predictor_prefilter_correlation_threshold
            ),
            "targets": prefilter.diagnostics,
        }
    _phase(progress_callback, "result_writing", "started")
    write_walk_forward_results(result, output)
    if prefilter is not None:
        prefilter.metrics.to_csv(output / "predictor_prefilter.csv", index=False)
        (output / "predictor_prefilter.json").write_text(
            json.dumps(
                {
                    "score_formula": PREFILTER_SCORE_FORMULA,
                    "targets": prefilter.diagnostics,
                },
                indent=2,
                ensure_ascii=False,
            ) + "\n",
            encoding="utf-8",
        )
    _phase(progress_callback, "result_writing", "completed")
    summary = {
        "job_type": spec.job_type.value,
        "metrics": _json_value(result.aggregate_global.iloc[0].to_dict()),
        "eligible_combinations": int(result.qualification["Eligible"].sum()),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
    }
    if prefilter is not None:
        summary["total_combinations"] = len(generated)
        summary["predictor_prefilter"] = _json_value(prefilter.diagnostics)
    return summary


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
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    _phase(progress_callback, "result_writing", "started")
    write_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "selected_configurations": _json_value(result.selected_configurations),
        "holdout_metrics": _json_value(result.holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
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
    source_generated = _qualified_sets_from_walk_forward_source(spec)
    if source_generated is not None:
        generated = source_generated
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        spec.config,
        combinations_per_target=spec.combinations_per_target,
        evaluate_final_holdout=spec.evaluate_final_holdout,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    _phase(progress_callback, "result_writing", "started")
    write_threshold_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "outcome": result.run_configuration["outcome"],
        "selected_thresholds": _json_value(result.calibration.selected_thresholds),
        "threshold_diagnostics": _json_value(
            result.run_configuration["threshold_diagnostics"]
        ),
        "missing_frozen_thresholds": _json_value(
            result.run_configuration["missing_frozen_thresholds"]
        ),
        "holdout_skipped_reason": result.run_configuration["holdout_skipped_reason"],
        "holdout_combination_counts": _json_value(
            result.run_configuration.get("holdout_combination_counts", {})
        ),
        "missing_threshold_count_up": result.run_configuration.get(
            "missing_threshold_count_up", 0
        ),
        "missing_threshold_count_down": result.run_configuration.get(
            "missing_threshold_count_down", 0
        ),
        "holdout_metrics": _json_value(result.holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_qualified_combinations": (
            None if source_generated is None else len(source_generated)
        ),
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
    active = repository.active_models()
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
    prepared, downloaded = _operational_prepared(
        spec,
        progress_callback,
        cancellation_check,
        preparation_config=effective_config,
    )
    _phase(progress_callback, "daily_prediction", "started")
    predictions = DailyPredictionService(repository).generate(
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
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
    active = repository.active_models()
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
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
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
