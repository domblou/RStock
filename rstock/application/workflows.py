"""Workflow adapters shared by the Laboratory worker and future CLI execution."""

from __future__ import annotations

import json
import math
from collections.abc import Callable
from dataclasses import dataclass, replace
from pathlib import Path
from time import perf_counter
from typing import Any

import numpy as np
import pandas as pd

from rstock.calibration import run_controlled_calibration, write_calibration_results
from rstock.calibration_sampling import policy_name
from rstock.calendars import offset_market_session
from rstock.evaluation import classification_metrics
from rstock.checkpoints import CheckpointManager
from rstock.combinations import generate_symbol_sets, generate_target_symbol_sets, symbol_set_id
from rstock.combination_planning import CombinationPlan, build_combination_plan
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
from rstock.modeling import (
    DirectionalXGBoostParameters,
    resolve_directional_xgboost_parameters,
)
from rstock.progress import (
    CancellationCheck,
    CancellationRequested,
    ProgressCallback,
    ProgressEvent,
    check_cancellation,
    report_progress,
)
from rstock.predictor_prefilter import PREFILTER_SCORE_FORMULA, select_predictors
from rstock.threshold_calibration import (
    EXPERIMENTAL_XGBOOST_PARAMETERS,
    apply_frozen_thresholds_by_set,
    evaluate_applied_thresholds,
    generate_holdout_probabilities,
    run_controlled_threshold_calibration,
    split_development_holdout,
    validate_threshold_calibration_config,
    write_threshold_calibration_results,
)
from rstock.threshold_parameter_calibration import (
    ThresholdCalibrationParameters,
    frozen_threshold_parameter_candidates,
    run_threshold_parameter_calibration,
    write_threshold_parameter_calibration_results,
)
from rstock.traceability import prepared_dataset_traceability
from rstock.walk_forward import (
    evaluate_prefilter_walk_forward,
    evaluate_walk_forward,
    write_walk_forward_results,
)
from rstock.streaming_walk_forward import (
    run_streamed_walk_forward,
    run_streamed_walk_forward_batch,
)

from .domain import ExperimentSpec, JobStatus, JobType
from .end_to_end import run_end_to_end
from .forced_candidate_validation import run_forced_candidate_validation
from .history_analysis import MIN_HOLDOUT_SIGNALS, threshold_promotion_guidance
from .orchestration_runtime import execute_child
from .processes import process_alive
from .production_repository import ProductionRepository
from .repository import RunRepository
from .walk_forward_batches import (
    PREFILTER_POLICY_VERSION,
    build_manifest,
    load_manifest,
    materialize_reservations,
    persist_or_validate_manifest,
    prefilter_digest,
)
from .production_services import (
    DailyPredictionService,
    OperationalUniverseService,
    ProductionSignalService,
    ProductionTrainingService,
    RealizedResultService,
)
from .services import MarketDataService


def _walk_forward_protocol_summary(config: object) -> str:
    mode = str(getattr(config, "walk_forward_window_mode"))
    if mode == "rolling":
        label = f"WF glissante {int(getattr(config, 'walk_forward_train_size'))}"
        train = None
    else:
        train = f"train min {int(getattr(config, 'walk_forward_min_train_size'))}"
        label = "WF expansive"
    train_text = "" if train is None else f" · {train}"
    return (
        f"{label}{train_text} · test {int(getattr(config, 'walk_forward_test_size'))} · "
        f"step {int(getattr(config, 'walk_forward_step_size'))}"
    )


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
    phase_started_at = perf_counter()
    _phase(progress_callback, "data_preparation", "started")
    historical_cutoff = (
        None
        if spec.historical_data_cutoff is None
        else pd.Timestamp(spec.historical_data_cutoff).normalize()
    )
    downloaded, calendars = MarketDataService().load(
        spec,
        as_of=None if historical_cutoff is None else historical_cutoff.date(),
        progress_callback=_phase_callback(progress_callback, "data_preparation"),
        cancellation_check=cancellation_check,
    )
    check_cancellation(cancellation_check)
    if historical_cutoff is not None:
        downloaded.prices = downloaded.prices.loc[
            downloaded.prices.index <= historical_cutoff
        ].copy()
    effective_end_date: pd.Timestamp | None = historical_cutoff
    # The offset belongs to the creation of a walk-forward period only. A
    # descendant receives its already-resolved end date through the cutoff and
    # must not consult the offset to resolve time. The legacy branch retains
    # prior behavior for historical derived snapshots without traceability.
    uses_legacy_derived_offset = (
        historical_cutoff is None
        and spec.job_type is not JobType.WALK_FORWARD
        and any(
            (
                spec.source_experiment_run,
                spec.source_walk_forward_run,
                spec.source_xgboost_calibration_run,
                spec.source_threshold_parameter_calibration_run,
            )
        )
    )
    applies_walk_forward_offset = historical_cutoff is None and (
        spec.job_type is JobType.WALK_FORWARD or uses_legacy_derived_offset
    )
    offset = None
    if applies_walk_forward_offset:
        offset = spec.config.walk_forward_end_offset_sessions
        if not isinstance(offset, int) or isinstance(offset, bool) or offset < 0:
            raise ValueError(
                "Décalage de fin walk-forward invalide : "
                f"offset demandé={offset!r}; un entier >= 0 est requis."
            )
    if offset is not None and offset > 0:
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
    # Retained as provenance only; descendants with a cutoff did not use it to
    # resolve their period.
    prepared.attrs["walk_forward_end_offset_sessions"] = (
        spec.config.walk_forward_end_offset_sessions
    )
    prepared.attrs["symbols_used"] = len(downloaded.symbols)
    if offset is not None and offset > 0:
        minimum_observations = (
            (
                spec.config.walk_forward_train_size
                if spec.config.walk_forward_window_mode == "rolling"
                else spec.config.walk_forward_min_train_size
            )
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
    _phase(
        progress_callback,
        "data_preparation",
        "completed",
        symbols=len(downloaded.symbols),
        rows=len(prepared),
        elapsed_seconds=perf_counter() - phase_started_at,
    )
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


def _persist_prepared_traceability(
    run_configuration: dict[str, object], prepared: pd.DataFrame, spec: ExperimentSpec
) -> dict[str, object]:
    """Persist compact code/data provenance alongside result configuration."""

    traceability = prepared_dataset_traceability(
        prepared,
        project_root=spec.config.project_root,
        symbols_used=int(prepared.attrs.get("symbols_used", len(spec.symbols))),
        source_prepared_dataset_sha256=spec.source_prepared_dataset_sha256,
    )
    run_configuration["traceability"] = traceability
    return traceability


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


def _require_exploitable_prefilter(univariate: object) -> None:
    """Fail only when local data exclusions leave no workable population."""

    telemetry = getattr(univariate, "telemetry")
    if "pairs_admissible" not in telemetry:
        return
    exploitable_targets = getattr(univariate, "exploitable_targets", ())
    if telemetry["pairs_admissible"] == 0 or not exploitable_targets:
        raise ValueError(
            "Predictor prefilter has no exploitable pairs after insufficient "
            "walk-forward observations were excluded"
        )


def _qualified_sets_from_walk_forward_source(
    spec: ExperimentSpec, *, allow_empty: bool = False
) -> pd.DataFrame | None:
    """Reuse the global qualified population from a source walk-forward.

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
        raise ValueError("Calibration source must be a walk-forward run")
    if status.get("status") != "completed":
        raise ValueError("Calibration source walk-forward is not completed")
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
    rows_by_set: dict[str, list[object]] = {}
    for value in eligible.get("Set", pd.Series(dtype=object)):
        try:
            symbols = json.loads(str(value))
        except json.JSONDecodeError as error:
            raise ValueError("Source walk-forward contains an invalid set identifier") from error
        if not isinstance(symbols, list) or len(symbols) < 2:
            raise ValueError("Source walk-forward contains an invalid qualified set")
        normalized = [str(symbol) for symbol in symbols]
        set_id = json.dumps(normalized, ensure_ascii=False, separators=(",", ":"))
        rows_by_set.setdefault(set_id, normalized)
    rows = list(rows_by_set.values())
    if not rows:
        if allow_empty:
            return pd.DataFrame(columns=["V0", "V1"])
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
    if output.name == "_working":
        return _resumable_walk_forward(
            spec, output, progress_callback, cancellation_check
        )
    prefilter = None
    prefilter_walk_forward_telemetry: dict[str, object] | None = None
    if spec.forced_symbol_sets is not None:
        prepared, _, _, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check
        )
        width = max(len(symbol_set) for symbol_set in spec.forced_symbol_sets)
        generated = pd.DataFrame(
            [list(symbol_set) + [None] * (width - len(symbol_set)) for symbol_set in spec.forced_symbol_sets],
            columns=[f"V{index}" for index in range(width)],
        )
        _phase(
            progress_callback,
            "forced_candidate_loading",
            "completed",
            combinations=len(generated),
        )
    elif spec.config.predictor_prefilter_enabled:
        prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check
        )
        _phase(progress_callback, "predictor_prefilter_generation", "started")
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
        _phase(
            progress_callback,
            "predictor_prefilter_generation",
            "completed",
            combinations=len(univariate_sets),
        )
        univariate = evaluate_prefilter_walk_forward(
            prepared,
            univariate_sets,
            prefilter_config,
            market_calendars=calendars,
            cancellation_check=cancellation_check,
            progress_callback=progress_callback,
        )
        prefilter_walk_forward_telemetry = univariate.telemetry
        _require_exploitable_prefilter(univariate)
        development = prepared.iloc[:-spec.config.final_holdout_size]
        _phase(progress_callback, "predictor_prefilter_selection", "started")
        prefilter = select_predictors(
            univariate.qualification,
            development,
            targets=target_symbols,
            candidate_symbols=predictor_symbols,
            config=spec.config,
            excluded_targets=getattr(univariate, "excluded_targets", {}),
        )
        _phase(
            progress_callback,
            "predictor_prefilter_selection",
            "completed",
            retained=sum(len(values) for values in prefilter.predictors_by_target.values()),
        )
        _phase(progress_callback, "combination_generation", "started")
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
    traceability = _persist_prepared_traceability(
        result.run_configuration, prepared, spec
    )
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
            "telemetry": prefilter_walk_forward_telemetry,
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
                    "telemetry": prefilter_walk_forward_telemetry,
                },
                indent=2,
                ensure_ascii=False,
            ) + "\n",
            encoding="utf-8",
        )
    _phase(progress_callback, "result_writing", "completed")
    summary = {
        "job_type": spec.job_type.value,
        "walk_forward_protocol": _walk_forward_protocol_summary(spec.config),
        "metrics": _json_value(result.aggregate_global.iloc[0].to_dict()),
        "eligible_combinations": int(result.qualification["Eligible"].sum()),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
    }
    if prefilter is not None:
        summary["total_combinations"] = len(generated)
        summary["predictor_prefilter"] = _json_value(prefilter.diagnostics)
    return summary


def _resumable_walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    """Production walk-forward path backed by versioned run checkpoints."""

    if (
        spec.config.walk_forward_max_combinations_per_batch is not None
        and spec.forced_symbol_sets is None
    ):
        return _planned_walk_forward(
            spec, output, progress_callback, cancellation_check
        )

    run_directory = output.parent
    checkpoint = CheckpointManager(
        run_directory,
        run_id=run_directory.name,
        job_type=spec.job_type.value,
        configuration_fingerprint=RunRepository(
            run_directory.parent
        ).configuration_fingerprint(run_directory.name, fallback=spec.fingerprint),
        batch_sizes={
            "predictor_prefilter_walk_forward": (
                spec.config.predictor_prefilter_batch_size
            ),
            "walk_forward": spec.config.walk_forward_batch_size,
            "final_holdout": spec.config.final_holdout_batch_size,
        },
    )

    if checkpoint.artifact_exists("prepared_snapshot"):
        prepared, preparation = checkpoint.load_snapshot()
        predictor_symbols = list(preparation["predictor_symbols"])
        target_symbols = list(preparation["target_symbols"])
        calendars = dict(preparation["calendars"])
    else:
        checkpoint.phase_started("data_preparation")
        prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check
        )
        checkpoint.commit_snapshot(
            prepared,
            {
                "predictor_symbols": predictor_symbols,
                "target_symbols": target_symbols,
                "calendars": calendars,
                "effective_end_date": prepared.attrs.get("effective_end_date"),
            },
        )
        checkpoint.phase_completed("data_preparation")

    prefilter = None
    prefilter_telemetry: dict[str, object] | None = None
    if spec.forced_symbol_sets is not None:
        if not spec.forced_symbol_sets:
            raise ValueError("Forced candidate validation has no candidate")
        if checkpoint.artifact_exists("generated_sets"):
            generated = checkpoint.load_artifact("generated_sets")
        else:
            checkpoint.phase_started("forced_candidate_loading")
            width = max(len(symbol_set) for symbol_set in spec.forced_symbol_sets)
            generated = pd.DataFrame(
                [list(symbol_set) + [None] * (width - len(symbol_set)) for symbol_set in spec.forced_symbol_sets],
                columns=[f"V{index}" for index in range(width)],
            )
            checkpoint.commit_artifact("generated_sets", generated)
            checkpoint.phase_completed("forced_candidate_loading")
            _phase(
                progress_callback,
                "forced_candidate_loading",
                "completed",
                combinations=len(generated),
            )
    elif spec.config.predictor_prefilter_enabled:
        if checkpoint.artifact_exists("prefilter_univariate_sets"):
            univariate_sets = checkpoint.load_artifact("prefilter_univariate_sets")
        else:
            checkpoint.phase_started("predictor_prefilter_generation")
            _phase(progress_callback, "predictor_prefilter_generation", "started")
            phase_started_at = perf_counter()
            univariate_sets = generate_symbol_sets(
                predictor_symbols,
                1,
                target_symbols=target_symbols,
                max_sets=spec.config.max_generated_sets,
            )
            checkpoint.commit_artifact("prefilter_univariate_sets", univariate_sets)
            checkpoint.phase_completed("predictor_prefilter_generation")
            _phase(
                progress_callback,
                "predictor_prefilter_generation",
                "completed",
                combinations=len(univariate_sets),
                elapsed_seconds=perf_counter() - phase_started_at,
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
        univariate = evaluate_prefilter_walk_forward(
            prepared,
            univariate_sets,
            prefilter_config,
            market_calendars=calendars,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
            checkpoint_manager=checkpoint,
        )
        checkpoint.commit_artifact("prefilter_qualification", univariate.qualification)
        checkpoint.phase_completed("predictor_prefilter_walk_forward")
        prefilter_telemetry = univariate.telemetry
        _require_exploitable_prefilter(univariate)
        if checkpoint.artifact_exists("prefilter_selection"):
            prefilter = checkpoint.load_artifact("prefilter_selection")
        else:
            checkpoint.phase_started("predictor_prefilter_selection")
            _phase(progress_callback, "predictor_prefilter_selection", "started")
            phase_started_at = perf_counter()
            development = prepared.iloc[:-spec.config.final_holdout_size]
            prefilter = select_predictors(
                univariate.qualification,
                development,
                targets=target_symbols,
                candidate_symbols=predictor_symbols,
                config=spec.config,
                excluded_targets=getattr(univariate, "excluded_targets", {}),
            )
            checkpoint.commit_artifact("prefilter_selection", prefilter)
            checkpoint.phase_completed("predictor_prefilter_selection")
            _phase(
                progress_callback,
                "predictor_prefilter_selection",
                "completed",
                retained=sum(
                    len(values) for values in prefilter.predictors_by_target.values()
                ),
                elapsed_seconds=perf_counter() - phase_started_at,
            )
        if checkpoint.artifact_exists("generated_sets"):
            generated = checkpoint.load_artifact("generated_sets")
        else:
            checkpoint.phase_started("combination_generation")
            _phase(progress_callback, "combination_generation", "started")
            phase_started_at = perf_counter()
            generated = generate_target_symbol_sets(
                prefilter.predictors_by_target,
                spec.config.permutation_depth,
                max_sets=spec.config.max_generated_sets,
            )
            if generated.empty:
                raise ValueError("Predictor prefilter retained no testable combinations")
            checkpoint.commit_artifact("generated_sets", generated)
            checkpoint.phase_completed("combination_generation")
            _phase(
                progress_callback,
                "combination_generation",
                "completed",
                combinations=len(generated),
                elapsed_seconds=perf_counter() - phase_started_at,
            )
    else:
        if checkpoint.artifact_exists("generated_sets"):
            generated = checkpoint.load_artifact("generated_sets")
        else:
            checkpoint.phase_started("combination_generation")
            _phase(progress_callback, "combination_generation", "started")
            phase_started_at = perf_counter()
            generated = generate_symbol_sets(
                predictor_symbols,
                spec.config.permutation_depth,
                target_symbols=target_symbols,
                max_sets=spec.config.max_generated_sets,
            )
            checkpoint.commit_artifact("generated_sets", generated)
            checkpoint.phase_completed("combination_generation")
            _phase(
                progress_callback,
                "combination_generation",
                "completed",
                combinations=len(generated),
                elapsed_seconds=perf_counter() - phase_started_at,
            )

    period = {
        "walk_forward_end_offset_sessions": int(
            spec.config.walk_forward_end_offset_sessions
        ),
        "effective_end_date": prepared.attrs.get("effective_end_date"),
    }
    extras: dict[str, object] = dict(period)
    traceability = _persist_prepared_traceability(extras, prepared, spec)
    if prefilter is not None:
        extras["predictor_prefilter"] = {
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
            "telemetry": prefilter_telemetry,
        }
    result = run_streamed_walk_forward(
        prepared,
        generated,
        spec.config,
        checkpoint,
        output,
        market_calendars=calendars,
        evaluate_holdout=spec.evaluate_final_holdout,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        run_configuration_extras=extras,
    )
    if prefilter is not None:
        prefilter.metrics.to_csv(output / "predictor_prefilter.csv", index=False)
        (output / "predictor_prefilter.json").write_text(
            json.dumps(
                {
                    "score_formula": PREFILTER_SCORE_FORMULA,
                    "targets": prefilter.diagnostics,
                    "telemetry": prefilter_telemetry,
                },
                indent=2,
                ensure_ascii=False,
            )
            + "\n",
            encoding="utf-8",
        )
    return {
        "job_type": spec.job_type.value,
        "metrics": _json_value(result.aggregate_global.iloc[0].to_dict()),
        "eligible_combinations": int(result.qualification["Eligible"].sum()),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
        "execution_telemetry": _json_value(result.telemetry),
        "walk_forward_protocol": _walk_forward_protocol_summary(spec.config),
        "checkpoint_manifest": "checkpoints/manifest.json",
        **(
            {"total_combinations": len(generated), "predictor_prefilter": _json_value(prefilter.diagnostics)}
            if prefilter is not None
            else {}
        ),
    }


def _prepared_calibration_population(
    spec: ExperimentSpec,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
    *,
    allow_empty: bool = False,
) -> tuple[pd.DataFrame, pd.DataFrame, bool]:
    source_generated = (
        _qualified_sets_from_walk_forward_source(spec, allow_empty=True)
        if allow_empty
        else _qualified_sets_from_walk_forward_source(spec)
    )
    if source_generated is None:
        prepared, generated, _ = _prepared_experiment(
            spec, progress_callback, cancellation_check
        )
        return prepared, generated, False
    prepared, _, _, _ = _prepared_inputs(
        spec, progress_callback, cancellation_check
    )
    _phase(progress_callback, "combination_generation", "started")
    _phase(
        progress_callback,
        "combination_generation",
        "completed",
        combinations=len(source_generated),
        source_walk_forward_run=spec.source_walk_forward_run,
    )
    return prepared, source_generated, True


def _xgboost_calibration(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, qualified_source = _prepared_calibration_population(
        spec, progress_callback, cancellation_check
    )
    sampling_policy = policy_name(
        spec.calibration_sampling_policy_version,
        qualified_walk_forward_source=qualified_source,
    )
    result = run_controlled_calibration(
        prepared,
        generated,
        spec.config,
        combinations_per_target=spec.combinations_per_target,
        sampling_policy=sampling_policy,
        global_max_combinations=(
            spec.config.xgboost_global_max_qualified_combinations
        ),
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    traceability = _persist_prepared_traceability(
        result.run_configuration, prepared, spec
    )
    _phase(progress_callback, "result_writing", "started")
    write_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "selected_configurations": _json_value(result.selected_configurations),
        "holdout_metrics": _json_value(result.holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
        "sampling_manifest": _json_value(
            result.run_configuration.get("sampling_manifest")
        ),
    }


def _threshold_calibration(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, qualified_source = _prepared_calibration_population(
        spec, progress_callback, cancellation_check
    )
    effective_xgboost = _resolve_threshold_xgboost_parameters(spec)
    sampling_policy = policy_name(
        spec.calibration_sampling_policy_version,
        qualified_walk_forward_source=qualified_source,
    )
    effective_threshold_config, threshold_parameter_source = (
        _resolve_threshold_calibration_config(spec)
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        effective_threshold_config,
        combinations_per_target=spec.combinations_per_target,
        sampling_policy=sampling_policy,
        evaluate_final_holdout=spec.evaluate_final_holdout,
        xgboost_parameters_by_direction={
            "Up": effective_xgboost.up,
            "Down": effective_xgboost.down,
        },
        xgboost_parameter_source=effective_xgboost.source,
        source_xgboost_calibration_run=spec.source_xgboost_calibration_run,
        frozen_xgboost_parameters_sha256=spec.frozen_xgboost_parameters_sha256,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    result.run_configuration["threshold_parameter_source"] = threshold_parameter_source
    result.run_configuration["source_threshold_parameter_calibration_run"] = (
        spec.source_threshold_parameter_calibration_run
    )
    result.run_configuration["frozen_threshold_calibration_parameters_sha256"] = (
        spec.frozen_threshold_calibration_parameters_sha256
    )
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    traceability = _persist_prepared_traceability(
        result.run_configuration, prepared, spec
    )
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
        "traceability": traceability,
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_threshold_parameter_calibration_run": (
            spec.source_threshold_parameter_calibration_run
        ),
        "source_qualified_combinations": (
            len(generated) if qualified_source else None
        ),
        "sampling_manifest": _json_value(
            result.run_configuration.get("sampling_manifest")
        ),
    }


def _fixed_candidate_evaluation(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    """Retrain offset models with reference parameters and apply reference thresholds."""

    if spec.frozen_xgboost_parameters is None:
        raise ValueError("Reference XGBoost parameters are required")
    if spec.frozen_threshold_calibration_parameters is None:
        raise ValueError("Reference threshold-calibration parameters are required")
    if spec.frozen_selected_thresholds_by_set is None:
        raise ValueError("Reference selected thresholds are required")
    if spec.forced_candidate_identities is None:
        raise ValueError("Forced candidate identities are required")

    if spec.job_type is JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC:
        prepared, _, _, _ = _prepared_inputs(
            spec, progress_callback, cancellation_check
        )
        _phase(progress_callback, "combination_generation", "started")
        forced_sets = spec.forced_symbol_sets or ()
        if forced_sets:
            width = max(len(symbol_set) for symbol_set in forced_sets)
            generated = pd.DataFrame(
                [
                    list(symbol_set) + [None] * (width - len(symbol_set))
                    for symbol_set in forced_sets
                ],
                columns=[f"V{index}" for index in range(width)],
            )
        else:
            generated = pd.DataFrame()
        _phase(
            progress_callback,
            "combination_generation",
            "completed",
            combinations=len(generated),
            source="forced_symbol_sets",
        )
    else:
        prepared, generated, _ = _prepared_calibration_population(
            spec,
            progress_callback,
            cancellation_check,
            allow_empty=True,
        )
    effective_xgboost = _resolve_threshold_xgboost_parameters(spec)
    effective_config, threshold_parameter_source = (
        _resolve_threshold_calibration_config(spec)
    )
    development, holdout, holdout_start = split_development_holdout(
        prepared, effective_config.final_holdout_size
    )
    identities = set(spec.forced_candidate_identities)
    prediction_columns = [
        "Set",
        "Observation",
        "Direction",
        "Window",
        "TrainEnd",
        "Date",
        "Probability",
        "Target",
        "IntradayReturn",
        "MFE",
        "MAE",
    ]
    _phase(progress_callback, "final_holdout", "started")
    candidate_errors: dict[str, str] = {}
    if generated.empty:
        raw_holdout = pd.DataFrame(columns=prediction_columns)
    elif spec.job_type is JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC:
        frames: list[pd.DataFrame] = []
        for _, candidate in generated.iterrows():
            candidate_frame = candidate.to_frame().T
            set_name = symbol_set_id(candidate)
            try:
                frames.append(generate_holdout_probabilities(
                    development,
                    holdout,
                    candidate_frame,
                    effective_config,
                    parameters_by_direction={
                        "Up": effective_xgboost.up,
                        "Down": effective_xgboost.down,
                    },
                    progress_callback=progress_callback,
                    cancellation_check=cancellation_check,
                ))
            except CancellationRequested:
                raise
            except Exception as error:
                candidate_errors[set_name] = str(error)
        raw_holdout = (
            pd.concat(frames, ignore_index=True)
            if frames
            else pd.DataFrame(columns=prediction_columns)
        )
    else:
        raw_holdout = generate_holdout_probabilities(
            development,
            holdout,
            generated,
            effective_config,
            parameters_by_direction={
                "Up": effective_xgboost.up,
                "Down": effective_xgboost.down,
            },
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
    raw_holdout = raw_holdout[
        [
            (str(set_name), str(direction)) in identities
            for set_name, direction in raw_holdout[["Set", "Direction"]]
            .itertuples(index=False, name=None)
        ]
    ].reset_index(drop=True)
    holdout_predictions = apply_frozen_thresholds_by_set(
        raw_holdout, spec.frozen_selected_thresholds_by_set
    )
    holdout_metrics = evaluate_applied_thresholds(
        holdout_predictions, effective_config
    )
    _phase(
        progress_callback,
        "final_holdout",
        "completed",
        combinations=len(generated),
        identities=len(identities),
    )

    run_configuration = {
        "protocol": "fixed_reference_model_and_threshold_evaluation_v1",
        "offset_sessions": effective_config.walk_forward_end_offset_sessions,
        "holdout_used_for_selection": False,
        "xgboost_calibration_performed": False,
        "threshold_parameter_calibration_performed": False,
        "threshold_selection_performed": False,
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_xgboost_calibration_run": spec.source_xgboost_calibration_run,
        "frozen_xgboost_parameters_sha256": (
            spec.frozen_xgboost_parameters_sha256
        ),
        "source_threshold_parameter_calibration_run": (
            spec.source_threshold_parameter_calibration_run
        ),
        "frozen_threshold_calibration_parameters_sha256": (
            spec.frozen_threshold_calibration_parameters_sha256
        ),
        "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        "frozen_selected_thresholds_sha256": (
            spec.frozen_selected_thresholds_sha256
        ),
        "forced_candidate_identities": [
            list(identity) for identity in spec.forced_candidate_identities
        ],
        "selected_thresholds_by_set": spec.frozen_selected_thresholds_by_set,
        "threshold_parameter_source": threshold_parameter_source,
        "xgboost_parameter_source": effective_xgboost.source,
        "development_end": development.index.max().isoformat(),
        "final_holdout_start": holdout_start.isoformat(),
        "final_holdout_size": effective_config.final_holdout_size,
        "evaluated_combinations": len(generated),
        "evaluated_identities": len(holdout_metrics),
        "candidate_errors": candidate_errors,
    }
    period = _persist_walk_forward_period(
        run_configuration, prepared, effective_config
    )
    traceability = _persist_prepared_traceability(
        run_configuration, prepared, spec
    )
    _phase(progress_callback, "result_writing", "started")
    output.mkdir(parents=True, exist_ok=True)
    generated.to_csv(output / "sampled_combinations.csv", index=False)
    holdout_predictions.to_csv(output / "holdout_predictions.csv", index=False)
    holdout_metrics.to_csv(output / "holdout_metrics.csv", index=False)
    (output / "selected_thresholds_by_set.json").write_text(
        json.dumps(
            spec.frozen_selected_thresholds_by_set,
            indent=2,
            ensure_ascii=False,
        )
        + "\n",
        encoding="utf-8",
    )
    (output / "run_configuration.json").write_text(
        json.dumps(run_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "walk_forward_protocol": _walk_forward_protocol_summary(spec.config),
        "protocol": run_configuration["protocol"],
        "holdout_metrics": _json_value(holdout_metrics.to_dict("records")),
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_xgboost_calibration_run": spec.source_xgboost_calibration_run,
        "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        "frozen_xgboost_parameters_sha256": spec.frozen_xgboost_parameters_sha256,
        "frozen_selected_thresholds_sha256": (
            spec.frozen_selected_thresholds_sha256
        ),
    }


def _qualification_holdout_diagnostic(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    """Evaluate persisted WF rejects on holdout without rerunning selection stages."""

    if spec.diagnostic_protocol != "qualification_holdout_diagnostic_v1":
        raise ValueError("Unsupported qualification holdout diagnostic protocol")
    if not spec.source_walk_forward_run:
        raise ValueError("Source forced Walk-forward run is required")
    source_results = (
        spec.config.project_root / "runs" / spec.source_walk_forward_run / "results"
    )
    qualification = pd.read_csv(source_results / "qualification.csv")
    predictions_path = source_results / "predictions.csv"
    predictions = (
        pd.read_csv(predictions_path)
        if predictions_path.is_file()
        else pd.DataFrame()
    )
    selected = spec.frozen_selected_thresholds_by_set or {}
    frozen_xgb = spec.frozen_xgboost_parameters or {}
    invalid: dict[tuple[str, str], str] = {}
    valid_identities: list[tuple[str, str]] = []
    valid_sets: list[tuple[str, ...]] = []
    set_by_name = {
        json.dumps(list(symbols), separators=(",", ":")): symbols
        for symbols in spec.forced_symbol_sets or ()
    }
    for identity in spec.forced_candidate_identities or ():
        set_name, direction = identity
        selection = selected.get(set_name, {}).get(direction, {})
        if not frozen_xgb.get(direction):
            invalid[identity] = "hyperparamètres figés absents"
        elif selection.get("status") != "selected" or selection.get("threshold") is None:
            invalid[identity] = "seuil de référence absent"
        elif set_name not in set_by_name:
            invalid[identity] = "combinaison source absente"
        else:
            valid_identities.append(identity)
            valid_sets.append(set_by_name[set_name])
    if valid_identities:
        evaluation_spec = replace(
            spec,
            forced_symbol_sets=tuple(valid_sets),
            forced_candidate_identities=tuple(valid_identities),
        )
        base_summary = _fixed_candidate_evaluation(
            evaluation_spec, output, progress_callback, cancellation_check
        )
        holdout = pd.read_csv(output / "holdout_metrics.csv")
        base_configuration_path = output / "run_configuration.json"
        base_configuration = (
            json.loads(base_configuration_path.read_text(encoding="utf-8"))
            if base_configuration_path.is_file()
            else {}
        )
        for set_name, error in base_configuration.get("candidate_errors", {}).items():
            for direction in ("Up", "Down"):
                invalid.setdefault((str(set_name), direction), str(error))
    else:
        output.mkdir(parents=True, exist_ok=True)
        holdout = pd.DataFrame()
        holdout.to_csv(output / "holdout_metrics.csv", index=False)
        base_summary = {}

    qualification_by_set = qualification.set_index("Set", drop=False)
    rows: list[dict[str, object]] = []
    for set_name, direction in spec.forced_candidate_identities or ():
        q = qualification_by_set.loc[set_name]
        if isinstance(q, pd.DataFrame):
            q = q.iloc[0]
        predictors = json.loads(str(q.get("Predictors", "[]")))
        second_worst: float | None = None
        window_aucs: list[float] = []
        if not predictions.empty:
            subset = predictions[predictions["Set"].astype(str) == set_name]
            target_column = f"{direction}Target"
            probability_column = f"{direction}Probability"
            for _, window in subset.groupby("Window", sort=True):
                metric = classification_metrics(
                    window[target_column],
                    np.zeros(len(window), dtype=int),
                    window[probability_column],
                )
                if metric.roc_auc is not None:
                    window_aucs.append(metric.roc_auc)
            if len(window_aucs) >= 2:
                second_worst = sorted(window_aucs)[1]
        metric_rows = (
            holdout[
                (holdout.get("Set", pd.Series(dtype=str)).astype(str) == set_name)
                & (holdout.get("Direction", pd.Series(dtype=str)).astype(str) == direction)
            ]
            if not holdout.empty
            else pd.DataFrame()
        )
        reason = invalid.get((set_name, direction))
        metric = None if metric_rows.empty else metric_rows.iloc[0]
        if metric is None and reason is None:
            reason = "données holdout insuffisantes"
        if metric is None:
            status = "Non évaluable"
            reading = "—"
        else:
            auc = pd.to_numeric(pd.Series([metric.get("ROCAUC")]), errors="coerce").iloc[0]
            signals = int(metric.get("SignalCount", 0))
            status = (
                "Holdout insuffisant"
                if signals < MIN_HOLDOUT_SIGNALS or pd.isna(auc)
                else "Holdout favorable" if float(auc) >= 0.50
                else "Holdout défavorable"
            )
            threshold = selected[set_name][direction]["threshold"]
            guidance_input = pd.DataFrame([{
                "Combinaison": set_name,
                "Cible": str(q.get("Observation", "")),
                "Predictors": " + ".join(str(item) for item in predictors),
                "Direction": direction,
                "Seuil calibré": threshold,
                "Signaux holdout": metric.get("SignalCount"),
                "AUC holdout": metric.get("ROCAUC"),
                "Précision holdout": metric.get("Precision"),
                "Rendement directionnel moyen": metric.get("DirectionalReturnMean"),
                "Fréquence mouvement opposé": metric.get("OppositeMoveFrequency"),
            }])
            guided = threshold_promotion_guidance(
                guidance_input, selected, promotion_config=spec.config
            ).iloc[0]
            reading = (
                "Aurait satisfait les critères holdout"
                if guided["Statut promotion"] == "Candidat"
                else "N’aurait pas satisfait les critères holdout"
            )
        rows.append({
            "Cible": str(q.get("Observation", "")),
            "Predictors": " + ".join(str(item) for item in predictors),
            "Set": set_name,
            "Direction": direction,
            "Worst AUC WF": q.get("ROCAUCWorst"),
            "2e pire AUC WF": second_worst,
            "AUC médiane WF": q.get("ROCAUCMedian"),
            "Ecart-type AUC WF": q.get("ROCAUCStd"),
            "% fenêtres > 0.50": q.get("PctWindowsAboveRandom"),
            "Raison rejet WF": str(q.get("IneligibilityReasons", "[]")),
            "AUC holdout diagnostic": None if metric is None else metric.get("ROCAUC"),
            "Precision": None if metric is None else metric.get("Precision"),
            "Rendement directionnel": None if metric is None else metric.get("DirectionalReturnMean"),
            "Mouvements opposés": None if metric is None else metric.get("OppositeMoveFrequency"),
            "Signaux": None if metric is None else metric.get("SignalCount"),
            "Seuil de référence": selected.get(set_name, {}).get(direction, {}).get("threshold"),
            "AUC WF par fenetre": json.dumps(window_aucs),
            "Statut diagnostique": status,
            "Lecture diagnostique": reading,
            "Raison non evaluable": reason,
        })
    results = pd.DataFrame(rows)
    results.to_csv(output / "diagnostic_results.csv", index=False)
    run_configuration = {
        "protocol": spec.diagnostic_protocol,
        "diagnostic_only": True,
        "xgb_recalibration": False,
        "threshold_recalibration": False,
        "walk_forward_rerun": False,
        "prefilter_rerun": False,
        "promotion_enabled": False,
        "source_end_to_end_run": (
            spec.source_end_to_end_run
            or RunRepository(output.parent.parent).run_metadata(output.parent.name).root_run_id
        ),
        "source_forced_candidate_validation_run": spec.source_forced_candidate_validation_run,
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        "frozen_xgboost_parameters_sha256": spec.frozen_xgboost_parameters_sha256,
        "frozen_selected_thresholds_sha256": spec.frozen_selected_thresholds_sha256,
        "candidate_count": len(results),
    }
    (output / "run_configuration.json").write_text(
        json.dumps(run_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    manifest = {
        "schema_version": 1,
        **run_configuration,
        "artifacts": ["diagnostic_results.csv", "holdout_metrics.csv", "run_configuration.json"],
    }
    (output / "diagnostic_manifest.json").write_text(
        json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    favorable = int((results["Statut diagnostique"] == "Holdout favorable").sum())
    non_evaluable = int((results["Statut diagnostique"] == "Non évaluable").sum())
    satisfied = int((results["Lecture diagnostique"] == "Aurait satisfait les critères holdout").sum())
    return {
        **base_summary,
        "job_type": spec.job_type.value,
        "protocol": spec.diagnostic_protocol,
        "candidate_count": len(results),
        "holdout_count": len(results) - non_evaluable,
        "non_evaluable_count": non_evaluable,
        "favorable_count": favorable,
        "unfavorable_count": len(results) - non_evaluable - favorable,
        "criteria_satisfied_count": satisfied,
        "result_files": sorted(path.name for path in output.iterdir()),
    }


def _threshold_parameter_parent(spec: ExperimentSpec) -> str | None:
    return (
        spec.source_experiment_run
        or spec.source_threshold_parameter_calibration_run
        or spec.source_xgboost_calibration_run
        or spec.source_walk_forward_run
    )


def _threshold_parameter_calibration(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    prepared, generated, qualified_source = _prepared_calibration_population(
        spec, progress_callback, cancellation_check
    )
    effective_xgboost = _resolve_threshold_xgboost_parameters(spec)
    sampling_policy = policy_name(
        spec.calibration_sampling_policy_version,
        qualified_walk_forward_source=qualified_source,
    )
    checkpoint_root = output.parent if output.name == "_working" else output
    checkpoint = CheckpointManager(
        checkpoint_root,
        run_id=checkpoint_root.name,
        job_type=spec.job_type.value,
        configuration_fingerprint=spec.fingerprint,
        batch_sizes={},
    )
    result = run_threshold_parameter_calibration(
        prepared,
        generated,
        spec.config,
        xgboost_parameters_by_direction={
            "Up": effective_xgboost.up,
            "Down": effective_xgboost.down,
        },
        xgboost_parameter_source=effective_xgboost.source,
        combinations_per_target=spec.combinations_per_target,
        sampling_policy=sampling_policy,
        max_directional_models=(
            spec.config.threshold_parameter_calibration_max_models
        ),
        candidates=frozen_threshold_parameter_candidates(output, spec.config),
        source_parent_run=_threshold_parameter_parent(spec),
        source_walk_forward_run=spec.source_walk_forward_run,
        source_xgboost_calibration_run=spec.source_xgboost_calibration_run,
        frozen_xgboost_parameters_sha256=spec.frozen_xgboost_parameters_sha256,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        checkpoint_manager=checkpoint,
    )
    period = _persist_walk_forward_period(
        result.run_configuration, prepared, spec.config
    )
    traceability = _persist_prepared_traceability(
        result.run_configuration, prepared, spec
    )
    _phase(progress_callback, "result_writing", "started")
    write_threshold_parameter_calibration_results(result, output)
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "walk_forward_protocol": _walk_forward_protocol_summary(spec.config),
        "selected_configuration": _json_value(result.selected_configuration),
        "candidate_count": len(result.development_by_configuration),
        "eligible_candidate_count": int(
            result.development_by_configuration["EligibleConfiguration"].sum()
        ),
        "source_parent_run": _threshold_parameter_parent(spec),
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_xgboost_calibration_run": spec.source_xgboost_calibration_run,
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
        "sampling_manifest": _json_value(
            result.run_configuration.get("sampling_manifest")
        ),
    }


def _walk_forward_checkpoint(
    repository: RunRepository, run_id: str, spec: ExperimentSpec
) -> CheckpointManager:
    return CheckpointManager(
        repository.run_directory(run_id),
        run_id=run_id,
        job_type=spec.job_type.value,
        configuration_fingerprint=repository.configuration_fingerprint(
            run_id, fallback=spec.fingerprint
        ),
        batch_sizes={
            "predictor_prefilter_walk_forward": (
                spec.config.predictor_prefilter_batch_size
            ),
            "walk_forward": spec.config.walk_forward_batch_size,
            "final_holdout": spec.config.final_holdout_batch_size,
        },
    )


def _load_or_prepare_parent_inputs(
    spec: ExperimentSpec,
    checkpoint: CheckpointManager,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> tuple[pd.DataFrame, list[str], list[str], dict[str, str]]:
    if checkpoint.artifact_exists("prepared_snapshot"):
        prepared, preparation = checkpoint.load_snapshot()
        return (
            prepared,
            list(preparation["predictor_symbols"]),
            list(preparation["target_symbols"]),
            dict(preparation["calendars"]),
        )
    checkpoint.phase_started("data_preparation")
    prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
        spec, progress_callback, cancellation_check
    )
    checkpoint.commit_snapshot(
        prepared,
        {
            "predictor_symbols": predictor_symbols,
            "target_symbols": target_symbols,
            "calendars": calendars,
            "effective_end_date": prepared.attrs.get("effective_end_date"),
        },
    )
    checkpoint.phase_completed("data_preparation")
    return prepared, predictor_symbols, target_symbols, calendars


def _planned_effective_plan(
    spec: ExperimentSpec,
    prepared: pd.DataFrame,
    predictor_symbols: list[str],
    target_symbols: list[str],
    calendars: dict[str, str],
    checkpoint: CheckpointManager,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> tuple[CombinationPlan, CombinationPlan, object | None, dict[str, object] | None, str, str]:
    raw_plan = build_combination_plan(
        target_symbols=target_symbols,
        predictor_symbols=predictor_symbols,
        permutation_depth=spec.config.permutation_depth,
    )
    if checkpoint.artifact_exists("raw_combination_plan"):
        persisted_raw = CombinationPlan.from_dict(
            checkpoint.load_artifact("raw_combination_plan")
        )
        if persisted_raw.plan_sha256 != raw_plan.plan_sha256:
            raise ValueError("Le plan brut ne correspond plus au checkpoint.")
        raw_plan = persisted_raw
    else:
        checkpoint.commit_artifact("raw_combination_plan", raw_plan.to_dict())

    prefilter = None
    telemetry: dict[str, object] | None = None
    policy_version = "disabled_v1"
    digest = prefilter_digest(raw_plan.predictors_by_target)
    if spec.config.predictor_prefilter_enabled:
        policy_version = PREFILTER_POLICY_VERSION
        if checkpoint.artifact_exists("prefilter_univariate_sets"):
            univariate_sets = checkpoint.load_artifact("prefilter_univariate_sets")
        else:
            checkpoint.phase_started("predictor_prefilter_generation")
            _phase(progress_callback, "predictor_prefilter_generation", "started")
            univariate_plan = build_combination_plan(
                target_symbols=target_symbols,
                predictor_symbols=predictor_symbols,
                permutation_depth=1,
            )
            univariate_sets = univariate_plan.slice(0, univariate_plan.count())
            checkpoint.commit_artifact("prefilter_univariate_sets", univariate_sets)
            checkpoint.phase_completed("predictor_prefilter_generation")
            _phase(
                progress_callback,
                "predictor_prefilter_generation",
                "completed",
                combinations=univariate_plan.count(),
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
        univariate = evaluate_prefilter_walk_forward(
            prepared,
            univariate_sets,
            prefilter_config,
            market_calendars=calendars,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
            checkpoint_manager=checkpoint,
        )
        checkpoint.commit_artifact("prefilter_qualification", univariate.qualification)
        checkpoint.phase_completed("predictor_prefilter_walk_forward")
        telemetry = univariate.telemetry
        _require_exploitable_prefilter(univariate)
        if checkpoint.artifact_exists("prefilter_selection"):
            prefilter = checkpoint.load_artifact("prefilter_selection")
        else:
            checkpoint.phase_started("predictor_prefilter_selection")
            _phase(progress_callback, "predictor_prefilter_selection", "started")
            prefilter = select_predictors(
                univariate.qualification,
                prepared.iloc[:-spec.config.final_holdout_size],
                targets=target_symbols,
                candidate_symbols=predictor_symbols,
                config=spec.config,
                excluded_targets=getattr(univariate, "excluded_targets", {}),
            )
            checkpoint.commit_artifact("prefilter_selection", prefilter)
            checkpoint.phase_completed("predictor_prefilter_selection")
            _phase(
                progress_callback,
                "predictor_prefilter_selection",
                "completed",
                retained=sum(
                    len(values)
                    for values in prefilter.predictors_by_target.values()
                ),
            )
        effective_plan = CombinationPlan.from_target_predictors(
            prefilter.predictors_by_target, spec.config.permutation_depth
        )
        digest = prefilter_digest(prefilter.predictors_by_target)
    else:
        effective_plan = raw_plan
    if effective_plan.count() < 1:
        raise ValueError("Le plan walk-forward effectif est vide.")
    if checkpoint.artifact_exists("effective_combination_plan"):
        persisted = CombinationPlan.from_dict(
            checkpoint.load_artifact("effective_combination_plan")
        )
        if persisted.plan_sha256 != effective_plan.plan_sha256:
            raise ValueError("Le plan effectif ne correspond plus au checkpoint.")
        effective_plan = persisted
    else:
        checkpoint.phase_started("combination_generation")
        checkpoint.commit_artifact(
            "effective_combination_plan", effective_plan.to_dict()
        )
        checkpoint.phase_completed("combination_generation")
    return raw_plan, effective_plan, prefilter, telemetry, policy_version, digest


def _worker_pid_alive(pid: object) -> bool:
    return process_alive(pid)


def _execute_reserved_child(repository: RunRepository, run_id: str) -> None:
    status = repository.status(run_id)
    current = JobStatus(status["status"])
    if current is JobStatus.COMPLETED:
        return
    if current is JobStatus.RUNNING:
        if _worker_pid_alive(status.get("pid")):
            raise RuntimeError(f"Le batch WF {run_id} est déjà en cours.")
        repository.transition(
            run_id,
            JobStatus.INTERRUPTED,
            error="Le processus worker enfant n'est plus actif.",
        )
        current = JobStatus.INTERRUPTED
    if current in {JobStatus.FAILED, JobStatus.CANCELLED, JobStatus.INTERRUPTED}:
        resumed = repository.prepare_resume(run_id)
        resumed["resume_requested"] = True
        repository.write_json(run_id, "status.json", resumed)
    execute_child(run_id)
    completed = repository.status(run_id)
    if completed["status"] != JobStatus.COMPLETED.value:
        raise RuntimeError(
            f"Le batch WF {run_id} a échoué : "
            f"{completed.get('error') or completed['status']}"
        )


def _ingest_child_checkpoints(
    repository: RunRepository,
    parent_checkpoint: CheckpointManager,
    manifest: dict[str, Any],
) -> int:
    batch_size = int(parent_checkpoint.batch_sizes["walk_forward"])
    expected_internal = sum(
        math.ceil(int(item["combination_count"]) / batch_size)
        for item in manifest["batches"]
    )
    parent_checkpoint.phase_started("walk_forward")
    parent_checkpoint.set_total_batches("walk_forward", expected_internal)
    completed_parent = set(parent_checkpoint.completed_batch_ids("walk_forward"))
    global_batch_id = 0
    for item in manifest["batches"]:
        child_run_id = str(item["child_run_id"])
        child_specification = repository.load_spec(child_run_id)
        child_checkpoint = _walk_forward_checkpoint(
            repository, child_run_id, child_specification
        )
        internal_count = math.ceil(int(item["combination_count"]) / batch_size)
        if child_checkpoint.completed_batch_ids("walk_forward") != tuple(
            range(internal_count)
        ):
            raise RuntimeError(f"Checkpoints incomplets pour {child_run_id}")
        for local_batch_id in range(internal_count):
            if global_batch_id not in completed_parent:
                payload = child_checkpoint.load_batch("walk_forward", local_batch_id)
                local_start = local_batch_id * batch_size
                count = min(
                    batch_size, int(item["combination_count"]) - local_start
                )
                first = int(item["range_start"]) + local_start
                parent_checkpoint.commit_batch(
                    "walk_forward",
                    global_batch_id,
                    payload,
                    first_index=first,
                    last_index=first + count - 1,
                    combination_count=count,
                    row_counts={
                        "windows": len(payload["windows"]),
                        "predictions": len(payload["predictions"]),
                    },
                )
            global_batch_id += 1
    if parent_checkpoint.completed_batch_ids("walk_forward") != tuple(
        range(expected_internal)
    ):
        raise RuntimeError("Agrégation des checkpoints enfants incomplète")
    parent_checkpoint.phase_completed("walk_forward")
    return expected_internal


def _planned_walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    run_directory = output.parent
    repository = RunRepository(run_directory.parent)
    run_id = run_directory.name
    checkpoint = _walk_forward_checkpoint(repository, run_id, spec)
    prepared, predictor_symbols, target_symbols, calendars = (
        _load_or_prepare_parent_inputs(
            spec, checkpoint, progress_callback, cancellation_check
        )
    )
    (
        raw_plan,
        effective_plan,
        prefilter,
        prefilter_telemetry,
        policy_version,
        digest,
    ) = _planned_effective_plan(
        spec,
        prepared,
        predictor_symbols,
        target_symbols,
        calendars,
        checkpoint,
        progress_callback,
        cancellation_check,
    )
    capacity = spec.config.walk_forward_max_combinations_per_batch
    assert capacity is not None
    effective_batch_count = math.ceil(effective_plan.count() / capacity)
    precomputed_batches: int | None = None
    generated: pd.DataFrame | None
    manifest = None
    if effective_batch_count == 1:
        generated = effective_plan.slice(0, effective_plan.count())
    else:
        generated = None
        existing_manifest = load_manifest(repository, run_id)
        proposed, reservations = build_manifest(
            repository,
            parent_run_id=run_id,
            parent_spec=spec,
            raw_plan=raw_plan,
            effective_plan=effective_plan,
            max_combinations_per_batch=capacity,
            prefilter_policy_version=policy_version,
            prefilter_sha256=digest,
            child_id_policy_version=(
                int(existing_manifest.get("child_id_policy_version", 1))
                if existing_manifest is not None
                else 2
            ),
            reserved_child_ids=(
                [str(item["child_run_id"]) for item in existing_manifest["batches"]]
                if existing_manifest is not None
                else None
            ),
        )
        manifest = persist_or_validate_manifest(repository, run_id, proposed)
        # IDs are durable before any child directory is created.
        materialize_reservations(
            repository, manifest=manifest, reservations=reservations
        )
        _phase(
            progress_callback,
            "walk_forward",
            "started",
            combinations=effective_plan.count(),
            child_batches=effective_batch_count,
        )
        completed_combinations = 0
        for batch_number, item in enumerate(manifest["batches"], start=1):
            check_cancellation(cancellation_check)
            _execute_reserved_child(repository, str(item["child_run_id"]))
            completed_combinations += int(item["combination_count"])
            report_progress(
                progress_callback,
                "walk_forward",
                substage=f"batch enfant {batch_number}/{effective_batch_count}",
                completed_units=completed_combinations,
                total_units=effective_plan.count(),
                details={
                    "child_run_id": item["child_run_id"],
                    "batch_index": item["batch_index"],
                    "child_batches": effective_batch_count,
                },
            )
        precomputed_batches = _ingest_child_checkpoints(
            repository, checkpoint, manifest
        )

    period = {
        "walk_forward_end_offset_sessions": int(
            spec.config.walk_forward_end_offset_sessions
        ),
        "effective_end_date": prepared.attrs.get("effective_end_date"),
    }
    extras: dict[str, object] = {
        **period,
        "combination_plan": {
            "version": effective_plan.plan_version,
            "sha256": effective_plan.plan_sha256,
            "raw_combination_count": raw_plan.count(),
            "prefiltered_combination_count": effective_plan.count(),
            "effective_batch_count": effective_batch_count,
        },
    }
    traceability = _persist_prepared_traceability(extras, prepared, spec)
    if prefilter is not None:
        extras["predictor_prefilter"] = {
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
            "telemetry": prefilter_telemetry,
        }
    result = run_streamed_walk_forward(
        prepared,
        generated,
        spec.config,
        checkpoint,
        output,
        market_calendars=calendars,
        evaluate_holdout=spec.evaluate_final_holdout,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        run_configuration_extras=extras,
        precomputed_walk_forward_batches=precomputed_batches,
        precomputed_combination_count=(
            effective_plan.count() if precomputed_batches is not None else None
        ),
    )
    if prefilter is not None:
        prefilter.metrics.to_csv(output / "predictor_prefilter.csv", index=False)
        (output / "predictor_prefilter.json").write_text(
            json.dumps(
                {
                    "score_formula": PREFILTER_SCORE_FORMULA,
                    "targets": prefilter.diagnostics,
                    "telemetry": prefilter_telemetry,
                },
                indent=2,
                ensure_ascii=False,
            )
            + "\n",
            encoding="utf-8",
        )
    return {
        "job_type": spec.job_type.value,
        "metrics": _json_value(result.aggregate_global.iloc[0].to_dict()),
        "eligible_combinations": int(result.qualification["Eligible"].sum()),
        "total_combinations": effective_plan.count(),
        "raw_combination_count": raw_plan.count(),
        "effective_batch_count": effective_batch_count,
        "result_files": sorted(path.name for path in output.iterdir()),
        **period,
        "traceability": traceability,
        "execution_telemetry": _json_value(result.telemetry),
        "walk_forward_protocol": _walk_forward_protocol_summary(spec.config),
        "checkpoint_manifest": "checkpoints/manifest.json",
        **(
            {"walk_forward_batch_manifest": "orchestration/walk_forward_batches.json"}
            if manifest is not None
            else {}
        ),
        **(
            {"predictor_prefilter": _json_value(prefilter.diagnostics)}
            if prefilter is not None
            else {}
        ),
    }


def _walk_forward_batch(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    if not spec.source_walk_forward_run:
        raise ValueError("WALK_FORWARD_BATCH requires its parent run")
    if spec.combination_range_start is None or spec.combination_range_stop is None:
        raise ValueError("WALK_FORWARD_BATCH requires a frozen range")
    repository = RunRepository(output.parent.parent)
    run_id = output.parent.name
    parent_id = spec.source_walk_forward_run
    manifest = load_manifest(repository, parent_id)
    if manifest is None:
        raise ValueError("Manifest du parent WALK_FORWARD_BATCH introuvable")
    matching = [
        item for item in manifest["batches"] if item["child_run_id"] == run_id
    ]
    if len(matching) != 1:
        raise ValueError("Réservation WALK_FORWARD_BATCH introuvable ou ambiguë")
    reservation = matching[0]
    if (
        int(reservation["range_start"]) != spec.combination_range_start
        or int(reservation["range_stop"]) != spec.combination_range_stop
        or str(reservation["expected_child_fingerprint"])
        != repository.configuration_fingerprint(run_id)
    ):
        raise ValueError("Identité WALK_FORWARD_BATCH incompatible avec le manifest")
    parent_spec = repository.load_spec(parent_id)
    parent_checkpoint = _walk_forward_checkpoint(repository, parent_id, parent_spec)
    prepared, preparation = parent_checkpoint.load_snapshot()
    plan = CombinationPlan.from_dict(
        parent_checkpoint.load_artifact("effective_combination_plan")
    )
    if (
        plan.plan_version != spec.combination_plan_version
        or plan.plan_sha256 != spec.combination_plan_sha256
        or plan.plan_sha256 != manifest["combination_plan_sha256"]
    ):
        raise ValueError("CombinationPlan enfant incompatible avec le parent")
    generated = plan.slice(
        spec.combination_range_start, spec.combination_range_stop
    )
    checkpoint = _walk_forward_checkpoint(repository, run_id, spec)
    batch_summary = run_streamed_walk_forward_batch(
        prepared,
        generated,
        spec.config,
        checkpoint,
        market_calendars=dict(preparation["calendars"]),
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    output.mkdir(parents=True, exist_ok=True)
    (output / "batch.json").write_text(
        json.dumps(
            {
                **batch_summary,
                "range_start": spec.combination_range_start,
                "range_stop": spec.combination_range_stop,
                "combination_plan_sha256": plan.plan_sha256,
            },
            indent=2,
            ensure_ascii=False,
        )
        + "\n",
        encoding="utf-8",
    )
    return {
        "job_type": spec.job_type.value,
        **batch_summary,
        "range_start": spec.combination_range_start,
        "range_stop": spec.combination_range_stop,
        "result_files": ["batch.json"],
    }


def _resolve_threshold_calibration_config(
    spec: ExperimentSpec,
) -> tuple[Any, str]:
    frozen = spec.frozen_threshold_calibration_parameters
    source = "frozen_snapshot"
    if frozen is None and spec.source_threshold_parameter_calibration_run:
        runs = RunRepository(spec.config.project_root / "runs")
        source_run = spec.source_threshold_parameter_calibration_run
        status = runs.status(source_run)
        if status.get("job_type") != JobType.THRESHOLD_PARAMETER_CALIBRATION.value:
            raise ValueError(
                "Threshold parameter source must be a parameter calibration run"
            )
        if status.get("status") != "completed":
            raise ValueError("Threshold parameter calibration source is not completed")
        path = (
            runs.run_directory(source_run)
            / "results"
            / "selected_threshold_calibration_configuration.json"
        )
        try:
            selected = json.loads(path.read_text(encoding="utf-8"))
            frozen = dict(selected["parameters"])
        except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError) as error:
            raise ValueError(
                "Threshold parameter calibration selection is unavailable"
            ) from error
        source = "referenced_calibration"
    if frozen is None:
        return spec.config, "run_snapshot"
    try:
        parameters = ThresholdCalibrationParameters.from_dict(frozen)
        effective = parameters.apply(spec.config)
        validate_threshold_calibration_config(effective)
    except (KeyError, TypeError, ValueError) as error:
        raise ValueError("Frozen threshold calibration parameters are invalid") from error
    return effective, source


def _resolve_threshold_xgboost_parameters(
    spec: ExperimentSpec,
) -> DirectionalXGBoostParameters:
    """Resolve threshold-model parameters from the immutable experiment spec."""

    referenced: dict[str, Any] | None = None
    if (
        spec.frozen_xgboost_parameters is None
        and spec.source_xgboost_calibration_run is not None
    ):
        runs = RunRepository(spec.config.project_root / "runs")
        source_run = spec.source_xgboost_calibration_run
        status = runs.status(source_run)
        if status.get("job_type") != JobType.XGBOOST_CALIBRATION.value:
            raise ValueError("XGBoost parameter source must be a calibration run")
        if status.get("status") != "completed":
            raise ValueError("XGBoost parameter source is not completed")
        path = runs.run_directory(source_run) / "results" / "selected_configurations.json"
        try:
            referenced = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise ValueError("XGBoost calibration selections are unavailable") from error
    legacy = (
        EXPERIMENTAL_XGBOOST_PARAMETERS
        if spec.xgboost_resolution_version < 1
        and spec.frozen_xgboost_parameters is None
        and spec.source_xgboost_calibration_run is None
        else None
    )
    return resolve_directional_xgboost_parameters(
        spec.config,
        frozen=spec.frozen_xgboost_parameters,
        referenced=referenced,
        legacy_fallback=legacy,
    )


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
    prediction_service = DailyPredictionService(repository)
    backfilled_predictions = prediction_service.backfill(
        prepared,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
    )
    current_predictions = prediction_service.generate(
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
    )
    predictions = pd.concat(
        [backfilled_predictions, current_predictions], ignore_index=True
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
    prediction_service = DailyPredictionService(repository)
    backfilled_predictions = prediction_service.backfill(
        prepared,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
    )
    current_predictions = prediction_service.generate(
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
    )
    predictions = pd.concat(
        [backfilled_predictions, current_predictions], ignore_index=True
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


def _end_to_end(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    return run_end_to_end(
        spec,
        output,
        progress_callback,
        cancellation_check,
        execute_reserved_child=_execute_reserved_child,
        phase_callback=_phase,
    )


def _forced_candidate_validation(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    return run_forced_candidate_validation(
        spec,
        output,
        progress_callback,
        cancellation_check,
        execute_reserved_child=_execute_reserved_child,
        phase_callback=_phase,
    )


@dataclass(slots=True)
class WorkflowRegistry:
    handlers: dict[JobType, WorkflowHandler]

    @classmethod
    def production(cls) -> "WorkflowRegistry":
        return cls(
            {
                JobType.WALK_FORWARD: _walk_forward,
                JobType.WALK_FORWARD_BATCH: _walk_forward_batch,
                JobType.XGBOOST_CALIBRATION: _xgboost_calibration,
                JobType.THRESHOLD_PARAMETER_CALIBRATION: (
                    _threshold_parameter_calibration
                ),
                JobType.THRESHOLD_CALIBRATION: _threshold_calibration,
                JobType.FIXED_CANDIDATE_EVALUATION: (
                    _fixed_candidate_evaluation
                ),
                JobType.FORCED_CANDIDATE_VALIDATION: (
                    _forced_candidate_validation
                ),
                JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC: (
                    _qualification_holdout_diagnostic
                ),
                JobType.PRODUCTION_TRAINING: _production_training,
                JobType.MARKET_UPDATE: _market_update,
                JobType.DAILY_PREDICTION: _daily_prediction,
                JobType.DAILY_SCREENING: _daily_screening,
                JobType.REALIZED_VALIDATION: _realized_validation,
                JobType.OPERATIONAL_RUN: _operational_run,
                JobType.END_TO_END: _end_to_end,
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
