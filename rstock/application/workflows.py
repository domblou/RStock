"""Workflow adapters shared by the Laboratory worker and future CLI execution."""

from __future__ import annotations

import json
import hashlib
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
from rstock.checkpoints import CheckpointIncompatibleError, CheckpointManager
from rstock.combinations import (
    canonical_combination_id_from_set, generate_symbol_sets,
    generate_target_symbol_sets, symbol_set_id,
)
from rstock.combination_planning import CombinationPlan, build_combination_plan
from rstock.config import RStockConfig
from rstock.features import prepare_dataset, require_complete_last_session
from rstock.market_cache import market_data_service
from rstock.modeling import (
    DirectionalXGBoostParameters,
    prefilter_xgboost_snapshot,
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
    holdout_combination_counts,
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
from rstock.traceability import (
    prepared_dataset_traceability,
    verify_prepared_dataset_digest,
)
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
from .forward_simulation import run_forward_simulation
from .history_analysis import MIN_HOLDOUT_SIGNALS, threshold_promotion_guidance
from .orchestration_runtime import execute_child
from .processes import process_alive
from .production_repository import ProductionRepository
from .production_quality_runtime import synchronize_production_quality
from .production_quality_rebuild import ProductionQualityRebuildRunner
from .repository import RunRepository
from .prefilter_experiments import prefilter_checkpoint_batch_sizes
from .prefilter_stability import aggregate_temporal_prefilter, resolve_stability_origins
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
    if spec.source_prefilter_run and spec.job_type is JobType.WALK_FORWARD:
        from .prefilter_contract import load
        from .derived_snapshot import load_source_prepared_snapshot
        repository = RunRepository(spec.config.project_root / "runs")
        contract = load(repository, spec)
        return load_source_prepared_snapshot(
            repository, spec, source_run_id=spec.source_prefilter_run,
            source_job_type=JobType.PREDICTOR_PREFILTER,
            expected_snapshot_sha256=contract["prepared_snapshot_sha256"],
        )
    if (spec.job_type is JobType.WALK_FORWARD and spec.prefilter_execution_version >= 2
            and spec.config.predictor_prefilter_enabled
            and spec.forced_symbol_sets is None and spec.walk_forward_derivation is None):
        raise ValueError("Walk-forward requires an explicit Prefilter reference")
    if spec.prepared_snapshot_required:
        if spec.job_type is JobType.PREDICTOR_PREFILTER:
            from .prefilter_experiments import validate_prefilter_source

            prepared, predictor_symbols, target_symbols, calendars = (
                validate_prefilter_source(
                    RunRepository(spec.config.project_root / "runs"), spec
                )
            )
            _phase(progress_callback, "data_preparation", "completed",
                   symbols=len(predictor_symbols), rows=len(prepared),
                   elapsed_seconds=perf_counter() - phase_started_at)
            return prepared, predictor_symbols, target_symbols, calendars
        from .derived_snapshot import load_source_prepared_snapshot

        if spec.forced_period_lock is not None:
            source = spec.source_walk_forward_run
            if source == spec.forced_period_lock["temporal_walk_forward_run_id"]:
                expected = spec.forced_period_lock["prepared_snapshot_sha256"]
                snapshot_path = (spec.config.project_root / "runs" / str(source)
                                 / "checkpoints" / "artifacts" / "prepared_snapshot.pkl")
                if not snapshot_path.is_file() or hashlib.sha256(snapshot_path.read_bytes()).hexdigest() != expected:
                    raise ValueError("Forced temporal snapshot digest differs")
            else:
                source_spec = RunRepository(spec.config.project_root / "runs").load_spec(str(source))
                if source_spec.forced_period_lock != spec.forced_period_lock:
                    raise ValueError("Forced Walk-forward period contract differs")

        prepared, predictor_symbols, target_symbols, calendars = (
            load_source_prepared_snapshot(
                RunRepository(spec.config.project_root / "runs"), spec,
                expected_snapshot_sha256=(
                    spec.walk_forward_derivation["prepared_snapshot_sha256"]
                    if spec.walk_forward_derivation is not None else None
                ),
            )
        )
        if spec.forced_period_lock is not None:
            from .forced_period import validate_forced_period
            validate_forced_period(prepared, spec.forced_period_lock)
        _phase(
            progress_callback, "data_preparation", "completed",
            symbols=len(predictor_symbols), rows=len(prepared),
            elapsed_seconds=perf_counter() - phase_started_at,
        )
        return prepared, predictor_symbols, target_symbols, calendars
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
    if spec.job_type in {JobType.WALK_FORWARD, JobType.PREDICTOR_PREFILTER}:
        require_complete_last_session(prepared, downloaded.symbols)
    if spec.job_type is JobType.PREDICTOR_PREFILTER and (
        prepared.empty or prepared.index.max().date().isoformat()
        != spec.historical_data_cutoff
    ):
        raise ValueError("Predictor prefilter prepared data misses the requested cutoff")
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
    source_run_id = (
        spec.source_walk_forward_run
        or spec.source_experiment_run
        or spec.source_end_to_end_run
    )
    digest_verification = verify_prepared_dataset_digest(
        prepared,
        expected_digest=spec.source_prepared_dataset_sha256,
        required=spec.prepared_dataset_digest_required,
        run_id=spec.execution_run_id,
        source_run_id=source_run_id,
        cutoff=(
            None if effective_end_date is None else effective_end_date.isoformat()
        ),
        stage=spec.job_type.value,
    )
    prepared.attrs["prepared_dataset_digest_verification"] = digest_verification
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
        digest_verification=prepared.attrs.get(
            "prepared_dataset_digest_verification"
        ),
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


def _prefilter_qualification_config(config: RStockConfig) -> RStockConfig:
    """Apply the prefilter thresholds to its existing univariate evaluator."""
    return replace(
        config,
        qualification_min_median_auc=config.predictor_prefilter_min_median_auc,
        qualification_min_pct_windows_above_random=(
            config.predictor_prefilter_min_pct_above_random
        ),
        qualification_min_worst_window_auc=config.predictor_prefilter_min_worst_auc,
        qualification_max_auc_std=config.predictor_prefilter_max_auc_std,
    )


def _inherit_walk_forward_prefilter(
    spec: ExperimentSpec, output: Path, configuration: dict[str, object],
) -> None:
    """Keep inherited prefilter artifacts and training provenance from the source."""
    if spec.source_prefilter_run:
        from .prefilter_contract import load, CONTRACT
        repository = RunRepository(spec.config.project_root / "runs")
        contract = load(repository, spec)
        source = repository.run_directory(spec.source_prefilter_run) / "results"
        output.mkdir(parents=True, exist_ok=True)
        for filename in ("predictor_prefilter.csv", "predictor_prefilter.json", "prefilter_contract.json",
                         "prefilter_round_selection_training.csv", "prefilter_round_selection.json"):
            path = source / filename
            if path.is_file():
                (output / filename).write_bytes(path.read_bytes())
        configuration["predictor_prefilter"] = {
            "enabled": True, "source_prefilter_run": spec.source_prefilter_run,
            "contract_sha256": spec.source_prefilter_contract_sha256,
            "selection_sha256": contract["selection_sha256"],
            "selection_mode": contract["selection_mode"],
            "xgboost_parameters": prefilter_xgboost_snapshot(repository.load_spec(spec.source_prefilter_run).config),
            "effective_configuration": contract["effective_configuration"],
        }
        return
    if spec.walk_forward_derivation is None or not spec.config.predictor_prefilter_enabled:
        return
    source_id = str(spec.walk_forward_derivation["source_run_id"])
    repository = RunRepository(spec.config.project_root / "runs")
    source_results = repository.run_directory(source_id) / "results"
    output.mkdir(parents=True, exist_ok=True)
    for filename in ("predictor_prefilter.csv", "predictor_prefilter.json"):
        source_path = source_results / filename
        if source_path.is_file():
            (output / filename).write_bytes(source_path.read_bytes())
    source_config_path = source_results / "run_configuration.json"
    provenance: dict[str, object] = {}
    if source_config_path.is_file():
        provenance = dict(json.loads(source_config_path.read_text(encoding="utf-8"))
                          .get("predictor_prefilter", {}))
    if not provenance and not any(
        (source_results / filename).is_file()
        for filename in ("predictor_prefilter.csv", "predictor_prefilter.json")
    ):
        return
    source_spec = repository.load_spec(source_id)
    provenance.setdefault("xgboost_parameters", prefilter_xgboost_snapshot(source_spec.config))
    configuration["predictor_prefilter"] = {
        **provenance, "enabled": True, "source_walk_forward_run": source_id,
    }


def _ensure_prefilter_checkpoint_protocol(checkpoint: CheckpointManager, config: RStockConfig | None = None) -> None:
    """Reject only legacy prefilter work; full WF checkpoints remain compatible."""

    protocol_artifact = "prefilter_execution_protocol"
    if config is not None:
        from rstock.walk_forward import ensure_prefilter_training_policy
        ensure_prefilter_training_policy(checkpoint, config)
    if checkpoint.artifact_exists(protocol_artifact):
        if checkpoint.load_artifact(protocol_artifact) != PREFILTER_POLICY_VERSION:
            raise CheckpointIncompatibleError(
                "Checkpoint préfiltre incompatible avec le protocole Up-only. "
                "Relancez le Walk-forward depuis le début."
            )
        return
    has_legacy_prefilter = bool(
        checkpoint.completed_batch_ids("predictor_prefilter_walk_forward")
    ) or any(
        checkpoint.artifact_exists(name)
        for name in ("prefilter_qualification", "prefilter_selection")
    )
    if has_legacy_prefilter:
        raise CheckpointIncompatibleError(
            "Checkpoint préfiltre Up+Down incompatible avec le protocole "
            "Up-only. Relancez le Walk-forward depuis le début."
        )
    checkpoint.commit_artifact(protocol_artifact, PREFILTER_POLICY_VERSION)


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


def _validate_walk_forward_prefilter_input(spec: ExperimentSpec) -> None:
    if spec.source_prefilter_run:
        from .prefilter_contract import load
        load(RunRepository(spec.config.project_root / "runs"), spec)
    elif (spec.prefilter_execution_version >= 2 and spec.config.predictor_prefilter_enabled
          and spec.forced_symbol_sets is None and spec.walk_forward_derivation is None):
        raise ValueError("Walk-forward requires an explicit Prefilter reference")


def _walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    _validate_walk_forward_prefilter_input(spec)
    if output.name == "_working":
        return _resumable_walk_forward(
            spec, output, progress_callback, cancellation_check
        )
    prefilter = None
    prefilter_walk_forward_telemetry: dict[str, object] | None = None
    if spec.walk_forward_derivation is not None:
        from .walk_forward_experiments import load_frozen_walk_forward_candidates

        prepared, _, _, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check,
        )
        generated = load_frozen_walk_forward_candidates(
            RunRepository(spec.config.project_root / "runs"), spec,
        )
        if not isinstance(generated, pd.DataFrame):
            raise ValueError("Derived Walk-forward expected frozen generated sets")
    elif spec.forced_symbol_sets is not None:
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
    elif spec.source_prefilter_run:
        from .prefilter_contract import plan
        prepared, _, _, calendars = _prepared_inputs(spec, progress_callback, cancellation_check)
        effective = plan(RunRepository(spec.config.project_root / "runs"), spec)
        generated = effective.slice(0, effective.count())
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
        prefilter_config = _prefilter_qualification_config(spec.config)
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
        evaluate_holdout=spec.evaluate_final_holdout,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    holdout_policy = _end_to_end_walk_forward_holdout_policy(spec)
    if holdout_policy is not None:
        result.run_configuration["final_holdout_policy"] = holdout_policy
    period = _persist_walk_forward_period(result.run_configuration, prepared, spec.config)
    traceability = _persist_prepared_traceability(
        result.run_configuration, prepared, spec
    )
    if prefilter is not None:
        result.run_configuration["predictor_prefilter"] = {
            "enabled": True,
            "score_formula": PREFILTER_SCORE_FORMULA,
            "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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
    _inherit_walk_forward_prefilter(spec, output, result.run_configuration)
    write_walk_forward_results(result, output)
    if prefilter is not None:
        prefilter.metrics.to_csv(output / "predictor_prefilter.csv", index=False)
        (output / "predictor_prefilter.json").write_text(
            json.dumps(
                {
                    "score_formula": PREFILTER_SCORE_FORMULA,
                    "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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


def _persist_prefilter_training(output: Path, checkpoints, config: RStockConfig) -> dict[str, object]:
    """Export reconciled atomic batch audits through the existing result contract."""
    from rstock.modeling import RoundSelectionPolicy, round_selection_coverage
    frames = []
    origins = []
    for manager in checkpoints:
        if manager.artifact_exists("prefilter_round_selection_training"):
            frame = manager.load_artifact("prefilter_round_selection_training")
            frames.append(frame)
            if manager.artifact_exists("prefilter_round_selection_telemetry"):
                origins.append(manager.load_artifact("prefilter_round_selection_telemetry"))
    training = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    def losses(frame):
        # Descriptive OOS losses retain the existing overlapping-window weighting;
        # they are not an independent-sample significance test.
        metrics = {}
        if "TestObservations" not in frame:
            return metrics
        weights = pd.to_numeric(frame["TestObservations"], errors="coerce")
        for metric in ("UpLogLoss", "UpBrier"):
            if metric in frame:
                values = pd.to_numeric(frame[metric], errors="coerce")
                valid = values.notna() & weights.gt(0)
                if valid.any():
                    metrics[metric] = float((values[valid] * weights[valid]).sum() / weights[valid].sum())
        for metric in ("UpROCAUC", "UpPRAUC"):
            if metric in frame:
                values = pd.to_numeric(frame[metric], errors="coerce").dropna()
                if len(values):
                    metrics[metric + "Median"] = float(values.median())
                    metrics[metric + "Windows"] = int(len(values))
        return metrics
    summary = {
        "round_selection_policy": RoundSelectionPolicy.from_config(config, scope="prefilter").snapshot(),
        "round_selection_coverage": {**round_selection_coverage(training),
            "Down": {"available": False, "status": "not_applicable"}},
        "round_selection_origins": origins,
        "selection_worker_seconds": sum(item.get("selection_worker_seconds", 0) for item in origins),
        "refit_worker_seconds": sum(item.get("refit_worker_seconds", 0) for item in origins),
        "selection_rounds_run": sum(item.get("selection_rounds_run", 0) for item in origins),
        "refit_rounds": sum(item.get("refit_rounds", 0) for item in origins),
        "all_evaluated_windows": losses(training),
        "optimized_windows_only": losses(training.loc[training["UpRoundSelectionUsed"].eq(True)]) if "UpRoundSelectionUsed" in training else {},
        "comparison_status": "résultat exploratoire",
    }
    if config.prefilter_xgb_round_selection_mode == "fixed":
        for key in ("selection_rounds_run", "refit_rounds", "selection_worker_seconds", "refit_worker_seconds"):
            summary.pop(key)
    if config.prefilter_xgb_round_selection_mode == "chronological":
        output.mkdir(parents=True, exist_ok=True)
        # Schema remains readable even if every candidate was locally excluded.
        if training.empty and not len(training.columns):
            training = pd.DataFrame(columns=["Set", "Observation", "Predictors", "Window", "OriginCutoff", "UpRoundSelectionMode"])
        training.to_csv(output / "prefilter_round_selection_training.csv", index=False)
        (output / "prefilter_round_selection.json").write_text(
            json.dumps(_json_value(summary), indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return summary


def _predictor_prefilter(spec, output, progress_callback, cancellation_check):
    result = _execute_predictor_prefilter(spec, output, progress_callback, cancellation_check)
    from .prefilter_contract import publish
    publish(RunRepository(spec.config.project_root / "runs"), output.parent.name, spec, output)
    result["result_files"] = sorted(path.name for path in output.iterdir())
    return result


def _execute_predictor_prefilter(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    """Run only the existing univariate prefilter and persist its selection."""
    repository = RunRepository(output.parent.parent)
    run_id = output.parent.name
    if spec.prefilter_derivation is not None:
        # Check the dependency even if this run already has its own checkpoint.
        from .prefilter_experiments import validate_prefilter_source

        validate_prefilter_source(repository, spec)
    checkpoint = CheckpointManager(
        output.parent, run_id=run_id, job_type=spec.job_type.value,
        configuration_fingerprint=repository.configuration_fingerprint(
            run_id, fallback=spec.fingerprint,
        ),
        batch_sizes=prefilter_checkpoint_batch_sizes(spec.config),
    )
    _ensure_prefilter_checkpoint_protocol(checkpoint, spec.config)
    if checkpoint.artifact_exists("prepared_snapshot"):
        prepared, preparation = checkpoint.load_snapshot()
        predictor_symbols = list(preparation["predictor_symbols"])
        target_symbols = list(preparation["target_symbols"])
        calendars = dict(preparation["calendars"])
    else:
        checkpoint.phase_started("data_preparation")
        prepared, predictor_symbols, target_symbols, calendars = _prepared_inputs(
            spec, progress_callback, cancellation_check,
        )
        checkpoint.commit_snapshot(prepared, {
            "predictor_symbols": predictor_symbols,
            "target_symbols": target_symbols,
            "calendars": calendars,
            "effective_end_date": prepared.attrs.get("effective_end_date"),
        })
        checkpoint.phase_completed("data_preparation")
    as_of = prepared.index.max().date().isoformat()
    if as_of != spec.historical_data_cutoff:
        raise ValueError("Predictor prefilter snapshot differs from the frozen cutoff")
    if checkpoint.artifact_exists("prefilter_univariate_sets"):
        univariate_sets = checkpoint.load_artifact("prefilter_univariate_sets")
    else:
        checkpoint.phase_started("predictor_prefilter_generation")
        _phase(progress_callback, "predictor_prefilter_generation", "started")
        from .prefilter_experiments import inherited_prefilter_input_sets
        univariate_sets = inherited_prefilter_input_sets(repository, spec)
        if univariate_sets is None:
            univariate_sets = generate_symbol_sets(
                predictor_symbols, 1, target_symbols=target_symbols,
                max_sets=spec.config.max_generated_sets,
            )
        checkpoint.commit_artifact("prefilter_univariate_sets", univariate_sets)
        checkpoint.phase_completed("predictor_prefilter_generation")
        _phase(progress_callback, "predictor_prefilter_generation", "completed",
               combinations=len(univariate_sets))
    if spec.prefilter_method == "temporal_consensus":
        from .prefilter_consensus import execute_consensus
        return execute_consensus(spec, output, checkpoint, prepared, univariate_sets,
                                 predictor_symbols, target_symbols, calendars,
                                 progress_callback, cancellation_check)
    if spec.prefilter_method == "temporal_stability":
        return _temporal_stability_prefilter(
            spec, output, checkpoint, prepared, univariate_sets,
            predictor_symbols, target_symbols, calendars,
            progress_callback, cancellation_check,
        )
    univariate = evaluate_prefilter_walk_forward(
        prepared, univariate_sets, _prefilter_qualification_config(spec.config),
        market_calendars=calendars, progress_callback=progress_callback,
        cancellation_check=cancellation_check, checkpoint_manager=checkpoint,
    )
    checkpoint.commit_artifact("prefilter_qualification", univariate.qualification)
    checkpoint.phase_completed("predictor_prefilter_walk_forward")
    _require_exploitable_prefilter(univariate)
    if checkpoint.artifact_exists("prefilter_selection"):
        prefilter = checkpoint.load_artifact("prefilter_selection")
    else:
        checkpoint.phase_started("predictor_prefilter_selection")
        _phase(progress_callback, "predictor_prefilter_selection", "started")
        prefilter = select_predictors(
            univariate.qualification,
            prepared.iloc[:-spec.config.final_holdout_size],
            targets=target_symbols, candidate_symbols=predictor_symbols,
            config=spec.config,
            excluded_targets=getattr(univariate, "excluded_targets", {}),
        )
        checkpoint.commit_artifact("prefilter_selection", prefilter)
        checkpoint.phase_completed("predictor_prefilter_selection")
        _phase(progress_callback, "predictor_prefilter_selection", "completed",
               retained=sum(len(items) for items in prefilter.predictors_by_target.values()))
    output.mkdir(parents=True, exist_ok=True)
    prefilter.metrics.to_csv(output / "predictor_prefilter.csv", index=False)
    univariate.qualification.to_csv(output / "prefilter_qualification.csv", index=False)
    traceability = _persist_prepared_traceability({}, prepared, spec)
    manifest = {
        "schema_version": 1,
        "prepared_dataset_as_of": as_of,
        **_persist_prefilter_training(output, [checkpoint], spec.config),
        "prepared_dataset_sha256": traceability["prepared_dataset_sha256"],
        "snapshot_source": (None if spec.prefilter_derivation is None
                            else spec.prefilter_derivation["source_run_id"]),
        "source_snapshot_sha256": (None if spec.prefilter_derivation is None
                                   else spec.prefilter_derivation["prepared_snapshot_sha256"]),
        "score_formula": PREFILTER_SCORE_FORMULA,
        "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
        "predictors_by_target": {
            target: list(items) for target, items in prefilter.predictors_by_target.items()
        },
        "targets": prefilter.diagnostics,
        "telemetry": univariate.telemetry,
    }
    (output / "predictor_prefilter.json").write_text(
        json.dumps(_json_value(manifest), indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    return {
        "job_type": spec.job_type.value,
        "prepared_dataset_as_of": as_of,
        "traceability": traceability,
        "predictor_prefilter": _json_value(prefilter.diagnostics),
        "retained_predictors": sum(len(items) for items in prefilter.predictors_by_target.values()),
        "univariate_pairs": len(univariate.qualification),
        "result_files": sorted(path.name for path in output.iterdir()),
        "checkpoint_manifest": "checkpoints/manifest.json",
        **({"prefilter_derivation": spec.prefilter_derivation}
           if spec.prefilter_derivation is not None else {}),
    }


def _temporal_stability_prefilter(
    spec: ExperimentSpec, output: Path, checkpoint: CheckpointManager,
    prepared: pd.DataFrame, univariate_sets: pd.DataFrame,
    predictor_symbols: list[str], target_symbols: list[str],
    calendars: dict[str, str], progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    """Evaluate prior origins from the one frozen prepared snapshot."""
    origins = resolve_stability_origins(
        prepared, cutoff=str(spec.historical_data_cutoff), calendar=spec.calendar,
        origin_count=spec.stability_origin_count,
        step_sessions=spec.stability_step_sessions,
        symbols=predictor_symbols,
    )
    repository = RunRepository(output.parent.parent)
    run_id = output.parent.name
    fingerprint = repository.configuration_fingerprint(run_id, fallback=spec.fingerprint)
    origin_tables: list[pd.DataFrame] = []
    qualifications: list[pd.DataFrame] = []
    telemetry: list[dict[str, object]] = []
    from .prefilter_progress import TemporalPrefilterProgress
    origin_checkpoints = []
    for origin in origins:
        check_cancellation(cancellation_check)
        date = origin.date().isoformat()
        origin_checkpoint = CheckpointManager(
            output.parent / "checkpoints" / "temporal_origins" / date,
            run_id=f"{run_id}:origin:{date}",
            job_type="predictor_prefilter_origin",
            configuration_fingerprint=hashlib.sha256(
                f"{fingerprint}:{date}".encode("utf-8")
            ).hexdigest(),
            batch_sizes=prefilter_checkpoint_batch_sizes(spec.config),
        )
        _ensure_prefilter_checkpoint_protocol(origin_checkpoint, spec.config)
        origin_checkpoints.append(origin_checkpoint)
    progress = TemporalPrefilterProgress(
        progress_callback, [o.date().isoformat() for o in origins], len(univariate_sets),
        origin_checkpoints, spec.config.predictor_prefilter_batch_size,
    )
    for index, origin in enumerate(origins):
        check_cancellation(cancellation_check)
        date = origin.date().isoformat()
        origin_view = prepared.loc[:origin].copy()
        origin_view.attrs["effective_end_date"] = origin.isoformat()
        origin_checkpoint = origin_checkpoints[index]
        origin_progress = progress.origin_callback(index)
        if origin_checkpoint.artifact_exists("prefilter_selection"):
            selection = origin_checkpoint.load_artifact("prefilter_selection")
            qualification = origin_checkpoint.load_artifact("prefilter_qualification")
            origin_telemetry = origin_checkpoint.load_artifact("prefilter_telemetry")
        else:
            univariate = evaluate_prefilter_walk_forward(
                origin_view, univariate_sets, _prefilter_qualification_config(spec.config),
                market_calendars=calendars, progress_callback=origin_progress,
                cancellation_check=cancellation_check,
                checkpoint_manager=origin_checkpoint,
            )
            _require_exploitable_prefilter(univariate)
            qualification = univariate.qualification
            origin_telemetry = univariate.telemetry
            selection = select_predictors(
                qualification, origin_view.iloc[:-spec.config.final_holdout_size],
                targets=target_symbols, candidate_symbols=predictor_symbols,
                config=spec.config,
                excluded_targets=getattr(univariate, "excluded_targets", {}),
            )
            origin_checkpoint.commit_artifact("prefilter_qualification", qualification)
            origin_checkpoint.commit_artifact("prefilter_telemetry", origin_telemetry)
            origin_checkpoint.commit_artifact("prefilter_selection", selection)
        progress.origin_completed(index)
        metrics = selection.metrics.copy()
        metrics["OriginCutoff"] = date
        origin_tables.append(metrics)
        qualified = qualification.copy()
        qualified["OriginCutoff"] = date
        qualifications.append(qualified)
        telemetry.append({"origin_cutoff": date, **origin_telemetry})
    progress.finish()
    checkpoint.phase_completed("predictor_prefilter_walk_forward")
    aggregate, details, retained = aggregate_temporal_prefilter(
        origin_tables, targets=target_symbols, predictors=predictor_symbols,
        config=spec.config,
        principal_development=prepared.iloc[:-spec.config.final_holdout_size],
    )
    checkpoint.phase_completed("predictor_prefilter_selection")
    output.mkdir(parents=True, exist_ok=True)
    aggregate.to_csv(output / "predictor_prefilter.csv", index=False)
    details.to_csv(output / "predictor_prefilter_origins.csv", index=False)
    pd.concat(qualifications, ignore_index=True).to_csv(
        output / "prefilter_qualification.csv", index=False,
    )
    traceability = _persist_prepared_traceability({}, prepared, spec)
    manifest = {
        "schema_version": 1,
        "prefilter_method": "temporal_stability",
        **_persist_prefilter_training(output, origin_checkpoints, spec.config),
        "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
        "stability_origin_count": spec.stability_origin_count,
        "stability_step_sessions": spec.stability_step_sessions,
        "origin_cutoffs": [origin.date().isoformat() for origin in origins],
        "rank_definition": "PrefilterScoreRank among evaluable univariate predictors",
        "top_n_frequency_definition": (
            "Fraction of origins eligible and within Top-N before correlation"
        ),
        "prepared_dataset_as_of": spec.historical_data_cutoff,
        "prepared_dataset_sha256": traceability["prepared_dataset_sha256"],
        "snapshot_source": (None if spec.prefilter_derivation is None
                            else spec.prefilter_derivation["source_run_id"]),
        "source_snapshot_sha256": (None if spec.prefilter_derivation is None
                                   else spec.prefilter_derivation["prepared_snapshot_sha256"]),
        "predictors_by_target": {
            target: list(items) for target, items in retained.items()
        },
        "telemetry": telemetry,
    }
    (output / "predictor_prefilter.json").write_text(
        json.dumps(_json_value(manifest), indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    return {
        "job_type": spec.job_type.value,
        "prefilter_method": spec.prefilter_method,
        "origin_cutoffs": manifest["origin_cutoffs"],
        "prepared_dataset_as_of": spec.historical_data_cutoff,
        "traceability": traceability,
        "retained_predictors": sum(len(items) for items in retained.values()),
        "univariate_pairs": len(details),
        "result_files": sorted(path.name for path in output.iterdir()),
        "checkpoint_manifest": "checkpoints/manifest.json",
        **({"prefilter_derivation": spec.prefilter_derivation}
           if spec.prefilter_derivation is not None else {}),
    }


def _end_to_end_walk_forward_holdout_policy(spec: ExperimentSpec) -> str | None:
    if not spec.source_end_to_end_run or spec.evaluate_final_holdout:
        return None
    if spec.forced_period_lock is not None:
        return "delegated_to_fixed_candidate_evaluation"
    if spec.pipeline_version >= 3:
        return "delegated_to_end_to_end_holdout_evaluation"
    return "delegated_to_threshold_calibration"


def _resumable_walk_forward(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    _validate_walk_forward_prefilter_input(spec)
    """Production walk-forward path backed by versioned run checkpoints."""

    if (
        spec.config.walk_forward_max_combinations_per_batch is not None
        and spec.forced_symbol_sets is None
        and not (
            spec.walk_forward_derivation is not None
            and spec.walk_forward_derivation.get("candidate_artifact") == "generated_sets"
        )
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
        if spec.forced_period_lock is not None:
            from .forced_period import validate_forced_period
            validate_forced_period(prepared, spec.forced_period_lock)
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
    if spec.walk_forward_derivation is not None:
        from .walk_forward_experiments import load_frozen_walk_forward_candidates

        source_generated = load_frozen_walk_forward_candidates(
            RunRepository(spec.config.project_root / "runs"), spec,
        )
        if not isinstance(source_generated, pd.DataFrame):
            raise ValueError("Derived Walk-forward expected frozen generated sets")
        if checkpoint.artifact_exists("generated_sets"):
            generated = checkpoint.load_artifact("generated_sets")
            if not generated.equals(source_generated):
                raise ValueError("Derived Walk-forward candidates differ from source")
        else:
            checkpoint.commit_artifact("generated_sets", source_generated)
            generated = source_generated
    elif spec.forced_symbol_sets is not None:
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
    elif spec.source_prefilter_run:
        from .prefilter_contract import plan
        effective = plan(RunRepository(spec.config.project_root / "runs"), spec)
        generated = effective.slice(0, effective.count())
        if checkpoint.artifact_exists("generated_sets"):
            if not generated.equals(checkpoint.load_artifact("generated_sets")):
                raise ValueError("Frozen Prefilter candidates changed")
        else:
            checkpoint.commit_artifact("generated_sets", generated)
    elif spec.config.predictor_prefilter_enabled:
        _ensure_prefilter_checkpoint_protocol(checkpoint, spec.config)
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
        prefilter_config = _prefilter_qualification_config(spec.config)
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
    if spec.walk_forward_derivation is not None:
        extras["walk_forward_derivation"] = spec.walk_forward_derivation
    holdout_policy = _end_to_end_walk_forward_holdout_policy(spec)
    if holdout_policy is not None:
        extras["final_holdout_policy"] = holdout_policy
    traceability = _persist_prepared_traceability(extras, prepared, spec)
    if prefilter is not None:
        extras["predictor_prefilter"] = {
            "enabled": True,
            "score_formula": PREFILTER_SCORE_FORMULA,
            "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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
    _inherit_walk_forward_prefilter(spec, output, extras)
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
                    "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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
        **({"walk_forward_derivation": spec.walk_forward_derivation}
           if spec.walk_forward_derivation is not None else {}),
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
    checkpoint_root = output.parent if output.name == "_working" else output
    checkpoint = CheckpointManager(
        checkpoint_root,
        run_id=checkpoint_root.name,
        job_type=spec.job_type.value,
        configuration_fingerprint=spec.fingerprint,
        batch_sizes={"xgboost_calibration": spec.config.walk_forward_batch_size},
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
        checkpoint_manager=checkpoint,
        evaluate_final_holdout=(not spec.source_end_to_end_run
                               or spec.e2e_xgboost_protocol_version == 1),
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
    if spec.experimental_overrides:
        result.run_configuration["experimental_overrides"] = list(
            spec.experimental_overrides
        )
        result.run_configuration["threshold_parameter_selection_source"] = (
            spec.source_threshold_parameter_calibration_run
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


def _stage_source_results(spec: ExperimentSpec, run_id: str | None, job_type: JobType) -> Path:
    if not run_id:
        raise ValueError(f"Missing {job_type.value} source run")
    repository = RunRepository(spec.config.project_root / "runs")
    status = repository.status(run_id)
    if status.get("job_type") != job_type.value or status.get("status") != JobStatus.COMPLETED.value:
        raise ValueError(f"Incomplete or incompatible {job_type.value} source: {run_id}")
    return repository.run_directory(run_id) / "results"


def _file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _holdout_evaluation(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    calibration = _stage_source_results(
        spec, spec.source_threshold_calibration_run, JobType.THRESHOLD_CALIBRATION
    )
    selection_path = calibration / "selected_thresholds_by_set.json"
    selected = json.loads(selection_path.read_text(encoding="utf-8"))
    if selected != spec.frozen_selected_thresholds_by_set:
        raise ValueError("Frozen threshold selection differs from calibration source")
    sampled_path = calibration / "sampled_combinations.csv"
    sampled = pd.read_csv(sampled_path)
    calibration_config = json.loads((calibration / "run_configuration.json").read_text(encoding="utf-8"))
    prepared, *_ = _prepared_inputs(spec, progress_callback, cancellation_check)
    development, holdout, holdout_start = split_development_holdout(
        prepared, int(calibration_config["final_holdout_size"])
    )
    if (
        development.index.max().isoformat() != calibration_config["development_end"]
        or holdout_start.isoformat() != calibration_config["final_holdout_start"]
    ):
        raise ValueError("Holdout period differs from frozen calibration boundary")
    eligible = any(
        choice.get("status") == "selected" and choice.get("threshold") is not None
        for directions in selected.values() for choice in directions.values()
    )
    predictions = pd.DataFrame()
    metrics = pd.DataFrame(columns=[
        "Set", "Observation", "Direction", "Threshold", "SignalCount",
        "ROCAUC", "Precision", "DirectionalReturnMean", "OppositeMoveFrequency",
    ])
    _phase(progress_callback, "final_holdout", "started")
    if spec.evaluate_final_holdout and eligible:
        directional = _resolve_threshold_xgboost_parameters(spec)
        raw = generate_holdout_probabilities(
            development, holdout, sampled, spec.config,
            parameters_by_direction={"Up": directional.up, "Down": directional.down},
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
        predictions = apply_frozen_thresholds_by_set(raw, selected)
        metrics = evaluate_applied_thresholds(predictions, spec.config)
    missing = list(calibration_config["missing_frozen_thresholds"])
    counts = holdout_combination_counts(selected, predictions)
    _phase(progress_callback, "final_holdout", "completed",
           models=len(sampled) if spec.evaluate_final_holdout and eligible else 0,
           rows=len(predictions))
    _phase(progress_callback, "result_writing", "started")
    output.mkdir(parents=True, exist_ok=True)
    metrics.to_csv(output / "holdout_metrics.csv", index=False)
    if not predictions.empty:
        predictions.to_csv(output / "holdout_predictions.csv", index=False)
    configuration = {
        "protocol": "frozen_threshold_holdout_v1",
        "holdout_requested": spec.evaluate_final_holdout,
        "holdout_evaluated": spec.evaluate_final_holdout and eligible,
        "holdout_skipped_reason": (
            "disabled" if not spec.evaluate_final_holdout else
            "no_eligible_frozen_threshold" if not eligible else None
        ),
        "outcome": (
            "completed_no_eligible_threshold" if spec.evaluate_final_holdout and not eligible
            else "completed_partial_holdout" if spec.evaluate_final_holdout and missing
            else "completed"
        ),
        "missing_frozen_thresholds": missing,
        "holdout_combination_counts": counts,
        "missing_threshold_count_up": sum(item["direction"] == "Up" for item in missing),
        "missing_threshold_count_down": sum(item["direction"] == "Down" for item in missing),
        "development_end": calibration_config["development_end"],
        "final_holdout_start": calibration_config["final_holdout_start"],
        "final_holdout_size": calibration_config["final_holdout_size"],
        "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        "selected_thresholds_sha256": _file_sha256(selection_path),
        "sampled_combinations_sha256": _file_sha256(sampled_path),
        "source_walk_forward_run": spec.source_walk_forward_run,
        "source_prepared_dataset_sha256": spec.source_prepared_dataset_sha256,
    }
    from rstock.modeling import write_probability_training_audit
    write_probability_training_audit(predictions, output, configuration)
    configuration["traceability"] = _persist_prepared_traceability(
        {}, prepared, spec
    )
    (output / "run_configuration.json").write_text(
        json.dumps(configuration, indent=2) + "\n", encoding="utf-8"
    )
    _phase(progress_callback, "result_writing", "completed")
    return {
        "job_type": spec.job_type.value,
        "holdout_metrics": _json_value(metrics.to_dict("records")),
        "holdout_predictions_count": len(predictions),
        "outcome": configuration["outcome"],
        "run_configuration": configuration,
        "traceability": configuration["traceability"],
    }


def _promotion_qualification(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    from .auto_promotion import _promotion_guidance
    from .history_analysis import _promotion_reasons, promotion_policy

    if spec.forced_period_lock is not None:
        fixed = _stage_source_results(
            spec, spec.source_threshold_calibration_run,
            JobType.FIXED_CANDIDATE_EVALUATION,
        )
        walk = _stage_source_results(spec, spec.source_walk_forward_run, JobType.WALK_FORWARD)
        selected_path = fixed / "selected_thresholds_by_set.json"
        selected = json.loads(selected_path.read_text(encoding="utf-8"))
        if selected != spec.frozen_selected_thresholds_by_set:
            raise ValueError("Forced threshold selection differs from reference")
        qualification_path = walk / "qualification.csv"
        walk_rows = pd.read_csv(qualification_path)
        holdout_path = fixed / "holdout_metrics.csv"
        try:
            metrics = pd.read_csv(holdout_path)
        except pd.errors.EmptyDataError:
            metrics = pd.DataFrame()
        fixed_config_path = fixed / "run_configuration.json"
        fixed_config = json.loads(fixed_config_path.read_text(encoding="utf-8"))
        errors = fixed_config.get("candidate_errors", {})
        rows = []
        for set_name, direction in spec.forced_candidate_identities or ():
            relevant = walk_rows[walk_rows["Set"].astype(str) == set_name]
            wf_passed = (
                len(relevant) == 1
                and str(relevant.iloc[0].get("Eligible", False)).lower() in {"true", "1"}
            )
            metric_rows = (metrics[
                (metrics["Set"].astype(str) == set_name)
                & (metrics["Direction"].astype(str) == direction)
            ] if {"Set", "Direction"}.issubset(metrics.columns) else pd.DataFrame())
            metric = metric_rows.iloc[0] if len(metric_rows) == 1 else None
            thresholds = selected.get(set_name, {})
            selection = thresholds.get(direction, {})
            record = {
                "Combinaison": set_name,
                "canonical_combination_id": canonical_combination_id_from_set(set_name, direction),
                "Cible": str(relevant.iloc[0].get("Observation", "")) if len(relevant) == 1 else "",
                "Direction": direction,
                "Seuil calibré": selection.get("threshold"),
                "Signaux holdout": None if metric is None else metric.get("SignalCount"),
                "AUC holdout": None if metric is None else metric.get("ROCAUC"),
                "Précision holdout": None if metric is None else metric.get("Precision"),
                "Rendement directionnel moyen": None if metric is None else metric.get("DirectionalReturnMean"),
                "Fréquence mouvement opposé": None if metric is None else metric.get("OppositeMoveFrequency"),
                "walk_forward_passed": wf_passed,
            }
            reasons = _promotion_reasons(pd.Series(record), selected, spec.config)
            if direction != "Up":
                reasons.append("Direction Up requise pour la promotion")
            if not wf_passed:
                reasons.append("Walk-forward forcé non qualifié")
            if len(metric_rows) != 1:
                reasons.append("Métrique holdout forcée absente ou ambiguë")
            if set_name in errors:
                reasons.append(f"Évaluation forcée échouée : {errors[set_name]}")
            for required_direction in ("Up", "Down"):
                required = thresholds.get(required_direction, {})
                if required.get("status") != "selected" or required.get("threshold") is None:
                    reasons.append(f"Seuil {required_direction} requis absent")
            record.update(candidate=not reasons, reasons=reasons,
                          **{"Statut promotion": "Candidat" if not reasons else "Non candidat",
                             "Raison": " ; ".join(reasons) if reasons else "Tous les critères passent"})
            rows.append(record)
        candidates = sorted({row["Combinaison"] for row in rows if row["candidate"]})
        source_digests = {
            "selected_thresholds_by_set.json": _file_sha256(selected_path),
            "holdout_metrics.csv": _file_sha256(holdout_path),
            "run_configuration.json": _file_sha256(fixed_config_path),
            "walk_forward_qualification.csv": _file_sha256(qualification_path),
        }
        payload = {
            "schema_version": 2,
            "protocol": "forced_candidate_promotion_qualification_v1",
            "source_walk_forward_run": spec.source_walk_forward_run,
            "source_fixed_candidate_evaluation_run": spec.source_threshold_calibration_run,
            "source_threshold_calibration_run": spec.source_threshold_calibration_run,
            "source_holdout_evaluation_run": spec.source_holdout_evaluation_run,
            "source_artifact_digests": source_digests,
            "period_lock": spec.forced_period_lock,
            "policy_parameters": promotion_policy(spec.config),
            "candidate_sets": candidates,
            "decisions": _json_value(rows),
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "qualification.json").write_text(
            json.dumps(payload, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
        )
        return {"job_type": spec.job_type.value, "candidate_count": len(candidates), **payload}

    calibration = _stage_source_results(
        spec, spec.source_threshold_calibration_run, JobType.THRESHOLD_CALIBRATION
    )
    holdout = _stage_source_results(
        spec, spec.source_holdout_evaluation_run, JobType.HOLDOUT_EVALUATION
    )
    selected_path = calibration / "selected_thresholds_by_set.json"
    selected = json.loads(selected_path.read_text(encoding="utf-8"))
    guidance = _promotion_guidance(
        holdout, selected, promotion_config=spec.config,
        calibration_results=calibration,
        require_holdout=True,
    )
    decisions = []
    for _, row in guidance.iterrows():
        set_name = str(row["Combinaison"])
        reasons = _promotion_reasons(row, selected, spec.config)
        directional = selected.get(set_name, {})
        for direction in ("Up", "Down"):
            choice = directional.get(direction, {})
            if choice.get("status") != "selected" or choice.get("threshold") is None:
                reasons.append(f"Seuil {direction} requis absent")
        values = row.to_dict()
        values["canonical_combination_id"] = canonical_combination_id_from_set(
            set_name, str(values.get("Direction", "Up"))
        )
        values.update(candidate=not reasons, reasons=reasons,
                      **{"Statut promotion": "Candidat" if not reasons else "Non candidat",
                         "Raison": " ; ".join(reasons) if reasons else "Tous les critères passent"})
        decisions.append(values)
    rows = _json_value(decisions)
    candidates = sorted({str(row["Combinaison"]) for row in rows if row["candidate"]})
    source_digests = {
        "selected_thresholds_by_set.json": _file_sha256(selected_path),
        "threshold_metrics_by_set.csv": _file_sha256(calibration / "threshold_metrics_by_set.csv"),
        "holdout_metrics.csv": _file_sha256(holdout / "holdout_metrics.csv"),
    }
    if (holdout / "holdout_predictions.csv").is_file():
        source_digests["holdout_predictions.csv"] = _file_sha256(
            holdout / "holdout_predictions.csv"
        )
    payload = {
        "schema_version": 1,
        "protocol": "holdout_promotion_qualification_v1",
        "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        "source_holdout_evaluation_run": spec.source_holdout_evaluation_run,
        "source_artifact_digests": source_digests,
        "policy_parameters": promotion_policy(spec.config),
        "candidate_sets": candidates,
        "decisions": rows,
    }
    output.mkdir(parents=True, exist_ok=True)
    (output / "qualification.json").write_text(
        json.dumps(payload, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    return {"job_type": spec.job_type.value, "candidate_count": len(candidates), **payload}


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
    if (spec.forced_period_lock is not None
            and effective_config.final_holdout_size != spec.forced_period_lock["final_holdout_size"]):
        raise ValueError("Forced holdout size differs from frozen temporal period")
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
    from rstock.modeling import write_probability_training_audit
    write_probability_training_audit(holdout_predictions, output, run_configuration)
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
    if spec.walk_forward_derivation is not None:
        from .walk_forward_experiments import load_frozen_walk_forward_candidates

        repository = RunRepository(spec.config.project_root / "runs")
        source_id = spec.source_walk_forward_run
        effective_plan = load_frozen_walk_forward_candidates(repository, spec)
        if not isinstance(effective_plan, CombinationPlan):
            raise ValueError("Derived Walk-forward expected a frozen combination plan")
        source_spec = repository.load_spec(source_id)
        source_checkpoint = _walk_forward_checkpoint(repository, source_id, source_spec)
        raw_plan = CombinationPlan.from_dict(
            source_checkpoint.load_artifact("raw_combination_plan")
        )
        for name, plan in (
            ("raw_combination_plan", raw_plan),
            ("effective_combination_plan", effective_plan),
        ):
            if checkpoint.artifact_exists(name):
                persisted = CombinationPlan.from_dict(checkpoint.load_artifact(name))
                if persisted.plan_sha256 != plan.plan_sha256:
                    raise ValueError("Derived Walk-forward combination plan changed")
            else:
                checkpoint.commit_artifact(name, plan.to_dict())
        return (
            raw_plan, effective_plan, None, None, "inherited_v1",
            prefilter_digest(effective_plan.predictors_by_target),
        )
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
    if spec.source_prefilter_run:
        from .prefilter_contract import plan
        effective_plan = plan(RunRepository(spec.config.project_root / "runs"), spec)
        policy_version = "external_prefilter_v1"
        digest = prefilter_digest(effective_plan.predictors_by_target)
    elif spec.config.predictor_prefilter_enabled:
        policy_version = PREFILTER_POLICY_VERSION
        _ensure_prefilter_checkpoint_protocol(checkpoint, spec.config)
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
    _validate_walk_forward_prefilter_input(spec)
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
        if spec.walk_forward_derivation is not None:
            proposed["walk_forward_derivation"] = {
                "source_run_id": spec.walk_forward_derivation["source_run_id"],
                "prepared_snapshot_sha256": spec.walk_forward_derivation["prepared_snapshot_sha256"],
                "candidate_artifact": spec.walk_forward_derivation["candidate_artifact"],
                "candidate_sha256": spec.walk_forward_derivation["candidate_sha256"],
            }
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
    if spec.walk_forward_derivation is not None:
        extras["walk_forward_derivation"] = spec.walk_forward_derivation
    holdout_policy = _end_to_end_walk_forward_holdout_policy(spec)
    if holdout_policy is not None:
        extras["final_holdout_policy"] = holdout_policy
    traceability = _persist_prepared_traceability(extras, prepared, spec)
    if prefilter is not None:
        extras["predictor_prefilter"] = {
            "enabled": True,
            "score_formula": PREFILTER_SCORE_FORMULA,
            "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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
    _inherit_walk_forward_prefilter(spec, output, extras)
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
                    "xgboost_parameters": prefilter_xgboost_snapshot(spec.config),
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
        **({"walk_forward_derivation": spec.walk_forward_derivation}
           if spec.walk_forward_derivation is not None else {}),
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


def _refresh_simulation_benchmark(
    spec: ExperimentSpec, cancellation_check: CancellationCheck | None
) -> None:
    """Keep SPY current without adding it to the model feature population."""

    benchmark_universe = pd.DataFrame(
        {
            "Symbol": ["SPY"],
            "ProviderSymbol": ["SPY"],
            "Exchange": [spec.calendar],
            "Calendar": [spec.calendar],
        }
    )
    market_data_service(spec.config).get_market_data(
        benchmark_universe,
        spec.config.model_history_days,
        cancellation_check=cancellation_check,
    )


def _market_update(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    repository = ProductionRepository(spec.config.project_root)
    operational = OperationalUniverseService(repository).tracked()
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
    _refresh_simulation_benchmark(spec, cancellation_check)
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
    tracked = repository.tracked_models()
    operational = OperationalUniverseService(repository).current(tracked)
    if tuple(spec.symbols) != operational.symbols:
        raise ValueError(
            "Operational universe changed after submission; submit a new prediction"
        )
    if not tracked:
        raise ValueError("No tracked production model")
    source_history = [
        int(model.source_configuration.get("rstock_config", {}).get("model_history_days", 0))
        for model in tracked
    ]
    effective_config = replace(
        spec.config,
        lag_depth=max(model.lag_depth for model in tracked),
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
        models=tracked,
    )
    current_predictions = prediction_service.generate(
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
        models=tracked,
    )
    predictions = pd.concat(
        [backfilled_predictions, current_predictions], ignore_index=True
    )
    check_cancellation(cancellation_check)
    if not predictions.empty:
        repository.append_table(
            "predictions", predictions, key="prediction_id",
            expected_models=tuple(tracked),
        )
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
    predictions = repository.read_tracked_model_table("predictions")
    signals = ProductionSignalService(repository).screen(
        predictions, cancellation_check=cancellation_check,
        persist=False, restrict_to_active_models=False,
    )
    check_cancellation(cancellation_check)
    if not signals.empty:
        repository.append_table("signals", signals, key="signal_id")
    _phase(progress_callback, "screening", "completed", records=len(signals))
    signals.to_csv(output / "screening.csv", index=False)
    active_signals = signals[
        signals["model_status_at_prediction"].isin({"active", "legacy_unknown"})
    ] if not signals.empty else signals
    watching_signals = signals[
        signals["model_status_at_prediction"].eq("watching")
    ] if not signals.empty else signals
    return {
        "job_type": spec.job_type.value,
        "categories": _json_value(active_signals["category"].value_counts().to_dict())
        if not active_signals.empty else {},
        "watching_categories": _json_value(watching_signals["category"].value_counts().to_dict())
        if not watching_signals.empty else {},
    }


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
    tracked = repository.tracked_models()
    operational = OperationalUniverseService(repository).current(tracked)
    if tuple(spec.symbols) != operational.symbols:
        raise ValueError(
            "Operational universe changed after submission; submit a new operational run"
        )
    if not tracked:
        raise ValueError("No tracked production model")
    source_history = [
        int(model.source_configuration.get("rstock_config", {}).get("model_history_days", 0))
        for model in tracked
    ]
    effective_config = replace(
        spec.config,
        lag_depth=max(model.lag_depth for model in tracked),
        model_history_days=max([spec.config.model_history_days, *source_history]),
    )
    prepared, downloaded = _operational_prepared(
        spec,
        progress_callback,
        cancellation_check,
        preparation_config=effective_config,
        phase_name="market_update",
    )
    _refresh_simulation_benchmark(spec, cancellation_check)
    check_cancellation(cancellation_check)
    _phase(progress_callback, "daily_prediction", "started")
    prediction_service = DailyPredictionService(repository)
    backfilled_predictions = prediction_service.backfill(
        prepared,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
        models=tracked,
    )
    current_predictions = prediction_service.generate(
        prepared,
        effective_config,
        market_data=getattr(downloaded, "prices", None),
        persist=False,
        models=tracked,
    )
    predictions = pd.concat(
        [backfilled_predictions, current_predictions], ignore_index=True
    )
    _phase(progress_callback, "daily_prediction", "completed", predictions=len(predictions))
    check_cancellation(cancellation_check)
    _phase(progress_callback, "screening", "started")
    signals = ProductionSignalService(repository).screen(
        predictions, cancellation_check=cancellation_check,
        persist=False, restrict_to_active_models=False,
    )
    _phase(progress_callback, "screening", "completed", records=len(signals))
    check_cancellation(cancellation_check)
    _phase(progress_callback, "realized_validation", "started")
    market_store = market_data_service(spec.config).store
    evaluation_batch = RealizedResultService(repository).evaluate(
        market_store.read,
        cancellation_check=cancellation_check,
        additional_predictions=predictions,
    )
    realized = evaluation_batch.results
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
        repository.append_tables(updates, expected_models=tuple(tracked))
    # Operational events are durable before this derived phase begins.  An
    # exception below deliberately fails the run without rolling them back.
    check_cancellation(cancellation_check)
    _phase(progress_callback, "production_quality", "started")
    as_of_session = pd.Timestamp(downloaded.prices.index.max()).normalize()
    # Reconcile recent persisted backfills after an interruption between the
    # atomic history publication and quality publication. Source fingerprints
    # skip unchanged identities before canonical observation construction.
    recent_predictions = repository.read_tracked_model_table("predictions")
    if not recent_predictions.empty:
        recent_dates = pd.to_datetime(
            recent_predictions.get(
                "prediction_date", pd.Series(index=recent_predictions.index, dtype=str)
            ),
            errors="coerce",
        )
        recent_predictions = recent_predictions[
            recent_dates >= as_of_session - pd.Timedelta(days=29)
        ].copy()
    quality_summary = synchronize_production_quality(
        spec.config.project_root,
        candidate_predictions=recent_predictions,
        candidate_signals=signals,
        new_results=realized,
        evaluations=evaluation_batch.evaluations,
        as_of_session=as_of_session,
        cancellation_check=cancellation_check,
    )
    _phase(progress_callback, "production_quality", "completed", **quality_summary)
    predictions.to_csv(output / "predictions.csv", index=False)
    signals.to_csv(output / "screening.csv", index=False)
    realized.to_csv(output / "realized_results.csv", index=False)
    return {
        "job_type": spec.job_type.value, "updated_symbols": len(downloaded.symbols),
        "predictions": len(predictions),
        "signals": int((
            signals["category"].eq("bullish_signal")
            & signals["model_status_at_prediction"].isin({"active", "legacy_unknown"})
        ).sum()) if not signals.empty else 0,
        "watching_signals": int((
            signals["category"].eq("bullish_signal")
            & signals["model_status_at_prediction"].eq("watching")
        ).sum()) if not signals.empty else 0,
        "realized_results": len(realized),
        "production_quality": quality_summary,
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


def _forward_simulation(
    spec: ExperimentSpec,
    output: Path,
    progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    _phase(progress_callback, "forward_simulation", "started")
    result = run_forward_simulation(
        spec, output, cancellation_check=cancellation_check,
        progress_callback=progress_callback,
    )
    _phase(progress_callback, "forward_simulation", "completed", **result)
    return result


def _production_quality_rebuild(
    spec: ExperimentSpec, output: Path, progress_callback: ProgressCallback | None,
    cancellation_check: CancellationCheck | None,
) -> dict[str, Any]:
    run_id = output.parent.name
    runs = RunRepository(output.parent.parent)
    _phase(progress_callback, "production_quality_rebuild", "started")
    result = ProductionQualityRebuildRunner(
        runs, run_id, spec.config.project_root
    ).execute(pd.Timestamp.utcnow().normalize(), cancellation_check=cancellation_check)
    _phase(progress_callback, "production_quality_rebuild", "completed", **result)
    return result


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
                JobType.PREDICTOR_PREFILTER: _predictor_prefilter,
                JobType.WALK_FORWARD_BATCH: _walk_forward_batch,
                JobType.XGBOOST_CALIBRATION: _xgboost_calibration,
                JobType.THRESHOLD_PARAMETER_CALIBRATION: (
                    _threshold_parameter_calibration
                ),
                JobType.THRESHOLD_CALIBRATION: _threshold_calibration,
                JobType.HOLDOUT_EVALUATION: _holdout_evaluation,
                JobType.PROMOTION_QUALIFICATION: _promotion_qualification,
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
                JobType.FORWARD_SIMULATION: _forward_simulation,
                JobType.PRODUCTION_QUALITY_REBUILD: _production_quality_rebuild,
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
        result = handler(spec, output, progress_callback, cancellation_check)
        if spec.config.market_context_enabled:
            from .market_context_runtime import optional_context_diagnostic

            diagnostic = optional_context_diagnostic(spec, output)
            if diagnostic is not None:
                result["market_context_diagnostic"] = {
                    "status": diagnostic["status"],
                    "protocol_id": diagnostic.get("protocol_id"),
                    "reason": diagnostic.get("reason"),
                }
        if spec.job_type in {JobType.END_TO_END, JobType.FORWARD_SIMULATION}:
            from .selection_diagnostic import optional_selection_diagnostic
            selection_diagnostic = optional_selection_diagnostic(spec, output)
            if selection_diagnostic is not None:
                result["selection_diagnostic"] = {key: selection_diagnostic.get(key) for key in ("status", "reason", "protocol")}
        return result
