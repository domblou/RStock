"""Development-only calibration of the threshold-calibrator policy."""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass, replace
from datetime import datetime, timezone
from pathlib import Path
from time import perf_counter
from typing import Any, Mapping, Sequence

import numpy as np
import pandas as pd

from .calibration import split_development_holdout
from .calibration_sampling import (
    GLOBAL_STRATIFIED_V2,
    PER_TARGET_V1,
    global_stratified_v2_sample,
    per_target_v1_sample,
)
from .checkpoints import CheckpointManager, _atomic_json
from .config import RStockConfig
from .modeling import XGBoostParameters
from .progress import CancellationCheck, ProgressCallback, check_cancellation, report_progress
from .threshold_calibration import (
    generate_development_probabilities,
    select_thresholds_from_invariant,
    threshold_calibration_invariant,
    validate_threshold_calibration_config,
)


THRESHOLD_PARAMETER_FIELDS = (
    "threshold_calibration_min_signals_per_window",
    "threshold_calibration_min_robust_signals",
    "threshold_calibration_min_window_fraction",
    "threshold_calibration_precision_tolerance",
    "threshold_calibration_quantiles",
    "threshold_calibration_grid_decimals",
)

SELECTION_ORDER = (
    "EligibleModelPct desc",
    "PrecisionMedian desc",
    "F1Median desc",
    "WindowCoverageMedian desc",
    "PrecisionStdMedian asc",
    "DirectionalReturnMeanMedian desc",
    "DirectionalReturnStdMedian asc",
    "OppositeMoveFrequencyMedian asc",
    "TotalSignals desc",
    "Configuration asc",
)


@dataclass(frozen=True, slots=True)
class ThresholdCalibrationParameters:
    min_signals_per_window: int
    min_robust_signals: int
    min_window_fraction: float
    precision_tolerance: float
    quantiles: tuple[float, ...]
    grid_decimals: int

    @classmethod
    def from_config(cls, config: RStockConfig) -> "ThresholdCalibrationParameters":
        return cls(
            min_signals_per_window=config.threshold_calibration_min_signals_per_window,
            min_robust_signals=config.threshold_calibration_min_robust_signals,
            min_window_fraction=config.threshold_calibration_min_window_fraction,
            precision_tolerance=config.threshold_calibration_precision_tolerance,
            quantiles=tuple(config.threshold_calibration_quantiles),
            grid_decimals=config.threshold_calibration_grid_decimals,
        )

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "ThresholdCalibrationParameters":
        return cls(
            min_signals_per_window=int(values["threshold_calibration_min_signals_per_window"]),
            min_robust_signals=int(values["threshold_calibration_min_robust_signals"]),
            min_window_fraction=float(values["threshold_calibration_min_window_fraction"]),
            precision_tolerance=float(values["threshold_calibration_precision_tolerance"]),
            quantiles=tuple(float(value) for value in values["threshold_calibration_quantiles"]),
            grid_decimals=int(values["threshold_calibration_grid_decimals"]),
        )

    def as_dict(self) -> dict[str, object]:
        return {
            "threshold_calibration_min_signals_per_window": self.min_signals_per_window,
            "threshold_calibration_min_robust_signals": self.min_robust_signals,
            "threshold_calibration_min_window_fraction": self.min_window_fraction,
            "threshold_calibration_precision_tolerance": self.precision_tolerance,
            "threshold_calibration_quantiles": list(self.quantiles),
            "threshold_calibration_grid_decimals": self.grid_decimals,
        }

    def apply(self, config: RStockConfig) -> RStockConfig:
        return replace(
            config,
            threshold_calibration_min_signals_per_window=self.min_signals_per_window,
            threshold_calibration_min_robust_signals=self.min_robust_signals,
            threshold_calibration_min_window_fraction=self.min_window_fraction,
            threshold_calibration_precision_tolerance=self.precision_tolerance,
            threshold_calibration_quantiles=self.quantiles,
            threshold_calibration_grid_decimals=self.grid_decimals,
        )

    @property
    def grid_key(self) -> str:
        """Canonical identity of the raw, quantile-dependent calculation."""
        raw = json.dumps(
            {"quantiles": self.quantiles, "grid_decimals": self.grid_decimals},
            separators=(",", ":"),
        ).encode("utf-8")
        return hashlib.sha256(raw).hexdigest()[:16]

    @property
    def digest(self) -> str:
        canonical = json.dumps(self.as_dict(), sort_keys=True, separators=(",", ":"))
        return hashlib.sha256(canonical.encode()).hexdigest()


@dataclass(slots=True)
class ThresholdParameterCalibrationResult:
    tested_configurations: pd.DataFrame
    development_by_configuration: pd.DataFrame
    development_by_window: pd.DataFrame
    selected_configuration: dict[str, object]
    development_predictions: pd.DataFrame
    sampled_combinations: pd.DataFrame
    run_configuration: dict[str, object]


def _unique(candidates: Sequence[ThresholdCalibrationParameters]) -> list[ThresholdCalibrationParameters]:
    unique: list[ThresholdCalibrationParameters] = []
    for candidate in candidates:
        if candidate not in unique:
            unique.append(candidate)
    return unique


def default_threshold_parameter_candidates(
    config: RStockConfig,
) -> list[ThresholdCalibrationParameters]:
    """Return a small deterministic one-factor-at-a-time policy search."""

    baseline = ThresholdCalibrationParameters.from_config(config)
    half_window = max(1, baseline.min_signals_per_window // 2)
    higher_window = max(1, math.ceil(baseline.min_signals_per_window * 1.5))
    half_robust = max(1, baseline.min_robust_signals // 2)
    higher_robust = max(1, math.ceil(baseline.min_robust_signals * 1.5))
    compact_quantiles = (0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.975, 0.99)
    tail_quantiles = (0.50, 0.65, 0.75, 0.85, 0.90, 0.925, 0.95, 0.975, 0.99)
    candidates = [
        baseline,
        replace(baseline, min_signals_per_window=half_window),
        replace(baseline, min_signals_per_window=higher_window),
        replace(baseline, min_robust_signals=half_robust),
        replace(baseline, min_robust_signals=higher_robust),
        replace(baseline, min_window_fraction=0.75),
        replace(baseline, min_window_fraction=1.0),
        replace(baseline, precision_tolerance=0.0),
        replace(baseline, precision_tolerance=0.01),
        replace(baseline, quantiles=compact_quantiles),
        replace(baseline, quantiles=tail_quantiles),
    ]
    validated: list[ThresholdCalibrationParameters] = []
    for candidate in _unique(candidates):
        validate_threshold_calibration_config(candidate.apply(config))
        validated.append(candidate)
    return validated


def frozen_threshold_parameter_candidates(
    directory: Path, config: RStockConfig
) -> list[ThresholdCalibrationParameters]:
    """Freeze candidate inputs once so a resumed run cannot change its search."""

    path = directory / "threshold_parameter_candidates.json"
    if path.exists():
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
            candidates = [
                ThresholdCalibrationParameters.from_dict(values)
                for values in payload["candidates"]
            ]
        except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError) as error:
            raise ValueError("Frozen threshold parameter candidates are invalid") from error
        if not candidates:
            raise ValueError("Frozen threshold parameter candidates are empty")
        return candidates
    candidates = default_threshold_parameter_candidates(config)
    directory.mkdir(parents=True, exist_ok=True)
    _atomic_json(
        path,
        {
            "schema_version": 1,
            "generation": "deterministic_controlled_one_factor_at_a_time_v1",
            "candidates": [candidate.as_dict() for candidate in candidates],
        },
    )
    return candidates


def threshold_parameter_table(
    candidates: Sequence[ThresholdCalibrationParameters],
) -> pd.DataFrame:
    rows = []
    for number, candidate in enumerate(candidates):
        rows.append({
            "Configuration": "baseline" if number == 0 else f"candidate_{number:02d}",
            "Baseline": number == 0,
            "ConfigurationDigest": candidate.digest,
            **candidate.as_dict(),
        })
    frame = pd.DataFrame(rows)
    if frame["Configuration"].duplicated().any() or frame["ConfigurationDigest"].duplicated().any():
        raise ValueError("Threshold parameter candidate identifiers must be unique")
    return frame


def _median(rows: pd.DataFrame, column: str) -> float:
    values = pd.to_numeric(rows.get(column, pd.Series(dtype=float)), errors="coerce")
    return float(values.median()) if values.notna().any() else np.nan


def _evaluate_candidate(
    configuration: str,
    parameters: ThresholdCalibrationParameters,
    predictions: Mapping[str, object],
    config: RStockConfig,
) -> tuple[dict[str, object], list[pd.DataFrame]]:
    effective = parameters.apply(config)
    calibrations = {
        set_name: select_thresholds_from_invariant(invariant, effective)
        for set_name, invariant in predictions.items()
    }
    selected_rows: list[pd.Series] = []
    window_frames: list[pd.DataFrame] = []
    for set_name in sorted(calibrations):
        calibration = calibrations[set_name]
        window_frames.append(
            calibration.metrics_by_window.assign(
                Configuration=configuration, Set=str(set_name)
            )
        )
        winners = calibration.metrics_by_threshold[
            calibration.metrics_by_threshold["Selected"]
        ].copy()
        if not winners.empty:
            winners["Set"] = str(set_name)
            selected_rows.extend(row for _, row in winners.iterrows())
    winners = pd.DataFrame(selected_rows)
    models_evaluated = len(calibrations) * 2
    models_eligible = len(winners)
    return {
        "Configuration": configuration,
        "ConfigurationDigest": parameters.digest,
        "EligibleConfiguration": models_eligible > 0,
        "ModelsEvaluated": models_evaluated,
        "ModelsEligible": models_eligible,
        "EligibleModelPct": (
            models_eligible / models_evaluated if models_evaluated else 0.0
        ),
        "TotalSignals": int(
            pd.to_numeric(winners.get("TotalSignals", pd.Series(dtype=float)), errors="coerce")
            .fillna(0)
            .sum()
        ),
        "PrecisionMedian": _median(winners, "Precision"),
        "F1Median": _median(winners, "F1Median"),
        "WindowCoverageMedian": _median(winners, "WindowCoverage"),
        "PrecisionStdMedian": _median(winners, "PrecisionStd"),
        "DirectionalReturnMeanMedian": _median(winners, "DirectionalReturnMean"),
        "DirectionalReturnStdMedian": _median(winners, "DirectionalReturnMeanStd"),
        "OppositeMoveFrequencyMedian": _median(winners, "OppositeMoveFrequency"),
        "Selected": False,
        "Rank": 0,
        "SelectionReason": None,
        "RejectionReason": None if models_eligible else "no_eligible_models",
    }, window_frames


def rank_threshold_parameter_configurations(metrics: pd.DataFrame) -> pd.DataFrame:
    """Apply an explicit stable lexicographic development-only ranking."""

    ranked = metrics.sort_values(
        [
            "EligibleModelPct", "PrecisionMedian", "F1Median",
            "WindowCoverageMedian", "PrecisionStdMedian",
            "DirectionalReturnMeanMedian", "DirectionalReturnStdMedian",
            "OppositeMoveFrequencyMedian", "TotalSignals", "Configuration",
        ],
        ascending=[False, False, False, False, True, False, True, True, False, True],
        na_position="last",
        kind="stable",
    ).reset_index(drop=True)
    ranked["Rank"] = np.arange(1, len(ranked) + 1)
    ranked.loc[0, "Selected"] = True
    ranked.loc[0, "SelectionReason"] = (
        "maximum_eligible_model_fraction_then_development_robustness_order"
    )
    rejected = ranked.index[1:]
    ranked.loc[rejected, "RejectionReason"] = ranked.loc[rejected, "RejectionReason"].fillna(
        "ranked_below_selected_configuration"
    )
    return ranked


def run_threshold_parameter_calibration(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    xgboost_parameters_by_direction: Mapping[str, XGBoostParameters],
    xgboost_parameter_source: str,
    combinations_per_target: int = 3,
    sampling_policy: str = PER_TARGET_V1,
    max_directional_models: int | None = None,
    candidates: Sequence[ThresholdCalibrationParameters] | None = None,
    source_parent_run: str | None = None,
    source_walk_forward_run: str | None = None,
    source_xgboost_calibration_run: str | None = None,
    frozen_xgboost_parameters_sha256: str | None = None,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    checkpoint_manager: CheckpointManager | None = None,
) -> ThresholdParameterCalibrationResult:
    """Select a calibrator policy using development data and never inspect holdout."""

    total_started_at = perf_counter()
    validate_threshold_calibration_config(config)
    candidate_list = list(candidates or default_threshold_parameter_candidates(config))
    if not candidate_list:
        raise ValueError("At least one threshold parameter candidate is required")
    tested = threshold_parameter_table(candidate_list)
    if sampling_policy == GLOBAL_STRATIFIED_V2:
        if max_directional_models is not None and (
            max_directional_models < 2 or max_directional_models % 2
        ):
            raise ValueError(
                "max_directional_models must be null or an even integer >= 2"
            )
        set_cap = (
            None
            if max_directional_models is None
            else max_directional_models // 2
        )
        sample = global_stratified_v2_sample(
            generated_sets,
            cap=set_cap,
            seed=config.xgb_seed,
            directional_model_cap=max_directional_models,
        )
    elif sampling_policy == PER_TARGET_V1:
        sample = per_target_v1_sample(
            generated_sets,
            per_target=combinations_per_target,
            seed=config.xgb_seed,
        )
    else:
        raise ValueError(f"Unsupported calibration sampling policy: {sampling_policy}")
    sampled = sample.combinations
    development, _, holdout_start = split_development_holdout(
        prepared, config.final_holdout_size
    )
    probability_started_at = perf_counter()
    report_progress(
        progress_callback, "walk_forward", substage="started",
        details={"phase_event": "started", "combinations": len(sampled)},
    )
    sample_ids = [str(value) for value in sample.manifest["selected_set_ids"]]
    prediction_key = hashlib.sha256("|".join(sample_ids).encode("utf-8")).hexdigest()
    prediction_reused = False
    if checkpoint_manager is not None and checkpoint_manager.artifact_exists("development_probabilities"):
        stored = checkpoint_manager.load_artifact("development_probabilities")
        if stored.get("sample_key") != prediction_key:
            raise ValueError("Checkpoint des probabilités incompatible avec l'échantillon")
        predictions = stored["predictions"]
        prediction_reused = True
    else:
        predictions = generate_development_probabilities(
            development, sampled, config,
            min_train_size=config.walk_forward_min_train_size,
            test_size=config.walk_forward_test_size,
            step_size=config.walk_forward_step_size,
            parameters_by_direction=xgboost_parameters_by_direction,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
        if checkpoint_manager is not None:
            checkpoint_manager.commit_artifact(
                "development_probabilities", {"sample_key": prediction_key, "predictions": predictions}
            )
    report_progress(
        progress_callback, "walk_forward", substage="completed",
        details={"phase_event": "completed", "probability_rows": len(predictions)},
    )
    probability_elapsed_seconds = perf_counter() - probability_started_at
    grid_started_at = perf_counter()
    invariants_by_grid: dict[str, Mapping[str, object]] = {}
    grid_reused = 0
    grid_timings: dict[str, float] = {}
    for parameters in candidate_list:
        if parameters.grid_key in invariants_by_grid:
            continue
        artifact = f"threshold_grid_{parameters.grid_key}"
        if checkpoint_manager is not None and checkpoint_manager.artifact_exists(artifact):
            payload = checkpoint_manager.load_artifact(artifact)
            if payload.get("prediction_key") == prediction_key:
                invariants_by_grid[parameters.grid_key] = payload["invariant"]
                grid_reused += 1
                grid_timings[parameters.grid_key] = 0.0
                continue
        effective = parameters.apply(config)
        started_at = perf_counter()
        invariant = {
            str(set_name): threshold_calibration_invariant(group, effective)
            for set_name, group in predictions.groupby("Set", sort=True)
        }
        grid_timings[parameters.grid_key] = perf_counter() - started_at
        invariants_by_grid[parameters.grid_key] = invariant
        if checkpoint_manager is not None:
            checkpoint_manager.commit_artifact(
                artifact, {"prediction_key": prediction_key, "invariant": invariant}
            )
    grid_elapsed_seconds = perf_counter() - grid_started_at
    rows: list[dict[str, object]] = []
    window_frames: list[pd.DataFrame] = []
    reused_candidates = 0
    selection_started_at = perf_counter()
    selection_timings: dict[str, float] = {}
    report_progress(progress_callback, "metrics", substage="started", details={"phase_event": "started"})
    for number, ((_, tested_row), parameters) in enumerate(
        zip(tested.iterrows(), candidate_list, strict=True), start=1
    ):
        check_cancellation(cancellation_check)
        artifact = f"parameter_candidate_{str(tested_row['Configuration'])}"
        if checkpoint_manager is not None and checkpoint_manager.artifact_exists(artifact):
            payload = checkpoint_manager.load_artifact(artifact)
            row, frames = payload["row"], payload["frames"]
            reused_candidates += 1
            selection_timings[str(tested_row["Configuration"])] = 0.0
        else:
            started_at = perf_counter()
            row, frames = _evaluate_candidate(
                str(tested_row["Configuration"]), parameters,
                invariants_by_grid[parameters.grid_key], config,
            )
            if checkpoint_manager is not None:
                checkpoint_manager.commit_artifact(artifact, {"row": row, "frames": frames})
            selection_timings[str(tested_row["Configuration"])] = perf_counter() - started_at
        rows.append(row)
        window_frames.extend(frames)
        report_progress(
            progress_callback, "metrics", substage=str(tested_row["Configuration"]),
            completed_units=number, total_units=len(candidate_list),
        )
    ranked = rank_threshold_parameter_configurations(pd.DataFrame(rows))
    selection_elapsed_seconds = perf_counter() - selection_started_at
    winner = ranked.iloc[0]
    winner_index = tested.index[tested["Configuration"] == winner["Configuration"]][0]
    winner_parameters = candidate_list[int(winner_index)]
    runner_up = ranked.iloc[1] if len(ranked) > 1 else None
    gap = (
        None
        if runner_up is None
        else float(winner["EligibleModelPct"] - runner_up["EligibleModelPct"])
    )
    selected = {
        "schema_version": 1,
        "calibration_logic": "threshold_parameter_calibration_v1",
        "configuration": str(winner["Configuration"]),
        "parameters": winner_parameters.as_dict(),
        "configuration_sha256": winner_parameters.digest,
        "selection_reason": winner["SelectionReason"],
        "selection_order": list(SELECTION_ORDER),
        "development_metrics": {
            key: (None if pd.isna(winner[key]) else winner[key].item() if isinstance(winner[key], np.generic) else winner[key])
            for key in (
                "ModelsEvaluated", "ModelsEligible", "EligibleModelPct", "TotalSignals",
                "PrecisionMedian", "F1Median", "WindowCoverageMedian",
                "PrecisionStdMedian", "DirectionalReturnMeanMedian",
                "DirectionalReturnStdMedian", "OppositeMoveFrequencyMedian",
            )
        },
        "runner_up": None if runner_up is None else str(runner_up["Configuration"]),
        "runner_up_primary_criterion_gap": gap,
        "candidates_tested": len(ranked),
        "eligible_candidates": int(ranked["EligibleConfiguration"].sum()),
        "parent_run": source_parent_run,
        "source_walk_forward_run": source_walk_forward_run,
        "source_xgboost_calibration_run": source_xgboost_calibration_run,
        "xgboost_parameter_source": xgboost_parameter_source,
        "frozen_xgboost_parameters_sha256": frozen_xgboost_parameters_sha256,
        "selected_at": datetime.now(timezone.utc).isoformat(),
    }
    sampled_output = sampled.copy()
    run_configuration = {
        "job_type": "threshold_parameter_calibration",
        "protocol": "development_only_global_threshold_calibrator_policy_selection",
        "holdout_used_for_selection": False,
        "holdout_evaluated": False,
        "final_holdout_start": holdout_start.isoformat(),
        "candidate_generation": "deterministic_controlled_one_factor_at_a_time_v1",
        "selection_order": list(SELECTION_ORDER),
        "parameters_serving_as_baseline": ThresholdCalibrationParameters.from_config(config).as_dict(),
        "selected_configuration": selected,
        "source_parent_run": source_parent_run,
        "source_walk_forward_run": source_walk_forward_run,
        "source_xgboost_calibration_run": source_xgboost_calibration_run,
        "xgboost_parameter_source": xgboost_parameter_source,
        "xgboost_parameters_by_direction": {
            direction: xgboost_parameters_by_direction[direction].as_dict()
            for direction in ("Up", "Down")
        },
        "frozen_xgboost_parameters_sha256": frozen_xgboost_parameters_sha256,
        "combinations_per_target": (
            combinations_per_target if sampling_policy == PER_TARGET_V1 else None
        ),
        "combination_sampling": sampling_policy,
        "sampling_manifest": sample.manifest,
        "sampled_combinations": len(sampled),
        "candidate_count": len(candidate_list),
        "eligible_candidate_count": int(ranked["EligibleConfiguration"].sum()),
        "development_probability_rows": len(predictions),
        "performance": {
            "development_probability_seconds": probability_elapsed_seconds,
            "threshold_grid_seconds": grid_elapsed_seconds,
            "selection_seconds": selection_elapsed_seconds,
            "total_seconds": perf_counter() - total_started_at,
            "threshold_grid_seconds_by_key": grid_timings,
            "selection_seconds_by_configuration": selection_timings,
            "threshold_grids_calculated": len(invariants_by_grid) - grid_reused,
            "threshold_grids_reused": grid_reused,
            "candidate_checkpoints_reused": reused_candidates,
        },
    }
    report_progress(progress_callback, "metrics", substage="completed", details={"phase_event": "completed"})
    return ThresholdParameterCalibrationResult(
        tested_configurations=tested,
        development_by_configuration=ranked,
        development_by_window=(
            pd.concat(window_frames, ignore_index=True) if window_frames else pd.DataFrame()
        ),
        selected_configuration=selected,
        development_predictions=predictions,
        sampled_combinations=sampled_output,
        run_configuration=run_configuration,
    )


def write_threshold_parameter_calibration_results(
    result: ThresholdParameterCalibrationResult, directory: Path
) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    result.tested_configurations.to_csv(
        directory / "tested_threshold_parameter_configurations.csv", index=False
    )
    result.development_by_configuration.to_csv(
        directory / "development_metrics_by_configuration.csv", index=False
    )
    result.development_by_window.to_csv(
        directory / "development_metrics_by_window.csv", index=False
    )
    result.development_predictions.to_csv(
        directory / "development_predictions.csv", index=False
    )
    result.sampled_combinations.to_csv(
        directory / "sampled_combinations.csv", index=False
    )
    (directory / "selected_threshold_calibration_configuration.json").write_text(
        json.dumps(result.selected_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    (directory / "run_configuration.json").write_text(
        json.dumps(result.run_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    (directory / "sampling_manifest.json").write_text(
        json.dumps(
            result.run_configuration["sampling_manifest"],
            indent=2,
            ensure_ascii=False,
        )
        + "\n",
        encoding="utf-8",
    )
