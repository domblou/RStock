"""Controlled XGBoost calibration with a strictly isolated final holdout."""

from __future__ import annotations

import hashlib
import json
import logging
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from .combinations import symbol_set_id, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, classification_metrics
from .features import (
    intraday_down_target_column,
    intraday_target_column,
    predictor_columns,
)
from .modeling import (
    XGBoostParameters,
    fit_booster,
    historical_xgboost_parameters,
    predict_probabilities,
)
from .parallel import process_cancellation_requested, run_combination_tasks
from .qualification import qualification_parameters
from .progress import (
    CancellationCheck,
    ProgressCallback,
    check_cancellation,
    report_progress,
)
from .walk_forward import expanding_windows


LOGGER = logging.getLogger(__name__)
SELECTION_FORMULA = (
    "0.30*ROCAUCMedian + 0.20*PRAUCMedian + 0.15*ROCAUCWorst + "
    "0.10*PRAUCWorst + 0.10*F1Median + "
    "0.05*ReasonablePositiveWindowPct + 0.05*UsefulProbabilityWindowPct - "
    "0.10*ROCAUCStd - 0.05*PRAUCStd - 0.20*CatastrophicWindowPct"
)


@dataclass(slots=True)
class CalibrationResult:
    tested_parameters: pd.DataFrame
    development_by_configuration: pd.DataFrame
    development_by_window: pd.DataFrame
    selected_configurations: dict[str, dict[str, object]]
    baseline_comparison: pd.DataFrame
    holdout_metrics: pd.DataFrame
    holdout_predictions: pd.DataFrame
    sampled_combinations: pd.DataFrame
    run_configuration: dict[str, object]


@dataclass(slots=True)
class _CalibrationCombinationResult:
    buckets: dict[tuple[str, str, int], dict[str, object]]


_CALIBRATION_TASK_CONTEXT: tuple[
    pd.DataFrame,
    RStockConfig,
    tuple[XGBoostParameters, ...],
    int,
    int,
    int,
] | None = None


def _set_calibration_task_context(
    context: tuple[
        pd.DataFrame,
        RStockConfig,
        tuple[XGBoostParameters, ...],
        int,
        int,
        int,
    ]
) -> None:
    global _CALIBRATION_TASK_CONTEXT
    _CALIBRATION_TASK_CONTEXT = context


def default_parameter_candidates(config: RStockConfig) -> list[XGBoostParameters]:
    """A small, deterministic search spanning depth, shrinkage and regularisation."""

    baseline = historical_xgboost_parameters(config)
    candidates = [
        baseline,
        XGBoostParameters(1, 0.05, 120, 1, 0.80, 0.80, 0.0, 0.0, 1.0),
        XGBoostParameters(2, 0.05, 120, 1, 0.80, 0.80, 0.0, 0.0, 1.0),
        XGBoostParameters(2, 0.10, 80, 3, 0.80, 0.80, 0.0, 0.0, 2.0),
        XGBoostParameters(2, 0.20, 40, 1, 1.00, 0.80, 0.1, 0.1, 1.0),
        XGBoostParameters(3, 0.03, 160, 1, 0.80, 0.80, 0.0, 0.0, 1.0),
        XGBoostParameters(3, 0.05, 120, 3, 0.80, 0.80, 0.1, 0.1, 2.0),
        XGBoostParameters(3, 0.10, 80, 1, 1.00, 1.00, 0.0, 0.0, 1.0),
        XGBoostParameters(3, 0.20, 40, 5, 0.80, 1.00, 0.2, 0.1, 2.0),
        XGBoostParameters(4, 0.05, 100, 5, 0.70, 0.80, 0.1, 0.5, 3.0),
        XGBoostParameters(4, 0.10, 60, 3, 0.80, 0.70, 0.2, 0.1, 2.0),
        XGBoostParameters(5, 0.05, 80, 5, 0.70, 0.70, 0.2, 0.5, 3.0),
    ]
    unique: list[XGBoostParameters] = []
    for candidate in candidates:
        if candidate not in unique:
            unique.append(candidate)
    return unique


def parameter_table(
    candidates: Sequence[XGBoostParameters], config: RStockConfig
) -> pd.DataFrame:
    baseline = historical_xgboost_parameters(config)
    rows = []
    for number, candidate in enumerate(candidates):
        rows.append(
            {
                "Configuration": "baseline" if candidate == baseline else f"candidate_{number:02d}",
                "HistoricalBaseline": candidate == baseline,
                **candidate.as_dict(),
            }
        )
    frame = pd.DataFrame(rows)
    if frame["Configuration"].duplicated().any():
        raise ValueError("Candidate configuration identifiers must be unique")
    return frame


def deterministic_combination_sample(
    generated_sets: pd.DataFrame,
    *,
    per_target: int,
    seed: int,
) -> pd.DataFrame:
    """Select the same balanced set sample regardless of input row ordering."""

    if per_target < 1:
        raise ValueError("per_target must be positive")
    ranked: list[tuple[str, str, str, pd.Series]] = []
    for _, row in generated_sets.iterrows():
        observation, _ = symbols_from_set(row)
        set_name = symbol_set_id(row)
        digest = hashlib.sha256(f"{seed}:{set_name}".encode()).hexdigest()
        ranked.append((observation, digest, set_name, row))
    selected: list[pd.Series] = []
    by_target: dict[str, list[tuple[str, str, pd.Series]]] = {}
    for observation, digest, set_name, row in ranked:
        by_target.setdefault(observation, []).append((digest, set_name, row))
    for observation in sorted(by_target):
        choices = sorted(by_target[observation], key=lambda item: (item[0], item[1]))
        selected.extend(item[2] for item in choices[:per_target])
    if not selected:
        raise ValueError("No combinations are available for calibration")
    return pd.DataFrame(selected).reset_index(drop=True)


def split_development_holdout(
    prepared: pd.DataFrame, final_holdout_size: int
) -> tuple[pd.DataFrame, pd.DataFrame, pd.Timestamp]:
    if not isinstance(prepared.index, pd.DatetimeIndex):
        raise TypeError("Prepared data must use a DatetimeIndex")
    if prepared.index.hasnans or prepared.index.has_duplicates:
        raise ValueError("Prepared data dates must be complete and unique")
    ordered = prepared.sort_index()
    if final_holdout_size < 1 or final_holdout_size >= len(ordered):
        raise ValueError("final_holdout_size must leave non-empty development history")
    holdout_start = ordered.index[-final_holdout_size]
    development = ordered.loc[ordered.index < holdout_start].copy()
    holdout = ordered.loc[ordered.index >= holdout_start].copy()
    if development.index.max() >= holdout.index.min():
        raise AssertionError("Final holdout leaked into development data")
    return development, holdout, holdout_start


def _configuration_lookup(
    candidates: Sequence[XGBoostParameters], config: RStockConfig
) -> dict[str, XGBoostParameters]:
    table = parameter_table(candidates, config)
    return {
        str(row["Configuration"]): candidate
        for (_, row), candidate in zip(table.iterrows(), candidates, strict=True)
    }


def _append_prediction_bucket(
    buckets: dict[tuple[str, str, int], dict[str, object]],
    key: tuple[str, str, int],
    *,
    actual: np.ndarray,
    predicted: np.ndarray,
    probabilities: np.ndarray,
    train: pd.DataFrame,
    test: pd.DataFrame,
) -> None:
    bucket = buckets.setdefault(
        key,
        {
            "actual": [],
            "predicted": [],
            "probabilities": [],
            "train_start": train.index.min(),
            "train_end": train.index.max(),
            "test_start": test.index.min(),
            "test_end": test.index.max(),
            "sets": 0,
        },
    )
    bucket["actual"].append(actual)
    bucket["predicted"].append(predicted)
    bucket["probabilities"].append(probabilities)
    bucket["sets"] = int(bucket["sets"]) + 1
    bucket["train_start"] = min(bucket["train_start"], train.index.min())
    bucket["train_end"] = max(bucket["train_end"], train.index.max())
    bucket["test_start"] = min(bucket["test_start"], test.index.min())
    bucket["test_end"] = max(bucket["test_end"], test.index.max())


def _calibration_combination(
    row_values: dict[str, object],
    context: tuple[
        pd.DataFrame,
        RStockConfig,
        tuple[XGBoostParameters, ...],
        int,
        int,
        int,
    ],
    cancellation_check: CancellationCheck | None,
) -> _CalibrationCombinationResult:
    """Evaluate all windows/configurations for one combination sequentially."""

    development, config, candidates, min_train_size, test_size, step_size = context
    lookup = _configuration_lookup(candidates, config)
    row = pd.Series(row_values)
    observation, feature_symbols = symbols_from_set(row)
    names = predictor_columns(
        development, feature_symbols, config.lag_depth, config.date_feature_regex
    )
    up_outcome = intraday_target_column(observation)
    down_outcome = intraday_down_target_column(observation)
    required = [*names, up_outcome, down_outcome]
    if not names or any(name not in development for name in required):
        raise ValueError(f"Incomplete columns for set targeting {observation}")
    model_data = development[required].dropna()
    buckets: dict[tuple[str, str, int], dict[str, object]] = {}
    windows = expanding_windows(len(model_data), min_train_size, test_size, step_size)
    for window in windows:
        check_cancellation(cancellation_check)
        train = model_data.iloc[window.train_slice]
        test = model_data.iloc[window.test_slice]
        if train.index.max() >= test.index.min():
            raise AssertionError("Calibration window leaked future test data")
        for configuration, parameters in lookup.items():
            for direction, outcome in (("Up", up_outcome), ("Down", down_outcome)):
                booster = fit_booster(train, names, outcome, config, parameters=parameters)
                probabilities = predict_probabilities(booster, test, names)
                actual = test[outcome].astype(int).to_numpy()
                predicted = binary_predictions(probabilities, config.prediction_threshold)
                _append_prediction_bucket(
                    buckets,
                    (configuration, direction, window.number),
                    actual=actual,
                    predicted=predicted,
                    probabilities=probabilities,
                    train=train,
                    test=test,
                )
    return _CalibrationCombinationResult(buckets)


def _calibration_process_task(
    row_values: dict[str, object],
) -> _CalibrationCombinationResult:
    if _CALIBRATION_TASK_CONTEXT is None:  # pragma: no cover - process invariant
        raise RuntimeError("Calibration worker context is unavailable")
    return _calibration_combination(
        row_values,
        _CALIBRATION_TASK_CONTEXT,
        process_cancellation_requested,
    )


def _merge_prediction_buckets(
    target: dict[tuple[str, str, int], dict[str, object]],
    source: dict[tuple[str, str, int], dict[str, object]],
) -> None:
    for key, bucket in source.items():
        existing = target.setdefault(
            key,
            {
                "actual": [],
                "predicted": [],
                "probabilities": [],
                "train_start": bucket["train_start"],
                "train_end": bucket["train_end"],
                "test_start": bucket["test_start"],
                "test_end": bucket["test_end"],
                "sets": 0,
            },
        )
        existing["actual"].extend(bucket["actual"])
        existing["predicted"].extend(bucket["predicted"])
        existing["probabilities"].extend(bucket["probabilities"])
        existing["sets"] = int(existing["sets"]) + int(bucket["sets"])
        existing["train_start"] = min(existing["train_start"], bucket["train_start"])
        existing["train_end"] = max(existing["train_end"], bucket["train_end"])
        existing["test_start"] = min(existing["test_start"], bucket["test_start"])
        existing["test_end"] = max(existing["test_end"], bucket["test_end"])


def _window_metric_rows(
    buckets: Mapping[tuple[str, str, int], dict[str, object]]
) -> pd.DataFrame:
    rows: list[dict[str, object]] = []
    for (configuration, direction, window), bucket in sorted(buckets.items()):
        actual = np.concatenate(bucket["actual"])
        predicted = np.concatenate(bucket["predicted"])
        probabilities = np.concatenate(bucket["probabilities"])
        metrics = classification_metrics(actual, predicted, probabilities)
        prevalence = metrics.prevalence
        positive_rate = float(predicted.mean()) if len(predicted) else 0.0
        probability_range = float(probabilities.max() - probabilities.min())
        useful_probability = bool(
            float(probabilities.std(ddof=0)) > 1e-6 and probability_range >= 0.01
        )
        reasonable_limit = max(0.50, min(1.0, 4.0 * prevalence))
        reasonable_positive = bool(predicted.sum() > 0 and positive_rate <= reasonable_limit)
        catastrophic = bool(
            metrics.roc_auc is None
            or metrics.pr_auc is None
            or metrics.roc_auc < 0.40
            or metrics.pr_auc < 0.50 * prevalence
        )
        rows.append(
            {
                "Configuration": configuration,
                "Direction": direction,
                "Window": window,
                "TrainStart": bucket["train_start"],
                "TrainEnd": bucket["train_end"],
                "TestStart": bucket["test_start"],
                "TestEnd": bucket["test_end"],
                "Sets": bucket["sets"],
                "Observations": len(actual),
                "PositiveOutcomes": int(actual.sum()),
                "PositivePredictions": int(predicted.sum()),
                "PositivePredictionRate": positive_rate,
                "ProbabilityMean": float(probabilities.mean()),
                "ProbabilityStd": float(probabilities.std(ddof=0)),
                "ProbabilityMin": float(probabilities.min()),
                "ProbabilityMax": float(probabilities.max()),
                "ProbabilityRange": probability_range,
                "UsefulProbability": useful_probability,
                "ReasonablePositiveClasses": reasonable_positive,
                "Catastrophic": catastrophic,
                **metrics.as_columns(),
            }
        )
    return pd.DataFrame(rows)


def _configuration_metric_rows(
    by_window: pd.DataFrame, parameters: pd.DataFrame
) -> pd.DataFrame:
    parameter_lookup = parameters.set_index("Configuration").to_dict("index")
    rows: list[dict[str, object]] = []
    for (configuration, direction), group in by_window.groupby(
        ["Configuration", "Direction"], sort=True
    ):
        roc = pd.to_numeric(group["ROCAUC"], errors="coerce")
        pr = pd.to_numeric(group["PRAUC"], errors="coerce")
        row: dict[str, object] = {
            "Configuration": configuration,
            "Direction": direction,
            **parameter_lookup[configuration],
            "Windows": int(group["Window"].nunique()),
            "Observations": int(group["Observations"].sum()),
            "ROCAUCMedian": float(roc.median()),
            "ROCAUCMean": float(roc.mean()),
            "ROCAUCStd": float(roc.std(ddof=0)),
            "ROCAUCWorst": float(roc.min()),
            "PRAUCMedian": float(pr.median()),
            "PRAUCMean": float(pr.mean()),
            "PRAUCStd": float(pr.std(ddof=0)),
            "PRAUCWorst": float(pr.min()),
            "PrecisionMedian": float(group["Precision"].median()),
            "RecallMedian": float(group["Recall"].median()),
            "F1Median": float(group["F1"].median()),
            "PrevalenceMedian": float(group["Prevalence"].median()),
            "PositivePredictionRateMedian": float(group["PositivePredictionRate"].median()),
            "ReasonablePositiveWindowPct": float(group["ReasonablePositiveClasses"].mean()),
            "UsefulProbabilityWindowPct": float(group["UsefulProbability"].mean()),
            "CatastrophicWindowPct": float(group["Catastrophic"].mean()),
        }
        row["SelectionScore"] = (
            0.30 * row["ROCAUCMedian"]
            + 0.20 * row["PRAUCMedian"]
            + 0.15 * row["ROCAUCWorst"]
            + 0.10 * row["PRAUCWorst"]
            + 0.10 * row["F1Median"]
            + 0.05 * row["ReasonablePositiveWindowPct"]
            + 0.05 * row["UsefulProbabilityWindowPct"]
            - 0.10 * row["ROCAUCStd"]
            - 0.05 * row["PRAUCStd"]
            - 0.20 * row["CatastrophicWindowPct"]
        )
        rows.append(row)
    result = pd.DataFrame(rows)
    result["Selected"] = False
    result["Rank"] = pd.Series(pd.NA, index=result.index, dtype="Int64")
    for direction, group in result.groupby("Direction", sort=True):
        ranked = group.sort_values(
            [
                "SelectionScore",
                "CatastrophicWindowPct",
                "ROCAUCWorst",
                "ROCAUCStd",
                "Configuration",
            ],
            ascending=[False, True, False, True, True],
            kind="stable",
        )
        result.loc[ranked.index, "Rank"] = np.arange(1, len(ranked) + 1)
        result.loc[ranked.index[0], "Selected"] = True
    return result.sort_values(["Direction", "Rank"], kind="stable").reset_index(drop=True)


def calibrate_development(
    development: pd.DataFrame,
    sampled_sets: pd.DataFrame,
    config: RStockConfig,
    candidates: Sequence[XGBoostParameters],
    *,
    min_train_size: int,
    test_size: int,
    step_size: int,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, XGBoostParameters]]:
    """Select parameters using development data only."""

    if not candidates:
        raise ValueError("At least one parameter candidate is required")
    lookup = _configuration_lookup(candidates, config)
    buckets: dict[tuple[str, str, int], dict[str, object]] = {}
    task_context = (
        development,
        config,
        tuple(candidates),
        min_train_size,
        test_size,
        step_size,
    )
    task_rows = [row.to_dict() for _, row in sampled_sets.iterrows()]
    combination_results = run_combination_tasks(
        task_rows,
        combination_workers=config.combination_workers,
        worker_context=task_context,
        context_initializer=_set_calibration_task_context,
        process_task=_calibration_process_task,
        serial_task=_calibration_combination,
        item_label=lambda values: symbol_set_id(pd.Series(values)),
        stage="xgboost_calibration",
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details={
            "configurations": len(candidates),
            "directions": 2,
            "combination_workers": config.combination_workers,
        },
    )
    for result in combination_results:
        _merge_prediction_buckets(buckets, result.buckets)

    by_window = _window_metric_rows(buckets)
    by_configuration = _configuration_metric_rows(
        by_window, parameter_table(candidates, config)
    )
    selected: dict[str, XGBoostParameters] = {}
    for direction in ("Up", "Down"):
        selected_id = str(
            by_configuration.loc[
                (by_configuration["Direction"] == direction)
                & by_configuration["Selected"],
                "Configuration",
            ].iloc[0]
        )
        selected[direction] = lookup[selected_id]
    return by_configuration, by_window, selected


def _selected_payload(
    selected: Mapping[str, XGBoostParameters],
    metrics: pd.DataFrame,
) -> dict[str, dict[str, object]]:
    payload: dict[str, dict[str, object]] = {}
    for direction, parameters in selected.items():
        row = metrics[(metrics["Direction"] == direction) & metrics["Selected"]].iloc[0]
        payload[direction] = {
            "configuration": str(row["Configuration"]),
            "parameters": parameters.as_dict(),
            "selection_score": float(row["SelectionScore"]),
        }
    return payload


def evaluate_locked_holdout(
    development: pd.DataFrame,
    holdout: pd.DataFrame,
    sampled_sets: pd.DataFrame,
    config: RStockConfig,
    selected: Mapping[str, XGBoostParameters],
    selected_payload: Mapping[str, Mapping[str, object]],
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Evaluate each frozen directional selection exactly once on final holdout."""

    buckets: dict[str, dict[str, list[np.ndarray]]] = {
        direction: {"actual": [], "predicted": [], "probabilities": []}
        for direction in ("Up", "Down")
    }
    prediction_rows: list[dict[str, object]] = []
    for completed, (_, row) in enumerate(sampled_sets.iterrows(), start=1):
        check_cancellation(cancellation_check)
        set_name = symbol_set_id(row)
        observation, feature_symbols = symbols_from_set(row)
        names = predictor_columns(
            development, feature_symbols, config.lag_depth, config.date_feature_regex
        )
        outcomes = {
            "Up": intraday_target_column(observation),
            "Down": intraday_down_target_column(observation),
        }
        required = [*names, *outcomes.values()]
        train = development[required].dropna()
        test = holdout[required].dropna()
        if train.empty or test.empty:
            continue
        if train.index.max() >= test.index.min():
            raise AssertionError("Final holdout leaked into calibration training data")
        set_predictions: dict[str, tuple[np.ndarray, np.ndarray]] = {}
        for direction, outcome in outcomes.items():
            booster = fit_booster(
                train, names, outcome, config, parameters=selected[direction]
            )
            probabilities = predict_probabilities(booster, test, names)
            predicted = binary_predictions(probabilities, config.prediction_threshold)
            actual = test[outcome].astype(int).to_numpy()
            buckets[direction]["actual"].append(actual)
            buckets[direction]["predicted"].append(predicted)
            buckets[direction]["probabilities"].append(probabilities)
            set_predictions[direction] = (predicted, probabilities)
        for position, date in enumerate(test.index):
            prediction_rows.append(
                {
                    "Set": set_name,
                    "Observation": observation,
                    "Predictors": json.dumps(feature_symbols, separators=(",", ":")),
                    "Date": date,
                    "UpTarget": int(test.iloc[position][outcomes["Up"]]),
                    "UpPrediction": int(set_predictions["Up"][0][position]),
                    "UpProbability": float(set_predictions["Up"][1][position]),
                    "DownTarget": int(test.iloc[position][outcomes["Down"]]),
                    "DownPrediction": int(set_predictions["Down"][0][position]),
                    "DownProbability": float(set_predictions["Down"][1][position]),
                }
            )
        report_progress(
            progress_callback,
            "final_holdout",
            substage=set_name,
            completed_units=completed,
            total_units=len(sampled_sets),
        )

    metric_rows: list[dict[str, object]] = []
    for direction in ("Up", "Down"):
        actual = np.concatenate(buckets[direction]["actual"])
        predicted = np.concatenate(buckets[direction]["predicted"])
        probabilities = np.concatenate(buckets[direction]["probabilities"])
        metrics = classification_metrics(actual, predicted, probabilities)
        metric_rows.append(
            {
                "Direction": direction,
                "Configuration": selected_payload[direction]["configuration"],
                **selected[direction].as_dict(),
                "Sets": len(sampled_sets),
                "Observations": len(actual),
                "PositivePredictionRate": float(predicted.mean()),
                "ProbabilityStd": float(probabilities.std(ddof=0)),
                **metrics.as_columns(),
            }
        )
    return pd.DataFrame(metric_rows), pd.DataFrame(prediction_rows)


def _baseline_comparison(by_configuration: pd.DataFrame) -> pd.DataFrame:
    rows: list[pd.Series] = []
    for direction in ("Up", "Down"):
        group = by_configuration[by_configuration["Direction"] == direction]
        baseline = group[group["HistoricalBaseline"]].iloc[0].copy()
        baseline["ConfigurationRole"] = "HistoricalBaseline"
        selected = group[group["Selected"]].iloc[0].copy()
        selected["ConfigurationRole"] = "Calibrated"
        rows.extend([baseline, selected])
    return pd.DataFrame(rows).reset_index(drop=True)


def run_controlled_calibration(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    candidates: Sequence[XGBoostParameters] | None = None,
    combinations_per_target: int = 3,
    min_train_size: int | None = None,
    test_size: int | None = None,
    step_size: int | None = None,
    final_holdout_size: int | None = None,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> CalibrationResult:
    """Calibrate on development, freeze selections, then open the holdout once."""

    candidate_list = list(candidates or default_parameter_candidates(config))
    min_train = min_train_size or config.walk_forward_min_train_size
    test_window = test_size or config.walk_forward_test_size
    step = step_size or config.walk_forward_step_size
    holdout_size = final_holdout_size or config.final_holdout_size
    sampled = deterministic_combination_sample(
        generated_sets, per_target=combinations_per_target, seed=config.xgb_seed
    )
    development, holdout, holdout_start = split_development_holdout(
        prepared, holdout_size
    )

    report_progress(progress_callback, "walk_forward", substage="started", details={"phase_event": "started", "combinations": len(sampled)})
    by_configuration, by_window, selected = calibrate_development(
        development,
        sampled,
        config,
        candidate_list,
        min_train_size=min_train,
        test_size=test_window,
        step_size=step,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    report_progress(progress_callback, "walk_forward", substage="completed", details={"phase_event": "completed", "windows": int(by_window["Window"].nunique())})
    selected_payload = _selected_payload(selected, by_configuration)
    selection_json = json.dumps(selected_payload, sort_keys=True, separators=(",", ":"))
    selection_digest = hashlib.sha256(selection_json.encode()).hexdigest()

    # This is the first operation permitted to inspect holdout values. Selection
    # is immutable and fingerprinted before the call.
    report_progress(progress_callback, "final_holdout", substage="started", details={"phase_event": "started"})
    holdout_metrics, holdout_predictions = evaluate_locked_holdout(
        development,
        holdout,
        sampled,
        config,
        selected,
        selected_payload,
        progress_callback,
        cancellation_check,
    )
    report_progress(progress_callback, "final_holdout", substage="completed", details={"phase_event": "completed"})
    report_progress(progress_callback, "metrics", substage="started", details={"phase_event": "started"})
    tested = parameter_table(candidate_list, config)
    sample_output = sampled.copy()
    sample_output.insert(
        0, "Set", [symbol_set_id(row) for _, row in sample_output.iterrows()]
    )
    run_configuration: dict[str, object] = {
        "calibration_protocol": "development_select_freeze_then_single_holdout_evaluation",
        "holdout_used_for_selection": False,
        "holdout_evaluation_count": 1,
        "selection_digest_before_holdout": selection_digest,
        "selection_formula": SELECTION_FORMULA,
        "catastrophic_window_definition": (
            "missing ROC/PR-AUC, ROC-AUC < 0.40, or PR-AUC < 0.50 * prevalence"
        ),
        "useful_probability_definition": (
            "probability standard deviation > 1e-6 and range >= 0.01"
        ),
        "reasonable_positive_definition": (
            "at least one positive prediction and rate <= max(0.50, 4 * prevalence)"
        ),
        "candidate_configurations": len(tested),
        "direction_configuration_evaluations": 2 * len(tested),
        "combination_sampling": (
            "SHA-256 rank of seed:set identifier; equal quota per target"
        ),
        "combination_sampling_seed": config.xgb_seed,
        "combination_workers": config.combination_workers,
        "combinations_per_target": combinations_per_target,
        "sampled_combinations": len(sampled),
        "permutation_depth": max(
            len(symbols_from_set(row)[1]) for _, row in sampled.iterrows()
        ),
        "symbols": sorted({symbols_from_set(row)[0] for _, row in sampled.iterrows()}),
        "lag_depth": config.lag_depth,
        "target": "intraday_return >= intraday_target_threshold",
        "down_target": "intraday_return <= -intraday_down_threshold",
        "intraday_target_threshold": config.intraday_target_threshold,
        "intraday_down_threshold": config.intraday_down_threshold,
        "prediction_threshold": config.prediction_threshold,
        "walk_forward_min_train_size": min_train,
        "walk_forward_test_size": test_window,
        "walk_forward_step_size": step,
        "development_start": development.index.min().isoformat(),
        "development_end": development.index.max().isoformat(),
        "final_holdout_start": holdout_start.isoformat(),
        "final_holdout_end": holdout.index.max().isoformat(),
        "final_holdout_size": holdout_size,
        "qualification_criteria_unchanged": qualification_parameters(config),
        "selected_configurations": selected_payload,
    }
    report_progress(progress_callback, "metrics", substage="completed", details={"phase_event": "completed"})
    return CalibrationResult(
        tested_parameters=tested,
        development_by_configuration=by_configuration,
        development_by_window=by_window,
        selected_configurations=selected_payload,
        baseline_comparison=_baseline_comparison(by_configuration),
        holdout_metrics=holdout_metrics,
        holdout_predictions=holdout_predictions,
        sampled_combinations=sample_output,
        run_configuration=run_configuration,
    )


def write_calibration_results(result: CalibrationResult, directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    result.tested_parameters.to_csv(directory / "tested_parameters.csv", index=False)
    result.development_by_configuration.to_csv(
        directory / "development_metrics_by_configuration.csv", index=False
    )
    result.development_by_window.to_csv(
        directory / "development_metrics_by_window.csv", index=False
    )
    result.baseline_comparison.to_csv(
        directory / "baseline_vs_calibrated_development.csv", index=False
    )
    result.holdout_metrics.to_csv(directory / "holdout_metrics.csv", index=False)
    result.holdout_predictions.to_csv(
        directory / "holdout_predictions.csv", index=False
    )
    result.sampled_combinations.to_csv(
        directory / "sampled_combinations.csv", index=False
    )
    (directory / "selected_configurations.json").write_text(
        json.dumps(result.selected_configurations, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    (directory / "run_configuration.json").write_text(
        json.dumps(result.run_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
