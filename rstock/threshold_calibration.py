"""Development-only calibration of directional probability decision thresholds."""

from __future__ import annotations

import hashlib
import json
import math
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from .calibration import deterministic_combination_sample, split_development_holdout
from .combinations import symbol_set_id, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, classification_metrics
from .features import (
    intraday_down_target_column,
    intraday_return_column,
    intraday_target_column,
    mae_column,
    mfe_column,
    predictor_columns,
)
from .modeling import XGBoostParameters, fit_booster, predict_probabilities
from .parallel import process_cancellation_requested, run_combination_tasks
from .progress import (
    CancellationCheck,
    ProgressCallback,
    check_cancellation,
    report_progress,
)
from .walk_forward import expanding_windows


EXPERIMENTAL_XGBOOST_PARAMETERS = XGBoostParameters(
    max_depth=1,
    eta=0.05,
    num_boost_round=120,
    min_child_weight=1,
    subsample=0.8,
    colsample_bytree=0.8,
    gamma=0.0,
    reg_alpha=0.0,
    reg_lambda=1.0,
)

THRESHOLD_SELECTION_ORDER = (
    "Eligible desc; WindowsMeetingMinSignals desc; CatastrophicWindows asc; "
    "PrecisionMedian desc; DirectionalReturnMeanMedian desc; "
    "OppositeMoveFrequencyMedian asc; PrecisionStd asc; F1Median desc; "
    "abs(Threshold - 0.5) asc; Threshold desc"
)

REQUIRED_PREDICTION_COLUMNS = {
    "Direction",
    "Window",
    "Date",
    "Probability",
    "Target",
    "IntradayReturn",
    "MFE",
    "MAE",
}


@dataclass(slots=True)
class ThresholdCalibrationResult:
    probability_distribution: pd.DataFrame
    threshold_grid: pd.DataFrame
    metrics_by_window: pd.DataFrame
    metrics_by_threshold: pd.DataFrame
    selected_thresholds: dict[str, dict[str, object]]
    baseline_comparison: pd.DataFrame


@dataclass(slots=True)
class ControlledThresholdCalibrationResult:
    development_predictions: pd.DataFrame
    calibration: ThresholdCalibrationResult
    sampled_combinations: pd.DataFrame
    holdout_predictions: pd.DataFrame
    holdout_metrics: pd.DataFrame
    run_configuration: dict[str, object]


_THRESHOLD_DEVELOPMENT_CONTEXT: tuple[
    pd.DataFrame, RStockConfig, int, int, int, XGBoostParameters
] | None = None


def _set_threshold_development_context(
    context: tuple[pd.DataFrame, RStockConfig, int, int, int, XGBoostParameters]
) -> None:
    global _THRESHOLD_DEVELOPMENT_CONTEXT
    _THRESHOLD_DEVELOPMENT_CONTEXT = context


def _threshold_development_combination(
    row_values: dict[str, object],
    context: tuple[pd.DataFrame, RStockConfig, int, int, int, XGBoostParameters],
    cancellation_check: CancellationCheck | None,
) -> list[dict[str, object]]:
    """Evaluate one combination while retaining sequential windows and directions."""

    development, config, min_train_size, test_size, step_size, parameters = context
    row = pd.Series(row_values)
    set_name = symbol_set_id(row)
    observation, feature_symbols = symbols_from_set(row)
    names = predictor_columns(
        development, feature_symbols, config.lag_depth, config.date_feature_regex
    )
    outcomes = {
        "Up": intraday_target_column(observation),
        "Down": intraday_down_target_column(observation),
    }
    diagnostics = [
        intraday_return_column(observation), mfe_column(observation), mae_column(observation)
    ]
    model_data = development[[*names, *outcomes.values(), *diagnostics]].dropna()
    records: list[dict[str, object]] = []
    for window in expanding_windows(len(model_data), min_train_size, test_size, step_size):
        check_cancellation(cancellation_check)
        train = model_data.iloc[window.train_slice]
        test = model_data.iloc[window.test_slice]
        if train.index.max() >= test.index.min():
            raise AssertionError("Threshold calibration leaked future test data")
        for direction, outcome in outcomes.items():
            booster = fit_booster(train, names, outcome, config, parameters=parameters)
            probabilities = predict_probabilities(booster, test, names)
            records.extend(
                {
                    "Set": set_name, "Observation": observation, "Direction": direction,
                    "Window": window.number, "TrainEnd": train.index.max(), "Date": date,
                    "Probability": float(probability), "Target": int(test.at[date, outcome]),
                    "IntradayReturn": float(test.at[date, intraday_return_column(observation)]),
                    "MFE": float(test.at[date, mfe_column(observation)]),
                    "MAE": float(test.at[date, mae_column(observation)]),
                }
                for date, probability in zip(test.index, probabilities, strict=True)
            )
    return records


def _threshold_development_process_task(
    row_values: dict[str, object],
) -> list[dict[str, object]]:
    if _THRESHOLD_DEVELOPMENT_CONTEXT is None:
        raise RuntimeError("Threshold worker context was not initialized")
    return _threshold_development_combination(
        row_values, _THRESHOLD_DEVELOPMENT_CONTEXT, process_cancellation_requested
    )


def validate_threshold_calibration_config(config: RStockConfig) -> None:
    if config.threshold_calibration_min_signals_per_window < 1:
        raise ValueError("threshold_calibration_min_signals_per_window must be positive")
    if not 0 < config.threshold_calibration_min_window_fraction <= 1:
        raise ValueError("threshold_calibration_min_window_fraction must be in (0, 1]")
    if config.threshold_calibration_grid_decimals < 1:
        raise ValueError("threshold_calibration_grid_decimals must be positive")
    quantiles = config.threshold_calibration_quantiles
    if not quantiles or any(not 0 < value < 1 for value in quantiles):
        raise ValueError("threshold_calibration_quantiles must contain values in (0, 1)")


def probability_distribution(predictions: pd.DataFrame) -> pd.DataFrame:
    """Describe development probabilities separately for Up and Down."""

    _validate_predictions(predictions)
    rows: list[dict[str, object]] = []
    for direction, group in predictions.groupby("Direction", sort=True):
        values = pd.to_numeric(group["Probability"], errors="raise")
        rows.append(
            {
                "Direction": direction,
                "Count": len(values),
                "Distinct": int(values.nunique()),
                "Mean": float(values.mean()),
                "Std": float(values.std(ddof=0)),
                "Min": float(values.min()),
                "P01": float(values.quantile(0.01)),
                "P05": float(values.quantile(0.05)),
                "P10": float(values.quantile(0.10)),
                "P25": float(values.quantile(0.25)),
                "P50": float(values.quantile(0.50)),
                "P75": float(values.quantile(0.75)),
                "P90": float(values.quantile(0.90)),
                "P95": float(values.quantile(0.95)),
                "P99": float(values.quantile(0.99)),
                "Max": float(values.max()),
            }
        )
    return pd.DataFrame(rows)


def adaptive_threshold_grid(
    predictions: pd.DataFrame, config: RStockConfig
) -> pd.DataFrame:
    """Build reproducible grids from pooled development-probability quantiles."""

    _validate_predictions(predictions)
    validate_threshold_calibration_config(config)
    rows: list[dict[str, object]] = []
    for direction, group in predictions.groupby("Direction", sort=True):
        probabilities = pd.to_numeric(group["Probability"], errors="raise")
        quantile_values = probabilities.quantile(
            list(config.threshold_calibration_quantiles)
        )
        candidates: dict[float, set[str]] = {0.5: {"reference"}}
        for quantile, value in quantile_values.items():
            threshold = round(
                float(np.clip(value, 0.0, 1.0)),
                config.threshold_calibration_grid_decimals,
            )
            candidates.setdefault(threshold, set()).add(f"q{float(quantile):g}")
        for threshold in sorted(candidates):
            rows.append(
                {
                    "Direction": direction,
                    "Threshold": threshold,
                    "Sources": ",".join(sorted(candidates[threshold])),
                    "ReferenceThreshold": threshold == 0.5,
                }
            )
    return pd.DataFrame(rows)


def _validate_predictions(predictions: pd.DataFrame) -> None:
    missing = REQUIRED_PREDICTION_COLUMNS - set(predictions.columns)
    if missing:
        raise ValueError(f"Missing threshold-calibration columns: {sorted(missing)}")
    if predictions.empty:
        raise ValueError("Threshold calibration requires development predictions")
    directions = set(predictions["Direction"])
    if directions != {"Up", "Down"}:
        raise ValueError("Development predictions must contain Up and Down directions")
    probabilities = pd.to_numeric(predictions["Probability"], errors="raise")
    if not probabilities.between(0.0, 1.0).all():
        raise ValueError("Probabilities must be between zero and one")


def _threshold_window_metrics(
    group: pd.DataFrame,
    direction: str,
    threshold: float,
    config: RStockConfig,
) -> dict[str, object]:
    probabilities = pd.to_numeric(group["Probability"], errors="raise").to_numpy()
    actual = pd.to_numeric(group["Target"], errors="raise").astype(int).to_numpy()
    predicted = binary_predictions(probabilities, threshold)
    metrics = classification_metrics(actual, predicted, probabilities)
    signals = group.loc[predicted.astype(bool)]
    returns = pd.to_numeric(signals["IntradayReturn"], errors="coerce").dropna()
    mfe = pd.to_numeric(signals["MFE"], errors="coerce").dropna()
    mae = pd.to_numeric(signals["MAE"], errors="coerce").dropna()
    if direction == "Up":
        favorable = returns >= config.intraday_target_threshold
        opposite = returns <= -config.intraday_down_threshold
        directional_returns = returns
    else:
        favorable = returns <= -config.intraday_down_threshold
        opposite = returns >= config.intraday_target_threshold
        directional_returns = -returns
    return {
        "SignalCount": int(predicted.sum()),
        "SignalProportion": float(predicted.mean()),
        "IntradayReturnMean": float(returns.mean()) if len(returns) else np.nan,
        "IntradayReturnMedian": float(returns.median()) if len(returns) else np.nan,
        "DirectionalReturnMean": (
            float(directional_returns.mean()) if len(directional_returns) else np.nan
        ),
        "FavorableMoveFrequency": float(favorable.mean()) if len(returns) else np.nan,
        "OppositeMoveFrequency": float(opposite.mean()) if len(returns) else np.nan,
        "MFEMean": float(mfe.mean()) if len(mfe) else np.nan,
        "MAEMean": float(mae.mean()) if len(mae) else np.nan,
        **metrics.as_columns(),
    }


def evaluate_threshold_grid(
    predictions: pd.DataFrame,
    threshold_grid: pd.DataFrame,
    config: RStockConfig,
) -> pd.DataFrame:
    """Calculate requested metrics for every threshold and development window."""

    _validate_predictions(predictions)
    validate_threshold_calibration_config(config)
    rows: list[dict[str, object]] = []
    for grid_row in threshold_grid.itertuples(index=False):
        direction = str(grid_row.Direction)
        threshold = float(grid_row.Threshold)
        directional = predictions[predictions["Direction"] == direction]
        for window, group in directional.groupby("Window", sort=True):
            rows.append(
                {
                    "Direction": direction,
                    "Threshold": threshold,
                    "Window": int(window),
                    "Start": pd.to_datetime(group["Date"]).min(),
                    "End": pd.to_datetime(group["Date"]).max(),
                    "Observations": len(group),
                    **_threshold_window_metrics(group, direction, threshold, config),
                }
            )
    return pd.DataFrame(rows)


def summarize_and_select_thresholds(
    metrics_by_window: pd.DataFrame, config: RStockConfig
) -> tuple[pd.DataFrame, dict[str, dict[str, object]]]:
    """Apply the predeclared stability-first threshold selection ordering."""

    validate_threshold_calibration_config(config)
    rows: list[dict[str, object]] = []
    minimum = config.threshold_calibration_min_signals_per_window
    for (direction, threshold), group in metrics_by_window.groupby(
        ["Direction", "Threshold"], sort=True
    ):
        windows = len(group)
        required_windows = math.ceil(
            windows * config.threshold_calibration_min_window_fraction
        )
        meets_minimum = group["SignalCount"] >= minimum
        directional_return = pd.to_numeric(
            group["DirectionalReturnMean"], errors="coerce"
        )
        catastrophic = (
            (group["Precision"] <= 0)
            | (directional_return <= 0)
            | (group["OppositeMoveFrequency"] > group["FavorableMoveFrequency"])
        )
        windows_meeting = int(meets_minimum.sum())
        rows.append(
            {
                "Direction": direction,
                "Threshold": float(threshold),
                "ReferenceThreshold": bool(np.isclose(threshold, 0.5)),
                "Windows": windows,
                "RequiredWindowsMeetingMinSignals": required_windows,
                "WindowsMeetingMinSignals": windows_meeting,
                "WindowCoverage": windows_meeting / windows,
                "Eligible": windows_meeting >= required_windows,
                "TotalSignals": int(group["SignalCount"].sum()),
                "SignalCountMedian": float(group["SignalCount"].median()),
                "SignalCountWorst": int(group["SignalCount"].min()),
                "SignalProportionMedian": float(group["SignalProportion"].median()),
                "SignalProportionStd": float(group["SignalProportion"].std(ddof=0)),
                "PrecisionMedian": float(group["Precision"].median()),
                "PrecisionStd": float(group["Precision"].std(ddof=0)),
                "PrecisionWorst": float(group["Precision"].min()),
                "RecallMedian": float(group["Recall"].median()),
                "F1Median": float(group["F1"].median()),
                "IntradayReturnMeanMedian": float(group["IntradayReturnMean"].median()),
                "IntradayReturnMedianMedian": float(group["IntradayReturnMedian"].median()),
                "DirectionalReturnMeanMedian": float(directional_return.median()),
                "DirectionalReturnMeanStd": float(directional_return.std(ddof=0)),
                "FavorableMoveFrequencyMedian": float(
                    group["FavorableMoveFrequency"].median()
                ),
                "OppositeMoveFrequencyMedian": float(
                    group["OppositeMoveFrequency"].median()
                ),
                "MFEMeanMedian": float(group["MFEMean"].median()),
                "MAEMeanMedian": float(group["MAEMean"].median()),
                "CatastrophicWindows": int(catastrophic.sum()),
                "DistanceFromReference": abs(float(threshold) - 0.5),
            }
        )
    summary = pd.DataFrame(rows)
    summary["Selected"] = False
    summary["SelectionRank"] = pd.Series(pd.NA, index=summary.index, dtype="Int64")
    selected: dict[str, dict[str, object]] = {}
    for direction, group in summary.groupby("Direction", sort=True):
        eligible = group[group["Eligible"]]
        if eligible.empty:
            selected[direction] = {
                "status": "no_eligible_threshold",
                "threshold": None,
                "selection_order": THRESHOLD_SELECTION_ORDER,
            }
            continue
        ranked = eligible.sort_values(
            [
                "WindowsMeetingMinSignals",
                "CatastrophicWindows",
                "PrecisionMedian",
                "DirectionalReturnMeanMedian",
                "OppositeMoveFrequencyMedian",
                "PrecisionStd",
                "F1Median",
                "DistanceFromReference",
                "Threshold",
            ],
            ascending=[False, True, False, False, True, True, False, True, False],
            kind="stable",
        )
        summary.loc[ranked.index, "SelectionRank"] = np.arange(1, len(ranked) + 1)
        winner_index = ranked.index[0]
        summary.loc[winner_index, "Selected"] = True
        winner = summary.loc[winner_index]
        selected[direction] = {
            "status": "selected",
            "threshold": float(winner["Threshold"]),
            "total_signals": int(winner["TotalSignals"]),
            "windows_meeting_min_signals": int(winner["WindowsMeetingMinSignals"]),
            "selection_order": THRESHOLD_SELECTION_ORDER,
        }
    return summary.sort_values(
        ["Direction", "Eligible", "SelectionRank", "Threshold"],
        ascending=[True, False, True, True],
        na_position="last",
        kind="stable",
    ).reset_index(drop=True), selected


def _baseline_comparison(summary: pd.DataFrame) -> pd.DataFrame:
    rows: list[pd.Series] = []
    for direction in ("Up", "Down"):
        directional = summary[summary["Direction"] == direction]
        reference = directional[directional["ReferenceThreshold"]]
        if not reference.empty:
            row = reference.iloc[0].copy()
            row["ThresholdRole"] = "Reference0.5"
            rows.append(row)
        winner = directional[directional["Selected"]]
        if not winner.empty:
            row = winner.iloc[0].copy()
            row["ThresholdRole"] = "Calibrated"
            rows.append(row)
    return pd.DataFrame(rows).reset_index(drop=True)


def calibrate_thresholds(
    development_predictions: pd.DataFrame, config: RStockConfig
) -> ThresholdCalibrationResult:
    """Calibrate Up and Down thresholds using development predictions only."""

    distribution = probability_distribution(development_predictions)
    grid = adaptive_threshold_grid(development_predictions, config)
    by_window = evaluate_threshold_grid(development_predictions, grid, config)
    summary, selected = summarize_and_select_thresholds(by_window, config)
    return ThresholdCalibrationResult(
        probability_distribution=distribution,
        threshold_grid=grid,
        metrics_by_window=by_window,
        metrics_by_threshold=summary,
        selected_thresholds=selected,
        baseline_comparison=_baseline_comparison(summary),
    )


def generate_development_probabilities(
    development: pd.DataFrame,
    sampled_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    min_train_size: int,
    test_size: int,
    step_size: int,
    parameters: XGBoostParameters = EXPERIMENTAL_XGBOOST_PARAMETERS,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> pd.DataFrame:
    """Generate out-of-sample development probabilities with expanding windows."""

    context = (development, config, min_train_size, test_size, step_size, parameters)
    results = run_combination_tasks(
        [row.to_dict() for _, row in sampled_sets.iterrows()],
        combination_workers=config.combination_workers,
        worker_context=context,
        context_initializer=_set_threshold_development_context,
        process_task=_threshold_development_process_task,
        serial_task=_threshold_development_combination,
        item_label=lambda values: symbol_set_id(pd.Series(values)),
        stage="threshold_calibration",
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details={"directions": 2, "combination_workers": config.combination_workers},
    )
    return pd.DataFrame([record for result in results for record in result])


def generate_holdout_probabilities(
    development: pd.DataFrame,
    holdout: pd.DataFrame,
    sampled_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    parameters: XGBoostParameters = EXPERIMENTAL_XGBOOST_PARAMETERS,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> pd.DataFrame:
    """Fit on development and produce untouched-holdout probabilities once."""

    records: list[dict[str, object]] = []
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
        diagnostics = [
            intraday_return_column(observation),
            mfe_column(observation),
            mae_column(observation),
        ]
        required = [*names, *outcomes.values(), *diagnostics]
        train = development[required].dropna()
        test = holdout[required].dropna()
        if train.index.max() >= test.index.min():
            raise AssertionError("Final holdout leaked into threshold training data")
        for direction, outcome in outcomes.items():
            booster = fit_booster(train, names, outcome, config, parameters=parameters)
            probabilities = predict_probabilities(booster, test, names)
            records.extend(
                {
                    "Set": set_name,
                    "Observation": observation,
                    "Direction": direction,
                    "Window": 0,
                    "TrainEnd": train.index.max(),
                    "Date": date,
                    "Probability": float(probability),
                    "Target": int(test.at[date, outcome]),
                    "IntradayReturn": float(
                        test.at[date, intraday_return_column(observation)]
                    ),
                    "MFE": float(test.at[date, mfe_column(observation)]),
                    "MAE": float(test.at[date, mae_column(observation)]),
                }
                for date, probability in zip(test.index, probabilities, strict=True)
            )
        report_progress(
            progress_callback,
            "final_holdout",
            substage=set_name,
            completed_units=completed,
            total_units=len(sampled_sets),
        )
    return pd.DataFrame(records)


def apply_frozen_thresholds(
    predictions: pd.DataFrame,
    selected_thresholds: Mapping[str, Mapping[str, object]],
) -> pd.DataFrame:
    """Apply already-selected thresholds without performing any selection."""

    result = predictions.copy()
    result["Threshold"] = np.nan
    result["Prediction"] = pd.Series(pd.NA, index=result.index, dtype="Int64")
    for direction in ("Up", "Down"):
        selection = selected_thresholds.get(direction, {})
        threshold = selection.get("threshold")
        if selection.get("status") != "selected" or threshold is None:
            raise ValueError(f"No frozen threshold is available for {direction}")
        mask = result["Direction"] == direction
        result.loc[mask, "Threshold"] = float(threshold)
        result.loc[mask, "Prediction"] = binary_predictions(
            result.loc[mask, "Probability"], float(threshold)
        )
    return result


def evaluate_applied_thresholds(
    predictions: pd.DataFrame, config: RStockConfig
) -> pd.DataFrame:
    rows = []
    for direction, group in predictions.groupby("Direction", sort=True):
        threshold = float(group["Threshold"].iloc[0])
        metrics = _threshold_window_metrics(group, direction, threshold, config)
        rows.append(
            {
                "Direction": direction,
                "Threshold": threshold,
                "Observations": len(group),
                **metrics,
            }
        )
    return pd.DataFrame(rows)


def run_controlled_threshold_calibration(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    combinations_per_target: int = 3,
    min_train_size: int | None = None,
    test_size: int | None = None,
    step_size: int | None = None,
    final_holdout_size: int | None = None,
    evaluate_final_holdout: bool = False,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> ControlledThresholdCalibrationResult:
    """Calibrate on development and optionally apply frozen thresholds to holdout."""

    validate_threshold_calibration_config(config)
    min_train = min_train_size or config.walk_forward_min_train_size
    test_window = test_size or config.walk_forward_test_size
    step = step_size or config.walk_forward_step_size
    holdout_size = final_holdout_size or config.final_holdout_size
    sampled = deterministic_combination_sample(
        generated_sets,
        per_target=combinations_per_target,
        seed=config.xgb_seed,
    )
    development, holdout, holdout_start = split_development_holdout(
        prepared, holdout_size
    )
    report_progress(progress_callback, "walk_forward", substage="started", details={"phase_event": "started", "combinations": len(sampled)})
    development_predictions = generate_development_probabilities(
        development,
        sampled,
        config,
        min_train_size=min_train,
        test_size=test_window,
        step_size=step,
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
    )
    report_progress(progress_callback, "walk_forward", substage="completed", details={"phase_event": "completed"})
    report_progress(progress_callback, "metrics", substage="started", details={"phase_event": "started"})
    calibration = calibrate_thresholds(development_predictions, config)
    frozen_json = json.dumps(
        calibration.selected_thresholds, sort_keys=True, separators=(",", ":")
    )
    frozen_digest = hashlib.sha256(frozen_json.encode()).hexdigest()

    holdout_predictions = pd.DataFrame()
    holdout_metrics = pd.DataFrame()
    if evaluate_final_holdout:
        report_progress(progress_callback, "final_holdout", substage="started", details={"phase_event": "started"})
        raw_holdout = generate_holdout_probabilities(
            development,
            holdout,
            sampled,
            config,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
        holdout_predictions = apply_frozen_thresholds(
            raw_holdout, calibration.selected_thresholds
        )
        holdout_metrics = evaluate_applied_thresholds(holdout_predictions, config)
        report_progress(progress_callback, "final_holdout", substage="completed", details={"phase_event": "completed"})
    else:
        report_progress(progress_callback, "final_holdout", substage="completed", details={"phase_event": "completed", "skipped": True})

    sampled_output = sampled.copy()
    sampled_output.insert(
        0, "Set", [symbol_set_id(row) for _, row in sampled.iterrows()]
    )
    run_configuration = {
        "protocol": "development_threshold_selection_then_optional_frozen_holdout",
        "holdout_used_for_selection": False,
        "holdout_evaluated": evaluate_final_holdout,
        "frozen_threshold_digest": frozen_digest,
        "xgboost_parameters": EXPERIMENTAL_XGBOOST_PARAMETERS.as_dict(),
        "selection_order": THRESHOLD_SELECTION_ORDER,
        "minimum_signals_per_window": (
            config.threshold_calibration_min_signals_per_window
        ),
        "minimum_window_fraction": config.threshold_calibration_min_window_fraction,
        "threshold_quantiles": list(config.threshold_calibration_quantiles),
        "threshold_grid_decimals": config.threshold_calibration_grid_decimals,
        "development_end": development.index.max().isoformat(),
        "final_holdout_start": holdout_start.isoformat(),
        "final_holdout_size": holdout_size,
        "combination_workers": config.combination_workers,
        "sampled_combinations": len(sampled),
        "selected_thresholds": calibration.selected_thresholds,
    }
    report_progress(progress_callback, "metrics", substage="completed", details={"phase_event": "completed"})
    return ControlledThresholdCalibrationResult(
        development_predictions=development_predictions,
        calibration=calibration,
        sampled_combinations=sampled_output,
        holdout_predictions=holdout_predictions,
        holdout_metrics=holdout_metrics,
        run_configuration=run_configuration,
    )


def write_threshold_calibration_results(
    result: ControlledThresholdCalibrationResult, directory: Path
) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    result.development_predictions.to_csv(
        directory / "development_probabilities.csv", index=False
    )
    result.calibration.probability_distribution.to_csv(
        directory / "probability_distribution.csv", index=False
    )
    result.calibration.threshold_grid.to_csv(
        directory / "threshold_grid.csv", index=False
    )
    result.calibration.metrics_by_window.to_csv(
        directory / "threshold_metrics_by_window.csv", index=False
    )
    result.calibration.metrics_by_threshold.to_csv(
        directory / "threshold_metrics_by_threshold.csv", index=False
    )
    result.calibration.baseline_comparison.to_csv(
        directory / "baseline_vs_calibrated_threshold.csv", index=False
    )
    result.sampled_combinations.to_csv(
        directory / "sampled_combinations.csv", index=False
    )
    (directory / "selected_thresholds.json").write_text(
        json.dumps(result.calibration.selected_thresholds, indent=2) + "\n",
        encoding="utf-8",
    )
    (directory / "run_configuration.json").write_text(
        json.dumps(result.run_configuration, indent=2) + "\n", encoding="utf-8"
    )
    if result.run_configuration["holdout_evaluated"]:
        result.holdout_predictions.to_csv(
            directory / "holdout_predictions.csv", index=False
        )
        result.holdout_metrics.to_csv(directory / "holdout_metrics.csv", index=False)
