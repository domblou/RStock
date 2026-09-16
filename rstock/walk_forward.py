"""Expanding-window temporal evaluation over successive future periods."""

from __future__ import annotations

import json
import logging
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from time import perf_counter

import numpy as np
import pandas as pd

from .combinations import symbol_set_id, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, classification_metrics
from .features import (
    close_to_close_return_column,
    intraday_down_target_column,
    intraday_return_column,
    intraday_target_column,
    mae_column,
    mfe_column,
    overnight_return_column,
    predictor_columns,
)
from .modeling import fit_booster, predict_probabilities
from .model_selection import model_selection_parameters, score_qualified_models
from .parallel import (
    iter_combination_batches,
    iter_indexed_combination_batches,
    process_cancellation_requested,
)
from .progress import (
    CancellationCheck,
    ProgressCallback,
    check_cancellation,
    report_progress,
)
from .qualification import (
    qualification_parameters,
    qualify_combinations,
    rank_qualified_combinations,
)
from .risk import conditional_signal_metrics, intraday_risk_metrics
from .telemetry import dataframe_bytes, process_rss_bytes


LOGGER = logging.getLogger(__name__)
INSUFFICIENT_WALK_FORWARD_OBSERVATIONS = "insufficient_walk_forward_observations"


@dataclass(frozen=True, slots=True)
class TemporalWindow:
    number: int
    train_slice: slice
    test_slice: slice


@dataclass(slots=True)
class WalkForwardResult:
    windows: pd.DataFrame
    predictions: pd.DataFrame
    aggregate_by_window: pd.DataFrame
    aggregate_by_set: pd.DataFrame
    aggregate_global: pd.DataFrame
    qualification: pd.DataFrame
    final_holdout: pd.DataFrame
    final_holdout_predictions: pd.DataFrame
    selection_results: pd.DataFrame
    risk_by_window: pd.DataFrame
    risk_by_set: pd.DataFrame
    risk_global: pd.DataFrame
    final_holdout_risk: pd.DataFrame
    run_configuration: dict[str, object]


@dataclass(slots=True)
class _WalkForwardCombinationResult:
    window_records: list[dict[str, object]]
    prediction_records: list[dict[str, object]]


@dataclass(slots=True)
class PrefilterWalkForwardResult:
    """Only the development qualification consumed by predictor selection."""

    qualification: pd.DataFrame
    telemetry: dict[str, object]
    exploitable_targets: tuple[str, ...]
    excluded_targets: dict[str, str]


class InsufficientWalkForwardObservations(ValueError):
    """A single combination cannot form even one development window."""

    def __init__(self, details: dict[str, object]) -> None:
        self.details = details
        super().__init__("Not enough observations for one walk-forward test window")


_WALK_FORWARD_TASK_CONTEXT: tuple[
    pd.DataFrame,
    RStockConfig,
    pd.Timestamp,
    Mapping[str, str],
    int,
    int,
    int,
] | None = None


def _validate_prepared_index(prepared: pd.DataFrame) -> None:
    """Enforce the shared walk-forward date-index contract."""

    if not isinstance(prepared.index, pd.DatetimeIndex):
        raise TypeError("Prepared data must use a DatetimeIndex")
    if prepared.index.hasnans or prepared.index.has_duplicates:
        raise ValueError("Prepared data dates must be complete and unique")


def _set_walk_forward_task_context(
    context: tuple[
        pd.DataFrame,
        RStockConfig,
        pd.Timestamp,
        Mapping[str, str],
        int,
        int,
        int,
    ]
) -> None:
    global _WALK_FORWARD_TASK_CONTEXT
    _WALK_FORWARD_TASK_CONTEXT = context


def _walk_forward_combination(
    row_values: dict[str, object],
    context: tuple[
        pd.DataFrame,
        RStockConfig,
        pd.Timestamp,
        Mapping[str, str],
        int,
        int,
        int,
    ],
    cancellation_check: CancellationCheck | None,
) -> _WalkForwardCombinationResult:
    """Evaluate one combination; its chronological windows remain sequential."""

    ordered, config, holdout_start, market_calendars, min_train, test_window, step = context
    row = pd.Series(row_values)
    observation, feature_symbols = symbols_from_set(row)
    up_outcome_name = intraday_target_column(observation)
    down_outcome_name = intraday_down_target_column(observation)
    names = predictor_columns(
        ordered,
        feature_symbols,
        config.lag_depth,
        config.date_feature_regex,
    )
    if up_outcome_name not in ordered or down_outcome_name not in ordered or not names:
        raise ValueError(f"Incomplete columns for set targeting {observation}")
    model_data = ordered[[*names, up_outcome_name, down_outcome_name]].dropna()
    rows_lost_to_lags = int(ordered[up_outcome_name].notna().sum() - len(model_data))
    set_name = symbol_set_id(row)
    predictors_json = json.dumps(feature_symbols, ensure_ascii=False, separators=(",", ":"))
    development_data = model_data.loc[model_data.index < holdout_start]
    if len(development_data) <= min_train:
        def date_range(frame: pd.DataFrame) -> tuple[str | None, str | None]:
            if frame.empty:
                return None, None
            return (
                pd.Timestamp(frame.index.min()).date().isoformat(),
                pd.Timestamp(frame.index.max()).date().isoformat(),
            )

        raw_min, raw_max = date_range(ordered)
        model_min, model_max = date_range(model_data)
        development_min, development_max = date_range(development_data)
        raise InsufficientWalkForwardObservations(
            {
                "RawObservations": len(ordered),
                "ModelObservations": len(model_data),
                "DevelopmentObservations": len(development_data),
                "RawDateMin": raw_min,
                "RawDateMax": raw_max,
                "ModelDateMin": model_min,
                "ModelDateMax": model_max,
                "DevelopmentDateMin": development_min,
                "DevelopmentDateMax": development_max,
                "MinimumRequiredObservations": min_train + 1,
                "RowsLostToLags": rows_lost_to_lags,
            }
        )
    window_records: list[dict[str, object]] = []
    prediction_records: list[dict[str, object]] = []
    for window in expanding_windows(len(development_data), min_train, test_window, step):
        check_cancellation(cancellation_check)
        train = development_data.iloc[window.train_slice]
        test = development_data.iloc[window.test_slice]
        if train.index.max() >= test.index.min():
            raise AssertionError("Walk-forward window leaked future test data")
        up_booster = fit_booster(train, names, up_outcome_name, config)
        down_booster = fit_booster(train, names, down_outcome_name, config)
        up_probabilities = predict_probabilities(up_booster, test, names)
        down_probabilities = predict_probabilities(down_booster, test, names)
        up_predicted = binary_predictions(up_probabilities, config.prediction_threshold)
        down_predicted = binary_predictions(down_probabilities, config.prediction_threshold)
        up_actual = test[up_outcome_name].astype(int).to_numpy()
        down_actual = test[down_outcome_name].astype(int).to_numpy()
        window_record: dict[str, object] = {
            "Set": set_name,
            "Observation": observation,
            "Predictors": predictors_json,
            "MarketCalendar": market_calendars.get(observation),
            "Window": window.number,
            "TrainStart": train.index.min(),
            "TrainEnd": train.index.max(),
            "TestStart": test.index.min(),
            "TestEnd": test.index.max(),
            "TrainObservations": len(train),
            "TestObservations": len(test),
            "Predictions": len(up_predicted),
            "UpPositiveOutcomes": int(up_actual.sum()),
            "DownPositiveOutcomes": int(down_actual.sum()),
            "RowsLostToLags": rows_lost_to_lags,
        }
        window_record.update(_prefixed_metric_record("Up", up_actual, up_predicted, up_probabilities))
        window_record.update(_prefixed_metric_record("Down", down_actual, down_predicted, down_probabilities))
        window_records.append(window_record)
        prediction_records.extend(
            {
                "Set": set_name,
                "Observation": observation,
                "Predictors": predictors_json,
                "MarketCalendar": market_calendars.get(observation),
                "Window": window.number,
                "Date": date,
                **_return_diagnostics(ordered, observation, date),
                "UpPrediction": int(up_prediction),
                "UpProbability": float(up_probability),
                "DownPrediction": int(down_prediction),
                "DownProbability": float(down_probability),
            }
            for date, up_prediction, up_probability, down_prediction, down_probability in zip(
                test.index,
                up_predicted,
                up_probabilities,
                down_predicted,
                down_probabilities,
                strict=True,
            )
        )
    return _WalkForwardCombinationResult(window_records, prediction_records)


def _walk_forward_process_task(
    row_values: dict[str, object],
) -> _WalkForwardCombinationResult:
    if _WALK_FORWARD_TASK_CONTEXT is None:  # pragma: no cover - process invariant
        raise RuntimeError("Walk-forward worker context is unavailable")
    return _walk_forward_combination(
        row_values,
        _WALK_FORWARD_TASK_CONTEXT,
        process_cancellation_requested,
    )


def _prefilter_combination(
    row_values: dict[str, object],
    context: tuple[
        pd.DataFrame,
        RStockConfig,
        pd.Timestamp,
        Mapping[str, str],
        int,
        int,
        int,
    ],
    cancellation_check: CancellationCheck | None,
) -> dict[str, object]:
    """Return one exact qualification row and discard prediction-level detail."""

    try:
        result = _walk_forward_combination(row_values, context, cancellation_check)
    except InsufficientWalkForwardObservations as error:
        _, _, _, market_calendars, _, _, _ = context
        row = pd.Series(row_values)
        observation, feature_symbols = symbols_from_set(row)
        predictors_json = json.dumps(
            feature_symbols, ensure_ascii=False, separators=(",", ":")
        )
        details = error.details
        LOGGER.warning(
            "Prefilter skipped insufficient walk-forward observations: "
            "target=%s predictors=%s raw=%s model=%s development=%s minimum=%s "
            "raw_dates=%s..%s model_dates=%s..%s development_dates=%s..%s",
            observation,
            predictors_json,
            details["RawObservations"],
            details["ModelObservations"],
            details["DevelopmentObservations"],
            details["MinimumRequiredObservations"],
            details["RawDateMin"],
            details["RawDateMax"],
            details["ModelDateMin"],
            details["ModelDateMax"],
            details["DevelopmentDateMin"],
            details["DevelopmentDateMax"],
        )
        return {
            "Set": symbol_set_id(row),
            "Observation": observation,
            "Predictors": predictors_json,
            "MarketCalendar": market_calendars.get(observation),
            "WindowsEvaluated": 0,
            "AUCWindows": 0,
            "ROCAUCMedian": np.nan,
            "ROCAUCMean": np.nan,
            "ROCAUCStd": np.nan,
            "ROCAUCWorst": np.nan,
            "PctWindowsAboveRandom": np.nan,
            "PRAUCMedian": np.nan,
            "MeanPrevalence": np.nan,
            "TotalObservations": 0,
            "PositiveObservations": 0,
            "AggregatePrecision": np.nan,
            "AggregateRecall": np.nan,
            "AggregateF1": np.nan,
            "Eligible": False,
            "IneligibilityReasons": json.dumps(
                [INSUFFICIENT_WALK_FORWARD_OBSERVATIONS], separators=(",", ":")
            ),
            "PrefilterSkipReason": INSUFFICIENT_WALK_FORWARD_OBSERVATIONS,
            **details,
        }
    qualification = qualify_combinations(
        pd.DataFrame(result.window_records),
        pd.DataFrame(result.prediction_records),
        context[1],
    )
    return qualification.drop(columns="EligibleRank").iloc[0].to_dict()


def _prefilter_population_diagnostics(
    records: list[dict[str, object]], task_rows: list[dict[str, object]]
) -> tuple[dict[str, object], tuple[str, ...], dict[str, str]]:
    """Summarize local data exclusions without conflating them with qualification."""

    requested_targets = tuple(
        dict.fromkeys(
            symbols_from_set(pd.Series(values))[0] for values in task_rows
        )
    )
    rows = pd.DataFrame(records)
    skip_reason = rows.get(
        "PrefilterSkipReason", pd.Series(pd.NA, index=rows.index, dtype="string")
    ).fillna("")
    skipped = skip_reason == INSUFFICIENT_WALK_FORWARD_OBSERVATIONS
    admissible = ~skipped
    exploitable_targets = tuple(
        target
        for target in requested_targets
        if bool((rows["Observation"].eq(target) & admissible).any())
    )
    excluded_targets = {
        target: INSUFFICIENT_WALK_FORWARD_OBSERVATIONS
        for target in requested_targets
        if target not in exploitable_targets
    }
    attempted = len(task_rows)
    admissible_count = int(admissible.sum())
    skipped_count = int(skipped.sum())
    return (
        {
            "pairs_attempted": attempted,
            "pairs_admissible": admissible_count,
            "pairs_skipped": skipped_count,
            "pair_coverage_percent": (
                0.0 if attempted == 0 else 100.0 * admissible_count / attempted
            ),
            "targets_requested": len(requested_targets),
            "targets_exploitable": len(exploitable_targets),
            "targets_excluded": [
                {"target": target, "reason": reason}
                for target, reason in excluded_targets.items()
            ],
        },
        exploitable_targets,
        excluded_targets,
    )


def _prefilter_process_task(row_values: dict[str, object]) -> dict[str, object]:
    if _WALK_FORWARD_TASK_CONTEXT is None:  # pragma: no cover - process invariant
        raise RuntimeError("Walk-forward worker context is unavailable")
    return _prefilter_combination(
        row_values,
        _WALK_FORWARD_TASK_CONTEXT,
        process_cancellation_requested,
    )


def expanding_windows(
    observation_count: int,
    min_train_size: int,
    test_size: int,
    step_size: int,
) -> list[TemporalWindow]:
    """Return expanding train windows followed strictly by future test windows."""

    if min_train_size < 1 or test_size < 1 or step_size < 1:
        raise ValueError("Walk-forward window sizes must be positive")
    if observation_count <= min_train_size:
        raise ValueError("Not enough observations for one walk-forward test window")

    windows: list[TemporalWindow] = []
    train_end = min_train_size
    number = 1
    while train_end < observation_count:
        test_end = min(train_end + test_size, observation_count)
        windows.append(
            TemporalWindow(number, slice(0, train_end), slice(train_end, test_end))
        )
        number += 1
        train_end += step_size
    return windows


def _metric_record(actual, predicted, probabilities) -> dict[str, object]:
    return classification_metrics(actual, predicted, probabilities).as_columns()


def _prefixed_metric_record(
    prefix: str, actual, predicted, probabilities
) -> dict[str, object]:
    return {
        f"{prefix}{name}": value
        for name, value in _metric_record(actual, predicted, probabilities).items()
    }


def _return_diagnostics(
    prepared: pd.DataFrame, observation: str, date: pd.Timestamp
) -> dict[str, float | int]:
    intraday_target = prepared.at[date, intraday_target_column(observation)]
    down_target = prepared.at[date, intraday_down_target_column(observation)]
    return {
        "OvernightReturn": float(
            prepared.at[date, overnight_return_column(observation)]
        ),
        "IntradayReturn": float(
            prepared.at[date, intraday_return_column(observation)]
        ),
        "CloseToCloseReturn": float(
            prepared.at[date, close_to_close_return_column(observation)]
        ),
        "IntradayTarget": int(intraday_target),
        "UpTarget": int(intraday_target),
        "DownTarget": int(down_target),
        "MFE": float(prepared.at[date, mfe_column(observation)]),
        "MAE": float(prepared.at[date, mae_column(observation)]),
    }


def _aggregate_predictions(
    predictions: pd.DataFrame,
    windows: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    windows_by_set = {
        set_name: group for set_name, group in windows.groupby("Set", sort=False)
    }
    empty_windows = windows.iloc[0:0]
    by_set: list[dict[str, object]] = []
    for set_name, group in predictions.groupby("Set", sort=False):
        window_group = windows_by_set.get(set_name, empty_windows)
        record: dict[str, object] = {
            "Set": set_name,
            "Observation": group.iloc[0]["Observation"],
            "Windows": int(group["Window"].nunique()),
            "TestObservations": len(group),
            "UniqueTestDates": int(group["Date"].nunique()),
            "Predictions": len(group),
            "UpPositiveOutcomes": int(group["UpTarget"].sum()),
            "DownPositiveOutcomes": int(group["DownTarget"].sum()),
        }
        record.update(
            _prefixed_metric_record(
                "Up", group["UpTarget"], group["UpPrediction"], group["UpProbability"]
            )
        )
        record.update(
            _prefixed_metric_record(
                "Down",
                group["DownTarget"],
                group["DownPrediction"],
                group["DownProbability"],
            )
        )
        for prefix in ("Up", "Down"):
            record[f"{prefix}AccuracyStd"] = float(
                window_group[f"{prefix}Accuracy"].std(ddof=0)
            )
            record[f"{prefix}AccuracyMin"] = float(
                window_group[f"{prefix}Accuracy"].min()
            )
            record[f"{prefix}AccuracyMax"] = float(
                window_group[f"{prefix}Accuracy"].max()
            )
        by_set.append(record)

    aggregate_by_set = pd.DataFrame(by_set)
    global_record: dict[str, object] = {
        "Sets": int(predictions["Set"].nunique()),
        "WindowEvaluations": len(windows),
        "TestObservations": len(predictions),
        "UniqueTestDates": int(predictions["Date"].nunique()),
        "Predictions": len(predictions),
        "UpPositiveOutcomes": int(predictions["UpTarget"].sum()),
        "DownPositiveOutcomes": int(predictions["DownTarget"].sum()),
    }
    global_record.update(
        _prefixed_metric_record(
            "Up",
            predictions["UpTarget"],
            predictions["UpPrediction"],
            predictions["UpProbability"],
        )
    )
    global_record.update(
        _prefixed_metric_record(
            "Down",
            predictions["DownTarget"],
            predictions["DownPrediction"],
            predictions["DownProbability"],
        )
    )
    return aggregate_by_set, pd.DataFrame([global_record])


def _aggregate_windows(
    predictions: pd.DataFrame, windows: pd.DataFrame
) -> pd.DataFrame:
    records: list[dict[str, object]] = []
    for window_number, group in predictions.groupby("Window", sort=True):
        definitions = windows[windows["Window"] == window_number]
        record: dict[str, object] = {
            "Window": int(window_number),
            "TrainStart": definitions["TrainStart"].min(),
            "TrainEnd": definitions["TrainEnd"].max(),
            "TestStart": definitions["TestStart"].min(),
            "TestEnd": definitions["TestEnd"].max(),
            "Sets": int(group["Set"].nunique()),
            "TestObservations": len(group),
            "UniqueTestDates": int(group["Date"].nunique()),
            "Predictions": len(group),
            "UpPositiveOutcomes": int(group["UpTarget"].sum()),
            "DownPositiveOutcomes": int(group["DownTarget"].sum()),
        }
        record.update(
            _prefixed_metric_record(
                "Up", group["UpTarget"], group["UpPrediction"], group["UpProbability"]
            )
        )
        record.update(
            _prefixed_metric_record(
                "Down",
                group["DownTarget"],
                group["DownPrediction"],
                group["DownProbability"],
            )
        )
        records.append(record)
    return pd.DataFrame(records)


def _risk_record(group: pd.DataFrame, config: RStockConfig) -> dict[str, object]:
    record: dict[str, object] = {}
    record.update(
        intraday_risk_metrics(
            group,
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    record.update(
        conditional_signal_metrics(
            group,
            "UpPrediction",
            "UpSignal",
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    record.update(
        conditional_signal_metrics(
            group,
            "DownPrediction",
            "DownSignal",
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    return record


def _aggregate_risk(
    predictions: pd.DataFrame, config: RStockConfig
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    by_window: list[dict[str, object]] = []
    for (set_name, window), group in predictions.groupby(
        ["Set", "Window"], sort=False
    ):
        record: dict[str, object] = {
            "Set": set_name,
            "Observation": group.iloc[0]["Observation"],
            "Predictors": group.iloc[0]["Predictors"],
            "Window": int(window),
            "Start": group["Date"].min(),
            "End": group["Date"].max(),
        }
        record.update(_risk_record(group, config))
        by_window.append(record)

    by_set: list[dict[str, object]] = []
    for set_name, group in predictions.groupby("Set", sort=False):
        record = {
            "Set": set_name,
            "Observation": group.iloc[0]["Observation"],
            "Predictors": group.iloc[0]["Predictors"],
        }
        record.update(_risk_record(group, config))
        by_set.append(record)

    unique_market_observations = predictions.drop_duplicates(
        ["Observation", "Date"]
    )
    global_record: dict[str, object] = {
        "Sets": int(predictions["Set"].nunique()),
        "UniqueMarketObservations": len(unique_market_observations),
    }
    global_record.update(
        intraday_risk_metrics(
            unique_market_observations,
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    global_record.update(
        conditional_signal_metrics(
            predictions,
            "UpPrediction",
            "UpSignal",
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    global_record.update(
        conditional_signal_metrics(
            predictions,
            "DownPrediction",
            "DownSignal",
            config.intraday_target_threshold,
            config.intraday_down_threshold,
        )
    )
    return pd.DataFrame(by_window), pd.DataFrame(by_set), pd.DataFrame([global_record])


def _aggregate_final_risk(
    predictions: pd.DataFrame, config: RStockConfig
) -> pd.DataFrame:
    records: list[dict[str, object]] = []
    for set_name, group in predictions.groupby("Set", sort=False):
        record: dict[str, object] = {
            "Set": set_name,
            "Observation": group.iloc[0]["Observation"],
            "Predictors": group.iloc[0]["Predictors"],
            "Start": group["Date"].min(),
            "End": group["Date"].max(),
        }
        record.update(_risk_record(group, config))
        records.append(record)
    return pd.DataFrame(records)


def _evaluate_final_holdout(
    ordered: pd.DataFrame,
    generated_sets: pd.DataFrame,
    qualification: pd.DataFrame,
    config: RStockConfig,
    holdout_start: pd.Timestamp,
    market_calendars: Mapping[str, str],
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Fit eligible sets on development only, then evaluate the untouched holdout."""

    generated_lookup = {
        symbol_set_id(row): row for _, row in generated_sets.iterrows()
    }
    metric_names = [
        "TN", "FP", "FN", "TP", "Accuracy", "Precision", "Recall", "F1",
        "ROCAUC", "PRAUC", "Prevalence",
    ]
    records: list[dict[str, object]] = []
    prediction_records: list[dict[str, object]] = []
    eligible = qualification[qualification["Eligible"]].sort_values("EligibleRank")
    for completed, (_, qualified) in enumerate(eligible.iterrows(), start=1):
        check_cancellation(cancellation_check)
        set_name = str(qualified["Set"])
        row = generated_lookup[set_name]
        observation, feature_symbols = symbols_from_set(row)
        up_outcome_name = intraday_target_column(observation)
        down_outcome_name = intraday_down_target_column(observation)
        names = predictor_columns(
            ordered,
            feature_symbols,
            config.lag_depth,
            config.date_feature_regex,
        )
        model_data = ordered[[*names, up_outcome_name, down_outcome_name]].dropna()
        development = model_data.loc[model_data.index < holdout_start]
        holdout = model_data.loc[model_data.index >= holdout_start]
        record: dict[str, object] = {
            "Set": set_name,
            "Observation": observation,
            "Predictors": json.dumps(feature_symbols, ensure_ascii=False, separators=(",", ":")),
            "MarketCalendar": market_calendars.get(observation),
            "FinalTrainStart": development.index.min() if not development.empty else pd.NaT,
            "FinalTrainEnd": development.index.max() if not development.empty else pd.NaT,
            "FinalTestStart": holdout.index.min() if not holdout.empty else pd.NaT,
            "FinalTestEnd": holdout.index.max() if not holdout.empty else pd.NaT,
            "FinalTrainObservations": len(development),
            "FinalTestObservations": len(holdout),
            "FinalPredictions": 0,
            "FinalUpPositiveOutcomes": (
                int(holdout[up_outcome_name].sum()) if not holdout.empty else 0
            ),
            "FinalDownPositiveOutcomes": (
                int(holdout[down_outcome_name].sum()) if not holdout.empty else 0
            ),
            "FinalAvailable": False,
            "FinalConfirmed": False,
        }
        record.update(
            {
                f"Final{direction}{name}": np.nan
                for direction in ("Up", "Down")
                for name in metric_names
            }
        )
        if development.empty or holdout.empty:
            records.append(record)
            continue
        if development.index.max() >= holdout.index.min():
            raise AssertionError("Final holdout leaked into model development data")

        up_booster = fit_booster(development, names, up_outcome_name, config)
        down_booster = fit_booster(development, names, down_outcome_name, config)
        up_probabilities = predict_probabilities(up_booster, holdout, names)
        down_probabilities = predict_probabilities(down_booster, holdout, names)
        up_predicted = binary_predictions(
            up_probabilities, config.prediction_threshold
        )
        down_predicted = binary_predictions(
            down_probabilities, config.prediction_threshold
        )
        up_actual = holdout[up_outcome_name].astype(int).to_numpy()
        down_actual = holdout[down_outcome_name].astype(int).to_numpy()
        up_metrics = classification_metrics(
            up_actual, up_predicted, up_probabilities
        )
        down_metrics = classification_metrics(
            down_actual, down_predicted, down_probabilities
        )
        record.update(
            {
                f"FinalUp{name}": value
                for name, value in up_metrics.as_columns().items()
            }
        )
        record.update(
            {
                f"FinalDown{name}": value
                for name, value in down_metrics.as_columns().items()
            }
        )
        record["FinalPredictions"] = len(up_predicted)
        record["FinalAvailable"] = True
        record["FinalConfirmed"] = bool(
            up_metrics.roc_auc is not None
            and up_metrics.roc_auc > config.final_confirmation_min_auc
        )
        records.append(record)
        prediction_records.extend(
            {
                "Set": set_name,
                "Observation": observation,
                "Predictors": record["Predictors"],
                "Date": date,
                **_return_diagnostics(ordered, observation, date),
                "UpPrediction": int(up_prediction),
                "UpProbability": float(up_probability),
                "DownPrediction": int(down_prediction),
                "DownProbability": float(down_probability),
            }
            for date, up_prediction, up_probability, down_prediction, down_probability in zip(
                holdout.index,
                up_predicted,
                up_probabilities,
                down_predicted,
                down_probabilities,
                strict=True,
            )
        )
        report_progress(
            progress_callback,
            "final_holdout",
            substage=set_name,
            completed_units=completed,
            total_units=len(eligible),
        )

    columns = [
        "Set", "Observation", "Predictors", "MarketCalendar",
        "FinalTrainStart", "FinalTrainEnd", "FinalTestStart", "FinalTestEnd",
        "FinalTrainObservations", "FinalTestObservations", "FinalPredictions",
        "FinalUpPositiveOutcomes", "FinalDownPositiveOutcomes",
        "FinalAvailable", "FinalConfirmed",
        *[
            f"Final{direction}{name}"
            for direction in ("Up", "Down")
            for name in metric_names
        ],
    ]
    prediction_columns = [
        "Set", "Observation", "Predictors", "Date", "OvernightReturn",
        "IntradayReturn", "CloseToCloseReturn", "IntradayTarget", "UpTarget",
        "DownTarget", "MFE", "MAE", "UpPrediction", "UpProbability",
        "DownPrediction", "DownProbability",
    ]
    return (
        pd.DataFrame(records, columns=columns),
        pd.DataFrame(prediction_records, columns=prediction_columns),
    )


def _combine_selection_results(
    qualification: pd.DataFrame, final_holdout: pd.DataFrame
) -> pd.DataFrame:
    final_metrics = final_holdout.drop(
        columns=["Observation", "Predictors"], errors="ignore"
    )
    combined = qualification.merge(final_metrics, on="Set", how="left")
    combined["FinalStatus"] = "not_evaluated_ineligible"
    eligible = combined["Eligible"]
    unavailable = eligible & ~combined["FinalAvailable"].fillna(False).astype(bool)
    confirmed = eligible & combined["FinalConfirmed"].fillna(False).astype(bool)
    combined.loc[unavailable, "FinalStatus"] = "not_confirmed_unavailable"
    combined.loc[eligible & ~unavailable, "FinalStatus"] = "not_confirmed"
    combined.loc[confirmed, "FinalStatus"] = "confirmed"
    return combined


def evaluate_prefilter_walk_forward(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    market_calendars: Mapping[str, str] | None = None,
    min_train_size: int | None = None,
    test_size: int | None = None,
    step_size: int | None = None,
    final_holdout_size: int | None = None,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
    checkpoint_manager: object | None = None,
) -> PrefilterWalkForwardResult:
    """Qualify univariate development models without building unused results.

    The scientific path is the same as :func:`evaluate_walk_forward`: each set
    is fitted on the same expanding windows and qualification is calculated from
    the same prediction rows.  Prediction-level records are released inside the
    worker after the one-row qualification record has been produced.
    """

    _validate_prepared_index(prepared)
    ordered = prepared.sort_index()
    min_train = (
        config.walk_forward_min_train_size
        if min_train_size is None
        else min_train_size
    )
    test_window = config.walk_forward_test_size if test_size is None else test_size
    step = config.walk_forward_step_size if step_size is None else step_size
    holdout_size = (
        config.final_holdout_size
        if final_holdout_size is None
        else final_holdout_size
    )
    if holdout_size < 1 or holdout_size >= len(ordered):
        raise ValueError("final_holdout_size must leave non-empty development history")
    holdout_start = ordered.index[-holdout_size]
    task_context = (
        ordered,
        config,
        holdout_start,
        market_calendars or {},
        min_train,
        test_window,
        step,
    )
    task_rows = [row.to_dict() for _, row in generated_sets.iterrows()]
    started_at = perf_counter()
    rss_before = process_rss_bytes()
    report_progress(
        progress_callback,
        "predictor_prefilter_walk_forward",
        substage="started",
        details={
            "phase_event": "started",
            "combinations": len(task_rows),
            "prepared_bytes": dataframe_bytes(ordered),
            "parent_rss_bytes": rss_before,
            "combination_workers": config.combination_workers,
            "xgb_threads_per_worker": config.xgb_nthread,
            "maximum_xgb_threads": config.combination_workers * config.xgb_nthread,
        },
    )
    batch_size = config.predictor_prefilter_batch_size
    total_batches = (len(task_rows) + batch_size - 1) // batch_size
    if checkpoint_manager is None:
        batches = iter_combination_batches(
            task_rows,
            batch_size=batch_size,
            combination_workers=config.combination_workers,
            worker_context=task_context,
            context_initializer=_set_walk_forward_task_context,
            process_task=_prefilter_process_task,
            serial_task=_prefilter_combination,
            item_label=lambda values: symbol_set_id(pd.Series(values)),
            stage="predictor_prefilter_walk_forward",
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
            details={"combination_workers": config.combination_workers},
        )
        records = [record for batch in batches for record in batch]
    else:
        manager = checkpoint_manager
        manager.set_total_batches("predictor_prefilter_walk_forward", total_batches)
        completed_ids = manager.completed_batch_ids("predictor_prefilter_walk_forward")
        for batch in iter_indexed_combination_batches(
            task_rows,
            batch_size=batch_size,
            combination_workers=config.combination_workers,
            worker_context=task_context,
            context_initializer=_set_walk_forward_task_context,
            process_task=_prefilter_process_task,
            serial_task=_prefilter_combination,
            item_label=lambda values: symbol_set_id(pd.Series(values)),
            stage="predictor_prefilter_walk_forward",
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
            details={"combination_workers": config.combination_workers},
            completed_batch_ids=completed_ids,
        ):
            checkpoint_started_at = perf_counter()
            payload = {"qualification": pd.DataFrame(batch.results)}
            manager.commit_batch(
                "predictor_prefilter_walk_forward",
                batch.batch_id,
                payload,
                first_index=batch.first_index,
                last_index=batch.last_index,
                combination_count=len(batch.results),
                row_counts={"qualification": len(payload["qualification"])},
            )
            report_progress(
                progress_callback,
                "predictor_prefilter_walk_forward",
                substage=f"batch {batch.batch_id + 1}/{total_batches}",
                completed_units=batch.last_index + 1,
                total_units=len(task_rows),
                details={
                    "batch_id": batch.batch_id,
                    "batch_number": batch.batch_id + 1,
                    "total_batches": total_batches,
                    "combinations": len(batch.results),
                    "rows": len(payload["qualification"]),
                    "elapsed_seconds": (
                        batch.elapsed_seconds + perf_counter() - checkpoint_started_at
                    ),
                    "calculation_seconds": batch.elapsed_seconds,
                    "parent_rss_bytes": process_rss_bytes(),
                    "checkpoint_written": True,
                },
            )
        records = []
        for batch_id in range(total_batches):
            payload = manager.load_batch("predictor_prefilter_walk_forward", batch_id)
            records.extend(payload["qualification"].to_dict("records"))
    population_diagnostics, exploitable_targets, excluded_targets = (
        _prefilter_population_diagnostics(records, task_rows)
    )
    qualification = rank_qualified_combinations(pd.DataFrame(records))
    elapsed = perf_counter() - started_at
    telemetry = {
        "elapsed_seconds": elapsed,
        "combinations": len(task_rows),
        "qualification_rows": len(qualification),
        "prepared_bytes": dataframe_bytes(ordered),
        "parent_rss_before_bytes": rss_before,
        "parent_rss_after_bytes": process_rss_bytes(),
        "combination_workers": config.combination_workers,
        "xgb_threads_per_worker": config.xgb_nthread,
        **population_diagnostics,
    }
    report_progress(
        progress_callback,
        "predictor_prefilter_walk_forward",
        substage="completed",
        details={"phase_event": "completed", **telemetry},
    )
    return PrefilterWalkForwardResult(
        qualification,
        telemetry,
        exploitable_targets,
        excluded_targets,
    )


def evaluate_walk_forward(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    market_calendars: Mapping[str, str] | None = None,
    min_train_size: int | None = None,
    test_size: int | None = None,
    step_size: int | None = None,
    final_holdout_size: int | None = None,
    evaluate_holdout: bool = True,
    progress_callback: ProgressCallback | None = None,
    cancellation_check: CancellationCheck | None = None,
) -> WalkForwardResult:
    """Qualify on development windows, then confirm on an untouched final holdout."""

    _validate_prepared_index(prepared)
    ordered = prepared.sort_index()
    min_train = (
        config.walk_forward_min_train_size
        if min_train_size is None
        else min_train_size
    )
    test_window = config.walk_forward_test_size if test_size is None else test_size
    step = config.walk_forward_step_size if step_size is None else step_size
    holdout_size = (
        config.final_holdout_size
        if final_holdout_size is None
        else final_holdout_size
    )
    if holdout_size < 1 or holdout_size >= len(ordered):
        raise ValueError("final_holdout_size must leave non-empty development history")
    holdout_start = ordered.index[-holdout_size]
    development_end = ordered.index[-holdout_size - 1]
    task_context = (
        ordered,
        config,
        holdout_start,
        market_calendars or {},
        min_train,
        test_window,
        step,
    )
    task_rows = [row.to_dict() for _, row in generated_sets.iterrows()]
    report_progress(
        progress_callback, "walk_forward", substage="started", details={"phase_event": "started", "combinations": len(task_rows)}
    )
    combination_batches = iter_combination_batches(
        task_rows,
        batch_size=config.walk_forward_batch_size,
        combination_workers=config.combination_workers,
        worker_context=task_context,
        context_initializer=_set_walk_forward_task_context,
        process_task=_walk_forward_process_task,
        serial_task=_walk_forward_combination,
        item_label=lambda values: symbol_set_id(pd.Series(values)),
        stage="walk_forward",
        progress_callback=progress_callback,
        cancellation_check=cancellation_check,
        details={"combination_workers": config.combination_workers},
    )
    window_records: list[dict[str, object]] = []
    prediction_records: list[dict[str, object]] = []
    for batch in combination_batches:
        for result in batch:
            window_records.extend(result.window_records)
            prediction_records.extend(result.prediction_records)
    report_progress(progress_callback, "walk_forward", substage="completed", details={"phase_event": "completed", "combinations": len(task_rows), "windows": len(window_records)})

    report_progress(progress_callback, "aggregation", substage="started", details={"phase_event": "started"})
    aggregation_started_at = perf_counter()
    windows_frame = pd.DataFrame(window_records)
    predictions_frame = pd.DataFrame(prediction_records)
    aggregation_timings = {
        "dataframe_creation": perf_counter() - aggregation_started_at,
    }
    if predictions_frame.empty:
        raise ValueError("Walk-forward evaluation produced no predictions")
    aggregate_predictions_started_at = perf_counter()
    aggregate_by_set, aggregate_global = _aggregate_predictions(
        predictions_frame, windows_frame
    )


    aggregation_timings["aggregate_predictions"] = (
        perf_counter() - aggregate_predictions_started_at
    )
    aggregate_windows_started_at = perf_counter()
    aggregate_by_window = _aggregate_windows(predictions_frame, windows_frame)
    aggregation_timings["aggregate_windows"] = (
        perf_counter() - aggregate_windows_started_at
    )
    aggregate_risk_started_at = perf_counter()
    risk_by_window, risk_by_set, risk_global = _aggregate_risk(
        predictions_frame, config
    )
    aggregation_timings["aggregate_risk"] = perf_counter() - aggregate_risk_started_at
    report_progress(
        progress_callback,
        "aggregation",
        substage="completed",
        details={
            "phase_event": "completed",
            "windows": len(windows_frame),
            "timings_seconds": {
                name: round(duration, 3)
                for name, duration in aggregation_timings.items()
            },
        },
    )
    report_progress(progress_callback, "qualification", substage="started", details={"phase_event": "started"})
    qualification = qualify_combinations(windows_frame, predictions_frame, config)
    eligibility = qualification[["Set", "Eligible", "EligibleRank"]]
    risk_by_window = risk_by_window.merge(eligibility, on="Set", how="left")
    risk_by_set = risk_by_set.merge(eligibility, on="Set", how="left")
    report_progress(progress_callback, "qualification", substage="completed", details={"phase_event": "completed", "eligible_combinations": int(qualification["Eligible"].sum())})
    report_progress(progress_callback, "final_holdout", substage="started", details={"phase_event": "started"})
    holdout_qualification = (
        qualification
        if evaluate_holdout
        else qualification.assign(Eligible=False)
    )
    final_holdout, final_predictions = _evaluate_final_holdout(
        ordered,
        generated_sets,
        holdout_qualification,
        config,
        holdout_start,
        market_calendars or {},
        progress_callback,
        cancellation_check,
    )
    report_progress(progress_callback, "final_holdout", substage="completed", details={"phase_event": "completed", "evaluated_combinations": len(final_holdout)})
    report_progress(progress_callback, "metrics", substage="started", details={"phase_event": "started"})
    final_holdout_risk = _aggregate_final_risk(final_predictions, config)
    if not final_holdout_risk.empty:
        final_holdout_risk = final_holdout_risk.merge(
            eligibility, on="Set", how="left"
        )
    selection_results = score_qualified_models(
        _combine_selection_results(qualification, final_holdout), config
    )
    aggregate_global["EligibleSets"] = int(qualification["Eligible"].sum())
    aggregate_global["EligiblePct"] = float(qualification["Eligible"].mean())
    aggregate_global["FinalConfirmedSets"] = int(
        final_holdout["FinalConfirmed"].sum()
    )
    report_progress(progress_callback, "metrics", substage="completed", details={"phase_event": "completed"})
    run_configuration: dict[str, object] = {
        "target": "intraday_return >= intraday_target_threshold",
        "down_target": "intraday_return <= -intraday_down_threshold",
        "intraday_target_threshold": config.intraday_target_threshold,
        "intraday_down_threshold": config.intraday_down_threshold,
        "conditional_signal": "predicted class == 1 (probability > 0.5)",
        "lag_depth": config.lag_depth,
        "lag_features": [f"intraday_J-{lag}" for lag in range(1, config.lag_depth + 1)],
        "walk_forward_min_train_size": min_train,
        "walk_forward_test_size": test_window,
        "walk_forward_step_size": step,
        "combination_workers": config.combination_workers,
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
        run_configuration["final_holdout_evaluated"] = False
    return WalkForwardResult(
        windows=windows_frame,
        predictions=predictions_frame,
        aggregate_by_window=aggregate_by_window,
        aggregate_by_set=aggregate_by_set,
        aggregate_global=aggregate_global,
        qualification=qualification,
        final_holdout=final_holdout,
        final_holdout_predictions=final_predictions,
        selection_results=selection_results,
        risk_by_window=risk_by_window,
        risk_by_set=risk_by_set,
        risk_global=risk_global,
        final_holdout_risk=final_holdout_risk,
        run_configuration=run_configuration,
    )


def write_walk_forward_results(result: WalkForwardResult, directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    result.windows.to_csv(directory / "windows.csv", index=False)
    result.predictions.to_csv(directory / "predictions.csv", index=False)
    result.aggregate_by_window.to_csv(directory / "aggregate_by_window.csv", index=False)
    result.aggregate_by_set.to_csv(directory / "aggregate_by_set.csv", index=False)
    result.aggregate_global.to_csv(directory / "aggregate_global.csv", index=False)
    result.qualification.to_csv(directory / "qualification.csv", index=False)
    result.final_holdout.to_csv(directory / "final_holdout.csv", index=False)
    result.final_holdout_predictions.to_csv(
        directory / "final_holdout_predictions.csv", index=False
    )
    result.selection_results.to_csv(directory / "selection_results.csv", index=False)
    result.risk_by_window.to_csv(directory / "risk_by_window.csv", index=False)
    result.risk_by_set.to_csv(directory / "risk_by_set.csv", index=False)
    result.risk_global.to_csv(directory / "risk_global.csv", index=False)
    result.final_holdout_risk.to_csv(
        directory / "final_holdout_risk.csv", index=False
    )
    (directory / "run_configuration.json").write_text(
        json.dumps(result.run_configuration, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
