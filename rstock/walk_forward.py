"""Expanding-window temporal evaluation over successive future periods."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path

import pandas as pd

from .combinations import symbol_set_id, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, classification_metrics
from .features import predictor_columns
from .modeling import fit_booster, predict_probabilities


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


def _aggregate_predictions(
    predictions: pd.DataFrame,
    windows: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    by_set: list[dict[str, object]] = []
    for set_name, group in predictions.groupby("Set", sort=False):
        window_group = windows[windows["Set"] == set_name]
        record: dict[str, object] = {
            "Set": set_name,
            "Observation": group.iloc[0]["Observation"],
            "Windows": int(group["Window"].nunique()),
            "TestObservations": len(group),
            "UniqueTestDates": int(group["Date"].nunique()),
            "Predictions": len(group),
            "PositiveOutcomes": int(group["Outcome"].sum()),
        }
        record.update(
            _metric_record(group["Outcome"], group["Prediction"], group["Probability"])
        )
        record["AccuracyStd"] = float(window_group["Accuracy"].std(ddof=0))
        record["AccuracyMin"] = float(window_group["Accuracy"].min())
        record["AccuracyMax"] = float(window_group["Accuracy"].max())
        by_set.append(record)

    aggregate_by_set = pd.DataFrame(by_set)
    global_record: dict[str, object] = {
        "Sets": int(predictions["Set"].nunique()),
        "WindowEvaluations": len(windows),
        "TestObservations": len(predictions),
        "UniqueTestDates": int(predictions["Date"].nunique()),
        "Predictions": len(predictions),
        "PositiveOutcomes": int(predictions["Outcome"].sum()),
    }
    global_record.update(
        _metric_record(
            predictions["Outcome"],
            predictions["Prediction"],
            predictions["Probability"],
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
            "PositiveOutcomes": int(group["Outcome"].sum()),
        }
        record.update(
            _metric_record(group["Outcome"], group["Prediction"], group["Probability"])
        )
        records.append(record)
    return pd.DataFrame(records)


def evaluate_walk_forward(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    market_calendars: Mapping[str, str] | None = None,
    min_train_size: int | None = None,
    test_size: int | None = None,
    step_size: int | None = None,
) -> WalkForwardResult:
    """Evaluate every symbol set on successive, strictly future windows."""

    if not isinstance(prepared.index, pd.DatetimeIndex):
        raise TypeError("Prepared data must use a DatetimeIndex")
    if prepared.index.hasnans or prepared.index.has_duplicates:
        raise ValueError("Prepared data dates must be complete and unique")
    ordered = prepared.sort_index()
    min_train = (
        config.walk_forward_min_train_size
        if min_train_size is None
        else min_train_size
    )
    test_window = config.walk_forward_test_size if test_size is None else test_size
    step = config.walk_forward_step_size if step_size is None else step_size
    window_records: list[dict[str, object]] = []
    prediction_records: list[dict[str, object]] = []

    for _, row in generated_sets.iterrows():
        observation, feature_symbols = symbols_from_set(row)
        outcome_name = f"{observation}.UPDW"
        names = predictor_columns(ordered, feature_symbols, config.date_feature_regex)
        if outcome_name not in ordered or not names:
            raise ValueError(f"Incomplete columns for set targeting {observation}")
        model_data = ordered[[*names, outcome_name]].dropna()
        set_name = symbol_set_id(row)
        for window in expanding_windows(len(model_data), min_train, test_window, step):
            train = model_data.iloc[window.train_slice]
            test = model_data.iloc[window.test_slice]
            if train.index.max() >= test.index.min():
                raise AssertionError("Walk-forward window leaked future test data")

            booster = fit_booster(train, names, outcome_name, config)
            probabilities = predict_probabilities(booster, test, names)
            predicted = binary_predictions(probabilities, config.prediction_threshold)
            actual = test[outcome_name].astype(int).to_numpy()
            metrics = _metric_record(actual, predicted, probabilities)
            window_record: dict[str, object] = {
                "Set": set_name,
                "Observation": observation,
                "MarketCalendar": (market_calendars or {}).get(observation),
                "Window": window.number,
                "TrainStart": train.index.min(),
                "TrainEnd": train.index.max(),
                "TestStart": test.index.min(),
                "TestEnd": test.index.max(),
                "TrainObservations": len(train),
                "TestObservations": len(test),
                "Predictions": len(predicted),
                "PositiveOutcomes": int(actual.sum()),
            }
            window_record.update(metrics)
            window_records.append(window_record)

            prediction_records.extend(
                {
                    "Set": set_name,
                    "Observation": observation,
                    "MarketCalendar": (market_calendars or {}).get(observation),
                    "Window": window.number,
                    "Date": date,
                    "Outcome": int(outcome),
                    "Prediction": int(prediction),
                    "Probability": float(probability),
                }
                for date, outcome, prediction, probability in zip(
                    test.index, actual, predicted, probabilities, strict=True
                )
            )

    windows_frame = pd.DataFrame(window_records)
    predictions_frame = pd.DataFrame(prediction_records)
    if predictions_frame.empty:
        raise ValueError("Walk-forward evaluation produced no predictions")
    aggregate_by_set, aggregate_global = _aggregate_predictions(
        predictions_frame, windows_frame
    )
    aggregate_by_window = _aggregate_windows(predictions_frame, windows_frame)
    return WalkForwardResult(
        windows=windows_frame,
        predictions=predictions_frame,
        aggregate_by_window=aggregate_by_window,
        aggregate_by_set=aggregate_by_set,
        aggregate_global=aggregate_global,
    )


def write_walk_forward_results(result: WalkForwardResult, directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    result.windows.to_csv(directory / "windows.csv", index=False)
    result.predictions.to_csv(directory / "predictions.csv", index=False)
    result.aggregate_by_window.to_csv(directory / "aggregate_by_window.csv", index=False)
    result.aggregate_by_set.to_csv(directory / "aggregate_by_set.csv", index=False)
    result.aggregate_global.to_csv(directory / "aggregate_global.csv", index=False)
