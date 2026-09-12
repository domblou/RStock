"""Training orchestration for generated RStock symbol sets."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from math import floor
from pathlib import Path

import numpy as np
import pandas as pd

from .combinations import symbol_set_id, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, classification_metrics, outcome_error
from .features import intraday_target_column, predictor_columns
from .modeling import fit_booster, predict_probabilities
from .persistence import ModelMetadata, model_store_transaction, save_model_bundle


@dataclass(slots=True)
class TrainingResult:
    evaluated_sets: pd.DataFrame
    survey_sets: pd.DataFrame


def train_models(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    market_calendars: Mapping[str, str],
    *,
    models_directory: Path | None = None,
) -> TrainingResult:
    """Train on the oldest 70% and evaluate on the newest 30% of valid rows."""

    directory = models_directory or config.models_path
    if not isinstance(prepared.index, pd.DatetimeIndex):
        raise TypeError("Prepared data must use a DatetimeIndex")
    if prepared.index.hasnans or prepared.index.has_duplicates:
        raise ValueError("Prepared data dates must be complete and unique")
    ordered = prepared.sort_index()
    evaluated = generated_sets.copy()
    evaluated["Err"] = 0.0
    evaluated["Set"] = ""
    for column in ("TN", "FP", "FN", "TP"):
        evaluated[column] = 0
    for column in (
        "Accuracy", "Precision", "Recall", "F1", "ROCAUC", "PRAUC",
        "Prevalence",
    ):
        evaluated[column] = np.nan
    for column in ("TrainStart", "TrainEnd", "TestStart", "TestEnd"):
        evaluated[column] = pd.NaT
    for column in ("TrainRows", "TestRows"):
        evaluated[column] = 0

    with model_store_transaction(directory) as staging_directory:
        for ordinal, (row_index, row) in enumerate(evaluated.iterrows()):
            observation, feature_symbols = symbols_from_set(row)
            outcome_name = intraday_target_column(observation)
            if outcome_name not in ordered:
                raise KeyError(f"Missing outcome column: {outcome_name}")
            if observation not in market_calendars:
                raise ValueError(f"No market calendar declared for {observation}")
            names = predictor_columns(
                ordered,
                feature_symbols,
                config.lag_depth,
                config.date_feature_regex,
            )
            if not names:
                raise ValueError(f"No predictors available for set targeting {observation}")

            model_data = ordered[[*names, outcome_name]].dropna()
            train_size = floor(config.train_fraction * len(model_data))
            if train_size < 1 or train_size >= len(model_data):
                raise ValueError(
                    f"Set targeting {observation} has insufficient complete observations"
                )
            train = model_data.iloc[:train_size]
            test = model_data.iloc[train_size:]
            if train.index.max() >= test.index.min():
                raise AssertionError("Temporal split leaked test dates into training")

            booster = fit_booster(train, names, outcome_name, config)
            probabilities = predict_probabilities(booster, test, names)
            predicted = binary_predictions(probabilities, config.prediction_threshold)
            error = outcome_error(predicted, test[outcome_name].to_numpy())
            metrics = classification_metrics(
                test[outcome_name].to_numpy(), predicted, probabilities
            )
            set_name = symbol_set_id(row)
            evaluated.at[row_index, "Err"] = error
            evaluated.at[row_index, "Set"] = set_name
            evaluated.at[row_index, "TrainStart"] = train.index.min()
            evaluated.at[row_index, "TrainEnd"] = train.index.max()
            evaluated.at[row_index, "TestStart"] = test.index.min()
            evaluated.at[row_index, "TestEnd"] = test.index.max()
            evaluated.at[row_index, "TrainRows"] = len(train)
            evaluated.at[row_index, "TestRows"] = len(test)
            for column, value in metrics.as_columns().items():
                evaluated.at[row_index, column] = value

            if error < config.keep_predictor_under:
                metadata = ModelMetadata(
                    schema_version=4,
                    set_name=set_name,
                    observation=observation,
                    features=feature_symbols,
                    predictor_columns=names,
                    error=error,
                    model_file="",
                    market_calendar=market_calendars[observation],
                    train_start=train.index.min().date().isoformat(),
                    train_end=train.index.max().date().isoformat(),
                    test_start=test.index.min().date().isoformat(),
                    test_end=test.index.max().date().isoformat(),
                    target_column=outcome_name,
                    target_definition="(Close_J / Open_J) - 1",
                    target_threshold=config.intraday_target_threshold,
                    lag_depth=config.lag_depth,
                    classification_metrics=metrics.as_dict(),
                )
                save_model_bundle(
                    booster, metadata, staging_directory, f"model_{ordinal:06d}"
                )

    survey = evaluated[evaluated["Err"] < config.keep_predictor_under].copy()
    return TrainingResult(evaluated_sets=evaluated, survey_sets=survey)
