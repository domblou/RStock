"""Training orchestration for generated RStock symbol sets."""

from __future__ import annotations

from dataclasses import dataclass
from math import floor
from pathlib import Path

import numpy as np
import pandas as pd

from .combinations import legacy_set_name, symbols_from_set
from .config import RStockConfig
from .evaluation import binary_predictions, calculate_error
from .features import predictor_columns
from .persistence import ModelMetadata, prepare_models_directory, save_model_bundle


@dataclass(slots=True)
class TrainingResult:
    evaluated_sets: pd.DataFrame
    survey_sets: pd.DataFrame


def _xgboost_module():
    try:
        import xgboost as xgb
    except ImportError as exc:  # pragma: no cover - depends on runtime install
        raise RuntimeError("Install xgboost to train RStock models") from exc
    return xgb


def train_models(
    prepared: pd.DataFrame,
    generated_sets: pd.DataFrame,
    config: RStockConfig,
    *,
    models_directory: Path | None = None,
    clear_existing_models: bool = True,
) -> TrainingResult:
    """Train, evaluate and persist models using phase-1 legacy semantics."""

    xgb = _xgboost_module()
    directory = models_directory or config.models_path
    prepare_models_directory(directory, clear_existing=clear_existing_models)
    shuffled = prepared.sample(frac=1, random_state=config.shuffle_seed)
    evaluated = generated_sets.copy()
    evaluated["Err"] = 0.0
    evaluated["Set"] = ""

    train_size = floor(config.train_fraction * len(shuffled))
    if train_size < 1 or train_size >= len(shuffled):
        raise ValueError("Prepared data must produce non-empty train and test sets")

    for ordinal, (row_index, row) in enumerate(evaluated.iterrows()):
        observation, feature_symbols = symbols_from_set(row)
        outcome_name = f"{observation}.UPDW"
        if outcome_name not in shuffled:
            raise KeyError(f"Missing outcome column: {outcome_name}")
        names = predictor_columns(shuffled, feature_symbols, config.date_feature_regex)
        if not names:
            raise ValueError(f"No predictors available for set targeting {observation}")

        rng = np.random.RandomState(config.split_seed)
        train_positions = rng.choice(len(shuffled), size=train_size, replace=False)
        train_mask = np.zeros(len(shuffled), dtype=bool)
        train_mask[train_positions] = True
        train = shuffled.iloc[train_mask]
        test = shuffled.iloc[~train_mask]

        dtrain = xgb.DMatrix(train[names], label=train[outcome_name], feature_names=names)
        booster = xgb.train(
            {
                "objective": "binary:logistic",
                "max_depth": config.xgb_max_depth,
                "eta": config.xgb_eta,
                "nthread": config.xgb_nthread,
                "seed": config.split_seed,
            },
            dtrain,
            num_boost_round=config.xgb_rounds,
            verbose_eval=False,
        )
        probabilities = booster.predict(xgb.DMatrix(test[names], feature_names=names))
        predicted = binary_predictions(probabilities, config.prediction_threshold)
        error = calculate_error(
            predicted,
            test[names],
            test[outcome_name].to_numpy(),
            config.error_metric,
        )
        set_name = legacy_set_name(row)
        evaluated.at[row_index, "Err"] = error
        evaluated.at[row_index, "Set"] = set_name

        if error < config.keep_predictor_under:
            metadata = ModelMetadata(
                schema_version=1,
                set_name=set_name,
                observation=observation,
                features=feature_symbols,
                predictor_columns=names,
                error=error,
                error_metric=config.error_metric,
                model_file="",
            )
            save_model_bundle(booster, metadata, directory, f"model_{ordinal:06d}")

    survey = evaluated[evaluated["Err"] < config.keep_predictor_under].copy()
    return TrainingResult(evaluated_sets=evaluated, survey_sets=survey)

