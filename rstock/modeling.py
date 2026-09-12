"""Shared XGBoost fitting and inference with the project's fixed parameters."""

from __future__ import annotations

from collections.abc import Sequence
from typing import Any

import numpy as np
import pandas as pd

from .config import RStockConfig


def xgboost_module() -> Any:
    try:
        import xgboost as xgb
    except ImportError as exc:  # pragma: no cover - depends on runtime install
        raise RuntimeError("Install xgboost to train RStock models") from exc
    return xgb


def fit_booster(
    train: pd.DataFrame,
    predictor_names: Sequence[str],
    outcome_name: str,
    config: RStockConfig,
) -> Any:
    """Fit one booster without changing the established XGBoost parameters."""

    xgb = xgboost_module()
    names = list(predictor_names)
    matrix = xgb.DMatrix(train[names], label=train[outcome_name], feature_names=names)
    return xgb.train(
        {
            "objective": "binary:logistic",
            "max_depth": config.xgb_max_depth,
            "eta": config.xgb_eta,
            "nthread": config.xgb_nthread,
            "seed": config.xgb_seed,
        },
        matrix,
        num_boost_round=config.xgb_rounds,
        verbose_eval=False,
    )


def predict_probabilities(
    booster: Any,
    frame: pd.DataFrame,
    predictor_names: Sequence[str],
) -> np.ndarray:
    xgb = xgboost_module()
    names = list(predictor_names)
    matrix = xgb.DMatrix(frame[names], feature_names=names)
    return np.asarray(booster.predict(matrix), dtype=float)
