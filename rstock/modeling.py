"""Shared XGBoost fitting and inference with the project's fixed parameters."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import asdict, dataclass
from typing import Any

import numpy as np
import pandas as pd

from .config import RStockConfig


@dataclass(frozen=True, slots=True)
class XGBoostParameters:
    """Validated XGBoost parameters that may be calibrated independently."""

    max_depth: int
    eta: float
    num_boost_round: int
    min_child_weight: float = 1.0
    subsample: float = 1.0
    colsample_bytree: float = 1.0
    gamma: float = 0.0
    reg_alpha: float = 0.0
    reg_lambda: float = 1.0

    def __post_init__(self) -> None:
        if self.max_depth < 1:
            raise ValueError("max_depth must be positive")
        if self.eta <= 0:
            raise ValueError("eta must be positive")
        if self.num_boost_round < 1:
            raise ValueError("num_boost_round must be positive")
        if self.min_child_weight < 0:
            raise ValueError("min_child_weight cannot be negative")
        for name in ("subsample", "colsample_bytree"):
            value = getattr(self, name)
            if not 0 < value <= 1:
                raise ValueError(f"{name} must be in (0, 1]")
        for name in ("gamma", "reg_alpha", "reg_lambda"):
            if getattr(self, name) < 0:
                raise ValueError(f"{name} cannot be negative")

    def as_dict(self) -> dict[str, int | float]:
        return asdict(self)

    def training_parameters(self, config: RStockConfig) -> dict[str, object]:
        values = self.as_dict()
        values.pop("num_boost_round")
        return {
            "objective": "binary:logistic",
            **values,
            "nthread": config.xgb_nthread,
            "seed": config.xgb_seed,
        }


@dataclass(frozen=True, slots=True)
class DirectionalXGBoostParameters:
    up: XGBoostParameters
    down: XGBoostParameters
    source: str

    def as_dict(self) -> dict[str, dict[str, int | float]]:
        return {"Up": self.up.as_dict(), "Down": self.down.as_dict()}


def selected_xgboost_parameters(
    selected: Mapping[str, Any],
) -> dict[str, dict[str, int | float]]:
    """Extract and validate Up/Down parameters from a calibration artifact."""

    return {
        direction: XGBoostParameters(
            **dict(selected[direction]["parameters"])
        ).as_dict()
        for direction in ("Up", "Down")
    }


def resolve_directional_xgboost_parameters(
    config: RStockConfig,
    *,
    frozen: Mapping[str, Mapping[str, int | float]] | None = None,
    referenced: Mapping[str, Any] | None = None,
    legacy_fallback: XGBoostParameters | None = None,
) -> DirectionalXGBoostParameters:
    """Resolve effective parameters with one explicit, shared priority rule."""

    if frozen is not None:
        values = {direction: dict(frozen[direction]) for direction in ("Up", "Down")}
        source = "frozen_snapshot"
    elif referenced is not None:
        values = selected_xgboost_parameters(referenced)
        source = "referenced_calibration"
    elif legacy_fallback is not None:
        values = {direction: legacy_fallback.as_dict() for direction in ("Up", "Down")}
        source = "legacy_fallback"
    else:
        baseline = historical_xgboost_parameters(config).as_dict()
        values = {direction: baseline for direction in ("Up", "Down")}
        source = "rstock_config"
    return DirectionalXGBoostParameters(
        up=XGBoostParameters(**values["Up"]),
        down=XGBoostParameters(**values["Down"]),
        source=source,
    )


def historical_xgboost_parameters(config: RStockConfig) -> XGBoostParameters:
    """Return the complete effective parameter set used by the legacy baseline."""

    return XGBoostParameters(
        max_depth=config.xgb_max_depth,
        eta=config.xgb_eta,
        num_boost_round=config.xgb_rounds,
        min_child_weight=config.xgb_min_child_weight,
        subsample=config.xgb_subsample,
        colsample_bytree=config.xgb_colsample_bytree,
        gamma=config.xgb_gamma,
        reg_alpha=config.xgb_reg_alpha,
        reg_lambda=config.xgb_reg_lambda,
    )


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
    *,
    parameters: XGBoostParameters | None = None,
) -> Any:
    """Fit one booster, defaulting to the established historical parameters."""

    xgb = xgboost_module()
    selected = parameters or historical_xgboost_parameters(config)
    names = list(predictor_names)
    matrix = xgb.DMatrix(train[names], label=train[outcome_name], feature_names=names)
    return xgb.train(
        selected.training_parameters(config),
        matrix,
        num_boost_round=selected.num_boost_round,
        verbose_eval=False,
    )


def fit_booster_matrix(matrix: Any, config: RStockConfig, *, parameters: XGBoostParameters) -> Any:
    """Train from an already-prepared DMatrix without altering parameters."""
    return xgboost_module().train(
        parameters.training_parameters(config), matrix,
        num_boost_round=parameters.num_boost_round, verbose_eval=False,
    )


def predict_probabilities_matrix(booster: Any, matrix: Any) -> np.ndarray:
    return np.asarray(booster.predict(matrix), dtype=float)


def predict_probabilities(
    booster: Any,
    frame: pd.DataFrame,
    predictor_names: Sequence[str],
) -> np.ndarray:
    xgb = xgboost_module()
    names = list(predictor_names)
    matrix = xgb.DMatrix(frame[names], feature_names=names)
    return np.asarray(booster.predict(matrix), dtype=float)
