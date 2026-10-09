"""Shared XGBoost parameters, chronological round selection and inference."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import asdict, dataclass
from typing import Any
import hashlib
import json
from time import perf_counter

import numpy as np
import pandas as pd

from .config import RStockConfig


ROUND_SELECTION_FIELDS = (
    "xgb_round_selection_mode", "xgb_early_stopping_max_rounds",
    "xgb_early_stopping_validation_sessions", "xgb_early_stopping_patience",
    "xgb_early_stopping_min_train_observations", "xgb_early_stopping_metric",
    "xgb_round_selection_protocol_version",
)
PREFILTER_ROUND_SELECTION_FIELDS = tuple("prefilter_" + name for name in ROUND_SELECTION_FIELDS)


@dataclass(frozen=True, slots=True)
class RoundSelectionPolicy:
    """Resolve one policy at the training boundary; selection stays shared."""

    scope: str
    values: Mapping[str, object]
    fallback_rounds: int
    fallback_source: str

    @classmethod
    def from_config(cls, config: RStockConfig, *, scope: str = "xgboost") -> RoundSelectionPolicy:
        if scope not in {"xgboost", "prefilter"}:
            raise ValueError("Unsupported round selection scope")
        prefix = "prefilter_" if scope == "prefilter" else ""
        source = "prefilter_xgb_num_boost_round" if prefix else "xgb_rounds"
        return cls(scope, {name: getattr(config, prefix + name) for name in ROUND_SELECTION_FIELDS},
                   getattr(config, source), source)

    def training_config(self, config: RStockConfig) -> RStockConfig:
        from dataclasses import replace
        return replace(config, **self.values, xgb_rounds=self.fallback_rounds)

    def snapshot(self) -> dict[str, object]:
        prefix = "prefilter_" if self.scope == "prefilter" else ""
        return {**{prefix + name: value for name, value in self.values.items()},
                "fallback_rounds": self.fallback_rounds, "fallback_source": self.fallback_source,
                "minimum_usable_observations": self.values["xgb_early_stopping_validation_sessions"] + self.values["xgb_early_stopping_min_train_observations"],
                "probability_loss_epsilon": 1e-15}


def round_selection_snapshot(config: RStockConfig) -> dict[str, object]:
    return RoundSelectionPolicy.from_config(config).snapshot()


def fit_chronological_booster(
    train: pd.DataFrame, predictor_names: Sequence[str], outcome_name: str,
    config: RStockConfig, *, parameters: XGBoostParameters,
    cancellation_check: Any = None,
    policy: RoundSelectionPolicy | None = None,
) -> tuple[Any, dict[str, object]]:
    """Select rounds using only the supplied train, then fit a fresh booster.

    The caller supplies the admissible dates and the scope's resolved policy.
    """
    from dataclasses import replace
    from .progress import check_cancellation

    policy = policy or RoundSelectionPolicy.from_config(config)
    config = policy.training_config(config)
    selection_started = perf_counter()

    names = list(predictor_names)
    if not isinstance(train.index, pd.DatetimeIndex) or not train.index.is_monotonic_increasing or not train.index.is_unique or train.index.hasnans:
        raise ValueError("Chronological fitting requires ordered, unique dates")
    if not train.index.normalize().is_unique:
        raise ValueError("Chronological fitting requires distinct trading sessions")
    if train[[*names, outcome_name]].isna().any().any():
        raise ValueError("Chronological fitting requires complete usable observations")
    validation_size = config.xgb_early_stopping_validation_sessions
    internal = train.iloc[:-validation_size]
    validation = train.iloc[-validation_size:]
    diagnostics: dict[str, object] = {
        "RoundSelectionMode": "chronological", "RoundSelectionUsed": False,
        "EarlyStoppingTriggered": False, "RoundSelectionFallbackReason": None,
        "RoundsRetained": config.xgb_rounds, "SelectionRoundsRun": 0,
        "BestIteration": None, "BestValidationLogLoss": None,
        "SelectionCapReached": False,
        "InternalTrainObservations": len(internal),
        "ValidationObservations": len(validation),
    }
    for prefix, frame in (("InternalTrain", internal), ("Validation", validation)):
        diagnostics[f"{prefix}Start"] = None if frame.empty else frame.index.min()
        diagnostics[f"{prefix}End"] = None if frame.empty else frame.index.max()
        diagnostics[f"{prefix}PositiveOutcomes"] = int(frame[outcome_name].sum())
    reason = None
    if len(train) < validation_size + config.xgb_early_stopping_min_train_observations:
        reason = "insufficient_history"
    elif validation[outcome_name].nunique() != 2:
        reason = "validation_single_class"
    elif internal[outcome_name].nunique() != 2:
        reason = "internal_train_single_class"
    else:
        xgb = xgboost_module()

        class CancellationCallback(xgb.callback.TrainingCallback):
            def after_iteration(self, model, epoch, evals_log):
                check_cancellation(cancellation_check)
                return False

        check_cancellation(cancellation_check)
        history: dict[str, Any] = {}
        selector = None
        try:
            selector = xgb.train(
                {**parameters.training_parameters(config), "eval_metric": "logloss"},
                xgb.DMatrix(internal[names], label=internal[outcome_name], feature_names=names),
                num_boost_round=config.xgb_early_stopping_max_rounds,
                evals=[(xgb.DMatrix(validation[names], label=validation[outcome_name], feature_names=names), "validation")],
                early_stopping_rounds=config.xgb_early_stopping_patience,
                maximize=False, evals_result=history, verbose_eval=False,
                callbacks=[CancellationCallback()],
            )
            losses = np.asarray(history["validation"]["logloss"], dtype=float)
            diagnostics["SelectionRoundsRun"] = len(losses)
            best = int(selector.best_iteration)
            score = float(selector.best_score)
            if not len(losses) or len(losses) > config.xgb_early_stopping_max_rounds or not np.isfinite(losses).all() or not np.isfinite(score) or not 0 <= best < len(losses):
                reason = "invalid_selection_result"
            else:
                diagnostics.update(
                    RoundSelectionUsed=True, RoundsRetained=best + 1,
                    SelectionRoundsRun=len(losses), BestIteration=best,
                    BestValidationLogLoss=score,
                    EarlyStoppingTriggered=len(losses) < config.xgb_early_stopping_max_rounds,
                    SelectionCapReached=len(losses) == config.xgb_early_stopping_max_rounds,
                )
        except (AttributeError, KeyError, ValueError, TypeError):
            if selector is None:
                raise
            reason = "selection_result_unavailable"
        finally:
            del selector
    diagnostics["RoundSelectionFallbackReason"] = reason
    diagnostics["SelectionSeconds"] = perf_counter() - selection_started
    diagnostics.update(TrainingIdentity=training_identity(train, names, outcome_name, config, parameters, policy=policy),
        TrainingStart=train.index.min(), TrainingEnd=train.index.max(), TrainingObservations=len(train),
        Outcome=outcome_name, PredictorColumns=names, Parameters=parameters.as_dict(),
        RoundSelectionPolicy=policy.snapshot())
    check_cancellation(cancellation_check)
    selected = replace(parameters, num_boost_round=int(diagnostics["RoundsRetained"]))
    # Final fitting has no validation set and never continues the internal model.
    refit_started = perf_counter()
    booster = _fit_fixed_booster(train, names, outcome_name, config, parameters=selected,
                         cancellation_check=cancellation_check)
    check_cancellation(cancellation_check)
    diagnostics["RefitSeconds"] = perf_counter() - refit_started
    return booster, diagnostics


def round_selection_coverage(windows: pd.DataFrame) -> dict[str, object]:
    """Coverage is counted per candidate/window and separately per direction."""
    result = {}
    for direction in ("Up", "Down"):
        key = f"{direction}RoundSelectionUsed"
        if key not in windows:
            result[direction] = {"available": False}
            continue
        used = windows[key].fillna(False).astype(bool)
        chronological = windows[f"{direction}RoundSelectionMode"].eq("chronological")
        rounds = pd.to_numeric(windows[f"{direction}RoundsRetained"], errors="coerce")
        reasons = windows.loc[chronological & ~used, f"{direction}RoundSelectionFallbackReason"].value_counts()
        result[direction] = {
            "available": True, "windows": len(windows), "optimized_windows": int(used.sum()),
            "optimized_percent": float(100 * used.mean()) if len(used) else 0.0,
            "fallback_windows": int((chronological & ~used).sum()),
            "fallback_percent": float(100 * (chronological & ~used).mean()) if len(used) else 0.0,
            "fallback_reasons": {str(k): int(v) for k, v in reasons.items()},
            "rounds_histogram": {str(int(k)): int(v) for k, v in rounds.value_counts().items()},
            "optimized_rounds_histogram": {str(int(k)): int(v) for k, v in rounds[used].value_counts().items()},
            "rounds_distribution": {str(k): float(v) for k, v in rounds.describe().items()} if len(rounds) else {},
        }
    return result


def round_selection_coverage_batches(frames: Any) -> dict[str, object]:
    """Reduce coverage without materializing all candidate-window diagnostics."""
    totals: dict[str, dict[str, Any]] = {}
    for frame in frames:
        for direction, values in round_selection_coverage(frame).items():
            if not values["available"]:
                continue
            target = totals.setdefault(direction, {"available": True, "windows": 0,
                "optimized_windows": 0, "fallback_windows": 0,
                "fallback_reasons": {}, "rounds_histogram": {}, "optimized_rounds_histogram": {}})
            for name in ("windows", "optimized_windows", "fallback_windows"):
                target[name] += values[name]
            for name in ("fallback_reasons", "rounds_histogram", "optimized_rounds_histogram"):
                for key, count in values[name].items():
                    target[name][key] = target[name].get(key, 0) + count
    for direction in ("Up", "Down"):
        target = totals.setdefault(direction, {"available": False})
        if not target["available"]:
            continue
        n = target["windows"]
        target["optimized_percent"] = 100 * target["optimized_windows"] / n if n else 0.0
        target["fallback_percent"] = 100 * target["fallback_windows"] / n if n else 0.0
        histogram = sorted((int(k), v) for k, v in target["rounds_histogram"].items())
        count = sum(v for _, v in histogram)
        def quantile(q):
            # Linear interpolation matches pandas' default sample quantiles.
            position = q * (count - 1)
            lower, upper = int(np.floor(position)), int(np.ceil(position))
            found, cumulative = [], 0
            for value, frequency in histogram:
                if cumulative <= lower < cumulative + frequency:
                    found.append(value)
                if cumulative <= upper < cumulative + frequency:
                    found.append(value)
                cumulative += frequency
            return found[0] + (found[-1] - found[0]) * (position - lower)
        target["rounds_distribution"] = ({"count": count, "min": histogram[0][0],
            "25%": quantile(.25), "50%": quantile(.5), "75%": quantile(.75),
            "max": histogram[-1][0]} if count else {})
    return totals


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
    *, config: RStockConfig | None = None,
) -> dict[str, dict[str, int | float]]:
    """Extract and validate Up/Down parameters from a calibration artifact."""

    if config is not None and config.xgb_round_selection_mode == "chronological":
        expected = round_selection_snapshot(config)
        if any(selected[direction].get("round_selection_policy") != expected for direction in ("Up", "Down")):
            raise ValueError("Chronological calibration policy missing or incompatible")
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
        values = selected_xgboost_parameters(referenced, config=config)
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


def prefilter_xgboost_parameters(config: RStockConfig) -> XGBoostParameters:
    """Read only the dedicated predictor-prefilter hyperparameters."""
    return XGBoostParameters(
        max_depth=config.prefilter_xgb_max_depth,
        eta=config.prefilter_xgb_eta,
        num_boost_round=config.prefilter_xgb_num_boost_round,
        min_child_weight=config.prefilter_xgb_min_child_weight,
        subsample=config.prefilter_xgb_subsample,
        colsample_bytree=config.prefilter_xgb_colsample_bytree,
        gamma=config.prefilter_xgb_gamma,
        reg_alpha=config.prefilter_xgb_reg_alpha,
        reg_lambda=config.prefilter_xgb_reg_lambda,
    )


def prefilter_xgboost_snapshot(config: RStockConfig) -> dict[str, int | float]:
    """Training provenance, including the dedicated seed and shared thread count."""
    return {
        **prefilter_xgboost_parameters(config).as_dict(),
        "seed": config.prefilter_xgb_seed,
        "nthread": config.xgb_nthread,
    }


def xgboost_module() -> Any:
    try:
        import xgboost as xgb
    except ImportError as exc:  # pragma: no cover - depends on runtime install
        raise RuntimeError("Install xgboost to train RStock models") from exc
    return xgb


def training_identity(train: pd.DataFrame, names: Sequence[str], outcome: str,
                      config: RStockConfig, parameters: XGBoostParameters, *,
                      policy: RoundSelectionPolicy | None = None) -> str:
    """Exact training identity; a different origin always requires reselection."""
    data = train[[*names, outcome]]
    digest = hashlib.sha256(pd.util.hash_pandas_object(data, index=True).values.tobytes())
    digest.update(json.dumps({"features": list(names), "outcome": outcome,
        "parameters": parameters.training_parameters(config),
        "policy": policy.snapshot() if policy else round_selection_snapshot(config)}, sort_keys=True).encode())
    return digest.hexdigest()


def booster_training_record(booster: Any) -> dict[str, object]:
    value = booster.attr("rstock_round_selection") if hasattr(booster, "attr") else None
    return json.loads(value) if value else {}


def append_training_record(records: list[dict[str, object]], start: int, booster: Any) -> None:
    """Store one audit record per origin, without duplicating it per prediction."""
    record = booster_training_record(booster)
    if record and len(records) > start:
        records[start]["RoundSelectionRecord"] = json.dumps(record, default=str, sort_keys=True)


def probability_training_records(predictions: pd.DataFrame) -> pd.DataFrame:
    rows = []
    if "RoundSelectionRecord" in predictions:
        for _, row in predictions.dropna(subset=["RoundSelectionRecord"]).iterrows():
            rows.append({**{key: row[key] for key in ("Set", "Direction", "Window") if key in row},
                         "PredictionOrigin": row.get("Date"),
                         **json.loads(row["RoundSelectionRecord"])})
    return pd.DataFrame(rows)


def training_records_coverage(records: pd.DataFrame) -> dict[str, object]:
    if records.empty:
        return {}
    result = {}
    for direction, group in records.groupby("Direction"):
        prefixed = group.rename(columns={key: f"{direction}{key}" for key in (
            "RoundSelectionMode", "RoundSelectionUsed", "RoundsRetained", "RoundSelectionFallbackReason")})
        result[direction] = round_selection_coverage(prefixed)[direction]
    return result


def write_probability_training_audit(predictions: pd.DataFrame, directory: Any,
                                     configuration: dict[str, object]) -> None:
    records = probability_training_records(predictions)
    if records.empty:
        return
    records.to_csv(directory / "round_selection_training.csv", index=False)
    configuration["round_selection_coverage"] = training_records_coverage(records)
    configuration["round_selection_policy"] = records.iloc[0]["RoundSelectionPolicy"]


def production_training_config(config: RStockConfig, model: Any) -> RStockConfig:
    """A persisted model owns its future training policy, including the fallback."""
    from dataclasses import replace
    policy = model.round_selection_policy
    if policy is None:
        source = model.source_configuration.get("rstock_config", {})
        if source.get("xgb_round_selection_mode") == "chronological":
            raise ValueError("Chronological production model is missing its frozen policy")
        changes = {"xgb_round_selection_mode": "fixed"}
    else:
        if any(key not in policy for key in (*ROUND_SELECTION_FIELDS, "fallback_rounds")):
            raise ValueError("Incomplete frozen round selection policy")
        changes = {key: policy[key] for key in ROUND_SELECTION_FIELDS}
        changes["xgb_rounds"] = policy["fallback_rounds"]
    return replace(config, **changes, xgb_seed=model.xgboost_seed, xgb_nthread=model.xgboost_threads)


def validate_production_round_contract(model: Any, metadata: Mapping[str, Any]) -> None:
    policy = model.round_selection_policy
    if policy is None:
        if model.source_configuration.get("rstock_config", {}).get("xgb_round_selection_mode") == "chronological":
            raise ValueError("Missing chronological production policy")
        return
    if policy.get("xgb_round_selection_mode") != "chronological":
        return
    records = metadata.get("round_selection_records", {})
    if metadata.get("round_selection_policy") != policy or any(
        not records.get(direction, {}).get("TrainingIdentity")
        or records[direction].get("RoundSelectionPolicy") != policy
        for direction in ("up", "down")
    ):
        raise ValueError("Incomplete or incompatible chronological production artifact")


def fit_booster(
    train: pd.DataFrame, predictor_names: Sequence[str], outcome_name: str,
    config: RStockConfig, *, parameters: XGBoostParameters | None = None,
    cancellation_check: Any = None,
) -> Any:
    """Apply the configured policy to every new training origin, then refit."""
    selected = parameters or historical_xgboost_parameters(config)
    if config.xgb_round_selection_mode == "chronological":
        booster, record = fit_chronological_booster(
            train, predictor_names, outcome_name, config, parameters=selected,
            cancellation_check=cancellation_check)
        if not hasattr(booster, "set_attr"):
            raise ValueError("Chronological booster cannot persist its training contract")
        booster.set_attr(rstock_round_selection=json.dumps(record, default=str, sort_keys=True))
        return booster
    return _fit_fixed_booster(train, predictor_names, outcome_name, config,
        parameters=selected, cancellation_check=cancellation_check)


def _fit_fixed_booster(
    train: pd.DataFrame,
    predictor_names: Sequence[str],
    outcome_name: str,
    config: RStockConfig,
    *,
    parameters: XGBoostParameters | None = None,
    cancellation_check: Any = None,
) -> Any:
    """Fit one booster, defaulting to the established historical parameters."""

    xgb = xgboost_module()
    selected = parameters or historical_xgboost_parameters(config)
    names = list(predictor_names)
    matrix = xgb.DMatrix(train[names], label=train[outcome_name], feature_names=names)
    extra = {}
    if cancellation_check is not None:
        from .progress import check_cancellation
        class CancellationCallback(xgb.callback.TrainingCallback):
            def after_iteration(self, model, epoch, evals_log):
                check_cancellation(cancellation_check)
                return False
        extra["callbacks"] = [CancellationCallback()]
    return xgb.train(
        selected.training_parameters(config),
        matrix,
        num_boost_round=selected.num_boost_round,
        verbose_eval=False,
        **extra,
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
