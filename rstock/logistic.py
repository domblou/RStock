"""Train-only standardized L2 logistic model and explicit versioned artifacts."""
from dataclasses import dataclass
import hashlib
import json
from pathlib import Path
from time import perf_counter

import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.special import expit

from .progress import check_cancellation

VERSION = "logistic_l2_v1"


def logistic_contract(config):
    return {"version": VERSION, "penalty": config.logistic_l2_penalty,
            "max_iterations": config.logistic_max_iterations,
            "gradient_tolerance": config.logistic_gradient_tolerance,
            "objective_tolerance": config.logistic_objective_tolerance,
            "scaling": "train_mean_std_ddof0", "intercept_penalized": False}


@dataclass
class LogisticModel:
    names: list[str]
    mean: np.ndarray
    scale: np.ndarray
    coefficients: np.ndarray
    record: dict

    def predict(self, frame, names):
        if list(names) != self.names:
            raise ValueError("Logistic predictor order differs from the fitted model")
        x = frame[self.names].to_numpy(dtype=float)
        if not np.isfinite(x).all():
            raise ValueError("Logistic prediction inputs must be finite")
        return expit(self.coefficients[0] + ((x-self.mean)/self.scale) @ self.coefficients[1:])

    def attr(self, name):
        return json.dumps(self.record, sort_keys=True) if name == "rstock_round_selection" else None

    def save_model(self, path):
        path = Path(path)
        value = {"version": VERSION, "features": self.names, "mean": self.mean.tolist(),
                 "scale": self.scale.tolist(), "coefficients": self.coefficients.tolist(), "record": self.record}
        temporary = path.with_suffix(path.suffix + ".tmp")
        temporary.write_text(json.dumps(value, allow_nan=False, sort_keys=True), encoding="utf8")
        temporary.replace(path)

    @classmethod
    def load(cls, path):
        value = json.loads(Path(path).read_text(encoding="utf8"))
        if value.get("version") != VERSION:
            raise ValueError("Unsupported logistic artifact version")
        names = value["features"]
        mean, scale, coefficients = [np.asarray(value[key], dtype=float) for key in ("mean", "scale", "coefficients")]
        if (not names or len(set(names)) != len(names) or not all(isinstance(n, str) for n in names)
                or mean.shape != (len(names),) or scale.shape != mean.shape
                or coefficients.shape != (len(names)+1,) or not np.isfinite(mean).all()
                or not np.isfinite(scale).all() or not np.isfinite(coefficients).all() or (scale <= 0).any()):
            raise ValueError("Invalid logistic artifact")
        if (not value["record"].get("Converged") or value["record"].get("Algorithm") != "logistic_l2"
                or value["record"].get("Contract", {}).get("version") != VERSION):
            raise ValueError("Unconverged logistic artifact")
        return cls(names, mean, scale, coefficients, value["record"])


def fit_logistic(train, names, outcome, config, cancellation_check=None):
    names = list(names)
    if not names or len(set(names)) != len(names) or train.empty:
        raise ValueError("Logistic training requires observations and distinct predictors")
    if (not isinstance(train.index, pd.DatetimeIndex) or train.index.hasnans
            or not train.index.is_unique or not train.index.is_monotonic_increasing):
        raise ValueError("Logistic training requires ordered unique dates")
    x = train[names].to_numpy(dtype=float)
    y = train[outcome].to_numpy(dtype=float)
    if not np.isfinite(x).all() or not np.isin(y, [0, 1]).all() or len(np.unique(y)) != 2:
        raise ValueError("Logistic training requires finite predictors and both binary classes")
    mean, scale = x.mean(axis=0), x.std(axis=0, ddof=0)
    scale[scale == 0] = 1.
    z = (x-mean)/scale
    prevalence = y.mean()
    initial = np.r_[np.log(prevalence/(1-prevalence)), np.zeros(len(names))]
    def objective(theta):
        check_cancellation(cancellation_check)
        logits = theta[0] + z @ theta[1:]
        residual = expit(logits)-y
        loss = np.logaddexp(0., logits).mean() - (y*logits).mean() + config.logistic_l2_penalty * (theta[1:] @ theta[1:])
        gradient = np.r_[residual.mean(), z.T @ residual/len(y) + 2*config.logistic_l2_penalty*theta[1:]]
        return loss, gradient
    started = perf_counter()
    check_cancellation(cancellation_check)
    result = minimize(objective, initial, jac=True, method="L-BFGS-B",
                      options={"maxiter": config.logistic_max_iterations,
                               "gtol": config.logistic_gradient_tolerance,
                               "ftol": config.logistic_objective_tolerance})
    check_cancellation(cancellation_check)
    if not result.success or not np.isfinite(result.x).all():
        raise ValueError(f"Logistic optimization did not converge: {result.message}")
    identity = hashlib.sha256(pd.util.hash_pandas_object(train[[*names, outcome]], index=True).values.tobytes()
                              + json.dumps({"features": names, "outcome": outcome, "contract": logistic_contract(config)}, sort_keys=True).encode()).hexdigest()
    record = {"Algorithm": "logistic_l2", "Contract": logistic_contract(config), "Converged": True,
              "Iterations": int(result.nit), "OptimizerMessage": str(result.message),
              "TrainingIdentity": identity, "TrainingStart": str(train.index.min()),
              "TrainingEnd": str(train.index.max()), "TrainingObservations": len(train),
              "GradientMax": float(np.max(np.abs(result.jac))), "FitSeconds": perf_counter()-started,
              "RoundSelectionMode": "not_applicable", "RoundSelectionUsed": False, "RoundsRetained": None}
    return LogisticModel(names, mean, scale, result.x, record)
