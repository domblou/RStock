"""Application services connecting validated experiments to daily operation."""

from __future__ import annotations

import hashlib
import json
import uuid
from typing import Any, Callable

import numpy as np
import pandas as pd

from rstock.calendars import next_market_session
from rstock.config import RStockConfig
from rstock.features import (
    intraday_down_target_column,
    intraday_lag_column,
    intraday_return_column,
    intraday_target_column,
    predictor_columns,
    prepare_prediction_row,
)
from rstock.modeling import XGBoostParameters, fit_booster, predict_probabilities
from rstock.persistence import load_booster, model_store_transaction
from rstock.progress import CancellationCheck, check_cancellation
from rstock.qualification import qualification_parameters

from .domain import ExperimentSpec, JobStatus, JobType
from .production_domain import OperationalUniverse, ProductionModel, ProductionModelStatus
from .production_repository import ProductionRepository
from .repository import RunRepository, utc_now


SUPPORTED_FEATURE_VERSIONS = {"rstock_features_v1"}


def _json_safe(values: dict[str, Any]) -> dict[str, Any]:
    return {
        str(key): (None if pd.isna(value) else value)
        for key, value in values.items()
    }


def _is_true(value: Any) -> bool:
    return value is True or str(value).strip().lower() in {"true", "1", "yes"}


class PromotionService:
    def __init__(self, runs: RunRepository, production: ProductionRepository) -> None:
        self.runs = runs
        self.production = production

    def promote(
        self,
        walk_forward_run: str,
        set_name: str,
        *,
        xgboost_calibration_run: str | None = None,
        threshold_calibration_run: str | None = None,
    ) -> tuple[ProductionModel, bool]:
        self._require_completed_run(walk_forward_run, JobType.WALK_FORWARD)
        spec = self.runs.load_spec(walk_forward_run)
        results = self.runs.run_directory(walk_forward_run) / "results"
        qualification = pd.read_csv(results / "qualification.csv")
        matched = qualification[qualification["Set"].astype(str) == set_name]
        if matched.empty:
            raise ValueError(f"Combination is absent from run: {set_name}")
        selected = matched.iloc[0]
        if not _is_true(selected["Eligible"]):
            raise ValueError("Only a qualified combination can be promoted")
        predictors = tuple(json.loads(str(selected["Predictors"])))
        target = str(selected["Observation"])
        xgb_parameters: dict[str, int | float] = {
            "max_depth": spec.config.xgb_max_depth,
            "eta": spec.config.xgb_eta,
            "num_boost_round": spec.config.xgb_rounds,
            "min_child_weight": spec.config.xgb_min_child_weight,
            "subsample": spec.config.xgb_subsample,
            "colsample_bytree": spec.config.xgb_colsample_bytree,
            "gamma": spec.config.xgb_gamma,
            "reg_alpha": spec.config.xgb_reg_alpha,
            "reg_lambda": spec.config.xgb_reg_lambda,
        }
        xgboost_seed = spec.config.xgb_seed
        xgboost_threads = spec.config.xgb_nthread
        calibration_sources: dict[str, Any] = {}
        down_xgb_parameters: dict[str, int | float] | None = None
        if xgboost_calibration_run:
            self._require_completed_run(
                xgboost_calibration_run, JobType.XGBOOST_CALIBRATION
            )
            xgb_spec = self.runs.load_spec(xgboost_calibration_run)
            self._assert_methodology_compatible(spec, xgb_spec, "XGBoost calibration")
            xgboost_seed = xgb_spec.config.xgb_seed
            xgboost_threads = xgb_spec.config.xgb_nthread
            calibration_sources["xgboost"] = xgb_spec.to_dict()
            path = self.runs.run_directory(xgboost_calibration_run) / "results" / "selected_configurations.json"
            calibrated = json.loads(path.read_text(encoding="utf-8"))
            # Direction-specific parameters are preserved; production V1 uses
            # the Up selection as the common architecture when they differ.
            xgb_parameters = dict(calibrated["Up"]["parameters"])
            down_xgb_parameters = dict(calibrated["Down"]["parameters"])
        up_threshold = spec.config.prediction_threshold
        down_threshold = spec.config.prediction_threshold
        if threshold_calibration_run:
            self._require_completed_run(
                threshold_calibration_run, JobType.THRESHOLD_CALIBRATION
            )
            threshold_spec = self.runs.load_spec(threshold_calibration_run)
            self._assert_methodology_compatible(
                spec, threshold_spec, "threshold calibration"
            )
            calibration_sources["thresholds"] = threshold_spec.to_dict()
            path = self.runs.run_directory(threshold_calibration_run) / "results" / "selected_thresholds.json"
            calibrated = json.loads(path.read_text(encoding="utf-8"))
            if any(
                calibrated.get(direction, {}).get("status") not in {None, "selected"}
                or calibrated.get(direction, {}).get("threshold") is None
                for direction in ("Up", "Down")
            ):
                raise ValueError("Threshold calibration has no eligible Up/Down selection")
            up_threshold = float(calibrated["Up"]["threshold"])
            down_threshold = float(calibrated["Down"]["threshold"])
        final_path = results / "final_holdout.csv"
        holdout_metrics: dict[str, Any] = {}
        if final_path.exists():
            final = pd.read_csv(final_path)
            row = final[final["Set"].astype(str) == set_name]
            if not row.empty:
                holdout_metrics = _json_safe(row.iloc[0].to_dict())
        fingerprint_values = {
            "target": target,
            "predictors": predictors,
            "walk_forward": walk_forward_run,
            "xgboost": xgboost_calibration_run,
            "thresholds": threshold_calibration_run,
        }
        fingerprint = hashlib.sha256(
            json.dumps(fingerprint_values, sort_keys=True).encode()
        ).hexdigest()
        for existing in self.production.models():
            if existing.training_metadata.get("promotion_fingerprint") == fingerprint:
                return existing, False
        model = ProductionModel(
            model_id=f"model_{uuid.uuid4().hex[:12]}",
            target=target,
            predictors=predictors,
            lag_depth=spec.config.lag_depth,
            target_definition=(
                f"Up: intraday return >= {spec.config.intraday_target_threshold}; "
                f"Down: intraday return <= -{spec.config.intraday_down_threshold}"
            ),
            up_target_threshold=spec.config.intraday_target_threshold,
            down_target_threshold=spec.config.intraday_down_threshold,
            xgboost_parameters=xgb_parameters,
            up_threshold=up_threshold,
            down_threshold=down_threshold,
            qualification_rules=qualification_parameters(spec.config),
            source_walk_forward_run=walk_forward_run,
            source_xgboost_calibration_run=xgboost_calibration_run,
            source_threshold_calibration_run=threshold_calibration_run,
            development_metrics=_json_safe(selected.to_dict()),
            holdout_metrics=holdout_metrics,
            created_at=utc_now(),
            training_metadata={
                "promotion_fingerprint": fingerprint,
                "calendar": spec.calendar,
                "calibration_source_configurations": calibration_sources,
            },
            down_xgboost_parameters=down_xgb_parameters,
            xgboost_seed=xgboost_seed,
            xgboost_threads=xgboost_threads,
            source_configuration=spec.to_dict(),
        )
        return self.production.add(model), True

    def _require_completed_run(self, run_id: str, job_type: JobType) -> None:
        status = self.runs.status(run_id)
        if status.get("job_type") != job_type.value:
            raise ValueError(
                f"Run {run_id} is not a {job_type.value} run"
            )
        if status.get("status") != JobStatus.COMPLETED.value:
            raise ValueError(f"Run {run_id} is not completed")

    @staticmethod
    def _assert_methodology_compatible(
        walk_forward: ExperimentSpec, calibration: ExperimentSpec, label: str
    ) -> None:
        fields = (
            "lag_depth",
            "intraday_target_threshold",
            "intraday_down_threshold",
        )
        mismatched = [
            name
            for name in fields
            if getattr(walk_forward.config, name) != getattr(calibration.config, name)
        ]
        if mismatched:
            raise ValueError(
                f"{label} is methodologically incompatible: {', '.join(mismatched)}"
            )


class ProductionTrainingService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def train(
        self,
        model_id: str,
        prepared: pd.DataFrame,
        config: RStockConfig,
        *,
        cancellation_check: CancellationCheck | None = None,
    ) -> ProductionModel:
        model = self.repository.get(model_id)
        if model.status == ProductionModelStatus.RETIRED:
            raise ValueError("A retired model cannot be trained")
        if model.status == ProductionModelStatus.ACTIVE:
            raise ValueError("Deactivate an active model before retraining it")
        names = predictor_columns(prepared, model.predictors, model.lag_depth, config.date_feature_regex)
        outcomes = {
            "up": intraday_target_column(model.target),
            "down": intraday_down_target_column(model.target),
        }
        training = prepared[[*names, *outcomes.values()]].dropna()
        if training.empty:
            raise ValueError("No complete production training observations")
        directional_parameters = {
            "up": XGBoostParameters(**model.xgboost_parameters),
            "down": XGBoostParameters(**(
                model.down_xgboost_parameters or model.xgboost_parameters
            )),
        }
        destination = self.repository.artifact_directory(model_id)
        version = (model.artifact_version or 0) + 1
        frozen_configuration = model.to_dict()
        frozen_configuration.pop("training_metadata", None)
        frozen_configuration["artifact_version"] = version
        with model_store_transaction(destination) as staging:
            for direction, outcome in outcomes.items():
                check_cancellation(cancellation_check)
                booster = fit_booster(
                    training, names, outcome, config,
                    parameters=directional_parameters[direction],
                )
                booster.save_model(staging / f"{direction}.ubj")
            check_cancellation(cancellation_check)
            metadata = {
                "schema_version": 1,
                "model_id": model_id,
                "artifact_version": version,
                "feature_version": model.feature_version,
                "predictor_columns": names,
                "outcomes": outcomes,
                "train_start": training.index.min().isoformat(),
                "train_end": training.index.max().isoformat(),
                "observations": len(training),
                "trained_at": utc_now(),
                "configuration": frozen_configuration,
            }
            (staging / "production.metadata.json").write_text(
                json.dumps(metadata, indent=2, ensure_ascii=False, default=str) + "\n",
                encoding="utf-8",
            )
        model.status = ProductionModelStatus.TRAINED
        model.artifact_version = version
        model.training_metadata = {**model.training_metadata, **metadata}
        self.repository.update(model)
        return model


class ProductionLifecycleService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def activate(self, model_id: str) -> ProductionModel:
        model = self.repository.get(model_id)
        directory = self.repository.artifact_directory(model_id)
        required = [directory / "up.ubj", directory / "down.ubj", directory / "production.metadata.json"]
        if model.status not in {ProductionModelStatus.TRAINED, ProductionModelStatus.INACTIVE}:
            raise ValueError("Only a trained or inactive model can be activated")
        if model.feature_version not in SUPPORTED_FEATURE_VERSIONS:
            raise ValueError("The production feature version is not supported")
        # This validates every frozen modeling parameter before the status can
        # become active, independently for Up and Down.
        XGBoostParameters(**model.xgboost_parameters)
        XGBoostParameters(**(model.down_xgboost_parameters or model.xgboost_parameters))
        if model.artifact_version is None or not all(path.is_file() for path in required):
            raise ValueError("Production artifacts are missing or incomplete")
        metadata = json.loads(required[-1].read_text(encoding="utf-8"))
        predictor_names = metadata.get("predictor_columns") or []
        required_lags = {
            intraday_lag_column(symbol, lag)
            for symbol in model.predictors
            for lag in range(1, model.lag_depth + 1)
        }
        if (
            metadata.get("model_id") != model.model_id
            or metadata.get("artifact_version") != model.artifact_version
            or metadata.get("feature_version") != model.feature_version
            or not predictor_names
            or not required_lags.issubset(set(predictor_names))
            or any(
                name not in required_lags | {"wday", "yday", "mon"}
                for name in predictor_names
            )
        ):
            raise ValueError("Production artifact metadata is incompatible or incomplete")
        model.status = ProductionModelStatus.ACTIVE
        self.repository.update(model)
        return model

    def deactivate(self, model_id: str) -> ProductionModel:
        model = self.repository.get(model_id)
        if model.status != ProductionModelStatus.ACTIVE:
            raise ValueError("Only an active model can be deactivated")
        model.status = ProductionModelStatus.INACTIVE
        self.repository.update(model)
        return model

    def retire(self, model_id: str) -> ProductionModel:
        model = self.repository.get(model_id)
        if model.status == ProductionModelStatus.ACTIVE:
            raise ValueError("Deactivate an active model before retiring it")
        model.status = ProductionModelStatus.RETIRED
        self.repository.update(model)
        return model


class OperationalUniverseService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def current(self) -> OperationalUniverse:
        active = [model for model in self.repository.models() if model.status == ProductionModelStatus.ACTIVE]
        used_by: dict[str, list[str]] = {}
        for model in active:
            for symbol in model.symbols:
                used_by.setdefault(symbol, []).append(model.model_id)
        return OperationalUniverse(
            model_ids=tuple(model.model_id for model in active),
            symbols=tuple(sorted(used_by)),
            used_by={symbol: tuple(ids) for symbol, ids in sorted(used_by.items())},
        )


class DailyPredictionService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def generate(
        self,
        prepared: pd.DataFrame,
        config: RStockConfig,
        *,
        persist: bool = True,
    ) -> pd.DataFrame:
        rows: list[dict[str, Any]] = []
        for model in self.repository.models():
            if model.status != ProductionModelStatus.ACTIVE:
                continue
            try:
                rows.append(self._predict_model(model, prepared))
            except Exception as error:
                # One stale/corrupt model must be visible as an operational
                # error without suppressing predictions from other models.
                rows.append(self._error_row(model, f"{type(error).__name__}: {error}"))
        frame = pd.DataFrame(rows)
        if persist and not frame.empty:
            self.repository.append_table("predictions", frame, key="prediction_id")
        return frame

    def _predict_model(
        self, model: ProductionModel, prepared: pd.DataFrame
    ) -> dict[str, Any]:
        directory = self.repository.artifact_directory(model.model_id)
        metadata = json.loads(
            (directory / "production.metadata.json").read_text(encoding="utf-8")
        )
        if (
            metadata.get("model_id") != model.model_id
            or metadata.get("artifact_version") != model.artifact_version
            or metadata.get("feature_version") != model.feature_version
        ):
            raise ValueError("incompatible production metadata")
        return_name = intraday_return_column(model.target)
        if return_name not in prepared:
            raise ValueError("missing target history")
        observations = prepared[return_name].dropna()
        if observations.empty:
            raise ValueError("missing target history")
        as_of = observations.index.max()
        calendar = str(model.training_metadata.get("calendar", "XNYS"))
        target_date = next_market_session(as_of, calendar)
        current = prepare_prediction_row(
            prepared, as_of_date=as_of, target_date=target_date, lag_depth=model.lag_depth
        )
        names = list(metadata["predictor_columns"])
        if any(name not in current or current[name].isna().any() for name in names):
            raise ValueError("missing predictors")
        up = float(predict_probabilities(load_booster(directory / "up.ubj"), current, names)[0])
        down = float(predict_probabilities(load_booster(directory / "down.ubj"), current, names)[0])
        signal_status = (
            "bullish_signal"
            if up >= model.up_threshold and down < model.down_threshold
            else "no_signal"
        )
        prediction_id = hashlib.sha256(
            f"{model.model_id}:{model.artifact_version}:{pd.Timestamp(target_date).date()}".encode()
        ).hexdigest()[:20]
        return {
            "prediction_id": prediction_id,
            "prediction_date": pd.Timestamp(target_date).date().isoformat(),
            "as_of_date": pd.Timestamp(as_of).date().isoformat(),
            "target": model.target,
            "predictors": json.dumps(model.predictors),
            "model_id": model.model_id,
            "model_version": model.artifact_version,
            "up_probability": up,
            "down_probability": down,
            "up_threshold": model.up_threshold,
            "down_threshold": model.down_threshold,
            "signal_status": signal_status,
            "status": "predicted",
            "error": None,
            "created_at": utc_now(),
        }

    @staticmethod
    def _error_row(model: ProductionModel, error: str, date: Any = None) -> dict[str, Any]:
        token = f"{model.model_id}:{date}:{error}:{utc_now()}"
        return {
            "prediction_id": hashlib.sha256(token.encode()).hexdigest()[:20],
            "prediction_date": None if date is None else pd.Timestamp(date).date().isoformat(),
            "as_of_date": None, "target": model.target, "predictors": json.dumps(model.predictors),
            "model_id": model.model_id, "model_version": model.artifact_version,
            "up_probability": np.nan, "down_probability": np.nan,
            "up_threshold": model.up_threshold, "down_threshold": model.down_threshold,
            "signal_status": "error", "status": "error", "error": error, "created_at": utc_now(),
        }


class ProductionSignalService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def screen(
        self,
        predictions: pd.DataFrame | None = None,
        *,
        cancellation_check: CancellationCheck | None = None,
        persist: bool = True,
    ) -> pd.DataFrame:
        frame = self.repository.read_table("predictions") if predictions is None else predictions.copy()
        rows = []
        for item in frame.to_dict("records"):
            check_cancellation(cancellation_check)
            if item.get("status") != "predicted":
                category = "error"
            else:
                bullish = (
                    float(item["up_probability"]) >= float(item["up_threshold"])
                    and float(item["down_probability"]) < float(item["down_threshold"])
                )
                category = "bullish_signal" if bullish else "no_signal"
            rows.append({
                "signal_id": str(item["prediction_id"]), "prediction_id": str(item["prediction_id"]),
                "prediction_date": item.get("prediction_date"), "model_id": item["model_id"],
                "target": item["target"], "category": category,
                "predictors": item.get("predictors"),
                "model_version": item.get("model_version"),
                "up_probability": item.get("up_probability"), "down_probability": item.get("down_probability"),
                "up_threshold": item.get("up_threshold"), "down_threshold": item.get("down_threshold"),
                "created_at": utc_now(),
            })
        result = pd.DataFrame(rows)
        if persist and not result.empty:
            self.repository.append_table("signals", result, key="signal_id")
        return result


class RealizedResultService:
    def __init__(self, repository: ProductionRepository) -> None:
        self.repository = repository

    def update(
        self,
        price_loader: Callable[[str], pd.DataFrame | None],
        *,
        cancellation_check: CancellationCheck | None = None,
        additional_predictions: pd.DataFrame | None = None,
        persist: bool = True,
    ) -> pd.DataFrame:
        predictions = self.repository.read_table("predictions")
        if additional_predictions is not None and not additional_predictions.empty:
            predictions = pd.concat(
                [predictions, additional_predictions], ignore_index=True
            ).drop_duplicates("prediction_id", keep="first")
        previous = self.repository.read_table("realized_results")
        completed = set(previous.get("prediction_id", pd.Series(dtype=str)).astype(str))
        models = {model.model_id: model for model in self.repository.models()}
        rows = []
        for item in predictions.to_dict("records"):
            check_cancellation(cancellation_check)
            prediction_id = str(item["prediction_id"])
            if (
                prediction_id in completed
                or item.get("status") != "predicted"
            ):
                continue
            prices = price_loader(str(item["target"]))
            if prices is None or prices.empty:
                continue
            date = pd.Timestamp(item["prediction_date"]).normalize()
            normalised = prices.copy()
            normalised.index = pd.to_datetime(normalised.index).normalize()
            if date not in normalised.index:
                continue
            price = normalised.loc[date]
            opened, high, low, closed = (float(price[name]) for name in ("Open", "High", "Low", "Close"))
            intraday = closed / opened - 1.0
            model = models.get(str(item["model_id"]))
            if model is None:
                continue
            rows.append({
                "result_id": prediction_id, "prediction_id": prediction_id,
                "model_id": item["model_id"], "target": item["target"],
                "prediction_date": item["prediction_date"],
                "open": opened, "high": high, "low": low, "close": closed,
                "intraday_return": intraday,
                "mfe": high / opened - 1.0, "mae": low / opened - 1.0,
                "up_target": int(intraday >= model.up_target_threshold),
                "down_target": int(intraday <= -model.down_target_threshold),
                "recorded_at": utc_now(),
            })
        result = pd.DataFrame(rows)
        if persist and not result.empty:
            self.repository.append_table("realized_results", result, key="result_id")
        return result
