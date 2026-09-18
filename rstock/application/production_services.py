"""Application services connecting validated experiments to daily operation."""

from __future__ import annotations

import hashlib
import json
import uuid
from dataclasses import replace
from typing import Any, Callable, Sequence

import numpy as np
import pandas as pd

from rstock.calendars import next_market_session
from rstock.config import RStockConfig
from rstock.data import prefix_symbol_columns
from rstock.features import (
    intraday_down_target_column,
    intraday_lag_column,
    intraday_return_column,
    intraday_target_column,
    predictor_columns,
    prediction_source_observations,
    prepare_dataset,
    prepare_prediction_row,
)
from rstock.modeling import (
    XGBoostParameters,
    fit_booster,
    predict_probabilities,
    resolve_directional_xgboost_parameters,
)
from rstock.persistence import load_booster, model_store_transaction
from rstock.progress import CancellationCheck, check_cancellation
from rstock.qualification import qualification_parameters
from rstock.model_selection import SCORE_COLUMNS

from .domain import ExperimentSpec, JobStatus, JobType
from .production_domain import OperationalUniverse, ProductionModel, ProductionModelStatus
from .production_repository import ProductionRepository
from .repository import RunRepository, utc_now


SUPPORTED_FEATURE_VERSIONS = {"rstock_features_v1"}


def _json_safe(values: dict[str, Any]) -> dict[str, Any]:
    normalized: dict[str, Any] = {}
    for key, value in values.items():
        if pd.isna(value):
            normalized[str(key)] = None
        elif isinstance(value, np.generic):
            normalized[str(key)] = value.item()
        else:
            normalized[str(key)] = value
    return normalized


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
        selected_threshold_direction: str | None = None,
    ) -> tuple[ProductionModel, bool]:
        if selected_threshold_direction not in {None, "Up", "Down"}:
            raise ValueError("selected_threshold_direction must be Up or Down")
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
        development_metrics = _json_safe(selected.to_dict())
        selection_path = results / "selection_results.csv"
        if selection_path.exists():
            selection_results = pd.read_csv(selection_path)
            scored = selection_results[
                selection_results["Set"].astype(str) == set_name
            ]
            if not scored.empty:
                score_values = scored.iloc[0]
                development_metrics.update(_json_safe({
                    name: score_values.get(name)
                    for name in SCORE_COLUMNS
                    if name in score_values.index
                }))
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
        threshold_spec: ExperimentSpec | None = None
        if threshold_calibration_run:
            self._require_completed_run(
                threshold_calibration_run, JobType.THRESHOLD_CALIBRATION
            )
            threshold_spec = self.runs.load_spec(threshold_calibration_run)
            self._assert_methodology_compatible(
                spec, threshold_spec, "threshold calibration"
            )
        provenance_spec = (
            threshold_spec
            if threshold_spec is not None
            and (
                threshold_spec.source_xgboost_calibration_run is not None
                or threshold_spec.frozen_xgboost_parameters is not None
            )
            else spec
        )
        inherited_xgboost_run = provenance_spec.source_xgboost_calibration_run
        frozen_xgboost = provenance_spec.frozen_xgboost_parameters
        resolved_xgboost_run = xgboost_calibration_run or inherited_xgboost_run
        if resolved_xgboost_run:
            self._require_completed_run(
                resolved_xgboost_run, JobType.XGBOOST_CALIBRATION
            )
            xgb_spec = self.runs.load_spec(resolved_xgboost_run)
            self._assert_methodology_compatible(spec, xgb_spec, "XGBoost calibration")
            xgboost_seed = xgb_spec.config.xgb_seed
            xgboost_threads = xgb_spec.config.xgb_nthread
            calibration_sources["xgboost"] = xgb_spec.to_dict()
            referenced_xgboost = None
            if xgboost_calibration_run or frozen_xgboost is None:
                path = self.runs.run_directory(resolved_xgboost_run) / "results" / "selected_configurations.json"
                referenced_xgboost = json.loads(path.read_text(encoding="utf-8"))
            directional_xgboost = resolve_directional_xgboost_parameters(
                xgb_spec.config,
                frozen=(
                    frozen_xgboost
                    if not xgboost_calibration_run
                    else None
                ),
                referenced=referenced_xgboost,
            )
            xgb_parameters = directional_xgboost.up.as_dict()
            down_xgb_parameters = directional_xgboost.down.as_dict()
        up_threshold = spec.config.prediction_threshold
        down_threshold = spec.config.prediction_threshold
        calibrated_signal_threshold = None
        calibration_metrics: dict[str, Any] = {}
        calibration_sample_size = None
        holdout_signal_metrics: dict[str, Any] = {}
        if threshold_calibration_run:
            assert threshold_spec is not None
            calibration_sources["thresholds"] = threshold_spec.to_dict()
            calibration_results = self.runs.run_directory(threshold_calibration_run) / "results"
            path = calibration_results / "selected_thresholds_by_set.json"
            if path.exists():
                by_set = json.loads(path.read_text(encoding="utf-8"))
                calibrated = by_set.get(set_name)
                if calibrated is None:
                    raise ValueError("Threshold calibration did not evaluate this combination")
            else:
                calibrated = json.loads(
                    (calibration_results / "selected_thresholds.json").read_text(
                        encoding="utf-8"
                    )
                )
            if any(
                calibrated.get(direction, {}).get("status") not in {None, "selected"}
                or calibrated.get(direction, {}).get("threshold") is None
                for direction in ("Up", "Down")
            ):
                raise ValueError("Threshold calibration has no eligible Up/Down selection")
            up_threshold = float(calibrated["Up"]["threshold"])
            down_threshold = float(calibrated["Down"]["threshold"])
            calibrated_signal_threshold = up_threshold
            calibration_metrics = _json_safe(
                dict(calibrated["Up"].get("calibration_metrics", {}))
            )
            sample_size = calibrated["Up"].get("calibration_sample_size")
            calibration_sample_size = None if sample_size is None else int(sample_size)
            holdout_path = calibration_results / "holdout_metrics.csv"
            if holdout_path.exists():
                signal_holdout = pd.read_csv(holdout_path)
                matched_holdout = signal_holdout[
                    (signal_holdout.get("Set", pd.Series(index=signal_holdout.index))
                     .astype(str) == set_name)
                    & (signal_holdout.get("Direction", pd.Series(index=signal_holdout.index))
                       .astype(str) == "Up")
                ]
                if not matched_holdout.empty:
                    holdout_signal_metrics = _json_safe(
                        matched_holdout.iloc[0].to_dict()
                    )
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
            "xgboost": resolved_xgboost_run,
            "thresholds": threshold_calibration_run,
        }
        fingerprint = hashlib.sha256(
            json.dumps(fingerprint_values, sort_keys=True).encode()
        ).hexdigest()
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
            source_xgboost_calibration_run=resolved_xgboost_run,
            source_threshold_calibration_run=threshold_calibration_run,
            development_metrics=development_metrics,
            holdout_metrics=holdout_metrics,
            created_at=utc_now(),
            training_metadata={
                "promotion_fingerprint": fingerprint,
                "selected_threshold_direction": selected_threshold_direction,
                "calendar": spec.calendar,
                "calibration_source_configurations": calibration_sources,
                "universe_roles": {
                    "primary_universe_id": spec.primary_universe_id,
                    "context_universe_ids": list(spec.context_universe_ids),
                    "target_symbols": list(spec.target_symbols),
                    "context_symbols": list(spec.context_symbols),
                    "predictor_symbols": list(spec.predictor_symbols),
                },
            },
            down_xgboost_parameters=down_xgb_parameters,
            xgboost_seed=xgboost_seed,
            xgboost_threads=xgboost_threads,
            source_configuration=spec.to_dict(),
            calibrated_signal_threshold=calibrated_signal_threshold,
            calibration_source_run=threshold_calibration_run,
            calibration_metrics=calibration_metrics,
            calibration_sample_size=calibration_sample_size,
            holdout_signal_metrics=holdout_signal_metrics,
        )
        return self.production.add_promoted_idempotently(model, fingerprint)

    def resolve_walk_forward_source(
        self, threshold_calibration_run: str, set_name: str
    ) -> str | None:
        """Find an unambiguous legacy source without relaxing qualification.

        New duplicated calibrations persist ``source_walk_forward_run``.  Older
        runs did not; they can be linked only when a completed walk-forward has
        the same frozen population *and* qualified the selected exact set.
        """

        calibration = self.runs.load_spec(threshold_calibration_run)
        calibration_status = self.runs.status(threshold_calibration_run)
        calibration_created_at = str(calibration_status.get("created_at", ""))
        explicit = calibration.source_walk_forward_run
        if explicit:
            return explicit
        frozen_fields = (
            "target_symbols", "context_symbols", "predictor_symbols", "calendar",
        )
        for status in self.runs.list_runs():
            candidate_id = str(status.get("run_id", ""))
            if (
                str(status.get("created_at", "")) > calibration_created_at
                or status.get("job_type") != JobType.WALK_FORWARD.value
                or status.get("status") != JobStatus.COMPLETED.value
            ):
                continue
            candidate = self.runs.load_spec(candidate_id)
            if any(
                getattr(candidate, name) != getattr(calibration, name)
                for name in frozen_fields
            ):
                continue
            qualification_path = self.runs.run_directory(candidate_id) / "results" / "qualification.csv"
            if not qualification_path.exists():
                continue
            qualification = pd.read_csv(qualification_path)
            matched = qualification[qualification.get("Set", pd.Series(dtype=str)).astype(str) == set_name]
            if not matched.empty and _is_true(matched.iloc[0].get("Eligible")):
                return candidate_id
        return None

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
        active = self.repository.active_models()
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
        market_data: pd.DataFrame | None = None,
        persist: bool = True,
    ) -> pd.DataFrame:
        rows: list[dict[str, Any]] = []
        for model in self.repository.active_models():
            try:
                rows.append(self._predict_model(model, prepared, market_data))
            except Exception as error:
                # One stale/corrupt model must be visible as an operational
                # error without suppressing predictions from other models.
                rows.append(self._error_row(model, f"{type(error).__name__}: {error}"))
        frame = pd.DataFrame(rows)
        if persist and not frame.empty:
            self.repository.append_table("predictions", frame, key="prediction_id")
        return frame

    def backfill(
        self,
        prepared: pd.DataFrame,
        *,
        market_data: pd.DataFrame | None = None,
        max_days: int = 30,
        persist: bool = True,
    ) -> pd.DataFrame:
        """Create missing operational predictions for recent observed sessions.

        This deliberately uses the production artifacts and active models at
        execution time. It is not the historical simulation/replay mechanism.
        """
        if max_days < 1:
            raise ValueError("max_days must be positive")
        if prepared.empty:
            return pd.DataFrame()
        dates = pd.DatetimeIndex(prepared.index).normalize()
        latest = dates.max()
        target_dates = dates[dates >= latest - pd.Timedelta(days=max_days - 1)]
        existing = set(
            self.repository.read_table("predictions")
            .get("prediction_id", pd.Series(dtype=str))
            .astype(str)
        )
        rows: list[dict[str, Any]] = []
        for model in self.repository.active_models():
            for target_date in target_dates:
                prediction_id = self._prediction_id(model, target_date)
                if prediction_id in existing:
                    continue
                try:
                    rows.append(
                        self._predict_model_for_target(
                            model,
                            prepared,
                            target_date=target_date,
                            market_data=market_data,
                        )
                    )
                except Exception:
                    # A backfill is retryable. Do not persist a synthetic error
                    # row that would prevent a later successful reconstruction.
                    continue
        frame = pd.DataFrame(rows)
        if persist and not frame.empty:
            self.repository.append_table("predictions", frame, key="prediction_id")
        return frame
    def replay(
        self,
        price_loader: Callable[[str], pd.DataFrame | None],
        config: RStockConfig,
        *,
        start_date: Any,
        end_date: Any,
        models: Sequence[ProductionModel] | None = None,
    ) -> pd.DataFrame:
        """Replay active models using only information preceding each target session.

        The persisted production boosters cannot safely be applied backwards:
        they were trained using observations that may postdate the simulated
        session.  Each replay fit therefore uses an expanding sample ending at
        ``as_of_date``, while preserving the model's frozen feature definition,
        XGBoost parameters, and decision thresholds.
        """

        start = pd.Timestamp(start_date).normalize()
        end = pd.Timestamp(end_date).normalize()
        rows: list[dict[str, Any]] = []
        training_config_by_model: dict[str, RStockConfig] = {}
        replay_models = self.repository.active_models() if models is None else models
        for model in replay_models:
            try:
                market_frames: list[pd.DataFrame] = []
                for symbol in model.symbols:
                    prices = price_loader(symbol)
                    if prices is None or prices.empty:
                        raise ValueError(f"missing market history for {symbol}")
                    market_frames.append(prefix_symbol_columns(prices, symbol))
                market_data = pd.concat(market_frames, axis=1).sort_index()
                prepared = prepare_dataset(
                    market_data,
                    model.symbols,
                    intraday_target_threshold=model.up_target_threshold,
                    lag_depth=model.lag_depth,
                    intraday_down_threshold=model.down_target_threshold,
                )
                metadata = json.loads(
                    (self.repository.artifact_directory(model.model_id)
                     / "production.metadata.json").read_text(encoding="utf-8")
                )
                if (
                    metadata.get("model_id") != model.model_id
                    or metadata.get("artifact_version") != model.artifact_version
                    or metadata.get("feature_version") != model.feature_version
                ):
                    raise ValueError("incompatible production metadata")
                names = list(metadata.get("predictor_columns") or [])
                if not names:
                    raise ValueError("missing persisted predictor definition")
                outcomes = {
                    "up": intraday_target_column(model.target),
                    "down": intraday_down_target_column(model.target),
                }
                directional_parameters = {
                    "up": XGBoostParameters(**model.xgboost_parameters),
                    "down": XGBoostParameters(**(
                        model.down_xgboost_parameters or model.xgboost_parameters
                    )),
                }
                training_config = training_config_by_model.setdefault(
                    model.model_id,
                    replace(
                        config,
                        xgb_seed=model.xgboost_seed,
                        xgb_nthread=model.xgboost_threads,
                    ),
                )
                target_dates = prepared.index[
                    (prepared.index >= start) & (prepared.index <= end)
                ]
                for target_date in target_dates:
                    try:
                        prior_dates = prepared.index[prepared.index < target_date]
                        if prior_dates.empty:
                            raise ValueError("no prior market observation")
                        as_of = prior_dates.max()
                        current = prepare_prediction_row(
                            prepared,
                            as_of_date=as_of,
                            target_date=target_date,
                            lag_depth=model.lag_depth,
                        )
                        if any(
                            name not in current or current[name].isna().any()
                            for name in names
                        ):
                            raise ValueError("missing predictors")
                        training = prepared.loc[
                            prepared.index <= as_of,
                            [*names, *outcomes.values()],
                        ].dropna()
                        if training.empty:
                            raise ValueError("no complete historical training observations")
                        probabilities: dict[str, float] = {}
                        for direction, outcome in outcomes.items():
                            booster = fit_booster(
                                training,
                                names,
                                outcome,
                                training_config,
                                parameters=directional_parameters[direction],
                            )
                            probabilities[direction] = float(
                                predict_probabilities(booster, current, names)[0]
                            )
                        prediction_id = hashlib.sha256(
                            f"historical:{model.model_id}:{model.artifact_version}:"
                            f"{pd.Timestamp(target_date).date()}".encode()
                        ).hexdigest()[:20]
                        rows.append({
                            "prediction_id": prediction_id,
                            "prediction_date": pd.Timestamp(target_date).date().isoformat(),
                            "as_of_date": pd.Timestamp(as_of).date().isoformat(),
                            "target": model.target,
                            "predictors": json.dumps(model.predictors),
                            "model_id": model.model_id,
                            "model_version": model.artifact_version,
                            "up_probability": probabilities["up"],
                            "down_probability": probabilities["down"],
                            "up_threshold": model.signal_threshold,
                            "down_threshold": model.down_threshold,
                            "signal_status": (
                                "bullish_signal"
                                if probabilities["up"] >= model.signal_threshold
                                and probabilities["down"] < model.down_threshold
                                else "no_signal"
                            ),
                            "status": "predicted",
                            "error": None,
                            "created_at": utc_now(),
                        })
                    except Exception as error:
                        rows.append(self._error_row(
                            model, f"{type(error).__name__}: {error}", target_date
                        ))
            except Exception as error:
                rows.append(self._error_row(model, f"{type(error).__name__}: {error}"))
        return pd.DataFrame(rows)

    def _predict_model(
        self,
        model: ProductionModel,
        prepared: pd.DataFrame,
        market_data: pd.DataFrame | None = None,
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
        features = _json_safe(current.loc[current.index[0], names].to_dict())
        source_observations = prediction_source_observations(
            prepared,
            names,
            as_of_date=as_of,
            market_data=market_data,
        )
        up = float(predict_probabilities(load_booster(directory / "up.ubj"), current, names)[0])
        down = float(predict_probabilities(load_booster(directory / "down.ubj"), current, names)[0])
        signal_status = (
            "bullish_signal"
            if up >= model.signal_threshold and down < model.down_threshold
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
            "feature_names": json.dumps(names),
            "features": json.dumps(features, ensure_ascii=False),
            "source_observations": json.dumps(source_observations, ensure_ascii=False),
            "model_id": model.model_id,
            "model_version": model.artifact_version,
            "up_probability": up,
            "down_probability": down,
            "up_threshold": model.signal_threshold,
            "down_threshold": model.down_threshold,
            "signal_status": signal_status,
            "status": "predicted",
            "error": None,
            "created_at": utc_now(),
        }

    def _predict_model_for_target(
        self,
        model: ProductionModel,
        prepared: pd.DataFrame,
        *,
        target_date: Any,
        market_data: pd.DataFrame | None = None,
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
        target = pd.Timestamp(target_date).normalize()
        observations = prepared.loc[prepared.index < target, return_name].dropna()
        if observations.empty:
            raise ValueError("no prior market observation")
        as_of = observations.index.max()
        current = prepare_prediction_row(
            prepared, as_of_date=as_of, target_date=target, lag_depth=model.lag_depth
        )
        names = list(metadata["predictor_columns"])
        if any(name not in current or current[name].isna().any() for name in names):
            raise ValueError("missing predictors")
        features = _json_safe(current.loc[current.index[0], names].to_dict())
        source_observations = prediction_source_observations(
            prepared, names, as_of_date=as_of, market_data=market_data
        )
        up = float(predict_probabilities(load_booster(directory / "up.ubj"), current, names)[0])
        down = float(predict_probabilities(load_booster(directory / "down.ubj"), current, names)[0])
        signal_status = (
            "bullish_signal"
            if up >= model.signal_threshold and down < model.down_threshold
            else "no_signal"
        )
        return {
            "prediction_id": self._prediction_id(model, target),
            "prediction_date": target.date().isoformat(),
            "as_of_date": pd.Timestamp(as_of).date().isoformat(),
            "target": model.target,
            "predictors": json.dumps(model.predictors),
            "feature_names": json.dumps(names),
            "features": json.dumps(features, ensure_ascii=False),
            "source_observations": json.dumps(source_observations, ensure_ascii=False),
            "model_id": model.model_id,
            "model_version": model.artifact_version,
            "up_probability": up,
            "down_probability": down,
            "up_threshold": model.signal_threshold,
            "down_threshold": model.down_threshold,
            "signal_status": signal_status,
            "status": "predicted",
            "error": None,
            "created_at": utc_now(),
        }

    @staticmethod
    def _prediction_id(model: ProductionModel, target_date: Any) -> str:
        return hashlib.sha256(
            f"{model.model_id}:{model.artifact_version}:{pd.Timestamp(target_date).date()}".encode()
        ).hexdigest()[:20]
    @staticmethod
    def _error_row(model: ProductionModel, error: str, date: Any = None) -> dict[str, Any]:
        token = f"{model.model_id}:{date}:{error}:{utc_now()}"
        return {
            "prediction_id": hashlib.sha256(token.encode()).hexdigest()[:20],
            "prediction_date": None if date is None else pd.Timestamp(date).date().isoformat(),
            "as_of_date": None, "target": model.target, "predictors": json.dumps(model.predictors),
            "feature_names": None, "features": None, "source_observations": None,
            "model_id": model.model_id, "model_version": model.artifact_version,
            "up_probability": np.nan, "down_probability": np.nan,
            "up_threshold": model.signal_threshold, "down_threshold": model.down_threshold,
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
        restrict_to_active_models: bool = True,
    ) -> pd.DataFrame:
        frame = (
            self.repository.read_active_model_table("predictions")
            if predictions is None
            else predictions.copy()
        )
        if restrict_to_active_models and not frame.empty:
            active_ids = self.repository.active_model_ids()
            if "model_id" not in frame:
                frame = frame.iloc[0:0].copy()
            else:
                frame = frame[frame["model_id"].astype(str).isin(active_ids)].copy()
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
            required_prices = pd.to_numeric(
                pd.Series({name: price.get(name) for name in ("Open", "High", "Low", "Close")}),
                errors="coerce",
            )
            if required_prices.isna().any() or not np.isfinite(required_prices.to_numpy()).all():
                continue
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
