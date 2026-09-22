"""Central configuration for every RStock workflow."""

from __future__ import annotations

import json
import os
import tempfile
from dataclasses import dataclass, fields, replace
from pathlib import Path
from typing import Mapping


@dataclass(frozen=True, slots=True)
class RStockConfig:
    """Central configuration for the active Python application."""

    project_root: Path
    models_directory: str = "Models"
    data_directory: str = "data"
    market_data_directory: str = "market"
    symbols_directory: str = "symbols"
    metadata_directory: str = "metadata"
    symbols_file: str = "symbols.csv"
    market_cache_metadata_file: str = "market_cache.json"
    symbols_to_survey_file: str = "SymbolsToSurvey.csv"
    prediction_result_file: str = "Prediction.csv"
    symbols_history_file: str = "SymbolsHistory.csv"
    walk_forward_directory: str = "WalkForward"

    max_symbols: int = 25
    selected_symbols: tuple[str, ...] | None = None
    market_cache_workers: int = 8
    combination_workers: int = 3
    predictor_prefilter_batch_size: int = 25
    walk_forward_batch_size: int = 25
    final_holdout_batch_size: int = 25
    walk_forward_max_combinations_per_batch: int | None = 2_200_000
    xgboost_global_max_qualified_combinations: int | None = 500
    threshold_parameter_calibration_max_models: int | None = 500
    model_history_days: int = 730
    prediction_history_days: int = 10

    intraday_target_threshold: float = 0.01
    intraday_down_threshold: float = 0.01
    lag_depth: int = 3
    permutation_depth: int = 3
    date_feature_regex: str = ""

    xgb_max_depth: int = 6
    xgb_eta: float = 1.0
    xgb_nthread: int = 2
    xgb_rounds: int = 4
    xgb_min_child_weight: float = 1.0
    xgb_subsample: float = 1.0
    xgb_colsample_bytree: float = 1.0
    xgb_gamma: float = 0.0
    xgb_reg_alpha: float = 0.0
    xgb_reg_lambda: float = 1.0
    keep_predictor_under: float = 0.2
    max_generated_sets: int = 100_000

    train_fraction: float = 0.7
    xgb_seed: int = 1234
    prediction_threshold: float = 0.5
    walk_forward_window_mode: str = "expanding"
    walk_forward_min_train_size: int = 252
    walk_forward_train_size: int = 252
    walk_forward_test_size: int = 63
    walk_forward_step_size: int = 63
    walk_forward_max_symbols: int = 4
    final_holdout_size: int = 63
    walk_forward_end_offset_sessions: int = 0

    # Calibration parameters for stability qualification. They are fixed before
    # the final holdout is evaluated and must not be tuned from holdout results.
    qualification_min_windows: int = 3
    qualification_min_median_auc: float = 0.55
    qualification_min_pct_windows_above_random: float = 2 / 3
    qualification_min_worst_window_auc: float = 0.45
    qualification_min_positive_observations: int = 20
    qualification_max_auc_std: float = 0.10
    final_confirmation_min_auc: float = 0.50

    # Final promotion policy, evaluated on holdout metrics after threshold
    # calibration. These values are persisted with every run snapshot.
    promotion_min_holdout_signals: int = 20
    promotion_min_holdout_auc: float = 0.60
    promotion_min_holdout_precision: float = 0.40
    promotion_min_mean_directional_return: float = 0.00
    promotion_max_opposite_movement_frequency: float = 0.30

    # Final ranking of models that already passed development qualification.
    # Missing components (for example signal calibration) are excluded and the
    # remaining weights are renormalized explicitly.
    model_selection_predictive_quality_weight: float = 0.15
    model_selection_stability_weight: float = 0.30
    model_selection_holdout_weight: float = 0.30
    model_selection_signal_quality_weight: float = 0.10
    model_selection_sample_adequacy_weight: float = 0.15

    # Optional development-only predictor prefilter applied before depth 2/3
    # combinations are materialized.
    predictor_prefilter_enabled: bool = False
    predictor_prefilter_top_n: int = 10
    predictor_prefilter_min_median_auc: float = 0.50
    predictor_prefilter_min_pct_above_random: float = 0.50
    predictor_prefilter_min_worst_auc: float = 0.40
    predictor_prefilter_max_auc_std: float = 0.15
    predictor_prefilter_correlation_threshold: float = 0.90

    # Decision-threshold calibration is performed only on development predictions.
    threshold_calibration_min_signals_per_window: int = 20
    # Total signals required before a threshold is preferred as a robust sample.
    # This is deliberately separate from the per-window eligibility requirement.
    threshold_calibration_min_robust_signals: int = 10
    threshold_calibration_min_window_fraction: float = 1.0
    # Precision band used before the Up economic tie-breaker.
    threshold_calibration_precision_tolerance: float = 0.01
    threshold_calibration_quantiles: tuple[float, ...] = (
        0.50,
        0.60,
        0.70,
        0.75,
        0.80,
        0.85,
        0.90,
        0.925,
        0.95,
        0.975,
        0.99,
    )
    threshold_calibration_grid_decimals: int = 6

    # Temporal validation compares the completed reference End-to-end run with
    # its reserved future-period child. These are policy inputs, persisted in
    # each snapshot so an historical decision is reproducible.
    temporal_min_candidate_yield_ratio: float = 0.25
    temporal_max_auc_degradation: float = 0.03
    temporal_min_precision_edge: float = 0.00
    temporal_min_mean_directional_return: float = 0.00
    temporal_confidence_level: float = 0.95
    temporal_max_ci_width: float = 0.20

    def __post_init__(self) -> None:
        if self.walk_forward_window_mode not in {"expanding", "rolling"}:
            raise ValueError(
                "walk_forward_window_mode must be 'expanding' or 'rolling'"
            )
        for name in (
            "walk_forward_min_train_size",
            "walk_forward_train_size",
            "walk_forward_test_size",
            "walk_forward_step_size",
        ):
            value = getattr(self, name)
            if not isinstance(value, int) or isinstance(value, bool) or value < 1:
                raise ValueError(f"{name} must be an integer >= 1")
        for name in (
            "temporal_min_candidate_yield_ratio",
            "temporal_max_auc_degradation",
            "temporal_max_ci_width",
        ):
            value = getattr(self, name)
            if not isinstance(value, (int, float)) or isinstance(value, bool) or value < 0:
                raise ValueError(f"{name} must be numeric and >= 0")
        for name in (
            "temporal_min_precision_edge",
            "temporal_min_mean_directional_return",
        ):
            value = getattr(self, name)
            if not isinstance(value, (int, float)) or isinstance(value, bool):
                raise ValueError(f"{name} must be numeric")
        confidence = self.temporal_confidence_level
        if (
            not isinstance(confidence, (int, float))
            or isinstance(confidence, bool)
            or not 0 < confidence < 1
        ):
            raise ValueError("temporal_confidence_level must be between 0 and 1")
        if (
            not isinstance(self.promotion_min_holdout_signals, int)
            or isinstance(self.promotion_min_holdout_signals, bool)
            or self.promotion_min_holdout_signals < 1
        ):
            raise ValueError("promotion_min_holdout_signals must be an integer >= 1")
        for name in (
            "promotion_min_holdout_auc",
            "promotion_min_holdout_precision",
            "promotion_max_opposite_movement_frequency",
        ):
            value = getattr(self, name)
            if not isinstance(value, (int, float)) or isinstance(value, bool) or not 0 <= value <= 1:
                raise ValueError(f"{name} must be between zero and one")
        value = self.promotion_min_mean_directional_return
        if not isinstance(value, (int, float)) or isinstance(value, bool):
            raise ValueError("promotion_min_mean_directional_return must be numeric")
        for name in (
            "walk_forward_max_combinations_per_batch",
            "xgboost_global_max_qualified_combinations",
        ):
            value = getattr(self, name)
            if value is not None and (
                not isinstance(value, int) or isinstance(value, bool) or value < 1
            ):
                raise ValueError(f"{name} must be null or an integer >= 1")
        threshold_cap = self.threshold_parameter_calibration_max_models
        if threshold_cap is not None and (
            not isinstance(threshold_cap, int)
            or isinstance(threshold_cap, bool)
            or threshold_cap < 2
            or threshold_cap % 2
        ):
            raise ValueError(
                "threshold_parameter_calibration_max_models must be null or an "
                "even integer >= 2"
            )

    def path(self, relative_name: str) -> Path:
        return self.project_root / relative_name

    @property
    def models_path(self) -> Path:
        return self.path(self.models_directory)

    @property
    def symbols_path(self) -> Path:
        return self.symbols_data_path / self.symbols_file

    @property
    def data_path(self) -> Path:
        return self.path(self.data_directory)

    @property
    def market_data_path(self) -> Path:
        return self.data_path / self.market_data_directory

    @property
    def symbols_data_path(self) -> Path:
        return self.data_path / self.symbols_directory

    @property
    def metadata_path(self) -> Path:
        return self.data_path / self.metadata_directory

    @property
    def market_cache_metadata_path(self) -> Path:
        return self.metadata_path / self.market_cache_metadata_file

    @property
    def survey_path(self) -> Path:
        return self.path(self.symbols_to_survey_file)

    @property
    def predictions_path(self) -> Path:
        return self.path(self.prediction_result_file)

    @property
    def history_path(self) -> Path:
        return self.path(self.symbols_history_file)

    @property
    def walk_forward_path(self) -> Path:
        return self.path(self.walk_forward_directory)


DEFAULT_CONFIG = RStockConfig(project_root=Path(__file__).resolve().parents[1])

# Fields absent from old immutable run snapshots must retain the behavior those
# runs were created with, rather than inheriting today's defaults.
HISTORICAL_MISSING_CONFIG_DEFAULTS: dict[str, object] = {
    # Before configurable geometry, every walk-forward was expanding. Keep that
    # scientific behavior when immutable historical snapshots omit these fields.
    "walk_forward_window_mode": "expanding",
    "walk_forward_train_size": 252,
    "walk_forward_end_offset_sessions": 63,
    "max_generated_sets": 1_000_000_000,
    # Batch execution was introduced with the fixed capacity below. Snapshots
    # created before the field was persisted must retain that capacity.
    "walk_forward_max_combinations_per_batch": 2_200_000,
    "xgboost_global_max_qualified_combinations": None,
    "threshold_parameter_calibration_max_models": None,
    # Promotion used these fixed values before they were persisted. Keep the
    # historical policy for snapshots that do not carry the new fields.
    "promotion_min_holdout_signals": 20,
    "promotion_min_holdout_auc": 0.60,
    "promotion_min_holdout_precision": 0.40,
    "promotion_min_mean_directional_return": 0.00,
    "promotion_max_opposite_movement_frequency": 0.30,
    # Runs created before temporal validation could not execute its comparison;
    # these values are inert unless the explicitly persisted feature flag is on.
    "temporal_min_candidate_yield_ratio": 0.25,
    "temporal_max_auc_degradation": 0.03,
    "temporal_min_precision_edge": 0.00,
    "temporal_min_mean_directional_return": 0.00,
    "temporal_confidence_level": 0.95,
    "temporal_max_ci_width": 0.20,
}

USER_SETTINGS_SCHEMA_VERSION = 1
USER_SETTINGS_RELATIVE_PATH = Path("data") / "config" / "user_settings.json"
UI_SETTINGS_DEFAULTS: dict[str, object] = {
    "lab_calendar": "XNYS",
    "lab_combinations_per_target": 3,
    "lab_evaluate_holdout": True,
    "max_concurrent_heavy_jobs": 1,
    "sensitivity_threshold_min": 0.10,
    "sensitivity_threshold_max": 0.60,
    "sensitivity_threshold_step": 0.025,
}


def user_settings_path(project_root: Path) -> Path:
    """Return the portable location of the persisted UI settings file."""

    return project_root / USER_SETTINGS_RELATIVE_PATH


def _coerce_config_value(name: str, value: object, default_value: object) -> object:
    """Validate a JSON value before applying it to ``RStockConfig``."""

    if name in {
        "walk_forward_max_combinations_per_batch",
        "xgboost_global_max_qualified_combinations",
        "threshold_parameter_calibration_max_models",
    } and value is None:
        return None

    if name == "selected_symbols":
        if value is None:
            return None
        if isinstance(value, (list, tuple)) and all(isinstance(item, str) for item in value):
            return tuple(value)
        raise ValueError("selected_symbols must be a list of strings or null")

    if name == "threshold_calibration_quantiles":
        if isinstance(value, (list, tuple)):
            return tuple(float(item) for item in value)
        raise ValueError("threshold_calibration_quantiles must be a list")

    if isinstance(default_value, bool):
        if isinstance(value, bool):
            return value
        raise ValueError(f"{name} must be a boolean")

    if isinstance(default_value, int) and not isinstance(default_value, bool):
        if isinstance(value, int) and not isinstance(value, bool):
            if name == "walk_forward_end_offset_sessions" and value < 0:
                raise ValueError(f"{name} must be an integer >= 0")
            if name.endswith("_batch_size") and value < 1:
                raise ValueError(f"{name} must be an integer >= 1")
            if name in {
                "walk_forward_max_combinations_per_batch",
                "xgboost_global_max_qualified_combinations",
            } and value < 1:
                raise ValueError(f"{name} must be an integer >= 1")
            if name == "threshold_parameter_calibration_max_models" and (
                value < 2 or value % 2
            ):
                raise ValueError(f"{name} must be an even integer >= 2")
            return value
        raise ValueError(f"{name} must be an integer")

    if isinstance(default_value, float):
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            return float(value)
        raise ValueError(f"{name} must be numeric")

    if isinstance(default_value, str):
        if isinstance(value, str):
            return value
        raise ValueError(f"{name} must be a string")

    if isinstance(default_value, tuple):
        if isinstance(value, (list, tuple)):
            return tuple(value)
        raise ValueError(f"{name} must be a list")

    return value


def _coerce_ui_value(name: str, value: object) -> object:
    if name == "lab_calendar":
        if isinstance(value, str) and value.strip():
            return value
        raise ValueError("lab_calendar must be a non-empty string")
    if name in {"lab_combinations_per_target", "max_concurrent_heavy_jobs"}:
        if isinstance(value, int) and not isinstance(value, bool) and value >= 1:
            return value
        raise ValueError(f"{name} must be an integer >= 1")
    if name == "lab_evaluate_holdout":
        if isinstance(value, bool):
            return value
        raise ValueError("lab_evaluate_holdout must be a boolean")
    if name in {
        "sensitivity_threshold_min",
        "sensitivity_threshold_max",
        "sensitivity_threshold_step",
    }:
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            numeric = float(value)
            if name == "sensitivity_threshold_step" and numeric > 0:
                return numeric
            if name != "sensitivity_threshold_step" and 0 <= numeric <= 1:
                return numeric
        raise ValueError(f"{name} must be a valid sensitivity threshold")
    raise ValueError(f"unknown UI setting: {name}")


def load_user_settings(
    default_config: RStockConfig,
) -> tuple[RStockConfig, dict[str, object], str | None]:
    """Load persisted settings while keeping defaults for missing/invalid fields.

    ``project_root`` is deliberately never read from disk so the same settings
    remain portable between Windows and Linux deployments.
    """

    ui_settings = dict(UI_SETTINGS_DEFAULTS)
    path = user_settings_path(default_config.project_root)
    if not path.exists():
        return default_config, ui_settings, None

    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        return default_config, ui_settings, f"Paramètres persistés illisibles : {error}"

    if not isinstance(payload, dict):
        return default_config, ui_settings, "Paramètres persistés invalides : objet JSON attendu."

    config_payload = payload.get("config", {})
    ui_payload = payload.get("ui", {})
    warnings: list[str] = []

    if not isinstance(config_payload, dict):
        warnings.append("section config invalide")
        config_payload = {}
    if not isinstance(ui_payload, dict):
        warnings.append("section ui invalide")
        ui_payload = {}

    config_field_names = {item.name for item in fields(RStockConfig)}
    overrides: dict[str, object] = {}
    for name, value in config_payload.items():
        if name == "project_root" or name not in config_field_names:
            continue
        try:
            overrides[name] = _coerce_config_value(
                name, value, getattr(default_config, name)
            )
        except (TypeError, ValueError):
            warnings.append(f"valeur ignorée pour {name}")

    try:
        loaded_config = replace(default_config, **overrides)
    except (TypeError, ValueError) as error:
        return default_config, ui_settings, f"Paramètres persistés invalides : {error}"

    for name, value in ui_payload.items():
        if name not in UI_SETTINGS_DEFAULTS:
            continue
        try:
            ui_settings[name] = _coerce_ui_value(name, value)
        except ValueError:
            warnings.append(f"valeur UI ignorée pour {name}")

    warning = None
    if warnings:
        warning = "Certains paramètres persistés ont été ignorés : " + ", ".join(warnings) + "."
    return loaded_config, ui_settings, warning


def _json_value(value: object) -> object:
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, tuple):
        return list(value)
    return value


def save_user_settings(
    config: RStockConfig,
    ui_settings: Mapping[str, object],
    *,
    default_config: RStockConfig = DEFAULT_CONFIG,
) -> Path:
    """Persist user-overridden settings atomically and return the file path."""

    config_values: dict[str, object] = {}
    for item in fields(RStockConfig):
        name = item.name
        if name == "project_root":
            continue
        value = getattr(config, name)
        default_value = getattr(default_config, name)
        if value != default_value:
            config_values[name] = _json_value(value)

    ui_values: dict[str, object] = {}
    for name, default_value in UI_SETTINGS_DEFAULTS.items():
        raw_value = ui_settings.get(name, default_value)
        value = _coerce_ui_value(name, raw_value)
        if value != default_value:
            ui_values[name] = _json_value(value)

    payload = {
        "schema_version": USER_SETTINGS_SCHEMA_VERSION,
        "config": config_values,
        "ui": ui_values,
    }

    path = user_settings_path(config.project_root)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            newline="\n",
            dir=path.parent,
            prefix=f"{path.name}.",
            suffix=".tmp",
            delete=False,
        ) as handle:
            json.dump(payload, handle, ensure_ascii=False, indent=2, sort_keys=True)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
            temporary_path = Path(handle.name)
        os.replace(temporary_path, path)
    except Exception:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)
        raise
    return path

