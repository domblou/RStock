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
    walk_forward_min_train_size: int = 252
    walk_forward_test_size: int = 63
    walk_forward_step_size: int = 63
    walk_forward_max_symbols: int = 4
    final_holdout_size: int = 63

    # Calibration parameters for stability qualification. They are fixed before
    # the final holdout is evaluated and must not be tuned from holdout results.
    qualification_min_windows: int = 3
    qualification_min_median_auc: float = 0.55
    qualification_min_pct_windows_above_random: float = 2 / 3
    qualification_min_worst_window_auc: float = 0.45
    qualification_min_positive_observations: int = 20
    qualification_max_auc_std: float = 0.10
    final_confirmation_min_auc: float = 0.50

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
    threshold_calibration_min_window_fraction: float = 1.0
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

USER_SETTINGS_SCHEMA_VERSION = 1
USER_SETTINGS_RELATIVE_PATH = Path("data") / "config" / "user_settings.json"
UI_SETTINGS_DEFAULTS: dict[str, object] = {
    "lab_calendar": "XNYS",
    "lab_combinations_per_target": 3,
    "lab_evaluate_holdout": True,
    "max_concurrent_heavy_jobs": 1,
}


def user_settings_path(project_root: Path) -> Path:
    """Return the portable location of the persisted UI settings file."""

    return project_root / USER_SETTINGS_RELATIVE_PATH


def _coerce_config_value(name: str, value: object, default_value: object) -> object:
    """Validate a JSON value before applying it to ``RStockConfig``."""

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

