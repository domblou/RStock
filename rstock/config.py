"""Central configuration for every RStock workflow."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


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
