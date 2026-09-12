"""Central configuration for every RStock workflow."""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Literal


ErrorMetric = Literal["legacy_predictors", "outcome"]


@dataclass(frozen=True, slots=True)
class RStockConfig:
    """Configuration equivalent to ``legacy_r/Settings.R``.

    Phase 2 evaluates against the actual outcome by default. The historical
    predictor-based calculation remains available as an explicit compatibility mode.
    """

    project_root: Path
    models_directory: str = "Models"
    symbols_file: str = "Symbols.csv"
    symbols_to_survey_file: str = "SymbolsToSurvey.csv"
    prediction_result_file: str = "Prediction.csv"
    symbols_history_file: str = "SymbolsHistory.csv"

    test_mode_max_symbols: int = 500
    test_mode_stock_symbols: tuple[str, ...] | None = None
    max_symbols_per_call: int = 1
    model_history_days: int = 730
    prediction_history_days: int = 10

    up_down_threshold: float = 0.01
    permutation_depth: int = 3
    date_feature_regex: str = ""

    xgb_max_depth: int = 6
    xgb_eta: float = 1.0
    xgb_nthread: int = 2
    xgb_rounds: int = 4
    keep_predictor_under: float = 0.2

    train_fraction: float = 0.7
    split_seed: int = 1234
    shuffle_seed: int | None = None
    prediction_threshold: float = 0.5
    error_metric: ErrorMetric = "outcome"
    legacy_history_value_comparison: bool = True
    raw_price_fields: tuple[str, ...] = field(
        default=("Open", "High", "Low", "Close", "Volume", "Adjusted")
    )

    def path(self, relative_name: str) -> Path:
        return self.project_root / relative_name

    @property
    def models_path(self) -> Path:
        return self.path(self.models_directory)

    @property
    def symbols_path(self) -> Path:
        return self.path(self.symbols_file)

    @property
    def survey_path(self) -> Path:
        return self.path(self.symbols_to_survey_file)

    @property
    def predictions_path(self) -> Path:
        return self.path(self.prediction_result_file)

    @property
    def history_path(self) -> Path:
        return self.path(self.symbols_history_file)


DEFAULT_CONFIG = RStockConfig(project_root=Path(__file__).resolve().parents[1])
