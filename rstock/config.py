"""Central configuration for every RStock workflow."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, slots=True)
class RStockConfig:
    """Central configuration for the active Python application."""

    project_root: Path
    models_directory: str = "Models"
    symbols_file: str = "Symbols.csv"
    symbols_to_survey_file: str = "SymbolsToSurvey.csv"
    prediction_result_file: str = "Prediction.csv"
    symbols_history_file: str = "SymbolsHistory.csv"

    max_symbols: int = 25
    selected_symbols: tuple[str, ...] | None = None
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
    max_generated_sets: int = 100_000

    train_fraction: float = 0.7
    xgb_seed: int = 1234
    prediction_threshold: float = 0.5

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
