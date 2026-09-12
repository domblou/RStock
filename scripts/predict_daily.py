"""Run daily inference, update history, then validate pending predictions."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

import pandas as pd

from rstock.config import DEFAULT_CONFIG
from rstock.data import download_market_data
from rstock.features import prepare_dataset
from rstock.history import append_symbol_history, read_symbol_history, write_symbol_history
from rstock.prediction import (
    append_predictions,
    predict_saved_models,
    survey_symbols,
    write_predictions,
)
from rstock.validation import validate_pending_predictions


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    args = parser.parse_args()
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    survey = pd.read_csv(config.survey_path)
    downloaded = download_market_data(
        survey_symbols(survey), config.prediction_history_days
    )
    if not downloaded.symbols:
        raise RuntimeError("No survey symbols could be downloaded")

    history = append_symbol_history(
        downloaded.prices, read_symbol_history(config.history_path)
    )
    write_symbol_history(history, config.history_path)
    prepared = prepare_dataset(
        downloaded.prices, downloaded.symbols, config.up_down_threshold
    )
    daily = predict_saved_models(prepared, survey, config)
    if daily.empty and not config.predictions_path.exists():
        raise RuntimeError("No saved model had all required predictors")
    predictions = append_predictions(daily, config.predictions_path)
    predictions = validate_pending_predictions(
        predictions,
        history,
        config.up_down_threshold,
        legacy_character_comparison=config.legacy_history_value_comparison,
    )
    write_predictions(predictions, config.predictions_path)
    print(f"Added {len(daily)} predictions to {config.predictions_path}")


if __name__ == "__main__":
    main()
