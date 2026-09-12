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
from rstock.symbols import read_symbol_universe
from rstock.validation import validate_pending_predictions


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    args = parser.parse_args()
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    survey = pd.read_csv(config.survey_path)
    symbols = survey_symbols(survey)
    universe = read_symbol_universe(config.symbols_path).set_index("Symbol")
    missing_symbols = sorted(set(symbols) - set(universe.index))
    if missing_symbols:
        raise ValueError(f"Survey symbols are missing from Symbols.csv: {missing_symbols}")
    provider_symbols = universe.loc[symbols, "ProviderSymbol"].to_dict()
    downloaded = download_market_data(
        symbols,
        config.prediction_history_days,
        provider_symbols=provider_symbols,
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
    observed_dates = {
        symbol: downloaded.prices.index[
            downloaded.prices[f"{symbol}.Close"].notna()
        ]
        for symbol in downloaded.symbols
    }
    daily = predict_saved_models(
        prepared, survey, config, observed_dates_by_symbol=observed_dates
    )
    if daily.empty and not config.predictions_path.exists():
        raise RuntimeError("No saved model had all required predictors")
    predictions = append_predictions(daily, config.predictions_path)
    predictions = validate_pending_predictions(
        predictions,
        history,
        config.up_down_threshold,
    )
    write_predictions(predictions, config.predictions_path)
    print(f"Added {len(daily)} predictions to {config.predictions_path}")


if __name__ == "__main__":
    main()
