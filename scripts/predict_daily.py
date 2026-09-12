"""Run daily inference, update history, then validate pending predictions."""

from __future__ import annotations

import argparse
import logging
from dataclasses import replace
from pathlib import Path

import pandas as pd

from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.history import append_symbol_history, read_symbol_history, write_symbol_history
from rstock.prediction import (
    append_predictions,
    predict_saved_models,
    survey_symbols,
    write_predictions,
)
from rstock.market_cache import market_data_service
from rstock.symbols import read_symbol_universe
from rstock.validation import validate_pending_predictions


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--force-refresh", action="store_true")
    parser.add_argument("--force-symbol", action="append", default=[])
    parser.add_argument("--lag-depth", type=int, default=DEFAULT_CONFIG.lag_depth)
    parser.add_argument(
        "--intraday-target-threshold",
        type=float,
        default=DEFAULT_CONFIG.intraday_target_threshold,
    )
    parser.add_argument(
        "--intraday-down-threshold",
        type=float,
        default=DEFAULT_CONFIG.intraday_down_threshold,
    )
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    config = replace(
        DEFAULT_CONFIG,
        project_root=args.project_root.resolve(),
        lag_depth=args.lag_depth,
        intraday_target_threshold=args.intraday_target_threshold,
        intraday_down_threshold=args.intraday_down_threshold,
    )

    survey = pd.read_csv(config.survey_path)
    symbols = survey_symbols(survey)
    universe = read_symbol_universe(config.symbols_path).set_index("Symbol")
    missing_symbols = sorted(set(symbols) - set(universe.index))
    if missing_symbols:
        raise ValueError(f"Survey symbols are missing from Symbols.csv: {missing_symbols}")
    requested_universe = universe.loc[symbols].reset_index()
    downloaded = market_data_service(config).get_market_data(
        requested_universe,
        config.prediction_history_days,
        force_refresh=args.force_refresh,
        force_symbols=set(args.force_symbol),
    )
    if not downloaded.symbols:
        raise RuntimeError("No survey symbols could be downloaded")

    history = append_symbol_history(
        downloaded.prices, read_symbol_history(config.history_path)
    )
    write_symbol_history(history, config.history_path)
    prepared = prepare_dataset(
        downloaded.prices,
        downloaded.symbols,
        config.intraday_target_threshold,
        config.lag_depth,
        config.intraday_down_threshold,
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
    )
    write_predictions(predictions, config.predictions_path)
    print(f"Added {len(daily)} predictions to {config.predictions_path}")


if __name__ == "__main__":
    main()
