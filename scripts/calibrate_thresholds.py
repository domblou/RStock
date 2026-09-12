"""Calibrate directional probability thresholds from the local market cache."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

import pandas as pd

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG, RStockConfig
from rstock.data import prefix_symbol_columns
from rstock.features import prepare_dataset
from rstock.market_cache import ParquetMarketDataStore
from rstock.threshold_calibration import (
    run_controlled_threshold_calibration,
    write_threshold_calibration_results,
)


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--symbols", nargs="+")
    parser.add_argument("--permutation-depth", type=int, default=1)
    parser.add_argument("--combinations-per-target", type=int, default=3)
    parser.add_argument(
        "--min-train-size", type=int, default=DEFAULT_CONFIG.walk_forward_min_train_size
    )
    parser.add_argument(
        "--test-size", type=int, default=DEFAULT_CONFIG.walk_forward_test_size
    )
    parser.add_argument(
        "--step-size", type=int, default=DEFAULT_CONFIG.walk_forward_step_size
    )
    parser.add_argument(
        "--final-holdout-size", type=int, default=DEFAULT_CONFIG.final_holdout_size
    )
    parser.add_argument(
        "--combination-workers", type=int, default=DEFAULT_CONFIG.combination_workers
    )
    parser.add_argument(
        "--min-signals-per-window",
        type=int,
        default=DEFAULT_CONFIG.threshold_calibration_min_signals_per_window,
    )
    parser.add_argument(
        "--min-window-fraction",
        type=float,
        default=DEFAULT_CONFIG.threshold_calibration_min_window_fraction,
    )
    parser.add_argument(
        "--threshold-quantiles",
        type=float,
        nargs="+",
        default=DEFAULT_CONFIG.threshold_calibration_quantiles,
    )
    parser.add_argument("--evaluate-final-holdout", action="store_true")
    parser.add_argument(
        "--output-directory",
        type=Path,
        default=Path("WalkForward/ThresholdCalibration"),
    )
    return parser


def _cached_prices(
    config: RStockConfig, requested_symbols: list[str] | None
) -> tuple[pd.DataFrame, list[str]]:
    store = ParquetMarketDataStore(
        config.market_data_path, config.market_cache_metadata_path
    )
    metadata = store.read_metadata()
    symbols = requested_symbols or sorted(
        str(symbol) for symbol in metadata.get("symbols", {})
    )
    if len(symbols) < 2:
        raise RuntimeError("At least two symbols must exist in the market cache")
    frames: list[pd.DataFrame] = []
    missing: list[str] = []
    for symbol in symbols:
        prices = store.read(symbol)
        if prices is None or prices.empty:
            missing.append(symbol)
        else:
            frames.append(prefix_symbol_columns(prices, symbol))
    if missing:
        raise RuntimeError(f"Missing cached market data for: {', '.join(missing)}")
    combined = pd.concat(frames, axis=1).sort_index()
    combined.index.name = "Date"
    return combined, symbols


def main() -> None:
    args = _parser().parse_args()
    root = args.project_root.resolve()
    config = replace(
        DEFAULT_CONFIG,
        project_root=root,
        threshold_calibration_min_signals_per_window=args.min_signals_per_window,
        threshold_calibration_min_window_fraction=args.min_window_fraction,
        threshold_calibration_quantiles=tuple(args.threshold_quantiles),
        combination_workers=args.combination_workers,
    )
    prices, symbols = _cached_prices(config, args.symbols)
    prepared = prepare_dataset(
        prices,
        symbols,
        config.intraday_target_threshold,
        config.lag_depth,
        config.intraday_down_threshold,
    )
    generated = generate_symbol_sets(
        symbols, args.permutation_depth, max_sets=config.max_generated_sets
    )
    result = run_controlled_threshold_calibration(
        prepared,
        generated,
        config,
        combinations_per_target=args.combinations_per_target,
        min_train_size=args.min_train_size,
        test_size=args.test_size,
        step_size=args.step_size,
        final_holdout_size=args.final_holdout_size,
        evaluate_final_holdout=args.evaluate_final_holdout,
    )
    output = args.output_directory
    if not output.is_absolute():
        output = root / output
    write_threshold_calibration_results(result, output)
    print(result.calibration.baseline_comparison.to_string(index=False))
    print(f"Results written to {output}")


if __name__ == "__main__":
    main()
