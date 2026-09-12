"""Evaluate current RStock signals over expanding temporal windows."""

from __future__ import annotations

import argparse
import logging
from dataclasses import replace
from pathlib import Path

import pandas as pd

from rstock.calendars import validate_calendar_name
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
from rstock.symbols import read_symbol_universe
from rstock.walk_forward import evaluate_walk_forward, write_walk_forward_results


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--symbols", nargs="+")
    parser.add_argument("--calendar")
    parser.add_argument("--force-refresh", action="store_true")
    parser.add_argument("--force-symbol", action="append", default=[])
    parser.add_argument("--history-days", type=int, default=DEFAULT_CONFIG.model_history_days)
    parser.add_argument(
        "--permutation-depth", type=int, default=DEFAULT_CONFIG.permutation_depth
    )
    parser.add_argument(
        "--min-train-size", type=int, default=DEFAULT_CONFIG.walk_forward_min_train_size
    )
    parser.add_argument(
        "--test-size", type=int, default=DEFAULT_CONFIG.walk_forward_test_size
    )
    parser.add_argument(
        "--step-size", type=int, default=DEFAULT_CONFIG.walk_forward_step_size
    )
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    if args.symbols:
        if not args.calendar:
            parser.error("--calendar is required when --symbols is used")
        validate_calendar_name(args.calendar)
        symbols = args.symbols
        market_calendars = {symbol: args.calendar for symbol in symbols}
        universe = pd.DataFrame(
            {
                "Symbol": symbols,
                "ProviderSymbol": symbols,
                "Exchange": args.calendar,
                "Calendar": args.calendar,
            }
        )
    else:
        universe = read_symbol_universe(config.symbols_path).iloc[
            : config.walk_forward_max_symbols
        ]
        symbols = universe["Symbol"].tolist()
        market_calendars = universe.set_index("Symbol")["Calendar"].to_dict()

    downloaded = market_data_service(config).get_market_data(
        universe,
        args.history_days,
        force_refresh=args.force_refresh,
        force_symbols=set(args.force_symbol),
    )
    if len(downloaded.symbols) < 2:
        raise RuntimeError("At least two symbols must be downloaded")
    if downloaded.failed_symbols:
        print(f"Cache/download issues for: {', '.join(downloaded.failed_symbols)}")

    prepared = prepare_dataset(
        downloaded.prices, downloaded.symbols, config.up_down_threshold
    )
    generated = generate_symbol_sets(
        downloaded.symbols,
        args.permutation_depth,
        max_sets=config.max_generated_sets,
    )
    result = evaluate_walk_forward(
        prepared,
        generated,
        config,
        market_calendars=market_calendars,
        min_train_size=args.min_train_size,
        test_size=args.test_size,
        step_size=args.step_size,
    )
    write_walk_forward_results(result, config.walk_forward_path)
    print(result.aggregate_global.to_string(index=False))
    print(f"Results written to {config.walk_forward_path}")


if __name__ == "__main__":
    main()
