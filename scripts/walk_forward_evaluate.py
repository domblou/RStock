"""Evaluate current RStock signals over expanding temporal windows."""

from __future__ import annotations

import argparse
import hashlib
import json
import logging
from dataclasses import asdict
from dataclasses import replace
from pathlib import Path

import pandas as pd

from rstock.calendars import validate_calendar_name
from rstock.checkpoints import CheckpointManager
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
from rstock.streaming_walk_forward import run_streamed_walk_forward
from rstock.symbols import read_symbol_universe


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--symbols", nargs="+")
    parser.add_argument("--calendar")
    parser.add_argument("--force-refresh", action="store_true")
    parser.add_argument("--force-symbol", action="append", default=[])
    parser.add_argument("--history-days", type=int, default=DEFAULT_CONFIG.model_history_days)
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
    parser.add_argument(
        "--final-holdout-size", type=int, default=DEFAULT_CONFIG.final_holdout_size
    )
    parser.add_argument(
        "--combination-workers", type=int, default=DEFAULT_CONFIG.combination_workers
    )
    parser.add_argument(
        "--batch-size", type=int, default=DEFAULT_CONFIG.walk_forward_batch_size
    )
    parser.add_argument(
        "--min-windows", type=int, default=DEFAULT_CONFIG.qualification_min_windows
    )
    parser.add_argument(
        "--min-median-auc",
        type=float,
        default=DEFAULT_CONFIG.qualification_min_median_auc,
    )
    parser.add_argument(
        "--min-pct-windows-above-random",
        type=float,
        default=DEFAULT_CONFIG.qualification_min_pct_windows_above_random,
    )
    parser.add_argument(
        "--min-worst-window-auc",
        type=float,
        default=DEFAULT_CONFIG.qualification_min_worst_window_auc,
    )
    parser.add_argument(
        "--min-positive-observations",
        type=int,
        default=DEFAULT_CONFIG.qualification_min_positive_observations,
    )
    parser.add_argument(
        "--max-auc-std",
        type=float,
        default=DEFAULT_CONFIG.qualification_max_auc_std,
    )
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    config = replace(
        DEFAULT_CONFIG,
        project_root=args.project_root.resolve(),
        lag_depth=args.lag_depth,
        intraday_target_threshold=args.intraday_target_threshold,
        intraday_down_threshold=args.intraday_down_threshold,
        qualification_min_windows=args.min_windows,
        qualification_min_median_auc=args.min_median_auc,
        qualification_min_pct_windows_above_random=(
            args.min_pct_windows_above_random
        ),
        qualification_min_worst_window_auc=args.min_worst_window_auc,
        qualification_min_positive_observations=args.min_positive_observations,
        qualification_max_auc_std=args.max_auc_std,
        combination_workers=args.combination_workers,
        walk_forward_batch_size=args.batch_size,
        final_holdout_batch_size=args.batch_size,
        walk_forward_min_train_size=args.min_train_size,
        walk_forward_test_size=args.test_size,
        walk_forward_step_size=args.step_size,
        final_holdout_size=args.final_holdout_size,
    )

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

    fingerprint_values = asdict(config)
    fingerprint_values["project_root"] = str(config.project_root)
    fingerprint_values.update({
        "symbols": symbols,
        "universe": universe.to_dict("records"),
        "market_calendars": market_calendars,
        "history_days": args.history_days,
        "force_refresh": bool(args.force_refresh),
        "force_symbols": sorted(args.force_symbol),
    })
    fingerprint = hashlib.sha256(
        json.dumps(fingerprint_values, sort_keys=True, default=str).encode("utf-8")
    ).hexdigest()
    checkpoint = CheckpointManager(
        config.walk_forward_path / ".checkpoint_runs" / fingerprint,
        run_id=fingerprint,
        job_type="walk_forward",
        configuration_fingerprint=fingerprint,
        batch_sizes={
            "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
            "walk_forward": config.walk_forward_batch_size,
            "final_holdout": config.final_holdout_batch_size,
        },
    )
    if checkpoint.artifact_exists("prepared_snapshot"):
        prepared, snapshot = checkpoint.load_snapshot()
        generated = snapshot["generated_sets"]
        market_calendars = snapshot["market_calendars"]
    else:
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
            downloaded.prices,
            downloaded.symbols,
            config.intraday_target_threshold,
            config.lag_depth,
            config.intraday_down_threshold,
        )
        generated = generate_symbol_sets(
            downloaded.symbols,
            args.permutation_depth,
            max_sets=config.max_generated_sets,
        )
        checkpoint.commit_snapshot(prepared, {
            "generated_sets": generated,
            "market_calendars": market_calendars,
        })
    result = run_streamed_walk_forward(
        prepared,
        generated,
        config,
        checkpoint,
        config.walk_forward_path,
        market_calendars=market_calendars,
    )
    print(result.aggregate_global.to_string(index=False))
    print(
        f"Eligible combinations: {int(result.qualification['Eligible'].sum())}/"
        f"{len(result.qualification)}"
    )
    print(f"Results written to {config.walk_forward_path}")


if __name__ == "__main__":
    main()
