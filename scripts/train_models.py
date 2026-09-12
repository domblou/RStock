"""Download model history and rebuild retained XGBoost models."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.data import download_market_data
from rstock.features import prepare_dataset
from rstock.symbols import read_symbols
from rstock.training import train_models


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    metric_group = parser.add_mutually_exclusive_group()
    metric_group.add_argument(
        "--legacy-error-metric",
        action="store_true",
        help="Use the historical predictor-based error for compatibility",
    )
    metric_group.add_argument(
        "--correct-error-metric",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    args = parser.parse_args()
    config = replace(
        DEFAULT_CONFIG,
        project_root=args.project_root.resolve(),
        error_metric="legacy_predictors" if args.legacy_error_metric else "outcome",
    )

    configured_symbols = (
        list(config.test_mode_stock_symbols)
        if config.test_mode_stock_symbols
        else read_symbols(config.symbols_path)[: config.test_mode_max_symbols]
    )
    downloaded = download_market_data(configured_symbols, config.model_history_days)
    if not downloaded.symbols:
        raise RuntimeError("No symbols could be downloaded")
    if downloaded.failed_symbols:
        print(f"Skipped {len(downloaded.failed_symbols)} failed symbols")

    prepared = prepare_dataset(
        downloaded.prices, downloaded.symbols, config.up_down_threshold
    )
    generated = generate_symbol_sets(downloaded.symbols, config.permutation_depth)
    result = train_models(prepared, generated, config)
    result.survey_sets.to_csv(config.survey_path, index=False)
    print(
        f"Evaluated {len(result.evaluated_sets)} sets; retained "
        f"{len(result.survey_sets)} in {config.models_path}"
    )


if __name__ == "__main__":
    main()
