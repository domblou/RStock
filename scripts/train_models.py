"""Download model history and rebuild retained XGBoost models."""

from __future__ import annotations

import argparse
import logging
from dataclasses import replace
from pathlib import Path

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.market_cache import market_data_service
from rstock.symbols import read_symbol_universe
from rstock.training import train_models


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--force-refresh", action="store_true")
    parser.add_argument("--force-symbol", action="append", default=[])
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    universe = read_symbol_universe(config.symbols_path)
    if config.selected_symbols:
        universe = universe.set_index("Symbol").loc[list(config.selected_symbols)].reset_index()
    else:
        universe = universe.iloc[: config.max_symbols]
    market_calendars = universe.set_index("Symbol")["Calendar"].to_dict()
    downloaded = market_data_service(config).get_market_data(
        universe,
        config.model_history_days,
        force_refresh=args.force_refresh,
        force_symbols=set(args.force_symbol),
    )
    if not downloaded.symbols:
        raise RuntimeError("No symbols could be downloaded")
    if downloaded.failed_symbols:
        print(
            "Cache/download issues for: "
            + ", ".join(downloaded.failed_symbols)
        )

    prepared = prepare_dataset(
        downloaded.prices, downloaded.symbols, config.up_down_threshold
    )
    generated = generate_symbol_sets(
        downloaded.symbols,
        config.permutation_depth,
        max_sets=config.max_generated_sets,
    )
    result = train_models(prepared, generated, config, market_calendars)
    result.survey_sets.to_csv(config.survey_path, index=False)
    print(
        f"Evaluated {len(result.evaluated_sets)} sets; retained "
        f"{len(result.survey_sets)} in {config.models_path}"
    )


if __name__ == "__main__":
    main()
