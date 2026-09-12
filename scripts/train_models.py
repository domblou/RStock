"""Download model history and rebuild retained XGBoost models."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.data import download_market_data
from rstock.features import prepare_dataset
from rstock.symbols import read_symbol_universe
from rstock.training import train_models


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    args = parser.parse_args()
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    universe = read_symbol_universe(config.symbols_path)
    if config.selected_symbols:
        universe = universe.set_index("Symbol").loc[list(config.selected_symbols)].reset_index()
    else:
        universe = universe.iloc[: config.max_symbols]
    configured_symbols = universe["Symbol"].tolist()
    provider_symbols = universe.set_index("Symbol")["ProviderSymbol"].to_dict()
    market_calendars = universe.set_index("Symbol")["Calendar"].to_dict()
    downloaded = download_market_data(
        configured_symbols,
        config.model_history_days,
        provider_symbols=provider_symbols,
    )
    if not downloaded.symbols:
        raise RuntimeError("No symbols could be downloaded")
    if downloaded.failed_symbols:
        print(f"Skipped {len(downloaded.failed_symbols)} failed symbols")

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
