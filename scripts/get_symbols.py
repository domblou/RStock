"""Refresh Symbols.csv from the configured listing source."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

from rstock.config import DEFAULT_CONFIG
from rstock.symbols import fetch_stock_universe, write_symbol_universe


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    args = parser.parse_args()
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    universe = fetch_stock_universe()
    write_symbol_universe(universe, config.symbols_path)
    print(f"Wrote {len(universe)} symbols to {config.symbols_path}")


if __name__ == "__main__":
    main()
