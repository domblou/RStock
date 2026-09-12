"""Refresh Symbols.csv from the configured listing source."""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path

from rstock.config import DEFAULT_CONFIG
from rstock.symbols import fetch_stock_symbols, write_symbols


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    args = parser.parse_args()
    config = replace(DEFAULT_CONFIG, project_root=args.project_root.resolve())

    symbols = fetch_stock_symbols()
    write_symbols(symbols, config.symbols_path)
    print(f"Wrote {len(symbols)} symbols to {config.symbols_path}")


if __name__ == "__main__":
    main()

