"""Repair missing historical primary-universe names without recalculating quality."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from rstock.application.production_universe_lineage_backfill import (
    backfill_primary_universe_names,
)
from rstock.config import DEFAULT_CONFIG


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Backfill only historically demonstrable production universe names."
    )
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    report = backfill_primary_universe_names(
        args.project_root, apply=not args.dry_run,
    )
    rendered = json.dumps(report, ensure_ascii=False, indent=2) + "\n"
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(rendered, encoding="utf-8")
    print(rendered, end="")


if __name__ == "__main__":
    main()
