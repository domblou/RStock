"""Descriptive protocol validation on an explicitly supplied SPY snapshot only.

Example: python scripts/validate_market_context_protocol.py --snapshot data/market/SPY.parquet --output diagnostics/context_protocol_validation
No RStock prediction or model-performance input is accepted.
"""
from __future__ import annotations

import argparse
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import pandas as pd
import exchange_calendars as xcals
from rstock.application.forward_diagnostic import _publish, digest
from rstock.application.market_context import adjusted_snapshot, descriptive_variants, ContextProtocol


EPISODES = {
    "2018_Q4": ("2018-10-01", "2018-12-31"),
    "2020_Feb_Mar": ("2020-02-19", "2020-03-31"),
    "2020_Apr_Jun": ("2020-04-01", "2020-06-30"),
    "2022_first_half": ("2022-01-03", "2022-06-30"),
    "2023_Aug_Oct": ("2023-08-01", "2023-10-31"),
    "2023_Nov_2024_Mar": ("2023-11-01", "2024-03-28"),
    "2024_Jul_Aug": ("2024-07-15", "2024-08-16"),
    "2025_Feb_Apr": ("2025-02-19", "2025-04-30"),
    "2025_May_Jun": ("2025-05-01", "2025-06-30"),
}


def validate(snapshot_path: Path, output: Path):
    raw = pd.read_parquet(snapshot_path) if snapshot_path.suffix == ".parquet" else pd.read_csv(snapshot_path, index_col=0, parse_dates=True)
    snapshot = adjusted_snapshot(raw)
    sessions = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range(snapshot.index.min(), snapshot.index.max())).tz_localize(None)
    variants = descriptive_variants(snapshot, sessions, EPISODES)
    frozen = snapshot.copy()
    frozen.index.name = "Date"
    _publish(output / "spy_adjusted_validation.csv", frozen.reset_index())
    _publish(output / "window_variants.csv", variants)
    missing = [name for name, (start, end) in EPISODES.items() if snapshot.index.min() > pd.Timestamp(start) or snapshot.index.max() < pd.Timestamp(end)]
    insufficient = variants.loc[variants.coverage.lt(.95), "episode"].unique().tolist()
    _publish(output / "validation_manifest.json", {
        "schema_version": 1, "protocol": "spy_adjusted_context_v1", "standard_protocol_id": ContextProtocol().identifier,
        "purpose": "Descriptive benchmark-only sensitivity check; no optimization and no model outcomes",
        "source": str(snapshot_path), "source_sha256": digest(snapshot_path),
        "snapshot_start": snapshot.index.min().isoformat(), "snapshot_end": snapshot.index.max().isoformat(),
        "episodes": EPISODES, "missing_or_partial_episodes": missing, "insufficient_warmup_episodes": insufficient,
        "status": "partial_historical_coverage" if missing or insufficient else "complete_descriptive_coverage",
        "defaults": {"trend": 63, "drawdown": 252, "volatility": 21},
        "selection": "Defaults fixed by protocol decision, not selected from model results or variant scores",
        "artifact_digests": {name: digest(output / name) for name in ("spy_adjusted_validation.csv", "window_variants.csv")},
    })
    return variants


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--snapshot", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = validate(args.snapshot, args.output)
    print(result.to_string(index=False))
