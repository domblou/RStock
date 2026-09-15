"""Small reproducible benchmark for legacy vs disk-backed walk-forward execution."""

from __future__ import annotations

import argparse
import gc
import json
import tempfile
import tracemalloc
from dataclasses import replace
from pathlib import Path
from time import perf_counter

import numpy as np
import pandas as pd

from rstock.checkpoints import CheckpointManager
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.streaming_walk_forward import run_streamed_walk_forward
from rstock.walk_forward import evaluate_prefilter_walk_forward, evaluate_walk_forward


def _inputs(root: Path, symbols: int, periods: int, batch_size: int):
    names = [f"S{index:02d}" for index in range(symbols)]
    dates = pd.bdate_range("2024-01-01", periods=periods)
    signal = np.arange(periods) % 2
    prices = pd.DataFrame(index=dates)
    for offset, symbol in enumerate(names):
        shifted = np.roll(signal, offset)
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        prices[f"{symbol}.High"] = np.maximum(prices[f"{symbol}.Close"], 100.0) + 1.0
        prices[f"{symbol}.Low"] = np.minimum(prices[f"{symbol}.Close"], 100.0) - 1.0
    config = replace(
        DEFAULT_CONFIG,
        project_root=root,
        permutation_depth=1,
        combination_workers=1,
        walk_forward_batch_size=batch_size,
        final_holdout_batch_size=batch_size,
        xgb_rounds=1,
        xgb_nthread=1,
        walk_forward_min_train_size=max(8, periods // 3),
        walk_forward_test_size=max(4, periods // 8),
        walk_forward_step_size=max(4, periods // 8),
        final_holdout_size=max(4, periods // 8),
        qualification_min_windows=1,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    return prepare_dataset(prices, names), generate_symbol_sets(names, 1), config


def _measure(operation):
    gc.collect()
    tracemalloc.start()
    started = perf_counter()
    value = operation()
    elapsed = perf_counter() - started
    _, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()
    return value, elapsed, peak


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--symbols", type=int, default=3)
    parser.add_argument("--periods", type=int, default=40)
    parser.add_argument("--batch-size", type=int, default=2)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="rstock-wf-benchmark-") as temporary:
        root = Path(temporary)
        prepared, generated, config = _inputs(
            root, args.symbols, args.periods, args.batch_size
        )
        legacy, legacy_seconds, legacy_peak = _measure(
            lambda: evaluate_walk_forward(prepared, generated, config)
        )
        legacy_rows = len(legacy.predictions)
        del legacy
        checkpoint = CheckpointManager(
            root / "run",
            run_id="run",
            job_type="walk_forward",
            configuration_fingerprint="benchmark",
            batch_sizes={
                "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
                "walk_forward": config.walk_forward_batch_size,
                "final_holdout": config.final_holdout_batch_size,
            },
        )
        streamed, streamed_seconds, streamed_peak = _measure(
            lambda: run_streamed_walk_forward(
                prepared,
                generated,
                config,
                checkpoint,
                root / "run" / "_working",
            )
        )
        legacy_prefilter, legacy_prefilter_seconds, legacy_prefilter_peak = _measure(
            lambda: evaluate_walk_forward(
                prepared, generated, config, evaluate_holdout=False
            )
        )
        legacy_prefilter_rows = len(legacy_prefilter.predictions)
        del legacy_prefilter
        lightweight_prefilter, lightweight_prefilter_seconds, lightweight_prefilter_peak = (
            _measure(
                lambda: evaluate_prefilter_walk_forward(
                    prepared, generated, config
                )
            )
        )
        print(json.dumps({
            "symbols": args.symbols,
            "combinations": len(generated),
            "prefilter": {
                "legacy": {
                    "seconds": legacy_prefilter_seconds,
                    "python_peak_bytes": legacy_prefilter_peak,
                    "prediction_rows_retained": legacy_prefilter_rows,
                },
                "lightweight": {
                    "seconds": lightweight_prefilter_seconds,
                    "python_peak_bytes": lightweight_prefilter_peak,
                    "qualification_rows_retained": len(
                        lightweight_prefilter.qualification
                    ),
                },
            },
            "legacy": {
                "seconds": legacy_seconds,
                "python_peak_bytes": legacy_peak,
                "prediction_rows_retained": legacy_rows,
            },
            "streamed": {
                "seconds": streamed_seconds,
                "python_peak_bytes": streamed_peak,
                "peak_batch_prediction_rows": streamed.telemetry[
                    "peak_batch_prediction_rows"
                ],
                "walk_forward_batches": streamed.telemetry["walk_forward_batches"],
                "aggregation_seconds": streamed.telemetry.get("aggregation_seconds"),
                "checkpoint_bytes": streamed.telemetry["checkpoint_bytes"],
            },
        }, indent=2))


if __name__ == "__main__":
    main()
