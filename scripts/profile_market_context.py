"""Profile persisted context inputs without writing to any run directory."""
from __future__ import annotations

import argparse
import cProfile
import io
import json
import pickle
import pstats
from pathlib import Path
from time import perf_counter

import pandas as pd

from rstock.application.market_context import aggregate_context, normalize_predictions
from rstock.application.market_context_runtime import prediction_groups


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run", type=Path)
    parser.add_argument("--sets", type=int, default=8)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--compare", type=Path)
    args = parser.parse_args()
    if args.sets < 1 or args.output.resolve().is_relative_to(args.run.parent.resolve()):
        raise ValueError("Use a positive sample size and an output outside the runs directory")
    manifest = json.loads((args.run / "results/context_diagnostic_manifest.json").read_text())
    context_path = args.run.parent / manifest["context_manifest"]
    context = pd.read_csv(context_path.parent / "market_context.csv")
    groups = prediction_groups(args.run / "results/predictions.csv")
    raw = []
    try:
        for _ in range(args.sets):
            group = next(groups, None)
            if group is None:
                break
            raw.append(group)
    finally:
        groups.close()
    profile = cProfile.Profile()
    started = perf_counter()
    profile.enable()
    outputs = [aggregate_context(
        normalize_predictions(group, signal_rule="persisted_wf_up"), context,
        stage="walk_forward", signal_rule="persisted_wf_up",
    ) for group in raw]
    profile.disable()
    elapsed = perf_counter() - started
    if args.compare:
        with args.compare.open("rb") as stream:
            expected = pickle.load(stream)
        assert len(outputs) == len(expected)
        for actual, reference in zip(outputs, expected, strict=True):
            for frame, old in zip(actual, reference, strict=True):
                pd.testing.assert_frame_equal(frame, old, check_exact=False, rtol=1e-12, atol=1e-12)
    args.output.mkdir(parents=True, exist_ok=True)
    with (args.output / "metrics.pkl").open("wb") as stream:
        pickle.dump(outputs, stream)
    report = {"sets": len(raw), "input_rows": sum(len(frame) for frame in raw),
              "elapsed_seconds": elapsed, "parity_verified": args.compare is not None}
    (args.output / "timing.json").write_text(json.dumps(report, indent=2))
    stats = io.StringIO()
    pstats.Stats(profile, stream=stats).sort_stats("cumulative").print_stats(20)
    (args.output / "profile.txt").write_text(stats.getvalue())
    print(json.dumps(report))
    print(stats.getvalue())


if __name__ == "__main__":
    main()
