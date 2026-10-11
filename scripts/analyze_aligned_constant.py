"""Read-only local WF comparison; output must remain outside historical runs."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import pandas as pd

from rstock.application.aligned_constant_comparison import reconstruct_aligned_constants
from rstock.application.common_prediction_comparison import compare_common_predictions, _read_predictions
from rstock.evaluation import classification_metrics


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run_ids", nargs="+")
    parser.add_argument("--project", type=Path, default=Path("."))
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    runs = args.project.resolve() / "runs"
    if args.output.resolve().is_relative_to(runs):
        raise ValueError("Analysis output must be outside runs")
    roots = {r: runs / r for r in args.run_ids}
    inputs = [root / name for root in roots.values() for name in (
        "config.json", "status.json", "results/run_configuration.json", "results/windows.csv",
        "results/predictions.csv", "checkpoints/artifacts/prepared_snapshot.pkl",
        "checkpoints/artifacts/prepared_snapshot.json")]
    digests = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in inputs}
    aligned, diagnostics = reconstruct_aligned_constants(roots)
    historical = {r: _read_predictions(root) for r, root in roots.items()}
    controlled = {**historical, **aligned}
    # Same evaluation dates for before/after: remove any change in test population
    # from the reported effect of changing only the baseline's training labels.
    rows = []
    keys = set.intersection(*(set(zip(f.Observation, f.Direction)) for f in historical.values()))
    for observation, direction in sorted(keys):
        dates = set.intersection(*(set(group.Date) for frames in (historical, controlled)
            for frame in frames.values()
            for _, group in frame[(frame.Observation == observation) & (frame.Direction == direction)].groupby("Set")))
        for protocol, frames in (("historical", historical), ("controlled", controlled)):
            for run_id, frame in frames.items():
                selected = frame[(frame.Observation == observation) & (frame.Direction == direction) & frame.Date.isin(dates)]
                for set_id, group in selected.groupby("Set"):
                    metrics = classification_metrics(group.Target, group.Prediction, group.Probability)
                    rows.append({"Protocol": protocol, "Run": run_id, "Observation": observation,
                        "Direction": direction, "Set": set_id, "Observations": len(group),
                        "FirstDate": group.Date.min(), "LastDate": group.Date.max(), **metrics.as_columns()})
    fair = pd.DataFrame(rows)
    scores = fair.groupby(["Protocol", "Run"])[["LogLoss", "Brier", "Observations"]].mean().reset_index()
    audit, full = compare_common_predictions(args.project, args.run_ids)
    if audit["training_alignment"]["status"] != "verified":
        raise ValueError("Full controlled comparison could not be verified")
    assert all(hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest for path, digest in digests.items())
    args.output.mkdir(parents=True, exist_ok=True)
    for run_id, frame in aligned.items():
        frame.to_csv(args.output / f"aligned_constant_{run_id}.csv", index=False)
    diagnostics.to_csv(args.output / "alignment_windows.csv", index=False)
    historical_windows = pd.concat([pd.read_csv(root / "results/windows.csv").assign(Run=r)
                                     for r, root in roots.items()], ignore_index=True)
    historical_windows.to_csv(args.output / "historical_windows.csv", index=False)
    fair.to_csv(args.output / "same_test_dates_metrics.csv", index=False)
    full.to_csv(args.output / "full_controlled_metrics.csv", index=False)
    summary = {"protocol": "common_availability_mask_verified_historical_xgb_windows",
        "historical_inputs_unchanged": True, "input_sha256": digests,
        "same_test_dates_scores": scores.to_dict("records"),
        "full_controlled_scores": full.groupby("Run")[["LogLoss", "Brier", "CommonObservations"]].mean().reset_index().to_dict("records"),
        "weighting": "equal_weight_per_target_direction_candidate; latest_historical_origin_per_date",
        "alignment_windows": len(diagnostics), "xgboost_training_executed": False}
    (args.output / "summary.json").write_text(json.dumps(summary, indent=2, default=str), encoding="utf-8")
    print(scores.to_string(index=False))
    print("\nFull controlled test population:")
    print(full.groupby("Run")[["LogLoss", "Brier", "CommonObservations"]].mean().to_string())


if __name__ == "__main__":
    main()
