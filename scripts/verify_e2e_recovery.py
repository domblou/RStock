"""Read-only E2E preflight and native parallel/serial probability comparison."""
from __future__ import annotations

import argparse
import hashlib
import json
import pickle
from dataclasses import replace
from pathlib import Path
from time import perf_counter

import pandas as pd

from rstock.application.end_to_end import validate_pipeline_resume, load_pipeline_manifest, build_stage_spec
from rstock.application.repository import RunRepository
from rstock.application.run_timing import observed_run_timing
from rstock.modeling import XGBoostParameters
from rstock.threshold_calibration import generate_development_probabilities, split_development_holdout
from rstock.traceability import prepared_dataset_hash


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--sets", type=int, default=4)
    args = parser.parse_args()
    if args.sets < 1:
        raise ValueError("Verification sample size must be positive")
    if args.output.resolve().is_relative_to(args.run.parent.resolve()):
        raise ValueError("Verification output must be outside the runs directory")
    repository = RunRepository(args.run.parent)
    root_id = args.run.name
    verified = validate_pipeline_resume(repository, root_id)
    manifest = load_pipeline_manifest(repository, root_id)
    parent = repository.load_spec(root_id)
    child_id = next(row["child_run_id"] for row in manifest["stages"] if row["stage_key"] == "threshold_parameter_calibration")
    child = build_stage_spec(repository, root_id, parent, "threshold_parameter_calibration", manifest)
    assert child.fingerprint == repository.configuration_fingerprint(child_id)
    source_id = child.source_walk_forward_run
    snapshot_path = repository.run_directory(source_id) / "checkpoints/artifacts/prepared_snapshot.pkl"
    metadata = json.loads(snapshot_path.with_suffix(".json").read_text())
    raw = snapshot_path.read_bytes()
    assert hashlib.sha256(raw).hexdigest() == metadata["sha256"]
    prepared = pickle.loads(raw)["prepared"]
    assert prepared_dataset_hash(prepared) == child.source_prepared_dataset_sha256
    sampled = pd.read_csv(repository.run_directory(child.source_xgboost_calibration_run) / "results/sampled_combinations.csv")
    sampled = sampled.drop(columns="Set").iloc[:args.sets]
    development, _, _ = split_development_holdout(prepared, child.config.final_holdout_size)
    parameters = {direction: XGBoostParameters(**values) for direction, values in child.frozen_xgboost_parameters.items()}
    kwargs = dict(min_train_size=child.config.walk_forward_min_train_size,
                  test_size=child.config.walk_forward_test_size, step_size=child.config.walk_forward_step_size,
                  parameters_by_direction=parameters)
    started = perf_counter()
    serial = generate_development_probabilities(development, sampled, replace(child.config, combination_workers=1), **kwargs)
    serial_seconds = perf_counter() - started
    started = perf_counter()
    parallel = generate_development_probabilities(development, sampled, child.config, **kwargs)
    parallel_seconds = perf_counter() - started
    pd.testing.assert_frame_equal(parallel, serial, check_exact=True)
    assert validate_pipeline_resume(repository, root_id) == verified
    report = {"run_id": root_id, "stages_verified": verified,
              "snapshot_fingerprint_verified": True, "prepared_dataset_digest_verified": True,
              "sampled_sets": len(sampled), "probability_rows": len(parallel), "exact_probability_parity": True,
              "serial_seconds": serial_seconds, "parallel_seconds": parallel_seconds,
              "threshold_parameter_timing": observed_run_timing(repository.run_directory(child_id), repository.status(child_id))}
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "readiness.json").write_text(json.dumps(report, indent=2))
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
