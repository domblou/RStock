"""Prepare research configurations and frozen local input; never submit jobs."""
from __future__ import annotations
import argparse
import hashlib
import json
from dataclasses import replace
from pathlib import Path

from rstock.application.repository import RunRepository
from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
from rstock.application.derived_snapshot import load_source_prepared_snapshot
from rstock.features import model_predictor_columns


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", default="20261011T115541_2a0e278563")
    parser.add_argument("--project", type=Path, default=Path("."))
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    repository = RunRepository(args.project.resolve() / "runs")
    if args.output.resolve().is_relative_to(repository.root.resolve()):
        raise ValueError("Configuration output must remain outside runs")
    source = repository.load_spec(args.source)
    expected = {"predictive_model_type": "target_only", "lag_depth": 3, "xgb_max_depth": 2,
                "xgb_eta": .2, "xgb_rounds": 20, "xgb_round_selection_mode": "fixed",
                "walk_forward_window_mode": "expanding"}
    if len(source.target_symbols) != 20 or any(getattr(source.config, k) != v for k, v in expected.items()):
        raise ValueError("Source differs from the approved MVP protocol")
    inputs = [p for p in repository.run_directory(args.source).rglob("*") if p.is_file()]
    digests = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
    control = replace(source, config=replace(source.config, target_volatility_enabled=False, target_volatility_sessions=21))
    constant = build_derived_walk_forward_spec(repository, args.source, {"predictive_model_type": "constant_probability"})
    enriched = build_derived_walk_forward_spec(repository, args.source,
        {"target_volatility_enabled": True, "target_volatility_sessions": 21})
    prepared, _, targets, _ = load_source_prepared_snapshot(repository, enriched,
        expected_snapshot_sha256=enriched.walk_forward_derivation["prepared_snapshot_sha256"])
    coverage = {}
    for target in targets:
        base = model_predictor_columns(prepared, target, [], control.config)
        added = model_predictor_columns(prepared, target, [], enriched.config)
        labels = [f"{target}.intraday_target", f"{target}.intraday_down_target"]
        old = prepared[[*base, *labels]].dropna().index
        new = prepared[[*added, *labels]].dropna().index
        coverage[target] = {"control_observations": len(old), "enriched_observations": len(new),
                            "same_training_calendar": old.equals(new)}
    assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest() == d for p, d in digests.items())
    args.output.mkdir(parents=True, exist_ok=True)
    for label, spec in (("A_constant", constant), ("B_control", control), ("C_volatility_21", enriched)):
        (args.output / f"{label}.json").write_text(json.dumps(spec.to_dict(), indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    report = {"source": args.source, "jobs_created": 0, "jobs_launched": 0,
              "source_quality": "retrospective_not_pit", "historical_inputs_unchanged": True,
              "coverage": coverage, "source_sha256": digests,
              "protocol": "Aligned constant on common WF training; no volatility definition search; independent holdout kept."}
    (args.output / "readiness.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps({k: v for k, v in report.items() if k != "source_sha256"}, indent=2))


if __name__ == "__main__":
    main()
