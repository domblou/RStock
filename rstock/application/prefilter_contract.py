"""Immutable public boundary between predictor selection and Walk-forward."""
from __future__ import annotations

import hashlib
import json
from dataclasses import asdict, replace
from pathlib import Path

from rstock.combination_planning import CombinationPlan
from .domain import ExperimentSpec, JobType
from .repository import RunRepository

CONTRACT = "results/prefilter_contract.json"


def digest(value: object) -> str:
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":"),
                                     ensure_ascii=False).encode()).hexdigest()


def publish(repository: RunRepository, run_id: str, spec: ExperimentSpec, output: Path) -> None:
    root = repository.run_directory(run_id)
    report = json.loads((output / "predictor_prefilter.json").read_text(encoding="utf-8"))
    sidecar = json.loads((root / "checkpoints/artifacts/prepared_snapshot.json").read_text(encoding="utf-8"))
    ordered = [[target, list(report["predictors_by_target"].get(target, []))]
               for target in (sorted(spec.target_symbols) if spec.prefilter_method == "temporal_consensus" else spec.target_symbols)]
    contract = {
        "schema_version": 1, "source_run_id": run_id,
        "configuration_fingerprint": repository.configuration_fingerprint(run_id),
        "prepared_dataset_sha256": report["prepared_dataset_sha256"],
        "prepared_snapshot_sha256": sidecar["sha256"],
        "cutoff": report["prepared_dataset_as_of"],
        "selection_mode": spec.prefilter_method,
        "origin_snapshots": report.get("origins", []),
        "origin_cutoffs": report.get("origin_cutoffs", [report["prepared_dataset_as_of"]]),
        "ordered_predictors_by_target": ordered,
        "selection_sha256": digest(ordered),
        "effective_configuration": {key: str(value) if isinstance(value, Path) else value
                                    for key, value in asdict(spec.config).items()},
    }
    contract["effective_configuration"].update(
        prefilter_selection_mode=spec.prefilter_method,
        stability_origin_count=spec.stability_origin_count,
        stability_step_sessions=spec.stability_step_sessions,
    )
    contract["contract_sha256"] = digest(contract)
    (output / "prefilter_contract.json").write_text(
        json.dumps(contract, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )


def load(repository: RunRepository, spec: ExperimentSpec) -> dict:
    if not spec.config.predictor_prefilter_enabled:
        raise ValueError("A Prefilter reference requires enabled predictor prefiltering")
    source = spec.source_prefilter_run
    if not source or not spec.source_prefilter_contract_sha256:
        raise ValueError("Walk-forward requires an explicit completed Prefilter reference")
    if repository.status(source).get("status") != "completed":
        raise ValueError("Source Prefilter is not completed")
    if repository.storage(source)["state"] != "full":
        raise ValueError("Source Prefilter snapshot has been purged")
    if repository.load_spec(source).job_type is not JobType.PREDICTOR_PREFILTER:
        raise ValueError("Source is not a Prefilter job")
    path = repository.run_directory(source) / CONTRACT
    raw = path.read_bytes()
    if hashlib.sha256(raw).hexdigest() != spec.source_prefilter_contract_sha256:
        raise ValueError("Frozen Prefilter contract changed")
    value = json.loads(raw)
    if value.get("cutoff") != spec.historical_data_cutoff:
        raise ValueError("Walk-forward cutoff must exactly match the frozen Prefilter cutoff")
    if (spec.prefilter_execution_version >= 2
            and spec.resolved_market_session_cutoff is not None
            and spec.resolved_market_session_cutoff != value["cutoff"]):
        raise ValueError("Resolved Walk-forward cutoff must match the frozen Prefilter cutoff")
    scientific = {key: item for key, item in value.items() if key != "contract_sha256"}
    if (value.get("schema_version") != 1 or value.get("contract_sha256") != digest(scientific)
            or value.get("source_run_id") != source
            or value.get("configuration_fingerprint") != repository.configuration_fingerprint(source)
            or value.get("selection_sha256") != digest(value["ordered_predictors_by_target"])
            or value.get("prepared_dataset_sha256") != spec.source_prepared_dataset_sha256):
        raise ValueError("Invalid Prefilter provenance or selection digest")
    snapshot = repository.run_directory(source) / "checkpoints/artifacts/prepared_snapshot.pkl"
    if hashlib.sha256(snapshot.read_bytes()).hexdigest() != value["prepared_snapshot_sha256"]:
        raise ValueError("Frozen Prefilter snapshot changed")
    # An inherited selection must never have seen the new WF holdout.
    source_spec = repository.load_spec(source)
    if (set(source_spec.predictor_symbols) != set(spec.predictor_symbols)
            or set(source_spec.target_symbols) != set(spec.target_symbols)
            or source_spec.calendar != spec.calendar):
        raise ValueError("Prefilter universe or calendar differs from Walk-forward")
    source_config = source_spec.config
    if spec.config.final_holdout_size > source_config.final_holdout_size:
        raise ValueError("Inherited Prefilter selection overlaps the new holdout; recompute Prefilter")
    return value


def inherit_reference(repository: RunRepository, spec: ExperimentSpec) -> ExperimentSpec:
    """Bind a newly created copy to its selected Prefilter; never alter a resumed run."""
    raw = (repository.run_directory(spec.source_prefilter_run) / CONTRACT).read_bytes()
    contract = json.loads(raw)
    inherited = replace(
        spec, historical_data_cutoff=contract["cutoff"],
        requested_historical_cutoff=None, resolved_market_session_cutoff=contract["cutoff"],
        source_prefilter_contract_sha256=hashlib.sha256(raw).hexdigest(),
        source_prepared_dataset_sha256=contract["prepared_dataset_sha256"],
        prepared_snapshot_required=True, prepared_dataset_digest_required=True,
    )
    load(repository, inherited)
    return inherited


def plan(repository: RunRepository, spec: ExperimentSpec) -> CombinationPlan:
    contract = load(repository, spec)
    mapping = dict(contract["ordered_predictors_by_target"])
    if not set(mapping).issubset(spec.target_symbols) or any(
        not set(items).issubset(spec.predictor_symbols) for items in mapping.values()
    ):
        raise ValueError("Prefilter universe differs from Walk-forward")
    result = CombinationPlan.from_target_predictors(mapping, spec.config.permutation_depth)
    if result.count() == 0:
        raise ValueError("Prefilter retained no testable combinations; Walk-forward cannot start")
    return result
