"""Durable orchestration contract for technical walk-forward child runs."""

from __future__ import annotations

import hashlib
import json
from datetime import datetime, timezone
from math import ceil
from typing import Any, Mapping, Sequence

from rstock.combination_planning import CombinationPlan

from .domain import ExperimentSpec, JobType, RunMetadata, RunRole
from .repository import RunRepository


MANIFEST_SCHEMA_VERSION = 1
MANIFEST_NAME = "orchestration/walk_forward_batches.json"
PREFILTER_POLICY_VERSION = "per_target_v1"
CHILD_ID_POLICY_DETERMINISTIC = 1
CHILD_ID_POLICY_RESERVED = 2


def _utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _canonical_sha256(value: object) -> str:
    payload = json.dumps(
        value, ensure_ascii=False, sort_keys=True, separators=(",", ":")
    )
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def symbols_sha256(symbols: Sequence[str]) -> str:
    return _canonical_sha256(list(symbols))


def prefilter_digest(predictors_by_target: Mapping[str, Sequence[str]]) -> str:
    return _canonical_sha256(
        [
            [str(target), [str(value) for value in predictors]]
            for target, predictors in predictors_by_target.items()
        ]
    )


def child_spec(
    parent: ExperimentSpec,
    *,
    parent_run_id: str,
    plan: CombinationPlan,
    range_start: int,
    range_stop: int,
) -> ExperimentSpec:
    return ExperimentSpec(
        job_type=JobType.WALK_FORWARD_BATCH,
        config=parent.config,
        symbols=parent.symbols,
        calendar=parent.calendar,
        combinations_per_target=parent.combinations_per_target,
        evaluate_final_holdout=False,
        universe_selection=parent.universe_selection,
        primary_universe_id=parent.primary_universe_id,
        context_universe_ids=parent.context_universe_ids,
        context_sample_size=parent.context_sample_size,
        context_selection_method=parent.context_selection_method,
        context_seed=parent.context_seed,
        target_symbols=parent.target_symbols,
        context_symbols=parent.context_symbols,
        predictor_symbols=parent.predictor_symbols,
        source_experiment_run=parent.source_experiment_run,
        source_walk_forward_run=parent_run_id,
        frozen_xgboost_parameters=parent.frozen_xgboost_parameters,
        frozen_threshold_calibration_parameters=(
            parent.frozen_threshold_calibration_parameters
        ),
        run_description=(
            f"Batch WF {range_start}:{range_stop} du run {parent_run_id}"
        ),
        historical_data_cutoff=parent.historical_data_cutoff,
        source_prepared_dataset_sha256=parent.source_prepared_dataset_sha256,
        source_end_to_end_run=parent.source_end_to_end_run,
        pipeline_version=parent.pipeline_version,
        calibration_sampling_policy_version=(
            parent.calibration_sampling_policy_version
        ),
        combination_plan_version=plan.plan_version,
        combination_plan_sha256=plan.plan_sha256,
        combination_range_start=range_start,
        combination_range_stop=range_stop,
    )


def build_manifest(
    repository: RunRepository,
    *,
    parent_run_id: str,
    parent_spec: ExperimentSpec,
    raw_plan: CombinationPlan,
    effective_plan: CombinationPlan,
    max_combinations_per_batch: int,
    prefilter_policy_version: str,
    prefilter_sha256: str,
    child_id_policy_version: int = CHILD_ID_POLICY_RESERVED,
    reserved_child_ids: Sequence[str] | None = None,
) -> tuple[dict[str, Any], list[tuple[ExperimentSpec, RunMetadata]]]:
    """Reserve child IDs and return one immutable manifest."""

    if child_id_policy_version not in {
        CHILD_ID_POLICY_DETERMINISTIC,
        CHILD_ID_POLICY_RESERVED,
    }:
        raise ValueError("Politique d'ID enfant WF incompatible")

    effective_count = effective_plan.count()
    batch_count = ceil(effective_count / max_combinations_per_batch)
    preview_batch_count = ceil(raw_plan.count() / max_combinations_per_batch)
    if reserved_child_ids is not None and len(reserved_child_ids) != batch_count:
        raise ValueError("Réservations enfant WF incomplètes")
    children: list[dict[str, Any]] = []
    reservations: list[tuple[ExperimentSpec, RunMetadata]] = []
    for batch_index in range(batch_count):
        start = batch_index * max_combinations_per_batch
        stop = min(start + max_combinations_per_batch, effective_count)
        relation_key = f"walk_forward_batch:{batch_index:06d}"
        if reserved_child_ids is not None:
            run_id = str(reserved_child_ids[batch_index])
        elif child_id_policy_version == CHILD_ID_POLICY_DETERMINISTIC:
            run_id = repository.deterministic_child_run_id(parent_run_id, relation_key)
        else:
            run_id = repository.generate_run_id()
        spec = child_spec(
            parent_spec,
            parent_run_id=parent_run_id,
            plan=effective_plan,
            range_start=start,
            range_stop=stop,
        )
        metadata = RunMetadata(
            run_role=RunRole.TECHNICAL_BATCH,
            visible_in_history=False,
            parent_run_id=parent_run_id,
            relation_key=relation_key,
            relation_type="walk_forward_batch",
            stage_key="walk_forward",
            stage_index=0,
            batch_id=f"{batch_index:06d}",
            batch_index=batch_index,
            batch_count=batch_count,
        )
        children.append(
            {
                "batch_id": f"{batch_index:06d}",
                "batch_index": batch_index,
                "batch_count": batch_count,
                "range_start": start,
                "range_stop": stop,
                "combination_count": stop - start,
                "child_run_id": run_id,
                "expected_child_fingerprint": spec.fingerprint,
            }
        )
        reservations.append((spec, metadata))
    manifest = {
        "schema_version": MANIFEST_SCHEMA_VERSION,
        "child_id_policy_version": child_id_policy_version,
        "parent_run_id": parent_run_id,
        "parent_fingerprint": repository.configuration_fingerprint(
            parent_run_id, fallback=parent_spec.fingerprint
        ),
        "combination_plan_version": effective_plan.plan_version,
        "combination_plan_sha256": effective_plan.plan_sha256,
        "raw_combination_plan_sha256": raw_plan.plan_sha256,
        "targets_sha256": symbols_sha256(parent_spec.target_symbols),
        "predictors_sha256": symbols_sha256(parent_spec.predictor_symbols),
        "context_sha256": symbols_sha256(parent_spec.context_symbols),
        "target_count": len(parent_spec.target_symbols),
        "predictor_count": len(parent_spec.predictor_symbols),
        "context_count": len(parent_spec.context_symbols),
        "depth": parent_spec.config.permutation_depth,
        "raw_combination_count": raw_plan.count(),
        "preview_batch_count": preview_batch_count,
        "prefiltered_combination_count": effective_count,
        "effective_batch_count": batch_count,
        "planned_batch_count": batch_count,
        "max_combinations_per_batch": max_combinations_per_batch,
        "prefilter_policy_version": prefilter_policy_version,
        "prefilter_digest": prefilter_sha256,
        "batch_count": batch_count,
        "created_at": _utc_now(),
        "batches": children,
    }
    return manifest, reservations


def persist_or_validate_manifest(
    repository: RunRepository, run_id: str, proposed: dict[str, Any]
) -> dict[str, Any]:
    """Persist once; on resume, reload and verify every frozen field."""

    path = repository.run_directory(run_id) / MANIFEST_NAME
    if path.exists():
        persisted = repository.read_json(run_id, MANIFEST_NAME)
        comparable = dict(proposed)
        comparable["created_at"] = persisted.get("created_at")
        # V1 manifests predate the explicit policy field.  Their deterministic
        # reservations remain authoritative and are never rewritten.
        if "child_id_policy_version" not in persisted:
            comparable.pop("child_id_policy_version", None)
        if persisted != comparable:
            raise ValueError(
                "Le manifest des batchs WF ne correspond plus au plan gelé."
            )
        return persisted
    path.parent.mkdir(parents=True, exist_ok=True)
    repository.write_json(run_id, MANIFEST_NAME, proposed)
    # Re-read the durable source of truth before creating any child.
    persisted = repository.read_json(run_id, MANIFEST_NAME)
    if persisted != proposed:
        raise ValueError("Le manifest des batchs WF n'a pas été persisté fidèlement.")
    return persisted


def materialize_reservations(
    repository: RunRepository,
    *,
    manifest: Mapping[str, Any],
    reservations: Sequence[tuple[ExperimentSpec, RunMetadata]],
) -> None:
    """Create absent directories with exactly the IDs already in the manifest."""

    batches = list(manifest.get("batches", []))
    if len(batches) != len(reservations):
        raise ValueError("Manifest WF incohérent avec ses réservations")
    for batch, (spec, metadata) in zip(batches, reservations, strict=True):
        run_id = str(batch["child_run_id"])
        directory = repository.run_directory(run_id)
        if directory.exists():
            existing = repository.child_for_relation(
                str(manifest["parent_run_id"]), metadata.relation_key or ""
            )
            if existing != run_id:
                raise ValueError("Relation de batch WF ambiguë")
            if repository.configuration_fingerprint(run_id) != str(
                batch["expected_child_fingerprint"]
            ):
                raise ValueError("Fingerprint d'un enfant WF incompatible")
            continue
        repository.create(spec, run_id=run_id, metadata=metadata)


def load_manifest(repository: RunRepository, run_id: str) -> dict[str, Any] | None:
    path = repository.run_directory(run_id) / MANIFEST_NAME
    return repository.read_json(run_id, MANIFEST_NAME) if path.exists() else None
