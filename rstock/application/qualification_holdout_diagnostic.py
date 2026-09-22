"""Materialization of the diagnostic holdout for rejected WF candidates."""

from __future__ import annotations

import json
from dataclasses import replace

import pandas as pd

from .domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunPurpose, RunRole
from .forced_candidate_validation import load_forced_validation_manifest
from .repository import RunRepository


PROTOCOL = "qualification_holdout_diagnostic_v1"
RELATION_KEY = f"qualification_holdout_diagnostic:{PROTOCOL}"
INSUFFICIENT_REASONS = {
    "insufficient_windows",
    "insufficient_auc_windows",
    "positive_observations",
}


def _source_walk_forward_run(repository: RunRepository, forced_run_id: str) -> str:
    manifest = load_forced_validation_manifest(repository, forced_run_id)
    if manifest is None:
        raise ValueError("Manifest de revalidation forcee absent")
    stage = next(
        (item for item in manifest["stages"] if item.get("stage_key") == "walk_forward"),
        None,
    )
    if not isinstance(stage, dict) or not stage.get("child_run_id"):
        raise ValueError("Child Walk-forward force absent")
    return str(stage["child_run_id"])


def rejected_identities(
    repository: RunRepository, forced_run_id: str
) -> tuple[tuple[tuple[str, ...], ...], tuple[tuple[str, str], ...]]:
    """Return persisted, evaluable WF rejects in the source identity order."""

    source = repository.load_spec(forced_run_id)
    if source.job_type is not JobType.FORCED_CANDIDATE_VALIDATION:
        raise ValueError("Une revalidation forcee est requise")
    wf_id = _source_walk_forward_run(repository, forced_run_id)
    path = repository.run_directory(wf_id) / "results" / "qualification.csv"
    if not path.is_file():
        raise ValueError("Qualification WF source absente")
    qualification = pd.read_csv(path)
    rows = {str(row["Set"]): row for _, row in qualification.iterrows()}
    identities: list[tuple[str, str]] = []
    sets: list[tuple[str, ...]] = []
    for set_name, direction in source.forced_candidate_identities or ():
        row = rows.get(set_name)
        if row is None or bool(row.get("Eligible", False)):
            continue
        try:
            reasons = set(json.loads(str(row.get("IneligibilityReasons", "[]"))))
        except json.JSONDecodeError:
            continue
        if reasons & INSUFFICIENT_REASONS:
            continue
        symbols = tuple(str(item) for item in json.loads(set_name))
        identities.append((set_name, direction))
        sets.append(symbols)
    return tuple(sets), tuple(identities)


def diagnostic_state(repository: RunRepository, forced_run_id: str) -> dict[str, object]:
    child = repository.child_for_relation(forced_run_id, RELATION_KEY)
    if child is None:
        sets, _ = rejected_identities(repository, forced_run_id)
        return {"exists": False, "candidate_count": len(sets)}
    status = repository.status(child)
    summary = repository.read_json(child, "summary.json")
    specification = repository.load_spec(child)
    return {
        "exists": True,
        "run_id": child,
        "status": status.get("status"),
        "created_at": status.get("created_at"),
        "candidate_count": summary.get(
            "candidate_count",
            len(specification.forced_candidate_identities or ()),
        ),
        "favorable_count": summary.get("favorable_count", 0),
        "unfavorable_count": summary.get("unfavorable_count", 0),
    }


def materialize_diagnostic(
    repository: RunRepository, forced_run_id: str
) -> tuple[str, ExperimentSpec, bool]:
    """Reserve one immutable diagnostic per source run and protocol."""

    if repository.status(forced_run_id).get("status") != JobStatus.COMPLETED.value:
        raise ValueError("La revalidation forcee doit etre terminee")
    existing = repository.child_for_relation(forced_run_id, RELATION_KEY)
    if existing is not None:
        return existing, repository.load_spec(existing), False
    source = repository.load_spec(forced_run_id)
    sets, identities = rejected_identities(repository, forced_run_id)
    if not identities:
        raise ValueError("Aucun rejet WF evaluable")
    wf_id = _source_walk_forward_run(repository, forced_run_id)
    metadata = repository.run_metadata(forced_run_id)
    spec = replace(
        source,
        job_type=JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC,
        forced_symbol_sets=sets,
        forced_candidate_identities=identities,
        source_walk_forward_run=wf_id,
        source_end_to_end_run=(
            metadata.root_run_id or metadata.parent_run_id or forced_run_id
        ),
        source_forced_candidate_validation_run=forced_run_id,
        run_description="Diagnostic holdout des rejets WF",
        diagnostic_protocol=PROTOCOL,
        diagnostic_only=True,
        xgb_recalibration=False,
        threshold_recalibration=False,
        walk_forward_rerun=False,
        prefilter_rerun=False,
        promotion_enabled=False,
        auto_promote_candidates=False,
    )
    run_id = repository.generate_run_id()
    repository.create(
        spec,
        run_id=run_id,
        metadata=RunMetadata(
            run_role=RunRole.QUALIFICATION_HOLDOUT_DIAGNOSTIC,
            run_purpose=RunPurpose.QUALIFICATION_HOLDOUT_DIAGNOSTIC,
            parent_run_id=forced_run_id,
            root_run_id=metadata.root_run_id or metadata.parent_run_id or forced_run_id,
            relation_key=RELATION_KEY,
            relation_type="qualification_holdout_diagnostic",
            stage_key="qualification_holdout_diagnostic",
            reference_run_id=metadata.reference_run_id,
            validation_run_id=metadata.validation_run_id,
        ),
    )
    return run_id, spec, True
