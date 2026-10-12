"""Registered analyses on immutable run artefacts, using the ordinary job system."""
from __future__ import annotations

from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Any, Callable
import hashlib
import json
import shutil
import os
from copy import deepcopy

from rstock.checkpoints import CheckpointManager, _atomic_json
from rstock.progress import check_cancellation, report_progress, CancellationRequested
from .domain import ExperimentSpec, JobType
from .repository import RunRepository


@dataclass(frozen=True)
class AnalysisSource:
    owner_run_id: str
    run_id: str
    directory: Path
    stage: str = "walk_forward"
    artifacts: dict[str, str] = field(default_factory=dict)

    @property
    def windows_path(self): return self.directory / "results/windows.csv"
    @property
    def predictions_path(self): return self.directory / "results/predictions.csv"
    @property
    def prepared_snapshot_path(self): return self.directory / "checkpoints/artifacts/prepared_snapshot.pkl"
    @property
    def prepared_metadata_path(self): return self.directory / "checkpoints/artifacts/prepared_snapshot.json"
    @property
    def configuration_path(self): return self.directory / "config.json"


@dataclass
class AnalysisContext:
    spec: ExperimentSpec
    source: AnalysisSource
    parameters: dict[str, Any]
    output: Path
    checkpoint: CheckpointManager
    progress_callback: Any = None
    cancellation_check: Any = None


@dataclass(frozen=True)
class AnalysisDefinition:
    id: str
    version: str
    label: str
    description: str
    required_artifacts: tuple[str, ...]
    validate_parameters: Callable[[dict], dict]
    preflight: Callable[[AnalysisSource, dict], dict]
    execute: Callable[[AnalysisContext], dict]
    render_parameters: Callable[[str], dict] | None = None
    result_tabs: tuple = ()
    render_results: Callable[[str, str, dict], None] | None = None
    compatible: Callable[[AnalysisSource], bool] | None = None


_REGISTERED: dict[str, AnalysisDefinition] = {}


def register_analysis(definition: AnalysisDefinition) -> None:
    if definition.id in _REGISTERED and _REGISTERED[definition.id] != definition:
        raise ValueError("analysis_definition_already_registered")
    _REGISTERED[definition.id] = definition


def analysis_catalogue() -> tuple[AnalysisDefinition, ...]:
    from .rare_relations_analysis import definition
    builtin = definition()
    if builtin.id not in _REGISTERED:
        register_analysis(builtin)
    return tuple(_REGISTERED.values())


def get_analysis(analysis_id: str) -> AnalysisDefinition:
    for item in analysis_catalogue():
        if item.id == analysis_id:
            return item
    raise ValueError("analysis_not_registered")


def compatible_analyses(repository, source_run_id):
    source = resolve_analysis_source(repository, source_run_id)
    return tuple(item for item in analysis_catalogue() if item.compatible is None or item.compatible(source))


def _safe_id(run_id: str) -> str:
    if not run_id or Path(run_id).name != run_id or run_id in {".", ".."}:
        raise ValueError("invalid_analysis_source")
    return run_id


def resolve_analysis_source(repository: RunRepository, run_id: str) -> AnalysisSource:
    run_id = _safe_id(run_id)
    root = repository.run_directory(run_id)
    status = repository.status(run_id)
    if status.get("status") != "completed":
        raise ValueError("analysis_source_not_completed")
    job = status.get("job_type")
    if job == JobType.END_TO_END.value:
        from .end_to_end import effective_stage_run_id
        path = root / "orchestration/pipeline.json"
        manifest = json.loads(path.read_text(encoding="utf-8"))
        physical = effective_stage_run_id(manifest, "walk_forward")
        if not physical or str(physical) == run_id or repository.status(str(physical)).get("job_type") != JobType.WALK_FORWARD.value:
            raise ValueError("analysis_walk_forward_stage_missing")
        source = resolve_analysis_source(repository, _safe_id(str(physical)))
        return replace(source, owner_run_id=run_id)
    if job != JobType.WALK_FORWARD.value:
        raise ValueError("analysis_source_type_incompatible")
    return AnalysisSource(run_id, run_id, root)


def _artifact(source: AnalysisSource, relative: str) -> Path:
    path = (source.directory / relative).resolve()
    if not path.is_relative_to(source.directory.resolve()):
        raise ValueError("analysis_artifact_outside_source")
    if not path.is_file():
        raise ValueError(f"analysis_artifact_missing: {relative}")
    return path


def digest(path: Path, cancellation_check=None) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(4 * 1024 * 1024), b""):
            check_cancellation(cancellation_check)
            h.update(block)
    return h.hexdigest()


def analysis_preflight(repository: RunRepository, source_run_id: str, analysis_id: str, parameters: dict) -> dict:
    """Validate inputs and resolve metadata; never score predictions or fit models."""
    item = get_analysis(analysis_id)
    source = resolve_analysis_source(repository, source_run_id)
    if item.compatible is not None and not item.compatible(source):
        return {"ready": False, "technical_compatible": False, "scientific_ready": False,
                "reasons": ["Analyse incompatible avec la source sélectionnée."], "parameters": parameters}
    parameters = item.validate_parameters(dict(parameters))
    missing = [name for name in item.required_artifacts if not (source.directory / name).is_file()]
    if missing:
        return {"ready": False, "technical_compatible": True, "technical_ready": False,
                "scientific_ready": False, "reasons": ["Artefacts manquants : " + ", ".join(missing)], "parameters": parameters}
    result = dict(item.preflight(source, parameters))
    result.update(technical_compatible=True, technical_ready=True, scientific_ready=bool(result.get("ready")))
    result["parameters"] = item.validate_parameters(result.pop("resolved_parameters", parameters))
    return result


def prepare_analysis(repository: RunRepository, source_run_id: str, analysis_id: str, parameters: dict, *, request_id: str) -> ExperimentSpec:
    item = get_analysis(analysis_id)
    preflight = analysis_preflight(repository, source_run_id, analysis_id, parameters)
    if not preflight.get("ready"):
        raise ValueError("; ".join(preflight.get("reasons", ["analysis_not_ready"])))
    source = resolve_analysis_source(repository, source_run_id)
    raw = repository.read_json(source.run_id, "config.json")
    original = ExperimentSpec.from_dict(raw)
    names = tuple(dict.fromkeys(("config.json", "status.json", *item.required_artifacts)))
    hashes = {name: digest(_artifact(source, name)) for name in names}
    snapshot_name = "checkpoints/artifacts/prepared_snapshot.pkl"
    metadata_name = "checkpoints/artifacts/prepared_snapshot.json"
    if snapshot_name in hashes and metadata_name in hashes:
        metadata = json.loads(_artifact(source, metadata_name).read_text(encoding="utf-8"))
        if metadata.get("sha256") != hashes[snapshot_name]:
            raise ValueError("analysis_prepared_snapshot_digest_inconsistent")
    owner_manifest = repository.run_directory(source.owner_run_id) / "orchestration/pipeline.json"
    owner_digest = digest(owner_manifest) if source.owner_run_id != source.run_id else None
    if owner_digest:
        from .forward_diagnostic import _stage_expected
        for name, actual in hashes.items():
            expected = _stage_expected(owner_manifest.parent.parent, "walk_forward", name)
            if expected and expected != actual:
                raise ValueError("analysis_inherited_artifact_digest_changed")
    manifest = {"schema_version": 1, "analysis_id": item.id, "definition_version": item.version,
        "owner_run_id": source.owner_run_id, "source_run_id": source.run_id,
        "source_stage": source.stage, "parameters": preflight["parameters"], "source_sha256": hashes,
        "request_id": request_id, "owner_manifest_sha256": owner_digest}
    if not request_id:
        raise ValueError("analysis_request_id_required")
    return ExperimentSpec(job_type=JobType.SCIENTIFIC_ANALYSIS, config=original.config,
        symbols=original.symbols, target_symbols=original.target_symbols,
        context_symbols=original.context_symbols, calendar=original.calendar,
        primary_universe_id=original.primary_universe_id, universe_selection=original.universe_selection,
        source_experiment_run=source.owner_run_id, source_walk_forward_run=source.run_id,
        scientific_analysis=manifest, evaluate_final_holdout=False,
        run_description=item.label)


def validate_analysis_spec(repository: RunRepository, spec: ExperimentSpec, cancellation_check=None) -> tuple[AnalysisDefinition, AnalysisSource]:
    manifest = deepcopy(spec.scientific_analysis)
    if not isinstance(manifest, dict):
        raise ValueError("analysis_manifest_missing")
    item = get_analysis(manifest["analysis_id"])
    if item.version != manifest["definition_version"]:
        raise ValueError("analysis_definition_version_changed")
    source = resolve_analysis_source(repository, manifest["owner_run_id"])
    if source.run_id != manifest["source_run_id"]:
        raise ValueError("analysis_physical_source_changed")
    if manifest.get("owner_manifest_sha256"):
        path = repository.run_directory(source.owner_run_id) / "orchestration/pipeline.json"
        if digest(path, cancellation_check) != manifest["owner_manifest_sha256"]:
            raise ValueError("analysis_owner_manifest_changed")
    preflight = analysis_preflight(repository, source.owner_run_id, item.id, manifest["parameters"])
    if not preflight.get("ready") or preflight["parameters"] != manifest["parameters"]:
        raise ValueError("analysis_parameters_not_ready_or_changed")
    required = set(("config.json", "status.json", *item.required_artifacts))
    if set(manifest["source_sha256"]) != required:
        raise ValueError("analysis_source_contract_incomplete")
    for name, expected in manifest["source_sha256"].items():
        if digest(_artifact(source, name), cancellation_check) != expected:
            raise ValueError(f"analysis_source_digest_changed: {name}")
    if manifest.get("source_stage") != "walk_forward" or spec.source_walk_forward_run != source.run_id or spec.source_experiment_run != source.owner_run_id:
        raise ValueError("analysis_source_reference_inconsistent")
    return item, replace(source, artifacts=manifest["source_sha256"])


def _output_path(directory, relative):
    path = (directory / relative).resolve()
    if not path.is_relative_to(directory.resolve()) or not path.is_file():
        raise ValueError("analysis_output_missing_or_outside_run")
    return path


def _copy_output(path, destination):
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + ".tmp")
    shutil.copyfile(path, temporary)
    os.replace(temporary, destination)


def _freeze_outputs(context, result):
    paths = result.get("result_files", [])
    store = context.checkpoint.root / "analysis_outputs"
    manifest = {}
    for relative in paths:
        source = _output_path(context.output, relative)
        destination = store / relative
        if not destination.resolve().is_relative_to(store.resolve()):
            raise ValueError("analysis_output_outside_checkpoint")
        _copy_output(source, destination)
        manifest[relative] = digest(destination, context.cancellation_check)
    context.checkpoint.commit_artifact("analysis_outputs", manifest)


def _restore_outputs(context):
    manifest = context.checkpoint.load_artifact("analysis_outputs")
    store = context.checkpoint.root / "analysis_outputs"
    for relative, expected in manifest.items():
        source = _output_path(store, relative)
        if digest(source, context.cancellation_check) != expected:
            raise ValueError("analysis_checkpoint_output_digest_changed")
        destination = context.output / relative
        if not destination.resolve().is_relative_to(context.output.resolve()):
            raise ValueError("analysis_output_outside_run")
        _copy_output(source, destination)


def validate_analysis_publication(run: Path):
    """Worker recovery verifies persisted output hashes, not just file presence."""
    repository = RunRepository(run.parent)
    spec = repository.load_spec(run.name)
    validate_analysis_spec(repository, spec)
    checkpoint = CheckpointManager(run, run_id=run.name, job_type=spec.job_type.value,
        configuration_fingerprint=spec.fingerprint, batch_sizes={"scientific_analysis":1})
    if not checkpoint.phase_is_completed("analysis"):
        raise ValueError("analysis_publication_incomplete_checkpoint")
    for relative, expected in checkpoint.load_artifact("analysis_outputs").items():
        if digest(_output_path(run / "results", relative)) != expected:
            raise ValueError("analysis_published_output_digest_changed")
    if json.loads((run/"results/analysis_manifest.json").read_text()) != spec.scientific_analysis:
        raise ValueError("analysis_published_manifest_changed")
    if json.loads((run/"results/analysis_result.json").read_text()) != checkpoint.load_artifact("analysis_result"):
        raise ValueError("analysis_published_result_changed")


def execute_analysis(spec, output, progress_callback, cancellation_check):
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    repository = RunRepository(Path(spec.config.project_root) / "runs")
    item, source = validate_analysis_spec(repository, spec, cancellation_check)
    checkpoint = CheckpointManager(output.parent, run_id=output.parent.name,
        job_type=spec.job_type.value, configuration_fingerprint=spec.fingerprint,
        batch_sizes={"scientific_analysis": 1})
    checkpoint.start_attempt(resumed=int(checkpoint.manifest.get("attempt_count", 0)) > 0)
    manifest_path = output / "analysis_manifest.json"
    manifest = deepcopy(spec.scientific_analysis)
    if manifest_path.exists() and json.loads(manifest_path.read_text()) != manifest:
        raise ValueError("analysis_manifest_changed")
    _atomic_json(manifest_path, manifest)
    context = AnalysisContext(spec, source, deepcopy(manifest["parameters"]), output, checkpoint, progress_callback, cancellation_check)
    report_progress(progress_callback, "scientific_analysis", completed_units=0, total_units=1)
    try:
        if checkpoint.phase_is_completed("analysis"):
            result = checkpoint.load_artifact("analysis_result")
            _restore_outputs(context)
        else:
            checkpoint.phase_started("analysis")
            result = item.execute(context)
            check_cancellation(cancellation_check)
            if spec.scientific_analysis != manifest:
                raise ValueError("analysis_frozen_parameters_mutated")
            validate_analysis_spec(repository, spec, cancellation_check)
            _freeze_outputs(context, result)
            checkpoint.commit_artifact("analysis_result", result)
            checkpoint.phase_completed("analysis")
        _atomic_json(output / "analysis_result.json", result)
    except Exception as error:
        checkpoint.finish_attempt("cancelled" if isinstance(error, CancellationRequested) else "failed", str(error))
        raise
    checkpoint.finish_attempt("completed")
    report_progress(progress_callback, "scientific_analysis", completed_units=1, total_units=1)
    return result
