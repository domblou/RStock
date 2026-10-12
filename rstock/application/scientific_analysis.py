"""Registered analyses on immutable run artefacts, using the ordinary job system."""
from __future__ import annotations

from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Any, Callable
import hashlib
import json
import os
import time
from contextlib import contextmanager
import pandas as pd
from copy import deepcopy

from rstock.checkpoints import CheckpointManager, _atomic_json
from rstock.progress import check_cancellation, report_progress, CancellationRequested
from .domain import ExperimentSpec, JobType
from .repository import RunRepository


RARE_ANALYSIS_PHASES = [("analysis_preparation", 20), ("analysis_reference", 15),
    ("analysis_scoring", 25), ("analysis_selection", 15), ("analysis_evaluation", 10),
    ("analysis_export", 5), ("analysis_outputs", 5), ("publishing", 5)]


@contextmanager
def analysis_phase(callback, name):
    """Use the ordinary reporter lifecycle; failed/cancelled phases stay open."""
    report_progress(callback, name, details={"phase_event": "started"})
    yield
    report_progress(callback, name, details={"phase_event": "completed"})


class AnalysisProgress:
    """Bounded progress writes with exact first/last counts for real work units."""
    def __init__(self, callback, stage, total, unit, substage=None):
        self.callback, self.stage, self.total = callback, stage, total
        self.unit, self.substage = unit, substage
        self.last = float("-inf")
        self.update(0)

    def update(self, completed):
        now = time.monotonic()
        if completed == self.total or now - self.last >= 1.:
            report_progress(self.callback, self.stage, substage=self.substage,
                completed_units=completed, total_units=self.total, details={"unit": self.unit})
            self.last = now


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
    interpret_results: Callable[[dict | None, dict, dict], dict]
    render_parameters: Callable[[str], dict] | None = None
    result_tabs: tuple = ()
    render_results: Callable[[str, str, dict], None] | None = None
    compatible: Callable[[AnalysisSource], bool] | None = None
    light_preflight: Callable[[AnalysisSource, dict], dict] | None = None
    blocking_reasons: tuple[str, ...] = ()
    worker_prepare: Callable[[AnalysisSource, dict, Any, Any], dict] | None = None


_REGISTERED: dict[str, AnalysisDefinition] = {}


def register_analysis(definition: AnalysisDefinition) -> None:
    if not callable(definition.interpret_results):
        raise ValueError("analysis_scientific_interpretation_required")
    if definition.id in _REGISTERED and _REGISTERED[definition.id] != definition:
        raise ValueError("analysis_definition_already_registered")
    _REGISTERED[definition.id] = definition


def interpretation_inputs_hash(result, manifest, status):
    payload = {"result": {k: v for k, v in (result or {}).items() if k != "result_files"},
               "manifest": {k: manifest.get(k) for k in ("analysis_id", "definition_version", "parameters", "source_run_id", "source_sha256")},
               "status": status.get("status")}
    return hashlib.sha256(json.dumps(payload, sort_keys=True, ensure_ascii=False, allow_nan=False).encode()).hexdigest()


def validate_interpretation(value):
    if not isinstance(value, dict) or not isinstance(value.get("version"), str) or not isinstance(value.get("verdict"), str) or not value["verdict"]:
        raise ValueError("analysis_interpretation_invalid")
    for key in ("understanding", "findings", "limits"):
        if not isinstance(value.get(key), list) or not value[key] or not all(isinstance(text, str) and text for text in value[key]):
            raise ValueError("analysis_interpretation_invalid: " + key)
    return value


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


def digest(path: Path, cancellation_check=None, *, on_bytes=None) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(4 * 1024 * 1024), b""):
            check_cancellation(cancellation_check)
            h.update(block)
            if on_bytes is not None:
                on_bytes(len(block))
    return h.hexdigest()


def worker_preflight(repository: RunRepository, source_run_id: str, analysis_id: str, parameters: dict, cancellation_check=None, progress_callback=None) -> dict:
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
    check_cancellation(cancellation_check)
    result = dict(item.worker_prepare(source, parameters, cancellation_check, progress_callback) if item.worker_prepare else item.preflight(source, parameters))
    result.update(technical_compatible=True, technical_ready=True, scientific_ready=bool(result.get("ready")))
    result["parameters"] = item.validate_parameters(result.pop("resolved_parameters", parameters))
    return result


def analysis_preflight(repository, source_run_id, analysis_id, parameters):
    """UI contract: metadata/stat only, never call the deep plugin preflight."""
    item = get_analysis(analysis_id)
    source = resolve_analysis_source(repository, source_run_id)
    parameters = item.validate_parameters(dict(parameters))
    compatible = item.compatible is None or item.compatible(source)
    missing = [name for name in item.required_artifacts if not (source.directory / name).is_file()]
    result = dict(item.light_preflight(source, parameters)) if item.light_preflight else {"ready": True}
    reasons = list(item.blocking_reasons)
    if not compatible:
        reasons.append("Analyse incompatible avec la source sélectionnée.")
    if missing:
        reasons.append("Artefacts manquants : " + ", ".join(missing))
    result["reasons"] = [*result.get("reasons", []), *reasons]
    result.update(ready=bool(result.get("ready")) and not reasons,
                  scientific_ready=bool(result.get("ready")) and not item.blocking_reasons,
                  technical_compatible=compatible, technical_ready=not missing,
                  parameters=parameters)
    return result


def _file_identity(path):
    stat = path.stat()
    return [stat.st_size, stat.st_mtime_ns, stat.st_ctime_ns, stat.st_ino]


def validate_analysis_request(repository, spec):
    manifest = spec.scientific_analysis
    item = get_analysis(manifest["analysis_id"])
    if item.version != manifest["definition_version"]:
        raise ValueError("analysis_definition_version_changed")
    source = resolve_analysis_source(repository, manifest["owner_run_id"])
    if source.run_id != manifest["source_run_id"]:
        raise ValueError("analysis_physical_source_changed")
    if manifest.get("source_stage") != "walk_forward" or spec.source_walk_forward_run != source.run_id or spec.source_experiment_run != source.owner_run_id:
        raise ValueError("analysis_source_reference_inconsistent")
    result = analysis_preflight(repository, source.owner_run_id, item.id, manifest["parameters"])
    if not result["ready"]:
        raise ValueError("; ".join(result["reasons"]))
    return item, source


def prepare_analysis(repository: RunRepository, source_run_id: str, analysis_id: str, parameters: dict, *, request_id: str) -> ExperimentSpec:
    item = get_analysis(analysis_id)
    preflight = analysis_preflight(repository, source_run_id, analysis_id, parameters)
    if not preflight.get("ready"):
        raise ValueError("; ".join(preflight.get("reasons", ["analysis_not_ready"])))
    source = resolve_analysis_source(repository, source_run_id)
    raw = repository.read_json(source.run_id, "config.json")
    original = ExperimentSpec.from_dict(raw)
    names = tuple(dict.fromkeys(("config.json", "status.json", *item.required_artifacts)))
    identities = {name: _file_identity(_artifact(source, name)) for name in names}
    owner_manifest = repository.run_directory(source.owner_run_id) / "orchestration/pipeline.json"
    manifest = {"schema_version": 2, "analysis_id": item.id, "definition_version": item.version,
        "owner_run_id": source.owner_run_id, "source_run_id": source.run_id,
        "source_stage": source.stage, "parameters": preflight["parameters"],
        "source_identities": identities, "request_id": request_id,
        "owner_manifest_identity": _file_identity(owner_manifest) if source.owner_run_id != source.run_id else None}
    if not request_id:
        raise ValueError("analysis_request_id_required")
    return ExperimentSpec(job_type=JobType.SCIENTIFIC_ANALYSIS, config=original.config,
        symbols=original.symbols, target_symbols=original.target_symbols,
        context_symbols=original.context_symbols, calendar=original.calendar,
        primary_universe_id=original.primary_universe_id, universe_selection=original.universe_selection,
        source_experiment_run=source.owner_run_id, source_walk_forward_run=source.run_id,
        scientific_analysis=manifest, evaluate_final_holdout=False,
        run_description=item.label)


def _hash_files(files, cancellation_check, callback, stage):
    progress = AnalysisProgress(callback, stage, sum(path.stat().st_size for path in files.values()), "octets", "Empreintes source")
    completed = 0
    def update(size):
        nonlocal completed
        completed += size
        progress.update(completed)
    return {name: digest(path, cancellation_check, on_bytes=update) for name, path in files.items()}


def _worker_contract(repository, spec, checkpoint, cancellation_check=None, progress_callback=None):
    request = spec.scientific_analysis
    if request.get("schema_version", 1) == 1:
        return deepcopy(request)
    if checkpoint.phase_is_completed("preparation"):
        return checkpoint.load_artifact("prepared_contract")
    item, source = validate_analysis_request(repository, spec)
    checkpoint.phase_started("preparation")
    identities = request["source_identities"]
    if set(identities) != set(("config.json", "status.json", *item.required_artifacts)):
        raise ValueError("analysis_source_contract_incomplete")
    owner = repository.run_directory(source.owner_run_id) / "orchestration/pipeline.json"
    if request.get("owner_manifest_identity") and _file_identity(owner) != request["owner_manifest_identity"]:
        raise ValueError("analysis_owner_manifest_changed")
    for name, expected in identities.items():
        if _file_identity(_artifact(source, name)) != expected:
            raise ValueError("analysis_source_identity_changed: " + name)
    hashes = _hash_files({name: _artifact(source, name) for name in identities}, cancellation_check, progress_callback, "analysis_preparation")
    preflight = worker_preflight(repository, source.owner_run_id, item.id, request["parameters"], cancellation_check, progress_callback)
    check_cancellation(cancellation_check)
    if not preflight.get("ready"):
        raise ValueError("; ".join(preflight.get("reasons", ["analysis_not_ready"])))
    for name, expected in identities.items():
        if _file_identity(_artifact(source, name)) != expected:
            raise ValueError("analysis_source_changed_during_preparation: " + name)
    snapshot = "checkpoints/artifacts/prepared_snapshot.pkl"
    metadata = "checkpoints/artifacts/prepared_snapshot.json"
    if snapshot in hashes and metadata in hashes:
        if json.loads(_artifact(source, metadata).read_text(encoding="utf-8"))["sha256"] != hashes[snapshot]:
            raise ValueError("analysis_prepared_snapshot_digest_inconsistent")
    owner_hash = digest(owner, cancellation_check) if request.get("owner_manifest_identity") else None
    if owner_hash:
        if _file_identity(owner) != request["owner_manifest_identity"]:
            raise ValueError("analysis_owner_manifest_changed")
        from .forward_diagnostic import _stage_expected
        for name, actual in hashes.items():
            expected = _stage_expected(owner.parent.parent, "walk_forward", name)
            if expected and expected != actual:
                raise ValueError("analysis_inherited_artifact_digest_changed")
    contract = {**deepcopy(request), "schema_version": 1, "source_sha256": hashes,
                "owner_manifest_sha256": owner_hash, "parameters": preflight["parameters"],
                "worker_prepared": True}
    checkpoint.commit_artifact("prepared_contract", contract)
    checkpoint.phase_completed("preparation")
    return contract


def validate_analysis_spec(repository: RunRepository, spec: ExperimentSpec, cancellation_check=None, progress_callback=None, progress_stage="analysis_preparation") -> tuple[AnalysisDefinition, AnalysisSource]:
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
    preflight = ({"ready": not item.blocking_reasons, "parameters": item.validate_parameters(dict(manifest["parameters"]))}
                 if manifest.get("worker_prepared") else
                 worker_preflight(repository, source.owner_run_id, item.id, manifest["parameters"], cancellation_check))
    if not preflight.get("ready") or preflight["parameters"] != manifest["parameters"]:
        raise ValueError("analysis_parameters_not_ready_or_changed")
    required = set(("config.json", "status.json", *item.required_artifacts))
    if set(manifest["source_sha256"]) != required:
        raise ValueError("analysis_source_contract_incomplete")
    actual_hashes = _hash_files({name: _artifact(source, name) for name in manifest["source_sha256"]}, cancellation_check, progress_callback, progress_stage)
    for name, expected in manifest["source_sha256"].items():
        if actual_hashes[name] != expected:
            raise ValueError(f"analysis_source_digest_changed: {name}")
    if manifest.get("source_stage") != "walk_forward" or spec.source_walk_forward_run != source.run_id or spec.source_experiment_run != source.owner_run_id:
        raise ValueError("analysis_source_reference_inconsistent")
    return item, replace(source, artifacts=manifest["source_sha256"])


def _output_path(directory, relative):
    path = (directory / relative).resolve()
    if not path.is_relative_to(directory.resolve()) or not path.is_file():
        raise ValueError("analysis_output_missing_or_outside_run")
    return path


def _copy_output(path, destination, cancellation_check=None, on_bytes=None):
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + ".tmp")
    with path.open("rb") as source, temporary.open("wb") as target:
        for block in iter(lambda: source.read(4 * 1024 * 1024), b""):
            check_cancellation(cancellation_check)
            target.write(block)
            if on_bytes is not None:
                on_bytes(len(block))
    os.replace(temporary, destination)


def _prepare_display_outputs(context, result):
    """Worker-only bounded previews; the UI never scans exported CSVs."""
    previews = []
    csv_files = [relative for relative in result.get("result_files", []) if Path(relative).suffix == ".csv"]
    progress = AnalysisProgress(context.progress_callback, "analysis_outputs", len(csv_files), "fichiers", "Aperçus bornés")
    for relative in list(result.get("result_files", [])):
        if Path(relative).suffix != ".csv":
            continue
        check_cancellation(context.cancellation_check)
        path = _output_path(context.output, relative)
        data = pd.read_csv(path, nrows=500)
        payload = json.loads(data.to_json(orient="split", date_format="iso"))
        preview = relative + ".preview.json"
        if len(json.dumps(payload).encode("utf-8")) > 2 * 1024 * 1024:
            payload = {"columns": list(data.columns), "data": [], "index": []}
        payload["row_limit"] = 500
        _atomic_json(context.output / preview, payload)
        previews.append(preview)
        progress.update(len(previews))
    result["result_files"] = [*result.get("result_files", []), *previews]


def _freeze_outputs(context, result):
    paths = result.get("result_files", [])
    store = context.checkpoint.root / "analysis_outputs"
    manifest = {}
    progress = AnalysisProgress(context.progress_callback, "analysis_outputs",
        2 * sum(_output_path(context.output, relative).stat().st_size for relative in paths), "octets", "Gel des sorties")
    completed = 0
    def update(size):
        nonlocal completed
        completed += size
        progress.update(completed)
    for relative in paths:
        source = _output_path(context.output, relative)
        destination = store / relative
        if not destination.resolve().is_relative_to(store.resolve()):
            raise ValueError("analysis_output_outside_checkpoint")
        _copy_output(source, destination, context.cancellation_check, update)
        manifest[relative] = digest(destination, context.cancellation_check, on_bytes=update)
    context.checkpoint.commit_artifact("analysis_outputs", manifest)


def _restore_outputs(context):
    manifest = context.checkpoint.load_artifact("analysis_outputs")
    store = context.checkpoint.root / "analysis_outputs"
    progress = AnalysisProgress(context.progress_callback, "analysis_restore",
        2 * sum(_output_path(store, relative).stat().st_size for relative in manifest), "octets", "Restauration des sorties figées")
    completed = 0
    def update(size):
        nonlocal completed
        completed += size
        progress.update(completed)
    for relative, expected in manifest.items():
        source = _output_path(store, relative)
        if digest(source, context.cancellation_check, on_bytes=update) != expected:
            raise ValueError("analysis_checkpoint_output_digest_changed")
        destination = context.output / relative
        if not destination.resolve().is_relative_to(context.output.resolve()):
            raise ValueError("analysis_output_outside_run")
        _copy_output(source, destination, context.cancellation_check, update)


def validate_analysis_publication(run: Path):
    """Worker recovery verifies persisted output hashes, not just file presence."""
    repository = RunRepository(run.parent)
    spec = repository.load_spec(run.name)
    checkpoint = CheckpointManager(run, run_id=run.name, job_type=spec.job_type.value,
        configuration_fingerprint=spec.fingerprint, batch_sizes={"scientific_analysis":1})
    contract = _worker_contract(repository, spec, checkpoint)
    validate_analysis_spec(repository, replace(spec, scientific_analysis=contract))
    if not checkpoint.phase_is_completed("analysis"):
        raise ValueError("analysis_publication_incomplete_checkpoint")
    for relative, expected in checkpoint.load_artifact("analysis_outputs").items():
        if digest(_output_path(run / "results", relative)) != expected:
            raise ValueError("analysis_published_output_digest_changed")
    if spec.scientific_analysis.get("schema_version", 1) >= 2:
        if json.loads((run/"results/analysis_preparation.json").read_text()) != contract:
            raise ValueError("analysis_published_preparation_changed")
    if json.loads((run/"results/analysis_manifest.json").read_text()) != spec.scientific_analysis:
        raise ValueError("analysis_published_manifest_changed")
    if json.loads((run/"results/analysis_result.json").read_text()) != checkpoint.load_artifact("analysis_result"):
        raise ValueError("analysis_published_result_changed")


def execute_analysis(spec, output, progress_callback, cancellation_check):
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    repository = RunRepository(Path(spec.config.project_root) / "runs")
    checkpoint = CheckpointManager(output.parent, run_id=output.parent.name,
        job_type=spec.job_type.value, configuration_fingerprint=spec.fingerprint,
        batch_sizes={"scientific_analysis": 1})
    checkpoint.start_attempt(resumed=int(checkpoint.manifest.get("attempt_count", 0)) > 0)
    manifest_path = output / "analysis_manifest.json"
    manifest = deepcopy(spec.scientific_analysis)
    if manifest_path.exists() and json.loads(manifest_path.read_text()) != manifest:
        raise ValueError("analysis_manifest_changed")
    _atomic_json(manifest_path, manifest)
    try:
        restored = checkpoint.phase_is_completed("analysis")
        phases = ([("analysis_preparation", 20), ("analysis_restore", 70), ("analysis_outputs", 5), ("publishing", 5)] if restored else
                  RARE_ANALYSIS_PHASES if manifest["analysis_id"] == "rare_relations_persistence" else
                  [("analysis_preparation", 20), ("scientific_analysis", 70), ("analysis_outputs", 5), ("publishing", 5)])
        if hasattr(progress_callback, "configure_phases"):
            progress_callback.configure_phases(phases)
        with analysis_phase(progress_callback, "analysis_preparation"):
            contract = _worker_contract(repository, spec, checkpoint, cancellation_check, progress_callback)
            resolved_spec = replace(spec, scientific_analysis=contract)
            item, source = validate_analysis_spec(repository, resolved_spec, cancellation_check, progress_callback)
            _atomic_json(output / "analysis_preparation.json", contract)
            context = AnalysisContext(resolved_spec, source, deepcopy(contract["parameters"]), output, checkpoint, progress_callback, cancellation_check)
        if checkpoint.phase_is_completed("analysis"):
            with analysis_phase(progress_callback, "analysis_restore"):
                result = checkpoint.load_artifact("analysis_result")
                _restore_outputs(context)
            with analysis_phase(progress_callback, "analysis_outputs"):
                _atomic_json(output / "analysis_result.json", result)
        else:
            checkpoint.phase_started("analysis")
            if item.id == "rare_relations_persistence":
                result = item.execute(context)
            else:
                with analysis_phase(progress_callback, "scientific_analysis"):
                    result = item.execute(context)
            check_cancellation(cancellation_check)
            if resolved_spec.scientific_analysis != contract:
                raise ValueError("analysis_frozen_parameters_mutated")
            with analysis_phase(progress_callback, "analysis_outputs"):
                validate_analysis_spec(repository, resolved_spec, cancellation_check, progress_callback, "analysis_outputs")
                result["result_files"] = [*result.get("result_files", []), "analysis_preparation.json"]
                interpretation = validate_interpretation(item.interpret_results(result, contract, {"status": "completed"}))
                _atomic_json(output / "scientific_interpretation.json", {
                    **interpretation, "input_sha256": interpretation_inputs_hash(result, contract, {"status": "completed"})})
                result["result_files"].append("scientific_interpretation.json")
                _prepare_display_outputs(context, result)
                _freeze_outputs(context, result)
                checkpoint.commit_artifact("analysis_result", result)
                checkpoint.phase_completed("analysis")
                _atomic_json(output / "analysis_result.json", result)
    except Exception as error:
        checkpoint.finish_attempt("cancelled" if isinstance(error, CancellationRequested) else "failed", str(error))
        raise
    checkpoint.finish_attempt("completed")
    return result
