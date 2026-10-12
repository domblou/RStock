"""Tiny persisted fixtures only: no archived production data or scientific fits."""
from dataclasses import replace
import json
from pathlib import Path

import pytest

from rstock.config import RStockConfig
from rstock.application.domain import ExperimentSpec, JobType, JobStatus
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.services import ExperimentService
from rstock.application.workflows import WorkflowRegistry
from rstock.application.run_detail_tabs import tabs_for_job
from rstock.application import scientific_analysis as analysis


class Backend:
    def __init__(self): self.calls = []
    def launch(self, *args): self.calls.append(args); return None


def test_progress_throttle_keeps_initial_final_and_periodic_real_counts(monkeypatch):
    now = [0.]
    monkeypatch.setattr(analysis.time, "monotonic", lambda: now[0])
    events = []
    progress = analysis.AnalysisProgress(events.append, "analysis_scoring", 100, "lignes")
    progress.update(1)
    now[0] = 1.1
    progress.update(40)
    progress.update(41)
    progress.update(100)
    assert [event.completed_units for event in events] == [0, 40, 100]


def test_cancelled_phase_keeps_duration_and_resources_without_false_completion(tmp_path):
    from rstock.application.resource_telemetry import ResourceRecorder
    from rstock.progress import CancellationRequested
    recorder = ResourceRecorder(tmp_path, "synthetic", {})
    events = []
    def callback(event):
        events.append(event)
        if event.details.get("phase_event") == "started":
            recorder.phase_started(event.stage)
    try:
        with pytest.raises(CancellationRequested):
            with analysis.analysis_phase(callback, "analysis_scoring"):
                raise CancellationRequested("synthetic cancellation")
    finally:
        recorder.close("cancelled")
    row = json.loads(recorder.summary_path.read_text(encoding="utf-8"))["attempts"][0]["phase_rows"][0]
    assert row["status"] == "cancelled" and row["duration_seconds"] >= 0
    assert row["finished_at"] is not None
    assert [event.details["phase_event"] for event in events] == ["started"]


@pytest.fixture
def setup(tmp_path, monkeypatch):
    repo = RunRepository(tmp_path / "runs")
    backend = Backend(); service = ExperimentService(RunService(repo, backend=backend))
    source = repo.create(ExperimentSpec(JobType.WALK_FORWARD, RStockConfig(project_root=tmp_path), ("AAA", "BBB")))
    repo.transition(source, JobStatus.RUNNING)
    repo.transition(source, JobStatus.COMPLETED)
    (repo.run_directory(source)/"results").mkdir()
    repo.write_json(source, "results/tiny.json", {"value": 2})
    def execute(ctx):
        ctx.checkpoint.commit_artifact("specialized_result", {"value":2})
        (ctx.output / "specialized.csv").write_text("value\n2\n", encoding="utf-8")
        return {"value":2, "complete":True, "result_files":["specialized.csv"]}
    item = analysis.AnalysisDefinition("tiny", "1", "Analyse fixture", "Tiny", ("results/tiny.json",),
        lambda p: p, lambda s,p:{"ready":p.get("inputs_ready") is True,"reasons":[] if p.get("inputs_ready") else ["requirements_missing"]},execute,
        interpret_results=lambda *a: {"version":"1", "understanding":["Fixture"], "findings":["Valeur 2"], "limits":["Synthétique"], "verdict":"Technique uniquement"},
        light_preflight=lambda s,p:{"ready":p.get("inputs_ready") is True,"reasons":[] if p.get("inputs_ready") else ["requirements_missing"]})
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda:(item,))
    return repo,service,source,item,backend


def test_missing_rules_block_before_digest_and_submission(setup, monkeypatch):
    repo,service,source,_,backend = setup
    monkeypatch.setattr(analysis,"digest",lambda *a,**k:pytest.fail("No heavy hash before requirements are met"))
    assert service.analysis_preflight(source,"tiny",{})["ready"] is False
    with pytest.raises(ValueError,match="requirements_missing"):
        service.start_analysis(source,"tiny",{},request_id="one")
    assert backend.calls==[] and len(repo.list_runs())==1


def test_submit_duplicate_independent_run_and_persisted_results(setup):
    repo,service,source,_,backend=setup
    original=(repo.run_directory(source)/"results/tiny.json").read_bytes()
    result=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    spec=repo.load_spec(result.run_id)
    assert ExperimentSpec.from_dict(spec.to_dict()).fingerprint==spec.fingerprint
    output=repo.run_directory(result.run_id)/"results"
    output.mkdir()
    WorkflowRegistry.production().execute(spec,output,progress_callback=None,cancellation_check=None)
    repo.transition(result.run_id,JobStatus.RUNNING); repo.transition(result.run_id,JobStatus.COMPLETED)
    duplicate=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    assert duplicate.run_id==result.run_id and not duplicate.created
    second=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="two")
    assert second.run_id!=result.run_id and len(backend.calls)==2
    assert json.loads((output/"analysis_result.json").read_text())["complete"]
    assert (repo.run_directory(source)/"results/tiny.json").read_bytes()==original
    assert len(service.analyses_for_source(source))==2


def test_changed_source_and_definition_block_resume(setup, monkeypatch):
    repo,service,source,item,backend=setup
    job=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    repo.transition(job.run_id,JobStatus.FAILED,error="fixture interruption")
    repo.write_json(source,"results/tiny.json",{"value":3})
    service.run_service.resume(job.run_id)
    with pytest.raises(ValueError,match="identity_changed"):
        analysis.execute_analysis(repo.load_spec(job.run_id), repo.run_directory(job.run_id)/"results", None, None)
    assert len(backend.calls)==2
    repo.transition(job.run_id, JobStatus.FAILED, error="changed source")
    repo.write_json(source,"results/tiny.json",{"value":2})
    monkeypatch.setattr(analysis,"analysis_catalogue",lambda:(replace(item,version="2"),))
    with pytest.raises(ValueError,match="version_changed"):
        service.run_service.resume(job.run_id)


def test_completed_checkpoint_reuses_result_after_publication_interruption(setup, monkeypatch):
    repo,service,source,item,backend=setup
    job=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    spec=repo.load_spec(job.run_id); output=repo.run_directory(job.run_id)/"results"
    output.mkdir()
    original=analysis._atomic_json
    def interrupted(path,values):
        if path.name=="analysis_result.json":raise RuntimeError("publication interruption")
        return original(path,values)
    monkeypatch.setattr(analysis,"_atomic_json",interrupted)
    with pytest.raises(RuntimeError,match="publication interruption"):
        analysis.execute_analysis(spec,output,None,None)
    monkeypatch.setattr(analysis,"_atomic_json",original)
    monkeypatch.setattr(analysis,"analysis_catalogue",lambda:(replace(item,execute=lambda ctx:pytest.fail("must reuse checkpoint")),))
    resumed_output=repo.run_directory(job.run_id)/"_new_working"
    assert analysis.execute_analysis(spec,resumed_output,None,None)["value"]==2
    assert (resumed_output/"specialized.csv").read_text()=="value\n2\n"


def test_source_types_and_common_tabs(setup):
    repo,service,source,item,backend=setup
    parent=repo.create(ExperimentSpec(JobType.END_TO_END,RStockConfig(project_root=repo.root.parent),("AAA","BBB")))
    repo.transition(parent,JobStatus.RUNNING);repo.transition(parent,JobStatus.COMPLETED)
    (repo.run_directory(parent)/"orchestration").mkdir()
    repo.write_json(parent,"orchestration/pipeline.json",{"schema_version":4,"stages":[{
        "stage_key":"walk_forward","mode":"inherited","source_run_id":source,"status":"completed"}]})
    resolved=analysis.resolve_analysis_source(repo,parent)
    assert resolved.owner_run_id==parent and resolved.run_id==source
    assert resolved.predictions_path==repo.run_directory(source)/"results/predictions.csv"
    assert [x.key for x in tabs_for_job(JobType.SCIENTIFIC_ANALYSIS)]==["summary","technical"]
    with pytest.raises(ValueError,match="invalid_analysis_source"):
        analysis.resolve_analysis_source(repo,"../unsafe")


def test_specialized_tabs_are_lazy_and_have_distinct_renderers(setup, monkeypatch):
    from rstock.application import scientific_analysis_ui as ui
    from rstock.application.run_detail_tabs import RunTabDefinition
    repo,service,source,item,backend=setup
    calls=[]; captured={}
    tabs=tuple(RunTabDefinition(key,key,frozenset({JobType.SCIENTIFIC_ANALYSIS}),"shared","fixture") for key in ["first","second"])
    plugin=replace(item,result_tabs=tabs,render_results=lambda key,run_id,detail:calls.append((key,detail["run_directory"])))
    monkeypatch.setattr(ui,"get_analysis",lambda *_:plugin)
    monkeypatch.setattr(ui,"render_lazy_tabs",lambda st,tabs,renderers,**kw:captured.update(tabs=tabs,renderers=renderers))
    detail={"configuration":{"scientific_analysis":{"analysis_id":"tiny","owner_run_id":source}}}
    ui.render_analysis_detail(object(),service,source,{"status":"completed"},detail,
        render_technical=lambda *a, **kw: None,navigate=lambda *_:None)
    assert [tab.key for tab in captured["tabs"]] == ["summary", "first", "second", "technical"]
    assert calls==[]
    captured["renderers"]["first"](); captured["renderers"]["second"]()
    assert [x[0] for x in calls]==["first","second"]
    assert all(x[1]==str(repo.run_directory(source)) for x in calls)


def test_worker_staging_contains_outputs_not_source_results(setup):
    repo,service,source,item,backend=setup
    job=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    spec=repo.load_spec(job.run_id); working=repo.run_directory(job.run_id)/".results.working"
    assert analysis.execute_analysis(spec,working,None,None)["complete"]
    assert (working/"analysis_manifest.json").is_file() and (working/"analysis_result.json").is_file()
    assert not (repo.run_directory(job.run_id)/"results").exists()


def test_technical_readiness_is_separate_from_science_and_catalogue_compatibility(setup, monkeypatch):
    repo,service,source,item,backend=setup
    ready=service.analysis_preflight(source,"tiny",{})
    assert ready["technical_compatible"] and ready["technical_ready"]
    assert not ready["scientific_ready"] and not ready["ready"]
    monkeypatch.setattr(analysis,"analysis_catalogue",lambda:(replace(item,compatible=lambda s:False),))
    assert analysis.compatible_analyses(repo,source)==()
    assert service.analysis_preflight(source,"tiny",{})["technical_compatible"] is False


def test_output_corruption_stops_republication_without_recalculation(setup, monkeypatch):
    repo,service,source,item,backend=setup
    job=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    spec=repo.load_spec(job.run_id); output=repo.run_directory(job.run_id)/"_working"
    analysis.execute_analysis(spec,output,None,None)
    (repo.run_directory(job.run_id)/"checkpoints/analysis_outputs/specialized.csv").write_text("corrupted")
    monkeypatch.setattr(analysis,"analysis_catalogue",lambda:(replace(item,execute=lambda ctx:pytest.fail("no recalculation")),))
    with pytest.raises(ValueError,match="output_digest_changed"):
        analysis.execute_analysis(spec,repo.run_directory(job.run_id)/"_resumed",None,None)


def test_worker_publication_recovery_checks_output_integrity(setup):
    from rstock.application.worker import _validate_publication_recovery
    repo,service,source,item,backend=setup
    job=service.start_analysis(source,"tiny",{"inputs_ready":True},request_id="one")
    spec=repo.load_spec(job.run_id); run=repo.run_directory(job.run_id); results=run/"results"
    result=analysis.execute_analysis(spec,results,None,None)
    _validate_publication_recovery(run,result)
    (results/"specialized.csv").write_text("wrong data")
    with pytest.raises(ValueError,match="published_output_digest_changed"):
        _validate_publication_recovery(run,result)


def test_launch_control_is_right_of_derivation_and_uses_pipeline_owner(monkeypatch):
    from rstock.application import streamlit_app as app
    from rstock.application import scientific_analysis_ui as ui
    from types import SimpleNamespace
    calls=[]
    class Column:
        def __init__(self,index):self.index=index
        def __enter__(self):calls.append(("column",self.index))
        def __exit__(self,*args):pass
    fake=SimpleNamespace(session_state={},container=lambda **kw:Column(0),
        button=lambda label,**kw:calls.append(("button",label)) or False)
    monkeypatch.setattr(app,"st",fake)
    monkeypatch.setattr(ui,"render_analysis_launcher",lambda st,service,source,**kw:calls.append(("launch",source)))
    monkeypatch.setattr(ui,"render_analysis_form",lambda st,service,source,selected,**kw:calls.append(("inline",source)))
    app._render_walk_forward_derived_creation("wf-child",{
        "status":{"status":"completed"},"storage":{"state":"full"},"analysis_owner_run_id":"e2e-owner"},object())
    assert calls==[("column",0),("button","Créer une expérience dérivée"),("launch","e2e-owner"),("inline","e2e-owner")]


def test_selection_displays_inline_form_without_raw_json(setup, monkeypatch):
    from rstock.application import scientific_analysis_ui as ui
    repo, service, source, item, _ = setup
    calls = []
    class Surface:
        def __init__(self): self.session_state = {}; self.scope = "page"
        def region(self, name):
            surface = self
            class Region:
                def __enter__(self): surface.scope = name
                def __exit__(self, *args): surface.scope = "page"
            return Region()
        def container(self, **kwargs): return self.region("inline")
        def selectbox(self, label, values, **kwargs):
            calls.append((self.scope, "selector")); return "tiny"
        def checkbox(self, *args, **kwargs): calls.append((self.scope, "confirmation")); return True
        def button(self, label, **kwargs):
            calls.append((self.scope, label, kwargs.get("disabled"))); return False
        def json(self, *args, **kwargs): pytest.fail("No raw JSON in configuration form")
        def warning(self, text): calls.append((self.scope, "warning", text))
        def info(self, text): calls.append((self.scope, "info", text))
        def subheader(self, text): calls.append((self.scope, "heading", text))
        def caption(self, text): calls.append((self.scope, "caption", text))
        def write(self, text): calls.append((self.scope, "text", text))
    fake = Surface()
    item = replace(item, render_parameters=lambda key: calls.append((fake.scope, "parameters")) or {},
        preflight=lambda *_: pytest.fail("Deep preparation during form rendering"))
    monkeypatch.setattr(ui, "get_analysis", lambda _: item)
    selected = ui.render_analysis_launcher(fake, service, source, key="fixture", navigate=lambda *_: None)
    assert selected == "tiny"
    assert calls == [("page", "Lancer une analyse", True), ("page", "selector")]
    calls.clear()
    ui.render_analysis_form(fake, service, source, selected, key="fixture", navigate=lambda *_: None)
    assert ("inline", "parameters") in calls and ("inline", "confirmation") in calls
    assert ("inline", "Lancer l’analyse sélectionnée", True) in calls
    assert all(call[0] == "inline" for call in calls)


def test_resolved_parameters_are_frozen_by_common_submission(setup, monkeypatch):
    repo, service, source, item, _ = setup
    resolved = {"selection_mode": "sessions", "selection_sessions": 3,
                "resolved_split": {"selection_end": "2025-07-15", "evaluation_start": "2025-07-16"}}
    # Only a synthetic plugin exercises this common submission contract.
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (replace(item,
        preflight=lambda *_: {"ready": True, "resolved_parameters": resolved},
        light_preflight=lambda *_: {"ready":True}),))
    spec = analysis.prepare_analysis(repo, source, "tiny", {}, request_id="resolved")
    assert spec.scientific_analysis["parameters"] == {}
    output = repo.run_directory(source)/"_fixture_output"
    analysis.execute_analysis(spec, output, None, None)
    from rstock.checkpoints import CheckpointManager
    checkpoint = CheckpointManager(output.parent, run_id=output.parent.name, job_type=spec.job_type.value, configuration_fingerprint=spec.fingerprint, batch_sizes={"scientific_analysis":1})
    assert checkpoint.load_artifact("prepared_contract")["parameters"] == resolved
    assert "results/tiny.json" in spec.scientific_analysis["source_identities"]
    assert spec.to_dict()["scientific_analysis"]["parameters"] == {}


def test_legacy_analysis_metadata_remains_readable_without_active_dependency(setup):
    repo, service, source, _, _ = setup
    spec = analysis.prepare_analysis(repo, source, "tiny", {"inputs_ready": True}, request_id="legacy")
    raw = spec.to_dict()
    raw["scientific_analysis"]["analysis_id"] = "rare_relations_persistence"
    raw["scientific_analysis"]["definition_version"] = "1"
    raw["scientific_analysis"]["parameters"] = {"protocol_id": "historical", "approval_digest": "historical-hash",
        "selection_end": "2025-07-15", "evaluation_end": "2026-04-02"}
    loaded = ExperimentSpec.from_dict(raw)
    assert loaded.to_dict()["scientific_analysis"] == raw["scientific_analysis"]
    import hashlib
    assert loaded.fingerprint == hashlib.sha256(json.dumps(raw, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def test_ui_submission_history_and_resume_never_read_source_payload(setup, monkeypatch):
    from rstock.application.history_index import clear_history_cache
    from rstock.application.history_ui import EXPERIMENT_JOB_TYPES, filter_runs, history_row
    from rstock.application.history_grid import history_grid_row
    repo, service, source, item, backend = setup
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (replace(item,
        preflight=lambda *_: pytest.fail("Deep preflight in UI")),))
    monkeypatch.setattr(analysis, "digest", lambda *a, **kw: pytest.fail("Hash in UI"))
    original_open = Path.open
    payload = repo.run_directory(source)/"results/tiny.json"
    def guarded(path, *args, **kw):
        if path == payload or path.suffix in {".csv", ".pkl"}:
            pytest.fail("Source payload read in UI: " + str(path))
        return original_open(path, *args, **kw)
    monkeypatch.setattr(Path, "open", guarded)
    for _ in range(3):
        assert service.analysis_preflight(source, "tiny", {"inputs_ready": True})["ready"]
        assert analysis.compatible_analyses(repo, source)
    job = service.start_analysis(source, "tiny", {"inputs_ready": True}, request_id="light")
    assert repo.status(job.run_id)["status"] == "pending"
    for _ in range(2):
        clear_history_cache()
        records = service.history_index(job_types=EXPERIMENT_JOB_TYPES)
        details = {x.status["run_id"]: x.detail() for x in records}
        filtered = filter_runs([x.status for x in records], allowed_types=EXPERIMENT_JOB_TYPES,
                               detail_loader=details.__getitem__)
        assert any(x["run_id"] == job.run_id for x in filtered)
        for status in filtered:
            row = history_row(status, details[status["run_id"]], {}, related_details=details)
            history_grid_row(row, details[row.run_id], universe_labels={}, related_details=details, runs_root=repo.root)
    repo.transition(job.run_id, JobStatus.FAILED, error="fixture")
    service.resume(job.run_id)
    assert len(backend.calls) == 2
    service.restart(job.run_id)
    assert len(backend.calls) == 3


def test_builtin_missing_artifacts_detected_without_calendar_or_hash(setup, monkeypatch):
    from rstock.application import rare_relations_analysis as rare
    repo, service, source, _, backend = setup
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (rare.definition(),))
    monkeypatch.setattr(rare, "source_calendar", lambda *a: pytest.fail("Calendar in UI"))
    monkeypatch.setattr(analysis, "digest", lambda *a: pytest.fail("Hash in UI"))
    result = service.analysis_preflight(source, rare.ANALYSIS_ID, {})
    assert not result["ready"] and not rare.definition().blocking_reasons
    with pytest.raises(ValueError, match="Artefacts manquants"):
        service.start_analysis(source, rare.ANALYSIS_ID, {}, request_id="blocked")
    assert not backend.calls


def test_preparation_cancellation_is_recorded_and_reuses_contract(setup, monkeypatch):
    from rstock.progress import CancellationRequested
    repo, service, source, item, _ = setup
    job = service.start_analysis(source, "tiny", {"inputs_ready": True}, request_id="cancel")
    spec = repo.load_spec(job.run_id)
    with pytest.raises(CancellationRequested):
        analysis.execute_analysis(spec, repo.run_directory(job.run_id)/"_cancelled", None, lambda: True)
    assert analysis.execute_analysis(spec, repo.run_directory(job.run_id)/"_resumed", None, None)["complete"]
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (replace(item,
        preflight=lambda *_: pytest.fail("Preparation must be reused"),
        execute=lambda *_: pytest.fail("Analysis must be reused")),))
    assert analysis.execute_analysis(spec, repo.run_directory(job.run_id)/"_republished", None, None)["complete"]


def test_legacy_manifest_executes_with_original_hash_contract(setup):
    repo, service, source, item, _ = setup
    requested = analysis.prepare_analysis(repo, source, "tiny", {"inputs_ready": True}, request_id="legacy")
    source_dir = repo.run_directory(source)
    legacy = {**requested.scientific_analysis, "schema_version": 1,
              "source_sha256": {name: analysis.digest(source_dir/name) for name in requested.scientific_analysis["source_identities"]},
              "owner_manifest_sha256": None}
    spec = replace(requested, scientific_analysis=legacy)
    job = service.submit(spec)
    assert analysis.execute_analysis(repo.load_spec(job.run_id), repo.run_directory(job.run_id)/"results", None, None)["complete"]


def test_source_digest_change_after_preparation_refuses_reuse(setup):
    repo, service, source, _, _ = setup
    job = service.start_analysis(source, "tiny", {"inputs_ready": True}, request_id="changed-after")
    spec = repo.load_spec(job.run_id)
    analysis.execute_analysis(spec, repo.run_directory(job.run_id)/"_first", None, None)
    repo.write_json(source, "results/tiny.json", {"value": 9})
    with pytest.raises(ValueError, match="digest_changed"):
        analysis.execute_analysis(spec, repo.run_directory(job.run_id)/"_second", None, None)


def test_results_renderer_reads_only_worker_preview(tmp_path, monkeypatch):
    import streamlit as st
    from rstock.application import rare_relations_analysis as rare, grid_dataframe
    results = tmp_path/"results"
    results.mkdir()
    csv = results/"persistence.csv"
    csv.write_text("Raw payload must not be opened")
    csv.with_name(csv.name+".preview.json").write_text(json.dumps({"columns":["Value"], "data":[[2]], "row_limit":500}))
    original_open = Path.open
    def guarded(path, *args, **kw):
        if path.suffix == ".csv": pytest.fail("CSV read during UI rendering")
        return original_open(path, *args, **kw)
    monkeypatch.setattr(Path, "open", guarded)
    monkeypatch.setattr(st, "caption", lambda *_: None)
    rendered = []
    monkeypatch.setattr(grid_dataframe, "dataframe", lambda data, **kw: rendered.append(data))
    rare.render_results("persistence", "fixture", {"run_directory":str(tmp_path)})
    assert rendered[0].Value.tolist() == [2]
