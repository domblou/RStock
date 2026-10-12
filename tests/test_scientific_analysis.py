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
        lambda p: p, lambda s,p:{"ready":p.get("inputs_ready") is True,"reasons":[] if p.get("inputs_ready") else ["requirements_missing"]},execute)
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
    with pytest.raises(ValueError,match="digest_changed"):
        service.run_service.resume(job.run_id)
    assert len(backend.calls)==1
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
    assert [x.key for x in tabs_for_job(JobType.SCIENTIFIC_ANALYSIS)]==["summary","resources","technical"]
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
        render_resources=lambda _:None,render_technical=lambda *_:None,navigate=lambda *_:None)
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
    fake=SimpleNamespace(session_state={},columns=lambda n:[Column(i) for i in range(n)],
        button=lambda label,**kw:calls.append(("button",label)) or False)
    monkeypatch.setattr(app,"st",fake)
    monkeypatch.setattr(ui,"render_analysis_launcher",lambda st,service,source,**kw:calls.append(("launch",source)))
    monkeypatch.setattr(ui,"render_analysis_form",lambda st,service,source,selected,**kw:calls.append(("inline",source)))
    app._render_walk_forward_derived_creation("wf-child",{
        "status":{"status":"completed"},"storage":{"state":"full"},"analysis_owner_run_id":"e2e-owner"},object())
    assert calls==[("column",0),("button","Créer une expérience dérivée"),("column",1),("launch","e2e-owner"),("inline","e2e-owner")]


def test_menu_only_selects_and_form_is_inline_without_raw_json(setup, monkeypatch):
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
        def popover(self, label): return self.region("menu")
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
    item = replace(item, render_parameters=lambda key: calls.append((fake.scope, "parameters")) or {})
    monkeypatch.setattr(ui, "get_analysis", lambda _: item)
    selected = ui.render_analysis_launcher(fake, service, source, key="fixture", navigate=lambda *_: None)
    assert calls == [("menu", "selector")]
    ui.render_analysis_form(fake, service, source, selected, key="fixture", navigate=lambda *_: None)
    assert ("inline", "parameters") in calls and ("inline", "confirmation") in calls
    assert ("inline", "Lancer l’analyse sélectionnée", True) in calls
    assert all(call[0] == "inline" for call in calls[1:])


def test_resolved_parameters_are_frozen_by_common_submission(setup, monkeypatch):
    repo, service, source, item, _ = setup
    resolved = {"selection_mode": "sessions", "selection_sessions": 3,
                "resolved_split": {"selection_end": "2025-07-15", "evaluation_start": "2025-07-16"}}
    # Only a synthetic plugin exercises submission; the real rare analysis stays blocked.
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (replace(item,
        preflight=lambda *_: {"ready": True, "resolved_parameters": resolved}),))
    spec = analysis.prepare_analysis(repo, source, "tiny", {}, request_id="resolved")
    assert spec.scientific_analysis["parameters"] == resolved
    assert "results/tiny.json" in spec.scientific_analysis["source_sha256"]
    assert spec.to_dict()["scientific_analysis"]["parameters"] == resolved


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
