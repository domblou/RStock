"""Public Prefilter boundary, new graph and historical graph coexistence."""
import hashlib
import json
from dataclasses import replace

import pytest

from test_prefilter_experiments import _fixture, _complete
from rstock.application import workflows, end_to_end
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.prefilter_contract import CONTRACT, plan
from rstock.application.derivation import stage_modes


def _source(tmp_path, monkeypatch):
    repository, spec, calls, _, original = _fixture(tmp_path, monkeypatch)
    source = repository.create(spec)
    summary = workflows._predictor_prefilter(spec, repository.run_directory(source) / "results", None, None)
    _complete(repository, source, summary)
    monkeypatch.setattr(workflows, "_prepared_inputs", original)
    path = repository.run_directory(source) / CONTRACT
    wf = replace(spec, job_type=JobType.WALK_FORWARD, source_prefilter_run=source,
                 source_prefilter_contract_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                 source_prepared_dataset_sha256=summary["traceability"]["prepared_dataset_sha256"],
                 prepared_snapshot_required=True)
    return repository, source, wf, calls


def test_frozen_prefilter_matches_original_selection_without_recalculation(tmp_path, monkeypatch):
    repository, source, wf, calls = _source(tmp_path, monkeypatch)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", lambda *a, **k: pytest.fail("recalculated"))
    prepared, predictors, targets, calendars = workflows._prepared_inputs(wf, None, None)
    effective = plan(repository, wf)
    selection = json.loads((repository.run_directory(source) / "results/predictor_prefilter.json").read_text())
    assert dict(effective.predictors_by_target) == {k: tuple(v) for k, v in selection["predictors_by_target"].items()}
    assert len(prepared) == 10 and calls == [True]


def test_new_graph_reserves_prefilter_before_wf_and_legacy_graph_unchanged(tmp_path, monkeypatch):
    repository, source, wf, _ = _source(tmp_path, monkeypatch)
    parent = replace(wf, job_type=JobType.END_TO_END, source_prefilter_run=None,
                     source_prefilter_contract_sha256=None, pipeline_version=4)
    root = repository.create(parent)
    manifest = end_to_end.build_pipeline_manifest(repository, root, parent)
    assert manifest["schema_version"] == 5
    assert [s["stage_key"] for s in manifest["stages"]][:2] == ["prefilter", "walk_forward"]
    manifest["stages"][0]["child_run_id"] = source
    child = end_to_end.build_stage_spec(repository, root, parent, "walk_forward", manifest)
    assert child.source_prefilter_run == source
    assert plan(repository, child).count() == 1
    old = end_to_end.build_pipeline_manifest(repository, root, replace(parent, pipeline_version=3))
    assert old["schema_version"] == 3 and old["stages"][0]["stage_key"] == "walk_forward"


def test_contract_tampering_and_holdout_overlap_are_rejected(tmp_path, monkeypatch):
    repository, source, wf, _ = _source(tmp_path, monkeypatch)
    with pytest.raises(ValueError, match="holdout"):
        plan(repository, replace(wf, config=replace(wf.config, final_holdout_size=2)))
    path = repository.run_directory(source) / CONTRACT
    path.write_text(path.read_text() + " ")
    with pytest.raises(ValueError, match="changed"):
        plan(repository, wf)


def test_historical_execution_policy_and_new_derivation_dag(tmp_path, monkeypatch):
    _, _, wf, _ = _source(tmp_path, monkeypatch)
    old = wf.to_dict()
    old.pop("prefilter_execution_version")
    assert ExperimentSpec.from_dict(old).prefilter_execution_version == 1
    assert wf.prefilter_execution_version == 2
    assert stage_modes("walk_forward", schema_version=3)["prefilter"] == "inherited"
    assert stage_modes("prefilter", schema_version=3)["walk_forward"] == "recomputed"


def test_new_standalone_wf_requires_explicit_prefilter(tmp_path, monkeypatch):
    _, _, wf, _ = _source(tmp_path, monkeypatch)
    with pytest.raises(ValueError, match="explicit Prefilter"):
        workflows._prepared_inputs(replace(wf, source_prefilter_run=None), None, None)


def test_empty_prefilter_stops_before_wf_materialization(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    evaluate = workflows.evaluate_prefilter_walk_forward
    def reject_all(*args, **kwargs):
        result = evaluate(*args, **kwargs)
        return replace(result, qualification=result.qualification.assign(Eligible=False))
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", reject_all)
    source = repository.create(spec)
    summary = workflows._predictor_prefilter(spec, repository.run_directory(source) / "results", None, None)
    _complete(repository, source, summary)
    assert summary["retained_predictors"] == 0
    parent = replace(spec, job_type=JobType.END_TO_END)
    root = repository.create(parent)
    manifest = end_to_end.build_pipeline_manifest(repository, root, parent)
    manifest["stages"][0]["child_run_id"] = source
    reserved_wf = manifest["stages"][1]["child_run_id"]
    with pytest.raises(ValueError, match="no testable combinations"):
        end_to_end.build_stage_spec(repository, root, parent, "walk_forward", manifest)
    assert not repository.run_directory(reserved_wf).exists()


def test_completed_wf_still_protects_its_prefilter_snapshot_from_purge(tmp_path, monkeypatch):
    from rstock.application.run_storage import RunStorageService
    repository, source, wf, _ = _source(tmp_path, monkeypatch)
    child = repository.create(wf)
    _complete(repository, child, {})
    eligibility = RunStorageService(repository).eligibility(source)
    assert not eligibility.eligible and child in eligibility.reason


def test_wf_ui_preview_consumes_the_frozen_prefilter_population(tmp_path, monkeypatch):
    from rstock.application import streamlit_app
    from contextlib import nullcontext
    repository, source, wf, _ = _source(tmp_path, monkeypatch)
    class Session(dict):
        def __getattr__(self, key):
            return self[key]
    class UI:
        session_state = Session(lab_config=wf.config, lab_target_symbols=list(wf.target_symbols),
                                lab_symbols=list(wf.predictor_symbols), lab_context_symbols=list(wf.context_symbols))
        metrics = {}
        def container(self, **kwargs):
            return nullcontext()
        def columns(self, number):
            return [self] * number
        def subheader(self, *args):
            pass
        def caption(self, *args):
            pass
        def metric(self, label, value):
            self.metrics[label] = value
        def error(self, message):
            pytest.fail(message)
    ui = UI()
    monkeypatch.setattr(streamlit_app, "st", ui)
    assert streamlit_app._combination_plan_preview(JobType.WALK_FORWARD, config=wf.config,
                                                 source_prefilter_run=source)
    assert ui.session_state["experiment-combination-preview"].effective_combination_count == 1


def test_end_to_end_resolves_offset_once_before_prefilter(tmp_path, monkeypatch):
    from datetime import date
    from types import SimpleNamespace
    repository, single, _, _, _ = _fixture(tmp_path, monkeypatch)
    parent = replace(single, job_type=JobType.END_TO_END, historical_data_cutoff=None,
                     config=replace(single.config, walk_forward_end_offset_sessions=2))
    monkeypatch.setattr(end_to_end, "date", SimpleNamespace(today=lambda: date(2026, 9, 25)))
    root = repository.create(parent)
    manifest = end_to_end.build_pipeline_manifest(repository, root, parent)
    assert manifest["prepared_dataset_as_of"] == "2026-09-23"
    child = end_to_end.build_stage_spec(repository, root, parent, "prefilter", manifest)
    assert child.historical_data_cutoff == "2026-09-23"
    assert child.config.walk_forward_end_offset_sessions == 0


@pytest.mark.parametrize("mode", ["single_origin", "temporal_stability", "temporal_consensus"])
def test_end_to_end_resume_and_derivations_keep_prefilter_source(tmp_path, monkeypatch, mode):
    from collections import Counter
    from test_end_to_end import _fake_registry, _resume
    from rstock.application.worker import execute_run
    from rstock.application.domain import JobStatus
    from rstock.application.derived_experiments import build_derived_spec
    from rstock.checkpoints import CheckpointManager
    import pandas as pd

    repository, single, _, _, original = _fixture(tmp_path, monkeypatch)
    fake_preparation = workflows._prepared_inputs
    def preparation(spec, *args):
        if spec.source_prefilter_run or spec.prepared_snapshot_required:
            return original(spec, *args)
        view, predictors, targets, calendars = fake_preparation(spec, *args)
        view.index = pd.date_range(end=spec.historical_data_cutoff, periods=10, freq="B")
        view.attrs["effective_end_date"] = view.index.max().isoformat()
        return view, predictors, targets, calendars
    monkeypatch.setattr(workflows, "_prepared_inputs", preparation)
    monkeypatch.setattr(end_to_end, "build_forward_model_snapshot", lambda *a, **k: None)
    root_spec = replace(single, job_type=JobType.END_TO_END, pipeline_version=4, prefilter_method=mode,
                        config=replace(single.config, temporal_consensus_step_sessions=1))
    root = repository.create(root_spec)
    calls = Counter()
    registry = _fake_registry(repository, calls)
    registry.handlers[JobType.PREDICTOR_PREFILTER] = workflows._predictor_prefilter
    original_threshold = registry.handlers[JobType.THRESHOLD_CALIBRATION]
    def threshold(spec, output, *args):
        result = original_threshold(spec, output, *args)
        pd.DataFrame(columns=["Set", "Direction"]).to_csv(output / "threshold_metrics_by_set.csv", index=False)
        pd.DataFrame([dict(Observation="AAA", Predictors='["BBB"]')]).to_csv(output / "sampled_combinations.csv", index=False)
        return result
    registry.handlers[JobType.THRESHOLD_CALIBRATION] = threshold
    failed = False
    def wf(spec, output, *args):
        nonlocal failed
        calls[JobType.WALK_FORWARD] += 1
        if not failed:
            failed = True
            raise RuntimeError("interruption after completed Prefilter")
        prepared, predictors, targets, calendars = preparation(spec, None, None)
        checkpoint = workflows._walk_forward_checkpoint(repository, output.parent.name, spec)
        checkpoint.commit_snapshot(prepared, dict(predictor_symbols=predictors, target_symbols=targets,
                                                 calendars=calendars, effective_end_date=prepared.attrs["effective_end_date"]))
        raw, effective, selected, *_ = workflows._planned_effective_plan(
            spec, prepared, predictors, targets, calendars, checkpoint, None, None)
        assert selected is None
        output.mkdir(exist_ok=True)
        pd.DataFrame([dict(Set="AAA<-BBB", Observation="AAA", Predictors='["BBB"]', Eligible=True,
                           ROCAUCMedian=.65)]).to_csv(output / "qualification.csv", index=False)
        configuration = {}
        workflows._inherit_walk_forward_prefilter(spec, output, configuration)
        (output / "run_configuration.json").write_text(json.dumps(configuration))
        trace = workflows._persist_prepared_traceability({}, prepared, spec)
        return dict(job_type=spec.job_type.value, traceability=trace)
    registry.handlers[JobType.WALK_FORWARD] = wf
    def terminal(spec, output, *args):
        output.mkdir(exist_ok=True)
        if spec.job_type is JobType.HOLDOUT_EVALUATION:
            pd.DataFrame(columns=["Set", "Direction"]).to_csv(output / "holdout_metrics.csv", index=False)
            (output / "run_configuration.json").write_text("{}")
        else:
            (output / "qualification.json").write_text('{"candidate_sets": [], "decisions": []}')
        return dict(job_type=spec.job_type.value)
    registry.handlers[JobType.HOLDOUT_EVALUATION] = terminal
    registry.handlers[JobType.PROMOTION_QUALIFICATION] = terminal
    execute_run(repository, root, 1, registry=registry)
    assert repository.status(root)["status"] == "failed"
    first = end_to_end.load_pipeline_manifest(repository, root)
    source = first["stages"][0]["child_run_id"]
    assert repository.status(source)["status"] == "completed"
    before = (repository.run_directory(source) / CONTRACT).read_bytes()
    _resume(repository, root, registry)
    assert repository.status(root)["status"] == "completed", repository.status(root).get("error")
    assert (repository.run_directory(source) / CONTRACT).read_bytes() == before
    assert calls[JobType.WALK_FORWARD] == 2
    derived = build_derived_spec(repository, root, "walk_forward", {"xgb_eta": .33})
    child = repository.create(derived)
    execute_run(repository, child, 1, registry=registry)
    assert repository.status(child)["status"] == "completed", repository.status(child).get("error")
    derived_manifest = end_to_end.load_pipeline_manifest(repository, child)
    assert derived_manifest["schema_version"] == 6
    assert derived_manifest["stages"][0]["source_run_id"] == source
    new_wf = repository.load_spec(derived_manifest["stages"][1]["child_run_id"])
    assert new_wf.source_prefilter_run == source
    assert new_wf.config.xgb_eta == .33
    fork = build_derived_spec(repository, root, "prefilter", {"predictor_prefilter_top_n": 2})
    fork_id = repository.create(fork)
    execute_run(repository, fork_id, 1, registry=registry)
    assert repository.status(fork_id)["status"] == "completed", repository.status(fork_id).get("error")
    fork_manifest = end_to_end.load_pipeline_manifest(repository, fork_id)
    assert fork_manifest["stages"][0]["child_run_id"] != source
