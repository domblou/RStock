"""Global origin telemetry, lifecycle, resume and job-card presentation."""
from contextlib import nullcontext
from dataclasses import replace

import pytest

from rstock.progress import ProgressEvent, CancellationRequested
from rstock.application.prefilter_progress import PHASE, TemporalPrefilterProgress
from rstock.application.runner import ProgressReporter
from rstock.application import streamlit_app as ui, workflows
from test_prefilter_experiments import _fixture


class Checkpoint:
    def __init__(self, selected=False, batches=()):
        self.selected, self.batches = selected, batches
    def artifact_exists(self, name):
        return self.selected
    def load_artifact(self, name):
        return {}
    def completed_batch_ids(self, phase):
        return self.batches
    def load_batch(self, phase, batch_id):
        return {}


def tracker(callback, checkpoints=None):
    return TemporalPrefilterProgress(callback, ["2026-07-07", "2026-06-16", "2026-05-15", "2026-04-16"],
                                     1190, checkpoints or [Checkpoint() for _ in range(4)], 50)


def test_four_origins_transition_and_last_batch_exactly_complete():
    events = []
    progress = tracker(events.append)
    first = progress.origin_callback(0)
    first(ProgressEvent(PHASE, "batch 20/24", 1000, 1190))
    assert events[-1].total_units == 4760 and events[-1].completed_units == 1000
    first(ProgressEvent(PHASE, details={"phase_event": "completed"}))
    assert not events[-1].details.get("phase_event")
    progress.origin_completed(0)
    second = progress.origin_callback(1)
    second(ProgressEvent(PHASE, "batch 2/24", 74, 1190))
    assert events[-1].completed_units == 1264
    assert events[-1].completed_units / events[-1].total_units == pytest.approx(.265546)
    assert events[-1].details["origin_number"] == 2
    progress.origin_completed(1)
    progress.origin_completed(2)
    last = progress.origin_callback(3)
    last(ProgressEvent(PHASE, "batch 24/24", 1190, 1190))
    assert events[-1].completed_units == events[-1].total_units == 4760
    progress.origin_completed(3)
    progress.finish()
    assert sum(e.details.get("phase_event") == "completed" for e in events) == 1


def test_resume_restores_origins_and_partial_committed_batches(monkeypatch):
    events = []
    progress = tracker(events.append, [Checkpoint(True), Checkpoint(True), Checkpoint(batches=(0, 1)), Checkpoint()])
    callback = progress.origin_callback(2)
    assert events[-1].completed_units == 2480
    assert events[-1].details["origin_completed_units"] == 100
    # Restarted lifecycle events cannot reset the restored 100 units.
    callback(ProgressEvent(PHASE, "started", 0, 1190, {"phase_event": "started"}))
    assert events[-1].completed_units == 2480
    monkeypatch.setattr("rstock.application.prefilter_progress.monotonic", lambda: progress.started + 10)
    callback(ProgressEvent(PHASE, "batch 3/24", 150, 1190))
    assert events[-1].completed_units == 2530
    assert events[-1].details["progress_rate_completed_units"] == 50
    assert events[-1].details["progress_rate_elapsed_seconds"] == 10


def test_global_eta_uses_remaining_units_and_only_work_since_resume(tmp_path, monkeypatch):
    repository, spec, *_ = _fixture(tmp_path, monkeypatch)
    run_id = repository.create(spec)
    reporter = ProgressReporter(repository, run_id)
    reporter.configure_phases([(PHASE, 90), ("publishing", 10)])
    progress = tracker(reporter, [Checkpoint(True), Checkpoint(), Checkpoint(), Checkpoint()])
    callback = progress.origin_callback(1)
    monkeypatch.setattr("rstock.application.prefilter_progress.monotonic", lambda: progress.started + 10)
    callback(ProgressEvent(PHASE, "batch 2/24", 74, 1190))
    persisted = repository.progress(run_id)
    assert persisted["stage_percent"] == pytest.approx(1264 / 4760 * 100)
    assert persisted["eta_seconds"] == pytest.approx((4760 - 1264) / (74 / 10))
    assert not any(p["status"] == "completed" for p in persisted["phase_history"])


@pytest.mark.parametrize("mode,count", [("single_origin", 1), ("temporal_consensus", 4), ("temporal_stability", 5)])
def test_real_prefilter_routes_preserve_single_and_count_all_origins(tmp_path, monkeypatch, mode, count):
    repository, single, *_ = _fixture(tmp_path, monkeypatch)
    prepare = workflows._prepared_inputs
    if mode == "temporal_consensus":
        def autonomous(spec, *args):
            import pandas as pd
            frame, predictors, targets, calendars = prepare(spec, *args)
            frame.index = pd.date_range(end=spec.historical_data_cutoff, periods=10, freq="B")
            frame.attrs["effective_end_date"] = frame.index.max().isoformat()
            return frame, predictors, targets, calendars
        monkeypatch.setattr(workflows, "_prepared_inputs", autonomous)
    spec = replace(single, prefilter_method=mode, config=replace(single.config, temporal_consensus_step_sessions=1))
    run_id = repository.create(spec)
    events = []
    workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", events.append, None)
    global_events = [e for e in events if e.details.get("progress_scope") == "temporal_prefilter"]
    if mode == "single_origin":
        assert not global_events
    else:
        assert global_events[-1].completed_units == global_events[-1].total_units == 3 * count
        assert {e.details["origin_number"] for e in global_events} == set(range(1, count + 1))
        # Reuse completed origin artifacts; progress starts already complete.
        events.clear()
        workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", events.append, None)
        restored = [e for e in events if e.details.get("progress_scope") == "temporal_prefilter"]
        assert restored[0].completed_units == restored[0].total_units == 3 * count


def test_job_card_displays_global_bar_origin_and_eta(monkeypatch):
    class UI:
        captions, bars = [], []
        def container(self, **kwargs): return nullcontext()
        def columns(self, *args): return [self] * 4
        def markdown(self, *args): pass
        def code(self, *args): pass
        def metric(self, *args): pass
        def button(self, *args, **kwargs): return False
        def caption(self, text): self.captions.append(text)
        def progress(self, value): self.bars.append(value)
    fake = UI()
    monkeypatch.setattr(ui, "st", fake)
    progress = dict(stage=PHASE, substage="batch 2/24", completed_units=1264, total_units=4760,
                    workflow_percent=95, stage_percent=1264/4760*100, eta_seconds=120,
                    details=dict(progress_scope="temporal_prefilter", origin_number=2, origin_count=4,
                                 origin_cutoff="2026-06-16", origin_completed_units=74, origin_total_units=1190))
    class Service:
        def runs(self): return [dict(run_id="prefilter", job_type="predictor_prefilter", status="running",
                                    created_at="2026-07-07T00:00:00+00:00")]
        def run(self, _): return dict(progress=progress, log_tail=[])
    assert ui._job_panel(Service())
    assert fake.bars == [pytest.approx(1264/4760)]
    assert any("Origine : 2 / 4" in text and "2026-06-16" in text for text in fake.captions)
    assert any("1264 / 4760 unités globales" in text and "74 / 1190" in text and "ETA" in text for text in fake.captions)


@pytest.mark.parametrize("mode,count", [("temporal_consensus", 4), ("temporal_stability", 5)])
def test_interruption_reconciles_real_origin_checkpoints(tmp_path, monkeypatch, mode, count):
    repository, single, *_ = _fixture(tmp_path, monkeypatch)
    prepare = workflows._prepared_inputs
    if mode == "temporal_consensus":
        def autonomous(spec, *args):
            import pandas as pd
            frame, predictors, targets, calendars = prepare(spec, *args)
            frame.index = pd.date_range(end=spec.historical_data_cutoff, periods=10, freq="B")
            frame.attrs["effective_end_date"] = frame.index.max().isoformat()
            return frame, predictors, targets, calendars
        monkeypatch.setattr(workflows, "_prepared_inputs", autonomous)
    spec = replace(single, prefilter_method=mode, config=replace(
        single.config, temporal_consensus_step_sessions=1, predictor_prefilter_batch_size=1,
    ))
    evaluate = workflows.evaluate_prefilter_walk_forward
    calls = []
    def interrupted(view, *args, **kwargs):
        calls.append(view.attrs["effective_end_date"])
        result = evaluate(view, *args, **kwargs)
        if len(calls) == 2:
            manager = kwargs["checkpoint_manager"]
            manager.set_total_batches(PHASE, 3)
            manager.commit_batch(PHASE, 0, {"qualification": result.qualification.iloc[:1]},
                                 first_index=0, last_index=0, combination_count=1,
                                 row_counts={"qualification": 1})
            kwargs["progress_callback"](ProgressEvent(PHASE, "batch 1/3", 1, 3))
            raise CancellationRequested("interrupted")
        return result
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", interrupted)
    run_id = repository.create(spec, run_id="p")  # Keep nested checkpoint paths below Windows MAX_PATH.
    output = repository.run_directory(run_id) / "results"
    with pytest.raises(CancellationRequested):
        workflows._predictor_prefilter(spec, output, None, None)
    events = []
    workflows._predictor_prefilter(spec, output, events.append, None)
    restored = [e for e in events if e.details.get("progress_scope") == "temporal_prefilter"]
    assert restored[0].completed_units == 4  # One full origin + one committed candidate.
    assert restored[0].total_units == 3 * count
    assert restored[-1].completed_units == 3 * count
    assert calls.count(calls[0]) == 1  # Completed origin was not recomputed.
