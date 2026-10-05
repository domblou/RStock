"""Exercise reactive launch controls and their persisted Prefilter configuration."""
from datetime import date

import pytest
from streamlit.testing.v1 import AppTest

from rstock.application import streamlit_app as ui, end_to_end
from rstock.application.domain import JobType
from rstock.application.repository import RunRepository
from rstock.application.prefilter_consensus import resolve_consensus_origins


@pytest.fixture
def launch_app(monkeypatch):
    monkeypatch.setattr(ui, "_page_header", lambda *_: None)
    monkeypatch.setattr(ui, "_render_locked_duplication_mode", lambda *_: False)
    monkeypatch.setattr(ui, "_experiment_universe_selector", lambda: True)
    monkeypatch.setattr(ui, "_combination_plan_preview", lambda *a, **k: True)
    monkeypatch.setattr(ui, "_live_job_panel", lambda *a, **k: None)
    monkeypatch.setattr(ui, "_render_experiment_submission_confirmation", lambda *a: None)
    app = AppTest.from_string('''
import streamlit as st
from dataclasses import replace
from rstock.config import DEFAULT_CONFIG
from rstock.application.universes import UniverseSelection
from rstock.application import streamlit_app as ui
from types import SimpleNamespace
if "lab_config" not in st.session_state:
    st.session_state.update(dict(
        lab_config=replace(DEFAULT_CONFIG, prefilter_selection_mode="single_origin",
            predictor_prefilter_enabled=True,
            temporal_consensus_origins=5, temporal_consensus_step_sessions=10,
            temporal_consensus_min_occurrences=4),
        lab_calendar="XNYS", lab_symbols=["AAA", "BBB"],
        lab_target_symbols=["AAA"], lab_context_symbols=["BBB"],
        lab_combinations_per_target=1, lab_evaluate_holdout=False,
        lab_universe_selection=UniverseSelection(), lab_market_benchmark_symbol=None,
        lab_context_universe_ids=[], lab_context_sample_size=None,
        lab_context_selection_method=None, lab_context_seed=None,
    ))
ui._experiments(SimpleNamespace(run_service=SimpleNamespace(
    repository=SimpleNamespace(list_run_ids=lambda: []))))
''').run()
    assert not app.exception
    app.selectbox[0].select("Préfiltre prédicteurs").run()
    assert not app.exception
    return app


def _mode(app, mode):
    app.radio(key="launch-prefilter-method").set_value(mode).run()
    assert not app.exception


def test_modes_update_immediately_and_keep_stability_controls(launch_app):
    app = launch_app
    assert not app.number_input
    _mode(app, "temporal_stability")
    assert app.number_input(key="launch-prefilter-origin-count").value == 5
    assert app.number_input(key="launch-prefilter-step-sessions").value == 1
    assert len(app.number_input) == 2
    _mode(app, "temporal_consensus")
    assert any(s.value == "Paramètres du consensus temporel" for s in app.subheader)
    assert app.number_input(key="launch-temporal_consensus_origins").value == 5
    assert app.number_input(key="launch-temporal_consensus_step_sessions").value == 10
    assert app.number_input(key="launch-temporal_consensus_min_occurrences").value == 4
    assert all(n.min == 1 for n in app.number_input)
    _mode(app, "single_origin")
    assert not app.number_input


def test_invalid_occurrence_threshold_blocks_submission(launch_app):
    app = launch_app
    _mode(app, "temporal_consensus")
    app.date_input[0].set_value(date(2026, 9, 25)).run()
    app.number_input(key="launch-temporal_consensus_origins").set_value(2).run()
    assert not app.exception
    assert any("ne peuvent pas dépasser" in e.value for e in app.error)
    assert app.button[0].disabled
    assert "pending-experiment-submission" not in app.session_state
    app.number_input(key="launch-temporal_consensus_min_occurrences").set_value(2).run()
    assert not app.button[0].disabled


@pytest.mark.parametrize("label,job_type", [
    ("Préfiltre prédicteurs", JobType.PREDICTOR_PREFILTER),
    ("End-to-end", JobType.END_TO_END),
])
def test_launch_values_snapshot_and_prefilter_stage(launch_app, tmp_path, label, job_type):
    app = launch_app
    app.selectbox[0].select(label).run()
    _mode(app, "temporal_consensus")
    app.number_input(key="launch-temporal_consensus_origins").set_value(6)
    app.number_input(key="launch-temporal_consensus_step_sessions").set_value(7)
    app.number_input(key="launch-temporal_consensus_min_occurrences").set_value(2)
    app.date_input[0].set_value(date(2026, 9, 25)).run()
    assert not app.exception
    expected = resolve_consensus_origins("2026-09-25", "XNYS", 6, 7)
    assert any(", ".join(o.date().isoformat() for o in expected) in c.value for c in app.caption)
    app.button[0].click().run()
    assert not app.exception
    spec = app.session_state["pending-experiment-submission"]
    assert spec.job_type is job_type
    assert spec.prefilter_method == spec.config.prefilter_selection_mode == "temporal_consensus"
    fields = dict(temporal_consensus_origins=6, temporal_consensus_step_sessions=7,
                  temporal_consensus_min_occurrences=2)
    assert all(getattr(spec.config, f) == value for f, value in fields.items())
    # Editing this experiment must not mutate Settings.
    assert app.session_state["lab_config"].temporal_consensus_origins == 5
    repository = RunRepository(tmp_path / "runs")
    root = repository.create(spec)
    assert repository.load_spec(root) == spec
    snapshot = repository.read_json(root, "config.json")
    assert all(snapshot["rstock_config"][f] == value for f, value in fields.items())
    if job_type is JobType.END_TO_END:
        manifest = end_to_end.build_pipeline_manifest(repository, root, spec)
        child = end_to_end.build_stage_spec(repository, root, spec, "prefilter", manifest)
        assert child.job_type is JobType.PREDICTOR_PREFILTER
        assert child.prefilter_method == "temporal_consensus"
        assert all(getattr(child.config, f) == value for f, value in fields.items())
        child_id = manifest["stages"][0]["child_run_id"]
        repository.create(child, run_id=child_id)
        assert repository.load_spec(child_id) == child
