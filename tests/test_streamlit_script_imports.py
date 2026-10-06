"""Run the actual Streamlit entry file outside its Python package."""
import ast
from dataclasses import replace
from datetime import date
import inspect
from pathlib import Path
from types import SimpleNamespace

import pytest
import streamlit as st
from streamlit.testing.v1 import AppTest

from rstock.application import streamlit_app as ui
from rstock.application import workflows
from rstock.application.domain import JobType
from rstock.application.universes import UniverseSelection
from test_prefilter_consensus import _consensus_source
from test_prefilter_experiments import _complete


APP = Path(ui.__file__)


def test_streamlit_entry_has_no_package_relative_imports():
    # This file is executed by Streamlit as __main__, including nested functions.
    tree = ast.parse(APP.read_text(encoding="utf-8"))
    assert not [node for node in ast.walk(tree)
                if isinstance(node, ast.ImportFrom) and node.level]


@pytest.mark.parametrize("label", ["Walk-forward", "Préfiltre prédicteurs", "End-to-end"])
def test_experiments_from_real_streamlit_entry(tmp_path, monkeypatch, label):
    repository, source_spec, source, summary, _ = _consensus_source(tmp_path, monkeypatch)
    _complete(repository, source, summary)
    service = SimpleNamespace(run_service=SimpleNamespace(repository=repository))
    executions = []

    class ExperimentsPage:
        def run(self):
            # Streamlit executes the actual file, not an import of streamlit_app.
            namespace = inspect.currentframe().f_back.f_globals
            assert namespace["__name__"] == "__main__"
            assert not namespace.get("__package__")
            executions.append(True)
            namespace["_render_locked_duplication_mode"] = lambda *_: False
            def editable_universe():
                assert st.session_state.lab_symbols != ["STALE"]
                return True
            namespace["_experiment_universe_selector"] = editable_universe
            namespace["_live_job_panel"] = lambda *a, **k: None
            namespace["_render_experiment_submission_confirmation"] = lambda *a: None
            # Keep the real _experiments, preview and import paths.
            namespace["_experiments"](service)

    monkeypatch.setattr(st, "navigation", lambda *a, **k: ExperimentsPage())
    monkeypatch.setattr(ui.MarketDataService, "available_symbols", lambda *a: [])
    app = AppTest.from_file(str(APP))
    initial_state = dict(
        lab_config=source_spec.config, lab_calendar="XNYS",
        lab_symbols=["STALE"],
        lab_target_symbols=["STALE"],
        lab_context_symbols=[],
        lab_combinations_per_target=1, lab_evaluate_holdout=False,
        lab_universe_selection=UniverseSelection(), lab_market_benchmark_symbol=None,
        lab_context_universe_ids=[], lab_context_sample_size=None,
        lab_context_selection_method=None, lab_context_seed=None,
    )
    for key, value in initial_state.items():
        app.session_state[key] = value
    app.run()
    assert not app.exception
    app.selectbox[0].select(label).run()
    assert not app.exception
    if label == "Walk-forward":
        app.selectbox(key="launch-wf-prefilter-source").select(source).run()
        assert not app.exception
        assert not app.date_input
        inherited = app.text_input(key="launch-wf-inherited-cutoff")
        assert inherited.disabled and inherited.value == "2026-09-25"
        assert not app.multiselect
        assert not any(s.label == "Sélection de l’univers principal" for s in app.selectbox)
        assert any("Univers hérités du Préfiltre" in s.value for s in app.subheader)
        assert app.session_state["lab_target_symbols"] == list(source_spec.target_symbols)
        assert app.session_state["lab_symbols"] == list(source_spec.predictor_symbols)
        app.session_state["pending-experiment-submission"] = replace(
            source_spec, job_type=JobType.WALK_FORWARD, prefilter_method="single_origin",
            source_prefilter_run=source,
            target_symbols=("BBB",), context_symbols=("AAA", "CCC", "DDD"),
        )
        app.run()
        assert not app.exception
        assert "pending-experiment-submission" not in app.session_state
        preview = app.session_state["experiment-combination-preview"]
        assert preview.effective_combination_count == 1
        assert any(m.label == "Combinaisons figées du Préfiltre" for m in app.metric)
        other_spec = replace(source_spec, historical_data_cutoff="2026-09-18",
                             context_symbols=tuple(reversed(source_spec.context_symbols)),
                             context_universe_ids=("another-context",), context_seed=42)
        other = repository.create(other_spec)
        other_summary = workflows._predictor_prefilter(
            other_spec, repository.run_directory(other) / "results", None, None,
        )
        _complete(repository, other, other_summary)
        app.run()
        app.selectbox(key="launch-wf-prefilter-source").select(other).run()
        assert not app.exception
        assert app.text_input(key="launch-wf-inherited-cutoff").value == "2026-09-18"
        assert app.session_state["lab_context_universe_ids"] == ["another-context"]
        assert app.session_state["lab_symbols"] == list(other_spec.predictor_symbols)
        source = other
    else:
        mode = app.radio(key="launch-prefilter-method")
        assert "Consensus temporel" in mode.options
        mode.set_value("temporal_consensus").run()
        app.date_input[0].set_value(date(2026, 9, 25)).run()
        assert not app.exception
        assert any("Cutoffs prévus" in c.value for c in app.caption)
    app.button[0].click().run()
    assert not app.exception
    pending = app.session_state["pending-experiment-submission"]
    if label == "Walk-forward":
        assert pending.source_prefilter_run == source
        assert pending.source_prefilter_contract_sha256
        assert pending.historical_data_cutoff == pending.resolved_market_session_cutoff == "2026-09-18"
        assert pending.requested_historical_cutoff is None
        assert pending.target_symbols == other_spec.target_symbols
        assert pending.predictor_symbols == other_spec.predictor_symbols
        assert pending.context_symbols == other_spec.context_symbols
        assert pending.context_universe_ids == other_spec.context_universe_ids
        assert pending.context_seed == 42
    else:
        assert pending.prefilter_method == "temporal_consensus"
        assert pending.config.temporal_consensus_origins == source_spec.config.temporal_consensus_origins
    assert executions
