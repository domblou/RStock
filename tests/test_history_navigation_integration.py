"""Exercise the real History fragment, widgets and lazy records in Streamlit."""
from streamlit.testing.v1 import AppTest
from test_history_index import make_run


def test_history_fragment_filters_paginates_and_preserves_off_page_selection(tmp_path):
    root = tmp_path / "runs"
    for number in range(60):
        make_run(root, f"run-{number:03}")
    script = '''
import streamlit as st
from types import SimpleNamespace
from unittest.mock import patch
from pathlib import Path
with patch("streamlit.navigation", lambda *args, **kwargs: SimpleNamespace(run=lambda: None)):
    from rstock.application import streamlit_app as app
from rstock.application.services import ExperimentService
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
st.session_state.lab_config = SimpleNamespace(project_root=Path(ROOT))
st.session_state.setdefault("wanted", ["run-059"])
service=ExperimentService(RunService(RunRepository(Path(ROOT)/"runs")))
def render(records, **kwargs):
    return {"selection": {"rows": [i for i,row in enumerate(records) if row["Run ID"] in st.session_state.wanted]}}
with patch.object(app, "_history_model_contexts", lambda _: {}), patch.object(app, "_universe_service", lambda: SimpleNamespace(records=lambda: [])), patch("rstock.application.history_grid.render_history_grid", render):
    app._history_runs_panel(service, allowed_types=app.EXPERIMENT_JOB_TYPES, key_prefix="qa-history")
'''.replace("ROOT", repr(str(tmp_path)))
    at = AppTest.from_string(script).run()
    assert not at.exception
    assert at.session_state["qa-history-selected-runs"] == ["run-059"]
    at.session_state["wanted"] = ["run-034"]
    at.number_input(key="qa-history-page").set_value(2).run()
    assert not at.exception
    assert at.session_state["qa-history-selected-runs"] == ["run-059", "run-034"]
    at.session_state["wanted"] = []
    at.run()
    assert not at.exception
    assert at.session_state["qa-history-selected-runs"] == ["run-059"]
    at.selectbox(key="qa-history-status").set_value("Tous").run()
    at.selectbox(key="qa-history-storage").set_value("Résumé seulement").run()
    assert not at.exception
    assert at.session_state["qa-history-selected-runs"] == []
    assert any("Aucun run" in element.value for element in at.info)
