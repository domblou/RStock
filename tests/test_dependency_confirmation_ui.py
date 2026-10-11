"""Preview feedback is rendered without an extra whole-page rerun."""
import json

import pytest
from streamlit.testing.v1 import AppTest

from rstock.application.domain import JobStatus, JobType
from rstock.application.repository import RunRepository
from test_run_delete import _run


@pytest.mark.parametrize("count", [1, 2])
def test_preview_displays_confirmation_in_same_render_and_does_not_delete(tmp_path, count):
    repository = RunRepository(tmp_path / "runs")
    identifiers = [_run(repository, JobType.WALK_FORWARD, JobStatus.COMPLETED) for _ in range(count)]
    script = '''
import streamlit as st
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch
with patch("streamlit.navigation", lambda *args, **kwargs: SimpleNamespace(run=lambda: None)):
    from rstock.application import streamlit_app as app
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.services import ExperimentService
st.session_state.lab_config=SimpleNamespace(project_root=Path(ROOT))
st.session_state.setdefault("renders",0)
service=ExperimentService(RunService(RunRepository(Path(ROOT)/"runs")))
def render(records, **kwargs):
    st.session_state.renders+=1
    st.session_state[kwargs["selection_key"]]=[row["Run ID"] for row in records]
    return {"selection":{"rows":list(range(len(records)))}}
with patch.object(app,"_history_model_contexts",lambda _:{}), patch.object(app,"_universe_service",lambda:SimpleNamespace(records=lambda:[])), patch("rstock.application.history_grid.render_history_grid",render):
    app._history_runs_panel(service,allowed_types=app.EXPERIMENT_JOB_TYPES,key_prefix="confirmation-test")
'''.replace("ROOT", repr(str(tmp_path)))
    at = AppTest.from_string(script).run()
    assert not at.exception
    renders = at.session_state["renders"]
    key = "delete-history-confirmation-test" if count == 1 else "delete-selected-confirmation-test"
    at.button(key=key).click().run()
    assert not at.exception
    assert at.session_state["renders"] == renders + 1
    assert any("irréversible" in warning.value for warning in at.warning)
    assert any("Confirmer la suppression définitive" in button.label for button in at.button)
    assert all(repository.run_directory(run_id).exists() for run_id in identifiers)
