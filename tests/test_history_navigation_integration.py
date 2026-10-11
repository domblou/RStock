"""Exercise the real History fragment, component state and action buttons."""
import json

from streamlit.testing.v1 import AppTest
from test_history_index import make_run


def history_app(tmp_path, count):
    for number in range(count):
        make_run(tmp_path / "runs", f"run-{number:03}")
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
service=ExperimentService(RunService(RunRepository(Path(ROOT)/"runs")))
with patch.object(app, "_history_model_contexts", lambda _: {}), patch.object(app, "_universe_service", lambda: SimpleNamespace(records=lambda: [])):
    app._history_runs_panel(service, allowed_types=app.EXPERIMENT_JOB_TYPES, key_prefix="qa-history")
'''.replace("ROOT", repr(str(tmp_path)))
    at = AppTest.from_string(script).run()
    assert not at.exception
    return at


def grid_args(at):
    return json.loads(at.get("component_instance")[0].proto.json_args)


def click(at, revision, ids, *, context=None):
    args = grid_args(at)
    message = dict(client_id="browser", revision=revision,
                   context=args["selection_sync"]["context"] if context is None else context,
                   selected_ids=ids)
    # Inject the same protobuf widget value Streamlit receives from the iframe.
    states = at._tree.get_widget_states()
    widget = states.widgets.add()
    widget.id = at.get("component_instance")[0].proto.id
    widget.json_value = json.dumps(message)
    at._run(states)
    assert not at.exception
    return grid_args(at)


def test_history_run_2_then_run_1_selection_and_actions_are_current_in_same_render(tmp_path):
    at = history_app(tmp_path, 3)
    ids = [row["Run ID"] for row in grid_args(at)["rows"]]
    args = click(at, 1, [ids[1]])
    assert args["selected_ids"] == [ids[1]]
    assert at.button(key="open-history-qa-history")
    args = click(at, 2, [ids[1], ids[0]])
    assert set(args["selected_ids"]) == {ids[0], ids[1]}
    assert set(at.session_state["qa-history-selected-runs"]) == {ids[0], ids[1]}
    assert at.button(key="compare-history-qa-history")
    # Refresh and replay of an older message cannot deselect the first run.
    at.button(key="qa-history-refresh").click().run()
    assert set(grid_args(at)["selected_ids"]) == {ids[0], ids[1]}
    args = click(at, 1, [ids[1]])
    assert set(args["selected_ids"]) == {ids[0], ids[1]}
    # Coalesced rapid clicks then explicit deselection.
    args = click(at, 8, ids)
    assert set(args["selected_ids"]) == set(ids)
    assert at.button(key="compare-history-qa-history")
    args = click(at, 9, [ids[0]])
    assert args["selected_ids"] == [ids[0]]
    assert at.button(key="open-history-qa-history")
    args = click(at, 10, [])
    assert args["selected_ids"] == []
    assert not any(button.label in ("Comparer les runs", "Ouvrir le run") for button in at.button)


def test_history_fragment_filters_paginates_and_preserves_off_page_selection(tmp_path):
    at = history_app(tmp_path, 60)
    old_context = grid_args(at)["selection_sync"]["context"]
    click(at, 1, ["run-059"])
    at.number_input(key="qa-history-page").set_value(2).run()
    assert not at.exception
    assert grid_args(at)["selected_ids"] == ["run-059"]
    click(at, 2, ["run-034"])
    assert at.session_state["qa-history-selected-runs"] == ["run-059", "run-034"]
    assert at.button(key="compare-history-qa-history")
    # Even a higher revision from an obsolete page is ignored.
    click(at, 50, [], context=old_context)
    assert at.session_state["qa-history-selected-runs"] == ["run-059", "run-034"]
    click(at, 3, [])
    assert at.session_state["qa-history-selected-runs"] == ["run-059"]
    at.number_input(key="qa-history-page").set_value(1).run()
    assert grid_args(at)["selected_ids"] == ["run-059"]
    at.selectbox(key="qa-history-storage").set_value("Résumé seulement").run()
    assert not at.exception
    assert at.session_state["qa-history-selected-runs"] == []
    assert any("Aucun run" in element.value for element in at.info)
    at.selectbox(key="qa-history-storage").set_value("Complet").run()
    assert not at.exception
    assert grid_args(at)["selected_ids"] == []
