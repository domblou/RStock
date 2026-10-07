import ast
from pathlib import Path

import pandas as pd
from streamlit.testing.v1 import AppTest

from rstock.application import streamlit_app as app
from rstock.application.grid_dataframe import prepare_dataframe


def test_all_dataframe_render_sites_use_shared_adapter():
    tree = ast.parse(Path(app.__file__).read_text(encoding="utf-8"))
    native = [node for node in ast.walk(tree) if isinstance(node, ast.Call)
              and isinstance(node.func, ast.Attribute) and isinstance(node.func.value, ast.Name)
              and node.func.value.id == "st" and node.func.attr in {"dataframe", "table", "data_editor"}]
    shared = [node for node in ast.walk(tree) if isinstance(node, ast.Call)
              and isinstance(node.func, ast.Name) and node.func.id == "render_dataframe"]
    assert native == []
    assert len(shared) == 82


def test_all_existing_column_help_catalogs_are_carried_verbatim():
    catalogs = {name: value for name, value in vars(app).items() if name.endswith("COLUMN_HELP") and isinstance(value, dict)}
    assert len(catalogs) >= 10
    for descriptions in catalogs.values():
        frame = pd.DataFrame(columns=list(descriptions))
        configs = app._grid_column_help_config(frame.columns, descriptions)
        _, _, options, _, _ = prepare_dataframe(frame, column_config=configs)
        assert {name: option["help"] for name, option in options.items()} == descriptions
    for configs in (app._models_grid_column_config(), app._model_detail_column_config(app._MODEL_DETAIL_WINDOWS_HELP)):
        _, _, options, _, _ = prepare_dataframe(pd.DataFrame(columns=list(configs)), column_config=configs)
        for name, config in configs.items():
            if config is not None:
                assert options[name]["help"] == config["help"]


def test_real_streamlit_multiple_grids_same_call_site_have_distinct_ids():
    at = AppTest.from_string('''
import streamlit as st
import pandas as pd
from rstock.application.grid_dataframe import dataframe
for title in ("First", "Second"):
    st.subheader(title)
    dataframe(pd.DataFrame({"Value": [1]}), hide_index=True)
with st.columns(2)[0]:
    dataframe(pd.DataFrame({"Value": [1]}), hide_index=True)
with st.columns(2)[1]:
    dataframe(pd.DataFrame({"Value": [1]}), hide_index=True)
''').run()
    assert not at.exception
    components = at.get("component_instance")
    assert len(components) == 4
    assert len({element.proto.id for element in components}) == 4
    at.run()
    assert not at.exception


def test_universes_real_page_selection_opens_detail_and_deselection_hides_it():
    at = AppTest.from_string('''
import streamlit as st
from types import SimpleNamespace
from unittest.mock import patch
from rstock.application import streamlit_app as app
from rstock.application.grid_dataframe import dataframe
records=[SimpleNamespace(universe_id="u1", name="First", symbols=["AAPL"], type=app.STANDARD_UNIVERSE_TYPE, benchmark_symbol=None, source="file", updated_at=None),
         SimpleNamespace(universe_id="u2", name="Second", symbols=["MSFT"], type=app.STANDARD_UNIVERSE_TYPE, benchmark_symbol=None, source="file", updated_at=None)]
st.session_state.setdefault("desired_selection", ["u2"])
def render(data, **kwargs):
    return dataframe(data, **kwargs, component=lambda **_: st.session_state.desired_selection)
with patch.object(app, "_universe_service", lambda: SimpleNamespace(records=lambda: records)), \
     patch.object(app, "_create_universe_panel", lambda service: None), \
     patch.object(app, "_universe_detail", lambda service, identity: st.text("DETAIL " + identity)), \
     patch.object(app, "_page_header", lambda title: st.header(title)), \
     patch.object(app, "render_dataframe", render):
    app._universes_page()
''').run()
    assert not at.exception
    assert at.session_state["selected_universe_id"] == "u2"
    assert [value.value for value in at.text] == ["DETAIL u2"]
    at.session_state["desired_selection"] = []
    at.run()
    assert not at.exception
    assert not at.text
    assert any("Sélectionnez un univers" in value.value for value in at.caption)
