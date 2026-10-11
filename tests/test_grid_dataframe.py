from datetime import date
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest
import streamlit as st

from rstock.application.grid_dataframe import dataframe, prepare_dataframe


def test_preserves_every_column_help_formats_hidden_columns_and_chart_bounds():
    configs = {
        "secret": None,
        "Price": st.column_config.NumberColumn("Prix", help="Prix scientifique exact.", width="small", format="%.2f $"),
        "Rate": st.column_config.NumberColumn(help="Taux", format="%+.2%"),
        "Progress": st.column_config.ProgressColumn(help="Avancement global", min_value=0, max_value=100, format="%.1f%%"),
        "Trend": st.column_config.LineChartColumn(help="Série complète", y_min=-1, y_max=1, color="#2563eb"),
    }
    table = pd.DataFrame([{"secret": "id", "Price": 12.345, "Rate": .125, "Progress": 26.6, "Trend": [0, .3, None, .2]}])
    rows, columns, options, _, all_columns = prepare_dataframe(table, column_config=configs)
    assert "secret" not in columns and "secret" in all_columns
    for name, config in configs.items():
        if config:
            assert options[name]["help"] == config["help"]
    assert rows[0][options["Price"]["display_key"]] == "12.35 $"
    assert rows[0][options["Rate"]["display_key"]] == "+12.50%"
    assert rows[0][options["Progress"]["display_key"]] == "26.6%"
    assert options["Trend"]["type"] == "line_chart"
    assert options["Trend"]["y_min"] == -1 and options["Trend"]["y_max"] == 1
    assert rows[0]["Trend"] == [0, .3, None, .2]


def test_styler_formats_and_conditional_styles_are_preserved():
    table = pd.DataFrame({"Gain": [1.23, -2.34], "Rate": [.5, .75]})
    styled = table.style.map(lambda value: "color: green; font-weight: 700" if value > 0 else "color: red", subset=["Gain"]).format({"Gain": "{:+.1f}"})
    rows, _, options, styles, _ = prepare_dataframe(styled, column_config={"Rate": st.column_config.NumberColumn(format="percent", help="Taux original")})
    assert rows[0][options["Gain"]["display_key"]] == "+1.2"
    assert styles[rows[0]["__grid_id"]]["Gain"] == {"color": "green", "font-weight": "700"}
    assert styles[rows[1]["__grid_id"]]["Gain"] == {"color": "red"}
    assert rows[0][options["Rate"]["display_key"]] == "50.00%"


def test_dataframe_adapter_returns_native_shaped_selection_and_stable_ids():
    table = pd.DataFrame({"model_id": ["m-a", "m-b"], "Value": [10, 20]})
    calls = []
    def component(**kwargs):
        calls.append(kwargs)
        return ["m-b:0"]
    event = dataframe(table, key="models", on_select="rerun", selection_mode="single-row", component=component)
    assert event.selection.rows == [1]
    reversed_event = dataframe(table.iloc[::-1], key="models", on_select="rerun", component=component)
    assert reversed_event.selection.rows == [0]
    assert calls[0]["selection_mode"] == "single"
    assert calls[0]["page_size"] == 25


def test_explicit_domain_ids_maintain_identity_across_metric_updates():
    rows, *_ = prepare_dataframe(pd.DataFrame({"Name": ["Universe"], "Size": [10]}), row_ids=["universe-id"])
    updated, *_ = prepare_dataframe(pd.DataFrame({"Name": ["Universe"], "Size": [20]}), row_ids=["universe-id"])
    assert rows[0]["__grid_id"] == updated[0]["__grid_id"] == "universe-id"


def test_numpy_dates_nullable_and_empty_data():
    rows, _, options, *_ = prepare_dataframe(pd.DataFrame({"Integer": pd.Series([12, pd.NA], dtype="Int64"), "Date": [date(2026, 7, 6), pd.NaT], "Bool": np.array([True, False])}))
    assert rows[0]["Integer"] == 12 and isinstance(rows[0]["Integer"], int)
    assert rows[1]["Integer"] is None and rows[1]["Date"] is None
    assert rows[0]["Bool"] is True and rows[1]["Bool"] is False
    assert rows[0]["Date"] == "2026-07-06"
    assert prepare_dataframe(pd.DataFrame(columns=["Empty"]))[0] == []


def test_unknown_specialized_renderer_fails_explicitly():
    with pytest.raises(ValueError, match="renderer unavailable"):
        prepare_dataframe(pd.DataFrame({"Image": ["data"]}), column_config={"Image": {"type_config": {"type": "image"}}})


def test_unsupported_format_does_not_silently_degrade():
    with pytest.raises(ValueError, match="requires a renderer"):
        prepare_dataframe(pd.DataFrame({"Score": [1]}), column_config={"Score": st.column_config.NumberColumn(format="compact")})
