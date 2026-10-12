"""Render the real form without the application's operational entry point."""
from pathlib import Path
from types import ModuleType
from dataclasses import replace
from datetime import date
import streamlit as st

root = Path(__file__).resolve().parents[2]
source_path = root/'rstock/application/streamlit_app.py'
source = source_path.read_text(encoding='utf-8').rsplit('\n_configure_top_navigation_spacing()', 1)[0]
app = ModuleType('logistic_operational_form_app')
app.__file__ = str(source_path)
exec(compile(source, str(source_path), 'exec'), app.__dict__)
st.session_state.setdefault('lab_config', replace(app.DEFAULT_CONFIG,
    project_root=root/'reports/adhoc/logistic_phase4', predictive_algorithm='logistic_l2', predictor_prefilter_enabled=False))
for name, value in app.UI_SETTINGS_DEFAULTS.items():
    st.session_state.setdefault(name, value)
st.session_state.setdefault('lab_calendar', 'XNYS')
app._render_locked_duplication_mode = lambda *a: False
app._experiment_universe_selector = lambda: False
app._live_job_panel = lambda *a, **k: False
selectbox = st.selectbox
date_input = st.date_input
def select(label, options, **kwargs):
    if label == 'Type de job':
        kwargs['index'] = list(options).index('End-to-end')
    return selectbox(label, options, **kwargs)
def dates(label, **kwargs):
    if label == 'Cutoff historique':
        kwargs['value'] = st.session_state.get('fixture_cutoff', date(2026, 6, 22))
    return date_input(label, **kwargs)
st.selectbox, st.date_input = select, dates
try:
    app._experiments(None)
finally:
    st.selectbox, st.date_input = selectbox, date_input
