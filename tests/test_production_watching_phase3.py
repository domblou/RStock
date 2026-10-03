"""Presentation of immutable observation and production periods."""

from pathlib import Path

import pandas as pd
import pytest

from rstock.application.model_ui import MODEL_STATUS_ORDER, model_status_label
from rstock.application.production_quality import normalize_quality_observations
from rstock.application.production_quality_ui import model_phase_metrics


APP = Path(__file__).resolve().parents[1] / "rstock" / "application" / "streamlit_app.py"


def test_model_phase_metrics_use_frozen_context_after_activation():
    observations = normalize_quality_observations(pd.DataFrame([
        {
            "prediction_id": "watch", "model_id": "one", "model_version": 1,
            "session_date": "2026-09-22", "prediction_origin": "scheduled_live",
            "model_status_at_prediction": "watching", "evaluation_status": "evaluated",
            "is_bullish_signal": True, "intraday_return": 0.02,
        },
        {
            "prediction_id": "active", "model_id": "one", "model_version": 1,
            "session_date": "2026-09-23", "prediction_origin": "scheduled_live",
            "model_status_at_prediction": "active", "evaluation_status": "evaluated",
            "is_bullish_signal": True, "intraday_return": -0.01,
        },
        {
            "prediction_id": "legacy", "model_id": "one", "model_version": 1,
            "session_date": "2026-09-24", "prediction_origin": "legacy_inferred_live",
            "evaluation_status": "evaluated", "is_bullish_signal": True,
            "intraday_return": 0.03,
        },
    ]))
    series = pd.DataFrame({"session_date": pd.bdate_range("2026-09-22", periods=3)})

    phases = model_phase_metrics(observations, series)

    assert phases["En observation"]["signal_count"] == 1
    assert phases["En observation"]["pnl"] == pytest.approx(200)
    assert phases["Production active"]["signal_count"] == 1
    assert phases["Production active"]["pnl"] == pytest.approx(-100)
    assert phases["Historique sans contexte"]["signal_count"] == 1
    assert phases["Historique sans contexte"]["pnl"] == pytest.approx(300)
    assert phases["Historique complet"]["signal_count"] == 3
    assert phases["Historique complet"]["pnl"] == pytest.approx(400)
    assert observations.iloc[0]["model_status_at_prediction"] == "watching"


def test_watching_status_filter_actions_and_distinct_surveillance_section():
    source = APP.read_text(encoding="utf-8")
    models = source.split("def _models_page", 1)[1].split("def _history_page", 1)[0]
    detail = source.split("def _render_model_quality_detail", 1)[1].split(
        "def _models_page", 1
    )[0]
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "def _surveillance_page", 1
    )[0]
    watching = source.split("def _render_watching_surveillance_section", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert "watching" in MODEL_STATUS_ORDER
    assert model_status_label("watching") == "En observation"
    assert "status_options" in models and "format_func=model_status_label" in models
    assert "service.watch(selected_id)" in models
    assert "Passer en observation" in models
    assert 'selected.status.value not in {"trained", "active"}' in models
    assert "service.activate(selected_id)" in models
    assert "service.deactivate(selected_id)" in models
    assert "model_phase_comparison_table(detail.observations, detail.series)" in detail
    assert "model_status_at_prediction" in detail
    assert 'st.subheader("Production")' in surveillance
    assert "_render_watching_surveillance_section(" in surveillance
    assert "watching_history()" in watching
    assert "watching_realized_results()" in watching
    assert "surveillance_display_session(" in watching
    assert "next_session_signals_view(" in watching
    assert "surveillance_display_session(" in surveillance
    assert "_render_surveillance_model_history(project_root)" in surveillance
    assert "_render_real_trade_from_prediction" not in watching
