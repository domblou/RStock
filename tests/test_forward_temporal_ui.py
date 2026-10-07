import hashlib
import json
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest

from rstock.application import streamlit_app as app
from rstock.application.forward_temporal_analysis import analyze_forward_periods
from rstock.calendars import forward_market_sessions


def _write_analysis(root: Path, run_id: str):
    destination = root / "runs" / run_id / "results"
    destination.mkdir(parents=True)
    sessions = forward_market_sessions("2026-01-02", "XNYS", 21)
    models = [{"source_model_id": "model-a", "target": "AAA"}]
    observations = pd.DataFrame([
        {"source_model_id": "model-a", "session_date": session.date().isoformat(),
         "signal": index == 0, "correct_direction": index == 0,
         "directional_return": 0.02 if index == 0 else 0.0}
        for index, session in enumerate(sessions)
    ])
    exclusions = pd.DataFrame(columns=["source_model_id", "session_date"])
    period, population, daily = analyze_forward_periods(
        observations, exclusions, models, sessions
    )
    files = {"forward_period_metrics.csv": period,
             "forward_population_metrics.csv": population,
             "forward_daily_metrics.csv": daily}
    for name, frame in files.items():
        frame.to_csv(destination / name, index=False)
    (destination / "forward_analysis_manifest.json").write_text(json.dumps({
        "forward_run_id": run_id, "source_e2e_run_id": "source-e2e",
        "forward_policy": "FROZEN", "model_count_t0": 1,
        "horizon_max": 21, "checkpoints": [21],
        "artifact_digests": {
            name: hashlib.sha256((destination / name).read_bytes()).hexdigest()
            for name in files
        },
    }), encoding="utf-8")


@pytest.mark.parametrize("view", ["Synthèse", "Modèles", "Évolution population"])
def test_forward_secondary_views_use_persisted_metrics_and_column_help(tmp_path, monkeypatch, view):
    _write_analysis(tmp_path, "forward-one")
    seen = {"frames": [], "helps": [], "charts": 0, "labels": []}

    def radio(label, values, **_kwargs):
        return view if label == "Analyse Forward" else values[0]

    def dataframe(frame, **kwargs):
        seen["frames"].append(frame)
        seen["helps"].append(kwargs.get("column_config", {}))
        return SimpleNamespace(selection=SimpleNamespace(rows=[0]))

    def chart(value, **_kwargs):
        value.to_dict()
        seen["charts"] += 1

    fake = SimpleNamespace(
        session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        radio=radio, dataframe=dataframe, altair_chart=chart,
        metric=lambda *args, **kwargs: seen["labels"].append(args[0]),
        subheader=lambda *args, **_kwargs: None,
        caption=lambda *args, **_kwargs: None,
        info=lambda *args, **_kwargs: None,
        error=lambda *args, **_kwargs: pytest.fail("Unexpected artifact error"),
        column_config=SimpleNamespace(Column=lambda **kwargs: kwargs,
                                      NumberColumn=lambda **kwargs: kwargs),
    )
    monkeypatch.setattr(app, "st", fake)
    monkeypatch.setattr(app, "render_dataframe", dataframe)
    app._render_forward_temporal_results("forward-one", {"total_signals": 1})
    assert seen["frames"]
    assert seen["charts"]
    for frame, config in zip(seen["frames"], seen["helps"]):
        assert set(frame.columns) == set(config)
        assert all(config[column]["help"] for column in frame.columns)


def test_legacy_forward_does_not_recompute_when_analysis_missing(tmp_path, monkeypatch):
    messages = []
    fake = SimpleNamespace(
        session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        info=messages.append,
    )
    monkeypatch.setattr(app, "st", fake)
    app._render_forward_temporal_results("old-forward", {})
    assert messages == ["Analyse temporelle détaillée indisponible pour ce run."]


def test_end_to_end_summary_offers_forward_from_persisted_pipeline_snapshot(
    tmp_path, monkeypatch,
):
    run_id = "historical-e2e"
    results = tmp_path / "runs" / run_id / "results"
    results.mkdir(parents=True)
    (results / "pipeline_summary.json").write_text(json.dumps({
        "forward_model_snapshot": {
            "candidate_count": 1,
            "resolved_market_session_cutoff": "2026-07-06",
        },
    }), encoding="utf-8")
    buttons = []
    captions = []
    fake = SimpleNamespace(
        session_state=SimpleNamespace(lab_calendar="XNYS"),
        caption=captions.append,
        radio=lambda _label, choices, **_kwargs: choices[0],
        button=lambda label, **_kwargs: buttons.append(label) or False,
        info=lambda *_args: None,
    )
    repository = SimpleNamespace(run_directory=lambda _run_id: tmp_path / "runs" / _run_id)
    service = SimpleNamespace(run_service=SimpleNamespace(repository=repository))
    monkeypatch.setattr(app, "st", fake)
    monkeypatch.setattr(app, "_render_derived_creation", lambda *_args: None)

    app._render_pipeline_summary(run_id, {
        "summary": {"job_type": "end_to_end", "forward_simulation": None},
        "configuration": {"requested_historical_cutoff": "2026-07-06"},
        "pipeline_stages": None,
    }, service)

    assert "Lancer cette Forward Simulation" in buttons
    assert any("séance résolue : 2026-07-06" in caption for caption in captions)
