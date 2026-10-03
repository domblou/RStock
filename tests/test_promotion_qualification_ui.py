import json
from contextlib import nullcontext
from pathlib import Path
from types import SimpleNamespace

import pandas as pd

from rstock.application.history_analysis import filter_threshold_calibration_results
from rstock.application.promotion_qualification_ui import (
    DEFAULT_PROMOTION_SORT, sort_promotion_decisions, upstream_diagnostic,
)
from rstock.application import streamlit_app


def _decision_rows():
    return pd.DataFrame([
        {"Combinaison": "B", "Statut promotion": "Non candidat", "Précision holdout": 0.95},
        {"Combinaison": "C", "Statut promotion": "Candidat", "Précision holdout": 0.60},
        {"Combinaison": "A", "Statut promotion": "Candidat", "Précision holdout": 0.80},
        {"Combinaison": "D", "Statut promotion": "Non candidat", "Précision holdout": 0.50},
    ])


def test_default_promotion_sort_groups_status_then_descending_holdout_precision():
    original = _decision_rows()

    sorted_rows = sort_promotion_decisions(original)

    assert sorted_rows["Combinaison"].tolist() == ["A", "C", "B", "D"]
    assert original["Combinaison"].tolist() == ["B", "C", "A", "D"]
    assert sorted_rows.columns.tolist() == original.columns.tolist()
    assert filter_threshold_calibration_results(
        original, direction="Toutes", sort_by=DEFAULT_PROMOTION_SORT,
    )["Combinaison"].tolist() == ["A", "C", "B", "D"]
    assert filter_threshold_calibration_results(
        original, direction="Toutes", sort_by="Précision holdout",
    )["Combinaison"].tolist() == ["B", "A", "C", "D"]


def test_upstream_diagnostic_reads_frozen_wf_and_selected_calibration(tmp_path):
    root = tmp_path / "runs"
    wf = root / "wf" / "results"
    calibration = root / "threshold" / "results"
    wf.mkdir(parents=True)
    calibration.mkdir(parents=True)
    pd.DataFrame([
        {"Set": "A", "ROCAUCMedian": 0.63, "ROCAUCWorst": 0.51, "ROCAUCStd": 0.04},
    ]).to_csv(wf / "qualification.csv", index=False)
    (root / "threshold" / "config.json").write_text(
        json.dumps({"source_walk_forward_run": "wf"}), encoding="utf-8"
    )
    (calibration / "selected_thresholds_by_set.json").write_text(json.dumps({
        "A": {"Up": {"total_signals": 87, "calibration_metrics": {
            "precision": 0.71, "directional_return_mean": 0.012,
        }}}
    }), encoding="utf-8")
    pd.DataFrame([{
        "Set": "A", "Direction": "Up", "Selected": True,
        "SignalCountsByWindow": "[9, 8, 14, 15, 14, 10, 17]",
    }]).to_csv(calibration / "threshold_metrics_by_set.csv", index=False)

    values = upstream_diagnostic(
        tmp_path, {"Combinaison": "A", "Direction": "Up"},
        threshold_run_id="threshold",
    )

    assert values == {
        "wf_median_auc": 0.63,
        "wf_worst_auc": 0.51,
        "wf_auc_std": 0.04,
        "signal_counts_by_window": [9, 8, 14, 15, 14, 10, 17],
        "calibration_total_signals": 87,
        "calibration_precision": 0.71,
        "calibration_directional_return_mean": 0.012,
    }


def test_upstream_diagnostic_does_not_infer_missing_or_ambiguous_metrics(tmp_path):
    results = tmp_path / "runs" / "threshold" / "results"
    results.mkdir(parents=True)
    (results / "selected_thresholds_by_set.json").write_text(json.dumps({
        "A": {"Up": {"total_signals": 0, "calibration_metrics": {}}}
    }), encoding="utf-8")
    pd.DataFrame([
        {"Set": "A", "Direction": "Up", "Selected": True, "SignalCountsByWindow": "[1, 2]"},
        {"Set": "A", "Direction": "Up", "Selected": True, "SignalCountsByWindow": "[3, 4]"},
    ]).to_csv(results / "threshold_metrics_by_set.csv", index=False)

    values = upstream_diagnostic(
        tmp_path, {"Combinaison": "A", "Direction": "Up"},
        threshold_run_id="threshold",
    )

    assert values["calibration_total_signals"] == 0
    assert values["signal_counts_by_window"] is None
    assert values["calibration_precision"] is None
    assert values["wf_median_auc"] is None


def test_forced_qualification_uses_explicit_reference_calibration_for_window_counts(tmp_path):
    root = tmp_path / "runs"
    fixed = root / "fixed"
    reference = root / "reference" / "results"
    (fixed / "results").mkdir(parents=True)
    reference.mkdir(parents=True)
    (fixed / "config.json").write_text(json.dumps({
        "source_threshold_calibration_run": "reference",
    }), encoding="utf-8")
    (fixed / "results" / "selected_thresholds_by_set.json").write_text(json.dumps({
        "A": {"Up": {"total_signals": 25, "calibration_metrics": {
            "precision": 0.7, "directional_return_mean": 0.01,
        }}}
    }), encoding="utf-8")
    pd.DataFrame([{
        "Set": "A", "Direction": "Up", "Selected": True,
        "SignalCountsByWindow": "[5, 5, 5, 5, 5]",
    }]).to_csv(reference / "threshold_metrics_by_set.csv", index=False)

    values = upstream_diagnostic(
        tmp_path, {"Combinaison": "A", "Direction": "Up"},
        threshold_run_id="fixed",
    )

    assert values["signal_counts_by_window"] == [5, 5, 5, 5, 5]
    assert values["calibration_precision"] == 0.7


def test_shared_grid_shows_panel_only_for_selected_candidate(monkeypatch, tmp_path):
    displayed = []
    panels = []

    def dataframe(table, **_kwargs):
        displayed.append(table["Combinaison"].tolist())
        rows = [0] if len(displayed) == 1 else []
        return SimpleNamespace(selection=SimpleNamespace(rows=rows))

    monkeypatch.setattr(streamlit_app.st, "dataframe", dataframe)
    monkeypatch.setattr(
        streamlit_app, "_render_upstream_qualification_diagnostic",
        lambda chosen, **kwargs: panels.append((chosen["Combinaison"], kwargs)),
    )

    selected = streamlit_app._render_qualification_decision_grid(
        _decision_rows(), key="standalone", project_root=tmp_path,
        qualification={"source_threshold_calibration_run": "threshold"},
    )
    absent = streamlit_app._render_qualification_decision_grid(
        _decision_rows(), key="e2e", project_root=tmp_path,
    )

    assert displayed == [["A", "C", "B", "D"], ["A", "C", "B", "D"]]
    assert selected["Combinaison"] == "A"
    assert absent is None
    assert len(panels) == 1
    assert panels[0][1]["threshold_run_id"] == "threshold"


def test_upstream_panel_formats_full_counts_and_missing_values(monkeypatch, tmp_path):
    metrics = []

    class Column:
        def metric(self, label, value, **kwargs):
            metrics.append((label, value, kwargs.get("help")))

    monkeypatch.setattr(streamlit_app.st, "container", lambda **_kwargs: nullcontext())
    monkeypatch.setattr(streamlit_app.st, "markdown", lambda *_args: None)
    monkeypatch.setattr(
        streamlit_app.st, "columns",
        lambda spec, **_kwargs: [Column() for _ in range(len(spec) if isinstance(spec, tuple) else spec)],
    )
    monkeypatch.setattr(streamlit_app, "upstream_diagnostic", lambda *_args, **_kwargs: {
        "wf_median_auc": None, "wf_worst_auc": 0.48, "wf_auc_std": None,
        "signal_counts_by_window": [9, 8, 14, 15, 14, 10, 17],
        "calibration_total_signals": None, "calibration_precision": 0.62,
        "calibration_directional_return_mean": None,
    })

    streamlit_app._render_upstream_qualification_diagnostic(
        {"Combinaison": "A"}, project_root=tmp_path,
        threshold_run_id="threshold", walk_forward_run_id="wf",
    )

    values = {label: value for label, value, _help in metrics}
    assert values["Signaux par fenêtre"] == "[9, 8, 14, 15, 14, 10, 17]"
    assert values["AUC médiane WF"] == "—"
    assert values["Total signaux calibration"] == "—"
    assert values["Précision calibration"] == "62.00%"
    assert all(help_text for label, _value, help_text in metrics if label in {
        "Pire AUC WF", "Dispersion AUC", "Précision calibration",
        "Rendement directionnel moyen calibration",
    })


def test_all_promotion_decision_views_use_shared_grid():
    source = Path(streamlit_app.__file__).read_text(encoding="utf-8")
    standalone = source.split("def _render_standard_results(", 1)[1].split(
        "def _render_standard_job_tabs(", 1
    )[0]
    pipeline_child = source.split("def _render_pipeline_child(", 1)[1].split(
        "def _render_pipeline_child_technical(", 1
    )[0]
    promotion = source.split("def _render_pipeline_promotion(", 1)[1].split(
        "def _render_candidate_identity_stability(", 1
    )[0]
    legacy = source.split("def _render_threshold_calibration_promotion(", 1)[1].split(
        "def _format_metric(", 1
    )[0]

    assert '_render_qualification_decision_grid(' in standalone
    assert '_render_job_detail_tabs(' in pipeline_child
    assert '_render_qualification_decision_grid(' in promotion
    assert '_render_qualification_decision_grid(' in legacy
    assert 'DEFAULT_PROMOTION_SORT' in legacy
