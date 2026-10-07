import json
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest

from rstock.application import forward_diagnostic as diagnostic
from rstock.application.forward_temporal_analysis import analyze_forward_periods
from rstock.application.run_comparison import comparison_types
from rstock.calendars import forward_market_sessions


def _json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding="utf-8")


def _model(name):
    return {"source_model_id": name, "set": json.dumps(["AAA", name], separators=(",", ":")),
            "target": "AAA", "direction": "Up", "up_threshold": 0.6, "down_threshold": 0.3}


def _fixture(tmp_path, *, derived=False, horizon=63):
    root = tmp_path / "runs"
    parent, source = root / "parent", root / ("derived" if derived else "parent")
    original = [_model("common"), _model("removed")]
    models = [_model("common"), _model("additional")] if derived else original
    for run, population in [(parent, original), (source, models)]:
        _json(run / "config.json", {"derivation": {"source_end_to_end_run_id": "parent",
              "overrides": [{"field": "promotion_min_holdout_signals", "old_value": 10, "new_value": 7}]} if run != parent else None})
        _json(run / "results/forward_model_snapshot.json", {"source_end_to_end_run_id": run.name,
              "resolved_market_session_cutoff": "2026-01-02", "models": population})
    for run, population, qid in [(parent, original, "q-parent"), (source, models, "q-derived" if derived else "q-parent")]:
        stages = [{"stage_key": key, "child_run_id": value} for key, value in
                  [("walk_forward", "wf"), ("threshold_calibration", "threshold"), ("holdout_evaluation", "holdout"), ("promotion_qualification", qid)]]
        if run != parent:
            for stage in stages[:-1]:
                stage.update(mode="inherited", source_run_id=stage.pop("child_run_id"))
            stages[-1]["mode"] = "executed"
        _json(run / "orchestration/pipeline.json", {"schema_version": 4 if run != parent else 3, "stages": stages})
        keys = {m["set"] for m in population}
        _json(root / qid / "results/qualification.json", {"decisions": [
            {"Combinaison": m["set"], "Direction": "Up", "candidate": m["set"] in keys,
             "reasons": [] if m["set"] in keys else ["Insufficient signals"]}
            for m in [_model("common"), _model("additional"), _model("removed")]]})
    (root / "wf/results").mkdir(parents=True)
    pd.DataFrame([{"Set": m["set"], "ROCAUCMedian": 0.7, "AggregatePrecision": 0.8}
                  for m in [_model("common"), _model("additional"), _model("removed")]]).to_csv(root / "wf/results/qualification.csv", index=False)
    records = []
    for model in [_model("common"), _model("additional"), _model("removed")]:
        for i, (p, down, target, ret) in enumerate([(0.6, 0.2, 1, 0.02), (0.9, 0.4, 0, -0.01), (0.2, 0.1, 0, -0.02)]):
            for direction, probability in [("Up", p), ("Down", down)]:
                records.append({"Set": model["set"], "Direction": direction, "Date": f"2025-12-{i+1:02}", "Window": 1,
                                "Probability": probability, "Target": target if direction == "Up" else 1-target,
                                "IntradayReturn": ret, "MFE": 0.03, "MAE": -0.03})
    (root / "holdout/results").mkdir(parents=True)
    pd.DataFrame(records).to_csv(root / "holdout/results/holdout_predictions.csv", index=False)
    pd.DataFrame([{"Set": m["set"], "Direction": "Up", "ROCAUC": 0.7, "Precision": 0.5}
                  for m in [_model("common"), _model("additional"), _model("removed")]]).to_csv(root / "holdout/results/holdout_metrics.csv", index=False)
    output = root / "forward/results"
    output.mkdir(parents=True)
    sessions = forward_market_sessions("2026-01-02", "XNYS", horizon)
    observations = pd.DataFrame([{"source_model_id": m["source_model_id"], "session_date": session.date().isoformat(),
        "prediction_probability": 0.9 if index % 2 == 0 else 0.1, "outcome": int(index % 2 == 0),
        "signal": index % 3 == 0, "directional_return": 0.02 if index % 2 == 0 else -0.01,
        "correct_direction": index % 2 == 0} for m in models for index, session in enumerate(sessions)])
    exclusions = pd.DataFrame(columns=["source_model_id", "session_date"])
    period, population, daily = analyze_forward_periods(observations, exclusions, models, sessions)
    files = {"forward_observations.csv": observations, "forward_exclusions.csv": exclusions,
             "forward_period_metrics.csv": period, "forward_population_metrics.csv": population, "forward_daily_metrics.csv": daily}
    for name, frame in files.items():
        frame.to_csv(output / name, index=False)
    _json(output / diagnostic.ANALYSIS, {"forward_run_id": "forward", "source_e2e_run_id": source.name,
          "source_snapshot_sha256": diagnostic.digest(source / "results/forward_model_snapshot.json"),
          "artifact_digests": {name: diagnostic.digest(output / name) for name in files}})
    return source, output


def test_normal_reference_combines_up_down_without_changing_qualification(tmp_path):
    source, output = _fixture(tmp_path)
    originals = {p: diagnostic.digest(p) for p in source.parent.rglob("*") if p.is_file()}
    diagnostic.materialize_forward_diagnostic(output)
    ref, metrics, bins = diagnostic.load_forward_diagnostic(output)
    model = ref["models"][0]
    assert model["origin"] == "normal"
    assert model["holdout_qualification"]["Precision"] == 0.5
    assert model["holdout_comparable"]["precision"] == 1.0
    assert model["holdout_comparable"]["signals"] == 1  # equality on Up is accepted, Down veto applied
    assert model["holdout_comparable"]["brier"] == pytest.approx((0.16+0.81+0.04)/3)
    cumulative = metrics[(metrics.source_model_id == "common") & (metrics.period_kind == "cumulative")]
    assert cumulative.horizon.tolist() == [21, 42, 63]
    assert cumulative.auc.tolist() == [1.0, 1.0, 1.0]
    assert cumulative.delta_auc.iloc[0] == pytest.approx(0.5)
    assert bins.groupby(["source_model_id", "period_kind", "horizon"]).observations.sum().iloc[0] == 21
    assert "pnl" in metrics  # economics joined from canonical temporal artifact
    for path, before in originals.items():
        if path.name != diagnostic.ANALYSIS:
            assert diagnostic.digest(path) == before


def test_derived_origin_and_removed_candidates_have_no_invented_forward(tmp_path):
    source, output = _fixture(tmp_path, derived=True)
    diagnostic.materialize_forward_diagnostic(output)
    ref, metrics, _ = diagnostic.load_forward_diagnostic(output)
    origins = {m["origin"]: m for m in ref["models"]}
    assert set(origins) == {"common", "additional", "removed"}
    assert origins["removed"]["source_model_id"] is None
    assert not origins["removed"]["forward_available"]
    assert origins["removed"]["holdout_comparable"]["auc"] == 0.5
    assert set(metrics.origin) == {"common", "additional"}
    assert ref["stages"]["walk_forward"] == "wf"
    assert ref["qualification_overrides"][0]["new_value"] == 7


def test_missing_historical_predictions_keeps_forward_metrics_and_null_deltas(tmp_path):
    source, output = _fixture(tmp_path)
    (source.parent / "holdout/results/holdout_predictions.csv").unlink()
    diagnostic.materialize_forward_diagnostic(output)
    ref, metrics, _ = diagnostic.load_forward_diagnostic(output)
    assert ref["models"][0]["holdout_comparable"]["availability"] == "unavailable_missing_holdout_predictions"
    assert metrics.delta_auc.isna().all()
    assert metrics.auc.notna().all()


def test_unpaired_or_changed_holdout_is_unavailable(tmp_path):
    source, _ = _fixture(tmp_path)
    path = source.parent / "holdout/results/holdout_predictions.csv"
    qpath = source.parent / "q-parent/results/qualification.json"
    q = json.loads(qpath.read_text())
    q["source_artifact_digests"] = {"holdout_predictions.csv": "0" * 64}
    _json(qpath, q)
    ref = diagnostic.build_t0_reference(source)
    assert ref["models"][0]["holdout_comparable"]["availability"] == "unavailable_missing_holdout_predictions"
    q.pop("source_artifact_digests")
    _json(qpath, q)
    rows = pd.read_csv(path)
    rows.drop(index=1).to_csv(path, index=False)
    ref = diagnostic.build_t0_reference(source)
    assert ref["models"][0]["holdout_comparable"]["availability"] == "unavailable_unpaired_holdout"


def test_single_class_zero_signal_and_probability_edges():
    frame = pd.DataFrame({"prediction_probability": [0.0, 1.0], "outcome": [1, 1],
                          "signal": [False, False], "directional_return": [0, 0]})
    metrics = diagnostic.probability_metrics(frame)
    assert metrics["auc"] is None
    assert metrics["precision"] is None
    assert metrics["brier"] == 0.5
    assert sum(b["observations"] for b in diagnostic.probability_bins(frame)) == 2
    frame.loc[0, "prediction_probability"] = float("inf")
    assert diagnostic.probability_metrics(frame)["availability"] == "unavailable_invalid_probabilities"


def test_interruption_reconciles_manifest_and_reuses_shared_reference(tmp_path, monkeypatch):
    source, output = _fixture(tmp_path)
    publish = diagnostic._publish
    def interrupt(path, value, **kwargs):
        if path.name == diagnostic.BINS:
            # Another component updates the persistent manifest during our work.
            current = json.loads((output / diagnostic.ANALYSIS).read_text())
            current["external_update"] = "preserve"
            _json(output / diagnostic.ANALYSIS, current)
            raise RuntimeError("interrupted")
        publish(path, value, **kwargs)
    monkeypatch.setattr(diagnostic, "_publish", interrupt)
    with pytest.raises(RuntimeError, match="interrupted"):
        diagnostic.materialize_forward_diagnostic(output)
    assert not (output / diagnostic.MANIFEST).exists()
    before = diagnostic.digest(source / "results" / diagnostic.REFERENCE)
    monkeypatch.setattr(diagnostic, "_publish", publish)
    diagnostic.materialize_forward_diagnostic(output)
    assert json.loads((output / diagnostic.ANALYSIS).read_text())["external_update"] == "preserve"
    assert diagnostic.digest(source / "results" / diagnostic.REFERENCE) == before
    monkeypatch.setattr(diagnostic, "analyze_diagnostic", lambda *args: pytest.fail("idempotent rebuild"))
    diagnostic.materialize_forward_diagnostic(output)
    diagnostic.load_forward_diagnostic(output)


def test_tampered_inputs_or_reference_are_rejected(tmp_path):
    source, output = _fixture(tmp_path)
    diagnostic.materialize_forward_diagnostic(output)
    with (output / "forward_observations.csv").open("a") as stream:
        stream.write("tamper")
    with pytest.raises(ValueError, match="artifact_changed"):
        diagnostic.load_forward_diagnostic(output)
    with pytest.raises(ValueError, match="input_changed"):
        diagnostic.materialize_forward_diagnostic(output)


def test_incomplete_horizon_and_comparison_legacy(tmp_path):
    _, output = _fixture(tmp_path, horizon=20)
    diagnostic.materialize_forward_diagnostic(output)
    _, metrics, _ = diagnostic.load_forward_diagnostic(output)
    assert set(metrics.period_kind) == {"full_run"}
    assert comparison_types(["forward_simulation", "forward_simulation"]) == "forward_simulation"
    table = diagnostic.forward_comparison_table(output.parent.parent, ["forward", "legacy"])
    assert table.loc[table.run_id == "legacy", "availability"].iloc[0] == "unavailable_legacy"


def test_wide_legacy_holdout_adapter_does_not_invent_returns():
    frame = pd.DataFrame({"Set": [_model("x")["set"]]*2, "Date": ["2025-01-01", "2025-01-02"],
                          "UpProbability": [0.9, 0.1], "DownProbability": [0.1, 0.8], "UpTarget": [1, 0], "DownTarget": [0, 1]})
    ref = diagnostic._holdout_reference(frame, _model("x"))
    assert ref["auc"] == 1.0
    assert ref["mean_return"] is None
    assert ref["economic_availability"] == "unavailable_missing_returns"


def test_ui_views_read_committed_reference_and_metrics(tmp_path):
    from rstock.application import forward_diagnostic_ui as ui
    _, output = _fixture(tmp_path, derived=True)
    diagnostic.materialize_forward_diagnostic(output)
    ref, metrics, bins = diagnostic.load_forward_diagnostic(output)
    frames, charts = [], []
    st = SimpleNamespace(caption=lambda *a, **k: None, subheader=lambda *a: None, info=lambda *a: None,
        radio=lambda label, options, **kw: options[0], selectbox=lambda label, options, **kw: list(options)[0],
        column_config=SimpleNamespace(Column=lambda **kw: kw), altair_chart=lambda c, **kw: charts.append(c.to_dict()))
    render = lambda frame, **kw: frames.append(frame)
    ui.render_summary(st, render, ref, metrics)
    ui.render_model(st, render, ref, metrics, bins, "common", "forward")
    ui.render_population(st, render, ref, metrics, "forward")
    assert len(charts) == 3
    assert any("Holdout T0 comparable" in f for f in frames)
    assert any("Forward" in f and f.Forward.eq("Indisponible dans ce dérivé").any() for f in frames)


def test_concurrent_reference_creation_keeps_one_persisted_winner(tmp_path):
    from concurrent.futures import ThreadPoolExecutor
    source, _ = _fixture(tmp_path)
    with ThreadPoolExecutor(max_workers=2) as executor:
        results = list(executor.map(diagnostic.ensure_t0_reference, [source, source]))
    assert results[0][0] == results[1][0]
    assert results[0][1] == results[1][1]
    assert not list((source / "results").glob("*.tmp"))


def test_final_analysis_publication_reconciles_concurrent_metadata(tmp_path, monkeypatch):
    _, output = _fixture(tmp_path)
    publish = diagnostic._publish
    def concurrent_update(path, value, **kwargs):
        publish(path, value, **kwargs)
        if path.name == diagnostic.BINS:
            current = json.loads((output / diagnostic.ANALYSIS).read_text())
            current["other_component"] = {"completed": True}
            _json(output / diagnostic.ANALYSIS, current)
    monkeypatch.setattr(diagnostic, "_publish", concurrent_update)
    diagnostic.materialize_forward_diagnostic(output)
    current = json.loads((output / diagnostic.ANALYSIS).read_text())
    assert current["other_component"] == {"completed": True}
    assert current["t0_reference"]["sha256"]
    assert current["diagnostic"]["sha256"]


def test_shared_reference_survives_missing_sources_and_detects_snapshot_change(tmp_path):
    source, output = _fixture(tmp_path)
    diagnostic.materialize_forward_diagnostic(output)
    (source.parent / "holdout/results/holdout_predictions.csv").unlink()
    reference, _ = diagnostic.ensure_t0_reference(source)
    assert reference["models"][0]["holdout_comparable"]["availability"] == "available"
    path = source / "results/forward_model_snapshot.json"
    value = json.loads(path.read_text())
    value["models"][0]["up_threshold"] = 0.95
    _json(path, value)
    with pytest.raises(ValueError, match="reference_mismatch"):
        diagnostic.ensure_t0_reference(source)


def test_all_excluded_rows_keep_economics_without_probabilistic_metrics(tmp_path):
    source, output = _fixture(tmp_path, horizon=21)
    sessions = forward_market_sessions("2026-01-02", "XNYS", 21)
    models = json.loads((source / "results/forward_model_snapshot.json").read_text())["models"]
    observations = pd.read_csv(output / "forward_observations.csv").iloc[:0]
    exclusions = pd.DataFrame([{"source_model_id": m["source_model_id"], "session_date": s.date().isoformat()}
                               for m in models for s in sessions])
    periods, _, _ = analyze_forward_periods(observations, exclusions, models, sessions)
    reference = diagnostic.build_t0_reference(source)
    metrics, bins = diagnostic.analyze_diagnostic(observations, periods, reference)
    assert metrics.auc.isna().all()
    assert metrics.delta_auc.isna().all()
    assert bins.empty
    assert "precision" not in metrics and "signals" not in metrics  # economics have one owner


def test_comparison_matches_scientific_identity_across_forward_runs(tmp_path):
    _, output = _fixture(tmp_path, derived=True)
    diagnostic.materialize_forward_diagnostic(output)
    other = output.parent.parent / "forward-two/results"
    other.mkdir(parents=True)
    for name in ["forward_observations.csv", "forward_exclusions.csv", "forward_period_metrics.csv"]:
        frame = pd.read_csv(output / name)
        frame.to_csv(other / name, index=False)
    analysis = json.loads((output / diagnostic.ANALYSIS).read_text())
    analysis["forward_run_id"] = "forward-two"
    analysis["artifact_digests"] = {name: diagnostic.digest(other / name) for name in ["forward_observations.csv", "forward_exclusions.csv", "forward_period_metrics.csv"]}
    _json(other / diagnostic.ANALYSIS, analysis)
    diagnostic.materialize_forward_diagnostic(other)
    table = diagnostic.forward_comparison_table(output.parent.parent, ["forward", "forward-two"])
    assert len(table) == 12  # 2 models * 3 checkpoints * 2 runs
    assert table.groupby("canonical_combination_id").run_id.nunique().eq(2).all()
    assert json.loads((other / diagnostic.MANIFEST).read_text())["t0_reference"] == json.loads((output / diagnostic.MANIFEST).read_text())["t0_reference"]


def test_forward_models_view_shows_cohort_and_t0_grid(tmp_path, monkeypatch):
    from rstock.application import streamlit_app as app
    source, output = _fixture(tmp_path, derived=True)
    analysis = json.loads((output / diagnostic.ANALYSIS).read_text())
    analysis.update(horizon_max=63, model_count_t0=2, checkpoints=[21, 42, 63], forward_policy="FROZEN")
    _json(output / diagnostic.ANALYSIS, analysis)
    diagnostic.materialize_forward_diagnostic(output)
    frames = []
    def render(frame, **kwargs):
        frames.append(frame)
        return SimpleNamespace(selection=SimpleNamespace(rows=[0]))
    st = SimpleNamespace(session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        caption=lambda *a, **k: None, subheader=lambda *a: None, info=lambda *a: None,
        error=lambda text: pytest.fail(text),
        radio=lambda label, options, **kw: "Modèles" if label == "Analyse Forward" else options[0],
        selectbox=lambda label, options, **kw: list(options)[0],
        column_config=SimpleNamespace(Column=lambda **kw: kw, NumberColumn=lambda **kw: kw),
        altair_chart=lambda c, **kw: c.to_dict())
    monkeypatch.setattr(app, "st", st)
    monkeypatch.setattr(app, "render_dataframe", render)
    app._render_forward_temporal_results("forward", {})
    grid = next(f for f in frames if "Origine qualification" in f)
    assert set(grid["Origine qualification"]) == {"Commun au parent", "Supplémentaire admis"}
    assert grid["Δ Brier +21"].notna().all()
