from dataclasses import replace
from types import SimpleNamespace
import json

import exchange_calendars as xcals
import numpy as np
import pandas as pd
import pytest

from rstock.config import RStockConfig
from rstock.application.domain import _config_from_dict, _config_to_dict, JobType
from rstock.application import market_context as mc
from rstock.application import market_context_runtime as runtime
from rstock.application.workflows import WorkflowRegistry


def prices():
    sessions = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2022-01-03", "2025-12-31")).tz_localize(None)
    values = 100 * np.exp(np.cumsum(.0002 + .009 * np.sin(np.arange(len(sessions))/11)))
    return pd.DataFrame({"Adjusted": values}, index=sessions)


def test_historical_config_disabled_and_explicit_values_prioritized(tmp_path):
    config = RStockConfig(project_root=tmp_path)
    historical = _config_to_dict(config)
    for key in list(historical):
        if key.startswith("market_context_"):
            del historical[key]
    restored = _config_from_dict(historical)
    assert restored.market_context_enabled is False
    assert restored.market_context_protocol_version == "spy_adjusted_context_v1"
    assert _config_from_dict(_config_to_dict(restored)) == restored
    custom = replace(config, market_context_enabled=True, market_context_trend_sessions=84)
    assert _config_from_dict(_config_to_dict(custom)) == custom
    assert mc.ContextProtocol.from_config(custom).identifier != mc.ContextProtocol().identifier
    with pytest.raises(ValueError, match="protocol version"):
        replace(config, market_context_protocol_version="unrecognized")


@pytest.mark.parametrize("value", [1, 2.5, True])
def test_invalid_window(value, tmp_path):
    with pytest.raises(ValueError):
        RStockConfig(project_root=tmp_path, market_context_trend_sessions=value)


def test_adjusted_not_raw_and_strict_past_information():
    raw = prices()
    raw["Close"] = raw.Adjusted
    raw.loc[raw.index[400]:, "Close"] *= .98  # Raw dividend discontinuity.
    snap = mc.adjusted_snapshot(raw)
    protocol = mc.ContextProtocol()
    sessions = raw.index[300:600]
    baseline, cuts = mc.build_context(snap, sessions, protocol, raw.index[260], raw.index[390])
    altered = snap.copy()
    altered.loc[raw.index[500]:] *= 3
    changed, _ = mc.build_context(altered, sessions, protocol, raw.index[260], raw.index[390], cuts)
    pd.testing.assert_frame_equal(baseline.iloc[:201], changed.iloc[:201])
    assert (pd.to_datetime(baseline.context_as_of_date) < pd.to_datetime(baseline.session_date)).all()
    with pytest.raises(ValueError, match="adjusted_close_unavailable"):
        mc.adjusted_snapshot(raw.drop(columns="Adjusted"))


def test_missing_session_not_bridged_and_boundaries_not_future_fitted():
    snap = mc.adjusted_snapshot(prices())
    dates = snap.index
    complete, cuts = mc.build_context(snap, dates[300:650], mc.ContextProtocol(), dates[260], dates[390])
    missing = snap.drop(index=dates[450])
    context, same = mc.build_context(missing, dates[300:650], mc.ContextProtocol(), dates[260], dates[390])
    assert cuts == same
    assert pd.isna(context.loc[context.session_date.eq(dates[451].strftime("%Y-%m-%d")), "trend"]).all()
    assert pd.isna(context.loc[context.session_date.eq(dates[451].strftime("%Y-%m-%d")), "volatility"]).all()


def test_adjusted_extension_accepts_uniform_revision_rejects_return_revision():
    snap = mc.adjusted_snapshot(prices())
    frozen = snap.iloc[:600]
    incoming = snap.iloc[570:700] * .97
    extended = mc.extend_adjusted_snapshot(frozen, incoming)
    pd.testing.assert_frame_equal(extended.iloc[:600], frozen)
    np.testing.assert_allclose(extended.adjusted_close, snap.iloc[:700].adjusted_close)
    incoming.iloc[10, 0] *= 1.1
    with pytest.raises(ValueError, match="historical_returns_changed"):
        mc.extend_adjusted_snapshot(frozen, incoming)


def test_publication_interrupt_retry_and_manifest_integrity(tmp_path, monkeypatch):
    snap = mc.adjusted_snapshot(prices())
    sessions = snap.index[300:600]
    original = mc._publish
    def interrupt(path, payload, **kwargs):
        if path.name == mc.CONTEXT_MANIFEST:
            raise RuntimeError("interrupted")
        return original(path, payload, **kwargs)
    monkeypatch.setattr(mc, "_publish", interrupt)
    with pytest.raises(RuntimeError):
        mc.publish_context(tmp_path, snap.iloc[:600], sessions, mc.ContextProtocol(), "2023-01-03", "2023-07-03")
    assert not list(tmp_path.rglob(mc.CONTEXT_MANIFEST))
    monkeypatch.setattr(mc, "_publish", original)
    path = mc.publish_context(tmp_path, snap.iloc[:600], sessions, mc.ContextProtocol(), "2023-01-03", "2023-07-03")
    assert mc.validate_context(path)["protocol_id"] == mc.ContextProtocol().identifier
    old = pd.read_csv(path.parent / "market_context.csv")
    extension = mc.publish_context(tmp_path, snap.iloc[570:700] * .97, snap.index[300:700], mc.ContextProtocol(), "ignored", "ignored", parent_manifest=path)
    new = pd.read_csv(extension.parent / "market_context.csv")
    pd.testing.assert_frame_equal(old, new.iloc[:len(old)])
    (extension.parent / "market_context.csv").write_text("corrupted")
    with pytest.raises(ValueError):
        mc.validate_context(extension)


def test_compact_metrics_use_all_probabilities_and_frozen_combined_rule():
    raw = prices()
    dates = raw.index[300:400]
    context, _ = mc.build_context(mc.adjusted_snapshot(raw), dates, mc.ContextProtocol(), raw.index[260], raw.index[390])
    records = []
    for i, day in enumerate(dates):
        for direction in ("Up", "Down"):
            records.append(dict(Set='["AAA","SPY"]', Date=str(day.date()), Window=1, Direction=direction,
                Probability=(.8 if i%2 else .2) if direction == "Up" else .4,
                Target=i%2, IntradayReturn=.01))
    predictions = pd.DataFrame(records)
    rows = mc.normalize_predictions(predictions, signal_rule="frozen_combined_up_down",
        thresholds={'["AAA","SPY"]': {"Up": {"threshold": .6}, "Down": {"threshold": .3}}})
    metrics, robustness = mc.aggregate_context(rows, context, stage="holdout", signal_rule="frozen_combined_up_down")
    assert metrics.signal_count.sum() == 0
    assert metrics.auc.dropna().eq(1).all()
    assert metrics.brier.dropna().round(8).eq(.04).all()
    assert metrics.loc[metrics.axis.eq("trend"), "observations"].sum() == len(dates)
    assert robustness.robustness_status.eq("unavailable_insufficient_context_support").all()
    unknown = mc.normalize_predictions(predictions, signal_rule="frozen_combined_up_down")
    unavailable, _ = mc.aggregate_context(unknown, context, stage="holdout", signal_rule="frozen_combined_up_down")
    assert unavailable.precision.isna().all()
    assert unavailable.auc.dropna().eq(1).all()


def setup_run(tmp_path):
    raw = prices()
    output = tmp_path / "runs/wf/results"
    output.mkdir(parents=True)
    pd.DataFrame([dict(TrainStart="2023-01-03", TestStart="2023-07-03")]).to_csv(output / "windows.csv", index=False)
    dates = raw.index[(raw.index >= "2023-07-03") & (raw.index <= "2023-10-02")]
    pd.DataFrame([dict(Set='["AAA","SPY"]', Date=str(d.date()), Window=1, UpProbability=.8,
        UpTarget=i%2, UpPrediction=True, DownProbability=.2, IntradayReturn=.01)
        for i,d in enumerate(dates)]).to_csv(output / "predictions.csv", index=False)
    class Provider:
        source_name = "test adjusted provider"
        calls = 0
        def fetch(self, symbol, start, end):
            assert symbol == "SPY"
            self.calls += 1
            return raw.loc[(raw.index >= pd.Timestamp(start)) & (raw.index < pd.Timestamp(end))]
    config = RStockConfig(project_root=tmp_path, market_context_enabled=True)
    spec = SimpleNamespace(config=config, job_type=JobType.WALK_FORWARD, source_experiment_run=None,
        source_end_to_end_run=None, source_walk_forward_run=None, source_threshold_calibration_run=None)
    return spec, output, Provider()


def test_runtime_shared_inherited_context_and_idempotent_read(tmp_path):
    spec, output, provider = setup_run(tmp_path)
    first = runtime.materialize_context_diagnostic(spec, output, provider=provider)
    runtime.materialize_context_diagnostic(spec, output, provider=provider)
    assert provider.calls == 1
    for owner in ("parent", "derived"):
        orchestration = tmp_path / "runs" / owner / "orchestration"
        orchestration.mkdir(parents=True)
        (orchestration / "pipeline.json").write_text(json.dumps({"schema_version": 4, "stages": [
            {"stage_key": "walk_forward", "mode": "inherited", "source_run_id": "wf"}]}))
        holdout = tmp_path / "runs" / (owner + "-holdout") / "results"
        holdout.mkdir(parents=True)
        records = []
        for day in ("2023-09-28", "2023-09-29"):
            for direction in ("Up", "Down"):
                records.append(dict(Set='["AAA","SPY"]', Date=day, Window=1, Direction=direction, Probability=.7, Target=1, IntradayReturn=.01))
        pd.DataFrame(records).to_csv(holdout / "holdout_predictions.csv", index=False)
        spec.job_type = JobType.HOLDOUT_EVALUATION
        spec.source_end_to_end_run = owner
        next_manifest = runtime.materialize_context_diagnostic(spec, holdout, provider=provider)
        assert next_manifest["context_manifest"] == first["context_manifest"]
        assert next_manifest["stage_references"]["walk_forward"]["path"].replace("\\", "/").startswith("wf/")
    assert provider.calls == 1


def test_disabled_diagnostic_does_not_acquire_or_change_result(tmp_path):
    spec = SimpleNamespace(config=RStockConfig(project_root=tmp_path), job_type=JobType.WALK_FORWARD)
    registry = WorkflowRegistry({JobType.WALK_FORWARD: lambda *_: {"scientific": "unchanged"}})
    assert registry.execute(spec, tmp_path, progress_callback=None, cancellation_check=None) == {"scientific": "unchanged"}


def test_failed_optional_diagnostic_preserves_available_manifest(tmp_path, monkeypatch):
    spec, output, provider = setup_run(tmp_path)
    runtime.materialize_context_diagnostic(spec, output, provider=provider)
    path = output / mc.DIAGNOSTIC_MANIFEST
    before = path.read_bytes()
    monkeypatch.setattr(runtime, "materialize_context_diagnostic", lambda *_: (_ for _ in ()).throw(ValueError("interruption")))
    assert runtime.optional_context_diagnostic(spec, output)["status"] == "unavailable"
    assert path.read_bytes() == before


def test_post_science_hook_and_diagnostic_interruption_recovery(tmp_path, monkeypatch):
    spec, output, provider = setup_run(tmp_path)
    original_publish = runtime._publish
    def interrupt(path, payload, **kwargs):
        if path.name == mc.DIAGNOSTIC_MANIFEST:
            raise RuntimeError("interrupted at commit")
        return original_publish(path, payload, **kwargs)
    monkeypatch.setattr(runtime, "_publish", interrupt)
    with pytest.raises(RuntimeError):
        runtime.materialize_context_diagnostic(spec, output, provider=provider)
    assert not (output / mc.DIAGNOSTIC_MANIFEST).exists()
    monkeypatch.setattr(runtime, "_publish", original_publish)
    materialize = runtime.materialize_context_diagnostic
    monkeypatch.setattr(runtime, "materialize_context_diagnostic", lambda spec, path: materialize(spec, path, provider=provider))
    before = (output / "predictions.csv").read_bytes()
    registry = WorkflowRegistry({JobType.WALK_FORWARD: lambda *_: {"science": "completed"}})
    result = registry.execute(spec, output, progress_callback=None, cancellation_check=None)
    assert result["science"] == "completed"
    assert result["market_context_diagnostic"]["status"] == "available"
    assert (output / "predictions.csv").read_bytes() == before
    assert provider.calls == 1  # Retry reused immutable SPY evidence.
    (output / mc.METRICS_FILE).write_text("partial interrupted publication")
    # A stale available commit with a mismatched aggregate is repaired on retry.
    assert registry.execute(spec, output, progress_callback=None, cancellation_check=None)["market_context_diagnostic"]["status"] == "available"
    runtime.load_context_diagnostic(output, tmp_path / "runs")


@pytest.mark.parametrize("derived", [False, True])
def test_forward_horizons_and_qualification_origin(tmp_path, derived):
    from test_forward_diagnostic import _fixture
    from rstock.application.forward_diagnostic import materialize_forward_diagnostic
    source, output = _fixture(tmp_path, derived=derived)
    materialize_forward_diagnostic(output)
    wf = tmp_path / "runs/wf/results"
    pd.DataFrame([dict(TrainStart="2023-01-03", TestStart="2023-07-03")]).to_csv(wf / "windows.csv", index=False)
    dates = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2022-01-03", "2026-12-31")).tz_localize(None)
    raw = pd.DataFrame({"Adjusted": 100 * np.exp(np.arange(len(dates)) * .0005)}, index=dates)
    class Provider:
        source_name = "test"
        def fetch(self, symbol, start, end):
            return raw.loc[(raw.index >= pd.Timestamp(start)) & (raw.index < pd.Timestamp(end))]
    spec = SimpleNamespace(config=RStockConfig(project_root=tmp_path, market_context_enabled=True),
        job_type=JobType.FORWARD_SIMULATION, source_end_to_end_run=source.name,
        source_experiment_run=None, source_walk_forward_run=None, source_threshold_calibration_run=None)
    manifest = runtime.materialize_context_diagnostic(spec, output, provider=Provider())
    _, context, metrics, summary = runtime.load_context_diagnostic(output, tmp_path / "runs")
    assert set(metrics.loc[metrics.period_kind.eq("cumulative"), "horizon"]) == {21, 42, 63}
    assert set(metrics.qualification_origin) == ({"common", "additional"} if derived else {"normal"})
    if derived:
        assert not metrics.model_key.str.contains("removed").any()
    assert metrics.groupby(["model_key", "period_kind", "horizon", "axis"]).observations.sum().gt(0).all()
    assert context.context_as_of_date.notna().all()


def test_ui_only_loads_compact_artifacts(tmp_path):
    from rstock.application.market_context_ui import render_context_diagnostic
    spec, output, provider = setup_run(tmp_path)
    runtime.materialize_context_diagnostic(spec, output, provider=provider)
    class UI:
        tables = []
        def expander(self, *_args, **_kwargs):
            from contextlib import nullcontext
            return nullcontext()
        def caption(self, *_): pass
        def selectbox(self, _label, values, **_): return values[0]
        def line_chart(self, *_): pass
        def write(self, *_): pass
        def dataframe(self, table, **_): self.tables.append(table)
    # Display deliberately works even if scientific predictions are purged.
    (output / "predictions.csv").unlink()
    ui = UI()
    render_context_diagnostic(ui, output, tmp_path / "runs")
    assert len(ui.tables) == 2


def test_descriptor_never_accepts_model_scores():
    snap = mc.adjusted_snapshot(prices())
    variants = mc.descriptive_variants(snap, snap.index, {"test": ("2024-01-02", "2024-03-28")})
    assert len(variants) == 12
    assert set(variants.columns).isdisjoint({"auc", "precision", "pnl", "model_key"})


def test_ambiguous_persisted_predictions_rejected():
    wide = pd.DataFrame([dict(Set='["AAA","SPY"]', Date="2024-01-02", Window=1,
        UpProbability=.7, UpTarget=1, UpPrediction=True)] * 2)
    with pytest.raises(ValueError, match="ambiguous_predictions"):
        mc.normalize_predictions(wide, signal_rule="persisted_wf_up")
    forward = pd.DataFrame([dict(source_model_id="model", session_date="2024-01-02")] * 2)
    with pytest.raises(ValueError, match="ambiguous_forward"):
        mc.normalize_predictions(forward, signal_rule="persisted_forward_combined")
