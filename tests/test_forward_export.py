import ast
import hashlib
import io
import json
from pathlib import Path
from types import SimpleNamespace
from zipfile import ZipFile

import pandas as pd
import pytest

from rstock.application import forward_diagnostic as diagnostic
from rstock.application import forward_export as export
from test_forward_diagnostic import _fixture


def bundle(raw):
    with ZipFile(io.BytesIO(raw)) as archive:
        files = {name: archive.read(name) for name in archive.namelist()}
    manifest = json.loads(files["manifest.json"])
    tables = {name: pd.read_csv(io.BytesIO(raw), dtype=export.STRING_COLUMNS) for name, raw in files.items() if name.endswith(".csv")}
    for name, entry in manifest["files"].items():
        assert hashlib.sha256(files[name]).hexdigest() == entry["sha256"]
    return files, manifest, tables


def file_hashes(directory):
    return {p.relative_to(directory).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest() for p in directory.rglob("*") if p.is_file()}


@pytest.mark.parametrize("horizon", [12, 63, 84, 126, 137])
def test_all_persisted_horizons_intervals_and_custom_end_without_recalculation(tmp_path, monkeypatch, horizon):
    source, output = _fixture(tmp_path, horizon=horizon)
    diagnostic.materialize_forward_diagnostic(output)
    original = file_hashes(tmp_path)
    monkeypatch.setattr(diagnostic, "materialize_forward_diagnostic", lambda *_: pytest.fail("Scientific computation in export"))
    monkeypatch.setattr(diagnostic, "probability_metrics", lambda *_: pytest.fail("Probability recalculation"))
    files, manifest, tables = bundle(export.build_forward_export(output))
    expected = pd.read_csv(output / "forward_period_metrics.csv", dtype=export.STRING_COLUMNS)
    actual = tables["performances_forward.csv"]
    assert len(actual) == len(expected)
    for column in expected:
        pd.testing.assert_series_equal(actual[column], expected[column], check_dtype=False)
    assert set(actual.horizon) == set(expected.horizon)
    assert actual.loc[actual.period_kind.eq("full_run"), "horizon"].eq(horizon).all()
    assert set(tables["observations_forward.csv"].source_model_id) == {"common", "removed"}
    assert len(tables["observations_forward.csv"]) == horizon * 2
    assert file_hashes(tmp_path) == original
    assert manifest["scope"] == "all_candidates_all_persisted_periods_independent_of_ui_filters"
    assert manifest["feature_drift"] == "not_evaluated"


def test_t0_populations_and_metric_semantics_are_separate_for_derivative(tmp_path):
    source, output = _fixture(tmp_path, derived=True, horizon=126)
    diagnostic.materialize_forward_diagnostic(output)
    files, manifest, tables = bundle(export.build_forward_export(output))
    t0 = tables["modeles_t0.csv"].set_index("origin")
    assert set(t0.index) == {"common", "additional", "removed"}
    assert not t0.loc["removed", "forward_available"]
    assert t0["wf__ROCAUCMedian"].eq(.7).all()
    assert t0["holdout_qualification__Precision"].eq(.5).all()
    assert t0["holdout_comparable__precision"].eq(1.0).all()
    assert "holdout_comparable__probability_bins" not in t0
    assert set(tables["performances_forward.csv"].source_model_id.dropna()) == {"common", "additional"}
    models = tables["performances_forward.csv"].query("scope == 'model'")
    assert models.canonical_combination_id.notna().all()
    assert models.delta_brier.notna().all()
    calibration = tables["calibration.csv"]
    assert set(calibration.stage) == {"forward", "holdout_comparable"}
    assert 126 in set(calibration.loc[calibration.stage.eq("forward"), "horizon"])
    assert calibration.canonical_combination_id.notna().all()
    assert manifest["parent_e2e_run_id"] == "parent"
    assert "qualification_overrides" in manifest["t0_provenance"]


def test_legacy_export_does_not_construct_missing_diagnostic(tmp_path, monkeypatch):
    source, output = _fixture(tmp_path)
    before = file_hashes(tmp_path)
    monkeypatch.setattr(diagnostic, "build_t0_reference", lambda *_: pytest.fail("T0 rebuild"))
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert manifest["unavailable"]["t0_diagnostic"] == "diagnostic_not_persisted"
    t0 = tables["modeles_t0.csv"]
    assert t0.t0_reference_status.eq("unavailable").all()
    assert t0.wf__status.eq("persisted").all()
    assert t0.wf__ROCAUCMedian.eq(.7).all()
    assert t0.holdout_qualification__Precision.eq(.5).all()
    assert t0.holdout_comparable__status.eq("unavailable").all()
    assert "auc" not in tables["performances_forward.csv"]
    assert file_hashes(tmp_path) == before


def test_purged_observations_keep_valid_t0_and_available_economics(tmp_path):
    source, output = _fixture(tmp_path)
    diagnostic.materialize_forward_diagnostic(output)
    (output / "forward_observations.csv").unlink()
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert "observations_forward.csv" in manifest["unavailable"]
    assert "forward_diagnostic_metrics" in manifest["unavailable"]
    assert tables["modeles_t0.csv"].holdout_comparable__precision.eq(1.0).all()
    assert "performances_forward.csv" in tables
    assert set(tables["calibration.csv"].stage) == {"holdout_comparable"}


def test_corrupt_scientific_periods_are_rejected(tmp_path):
    _, output = _fixture(tmp_path)
    with (output / "forward_period_metrics.csv").open("ab") as stream:
        stream.write(b"\n")
    with pytest.raises(ValueError, match="integrity_error"):
        export.build_forward_export(output)


def test_corrupt_optional_reference_is_unavailable_not_invented(tmp_path):
    source, output = _fixture(tmp_path)
    diagnostic.materialize_forward_diagnostic(output)
    (source / "results" / diagnostic.REFERENCE).write_text("{}")
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert "t0_diagnostic" in manifest["unavailable"]
    assert tables["modeles_t0.csv"].holdout_comparable__status.eq("unavailable").all()


def test_empty_signal_models_and_exclusion_reasons_are_kept(tmp_path):
    source, output = _fixture(tmp_path)
    observations = pd.read_csv(output / "forward_observations.csv")
    observations.loc[observations.source_model_id.eq("common"), "signal"] = False
    excluded_row = observations.loc[observations.source_model_id.eq("removed")].iloc[0]
    observations = observations.drop(excluded_row.name)
    exclusions = pd.DataFrame([dict(source_model_id="removed", session_date=excluded_row.session_date,
        exclusion_reason="missing_predictor_features", invalid_fields="SPY.Lag1")])
    from rstock.application.forward_temporal_analysis import analyze_forward_periods
    snapshot = json.loads((source / "results/forward_model_snapshot.json").read_text())
    dates = pd.DatetimeIndex(sorted(pd.to_datetime(pd.concat([observations.session_date, exclusions.session_date]).unique())))
    periods, population, daily = analyze_forward_periods(observations, exclusions, snapshot["models"], dates)
    updates = {"forward_observations.csv": observations, "forward_exclusions.csv": exclusions,
        "forward_period_metrics.csv": periods, "forward_population_metrics.csv": population, "forward_daily_metrics.csv": daily}
    analysis = json.loads((output / diagnostic.ANALYSIS).read_text())
    for name, frame in updates.items():
        frame.to_csv(output / name, index=False)
        analysis["artifact_digests"][name] = diagnostic.digest(output / name)
    (output / diagnostic.ANALYSIS).write_text(json.dumps(analysis))
    diagnostic.materialize_forward_diagnostic(output)
    _, _, tables = bundle(export.build_forward_export(output))
    quiet = tables["performances_forward.csv"].query("source_model_id == 'common'")
    assert quiet.signals.eq(0).all()
    assert quiet.precision.isna().all()
    assert quiet.mean_return.isna().all()
    assert tables["exclusions_forward.csv"].iloc[0].exclusion_reason == "missing_predictor_features"


def test_export_detects_source_change_before_return(tmp_path, monkeypatch):
    _, output = _fixture(tmp_path)
    original = export._readme
    def change_source(*args):
        (output / "forward_daily_metrics.csv").write_text("changed concurrently")
        return original(*args)
    monkeypatch.setattr(export, "_readme", change_source)
    with pytest.raises(ValueError, match="changed_during_generation"):
        export.build_forward_export(output)


def test_outside_runs_reference_not_read(tmp_path):
    _, output = _fixture(tmp_path)
    analysis = json.loads((output / diagnostic.ANALYSIS).read_text())
    analysis["source_e2e_run_id"] = "../../outside"
    (output / diagnostic.ANALYSIS).write_text(json.dumps(analysis))
    _, manifest, _ = bundle(export.build_forward_export(output))
    assert "outside_runs" in manifest["unavailable"]["model_snapshot"]


def test_market_context_and_compact_aggregates_are_exported(tmp_path):
    import exchange_calendars as xcals
    import numpy as np
    from rstock.application.domain import JobType
    from rstock.application.market_context_runtime import materialize_context_diagnostic
    from rstock.config import RStockConfig
    source, output = _fixture(tmp_path, derived=True, horizon=126)
    diagnostic.materialize_forward_diagnostic(output)
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
    materialize_context_diagnostic(spec, output, provider=Provider())
    before = file_hashes(tmp_path)
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert {"market_context.csv", "context_metrics.csv", "context_robustness.csv"}.issubset(tables)
    assert set(tables["context_metrics.csv"].canonical_combination_id) <= set(tables["modeles_t0.csv"].canonical_combination_id)
    assert tables["context_metrics.csv"].context_protocol_id.notna().all()
    assert tables["context_metrics.csv"].context_revision.notna().all()
    assert manifest["context_provenance"][0]["context"]["protocol"]["benchmark"] == "SPY"
    assert file_hashes(tmp_path) == before


def test_single_download_button_is_lazy_and_has_no_selection_parameters(tmp_path, monkeypatch):
    source, output = _fixture(tmp_path)
    calls = []
    monkeypatch.setattr(export, "build_forward_export", lambda path: calls.append(path) or b"zip")
    captured = {}
    fake = SimpleNamespace(download_button=lambda label, **kwargs: captured.update(label=label, **kwargs))
    export.render_forward_export(fake, output)
    assert calls == []
    assert callable(captured["data"])
    assert captured["data"]() == b"zip"
    assert calls == [output]
    assert captured["mime"] == "application/zip"
    assert captured["on_click"] == "ignore"
    tree = ast.parse(Path("rstock/application/streamlit_app.py").read_text(encoding="utf-8"))
    renderer = next(node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == "_render_forward_temporal_results")
    button = next(node for node in ast.walk(renderer) if isinstance(node, ast.Call) and getattr(node.func, "id", None) == "render_forward_export")
    view = next(node for node in ast.walk(renderer) if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
        and node.func.attr == "radio")
    assert button.lineno < view.lineno


def test_numeric_model_identifiers_keep_leading_zeroes(tmp_path):
    source, output = _fixture(tmp_path)
    diagnostic.materialize_forward_diagnostic(output)
    replacements = {"common": "00001234", "removed": "00005678"}
    snapshot_path = source / "results/forward_model_snapshot.json"
    snapshot = json.loads(snapshot_path.read_text())
    for model in snapshot["models"]:
        model["source_model_id"] = replacements[model["source_model_id"]]
    snapshot_path.write_text(json.dumps(snapshot))
    analysis = json.loads((output / diagnostic.ANALYSIS).read_text())
    analysis["source_snapshot_sha256"] = diagnostic.digest(snapshot_path)
    for name in (*export.CORE_FILES, diagnostic.METRICS, diagnostic.BINS):
        frame = pd.read_csv(output / name, dtype=export.STRING_COLUMNS)
        if "source_model_id" in frame:
            frame["source_model_id"] = frame.source_model_id.replace(replacements)
        frame.to_csv(output / name, index=False)
        if name in export.CORE_FILES:
            analysis["artifact_digests"][name] = diagnostic.digest(output / name)
    reference_path = source / "results" / diagnostic.REFERENCE
    reference = json.loads(reference_path.read_text())
    reference["source_snapshot_sha256"] = analysis["source_snapshot_sha256"]
    for model in reference["models"]:
        model["source_model_id"] = replacements[model["source_model_id"]]
    reference_path.write_text(json.dumps(reference))
    manifest_path = output / diagnostic.MANIFEST
    manifest = json.loads(manifest_path.read_text())
    manifest["t0_reference"]["sha256"] = diagnostic.digest(reference_path)
    for group in ("input_digests", "artifact_digests"):
        manifest[group] = {name: diagnostic.digest(output / name) for name in manifest[group]}
    manifest_path.write_text(json.dumps(manifest))
    analysis["diagnostic"]["sha256"] = diagnostic.digest(manifest_path)
    analysis["t0_reference"] = manifest["t0_reference"]
    (output / diagnostic.ANALYSIS).write_text(json.dumps(analysis))
    _, _, tables = bundle(export.build_forward_export(output))
    for name in ("performances_forward.csv", "observations_forward.csv", "evolution_quotidienne.csv", "modeles_t0.csv", "calibration.csv"):
        assert set(tables[name].source_model_id.dropna()) == set(replacements.values())
    assert tables["performances_forward.csv"].query("scope == 'model'").delta_auc.notna().all()


def test_missing_metrics_do_not_gain_modern_defaults_in_manifest(tmp_path):
    _, output = _fixture(tmp_path)
    _, manifest, _ = bundle(export.build_forward_export(output))
    assert manifest["summary"] == {}
    assert "notional_per_signal" not in manifest["configuration"].get("config", {})


def test_readonly_legacy_csv_without_manifest_is_explicitly_unverified(tmp_path):
    _, output = _fixture(tmp_path)
    (output / diagnostic.ANALYSIS).unlink()
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert "source_e2e_run_id" in manifest["unavailable"]
    assert tables["observations_forward.csv"].source_model_id.notna().all()
    assert all(source["verification"] == "persisted_without_expected_digest" for source in manifest["sources"].values())


def test_empty_forward_button_is_disabled_without_creating_artifacts(tmp_path):
    captured = {}
    export.render_forward_export(SimpleNamespace(download_button=lambda *args, **kwargs: captured.update(kwargs)), tmp_path / "run/results")
    assert captured["disabled"]
    assert not (tmp_path / "run").exists()


@pytest.mark.parametrize("enabled,reason", [
    (False, "disabled_in_run_configuration"),
    (True, "diagnostic_not_persisted"),
    (None, "historical_context_not_enabled"),
])
def test_context_absence_and_actual_serialized_configuration(tmp_path, monkeypatch, enabled, reason):
    _, output = _fixture(tmp_path)
    settings = {"market_context_trend_sessions": 84, "project_root": "private-path"}
    if enabled is not None:
        settings["market_context_enabled"] = enabled
    (output.parent / "config.json").write_text(json.dumps({"rstock_config": settings,
        "config": {"market_context_enabled": True, "market_context_trend_sessions": 42}}))
    from rstock.application import market_context_runtime as runtime
    monkeypatch.setattr(runtime, "materialize_context_diagnostic", lambda *_: pytest.fail("Diagnostic recalculation"))
    monkeypatch.setattr(runtime.YahooFinanceProvider, "fetch", lambda *_: pytest.fail("Historical download"))
    before = file_hashes(tmp_path)
    _, manifest, tables = bundle(export.build_forward_export(output))
    assert manifest["unavailable"]["context"] == reason
    assert manifest["context_coverage"]["forward"]["reason"] == reason
    assert manifest["configuration"]["config"]["market_context_trend_sessions"] == 84
    assert "project_root" not in manifest["configuration"]["config"]
    assert "market_context.csv" not in tables
    assert file_hashes(tmp_path) == before


@pytest.mark.parametrize("derived", [False, True])
@pytest.mark.parametrize("forward_state", ["missing", "failed", "available"])
def test_upstream_context_discovery_with_inherited_stages(tmp_path, monkeypatch, derived, forward_state):
    from rstock.application import market_context_runtime as runtime
    from rstock.application.domain import JobType
    from rstock.config import RStockConfig
    source, output = _fixture(tmp_path, derived=derived)
    diagnostic.materialize_forward_diagnostic(output)
    wf = tmp_path / "runs/wf/results"
    pd.DataFrame([dict(TrainStart="2023-01-03", TestStart="2023-07-03")]).to_csv(wf / "windows.csv", index=False)
    holdout = tmp_path / "runs/holdout/results"
    predictions = pd.read_csv(holdout / "holdout_predictions.csv")
    predictions["Prediction"] = predictions.Probability.ge(.6)
    predictions.to_csv(wf / "predictions.csv", index=False)
    # Explicit test-only adjusted evidence covering the fixture's Forward.
    import numpy as np
    import exchange_calendars as xcals
    dates = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2022-01-03", "2026-12-31")).tz_localize(None)
    raw = pd.DataFrame({"Adjusted": 100 * np.exp(np.arange(len(dates)) * .0005)}, index=dates)
    class Provider:
        source_name = "test"
        def fetch(self, symbol, start, end):
            return raw.loc[(raw.index >= pd.Timestamp(start)) & (raw.index < pd.Timestamp(end))]
    spec = SimpleNamespace(config=RStockConfig(project_root=tmp_path, market_context_enabled=True),
        job_type=JobType.WALK_FORWARD, source_end_to_end_run=source.name,
        source_experiment_run=None, source_walk_forward_run=None, source_threshold_calibration_run=None)
    runtime.materialize_context_diagnostic(spec, wf, provider=Provider())
    spec.job_type = JobType.HOLDOUT_EVALUATION
    runtime.materialize_context_diagnostic(spec, holdout, provider=Provider())
    if forward_state == "available":
        spec.job_type = JobType.FORWARD_SIMULATION
        runtime.materialize_context_diagnostic(spec, output, provider=Provider())
    elif forward_state == "failed":
        (output / export.DIAGNOSTIC_MANIFEST).write_text(json.dumps({"status": "unavailable", "reason": "interrupted"}))
    monkeypatch.setattr(runtime.YahooFinanceProvider, "fetch", lambda *_: pytest.fail("Export must not acquire SPY"))
    before = file_hashes(tmp_path)
    _, manifest, tables = bundle(export.build_forward_export(output))
    expected = {"walk_forward", "holdout"} | ({"forward"} if forward_state == "available" else set())
    assert set(tables["context_metrics.csv"].stage) == expected
    assert set(tables["context_robustness.csv"].stage) == expected
    assert set(tables["context_metrics.csv"].context_source_run_id) == {"wf", "holdout"} | ({"forward"} if forward_state == "available" else set())
    assert manifest["context_coverage"]["walk_forward"]["status"] == "available"
    assert manifest["context_coverage"]["holdout"]["status"] == "available"
    assert len(manifest["context_provenance"]) == len(expected)  # no duplicate references
    assert set(tables["context_metrics.csv"].canonical_combination_id) <= set(tables["modeles_t0.csv"].canonical_combination_id)
    if derived:
        removed = tables["modeles_t0.csv"].query("origin == 'removed'").iloc[0].canonical_combination_id
        assert removed in set(tables["context_metrics.csv"].query("stage == 'walk_forward'").canonical_combination_id)
        assert removed not in set(tables["context_metrics.csv"].query("stage == 'forward'").canonical_combination_id)
        assert tables["context_metrics.csv"].loc[tables["context_metrics.csv"].canonical_combination_id.eq(removed), "origin"].eq("removed").all()
    assert file_hashes(tmp_path) == before
    if forward_state == "available":
        path = output / export.DIAGNOSTIC_MANIFEST
        saved = json.loads(path.read_text())
        saved["stage_references"]["walk_forward"]["sha256"] = "0" * 64
        path.write_text(json.dumps(saved))
        _, broken, partial = bundle(export.build_forward_export(output))
        assert "integrity_error" in broken["unavailable"]["context:wf"]
        assert "walk_forward" not in set(partial["context_metrics.csv"].stage)
        assert "forward" in set(partial["context_metrics.csv"].stage)
