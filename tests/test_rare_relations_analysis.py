import hashlib
import json
import pickle
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.features import intraday_target_column, intraday_down_target_column
from rstock.application import rare_relations_analysis as rare


def large_source(directory, evaluation_pattern="positive"):
    """Synthetic archives only; never train, fetch data, or read real runs."""
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "results").mkdir(exist_ok=True)
    (directory / "checkpoints/artifacts").mkdir(parents=True, exist_ok=True)
    dates = pd.bdate_range("2023-01-02", periods=380)
    up, down = intraday_target_column("AAA"), intraday_down_target_column("AAA")
    y = np.arange(len(dates)) % 2
    frame = pd.DataFrame({"feature": np.arange(len(dates)), up: y, down: 1-y}, index=dates)
    rng = np.random.default_rng(17)
    quality = rng.uniform(.7, .9, 300)
    if evaluation_pattern == "negative":
        quality[180:] = rng.uniform(.15, .3, 120)
    elif evaluation_pattern == "one_segment":
        quality[180:220] = rng.uniform(.94, .98, 40)
        quality[220:] = rng.uniform(.4, .49, 80)
    elif evaluation_pattern == "best_segment_only":
        quality[180:220] = rng.uniform(.94, .98, 40)
        quality[220:260] = rng.uniform(.505, .51, 40)
        quality[260:] = rng.uniform(.38, .42, 40)
    windows, predictions = [], []
    for candidate in ("good", "missing_evaluation", "bad", "missing_selection", "absent"):
        windows.append({"Set": candidate, "Window": 1, "Observation": "AAA", "FeatureColumns": '["feature"]',
                        "TrainStart": dates[0], "TrainEnd": dates[79], "TrainObservations": 80,
                        "TestStart": dates[80], "TestEnd": dates[-1], "TestObservations": 300})
        q = rng.uniform(.15, .3, 300) if candidate == "bad" else quality
        p = np.where(y[80:] == 1, q, 1-q)
        for i, date in enumerate(dates[80:]):
            if candidate == "absent" or (candidate == "missing_selection" and i == 0) or (candidate == "missing_evaluation" and i == 299):
                continue
            predictions.append({"Set": candidate, "Window": 1, "Observation": "AAA", "Date": date,
                                "UpProbability": p[i], "IntradayTarget": y[i+80]})
    pd.DataFrame(windows).to_csv(directory / "results/windows.csv", index=False)
    pd.DataFrame(predictions).to_csv(directory / "results/predictions.csv", index=False)
    raw = pickle.dumps({"prepared": frame})
    (directory / "checkpoints/artifacts/prepared_snapshot.pkl").write_bytes(raw)
    (directory / "checkpoints/artifacts/prepared_snapshot.json").write_text(json.dumps({"sha256": hashlib.sha256(raw).hexdigest()}))
    return frame


def analysis_job(tmp_path, monkeypatch, pattern="positive"):
    from rstock.config import RStockConfig
    from rstock.application.domain import ExperimentSpec, JobType, JobStatus
    from rstock.application.repository import RunRepository
    from rstock.application.runner import RunService
    from rstock.application.services import ExperimentService
    from rstock.application import scientific_analysis as analysis
    repo = RunRepository(tmp_path / "runs")
    class Backend:
        def launch(self, *args):
            pass  # no worker, fits or production job may be started by this test
    service = ExperimentService(RunService(repo, backend=Backend()))
    source = repo.create(ExperimentSpec(JobType.WALK_FORWARD, RStockConfig(project_root=tmp_path), ("AAA", "BBB")))
    repo.transition(source, JobStatus.RUNNING)
    repo.transition(source, JobStatus.COMPLETED)
    large_source(repo.run_directory(source), pattern)
    monkeypatch.setattr(analysis, "analysis_catalogue", lambda: (rare.definition(),))
    job = service.start_analysis(source, rare.ANALYSIS_ID, {}, request_id="synthetic")
    return repo, repo.load_spec(job.run_id), repo.run_directory(job.run_id) / "results"


def test_holm_family_and_hac_match_explicit_formula():
    from math import erfc, sqrt
    assert np.allclose(rare.holm(np.array([.01, .04, .03, 1.])), [.04, .09, .09, 1.])
    assert len(rare.holm(np.array([]))) == 0
    gains = .02 + .1 * np.sin(np.arange(180) / 3.)
    result = rare.hac_test(gains)
    x = gains - gains.mean()
    lag = int(np.ceil(np.cbrt(len(x))))
    ps = []
    for q, key in ((lag, "L"), (2*lag, "2L")):
        gamma = [sum(x[k:] * x[:len(x)-k]) / len(x) for k in range(q+1)]
        variance = gamma[0] + 2*sum((1-k/(q+1))*gamma[k] for k in range(1, q+1))
        se = sqrt(variance / len(x))
        assert result["SE"+key] == pytest.approx(se)
        ps.append(.5*erfc(gains.mean()/se/sqrt(2)))
    assert result["P"] == pytest.approx(max(ps))
    assert not rare.hac_test(np.ones(180))["InferenceAvailable"]
    assert not rare.hac_test(np.array([np.nan]*180))["InferenceAvailable"]


@pytest.mark.parametrize("n,positives,missing,admissible", [
    (99, 49, False, False), (100, 50, False, True), (120, 60, False, True),
    (120, 19, False, False), (180, 90, True, False),
])
def test_support_coverage_and_no_brier_gate(n, positives, missing, admissible):
    gain = .05 + .01*np.sin(np.arange(n))
    values = np.column_stack([.7-gain, np.full(n, .7), np.full(n, .3), np.full(n, .2)])
    if missing:
        values[-1] = np.nan
    y = np.zeros(n); y[:positives] = 1
    row = rare.period_table(["a"], {"a":"AAA"}, values[None], {"AAA": y}).iloc[0]
    assert row.Admissible == admissible
    assert row.MeanGainBrier < 0  # descriptive only
    if not admissible:
        assert row.P == 1 and row.ExclusionReason


@pytest.mark.parametrize("pattern,decision", [
    ("positive", "persistance historique soutenue"),
    ("negative", "persistance non démontrée"),
    ("one_segment", "persistance non démontrée"),
    ("best_segment_only", "persistance non démontrée"),
])
def test_complete_synthetic_workflow_and_evaluation_rules(tmp_path, monkeypatch, pattern, decision):
    from rstock.application import scientific_analysis as analysis
    repo, spec, output = analysis_job(tmp_path, monkeypatch, pattern)
    source = repo.run_directory(spec.source_walk_forward_run)
    before = {p: p.read_bytes() for p in source.rglob("*") if p.is_file()}
    result = analysis.execute_analysis(spec, output, None, None)
    assert result["selected_count"] == 2 and result["population_count"] == 5
    assert result["evaluable_count"] == 1 and result["indeterminate_count"] == 1
    selection = pd.read_csv(output / "selection.csv").set_index("Set")
    persistence = pd.read_csv(output / "persistence.csv").set_index("Set")
    assert selection.loc["good", "Selected"] and selection.loc["missing_evaluation", "Selected"]
    assert selection.loc["good", "Rank"] == 1 and selection.loc["missing_evaluation", "Rank"] == 2
    assert not selection.loc["missing_selection", "Selected"] and not selection.loc["absent", "Selected"]
    assert persistence.loc["good", "Decision"] == decision
    assert persistence.loc["missing_evaluation", "Decision"] == "indéterminé"
    assert persistence.loc["missing_evaluation", "P"] == persistence.loc["missing_evaluation", "AdjustedP"] == 1
    assert persistence.loc["good", "AdjustedP"] == pytest.approx(min(1, 2*persistence.loc["good", "P"]))
    rules = json.loads((output / "scientific_rules.json").read_text(encoding="utf-8"))
    assert set(rules["selected_relations"]) == {"good", "missing_evaluation"}
    assert rules["rules"] == rare.RULES and rules["source_sha256"]
    assert all((output / relative).is_file() for relative in result["result_files"])
    interpretation = json.loads((output / "scientific_interpretation.json").read_text(encoding="utf-8"))
    analysis.validate_interpretation(interpretation)
    assert interpretation["input_sha256"]
    assert "historique conditionnelle" in " ".join(interpretation["limits"]) or "historiques" in " ".join(interpretation["limits"])
    assert before == {p:p.read_bytes() for p in before}


def test_significance_alone_cannot_bypass_one_percent_gain(tmp_path, monkeypatch):
    from rstock.application import scientific_analysis as analysis
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    source = repo.run_directory(spec.source_walk_forward_run)
    predictions = pd.read_csv(source / "results/predictions.csv")
    quality = .502 + .0001*np.sin(np.arange(len(predictions)))
    predictions["UpProbability"] = np.where(predictions.IntradayTarget == 1, quality, 1-quality)
    predictions.to_csv(source / "results/predictions.csv", index=False)
    spec = analysis.prepare_analysis(repo, spec.source_walk_forward_run, rare.ANALYSIS_ID, {}, request_id="small-gain")
    result = analysis.execute_analysis(spec, output, None, None)
    selection = pd.read_csv(output / "selection.csv").set_index("Set")
    assert selection.loc["good", "AdjustedP"] <= .05
    assert 0 < selection.loc["good", "RelativeGain"] < .01
    assert result["selected_count"] == 0
    assert "gain relatif" in selection.loc["good", "SelectionReason"]


def test_selection_checkpoint_reused_after_evaluation_interruption(tmp_path, monkeypatch):
    from rstock.application import scientific_analysis as analysis
    from rstock.progress import CancellationRequested
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    original = rare.period_table
    def interrupted(candidates, *args, **kwargs):
        if len(candidates) == 2:
            raise CancellationRequested("synthetic evaluation cancellation")
        return original(candidates, *args, **kwargs)
    monkeypatch.setattr(rare, "period_table", interrupted)
    with pytest.raises(CancellationRequested):
        analysis.execute_analysis(spec, output, None, None)
    def resumed(candidates, *args, **kwargs):
        assert len(candidates) == 2, "Selection must come from its committed checkpoint"
        return original(candidates, *args, **kwargs)
    monkeypatch.setattr(rare, "period_table", resumed)
    assert analysis.execute_analysis(spec, output, None, None)["sustained_count"] == 1
    monkeypatch.setattr(rare, "execute", lambda *_: pytest.fail("Reuse completed analysis"))
    republished = output.parent / "_republished"
    assert analysis.execute_analysis(spec, republished, None, None)["selected_count"] == 2
    assert (republished / "selection.csv").read_bytes() == (output / "selection.csv").read_bytes()
    # Source changes must still prevent checkpoint reuse.
    source = repo.run_directory(spec.source_walk_forward_run) / "results/predictions.csv"
    source.write_bytes(source.read_bytes() + b"\n")
    with pytest.raises(ValueError, match="digest_changed"):
        analysis.execute_analysis(spec, republished, None, None)


def test_old_definition_readable_but_not_executable(tmp_path, monkeypatch):
    from dataclasses import replace
    from rstock.application import scientific_analysis as analysis
    from rstock.application.domain import ExperimentSpec
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    old = replace(spec, scientific_analysis={**spec.scientific_analysis, "definition_version": "2",
                  "parameters": {"selection_mode":"proportion", "selection_fraction":.6}})
    assert ExperimentSpec.from_dict(old.to_dict()).to_dict() == old.to_dict()
    with pytest.raises(ValueError, match="version_changed"):
        analysis.execute_analysis(old, output, None, None)


def test_real_analysis_phases_feed_progress_and_resource_recorder(tmp_path, monkeypatch):
    from rstock.application import scientific_analysis as analysis, resource_telemetry
    from rstock.application.runner import ProgressReporter
    from rstock.telemetry import ProcessSample
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    run_id = output.parent.name
    cpu = [0.]
    def sample(pid):
        cpu[0] += .01
        return ProcessSample(pid, 10, cpu[0], 1024*1024, None)
    monkeypatch.setattr(resource_telemetry, "process_sample", sample)
    monkeypatch.setattr(resource_telemetry, "descendant_pids", lambda _: set())
    clock = [0.]
    def monotonic():
        clock[0] += .01  # deterministic sampling intervals even for the tiny publishing phase
        return clock[0]
    monkeypatch.setattr(resource_telemetry.time, "monotonic", monotonic)
    recorder = resource_telemetry.ResourceRecorder(output.parent, run_id, {})
    reporter = ProgressReporter(repo, run_id)
    reporter.resources = recorder
    events = []
    original_update = analysis.AnalysisProgress.update
    def unthrottled(self, completed):
        self.last = float("-inf")  # expose each small synthetic unit, without sleeping
        original_update(self, completed)
    monkeypatch.setattr(analysis.AnalysisProgress, "update", unthrottled)
    class Callback:
        configure_phases = reporter.configure_phases
        def __call__(self, event):
            events.append(event)
            reporter(event)
    try:
        result = analysis.execute_analysis(spec, output, Callback(), None)
        # The ordinary worker owns final publication and global completion.
        assert repo.read_json(run_id, "progress.json")["workflow_percent"] < 100
        reporter.phase_started("publishing")
        reporter.phase_completed("publishing")
        reporter.complete_workflow()
    finally:
        recorder.close("completed")
    assert result["sustained_count"] == 1
    phases = json.loads(recorder.summary_path.read_text(encoding="utf-8"))["attempts"][0]["phase_rows"]
    expected = [name for name, _ in analysis.RARE_ANALYSIS_PHASES]
    assert [row["name"] for row in phases] == expected
    assert all(row["status"] == "completed" and row["duration_seconds"] >= 0 for row in phases)
    assert all(row["cpu_seconds"] > 0 and row["rss_peak_sampled_bytes"] > 0 for row in phases)
    scoring = next(row for row in phases if row["name"] == "analysis_scoring")
    assert scoring["completed_items"] == 1198 and scoring["item_kind"] == "lignes"
    for stage in ("analysis_scoring", "analysis_selection", "analysis_evaluation"):
        assert any(e.stage == stage and e.completed_units is not None and 0 < e.completed_units < e.total_units for e in events)
    for unit in ("octets", "lignes", "fenêtres", "relations", "fichiers"):
        assert any(e.details.get("unit") == unit for e in events)
    assert repo.read_json(run_id, "progress.json")["workflow_percent"] == 100


def test_resume_reports_restoration_not_recomputed_analysis(tmp_path, monkeypatch):
    from rstock.application import scientific_analysis as analysis
    from rstock.application.runner import ProgressReporter
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    analysis.execute_analysis(spec, output, None, None)
    monkeypatch.setattr(rare, "execute", lambda *_: pytest.fail("No recalculation"))
    events = []
    reporter = ProgressReporter(repo, output.parent.name)
    class Callback:
        configure_phases = reporter.configure_phases
        def __call__(self, event):
            events.append(event)
            reporter(event)
    assert analysis.execute_analysis(spec, output.parent / "_restore", Callback(), None)["selected_count"] == 2
    phases = [e.stage for e in events if e.details.get("phase_event") == "started"]
    assert phases == ["analysis_preparation", "analysis_restore", "analysis_outputs"]
    assert any(e.stage == "analysis_restore" and e.details.get("unit") == "octets" and e.completed_units == e.total_units for e in events)


@pytest.mark.parametrize("kind,reason", [("short", "aucune relation admissible"),
    ("losers", "aucune relation sélectionnée"), ("missing_evaluation", "aucune évaluation concluante")])
def test_scientific_stops_publish_diagnostics_without_relaxation(tmp_path, monkeypatch, kind, reason):
    from rstock.application import scientific_analysis as analysis
    repo, spec, output = analysis_job(tmp_path, monkeypatch)
    source = repo.run_directory(spec.source_walk_forward_run)
    # Create a new immutable request after changing the synthetic source.
    predictions = pd.read_csv(source / "results/predictions.csv")
    if kind == "short":
        predictions = predictions.loc[predictions.Date.isin(predictions.Date.unique()[:100])]
    elif kind == "losers":
        predictions["UpProbability"] = 1 - predictions.UpProbability
        predictions = predictions.loc[predictions.Set.ne("bad")]
    else:
        predictions = predictions.loc[predictions.Set.ne("good")]
    predictions.to_csv(source / "results/predictions.csv", index=False)
    spec = analysis.prepare_analysis(repo, spec.source_walk_forward_run, rare.ANALYSIS_ID, {}, request_id=kind)
    result = analysis.execute_analysis(spec, output, None, None)
    assert result["stop_reason"] == reason
    assert result["sustained_count"] == 0
    interpretation = json.loads((output / "scientific_interpretation.json").read_text(encoding="utf-8"))
    if result["selected_count"] == 0:
        assert "sans objet" in " ".join(interpretation["findings"])
    table = pd.read_csv(output / "persistence.csv")
    assert "MeanConstantLogLoss" in table and "Segment3Gain" in table


def fixture_source(tmp_path):
    index = pd.bdate_range("2025-07-07", periods=10)
    up, down = intraday_target_column("AAA"), intraday_down_target_column("AAA")
    frame = pd.DataFrame({"feature": np.arange(10, dtype=float),
        up: [0, 1, 1, 1, 0, 1, 0, 1, 0, 1], down: [0, 0, 0, 0, 1, 0, 1, 0, 1, 0]}, index=index)
    frame.loc[index[1], "feature"] = np.nan
    frame.loc[index[2], down] = np.nan
    key = '["AAA", "BBB"]'
    windows = pd.DataFrame([
        {"Set": key, "Window": 1, "Observation": "AAA", "FeatureColumns": '["feature"]',
         "TrainStart": index[0], "TrainEnd": index[3], "TrainObservations": 2,
         "TestStart": index[4], "TestEnd": index[6], "TestObservations": 3},
        {"Set": key, "Window": 2, "Observation": "AAA", "FeatureColumns": '["feature"]',
         "TrainStart": index[0], "TrainEnd": index[6], "TrainObservations": 5,
         "TestStart": index[7], "TestEnd": index[9], "TestObservations": 3},
    ])
    predictions = pd.DataFrame({"Set": key, "Window": [1]*3+[2]*3,
        "Date": index[4:].strftime("%Y-%m-%d"), "Observation": "AAA",
        "IntradayTarget": frame[up].iloc[4:].to_numpy(), "UpProbability": [.3, .7, .2, .6, .4, .6]})
    source = tmp_path / "source"
    (source / "results").mkdir(parents=True)
    (source / "checkpoints/artifacts").mkdir(parents=True)
    windows.to_csv(source / "results/windows.csv", index=False)
    predictions.to_csv(source / "results/predictions.csv", index=False)
    raw = pickle.dumps({"prepared": frame})
    (source / "checkpoints/artifacts/prepared_snapshot.pkl").write_bytes(raw)
    (source / "checkpoints/artifacts/prepared_snapshot.json").write_text(json.dumps({"sha256": hashlib.sha256(raw).hexdigest()}))
    return source, frame, windows, predictions


def test_mask_and_constant_never_use_test_labels(tmp_path):
    _, frame, windows, predictions = fixture_source(tmp_path)
    refs, size = rare.build_references(frame, windows)
    ref = refs[(windows.Set.iloc[0], 1)]
    assert ref.probability == .5  # both feature and Down-label masks are applied
    changed = frame.copy()
    changed.loc[changed.index[4:], intraday_target_column("AAA")] = 1
    other, _ = rare.build_references(changed, windows)
    assert other[(windows.Set.iloc[0], 1)].probability == ref.probability
    scored = rare.score_chunk(predictions, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    assert scored.Period.tolist() == ["selection"]*3 + ["evaluation"]*3
    assert np.allclose(scored.GainLogLoss, scored.ConstantLogLoss - scored.LogLoss)


def test_archive_corruption_and_duplicate_rows_rejected(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    path = source / "checkpoints/artifacts/prepared_snapshot.pkl"
    path.write_bytes(path.read_bytes()+b"changed")
    with pytest.raises(ValueError, match="Empreinte"):
        rare.load_snapshot(path, path.with_suffix(".json"))
    refs, size = rare.build_references(frame, windows)
    seen = np.zeros(size, bool)
    rare.score_chunk(predictions.iloc[:2], frame, refs, seen, "2025-07-15", "2025-07-18")
    with pytest.raises(ValueError, match="déjà rencontrée"):
        rare.score_chunk(predictions.iloc[:1], frame, refs, seen, "2025-07-15", "2025-07-18")


def test_bad_labels_dates_and_overlapping_windows_rejected(tmp_path):
    _, frame, windows, predictions = fixture_source(tmp_path)
    refs, size = rare.build_references(frame, windows)
    bad = predictions.copy()
    bad.loc[0, "IntradayTarget"] = 1
    with pytest.raises(ValueError, match="Labels"):
        rare.score_chunk(bad, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    bad = predictions.copy()
    bad.loc[0, "Date"] = "2025-07-07"
    with pytest.raises(ValueError, match="calendrier"):
        rare.score_chunk(bad, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    duplicate = windows.iloc[[0, 0]].copy()
    duplicate.iloc[1, duplicate.columns.get_loc("Window")] = 2
    with pytest.raises(ValueError, match="chevauchantes"):
        rare.build_references(frame, duplicate)


def test_rules_are_frozen_and_deep_preflight_is_ready(tmp_path):
    source, _, _, _ = fixture_source(tmp_path)
    parameters = rare.validate_parameters({})
    preview = rare.preflight(SimpleNamespace(directory=source), parameters)
    assert preview["ready"] and not preview["reasons"]
    assert preview["resolved_parameters"]["rules"] == rare.RULES
    assert preview["temporal_summary"]["selection_sessions"] == 3
    with pytest.raises(ValueError, match="Paramètres inconnus"):
        rare.validate_parameters({"approved": True})
    with pytest.raises(ValueError, match="règles scientifiques"):
        rare.validate_parameters({"rules": {**rare.RULES, "alpha": .1}})


def test_calendar_default_and_session_mode_resolve_same_common_boundary(tmp_path):
    source, _, _, _ = fixture_source(tmp_path)
    before = {path: path.read_bytes() for path in source.rglob("*") if path.is_file()}
    calendar = rare.source_calendar(source)
    split = rare.resolve_split(calendar, {})
    assert split["total_sessions"] == 6 and split["selection_sessions"] == 3
    assert split["wf_start"] == "2025-07-11" and split["wf_end"] == "2025-07-18"
    assert split["selection_end"] == "2025-07-15" and split["evaluation_start"] == "2025-07-16"
    assert split == rare.resolve_split(calendar, {"selection_mode": "sessions", "selection_sessions": 3})
    assert before == {path: path.read_bytes() for path in before}
    frozen = rare.preflight(SimpleNamespace(directory=source), {})["resolved_parameters"]
    assert rare.resolve_split(calendar, frozen) == split
    changed = {**frozen, "selection_fraction": .8}
    with pytest.raises(ValueError, match="découpage figé"):
        rare.resolve_split(calendar, changed)


def test_partial_windows_and_missing_candidate_dates_preserve_union_calendar(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    other = windows.copy()
    other["Set"] = '["AAA", "CCC"]'
    windows = pd.concat([windows, other], ignore_index=True)
    rows = predictions.iloc[[0, 2, 4]].copy()
    rows["Set"] = other.Set.iloc[0]
    windows.to_csv(source / "results/windows.csv", index=False)
    pd.concat([predictions, rows], ignore_index=True).to_csv(source / "results/predictions.csv", index=False)
    calendar = rare.source_calendar(source)
    split = rare.resolve_split(calendar, {})
    assert split["total_sessions"] == 6 and split["selection_end"] == "2025-07-15"
    assert split["coverage"][0]["selection_sessions"] == 3
    assert split["coverage"][1]["selection_sessions"] == 2
    assert split["coverage"][1]["evaluation_sessions"] == 1
    assert len(split["warnings"]) == 2
    # A date absent from every archived prediction is not manufactured from the snapshot.
    archived = pd.concat([predictions, rows], ignore_index=True)
    archived = archived.loc[archived.Date.ne(predictions.Date.iloc[1])]
    archived.to_csv(source / "results/predictions.csv", index=False)
    updated = rare.source_calendar(source)
    assert len(updated["dates"]) == 5 and pd.Timestamp(predictions.Date.iloc[1]) not in updated["dates"]


@pytest.mark.parametrize("parameters", [
    {"selection_mode": "sessions", "selection_sessions": 6},
    {"selection_fraction": .01},
    {"selection_mode": "sessions", "selection_sessions": 3, "selection_fraction": .6},
    {"selection_fraction": 1.}, {"selection_fraction": True},
    {"selection_mode": "sessions", "selection_sessions": 2.5},
])
def test_empty_periods_and_exclusive_modes_rejected(tmp_path, parameters):
    source, _, _, _ = fixture_source(tmp_path)
    with pytest.raises(ValueError):
        rare.resolve_split(rare.source_calendar(source), parameters)


def test_calendar_does_not_depend_on_future_probabilities_or_loss_values(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    initial = rare.resolve_split(rare.source_calendar(source), {})
    predictions.loc[3:, "UpProbability"] = [.99, .01, .99]
    predictions.to_csv(source / "results/predictions.csv", index=False)
    assert rare.resolve_split(rare.source_calendar(source), {}) == initial
    assert not hasattr(rare, "ApprovedProtocol") and not hasattr(rare, "APPROVED_PROTOCOLS")


def test_invalid_or_missing_prediction_calendar_blocks_preview(tmp_path):
    source, _, _, predictions = fixture_source(tmp_path)
    predictions.loc[0, "Date"] = "2025-07-07"
    predictions.to_csv(source / "results/predictions.csv", index=False)
    with pytest.raises(ValueError, match="calendrier"):
        rare.source_calendar(source)
    predictions.iloc[:0].to_csv(source / "results/predictions.csv", index=False)
    with pytest.raises(ValueError, match="deux séances"):
        rare.source_calendar(source)
