import hashlib
import json
from dataclasses import replace

import pandas as pd
import pytest

from test_prefilter_experiments import _fixture, _complete
from rstock.application import workflows
from rstock.application.domain import ExperimentSpec
from rstock.application.prefilter_consensus import aggregate_consensus, resolve_consensus_origins
from rstock.application.prefilter_experiments import build_derived_prefilter_spec
from rstock.application.experiment_duplication import config_from_historical_snapshot
from rstock.config import DEFAULT_CONFIG


def test_calendar_origins_and_future_rejection(monkeypatch):
    origins = resolve_consensus_origins("2026-09-25", "XNYS", 4, 21)
    # Market session offsets include holidays, rather than business-day arithmetic.
    from rstock.calendars import offset_market_session
    assert origins == tuple(pd.Timestamp(offset_market_session("2026-09-25", "XNYS", n)) for n in (0, 21, 42, 63))
    assert origins[1] != pd.Timestamp("2026-09-25") - pd.Timedelta(days=21)
    monkeypatch.setattr("rstock.application.prefilter_consensus.offset_market_session",
                        lambda cutoff, calendar, n: pd.Timestamp(cutoff) + pd.Timedelta(days=n))
    with pytest.raises(ValueError, match="earlier"):
        resolve_consensus_origins("2026-09-25", "XNYS", 4, 21)


def test_consensus_counts_final_selection_without_scores_top_n_or_correlation():
    origins = pd.date_range("2026-09-21", periods=4)
    # Both B and C reach 3/4 although origin Top N is only two: union keeps both.
    tables = []
    for i in range(4):
        tables.append(pd.DataFrame([
            dict(Observation="A", Predictor="B", PrefilterStatus="retained" if i != 0 else "removed_redundancy", PrefilterScore=-999, PrefilterRank=2),
            dict(Observation="A", Predictor="C", PrefilterStatus="retained" if i != 1 else "rejected_top_n", PrefilterScore=999, PrefilterRank=1),
            dict(Observation="D", Predictor="B", PrefilterStatus="retained" if i == 0 else "rejected_threshold", PrefilterScore=999),
        ]))
    aggregate, retained = aggregate_consensus(tables, origins=origins, targets=["D", "A"], predictors=["C", "B"], min_occurrences=3)
    assert retained == {"A": ["B", "C"], "D": []}
    assert aggregate.query("Observation == 'A'")["selection_frequency"].tolist() == [.75, .75]
    assert aggregate["candidate_id"].is_unique
    scrambled, retained_again = aggregate_consensus([t.iloc[::-1] for t in tables], origins=origins, targets=["A", "D"], predictors=["B", "C"], min_occurrences=3)
    pd.testing.assert_frame_equal(aggregate, scrambled)
    assert retained_again == retained


def test_defaults_persistence_and_historical_mode(tmp_path):
    assert DEFAULT_CONFIG.temporal_consensus_origins == 4
    assert DEFAULT_CONFIG.temporal_consensus_step_sessions == 21
    assert DEFAULT_CONFIG.temporal_consensus_min_occurrences == 3
    old = config_from_historical_snapshot({}, current_project_root=tmp_path)
    assert old.prefilter_selection_mode == "single_origin"
    with pytest.raises(ValueError, match="exceed"):
        replace(DEFAULT_CONFIG, temporal_consensus_min_occurrences=5)
    from rstock.config import save_user_settings, load_user_settings, UI_SETTINGS_DEFAULTS
    base = replace(DEFAULT_CONFIG, project_root=tmp_path)
    consensus = replace(base, prefilter_selection_mode="temporal_consensus",
                        temporal_consensus_origins=5, temporal_consensus_step_sessions=10,
                        temporal_consensus_min_occurrences=4)
    save_user_settings(consensus, UI_SETTINGS_DEFAULTS, default_config=base)
    restored, _, warning = load_user_settings(base)
    assert warning is None and restored == consensus


def test_settings_ui_exposes_all_modes_and_restores_consensus_values(monkeypatch):
    from contextlib import nullcontext
    from rstock.application import streamlit_app
    seen = {}
    class UI:
        def container(self, **kwargs):
            assert kwargs == {"border": True}
            return nullcontext()
        def subheader(self, title):
            assert title == "Sélection temporelle"
        def selectbox(self, label, choices, **kwargs):
            assert choices == ("single_origin", "temporal_stability", "temporal_consensus")
            return "temporal_consensus"
        def number_input(self, label, **kwargs):
            seen[label] = kwargs
            return kwargs["value"]
        def columns(self, number):
            return [self] * number
        def caption(self, *args):
            pass
    monkeypatch.setattr(streamlit_app, "st", UI())
    values = streamlit_app._prefilter_temporal_settings(DEFAULT_CONFIG)
    assert values == dict(prefilter_selection_mode="temporal_consensus", temporal_consensus_origins=4,
                          temporal_consensus_step_sessions=21, temporal_consensus_min_occurrences=3)
    assert all(not value["disabled"] for value in seen.values())


def test_comparator_distinguishes_internal_occurrences_and_between_run_overlap(tmp_path, monkeypatch):
    from rstock.application.prefilter_comparison import load_prefilter_comparison, compare_prefilter_candidates
    repository, spec, run_id, summary, _ = _consensus_source(tmp_path, monkeypatch)
    _complete(repository, run_id, summary)
    consensus = load_prefilter_comparison(tmp_path, run_id)
    assert consensus.occurrence_distribution == summary["occurrence_distribution"]
    assert consensus.display_row()["Sélection 4/4"] == 1
    assert consensus.retained == 1
    other = replace(consensus, run_id="another-consensus")
    assert compare_prefilter_candidates([consensus, other]).overlap_rate == 1.


def _consensus_source(tmp_path, monkeypatch):
    repository, single, _, _, _ = _fixture(tmp_path, monkeypatch)
    config = replace(single.config, temporal_consensus_origins=4,
                     temporal_consensus_step_sessions=1, temporal_consensus_min_occurrences=3)
    spec = replace(single, config=config, prefilter_method="temporal_consensus")
    calls = []
    prepare = workflows._prepared_inputs
    def autonomous(origin_spec, *args):
        calls.append(origin_spec.historical_data_cutoff)
        frame, predictors, targets, calendars = prepare(origin_spec, *args)
        # Every autonomous origin receives an identical amount of history.
        frame.index = pd.date_range(end=origin_spec.historical_data_cutoff, periods=10, freq="B")
        frame.attrs["effective_end_date"] = frame.index.max().isoformat()
        return frame, predictors, targets, calendars
    monkeypatch.setattr(workflows, "_prepared_inputs", autonomous)
    run_id = repository.create(spec)
    summary = workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", None, None)
    return repository, spec, run_id, summary, calls


def test_autonomous_origin_snapshots_resume_and_derived_reuse(tmp_path, monkeypatch):
    original_preparation = workflows._prepared_inputs
    repository, spec, run_id, summary, calls = _consensus_source(tmp_path, monkeypatch)
    assert calls == ["2026-09-25", "2026-09-24", "2026-09-23", "2026-09-22"]
    assert summary["occurrence_distribution"]["4"] == 1
    assert summary["retained_predictors"] == 1
    report = json.loads((repository.run_directory(run_id) / "results/predictor_prefilter.json").read_text())
    assert all(pd.Timestamp(item["first_date"]) == pd.date_range(end=item["last_date"], periods=10, freq="B")[0] for item in report["origins"])
    def forbidden(*args, **kwargs):
        pytest.fail("Origin recomputed or data downloaded")
    monkeypatch.setattr(workflows, "_prepared_inputs", forbidden)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", forbidden)
    again = workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", None, None)
    assert again["origins"] == summary["origins"]
    _complete(repository, run_id, summary)
    restored = ExperimentSpec.from_dict(spec.to_dict())
    assert restored.config == spec.config and restored.prefilter_method == "temporal_consensus"
    # Restore the scientific evaluator; derived run reuses data, not selections.
    _, _, _, _, original = _fixture(tmp_path, monkeypatch)
    monkeypatch.setattr(workflows, "_prepared_inputs", original_preparation)
    derived = build_derived_prefilter_spec(repository, run_id, {"predictor_prefilter_top_n": 2})
    child = repository.create(derived)
    result = workflows._predictor_prefilter(derived, repository.run_directory(child) / "results", None, None)
    assert result["retained_predictors"] == 2
    assert [o["prepared_dataset_sha256"] for o in result["origins"]] == [o["prepared_dataset_sha256"] for o in summary["origins"]]


def test_incomplete_origin_fails_without_reducing_denominator(tmp_path, monkeypatch):
    repository, single, _, _, _ = _fixture(tmp_path, monkeypatch)
    spec = replace(single, prefilter_method="temporal_consensus")
    run_id = repository.create(spec)
    with pytest.raises(ValueError, match="origin .* not executable"):
        workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", None, None)
    assert not (repository.run_directory(run_id) / "results/prefilter_contract.json").exists()


def test_consensus_union_can_exceed_origin_top_n_and_all_frequencies():
    tables = [pd.DataFrame([dict(Observation="A", Predictor=pred,
                                PrefilterStatus="retained" if j != i else "rejected_top_n")
                           for j, pred in enumerate("BCDE")]) for i in range(4)]
    result, retained = aggregate_consensus(tables, origins=pd.date_range("2026-09-21", periods=4),
                                         targets=["A"], predictors=list("BCDE"), min_occurrences=3)
    assert len(retained["A"]) == 4  # Each origin retained only three.
    assert result["selection_count"].tolist() == [3] * 4
    tables = [pd.DataFrame([dict(Observation="A", Predictor=pred,
                                PrefilterStatus="retained" if i < j + 1 else "rejected_top_n")
                           for j, pred in enumerate("BCDE")]) for i in range(4)]
    result, _ = aggregate_consensus(tables, origins=pd.date_range("2026-09-21", periods=4),
                                   targets=["A"], predictors=list("BCDE"), min_occurrences=3)
    assert result["selection_frequency"].tolist() == [.25, .5, .75, 1.]


def test_preview_does_not_cap_consensus_population_at_origin_top_n():
    from rstock.combination_planning import build_combination_plan, build_combination_preview
    raw = build_combination_plan(target_symbols=["A"], predictor_symbols=list("ABCDE"), permutation_depth=1)
    legacy = build_combination_preview(raw, max_combinations_per_batch=10,
                                       prefilter_enabled=True, prefilter_top_n=3)
    consensus = build_combination_preview(raw, max_combinations_per_batch=10,
                                          prefilter_enabled=True, prefilter_top_n=3,
                                          prefilter_selection_mode="temporal_consensus")
    assert legacy.max_combinations_after_prefilter == 3
    assert consensus.max_combinations_after_prefilter == 4


def test_interrupted_consensus_reconciles_and_reuses_completed_origins(tmp_path, monkeypatch):
    repository, single, _, _, _ = _fixture(tmp_path, monkeypatch)
    spec = replace(single, prefilter_method="temporal_consensus", config=replace(
        single.config, temporal_consensus_step_sessions=1))
    original_prepare = workflows._prepared_inputs
    def prepare(origin_spec, *args):
        view, predictors, targets, calendars = original_prepare(origin_spec, *args)
        view.index = pd.date_range(end=origin_spec.historical_data_cutoff, periods=10, freq="B")
        view.attrs["effective_end_date"] = view.index.max().isoformat()
        return view, predictors, targets, calendars
    monkeypatch.setattr(workflows, "_prepared_inputs", prepare)
    evaluate = workflows.evaluate_prefilter_walk_forward
    called = []
    def interrupted(view, *args, **kwargs):
        called.append(view.index.max().date().isoformat())
        if len(called) == 3:
            raise RuntimeError("interrupted third origin")
        return evaluate(view, *args, **kwargs)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", interrupted)
    run_id = repository.create(spec)
    output = repository.run_directory(run_id) / "results"
    with pytest.raises(RuntimeError, match="third origin"):
        workflows._predictor_prefilter(spec, output, None, None)
    assert not (output / "prefilter_contract.json").exists()
    completed = repository.run_directory(run_id) / "checkpoints/consensus_origins/2026-09-25/checkpoints/artifacts/prefilter_selection.pkl"
    before = completed.read_bytes()
    remaining = []
    def resume(view, *args, **kwargs):
        remaining.append(view.index.max().date().isoformat())
        return evaluate(view, *args, **kwargs)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", resume)
    result = workflows._predictor_prefilter(spec, output, None, None)
    assert remaining == ["2026-09-23", "2026-09-22"]
    assert completed.read_bytes() == before
    assert len(result["origins"]) == 4


def test_origin_is_identical_to_autonomous_single_and_ignores_future_data(tmp_path, monkeypatch):
    import numpy as np
    from types import SimpleNamespace
    from rstock.application.domain import JobType
    from rstock.application.repository import RunRepository
    from rstock.application import prefilter_consensus
    from rstock.predictor_prefilter import select_predictors
    repository = RunRepository(tmp_path / "runs")
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, predictor_prefilter_enabled=True,
                     walk_forward_end_offset_sessions=0, model_history_days=60,
                     final_holdout_size=2, temporal_consensus_origins=2,
                     temporal_consensus_step_sessions=21, temporal_consensus_min_occurrences=1)
    spec = ExperimentSpec(job_type=JobType.PREDICTOR_PREFILTER, config=config,
                          symbols=("AAA", "BBB"), target_symbols=("AAA",), context_symbols=("BBB",),
                          historical_data_cutoff="2026-09-25", prefilter_method="temporal_consensus")
    index = pd.bdate_range("2026-05-01", "2026-10-09")
    prices = pd.DataFrame(index=index)
    for symbol in spec.symbols:
        prices[f"{symbol}.Open"] = 100.
        prices[f"{symbol}.Close"] = np.where(np.arange(len(index)) % 2, 102., 98.)
        prices[f"{symbol}.High"] = 103.
        prices[f"{symbol}.Low"] = 97.
    loaded = []
    def market(_self, origin_spec, *, as_of, **kwargs):
        loaded.append((as_of, origin_spec.config.model_history_days))
        # Deliberately return future rows to test the preparation's PIT boundary.
        view = prices.loc[pd.Timestamp(as_of) - pd.Timedelta(days=origin_spec.config.model_history_days):].copy()
        return SimpleNamespace(prices=view, symbols=list(spec.symbols)), {s: "XNYS" for s in spec.symbols}
    monkeypatch.setattr(workflows.MarketDataService, "load", market)
    from rstock.walk_forward import PrefilterWalkForwardResult
    def evaluate(view, sets, config, **kwargs):
        qualification = pd.DataFrame([dict(Observation="AAA", Predictors='["BBB"]', Eligible=True,
                                           ROCAUCMedian=.7, ROCAUCWorst=.6, ROCAUCStd=.01,
                                           PctWindowsAboveRandom=.8)])
        return PrefilterWalkForwardResult(qualification, {"pairs_admissible": 1}, ("AAA",), {})
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", evaluate)
    run_id = repository.create(spec)
    workflows._predictor_prefilter(spec, repository.run_directory(run_id) / "results", None, None)
    origin = resolve_consensus_origins(spec.historical_data_cutoff, spec.calendar, 2, 21)[1]
    origin_spec = replace(spec, prefilter_method="single_origin", historical_data_cutoff=origin.date().isoformat())
    before, *_ = workflows._prepared_inputs(origin_spec, None, None)
    origin_root = repository.run_directory(run_id) / "checkpoints/consensus_origins" / origin.date().isoformat()
    import pickle
    payload = pickle.loads((origin_root / "checkpoints/artifacts/prepared_snapshot.pkl").read_bytes())
    pd.testing.assert_frame_equal(payload["prepared"], before)
    selected_before = select_predictors(evaluate(before, None, config).qualification,
                                       before.iloc[:-config.final_holdout_size],
                                       targets=["AAA"], candidate_symbols=["AAA", "BBB"], config=config)
    prices.loc[prices.index > origin] = 99999.
    after, *_ = workflows._prepared_inputs(origin_spec, None, None)
    pd.testing.assert_frame_equal(before, after)
    selected_after = select_predictors(evaluate(after, None, config).qualification,
                                      after.iloc[:-config.final_holdout_size],
                                      targets=["AAA"], candidate_symbols=["AAA", "BBB"], config=config)
    pd.testing.assert_frame_equal(selected_before.metrics, selected_after.metrics)
    assert selected_before.predictors_by_target == selected_after.predictors_by_target
    assert all(days == 60 for _, days in loaded)
