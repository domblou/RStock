"""Chronological protocol tests using fake boosters, never experiment training."""
from dataclasses import replace
from types import SimpleNamespace
import sqlite3

import numpy as np
import pandas as pd
import pytest

from rstock import modeling, walk_forward, streaming_walk_forward as streaming
from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_duplication import walk_forward_duplication_draft, experiment_spec_from_duplication
from rstock.application.round_selection_comparison import temporal_block_interval, _window_losses
from rstock.evaluation import classification_metrics
from rstock.progress import CancellationRequested


def _train(n=315):
    return pd.DataFrame({"feature": np.linspace(-1, 1, n), "label": np.arange(n) % 2},
                        index=pd.bdate_range("2020-01-01", periods=n))


def _fake_selection(monkeypatch, *, best=4, rounds=35, score=.4):
    calls, final = [], []
    class Matrix:
        def __init__(self, data, label=None, feature_names=None):
            self.data, self.label = data, label
    def train(params, matrix, **kwargs):
        calls.append((params, matrix, kwargs))
        kwargs["evals_result"]["validation"] = {"logloss": [score] * rounds}
        for callback in kwargs["callbacks"]:
            callback.after_iteration(None, 0, {})
        return SimpleNamespace(best_iteration=best, best_score=score)
    xgb = SimpleNamespace(DMatrix=Matrix, train=train, callback=SimpleNamespace(TrainingCallback=object), __version__="fake")
    monkeypatch.setattr(modeling, "xgboost_module", lambda: xgb)
    monkeypatch.setattr(walk_forward, "xgboost_module", lambda: xgb)
    def fit(frame, names, target, config, **kwargs):
        final.append((frame.copy(), kwargs["parameters"].num_boost_round))
        return "final_model"
    monkeypatch.setattr(modeling, "_fit_fixed_booster", fit)
    return calls, final


def test_historical_deserialization_and_duplication_keep_fixed_and_recorded_rounds(tmp_path):
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_rounds=17), symbols=("AAA", "BBB"))
    payload = spec.to_dict()
    for name in modeling.ROUND_SELECTION_FIELDS:
        payload["rstock_config"].pop(name)
    restored = ExperimentSpec.from_dict(payload)
    assert restored.config.xgb_round_selection_mode == "fixed"
    assert restored.config.xgb_rounds == 17
    draft = walk_forward_duplication_draft("source", {"configuration": payload, "summary": {}})
    duplicated = experiment_spec_from_duplication(draft, current_config=replace(spec.config, xgb_rounds=80), use_run_config=True)
    assert duplicated.config.xgb_rounds == 17
    assert duplicated.config.xgb_round_selection_mode == "fixed"
    payload["rstock_config"].update(xgb_round_selection_mode="chronological", xgb_early_stopping_patience=12)
    payload["evaluate_final_holdout"] = False
    explicit = ExperimentSpec.from_dict(payload)
    assert explicit.config.xgb_round_selection_mode == "chronological"
    assert explicit.config.xgb_early_stopping_patience == 12


@pytest.mark.parametrize("n,reason", [(314, "insufficient_history"), (315, None)])
def test_boundary_best_iteration_and_fresh_full_refit(monkeypatch, tmp_path, n, reason):
    calls, final = _fake_selection(monkeypatch)
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_rounds=17, xgb_round_selection_mode="chronological")
    train = _train(n)
    booster, diagnostics = modeling.fit_chronological_booster(train, ["feature"], "label", config, parameters=modeling.historical_xgboost_parameters(config))
    assert booster == "final_model"
    assert diagnostics["RoundSelectionFallbackReason"] == reason
    assert final[0][1] == (17 if reason else 5)
    pd.testing.assert_frame_equal(final[0][0], train)
    if not reason:
        assert len(calls[0][1].data) == 252
        validation = calls[0][2]["evals"][0][0]
        assert len(validation.data) == 63
        assert calls[0][1].data.index.max() < validation.data.index.min()
        assert calls[0][2]["early_stopping_rounds"] == 30
        assert calls[0][0]["eval_metric"] == "logloss"
        assert diagnostics["RoundSelectionUsed"]


@pytest.mark.parametrize("part,reason", [("validation", "validation_single_class"), ("train", "internal_train_single_class")])
def test_single_class_falls_back_to_configured_rounds(monkeypatch, tmp_path, part, reason):
    calls, final = _fake_selection(monkeypatch)
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_rounds=23)
    train = _train()
    train.iloc[-63:] = train.iloc[-63:].assign(label=0) if part == "validation" else train.iloc[-63:]
    if part == "train":
        train.iloc[:-63, train.columns.get_loc("label")] = 0
    _, diagnostics = modeling.fit_chronological_booster(train, ["feature"], "label", config, parameters=modeling.historical_xgboost_parameters(config))
    assert diagnostics["RoundSelectionFallbackReason"] == reason
    assert final[0][1] == 23
    assert not calls


def test_cap_is_successful_selection_and_invalid_score_is_fallback(monkeypatch):
    config = replace(DEFAULT_CONFIG, xgb_rounds=19)
    for score, expected in ((.4, 500), (float("nan"), 19)):
        _fake_selection(monkeypatch, best=499, rounds=500, score=score)
        _, result = modeling.fit_chronological_booster(_train(), ["feature"], "label", config, parameters=modeling.historical_xgboost_parameters(config))
        assert result["RoundsRetained"] == expected
        assert result["RoundSelectionUsed"] == (expected == 500)
        assert result["SelectionCapReached"] == (expected == 500)


def test_cancellation_during_selection_is_not_a_fallback(monkeypatch):
    calls, final = _fake_selection(monkeypatch)
    counter = 0
    def cancellation():
        nonlocal counter
        counter += 1
        return counter >= 2
    with pytest.raises(CancellationRequested):
        modeling.fit_chronological_booster(_train(), ["feature"], "label", DEFAULT_CONFIG, parameters=modeling.historical_xgboost_parameters(DEFAULT_CONFIG), cancellation_check=cancellation)
    assert not final


def test_final_training_checks_cancellation_inside_iterations(monkeypatch):
    class Matrix:
        def __init__(self, *args, **kwargs):
            pass
    def train(params, matrix, **kwargs):
        kwargs["callbacks"][0].after_iteration(None, 0, {})
        raise AssertionError("cancelled final training should not finish")
    xgb = SimpleNamespace(DMatrix=Matrix, train=train, callback=SimpleNamespace(TrainingCallback=object))
    monkeypatch.setattr(modeling, "xgboost_module", lambda: xgb)
    with pytest.raises(CancellationRequested):
        modeling.fit_booster(_train(), ["feature"], "label", DEFAULT_CONFIG, cancellation_check=lambda: True)


def test_prefilter_explicitly_disables_shared_chronological_policy(monkeypatch):
    seen = []
    def evaluator(*args, **kwargs):
        seen.append(kwargs)
        return SimpleNamespace(window_records=[{}], prediction_records=[{}])
    monkeypatch.setattr(walk_forward, "_walk_forward_combination", evaluator)
    monkeypatch.setattr(walk_forward, "qualify_combinations", lambda *_: pd.DataFrame({"EligibleRank": [1], "Eligible": [True]}))
    config = replace(DEFAULT_CONFIG, xgb_round_selection_mode="chronological")
    walk_forward._prefilter_combination({}, (None, config, None, {}, 252, 63, 63), None)
    assert seen[0]["round_selection_enabled"] is False
    assert seen[0]["parameters"].num_boost_round == config.prefilter_xgb_num_boost_round


def test_complete_pipeline_accepts_chronological_and_rejects_legacy_contract(tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological")
    for job in (JobType.END_TO_END, JobType.FORWARD_SIMULATION, JobType.HOLDOUT_EVALUATION, JobType.PRODUCTION_TRAINING):
        required = ({"source_end_to_end_run": "parent", "forward_simulation_start_date": "2026-01-05",
                     "forward_simulation_end_date": "2026-04-05"} if job is JobType.FORWARD_SIMULATION
                    else {"model_id": "model"} if job is JobType.PRODUCTION_TRAINING else {})
        ExperimentSpec(job_type=job, config=config, symbols=("AAA", "BBB"), **required)
        with pytest.raises(ValueError, match="v2 training contract"):
            ExperimentSpec(job_type=job, config=config, symbols=("AAA", "BBB"), e2e_xgboost_protocol_version=1, **required)
    with pytest.raises(ValueError, match="evaluate_final_holdout"):
        ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config, symbols=("AAA", "BBB"))


def _pairs(n=12, overlap=1):
    dates = pd.bdate_range("2020-01-01", periods=n + overlap)
    return pd.DataFrame([{"Direction": direction, "CanonicalId": candidate,
        "TestStart": dates[i], "TestEnd": dates[i + overlap - 1],
        "DeltaLogLoss": -.02 + .015 * np.sin(i / 3)}
        for direction in ("Up", "Down") for candidate in ("A", "B") for i in range(n)])


def test_short_or_overlapping_history_is_exploratory():
    result = temporal_block_interval(_pairs())
    assert result["interval"] is None
    assert result["status"] == "résultat exploratoire"
    overlapping = temporal_block_interval(_pairs(120, 10))
    assert overlapping["overlap_span"] == 10
    assert overlapping["interval"] is None


def test_joint_blocks_do_not_fake_precision_by_multiplying_correlated_candidates():
    pairs = _pairs(240)
    result = temporal_block_interval(pairs, draws=100)
    duplicate = pairs.copy()
    duplicate["CanonicalId"] += "duplicate"
    enlarged = temporal_block_interval(pd.concat([pairs, duplicate]), draws=100)
    assert result["interval"] is not None
    assert enlarged["interval"] == pytest.approx(result["interval"])


def test_sql_losses_match_common_metrics():
    frame = pd.DataFrame({"UpTarget": [0, 1, 1, 0], "UpPrediction": [0, 1, 0, 1], "UpProbability": [0, 1, .4, .8]})
    with sqlite3.connect(":memory:") as connection:
        frame.to_sql("predictions", connection, index=False)
        actual = streaming._sql_classification_metrics(connection, "Up")
    expected = classification_metrics(frame.UpTarget, frame.UpPrediction, frame.UpProbability).as_columns()
    for name in expected:
        assert actual[name] == pytest.approx(expected[name])


def test_coverage_counts_up_and_down_separately_and_survives_batch_reduction():
    windows = pd.DataFrame({
        "UpRoundSelectionUsed": [True, False], "DownRoundSelectionUsed": [False, False],
        "UpRoundSelectionMode": ["chronological"] * 2, "DownRoundSelectionMode": ["chronological"] * 2,
        "UpRoundsRetained": [5, 17], "DownRoundsRetained": [17, 17],
        "UpRoundSelectionFallbackReason": [None, "insufficient_history"],
        "DownRoundSelectionFallbackReason": ["validation_single_class"] * 2})
    result = modeling.round_selection_coverage_batches([windows.iloc[:1], windows.iloc[1:]])
    assert result["Up"]["optimized_windows"] == 1
    assert result["Up"]["optimized_percent"] == 50
    assert result["Down"]["fallback_percent"] == 100
    assert result["Up"]["rounds_distribution"]["50%"] == 11


def test_chronological_derivation_preserves_population_and_reference_rounds(tmp_path):
    from test_walk_forward_derivation import _parent
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec, load_frozen_walk_forward_candidates
    repository, source, expected, _ = _parent(tmp_path)
    derived = build_derived_walk_forward_spec(repository, source, {"xgb_round_selection_mode": "chronological", "evaluate_final_holdout": False})
    assert derived.config.xgb_rounds == repository.load_spec(source).config.xgb_rounds
    actual = load_frozen_walk_forward_candidates(repository, derived)
    pd.testing.assert_frame_equal(actual.slice(0, actual.count()), expected)
    with pytest.raises(ValueError, match="preserve reference"):
        build_derived_walk_forward_spec(repository, source, {"xgb_round_selection_mode": "chronological", "xgb_rounds": 80, "evaluate_final_holdout": False})


def test_resume_keeps_optimized_diagnostics_and_stale_manifest_is_reconciled(tmp_path, monkeypatch):
    from test_walk_forward_resume import _fixture, _InlineRiskExecutor
    from rstock.checkpoints import CheckpointManager
    prepared, generated, config, manager, output = _fixture(tmp_path)
    config = replace(config, xgb_round_selection_mode="chronological", xgb_early_stopping_validation_sessions=4, xgb_early_stopping_min_train_observations=4)
    _fake_selection(monkeypatch, best=1, rounds=3)
    monkeypatch.setattr(walk_forward, "predict_probabilities_matrix", lambda booster, matrix: np.full(len(matrix.data), .5))
    monkeypatch.setattr(streaming, "ProcessPoolExecutor", _InlineRiskExecutor)
    stale = CheckpointManager(manager.root.parent, run_id="run", job_type="walk_forward", configuration_fingerprint="resume-fixture", batch_sizes=manager.batch_sizes)
    stale.start_attempt(resumed=False)
    manager = CheckpointManager(manager.root.parent, run_id="run", job_type="walk_forward", configuration_fingerprint="resume-fixture", batch_sizes=manager.batch_sizes)
    original = streaming._insert_predictions
    monkeypatch.setattr(streaming, "_insert_predictions", lambda *_: (_ for _ in ()).throw(RuntimeError("interrupted")))
    with pytest.raises(RuntimeError, match="interrupted"):
        streaming.run_streamed_walk_forward(prepared, generated, config, manager, output, evaluate_holdout=False)
    saved = manager.load_batch("walk_forward", 0)["windows"].copy()
    monkeypatch.setattr(streaming, "_insert_predictions", original)
    monkeypatch.setattr(streaming, "_walk_forward_combination", lambda *_: (_ for _ in ()).throw(AssertionError("completed batch rerun")))
    result = streaming.run_streamed_walk_forward(prepared, generated, config, manager, output, evaluate_holdout=False)
    pd.testing.assert_frame_equal(saved, manager.load_batch("walk_forward", 0)["windows"])
    assert result.run_configuration["round_selection_coverage"]["Up"]["optimized_percent"] == 100
    assert result.run_configuration["round_selection_coverage"]["Down"]["fallback_percent"] == 100
    stale.finish_attempt("completed")
    assert stale.completed_batch_ids("walk_forward") == manager.completed_batch_ids("walk_forward")
    assert stale.manifest["attempt_count"] == 1


def test_up_down_are_independent_and_test_labels_cannot_select_rounds(monkeypatch):
    calls, final = _fake_selection(monkeypatch)
    index = pd.bdate_range("2020-01-01", periods=405)
    prepared = pd.DataFrame({"BBB_intraday_J-1": np.linspace(-1, 1, len(index)),
        "AAA.intraday_target": np.arange(len(index)) % 2,
        "AAA.intraday_down_target": np.arange(len(index)) % 2}, index=index)
    prepared.iloc[252:315, prepared.columns.get_loc("AAA.intraday_down_target")] = 0
    for name in ("overnight_return", "intraday_return", "close_to_close_return", "mfe", "mae"):
        prepared[f"AAA.{name}"] = 0.
    config = replace(DEFAULT_CONFIG, lag_depth=1, xgb_round_selection_mode="chronological", xgb_rounds=17)
    monkeypatch.setattr(walk_forward, "predict_probabilities_matrix", lambda booster, matrix: np.full(len(matrix.data), .5))
    context = (prepared, config, index[378], {}, 315, 63, 63)
    first = walk_forward._walk_forward_combination({"V0": "AAA", "V1": "BBB"}, context, None)
    assert first.window_records[0]["UpRoundsRetained"] == 5
    assert first.window_records[0]["DownRoundsRetained"] == 17
    assert first.window_records[0]["DownRoundSelectionFallbackReason"] == "validation_single_class"
    original_train = [frame.copy() for frame, _ in final]
    prepared.loc[index[315:378], ["AAA.intraday_target", "AAA.intraday_down_target"]] = 1
    second = walk_forward._walk_forward_combination({"V0": "AAA", "V1": "BBB"}, context, None)
    assert second.window_records[0]["UpRoundsRetained"] == 5
    assert second.window_records[0]["DownRoundsRetained"] == 17
    for before, (after, _) in zip(original_train, final[2:]):
        pd.testing.assert_frame_equal(before, after)


def test_paired_comparison_retains_ineligible_candidates_and_rejects_labels_changed(tmp_path, monkeypatch):
    from test_walk_forward_derivation import _parent
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
    from rstock.application.round_selection_comparison import compare_round_selection
    from rstock.application.domain import JobStatus
    repository, source, _, _ = _parent(tmp_path)
    derived = build_derived_walk_forward_spec(repository, source, {"xgb_round_selection_mode": "chronological", "evaluate_final_holdout": False})
    child = repository.create(derived)
    repository.transition(child, JobStatus.RUNNING)
    repository.transition(child, JobStatus.COMPLETED)
    set_id = '["AAA","BBB"]'
    predictions = pd.DataFrame({"Set": [set_id] * 8, "Window": [1] * 4 + [2] * 4,
        "Date": pd.bdate_range("2021-01-01", periods=8), "IntradayTarget": [0, 1] * 4,
        "DownTarget": [1, 0] * 4, "UpProbability": [.3, .7] * 4, "DownProbability": [.7, .3] * 4})
    windows = pd.DataFrame({"Set": [set_id] * 2, "Window": [1, 2], "TrainStart": ["2020-01-01"] * 2,
        "TrainEnd": ["2020-12-31", "2021-01-06"], "TestStart": [predictions.Date.iloc[0], predictions.Date.iloc[4]],
        "TestEnd": [predictions.Date.iloc[3], predictions.Date.iloc[7]], "TestObservations": [4, 4],
        "UpROCAUC": [1., 1.], "DownROCAUC": [1., 1.]})
    for run_id in (source, child):
        root = repository.run_directory(run_id) / "results"
        root.mkdir(exist_ok=True)
        actual = windows.copy()
        if run_id == child:
            for direction in ("Up", "Down"):
                actual[f"{direction}RoundSelectionUsed"] = [False, True]
                actual[f"{direction}RoundSelectionMode"] = "chronological"
                actual[f"{direction}RoundsRetained"] = [4, 3]
                actual[f"{direction}RoundSelectionFallbackReason"] = ["insufficient_history", None]
        actual.to_csv(root / "windows.csv", index=False)
        repository.write_json(run_id, "results/run_configuration.json", {"round_selection_coverage": modeling.round_selection_coverage(actual)})
        predictions.to_csv(root / "predictions.csv", index=False)
        pd.DataFrame({"Set": [set_id], "Eligible": [run_id == source]}).to_csv(root / "qualification.csv", index=False)
    summary, pairs = compare_round_selection(tmp_path, source, child)
    assert len(pairs) == 4
    assert summary["all_windows"]["candidate_direction_windows"] == 4
    assert summary["optimized_windows"]["candidate_direction_windows"] == 2
    assert summary["coverage"]["Down"]["optimized_percent"] == 50
    assert summary["all_windows"]["inference"]["status"] == "résultat exploratoire"
    from contextlib import nullcontext
    from rstock.application import streamlit_app
    texts, tables = [], []
    class FakeStreamlit:
        session_state = SimpleNamespace(lab_config=replace(DEFAULT_CONFIG, project_root=tmp_path))
        def caption(self, value):
            texts.append(value)
        def info(self, value):
            texts.append(value)
        def subheader(self, value):
            pass
        def bar_chart(self, value):
            pass
        def line_chart(self, value):
            pass
        def download_button(self, *args, **kwargs):
            pass
        def expander(self, value):
            return nullcontext()
    monkeypatch.setattr(streamlit_app, "st", FakeStreamlit())
    monkeypatch.setattr(streamlit_app, "render_dataframe", lambda frame, **kwargs: tables.append(frame))
    # Streamlit also executes this file as a script without a package context.
    monkeypatch.setattr(streamlit_app, "__package__", "")
    details = [{"configuration": repository.load_spec(run_id).to_dict()} for run_id in (source, child)]
    streamlit_app._render_round_selection_summary(child, details[1])
    streamlit_app._render_paired_round_comparison(details, [source, child])
    assert any("Résultat exploratoire" in value for value in texts)
    assert any("% optimisées" in frame.columns for frame in tables)
    assert any("IC 95 % log loss" in frame.columns for frame in tables)
    predictions.loc[0, "IntradayTarget"] = 1
    predictions.to_csv(repository.run_directory(child) / "results" / "predictions.csv", index=False)
    with pytest.raises(ValueError, match="test dates/labels differ"):
        compare_round_selection(tmp_path, source, child)
