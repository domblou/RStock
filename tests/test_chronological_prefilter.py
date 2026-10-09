"""Independent prefilter policy, using fake boosters and frozen market inputs."""
from dataclasses import replace
import hashlib
import json

import numpy as np
import pandas as pd
import pytest

from test_chronological_pipeline import fake_xgb, _data
from rstock import modeling, walk_forward
from rstock.application import workflows, streamlit_app
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.experiment_duplication import config_from_historical_snapshot
from rstock.application.prefilter_experiments import build_derived_prefilter_spec, _validate_changes
from rstock.application.prefilter_comparison import load_prefilter_comparison
from rstock.application.prefilter_contract import load
from rstock.application.derivation import ParameterOverride, validate_overrides, stage_modes
from rstock.application.repository import RunRepository
from rstock.checkpoints import CheckpointManager, CheckpointIncompatibleError
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.progress import CancellationRequested


def _setup(tmp_path, monkeypatch):
    prepared, sets, config = _data(tmp_path)
    config = replace(config, xgb_round_selection_mode="fixed", xgb_seed=99,
        prefilter_xgb_round_selection_mode="chronological", prefilter_xgb_seed=777,
        prefilter_xgb_num_boost_round=17, predictor_prefilter_enabled=True,
        prefilter_xgb_early_stopping_max_rounds=500, walk_forward_min_train_size=252,
        predictor_prefilter_batch_size=1, qualification_min_windows=1,
        qualification_min_positive_observations=0,
        predictor_prefilter_min_median_auc=0, predictor_prefilter_min_worst_auc=0,
        predictor_prefilter_min_pct_above_random=0, predictor_prefilter_max_auc_std=1)
    monkeypatch.setattr(walk_forward, "xgboost_module", modeling.xgboost_module)
    def prepare(spec, *args):
        view = prepared.loc[:spec.historical_data_cutoff].copy()
        view.attrs["effective_end_date"] = view.index.max().isoformat()
        return view, ["AAA", "BBB"], ["AAA"], {"AAA": "XNYS", "BBB": "XNYS"}
    monkeypatch.setattr(workflows, "_prepared_inputs", prepare)
    spec = ExperimentSpec(job_type=JobType.PREDICTOR_PREFILTER, config=config,
        symbols=("AAA", "BBB"), target_symbols=("AAA",), context_symbols=("BBB",),
        historical_data_cutoff=prepared.index.max().date().isoformat())
    return prepared, sets, config, spec


def _manager(path, config):
    return CheckpointManager(path, run_id="prefilter", job_type="predictor_prefilter",
        configuration_fingerprint="frozen", batch_sizes={"predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size})


def test_historical_absence_never_inherits_chronological_wf_and_explicit_values_win(tmp_path):
    spec = ExperimentSpec(job_type=JobType.PREDICTOR_PREFILTER,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological", predictor_prefilter_enabled=True),
        symbols=("AAA", "BBB"), historical_data_cutoff="2026-09-25")
    snapshot = spec.to_dict()
    for field in modeling.PREFILTER_ROUND_SELECTION_FIELDS:
        del snapshot["rstock_config"][field]
    restored = ExperimentSpec.from_dict(snapshot)
    duplicated = config_from_historical_snapshot(snapshot["rstock_config"], current_project_root=tmp_path)
    for config in (restored.config, duplicated):
        assert config.prefilter_xgb_round_selection_mode == "fixed"
        assert config.xgb_round_selection_mode == "chronological"
    snapshot["rstock_config"].update(prefilter_xgb_round_selection_mode="chronological",
        prefilter_xgb_early_stopping_max_rounds=701, prefilter_xgb_early_stopping_patience=8)
    explicit = ExperimentSpec.from_dict(snapshot).config
    assert explicit.prefilter_xgb_early_stopping_max_rounds == 701
    assert explicit.prefilter_xgb_early_stopping_patience == 8


@pytest.mark.parametrize("field,value", [
    ("prefilter_xgb_round_selection_mode", "auto"),
    ("prefilter_xgb_early_stopping_max_rounds", 0),
    ("prefilter_xgb_early_stopping_validation_sessions", True),
    ("prefilter_xgb_early_stopping_min_train_observations", 1.5),
    ("prefilter_xgb_early_stopping_patience", -1),
    ("prefilter_xgb_early_stopping_metric", "auc"),
    ("prefilter_xgb_round_selection_protocol_version", "unknown"),
])
def test_invalid_policy_cannot_enter_a_derived_run(field, value):
    source = ExperimentSpec(job_type=JobType.PREDICTOR_PREFILTER,
        config=replace(DEFAULT_CONFIG, predictor_prefilter_enabled=True), symbols=("AAA", "BBB"), historical_data_cutoff="2026-09-25")
    with pytest.raises(ValueError):
        _validate_changes(source, {field: value})


def test_sessions_are_distinct_after_lags_and_exclusions_and_disjoint_from_test(fake_xgb, tmp_path, monkeypatch):
    prepared, sets, config, _ = _setup(tmp_path, monkeypatch)
    # Simulate closures/missing prices before preparation; lag exclusions happen
    # before the selector sees rows. No calendar-day padding is introduced.
    prepared = prepared.drop(prepared.index[[50, 80, 100]])
    prepared.loc[prepared.index[120], "BBB_intraday_J-1"] = np.nan
    result = walk_forward.evaluate_prefilter_walk_forward(prepared, sets, config)
    audit = result.round_selection_training
    optimized = audit.loc[audit.UpRoundSelectionUsed]
    assert len(optimized) > 0
    assert (optimized.UpValidationObservations == 63).all()
    assert (optimized.UpInternalTrainObservations >= 252).all()
    assert (optimized.UpInternalTrainEnd < optimized.UpValidationStart).all()
    assert (optimized.UpValidationEnd < optimized.TestStart).all()
    holdout = prepared.index[-config.final_holdout_size]
    for matrix, parameters, kwargs in fake_xgb:
        assert matrix.data.index.is_unique
        assert matrix.data.index.normalize().is_unique
        assert not matrix.data.isna().any().any()
        assert matrix.data.index.max() < holdout
        assert parameters["seed"] == 777
        assert parameters["max_depth"] == config.prefilter_xgb_max_depth
        if "evals" in kwargs:
            validation = kwargs["evals"][0][0].data
            assert len(validation) == 63 and validation.index.is_unique
            assert matrix.data.index.intersection(validation.index).empty
            assert matrix.data.index.max() < validation.index.min() < holdout
            assert kwargs["num_boost_round"] == 500
    coverage = result.telemetry["round_selection_coverage"]
    assert coverage["Up"]["optimized_windows"] == len(optimized)
    assert coverage["Up"]["fallback_windows"] > 0
    assert coverage["Down"]["status"] == "not_applicable"
    assert set(audit.loc[~audit.UpRoundSelectionUsed, "UpRoundsRetained"]) == {17}
    assert set(optimized.UpRoundsRetained) == {3}
    assert all(item["fallback_source"] == "prefilter_xgb_num_boost_round" for item in audit.UpRoundSelectionPolicy)


def test_same_day_rows_are_not_counted_as_two_sessions(fake_xgb):
    frame = pd.DataFrame({"x": [1, 2], "label": [0, 1]},
        index=pd.to_datetime(["2020-01-02 09:00", "2020-01-02 10:00"]))
    with pytest.raises(ValueError, match="distinct trading sessions"):
        modeling.fit_chronological_booster(frame, ["x"], "label", DEFAULT_CONFIG,
            parameters=modeling.prefilter_xgboost_parameters(DEFAULT_CONFIG))
    assert not fake_xgb


def test_holdout_mutation_cannot_change_prefilter_selection(fake_xgb, tmp_path, monkeypatch):
    prepared, sets, config, _ = _setup(tmp_path, monkeypatch)
    first = walk_forward.evaluate_prefilter_walk_forward(prepared, sets, config)
    poisoned = prepared.copy()
    poisoned.iloc[-config.final_holdout_size:] = 999
    second = walk_forward.evaluate_prefilter_walk_forward(poisoned, sets, config)
    pd.testing.assert_frame_equal(first.qualification, second.qualification)
    assert first.round_selection_training.UpTrainingIdentity.tolist() == second.round_selection_training.UpTrainingIdentity.tolist()


def test_rolling_252_never_silently_extends_history(fake_xgb, tmp_path, monkeypatch):
    prepared, sets, config, _ = _setup(tmp_path, monkeypatch)
    config = replace(config, walk_forward_window_mode="rolling", walk_forward_train_size=252)
    result = walk_forward.evaluate_prefilter_walk_forward(prepared, sets, config)
    coverage = result.telemetry["round_selection_coverage"]["Up"]
    assert coverage["optimized_percent"] == 0
    assert coverage["fallback_percent"] == 100
    assert set(result.round_selection_training.UpRoundSelectionFallbackReason) == {"insufficient_history"}
    assert all(kwargs["num_boost_round"] == 17 for _, _, kwargs in fake_xgb)


@pytest.mark.parametrize("method", ["single_origin", "temporal_stability", "temporal_consensus"])
def test_origin_contract(fake_xgb, tmp_path, monkeypatch, method):
    _, _, config, spec = _setup(tmp_path, monkeypatch)
    spec = replace(spec, prefilter_method=method, stability_origin_count=2, stability_step_sessions=21,
        config=replace(config, temporal_consensus_origins=2, temporal_consensus_min_occurrences=1))
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(spec)
    output = repository.run_directory(run_id) / "results"
    summary = workflows._predictor_prefilter(spec, output, None, None)
    repository.write_json(run_id, "summary.json", summary)
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.COMPLETED)
    audit = pd.read_csv(output / "prefilter_round_selection_training.csv")
    assert audit.OriginCutoff.nunique() == (1 if method == "single_origin" else 2)
    costs = json.loads((output / "prefilter_round_selection.json").read_text(encoding="utf-8"))
    assert costs["selection_rounds_run"] > 0 and costs["refit_rounds"] > 0
    assert costs["selection_worker_seconds"] >= 0 and costs["refit_worker_seconds"] >= 0
    assert costs["optimized_windows_only"]["UpLogLoss"] >= 0
    assert costs["optimized_windows_only"]["UpBrier"] >= 0
    assert 0 <= costs["optimized_windows_only"]["UpROCAUCMedian"] <= 1
    assert costs["comparison_status"] == "résultat exploratoire"
    compared = load_prefilter_comparison(tmp_path, run_id)
    assert compared.settings["prefilter_xgb_round_selection_mode"] == "chronological"
    assert compared.display_row()["Fenêtres Up optimisées"] > 0
    raw = (output / "prefilter_contract.json").read_bytes()
    assert json.loads(raw)["schema_version"] == 2
    from rstock.application.end_to_end import artifact_digests
    stage_digests = artifact_digests(repository, run_id, "prefilter")
    assert "results/prefilter_round_selection_training.csv" in stage_digests
    wf = replace(spec, job_type=JobType.WALK_FORWARD, source_prefilter_run=run_id,
        prefilter_method="single_origin", stability_origin_count=5, stability_step_sessions=1,
        source_prefilter_contract_sha256=hashlib.sha256(raw).hexdigest(),
        source_prepared_dataset_sha256=summary["traceability"]["prepared_dataset_sha256"],
        evaluate_final_holdout=False)
    load(repository, wf)
    call_count = len(fake_xgb)
    resumed = workflows._predictor_prefilter(spec, output, None, None)
    assert resumed["result_files"] == summary["result_files"]
    assert len(fake_xgb) == call_count
    # Contract audit digests remain stable across a full resume.
    assert (output / "prefilter_contract.json").read_bytes() == raw
    (output / "prefilter_round_selection_training.csv").write_text("changed", encoding="utf-8")
    with pytest.raises(ValueError, match="training audit changed"):
        load(repository, wf)


def test_batch_interruption_keeps_audit_and_skips_committed_fits(fake_xgb, tmp_path, monkeypatch):
    prepared, _, config, _ = _setup(tmp_path, monkeypatch)
    sets = generate_symbol_sets(("AAA", "BBB"), 1)
    path = tmp_path / "checkpoint"
    worker = _manager(path, config)
    worker.start_attempt(resumed=False)
    workflow = _manager(path, config)
    commit = workflow.commit_batch
    def interrupt(*args, **kwargs):
        commit(*args, **kwargs)
        raise CancellationRequested("after durable batch")
    monkeypatch.setattr(workflow, "commit_batch", interrupt)
    with pytest.raises(CancellationRequested):
        walk_forward.evaluate_prefilter_walk_forward(prepared, sets, config, checkpoint_manager=workflow)
    committed_calls = len(fake_xgb)
    worker.finish_attempt("cancelled")  # stale worker reconciles workflow writes
    resumed = _manager(path, config)
    assert resumed.completed_batch_ids("predictor_prefilter_walk_forward") == (0,)
    first_audit = resumed.load_batch("predictor_prefilter_walk_forward", 0)["round_selection_training"]
    result = walk_forward.evaluate_prefilter_walk_forward(prepared, sets, config, checkpoint_manager=resumed)
    assert len(fake_xgb) == committed_calls * 2  # only the other pair is fitted
    assert len(result.round_selection_training) == len(first_audit) * 2
    pd.testing.assert_frame_equal(resumed.load_artifact("prefilter_round_selection_training"), result.round_selection_training)


def test_policy_changes_and_legacy_batches_cannot_resume_silently(tmp_path):
    fixed = DEFAULT_CONFIG
    manager = _manager(tmp_path / "legacy", fixed)
    manager.commit_artifact("prefilter_qualification", pd.DataFrame())
    walk_forward.ensure_prefilter_training_policy(manager, fixed)
    chronological = replace(fixed, prefilter_xgb_round_selection_mode="chronological")
    with pytest.raises(CheckpointIncompatibleError, match="Historical fixed"):
        walk_forward.ensure_prefilter_training_policy(manager, chronological)
    fresh = _manager(tmp_path / "new", chronological)
    walk_forward.ensure_prefilter_training_policy(fresh, chronological)
    for config in (fixed, replace(chronological, prefilter_xgb_early_stopping_max_rounds=400)):
        with pytest.raises(CheckpointIncompatibleError, match="policy differs"):
            walk_forward.ensure_prefilter_training_policy(fresh, config)


def test_prefilter_derivation_reuses_snapshot_and_exact_input_pairs(fake_xgb, tmp_path, monkeypatch):
    _, _, config, spec = _setup(tmp_path, monkeypatch)
    fixed = replace(spec, config=replace(config, prefilter_xgb_round_selection_mode="fixed"))
    repository = RunRepository(tmp_path / "runs")
    source_id = repository.create(fixed)
    summary = workflows._predictor_prefilter(fixed, repository.run_directory(source_id) / "results", None, None)
    repository.write_json(source_id, "summary.json", summary)
    repository.transition(source_id, JobStatus.RUNNING)
    repository.transition(source_id, JobStatus.COMPLETED)
    derived = build_derived_prefilter_spec(repository, source_id, {
        "prefilter_xgb_round_selection_mode": "chronological", "prefilter_xgb_early_stopping_max_rounds": 600})
    assert derived.config.xgb_round_selection_mode == "fixed"
    assert derived.prefilter_derivation["source_univariate_sets_sha256"]
    monkeypatch.setattr(workflows, "generate_symbol_sets", lambda *args, **kwargs: pytest.fail("input discovery must be inherited"))
    from rstock.application.derived_snapshot import load_source_prepared_snapshot
    def inherited(spec, *args):
        return load_source_prepared_snapshot(repository, spec, source_run_id=source_id,
            source_job_type=JobType.PREDICTOR_PREFILTER,
            expected_snapshot_sha256=derived.prefilter_derivation["prepared_snapshot_sha256"])
    monkeypatch.setattr(workflows, "_prepared_inputs", inherited)
    run_id = repository.create(derived)
    workflows._predictor_prefilter(derived, repository.run_directory(run_id) / "results", None, None)
    assert any(kwargs.get("num_boost_round") == 600 for _, _, kwargs in fake_xgb)
    input_path = repository.run_directory(source_id) / "checkpoints/artifacts/prefilter_univariate_sets.pkl"
    input_path.write_bytes(b"changed")
    from rstock.application.prefilter_experiments import validate_prefilter_source
    with pytest.raises(ValueError, match="input pairs changed"):
        validate_prefilter_source(repository, derived)
    input_path.unlink()
    with pytest.raises(ValueError, match="requires frozen Prefilter input pairs"):
        build_derived_prefilter_spec(repository, source_id, {"prefilter_xgb_round_selection_mode": "chronological"})


def test_e2e_prefilter_owns_policy_and_wf_fork_cannot_recompute_it():
    overrides = (ParameterOverride("prefilter_xgb_round_selection_mode", "fixed", "chronological"),)
    validate_overrides("prefilter", overrides, schema_version=3)
    with pytest.raises(ValueError, match="inherited"):
        validate_overrides("walk_forward", overrides, schema_version=3)
    modes = stage_modes("prefilter", schema_version=3)
    assert modes["prefilter"] == "recomputed" and modes["walk_forward"] == "recomputed"
    assert stage_modes("walk_forward", schema_version=3)["prefilter"] == "inherited"


def test_shared_ui_exposes_configurable_policy_without_a_new_button(monkeypatch):
    seen = {}
    class UI:
        def selectbox(self, label, choices, **kwargs):
            seen[kwargs["key"]] = choices
            return "chronological"
        def number_input(self, label, **kwargs):
            seen[kwargs["key"]] = kwargs
            return 650 if kwargs["key"].endswith("max_rounds") else kwargs["value"]
        def caption(self, *args):
            pass
    monkeypatch.setattr(streamlit_app, "st", UI())
    values = streamlit_app._prefilter_round_selection_inputs(DEFAULT_CONFIG, "derive-")
    assert values["prefilter_xgb_round_selection_mode"] == "chronological"
    assert values["prefilter_xgb_early_stopping_max_rounds"] == 650
    assert "max_value" not in seen["derive-prefilter_xgb_early_stopping_max_rounds"]
