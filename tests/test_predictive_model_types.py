import json
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_duplication import config_from_historical_snapshot
from rstock.combination_planning import CombinationPlan, build_combination_plan
from rstock.combinations import generate_symbol_sets, predictive_model_identity
from rstock.features import model_predictor_columns
from rstock.modeling import fit_booster, predict_probabilities
from rstock.walk_forward import evaluate_walk_forward
import rstock.modeling as modeling
import rstock.walk_forward as wf

KINDS = ("constant_probability", "target_only", "target_and_external", "external_only")


def prepared():
    dates = pd.bdate_range("2025-01-01", periods=24)
    frame = pd.DataFrame({"AAA.intraday_target": np.arange(24) % 2,
        "AAA.intraday_down_target": (np.arange(24) % 3 == 0).astype(int),
        "AAA_intraday_J-1": np.arange(24) / 100, "BBB_intraday_J-1": np.arange(24) / 200,
        "mon": 0}, index=dates)
    for name in ("intraday_return", "overnight_return", "close_to_close_return", "mfe", "mae"):
        frame["AAA." + name] = 0.
    return frame


def config(tmp_path, kind, **values):
    return replace(DEFAULT_CONFIG, project_root=tmp_path, predictive_model_type=kind,
        lag_depth=1, permutation_depth=1, combination_workers=1, xgb_nthread=1, xgb_rounds=1,
        walk_forward_min_train_size=8, walk_forward_test_size=4, walk_forward_step_size=4,
        final_holdout_size=4, qualification_min_windows=2,
        qualification_min_positive_observations=1, **values)


@pytest.mark.parametrize("kind,columns", [
    ("constant_probability", []), ("target_only", ["AAA_intraday_J-1"]),
    ("target_and_external", ["AAA_intraday_J-1", "BBB_intraday_J-1"]),
    ("external_only", ["BBB_intraday_J-1", "mon"]),
])
def test_exact_information_sets(tmp_path, kind, columns):
    cfg = config(tmp_path, kind, date_feature_regex="mon")
    assert model_predictor_columns(prepared(), "AAA", ["BBB"], cfg) == columns


@pytest.mark.parametrize("kind", KINDS)
def test_snapshot_roundtrip_and_legacy_default(tmp_path, kind):
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config(tmp_path, kind), symbols=("AAA", "BBB"))
    raw = spec.to_dict()
    assert config_from_historical_snapshot(raw["rstock_config"], current_project_root=tmp_path).predictive_model_type == kind
    assert ExperimentSpec.from_dict(raw).config.predictive_model_type == kind
    raw["rstock_config"].pop("predictive_model_type")
    assert ExperimentSpec.from_dict(raw).config.predictive_model_type == "external_only"
    assert config_from_historical_snapshot(raw["rstock_config"], current_project_root=tmp_path).predictive_model_type == "external_only"


@pytest.mark.parametrize("kind", KINDS[:2])
def test_one_target_plan_roundtrip_and_no_prefilter(tmp_path, kind):
    cfg = config(tmp_path, kind, predictor_prefilter_enabled=True)
    assert cfg.predictor_prefilter_enabled is False
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=cfg, symbols=("AAA",))
    plan = build_combination_plan(target_symbols=spec.target_symbols, predictor_symbols=spec.symbols,
        permutation_depth=cfg.permutation_depth, predictive_model_type=kind)
    assert plan.count() == 1
    assert plan.row_at(0) == ("AAA",)
    assert CombinationPlan.from_dict(plan.to_dict()).plan_sha256 == plan.plan_sha256
    assert generate_symbol_sets(["AAA"], 3, predictive_model_type=kind).to_dict("records") == [{"V0": "AAA"}]
    assert predictive_model_identity('["AAA"]', "constant_probability", "Up") != predictive_model_identity('["AAA"]', "target_only", "Up")


def test_invalid_type_and_operational_guard(tmp_path):
    with pytest.raises(ValueError, match="predictive_model_type"):
        replace(DEFAULT_CONFIG, predictive_model_type="unknown")
    for kind in KINDS[:-1]:
        with pytest.raises(ValueError, match="research"):
            ExperimentSpec(job_type=JobType.END_TO_END, config=config(tmp_path, kind), symbols=("AAA", "BBB"), auto_promote_candidates=True)


def test_constant_probabilities_use_known_labels_and_are_not_complements(tmp_path, monkeypatch):
    monkeypatch.setattr(modeling, "xgboost_module", lambda: pytest.fail("XGBoost must not be loaded"))
    train = prepared().iloc[:8].copy()
    train.loc[train.index[-1], "AAA.intraday_target"] = np.nan
    cfg = config(tmp_path, "constant_probability")
    up = fit_booster(train, [], "AAA.intraday_target", cfg)
    down = fit_booster(train, [], "AAA.intraday_down_target", cfg)
    assert up.probability == 3 / 7
    assert down.probability == 3 / 8
    np.testing.assert_array_equal(predict_probabilities(up, prepared(), []), np.full(24, 3 / 7))
    assert up.record["TrainObservations"] == 7
    assert up.record["TrainEnd"] == train.index[-2]
    with pytest.raises(ValueError, match="known binary"):
        fit_booster(train.iloc[:0], [], "AAA.intraday_target", cfg)


@pytest.mark.parametrize("window_mode", ["expanding", "rolling"])
def test_constant_full_wf_without_xgboost_and_frozen_test_probabilities(tmp_path, monkeypatch, window_mode):
    monkeypatch.setattr(modeling, "xgboost_module", lambda: pytest.fail("No XGBoost"))
    monkeypatch.setattr(wf, "xgboost_module", lambda: pytest.fail("No WF XGBoost"))
    cfg = config(tmp_path, "constant_probability", walk_forward_window_mode=window_mode, walk_forward_train_size=8)
    frame = prepared()
    result = evaluate_walk_forward(frame, pd.DataFrame({"V0": ["AAA"]}), cfg)
    for _, window in result.windows.iterrows():
        train = frame.loc[window["TrainStart"]:window["TrainEnd"]]
        predictions = result.predictions[result.predictions["Window"] == window["Window"]]
        assert predictions["UpProbability"].eq(train["AAA.intraday_target"].mean()).all()
        assert predictions["DownProbability"].eq(train["AAA.intraday_down_target"].mean()).all()
    assert result.run_configuration["candidate_status"] == "no_qualified_candidates"
    assert result.run_configuration["holdout_status"] == "not_executed"
    assert result.run_configuration["data_status"] == "available"


@pytest.mark.parametrize("kind", KINDS)
def test_real_e2e_uses_one_type_and_finishes_without_qualified_candidates(tmp_path, monkeypatch, kind):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    from rstock.application.end_to_end import load_pipeline_manifest
    cfg = config(tmp_path, kind, qualification_min_median_auc=1.0,
                 qualification_min_pct_windows_above_random=1.0)
    spec = ExperimentSpec(job_type=JobType.END_TO_END, config=replace(cfg, qualification_min_windows=100), symbols=("AAA", "BBB"), target_symbols=("AAA",), context_symbols=("BBB",))
    repository = RunRepository(tmp_path / "runs")
    root = repository.create(spec)
    monkeypatch.setattr(workflows, "_prepared_inputs", lambda *args, **kwargs: (prepared(), ["AAA", "BBB"], ["AAA"], {}))
    if kind == "constant_probability":
        monkeypatch.setattr(modeling, "xgboost_module", lambda: pytest.fail("No constant XGBoost"))
        monkeypatch.setattr(wf, "xgboost_module", lambda: pytest.fail("No constant WF XGBoost"))
        import rstock.streaming_walk_forward as streaming
        monkeypatch.setattr(streaming, "xgboost_module", lambda: pytest.fail("No constant streaming XGBoost"))
    execute_run(repository, root, 1)
    assert repository.status(root)["status"] == "completed", repository.status(root).get("error")
    summary = repository.summary(root)
    assert summary["candidate_status"] == "no_qualified_candidates"
    assert summary["holdout_status"] == "not_executed"
    assert summary["data_status"] == "available"
    manifest = load_pipeline_manifest(repository, root)
    child = manifest["stages"][0]["child_run_id"]
    assert repository.load_spec(child).config.predictive_model_type == kind
    assert (repository.run_directory(child) / "results/predictions.csv").is_file()


def test_constant_streamed_resume_reconciles_and_does_not_retrain(tmp_path, monkeypatch):
    import rstock.streaming_walk_forward as streaming
    from rstock.checkpoints import CheckpointManager, CheckpointIncompatibleError
    cfg = config(tmp_path, "constant_probability")
    root = tmp_path / "run"; output = root / "results"
    batch_sizes = {"walk_forward": cfg.walk_forward_batch_size, "final_holdout": cfg.final_holdout_batch_size,
                   "predictor_prefilter_walk_forward": cfg.predictor_prefilter_batch_size}
    def manager(identity="constant"):
        return CheckpointManager(root, run_id="run", job_type="walk_forward", configuration_fingerprint=identity, batch_sizes=batch_sizes)
    original = streaming._insert_predictions
    monkeypatch.setattr(streaming, "_insert_predictions", lambda *a, **k: (_ for _ in ()).throw(RuntimeError("interrupted")))
    with pytest.raises(RuntimeError, match="interrupted"):
        streaming.run_streamed_walk_forward(prepared(), pd.DataFrame({"V0": ["AAA"]}), cfg, manager(), output)
    resumed = manager()
    assert resumed.completed_batch_ids("walk_forward") == (0,)
    monkeypatch.setattr(streaming, "_insert_predictions", original)
    monkeypatch.setattr(streaming, "_walk_forward_combination", lambda *a, **k: pytest.fail("Completed constant batches must not be replayed"))
    result = streaming.run_streamed_walk_forward(prepared(), pd.DataFrame({"V0": ["AAA"]}), cfg, resumed, output)
    assert result.run_configuration["predictive_model_type"] == "constant_probability"
    assert manager().completed_batch_ids("walk_forward") == (0,)
    with pytest.raises(CheckpointIncompatibleError):
        manager("target_only")



def test_constant_e2e_calibrations_and_holdout_do_not_load_xgboost(tmp_path, monkeypatch):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    from rstock.application.end_to_end import load_pipeline_manifest
    import rstock.streaming_walk_forward as streaming
    cfg = replace(config(tmp_path, "constant_probability"), qualification_min_median_auc=0,
        qualification_min_pct_windows_above_random=0, qualification_min_worst_window_auc=0,
        qualification_max_auc_std=1., threshold_calibration_min_signals_per_window=1,
        threshold_calibration_min_robust_signals=1)
    spec = ExperimentSpec(job_type=JobType.END_TO_END, config=cfg, symbols=("AAA",), historical_data_cutoff=prepared().index.max().date().isoformat())
    repository = RunRepository(tmp_path / "runs"); root = repository.create(spec)
    monkeypatch.setattr(workflows, "_prepared_inputs", lambda *a, **k: (prepared(), ["AAA"], ["AAA"], {}))
    for module in (modeling, wf, streaming):
        monkeypatch.setattr(module, "xgboost_module", lambda: pytest.fail("Constant E2E must never load XGBoost"))
    execute_run(repository, root, 1)
    assert repository.status(root)["status"] == "completed", repository.status(root).get("error")
    manifest = load_pipeline_manifest(repository, root)
    stages = {item["stage_key"]: item for item in manifest["stages"]}
    calibration = repository.run_directory(stages["xgboost_calibration"]["child_run_id"])
    assert json.loads((calibration / "results/selected_configurations.json").read_text())["status"] == "not_applicable"
    assert repository.status(stages["threshold_calibration"]["child_run_id"])["status"] == "completed"



@pytest.mark.parametrize("kind", ["constant_probability", "target_only", "target_and_external"])
def test_real_wf_derivation_reuses_dataset_and_rebuilds_correct_candidates(tmp_path, monkeypatch, kind):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
    repo = RunRepository(tmp_path / "runs")
    source = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config(tmp_path, "external_only"),
        symbols=("AAA", "BBB"), target_symbols=("AAA",), context_symbols=("BBB",))
    parent = repo.create(source)
    with monkeypatch.context() as patch:
        patch.setattr(workflows, "_prepared_inputs", lambda *a, **k: (prepared(), ["AAA", "BBB"], ["AAA"], {}))
        execute_run(repo, parent, 1)
    assert repo.status(parent)["status"] == "completed", repo.status(parent).get("error")
    original_config = (repo.run_directory(parent) / "config.json").read_bytes()
    original_predictions = (repo.run_directory(parent) / "results/predictions.csv").read_bytes()
    derived = build_derived_walk_forward_spec(repo, parent, {"predictive_model_type": kind})
    child = repo.create(derived)
    execute_run(repo, child, 1)
    assert repo.status(child)["status"] == "completed", repo.status(child).get("error")
    rows = pd.read_csv(repo.run_directory(child) / "results/predictions.csv")
    assert rows["PredictiveModelType"].eq(kind).all()
    assert rows["FeatureColumns"].map(json.loads).iloc[0] == {
        "constant_probability": [], "target_only": ["AAA_intraday_J-1"],
        "target_and_external": ["AAA_intraday_J-1", "BBB_intraday_J-1"],
    }[kind]
    assert (repo.run_directory(parent) / "config.json").read_bytes() == original_config
    assert (repo.run_directory(parent) / "results/predictions.csv").read_bytes() == original_predictions


@pytest.mark.parametrize("value", [0, 1])
def test_constant_accepts_extreme_prevalence_without_smoothing(tmp_path, value):
    frame = prepared().iloc[:8].copy(); frame["AAA.intraday_target"] = value
    model = fit_booster(frame, [], "AAA.intraday_target", config(tmp_path, "constant_probability"))
    assert model.probability == value
    assert predict_probabilities(model, frame, []).tolist() == [value] * len(frame)



@pytest.mark.parametrize("kind", ["target_only", "target_and_external", "external_only"])
def test_xgboost_types_share_chronological_round_selection(tmp_path, kind):
    cfg = config(tmp_path, kind, xgb_round_selection_mode="chronological",
        xgb_early_stopping_max_rounds=3, xgb_early_stopping_validation_sessions=4,
        xgb_early_stopping_min_train_observations=4, xgb_early_stopping_patience=1)
    generated = generate_symbol_sets(["AAA", "BBB"], 1, target_symbols=["AAA"], predictive_model_type=kind)
    result = evaluate_walk_forward(prepared(), generated, cfg, evaluate_holdout=False)
    assert result.windows["UpRoundSelectionMode"].eq("chronological").all()
    assert result.windows["DownRoundSelectionMode"].eq("chronological").all()
    assert result.windows["UpRoundsRetained"].ge(1).all()
    assert result.windows["DownRoundsRetained"].ge(1).all()
    assert result.predictions["PredictiveModelType"].eq(kind).all()


def test_constant_ignores_xgboost_round_selection_policy(tmp_path, monkeypatch):
    monkeypatch.setattr(modeling, "xgboost_module", lambda: pytest.fail("No XGBoost"))
    monkeypatch.setattr(wf, "xgboost_module", lambda: pytest.fail("No XGBoost"))
    cfg = config(tmp_path, "constant_probability", xgb_round_selection_mode="chronological")
    result = evaluate_walk_forward(prepared(), pd.DataFrame({"V0": ["AAA"]}), cfg)
    assert result.run_configuration["round_selection"] == {"mode": "not_applicable"}


def test_derivation_refuses_missing_target_lags_before_submission(tmp_path, monkeypatch):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
    repo = RunRepository(tmp_path / "runs")
    source = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config(tmp_path, "external_only"),
        symbols=("AAA", "BBB"), target_symbols=("AAA",), context_symbols=("BBB",))
    parent = repo.create(source)
    frame = prepared().drop(columns="AAA_intraday_J-1")
    monkeypatch.setattr(workflows, "_prepared_inputs", lambda *a, **k: (frame, ["AAA", "BBB"], ["AAA"], {}))
    execute_run(repo, parent, 1)
    assert repo.status(parent)["status"] == "completed"
    with pytest.raises(ValueError, match="input columns unavailable"):
        build_derived_walk_forward_spec(repo, parent, {"predictive_model_type": "target_only"})
