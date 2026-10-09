"""Complete training-policy contracts, using fake boosters only."""
import json
from dataclasses import replace
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock import modeling, calibration, threshold_calibration
from rstock.checkpoints import CheckpointManager
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application import workflows
from rstock.application.end_to_end import build_stage_spec, build_pipeline_manifest, _stage_parameter_contract
from rstock.application.derivation import derivation_graph, stage_modes
from rstock.application.forward_simulation import _validate_round_selection_contract


@pytest.fixture
def fake_xgb(monkeypatch):
    calls = []
    class Matrix:
        def __init__(self, data, label=None, feature_names=None):
            self.data, self.label = data.copy(), label
        def set_label(self, label):
            self.label = label
    class Booster:
        best_iteration = 2
        best_score = .4
        def __init__(self):
            self.attributes = {}
        def set_attr(self, **values):
            self.attributes.update(values)
        def attr(self, name):
            return self.attributes.get(name)
        def predict(self, matrix):
            return np.linspace(.1, .9, len(matrix.data))
        def save_model(self, path):
            path.write_text(json.dumps(self.attributes), encoding="utf-8")
    def train(parameters, matrix, **kwargs):
        calls.append((matrix, parameters, kwargs))
        if "evals_result" in kwargs:
            kwargs["evals_result"]["validation"] = {"logloss": [.6, .5, .4, .45, .5]}
        return Booster()
    xgb = SimpleNamespace(DMatrix=Matrix, train=train, callback=SimpleNamespace(TrainingCallback=object))
    monkeypatch.setattr(modeling, "xgboost_module", lambda: xgb)
    monkeypatch.setattr(calibration, "xgboost_module", lambda: xgb)
    return calls


def _data(tmp_path):
    dates = pd.bdate_range("2020-01-01", periods=425)
    prices = pd.DataFrame(index=dates)
    for symbol in ("AAA", "BBB"):
        close = 100 * (1 + np.where(np.arange(len(dates)) % 2, .02, -.02))
        prices[f"{symbol}.Open"] = 100.
        prices[f"{symbol}.Close"] = close
        prices[f"{symbol}.High"] = np.maximum(close, 100) + 1
        prices[f"{symbol}.Low"] = np.minimum(close, 100) - 1
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, lag_depth=1, combination_workers=1,
        xgb_round_selection_mode="chronological", xgb_rounds=80, xgb_early_stopping_max_rounds=12,
        xgb_early_stopping_patience=2, final_holdout_size=25,
        walk_forward_min_train_size=315, walk_forward_test_size=20, walk_forward_step_size=20)
    return prepare_dataset(prices, ("AAA", "BBB")), generate_symbol_sets(("AAA", "BBB"), 1).iloc[:1], config


def test_common_policy_reselects_each_origin_and_fallback_uses_reference(fake_xgb, tmp_path):
    frame = pd.DataFrame({"feature": np.arange(335), "label": np.arange(335) % 2},
                         index=pd.bdate_range("2020-01-01", periods=335))
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological", xgb_rounds=80)
    parameters = modeling.XGBoostParameters(2, .1, 120)
    records = []
    for size in (314, 315, 335):
        booster = modeling.fit_booster(frame.iloc[:size], ["feature"], "label", config, parameters=parameters)
        records.append(modeling.booster_training_record(booster))
    assert [record["RoundsRetained"] for record in records] == [80, 3, 3]
    assert len({record["TrainingIdentity"] for record in records}) == 3
    assert len(fake_xgb) == 5  # One fallback fit; selection and fresh fit twice.
    for matrix, _, kwargs in fake_xgb:
        if "evals" in kwargs:
            assert matrix.data.index.max() < kwargs["evals"][0][0].data.index.min()


def test_calibration_chronological_deduplicates_rounds_and_never_opens_holdout(fake_xgb, tmp_path, monkeypatch):
    prepared, sets, config = _data(tmp_path)
    candidates = [modeling.historical_xgboost_parameters(config),
                  replace(modeling.historical_xgboost_parameters(config), num_boost_round=120)]
    monkeypatch.setattr(calibration, "evaluate_locked_holdout", lambda *a, **k: pytest.fail("intermediate holdout opened"))
    results = []
    for poisoned in (False, True):
        data = prepared.copy()
        if poisoned:
            data.iloc[-25:] = 9999
        result = calibration.run_controlled_calibration(data, sets, config, candidates=candidates,
                                                       evaluate_final_holdout=False)
        results.append(result)
        assert result.run_configuration["candidate_configurations"] == 1
        assert result.run_configuration["holdout_evaluation_count"] == 0
        assert result.holdout_predictions.empty
    assert results[0].selected_configurations == results[1].selected_configurations
    pd.testing.assert_frame_equal(results[0].development_by_configuration, results[1].development_by_configuration)
    output = tmp_path / "results"
    calibration.write_calibration_results(results[0], output)
    records = pd.read_csv(output / "round_selection_training.csv")
    assert "RoundSelectionRecords" not in pd.read_csv(output / "development_metrics_by_window.csv")
    assert set(records.Direction) == {"Up", "Down"}
    assert pd.to_datetime(records.TrainingEnd).max() < prepared.index[-25]
    assert results[0].run_configuration["round_selection_coverage"]["Up"]["optimized_percent"] == 100


def test_holdout_training_and_probability_audit_have_only_development_origins(fake_xgb, tmp_path):
    prepared, sets, config = _data(tmp_path)
    development, holdout, _ = calibration.split_development_holdout(prepared, 25)
    parameters = {direction: modeling.historical_xgboost_parameters(config) for direction in ("Up", "Down")}
    predictions = threshold_calibration.generate_holdout_probabilities(
        development, holdout, sets, config, parameters_by_direction=parameters)
    records = modeling.probability_training_records(predictions)
    assert len(records) == 2
    assert set(records.Direction) == {"Up", "Down"}
    assert pd.to_datetime(records.ValidationEnd).max() < holdout.index.min()
    assert pd.to_datetime(records.TrainingEnd).max() < holdout.index.min()
    assert len(fake_xgb) == 4


def test_chronological_calibration_resume_reuses_recorded_selection(fake_xgb, tmp_path, monkeypatch):
    prepared, sets, config = _data(tmp_path)
    spec = ExperimentSpec(JobType.XGBOOST_CALIBRATION, config, symbols=("AAA", "BBB"))
    manager = CheckpointManager(tmp_path / "run", run_id="run", job_type=spec.job_type.value,
        configuration_fingerprint=spec.fingerprint, batch_sizes={"xgboost_calibration": config.walk_forward_batch_size})
    commit = manager.commit_batch
    def interrupt(*args, **kwargs):
        commit(*args, **kwargs)
        raise RuntimeError("interrupted after durable batch")
    monkeypatch.setattr(manager, "commit_batch", interrupt)
    arguments = dict(candidates=[modeling.historical_xgboost_parameters(config)],
                     evaluate_final_holdout=False, checkpoint_manager=manager)
    with pytest.raises(RuntimeError, match="durable batch"):
        calibration.run_controlled_calibration(prepared, sets, config, **arguments)
    call_count = len(fake_xgb)
    monkeypatch.setattr(manager, "commit_batch", commit)
    result = calibration.run_controlled_calibration(prepared, sets, config, **arguments)
    assert len(fake_xgb) == call_count
    assert json.loads(result.development_by_window.iloc[0].RoundSelectionRecords)[0]["TrainingIdentity"]


@pytest.mark.parametrize("mode", ["fixed", "chronological"])
@pytest.mark.parametrize("version,expected", [(1, True), (2, False)])
def test_e2e_calibration_holdout_protocol_is_explicit(tmp_path, monkeypatch, mode, version, expected):
    if mode == "chronological" and version == 1:
        return  # This combination is rejected at domain construction.
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode=mode)
    spec = ExperimentSpec(JobType.XGBOOST_CALIBRATION, config, symbols=("AAA", "BBB"),
                          source_end_to_end_run="parent", e2e_xgboost_protocol_version=version)
    monkeypatch.setattr(workflows, "_prepared_calibration_population", lambda *args: (None, None, False))
    def stop(*args, **kwargs):
        assert kwargs["evaluate_final_holdout"] is expected
        raise RuntimeError("verified before training")
    monkeypatch.setattr(workflows, "run_controlled_calibration", stop)
    with pytest.raises(RuntimeError, match="before training"):
        workflows._xgboost_calibration(spec, tmp_path / "results", None, None)


def test_historical_e2e_absence_restores_v1_and_existing_children_remain_fixed(tmp_path):
    from rstock.application.repository import RunRepository
    spec = ExperimentSpec(JobType.END_TO_END, replace(DEFAULT_CONFIG, project_root=tmp_path), symbols=("AAA", "BBB"))
    values = spec.to_dict()
    values.pop("e2e_xgboost_protocol_version")
    old = ExperimentSpec.from_dict(values)
    assert old.e2e_xgboost_protocol_version == 1
    assert old.config.xgb_round_selection_mode == "fixed"
    assert "round_selection_policy" not in _stage_parameter_contract(old, "walk_forward")
    repository = RunRepository(tmp_path / "runs")
    parent = replace(spec, config=replace(spec.config, xgb_round_selection_mode="chronological"))
    root = repository.create(parent)
    manifest = build_pipeline_manifest(repository, root, parent)
    child = build_stage_spec(repository, root, parent, "walk_forward", manifest)
    assert not child.evaluate_final_holdout
    assert child.config.xgb_round_selection_mode == "chronological"


def test_existing_derivation_graph_owns_policy_at_wf_and_keeps_all_downstream_stages():
    for schema in (2, 3):
        _, fields = derivation_graph(schema)
        assert set(modeling.ROUND_SELECTION_FIELDS) <= fields["walk_forward"]
        modes = stage_modes("walk_forward", schema_version=schema)
        assert all(modes[key] == "recomputed" for key in (
            "walk_forward", "xgboost_calibration", "threshold_parameter_calibration",
            "threshold_calibration", "holdout_evaluation", "promotion_qualification"))
        if schema == 3:
            assert modes["prefilter"] == "inherited"


def test_production_policy_is_frozen_and_missing_chronological_contract_is_rejected(tmp_path):
    from test_production_application import _model
    model = _model()
    current = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological", xgb_rounds=99)
    assert modeling.production_training_config(current, model).xgb_round_selection_mode == "fixed"
    frozen = replace(current, xgb_rounds=80, xgb_early_stopping_patience=17)
    model.round_selection_policy = modeling.round_selection_snapshot(frozen)
    effective = modeling.production_training_config(replace(current, xgb_early_stopping_patience=2), model)
    assert effective.xgb_rounds == 80 and effective.xgb_early_stopping_patience == 17
    with pytest.raises(ValueError, match="artifact"):
        modeling.validate_production_round_contract(model, {})
    model.round_selection_policy = None
    model.source_configuration = {"rstock_config": {"xgb_round_selection_mode": "chronological"}}
    with pytest.raises(ValueError, match="missing its frozen policy"):
        modeling.production_training_config(current, model)


def test_chronological_forward_cannot_resume_a_fixed_snapshot(tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological")
    spec = ExperimentSpec(JobType.END_TO_END, config, symbols=("AAA", "BBB"))
    with pytest.raises(ValueError, match="policy_missing"):
        _validate_round_selection_contract({"schema_version": 1, "models": []}, spec)


def test_e2e_reuses_shared_frozen_wf_input_contract(tmp_path):
    from test_walk_forward_derivation import _parent
    from rstock.application.walk_forward_experiments import freeze_walk_forward_input, load_frozen_walk_forward_candidates
    import hashlib
    repository, source_id, expected, _ = _parent(tmp_path)
    source = repository.load_spec(source_id)
    snapshot = repository.run_directory(source_id) / "checkpoints/artifacts/prepared_snapshot.pkl"
    child = replace(source, config=replace(source.config, xgb_round_selection_mode="chronological"),
                    evaluate_final_holdout=False, source_end_to_end_run="new-e2e")
    frozen = freeze_walk_forward_input(repository, child, source_id, hashlib.sha256(snapshot.read_bytes()).hexdigest())
    plan = load_frozen_walk_forward_candidates(repository, frozen)
    pd.testing.assert_frame_equal(plan.slice(0, plan.count()), expected)
    assert frozen.source_end_to_end_run == "new-e2e"


def test_e2e_stage_specs_propagate_policy_and_reserve_holdout(tmp_path, monkeypatch):
    from rstock.application import end_to_end
    from rstock.application.repository import RunRepository
    from rstock.threshold_parameter_calibration import ThresholdCalibrationParameters
    repository = RunRepository(tmp_path / "runs")
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_round_selection_mode="chronological")
    parent = ExperimentSpec(JobType.END_TO_END, config, symbols=("AAA", "BBB"), historical_data_cutoff="2026-09-25")
    root = repository.create(parent)
    manifest = build_pipeline_manifest(repository, root, parent)
    monkeypatch.setattr(end_to_end, "_walk_forward_traceability", lambda *a: ("2026-09-25", "d" * 64))
    selected = {direction: {"parameters": modeling.historical_xgboost_parameters(config).as_dict(),
                           "round_selection_policy": modeling.round_selection_snapshot(config)} for direction in ("Up", "Down")}
    def artifact(repo, run, filename):
        if filename == "selected_configurations.json":
            return selected
        if filename == "selected_threshold_calibration_configuration.json":
            return {"parameters": ThresholdCalibrationParameters.from_config(config).as_dict()}
        return {}
    monkeypatch.setattr(end_to_end, "_read_result_json", artifact)
    for stage in ("walk_forward", "xgboost_calibration", "threshold_parameter_calibration",
                  "threshold_calibration", "holdout_evaluation", "promotion_qualification"):
        child = build_stage_spec(repository, root, parent, stage, manifest)
        assert child.config.xgb_round_selection_mode == "chronological"
        assert child.e2e_xgboost_protocol_version == 2
        assert child.source_end_to_end_run == root
        if stage in {"walk_forward", "threshold_calibration"}:
            assert child.evaluate_final_holdout is False
        if stage == "holdout_evaluation":
            assert child.evaluate_final_holdout is True


def test_e2e_form_exposes_policy_without_an_additional_button(tmp_path, monkeypatch):
    from test_derived_workflow import _source
    from contextlib import nullcontext
    from rstock.application import streamlit_app
    repository, source_id, _, _ = _source(tmp_path, split=True)
    source = repository.load_spec(source_id)
    controls, buttons, headings = [], [], []
    class UI:
        session_state = {}
        def button(self, label, **kwargs):
            buttons.append(label)
            return label == "Créer une expérience dérivée"
        def selectbox(self, label, choices, **kwargs):
            controls.append(label)
            return choices[kwargs.get("index", 0)]
        def number_input(self, label, **kwargs):
            controls.append(label)
            return kwargs["value"]
        def checkbox(self, label, **kwargs):
            return kwargs["value"]
        def text_input(self, label, **kwargs):
            return kwargs["value"]
        def container(self, **kwargs):
            return nullcontext()
        def subheader(self, text):
            headings.append(text)
        def caption(self, *args):
            pass
        def info(self, *args):
            pass
        def write(self, *args):
            pass
    monkeypatch.setattr(streamlit_app, "st", UI())
    detail = {"configuration": source.to_dict(), "status": {"status": "completed"}}
    service = SimpleNamespace(run_service=SimpleNamespace(repository=repository))
    streamlit_app._render_derived_creation(source_id, detail, service)
    assert "Sélection des tours XGBoost (Walk-forward et aval)" in controls
    assert set(modeling.ROUND_SELECTION_FIELDS[1:5]) <= set(controls)
    assert buttons == ["Créer une expérience dérivée", "Lancer l’expérience dérivée"]


def test_production_training_persists_actual_rounds_and_frozen_policy(fake_xgb, tmp_path):
    from test_production_application import _model
    from rstock.application.production_services import ProductionTrainingService
    from rstock.application.production_repository import ProductionRepository
    prepared, _, config = _data(tmp_path)
    model = _model()
    model.round_selection_policy = modeling.round_selection_snapshot(config)
    repository = ProductionRepository(tmp_path)
    repository.add(model)
    trained = ProductionTrainingService(repository).train(model.model_id, prepared,
        replace(config, xgb_round_selection_mode="fixed", xgb_rounds=1))
    metadata = trained.training_metadata
    assert metadata["round_selection_policy"] == model.round_selection_policy
    assert metadata["round_selection_records"]["up"]["RoundsRetained"] == 3
    assert metadata["round_selection_records"]["down"]["RoundsRetained"] == 3
    modeling.validate_production_round_contract(trained, metadata)
    assert len(fake_xgb) == 4
    for direction in ("up", "down"):
        stored = json.loads((repository.artifact_directory(model.model_id) / f"{direction}.ubj").read_text())
        assert json.loads(stored["rstock_round_selection"])["TrainingIdentity"] == metadata["round_selection_records"][direction]["TrainingIdentity"]


@pytest.mark.parametrize("mode,training_calls", [("DAILY_RETRAIN", 12), ("FROZEN_AT_START", 4)])
def test_production_replay_selects_at_available_origins_only(fake_xgb, tmp_path, mode, training_calls):
    from test_production_application import _model
    from rstock.application.production_services import DailyPredictionService, HistoricalReplayMode
    from rstock.application.production_repository import ProductionRepository
    prepared, _, config = _data(tmp_path)
    model = _model()
    model.artifact_version = 1
    model.round_selection_policy = modeling.round_selection_snapshot(config)
    repository = ProductionRepository(tmp_path)
    repository.add(model)
    directory = repository.artifact_directory(model.model_id)
    directory.mkdir(parents=True)
    (directory / "production.metadata.json").write_text(json.dumps({
        "model_id": model.model_id, "artifact_version": 1, "feature_version": model.feature_version,
        "predictor_columns": ["BBB_intraday_J-1"]}), encoding="utf-8")
    dates = prepared.index
    close = 100 * (1 + np.where(np.arange(len(dates)) % 2, .02, -.02))
    prices = pd.DataFrame({"Open": 100., "Close": close,
        "High": np.maximum(close, 100) + 1, "Low": np.minimum(close, 100) - 1}, index=dates)
    result = DailyPredictionService(repository).replay(lambda symbol: prices,
        replace(config, xgb_round_selection_mode="fixed"), start_date=dates[-3], end_date=dates[-1],
        models=[model], mode=HistoricalReplayMode[mode])
    assert result.status.eq("predicted").all(), result.get("error")
    assert len(result) == 3 and len(fake_xgb) == training_calls
    identities = set()
    for _, row in result.iterrows():
        records = json.loads(row.round_selection_records)
        for record in records.values():
            assert pd.Timestamp(record["TrainingEnd"]) < pd.Timestamp(row.prediction_date)
            assert pd.Timestamp(record["ValidationEnd"]) <= pd.Timestamp(row.as_of_date)
            identities.add(record["TrainingIdentity"])
    assert len(identities) == (6 if mode == "DAILY_RETRAIN" else 2)
