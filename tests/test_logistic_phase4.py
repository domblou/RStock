"""Operational logistic contracts on synthetic data only."""
from dataclasses import replace
import json
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import (
    DailyPredictionService, ProductionLifecycleService, ProductionTrainingService, PromotionService,
)
from rstock.application.repository import RunRepository
from rstock.modeling import production_training_config
from rstock.persistence import load_booster, validate_logistic_artifacts
from rstock.progress import CancellationRequested
from test_logistic_model import fixture
from test_production_application import _model, _promotion_run


def logistic_production(tmp_path):
    frame, _, cfg = fixture(tmp_path)
    cfg = replace(cfg, date_feature_regex=r"^does_not_exist$")
    model = replace(_model(), xgboost_parameters={}, source_xgboost_calibration_run=None,
        source_configuration=ExperimentSpec(JobType.WALK_FORWARD, cfg, symbols=('AAA','BBB')).to_dict())
    repo = ProductionRepository(tmp_path)
    repo.add(model)
    return repo, model, frame, cfg


def test_production_train_observe_predict_and_source_policy(tmp_path, monkeypatch):
    import rstock.modeling as modeling
    monkeypatch.setattr(modeling, 'xgboost_module', lambda: pytest.fail('Loaded XGBoost'))
    repo, model, frame, cfg = logistic_production(tmp_path)
    changed = replace(cfg, predictive_algorithm='xgboost', logistic_l2_penalty=.9, lag_depth=4)
    assert production_training_config(changed, model).logistic_l2_penalty == cfg.logistic_l2_penalty
    events = []
    trained = ProductionTrainingService(repo).train(model.model_id, frame, changed, progress_callback=events.append)
    assert trained.status == ProductionModelStatus.TRAINED
    directory = repo.artifact_directory(model.model_id)
    metadata = json.loads((directory/'production.metadata.json').read_text())
    validate_logistic_artifacts(directory, metadata)
    assert not list(directory.glob('*.ubj'))
    assert metadata['round_selection_policy'] == {'mode':'not_applicable'}
    assert events[-1].completed_units == 2
    lifecycle = ProductionLifecycleService(repo)
    lifecycle.watch(model.model_id)
    lifecycle.activate(model.model_id)
    frame['BBB.intraday_return'] = np.arange(len(frame)) / 100
    result = DailyPredictionService(repo).generate(frame, changed)
    assert len(result) == 1 and result.iloc[0]['status'] == 'predicted'
    row = pd.DataFrame(json.loads(result.iloc[0]['features']), index=[0])
    expected = load_booster(directory/'up.logistic.json').predict(row, ['BBB_intraday_J-1'])[0]
    assert result.iloc[0]['up_probability'] == pytest.approx(expected)
    (directory/'up.logistic.json').write_text('{}')
    with pytest.raises(ValueError, match='artifact has changed'):
        lifecycle._validate_artifacts(repo.get(model.model_id))


def test_production_cancellation_is_atomic_and_published_training_is_reconciled(tmp_path, monkeypatch):
    import rstock.application.production_services as service
    repo, model, frame, cfg = logistic_production(tmp_path)
    original_fit = service.fit_booster
    calls = []
    def cancel_down(*args, **kwargs):
        calls.append(args[2])
        if len(calls) == 2:
            raise CancellationRequested('test')
        return original_fit(*args, **kwargs)
    monkeypatch.setattr(service, 'fit_booster', cancel_down)
    with pytest.raises(CancellationRequested):
        ProductionTrainingService(repo).train(model.model_id, frame, cfg)
    assert not repo.artifact_directory(model.model_id).exists()
    assert repo.get(model.model_id).artifact_version is None
    monkeypatch.setattr(service, 'fit_booster', original_fit)
    mutate = repo.mutate_model
    monkeypatch.setattr(repo, 'mutate_model', lambda *a, **k: (_ for _ in ()).throw(RuntimeError('after publication')))
    with pytest.raises(RuntimeError, match='after publication'):
        ProductionTrainingService(repo).train(model.model_id, frame, cfg)
    monkeypatch.setattr(repo, 'mutate_model', mutate)
    monkeypatch.setattr(service, 'fit_booster', lambda *a, **k: pytest.fail('Recomputed published training'))
    trained = ProductionTrainingService(repo).train(model.model_id, frame, cfg)
    assert trained.status == ProductionModelStatus.TRAINED and trained.artifact_version == 1


def test_historical_registry_keeps_xgboost_and_missing_logistic_policy_is_rejected(tmp_path):
    _, _, _, cfg = logistic_production(tmp_path)
    legacy = ProductionModel.from_dict(_model('legacy').to_dict())
    assert production_training_config(cfg, legacy).predictive_algorithm == 'xgboost'
    raw = legacy.to_dict()
    raw['source_configuration'] = {'rstock_config': {'predictive_algorithm': 'logistic_l2'}}
    with pytest.raises(ValueError, match='Incomplete frozen logistic'):
        production_training_config(cfg, ProductionModel.from_dict(raw))


def test_logistic_promotion_is_idempotent_and_rejects_other_algorithm(tmp_path):
    runs, run = _promotion_run(tmp_path)
    _, _, cfg = fixture(tmp_path)
    cfg = replace(cfg, xgb_round_selection_mode='fixed')
    source = replace(runs.load_spec(run), config=cfg)
    runs.write_json(run, 'config.json', source.to_dict())
    service = PromotionService(runs, ProductionRepository(tmp_path))
    promoted, created = service.promote(run, 'AAA<-BBB')
    repeated, again = service.promote(run, 'AAA<-BBB')
    assert created and not again and promoted.model_id == repeated.model_id
    assert promoted.predictive_algorithm == 'logistic_l2' and promoted.xgboost_parameters == {}
    assert promoted.source_xgboost_calibration_run is None
    with pytest.raises(ValueError, match='predictive_algorithm'):
        service._assert_methodology_compatible(source, replace(source, config=replace(cfg, predictive_algorithm='xgboost')), 'threshold')


def forward_fixture(tmp_path, monkeypatch):
    import rstock.application.forward_simulation as forward
    _, _, cfg = fixture(tmp_path)
    import exchange_calendars as xcals
    index = xcals.get_calendar('XNYS').sessions_in_range('2026-05-01', '2026-06-30')[:36]
    if index.tz is not None:
        index = index.tz_localize(None)
    prices = pd.DataFrame(index=index)
    for symbol, offset in [('AAA',0),('BBB',1),('CCC',2)]:
        returns = np.where((np.arange(len(index))+offset)%3 == 0, -.03, .03)
        for name, values in [('Open', np.full(len(index),100.)), ('Close',100*(1+returns)),
                             ('High',np.full(len(index),104.)), ('Low',np.full(len(index),96.))]:
            prices[f'{symbol}.{name}'] = values
    cfg = replace(cfg, date_feature_regex=r'^does_not_exist$')
    cutoff = str(index[-6].date())
    source = ExperimentSpec(JobType.END_TO_END, cfg, symbols=('AAA','BBB','CCC'),
        historical_data_cutoff=cutoff, resolved_market_session_cutoff=cutoff)
    repo = RunRepository(tmp_path/'runs')
    root = repo.create(source)
    stages = {}
    for stage, job in [('xgboost_calibration', JobType.XGBOOST_CALIBRATION),
                       ('threshold_calibration', JobType.THRESHOLD_CALIBRATION),
                       ('promotion_qualification', JobType.PROMOTION_QUALIFICATION)]:
        stages[stage] = repo.create(replace(source, job_type=job))
        (repo.run_directory(stages[stage])/'results').mkdir()
    sets = ['["AAA","BBB"]','["AAA","CCC"]']
    repo.write_json(stages['threshold_calibration'], 'results/selected_thresholds_by_set.json',
        {s:{'Up':{'threshold':.5},'Down':{'threshold':.5}} for s in sets})
    repo.write_json(stages['promotion_qualification'], 'results/qualification.json',
        {'decisions':[{'Combinaison':s,'Direction':'Up','Statut promotion':'Candidat'} for s in sets]})
    monkeypatch.setattr(forward, '_stage_ids', lambda *a: stages)
    class Market:
        def load(self, *a, **k):
            return SimpleNamespace(prices=prices, symbols=['AAA','BBB','CCC']), {}
    monkeypatch.setattr(forward, 'MarketDataService', Market)
    (repo.run_directory(root)/'results').mkdir()
    return forward, repo, root, source, index


def test_forward_snapshot_resume_reuses_finished_model_and_checks_integrity(tmp_path, monkeypatch):
    forward, repo, root, source, _ = forward_fixture(tmp_path, monkeypatch)
    def stop(event):
        if event.substage == 'trained':
            raise CancellationRequested('after model commit')
    with pytest.raises(CancellationRequested):
        forward.build_forward_model_snapshot(repo, root, source, progress_callback=stop)
    fit = forward.fit_booster
    calls = []
    def count(*a, **k):
        calls.append(a[2])
        return fit(*a, **k)
    monkeypatch.setattr(forward, 'fit_booster', count)
    events = []
    snapshot = forward.build_forward_model_snapshot(repo, root, source, progress_callback=events.append)
    assert len(calls) == 2 and events[0].substage == 'reused'
    assert snapshot['candidate_count'] == 2
    monkeypatch.setattr(forward, 'fit_booster', lambda *a, **k: pytest.fail('Refitted frozen model'))
    assert forward.build_forward_model_snapshot(repo, root, source)['snapshot_sha256'] == snapshot['snapshot_sha256']
    directory = repo.run_directory(root)/'results'/forward.SNAPSHOT_DIRECTORY/snapshot['models'][0]['source_model_id']
    (directory/'down.logistic.json').write_text('{}')
    with pytest.raises(ValueError, match='artifact has changed'):
        forward.build_forward_model_snapshot(repo, root, source)


def test_forward_predictions_resume_without_retraining_or_duplicate_rows(tmp_path, monkeypatch):
    forward, repo, root, source, index = forward_fixture(tmp_path, monkeypatch)
    snapshot = forward.build_forward_model_snapshot(repo, root, source)
    spec = replace(source, job_type=JobType.FORWARD_SIMULATION, source_end_to_end_run=root,
        source_forward_model_snapshot_sha256=snapshot['snapshot_sha256'],
        forward_simulation_start_date=str(index[-5].date()), forward_simulation_end_date=str(index[-1].date()))
    run = repo.create(spec)
    output = repo.run_directory(run)/'_working'
    output.mkdir()
    monkeypatch.setattr(forward, 'fit_booster', lambda *a, **k: pytest.fail('Forward retrained'))
    def stop(event):
        if event.details.get('checkpoint_written'):
            raise CancellationRequested('after inference commit')
    with pytest.raises(CancellationRequested):
        forward.run_forward_simulation(spec, output, progress_callback=stop)
    events = []
    resumed = forward.run_forward_simulation(spec, output, progress_callback=events.append)
    rows = pd.read_csv(output/'forward_observations.csv')
    assert len(rows) > 0 and not rows.duplicated(['source_model_id','session_date']).any()
    assert all(event.completed_units == 2 for event in events if event.details.get('checkpoint_written'))
    before = (output/'forward_observations.csv').read_bytes()
    assert forward.run_forward_simulation(spec, output)['evaluated_observations'] == resumed['evaluated_observations']
    assert (output/'forward_observations.csv').read_bytes() == before


def test_logistic_external_e2e_publishes_frozen_snapshot(tmp_path, monkeypatch):
    import rstock.modeling as modeling
    from rstock.application import workflows
    import rstock.application.forward_simulation as forward
    from rstock.application.worker import execute_run
    from rstock.application.end_to_end import load_pipeline_manifest
    frame, _, cfg = fixture(tmp_path)
    frame['AAA.intraday_return'] = np.where(frame['AAA.intraday_target'].eq(1), .02, -.02)
    frame['AAA.intraday_down_target'] = 1-frame['AAA.intraday_target']
    cfg = replace(cfg, threshold_calibration_min_signals_per_window=1,
        threshold_calibration_min_robust_signals=1, threshold_parameter_calibration_max_models=2,
        promotion_min_holdout_signals=1, promotion_min_holdout_auc=.5,
        promotion_min_holdout_precision=.5, promotion_max_opposite_movement_frequency=1.)
    spec = ExperimentSpec(JobType.END_TO_END, cfg, symbols=('AAA','BBB'), target_symbols=('AAA',), context_symbols=('BBB',), predictor_symbols=('AAA','BBB'),
        historical_data_cutoff=str(frame.index[-1].date()), resolved_market_session_cutoff=str(frame.index[-1].date()),
        auto_promote_candidates=True)
    repo = RunRepository(tmp_path/'runs')
    root = repo.create(spec)
    monkeypatch.setattr(workflows, '_prepared_inputs', lambda *a, **k:(frame,['AAA','BBB'],['AAA'],{}))
    monkeypatch.setattr(modeling, 'xgboost_module', lambda: pytest.fail('E2E loaded XGBoost'))
    class Market:
        def load(self, *a, **k):
            return SimpleNamespace(prices=frame, symbols=['AAA','BBB']), {}
    monkeypatch.setattr(forward, 'MarketDataService', Market)
    monkeypatch.setattr(forward, 'prepare_dataset', lambda *a, **k:frame.copy())
    execute_run(repo,root,1)
    assert repo.status(root)['status'] == 'completed', repo.status(root).get('error')
    snapshot = json.loads((repo.run_directory(root)/'results'/forward.SNAPSHOT_FILENAME).read_text())
    assert snapshot['predictive_algorithm'] == 'logistic_l2'
    assert snapshot['candidate_count'] > 0
    models = ProductionRepository(tmp_path).models()
    assert models and all(model.predictive_algorithm == 'logistic_l2' and model.source_xgboost_calibration_run is None for model in models)
    assert load_pipeline_manifest(repo,root)['auto_promote_candidates'] is True
