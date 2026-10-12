from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobType
from rstock.logistic import LogisticModel, fit_logistic
from rstock.modeling import fit_booster, predict_probabilities
from rstock.persistence import ModelMetadata, save_model_bundle, iter_model_metadata, load_booster
from rstock.progress import CancellationRequested
from rstock.walk_forward import evaluate_walk_forward
import rstock.walk_forward as wf
import rstock.streaming_walk_forward as streaming


def fixture(tmp_path):
    index = pd.bdate_range('2025-01-01', periods=24)
    frame = pd.DataFrame({'AAA.intraday_target': np.arange(24)%2,
        'AAA.intraday_down_target': (np.arange(24)%3==0).astype(int),
        'AAA_intraday_J-1': np.arange(24)/100,
        'BBB_intraday_J-1': (np.arange(24)%2)/100}, index=index)
    for name in ('intraday_return','overnight_return','close_to_close_return','mfe','mae'):
        frame['AAA.'+name]=0.
    cfg=replace(DEFAULT_CONFIG,project_root=tmp_path,predictive_algorithm='logistic_l2',
        lag_depth=1,permutation_depth=1,combination_workers=1,xgb_round_selection_mode='chronological',
        walk_forward_min_train_size=8,walk_forward_test_size=4,walk_forward_step_size=4,
        final_holdout_size=4,walk_forward_batch_size=1,final_holdout_batch_size=1,
        qualification_min_windows=2,qualification_min_positive_observations=1,
        qualification_min_median_auc=0.,qualification_min_worst_window_auc=0.,
        qualification_min_pct_windows_above_random=0.,qualification_max_auc_std=1.)
    return frame,pd.DataFrame({'V0':['AAA'],'V1':['BBB']}),cfg


def test_logistic_matches_historical_objective_and_artifact(tmp_path):
    frame,_,cfg=fixture(tmp_path);names=['AAA_intraday_J-1','BBB_intraday_J-1'];label='AAA.intraday_target'
    model=fit_booster(frame.iloc[:16],names,label,cfg)
    # Independent fit with the exact previously adopted objective.
    from scipy.optimize import minimize
    from scipy.special import expit
    x=frame.iloc[:16][names].to_numpy();y=frame.iloc[:16][label].to_numpy()
    mean=x.mean(0);scale=x.std(0);scale[scale==0]=1;z=(x-mean)/scale
    def historical(theta):
        score=theta[0]+z@theta[1:];residual=expit(score)-y
        return (np.mean(np.logaddexp(0,score)-y*score)+.05*np.dot(theta[1:],theta[1:]),
                np.r_[residual.mean(),z.T@residual/len(y)+.1*theta[1:]])
    expected=minimize(historical,np.zeros(3),jac=True,method='L-BFGS-B',options={'maxiter':1000,'gtol':1e-8,'ftol':1e-12})
    np.testing.assert_allclose(model.coefficients,expected.x,atol=1e-10)
    np.testing.assert_array_equal(model.mean,mean)
    metadata=ModelMetadata(4,'set','AAA',['BBB'],names,0.,'', 'XNYS','2025-01-01','2025-01-22','2025-01-23','2025-02-01',label,'intraday',.01,1)
    save_model_bundle(model,metadata,tmp_path,'model')
    path,saved=iter_model_metadata(tmp_path)[0]
    assert saved.predictive_algorithm=='logistic_l2' and path.name.endswith('.logistic.json')
    np.testing.assert_array_equal(predict_probabilities(load_booster(path),frame.iloc[16:],names),model.predict(frame.iloc[16:],names))
    with pytest.raises(ValueError,match='order'):model.predict(frame,names[::-1])
    # Test values cannot influence the stored scaler.
    extreme=frame.iloc[16:].copy();extreme[names]=1e6
    model.predict(extreme,names)
    np.testing.assert_array_equal(model.mean,mean)


def test_failure_and_cancellation(tmp_path,monkeypatch):
    frame,_,cfg=fixture(tmp_path);names=['BBB_intraday_J-1'];label='AAA.intraday_target'
    one=frame.copy();one[label]=1
    with pytest.raises(ValueError,match='both binary'):fit_logistic(one,names,label,cfg)
    def cancelled():raise CancellationRequested('stop')
    with pytest.raises(CancellationRequested):fit_logistic(frame,names,label,cfg,cancelled)
    import rstock.logistic as logistic
    from types import SimpleNamespace
    monkeypatch.setattr(logistic,'minimize',lambda *a,**k:SimpleNamespace(success=False,x=np.zeros(2),message='failed'))
    with pytest.raises(ValueError,match='did not converge'):fit_logistic(frame,names,label,cfg)


def test_historical_config_and_job_guards(tmp_path):
    _,_,cfg=fixture(tmp_path)
    spec=ExperimentSpec(job_type=JobType.WALK_FORWARD,config=cfg,symbols=('AAA','BBB'))
    assert ExperimentSpec.from_dict(spec.to_dict()).config==cfg
    raw=spec.to_dict()
    for key in list(raw['rstock_config']):
        if key.startswith('logistic_') or key=='predictive_algorithm':raw['rstock_config'].pop(key)
    raw['rstock_config']['xgb_round_selection_mode']='fixed'
    assert ExperimentSpec.from_dict(raw).config.predictive_algorithm=='xgboost'
    raw['rstock_config']['predictive_model_type']='constant_probability'
    assert ExperimentSpec.from_dict(raw).config.predictive_model_type=='constant_probability'
    constant=replace(DEFAULT_CONFIG,predictive_algorithm='constant_probability')
    assert constant.predictive_model_type=='constant_probability' and not constant.predictor_prefilter_enabled
    for job in (JobType.FORWARD_SIMULATION,JobType.PRODUCTION_TRAINING):
        with pytest.raises(ValueError,match='research WF/E2E'):ExperimentSpec(job_type=job,config=replace(cfg,predictive_model_type='target_only'),symbols=('AAA','BBB'))
    for field,value in [('logistic_l2_penalty',0),('logistic_gradient_tolerance',np.nan),('logistic_max_iterations',True)]:
        with pytest.raises(ValueError,match=field):replace(cfg,**{field:value})


@pytest.mark.parametrize('kind',['external_only','target_only','target_and_external'])
def test_wf_holdout_uses_logistic_without_xgboost(tmp_path,monkeypatch,kind):
    import rstock.modeling as modeling
    frame,generated,cfg=fixture(tmp_path);cfg=replace(cfg,predictive_model_type=kind)
    if kind=='target_only':generated=pd.DataFrame({'V0':['AAA']})
    for module in (wf,modeling,streaming):monkeypatch.setattr(module,'xgboost_module',lambda:pytest.fail('XGBoost used'))
    result=evaluate_walk_forward(frame,generated,cfg)
    assert result.windows.PredictiveAlgorithm.eq('logistic_l2').all()
    assert result.windows.UpConverged.all() and result.windows.DownConverged.all()
    assert result.run_configuration['round_selection']['mode']=='not_applicable'
    assert result.run_configuration['holdout_status']=='executed'
    assert result.final_holdout.FinalAvailable.all()


def test_streaming_interruption_reuses_batch_and_matches_wf(tmp_path,monkeypatch):
    from rstock.checkpoints import CheckpointManager
    frame,generated,cfg=fixture(tmp_path)
    expected=evaluate_walk_forward(frame,generated,cfg)
    root=tmp_path/'run'
    def manager():return CheckpointManager(root,run_id='run',job_type='walk_forward',configuration_fingerprint='logistic-fixture',batch_sizes={'walk_forward':1,'final_holdout':1})
    checkpoint=manager();original=checkpoint.commit_batch
    def interrupted(phase,*args,**kwargs):
        result=original(phase,*args,**kwargs)
        if phase=='walk_forward':raise CancellationRequested('after durable commit')
        return result
    monkeypatch.setattr(checkpoint,'commit_batch',interrupted)
    with pytest.raises(CancellationRequested):streaming.run_streamed_walk_forward(frame,generated,cfg,checkpoint,root/'working')
    restored=manager()
    assert restored.completed_batch_ids('walk_forward')==(0,)
    monkeypatch.setattr(streaming,'_walk_forward_combination',lambda *a,**k:pytest.fail('completed batch recalculated'))
    result=streaming.run_streamed_walk_forward(frame,generated,cfg,restored,root/'working')
    actual=pd.read_csv(root/'working/predictions.csv')
    np.testing.assert_allclose(actual.UpProbability,expected.predictions.UpProbability,atol=1e-14)
    np.testing.assert_allclose(actual.DownProbability,expected.predictions.DownProbability,atol=1e-14)
    assert result.run_configuration['predictive_algorithm']=='logistic_l2'


def test_worker_lots_reconcile_completed_children_after_aggregation_failure(tmp_path,monkeypatch):
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    frame,_,cfg=fixture(tmp_path)
    cfg=replace(cfg,walk_forward_max_combinations_per_batch=1)
    frame['BBB.intraday_target']=1-frame['AAA.intraday_target']
    frame['BBB.intraday_down_target']=frame['AAA.intraday_down_target']
    for name in ('intraday_return','overnight_return','close_to_close_return','mfe','mae'):
        frame['BBB.'+name]=0.
    monkeypatch.setattr(workflows,'_prepared_inputs',lambda *a,**k:(frame,['AAA','BBB'],['AAA','BBB'],{}))
    repository=RunRepository(tmp_path/'runs')
    spec=ExperimentSpec(job_type=JobType.WALK_FORWARD,config=cfg,symbols=('AAA','BBB'))
    parent=repository.create(spec)
    insert=streaming._insert_predictions
    monkeypatch.setattr(streaming,'_insert_predictions',lambda *a,**k:(_ for _ in ()).throw(RuntimeError('aggregation interrupted')))
    execute_run(repository,parent,1)
    assert repository.status(parent)['status']=='failed'
    children=tuple(repository.list_children(parent))
    assert len(children)==2
    assert all(repository.status(c)['status']=='completed' for c in children)
    assert all(repository.load_spec(c).config.predictive_algorithm=='logistic_l2' for c in children)
    monkeypatch.setattr(streaming,'_insert_predictions',insert)
    monkeypatch.setattr(workflows,'_walk_forward_batch',lambda *a,**k:pytest.fail('completed child relaunched'))
    status=repository.prepare_resume(parent);status['resume_requested']=True
    repository.write_json(parent,'status.json',status)
    execute_run(repository,parent,1)
    assert repository.status(parent)['status']=='completed',repository.status(parent).get('error')
    assert tuple(repository.list_children(parent))==children
    config=repository.read_json(parent,'results/run_configuration.json')
    assert config['predictive_algorithm']=='logistic_l2'
