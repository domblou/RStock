from dataclasses import replace
import json
import numpy as np
import pandas as pd
import pytest
import exchange_calendars as xcals

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.application.worker import execute_run
from rstock.application import workflows
from rstock.application.prefilter_experiments import build_derived_prefilter_spec
from rstock.application.prefilter_comparison import load_prefilter_comparison, prefilter_evaluation_comparability
from rstock.walk_forward import evaluate_prefilter_walk_forward
from rstock.checkpoints import CheckpointManager
from rstock.progress import CancellationRequested
from rstock.checkpoints import CheckpointIncompatibleError
from test_logistic_model import fixture


def data(tmp_path):
    frame, sets, cfg = fixture(tmp_path)
    frame.index = xcals.get_calendar('XNYS').sessions_in_range('2025-01-02','2025-03-01')[:len(frame)]
    if frame.index.tz is not None: frame.index=frame.index.tz_localize(None)
    frame['BBB.intraday_return'] = .01
    cfg = replace(cfg, prefilter_algorithm='logistic_l2', prefilter_logistic_l2_penalty=.13,
        predictor_prefilter_enabled=True, predictor_prefilter_top_n=1,
        predictor_prefilter_min_median_auc=0.,predictor_prefilter_min_worst_auc=0.,
        predictor_prefilter_min_pct_above_random=0.,predictor_prefilter_max_auc_std=1.,
        prefilter_xgb_num_boost_round=2)
    return frame,sets,cfg


def test_prefilter_logistic_constant_and_holdout_invariance(tmp_path,monkeypatch):
    import rstock.modeling as modeling
    monkeypatch.setattr(modeling,'xgboost_module',lambda:pytest.fail('Loaded XGBoost'))
    frame,sets,cfg=data(tmp_path)
    checkpoint=CheckpointManager(tmp_path/'checkpoint',run_id='fixture',job_type='predictor_prefilter',
        configuration_fingerprint='fixture',batch_sizes={'predictor_prefilter_walk_forward':1})
    result=evaluate_prefilter_walk_forward(frame,sets,cfg,checkpoint_manager=checkpoint)
    audit=checkpoint.load_artifact('prefilter_round_selection_training')
    assert audit.PredictiveAlgorithm.eq('logistic_l2').all()
    assert audit.Converged.all() and all(c['penalty']==.13 for c in audit.LogisticContract)
    assert (pd.to_datetime(audit.TestEnd)<frame.index[-cfg.final_holdout_size]).all()
    assert audit.ConstantProbability.eq(.5).all()
    assert np.allclose(audit.ConstantLogLoss,np.log(2)) and np.allclose(audit.ConstantBrier,.25)
    changed=frame.copy()
    changed.iloc[-cfg.final_holdout_size:,:]=999
    result2=evaluate_prefilter_walk_forward(changed,sets,cfg)
    for field in ('ROCAUCMedian','ROCAUCWorst','ROCAUCStd','PctWindowsAboveRandom','Eligible'):
        pd.testing.assert_series_equal(result.qualification[field],result2.qualification[field])
    monkeypatch.setattr(modeling,'fit_booster',lambda *a,**k:pytest.fail('Recomputed completed batch'))
    import rstock.walk_forward as wf
    monkeypatch.setattr(wf,'fit_booster',lambda *a,**k:pytest.fail('Recomputed completed batch'))
    resumed=evaluate_prefilter_walk_forward(frame,sets,cfg,checkpoint_manager=checkpoint)
    pd.testing.assert_frame_equal(result.qualification,resumed.qualification)
    with pytest.raises(CheckpointIncompatibleError,match='frozen input'):
        evaluate_prefilter_walk_forward(changed,sets,cfg,checkpoint_manager=checkpoint)


@pytest.mark.parametrize('method',['single_origin','temporal_stability','temporal_consensus'])
def test_frozen_prefilter_derivation_comparison_and_history(tmp_path,monkeypatch,method):
    frame,sets,cfg=data(tmp_path)
    cfg=replace(cfg,prefilter_algorithm='xgboost',predictive_algorithm='logistic_l2',
        temporal_consensus_origins=2,temporal_consensus_step_sessions=1,temporal_consensus_min_occurrences=1)
    spec=ExperimentSpec(JobType.PREDICTOR_PREFILTER,cfg,symbols=('AAA','BBB'),
        target_symbols=('AAA',),context_symbols=('BBB',),predictor_symbols=('AAA','BBB'),
        historical_data_cutoff=str(frame.index[-1].date()))
    spec=replace(spec,prefilter_method=method,stability_origin_count=2,stability_step_sessions=1)
    repo=RunRepository(tmp_path/'runs');parent=repo.create(spec)
    original=workflows._prepared_inputs
    def inputs(spec,*a,**k):
        if spec.prepared_snapshot_required:return original(spec,*a,**k)
        return frame.loc[:spec.historical_data_cutoff].copy(),['AAA','BBB'],['AAA'],{}
    monkeypatch.setattr(workflows,'_prepared_inputs',inputs)
    execute_run(repo,parent,1)
    assert repo.status(parent)['status']=='completed',repo.status(parent).get('error')
    child=build_derived_prefilter_spec(repo,parent,{'prefilter_algorithm':'logistic_l2'})
    assert child.config.predictive_algorithm==repo.load_spec(parent).config.predictive_algorithm
    derived=repo.create(child);execute_run(repo,derived,1)
    assert repo.status(derived)['status']=='completed',repo.status(derived).get('error')
    a,b=[load_prefilter_comparison(tmp_path,r) for r in (parent,derived)]
    assert prefilter_evaluation_comparability([a,b])['status']=='verified'
    assert a.evaluation['metrics']['ConstantLogLoss']==b.evaluation['metrics']['ConstantLogLoss']
    assert a.settings['prefilter_algorithm']=='xgboost' and b.settings['prefilter_algorithm']=='logistic_l2'
    assert prefilter_evaluation_comparability([replace(a,evaluation={}),b])['status']=='unverifiable'
    assert prefilter_evaluation_comparability([a,replace(b,evaluation={**b.evaluation,'evaluation_sha256':'changed'})])['status']=='incompatible'
    raw=spec.to_dict()
    for key in list(raw['rstock_config']):
        if key=='prefilter_algorithm' or key.startswith('prefilter_logistic_'):raw['rstock_config'].pop(key)
    assert ExperimentSpec.from_dict(raw).config.prefilter_algorithm=='xgboost'
    assert repo.read_json(derived,'results/prefilter_contract.json')['schema_version']==3
    from rstock.application.prefilter_contract import inherit_reference
    wf=replace(child,job_type=JobType.WALK_FORWARD,prefilter_derivation=None,
        prefilter_method='single_origin',stability_origin_count=5,stability_step_sessions=1,
        source_experiment_run=None,source_prefilter_run=derived,
        config=replace(child.config,predictive_algorithm='logistic_l2'))
    wf=inherit_reference(repo,wf)
    loaded,_,_,_=original(wf,None,None)
    pd.testing.assert_frame_equal(loaded,frame)


def test_prefilter_monoclass_is_explicit_without_fallback(tmp_path):
    frame,sets,cfg=data(tmp_path)
    frame['AAA.intraday_target']=1
    with pytest.raises(ValueError,match='both binary classes'):
        evaluate_prefilter_walk_forward(frame,sets,cfg)


def test_prefilter_interruption_after_commit_reuses_batch(tmp_path,monkeypatch):
    import rstock.walk_forward as wf
    frame,sets,cfg=data(tmp_path)
    manager=CheckpointManager(tmp_path/'checkpoint',run_id='run',job_type='predictor_prefilter',
        configuration_fingerprint='fixture',batch_sizes={'predictor_prefilter_walk_forward':1})
    def interrupt(event):
        if event.details.get('checkpoint_written'):
            raise CancellationRequested('after commit')
    with pytest.raises(CancellationRequested):
        evaluate_prefilter_walk_forward(frame,sets,cfg,checkpoint_manager=manager,progress_callback=interrupt)
    monkeypatch.setattr(wf,'fit_booster',lambda *a,**k:pytest.fail('Refitted committed batch'))
    result=evaluate_prefilter_walk_forward(frame,sets,cfg,checkpoint_manager=manager)
    assert not result.qualification.empty


def test_prefilter_selector_is_independent_and_has_no_constant_choice():
    from streamlit.testing.v1 import AppTest
    app=AppTest.from_string('''
import streamlit as st
from rstock.config import DEFAULT_CONFIG
from rstock.application.predictive_model_ui import prefilter_configuration_input
config=prefilter_configuration_input(DEFAULT_CONFIG,'prefilter',st)
st.json({'prefilter':config.prefilter_algorithm,'final':config.predictive_algorithm})
''').run()
    assert not app.exception
    assert app.selectbox[0].options==['XGBoost','Régression logistique L2']
    app.selectbox[0].select('logistic_l2').run()
    assert not app.exception and len(app.number_input)==4
    assert json.loads(app.json[0].value)=={'prefilter':'logistic_l2','final':'xgboost'}
