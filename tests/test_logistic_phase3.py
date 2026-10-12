from dataclasses import replace
import json

import pytest
from streamlit.testing.v1 import AppTest

from rstock.config import DEFAULT_CONFIG
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.predictive_model_ui import predictive_configuration_input
from test_logistic_model import fixture


def test_selectors_switch_algorithms_and_constant_disables_variables():
    app=AppTest.from_string('''
import streamlit as st
from rstock.config import DEFAULT_CONFIG
from rstock.application.predictive_model_ui import predictive_configuration_input
cfg = predictive_configuration_input(DEFAULT_CONFIG, "model", st)
st.json({"algorithm":cfg.predictive_algorithm,"variables":cfg.predictive_model_type})
''').run()
    assert not app.exception
    assert [x.label for x in app.selectbox]==['Algorithme prédictif','Type de variables']
    assert app.selectbox[0].options==['XGBoost','Régression logistique L2','Probabilité constante']
    app.selectbox[0].select('constant_probability').run()
    assert app.selectbox[1].disabled
    assert json.loads(app.json[0].value)['variables']=='constant_probability'
    app.selectbox[0].select('logistic_l2').run()
    assert not app.exception and not app.selectbox[1].disabled
    assert len(app.number_input)==4
    app.selectbox[1].select('target_only').run()
    assert json.loads(app.json[0].value)=={'algorithm':'logistic_l2','variables':'target_only'}


def test_logistic_e2e_without_xgboost_including_calibrations_holdout(tmp_path,monkeypatch):
    import rstock.modeling as modeling
    import rstock.walk_forward as wf
    import rstock.streaming_walk_forward as streaming
    from rstock.application import workflows
    from rstock.application.repository import RunRepository
    from rstock.application.worker import execute_run
    from rstock.application.end_to_end import load_pipeline_manifest
    frame,_,cfg=fixture(tmp_path)
    cfg=replace(cfg,predictive_model_type='target_only',threshold_calibration_min_signals_per_window=1,
                threshold_calibration_min_robust_signals=1,threshold_parameter_calibration_max_models=2)
    spec=ExperimentSpec(job_type=JobType.END_TO_END,config=cfg,symbols=('AAA',),historical_data_cutoff=str(frame.index[-1].date()))
    repo=RunRepository(tmp_path/'runs');root=repo.create(spec)
    monkeypatch.setattr(workflows,'_prepared_inputs',lambda *a,**k:(frame,['AAA'],['AAA'],{}))
    for module in (modeling,wf,streaming):
        monkeypatch.setattr(module,'xgboost_module',lambda:pytest.fail('Logistic E2E loaded XGBoost'))
    execute_run(repo,root,1)
    assert repo.status(root)['status']=='completed',repo.status(root).get('error')
    manifest=load_pipeline_manifest(repo,root)
    stages={stage['stage_key']:stage for stage in manifest['stages']}
    assert stages['xgboost_calibration']['execution_status']=='not_applicable'
    assert stages['xgboost_calibration']['not_executed_reason']=='logistic_l2'
    assert repo.status(stages['threshold_calibration']['child_run_id'])['status']=='completed'
    assert repo.status(stages['holdout_evaluation']['child_run_id'])['status']=='completed'


def test_derivation_accepts_algorithm_fields_and_preserves_history(tmp_path):
    from rstock.application.walk_forward_experiments import WF_DERIVATION_CONFIG_FIELDS
    from rstock.application.derivation import PREFILTER_STAGE_PARAMETER_FIELDS
    for field in ('predictive_algorithm','logistic_l2_penalty','logistic_max_iterations'):
        assert field in WF_DERIVATION_CONFIG_FIELDS
        assert field in PREFILTER_STAGE_PARAMETER_FIELDS['walk_forward']
        assert field not in PREFILTER_STAGE_PARAMETER_FIELDS['prefilter']
    legacy=ExperimentSpec(job_type=JobType.END_TO_END,config=replace(DEFAULT_CONFIG,project_root=tmp_path),symbols=('AAA','BBB')).to_dict()
    for key in list(legacy['rstock_config']):
        if key=='predictive_algorithm' or key.startswith('logistic_'):legacy['rstock_config'].pop(key)
    assert ExperimentSpec.from_dict(legacy).config.predictive_algorithm=='xgboost'


def test_logistic_comparison_reconstructs_constant_with_common_training(tmp_path):
    from test_aligned_constant_comparison import fixture_runs
    from rstock.application.common_prediction_comparison import compare_common_predictions
    roots,_=fixture_runs(tmp_path,algorithm='logistic_l2')
    audit,rows=compare_common_predictions(tmp_path,[root.name for root in roots.values()],align_constant=True)
    assert audit['status']=='available'
    assert audit['training_alignment']['status']=='verified'
    assert set(rows.PredictiveAlgorithm)=={'constant_probability','logistic_l2'}


def test_switch_algorithm_in_frozen_wf_derivation(tmp_path):
    from test_walk_forward_derivation import _parent
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
    repo,parent,_,_=_parent(tmp_path)
    child=build_derived_walk_forward_spec(repo,parent,{'predictive_algorithm':'logistic_l2'})
    assert child.config.predictive_algorithm=='logistic_l2'
    assert child.prepared_snapshot_required
    assert child.frozen_xgboost_parameters is None


def test_e2e_algorithm_changes_belong_to_wf_fork(tmp_path):
    from test_derived_workflow import _source
    from rstock.application.derived_experiments import build_derived_spec
    repo,parent,_,_=_source(tmp_path,split=True)
    child=build_derived_spec(repo,parent,'walk_forward',{'predictive_algorithm':'logistic_l2'})
    assert child.config.predictive_algorithm=='logistic_l2'
    assert child.derivation.fork_stage=='walk_forward'
    assert child.frozen_xgboost_parameters is None
    with pytest.raises(ValueError):
        build_derived_spec(repo,parent,'threshold_calibration',{'predictive_algorithm':'logistic_l2'})
