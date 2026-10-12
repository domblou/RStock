"""Temporal L2 protocol on synthetic XNYS data; no acquisitions."""
from dataclasses import replace
import json

import exchange_calendars as xcals
import numpy as np
import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.application.production_repository import ProductionRepository
from rstock.application.worker import execute_run
from rstock.application import workflows, end_to_end
from rstock.progress import CancellationRequested
from test_logistic_model import fixture


@pytest.mark.parametrize('precision_edge,expected', [(.01,'passed'), (.99,'failed')])
def test_logistic_three_pass_temporal_protocol_and_promotion(tmp_path, monkeypatch, precision_edge, expected):
    from rstock.logistic import logistic_contract
    import rstock.modeling as modeling
    base, _, cfg = fixture(tmp_path)
    frame = pd.concat([base]*6, ignore_index=True)
    frame.index = xcals.get_calendar('XNYS').sessions_in_range('2025-01-02','2025-12-31')[:len(frame)]
    if frame.index.tz is not None:
        frame.index = frame.index.tz_localize(None)
    frame['AAA.intraday_down_target'] = 1-frame['AAA.intraday_target']
    frame['AAA.intraday_return'] = np.where(frame['AAA.intraday_target'].eq(1),.02,-.02)
    cfg = replace(cfg, logistic_l2_penalty=.13, final_holdout_size=20, walk_forward_step_size=12,
        threshold_calibration_min_signals_per_window=1, threshold_calibration_min_robust_signals=1,
        threshold_parameter_calibration_max_models=2, promotion_min_holdout_signals=1,
        promotion_min_holdout_auc=.5, promotion_min_holdout_precision=.5,
        promotion_max_opposite_movement_frequency=1., temporal_min_precision_edge=precision_edge,
        temporal_max_ci_width=1.)
    spec = ExperimentSpec(JobType.END_TO_END, cfg, symbols=('AAA','BBB'), target_symbols=('AAA',),
        context_symbols=('BBB',), predictor_symbols=('AAA','BBB'),
        temporal_validation_enabled=True, auto_promote_candidates=True)
    repo = RunRepository(tmp_path/'runs')
    root = repo.create(spec)
    original_prepared = workflows._prepared_inputs
    def inputs(specification, *a, **k):
        if specification.prepared_snapshot_required:
            return original_prepared(specification, *a, **k)
        offset = specification.config.walk_forward_end_offset_sessions
        prepared = frame.iloc[:-offset].copy() if offset else frame.copy()
        prepared.attrs['effective_end_date'] = str(prepared.index[-1].date())
        return prepared, ['AAA','BBB'], ['AAA'], {}
    monkeypatch.setattr(workflows,'_prepared_inputs',inputs)
    monkeypatch.setattr(modeling,'xgboost_module',lambda:pytest.fail('Logistic temporal validation loaded XGBoost'))
    original_child = workflows._execute_reserved_child
    interrupted = False
    def child(*a, **k):
        nonlocal interrupted
        result = original_child(*a, **k)
        if repo.load_spec(a[1]).job_type is JobType.FORCED_CANDIDATE_VALIDATION and not interrupted:
            interrupted = True
            raise CancellationRequested('after completed forced child')
        return result
    monkeypatch.setattr(workflows,'_execute_reserved_child',child)
    execute_run(repo,root,1)
    assert repo.status(root)['status'] == 'cancelled', repo.status(root).get('error')
    manifest = end_to_end.load_pipeline_manifest(repo,root)
    stages = {s['stage_key']:s for s in manifest['stages']}
    temporal = stages['temporal_validation_end_to_end']['child_run_id']
    forced = stages[end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE]['child_run_id']
    forced_spec = repo.load_spec(forced)
    assert repo.status(temporal)['status'] == repo.status(forced)['status'] == 'completed'
    assert forced_spec.frozen_xgboost_parameters is None and forced_spec.source_xgboost_calibration_run is None
    assert logistic_contract(forced_spec.config) == logistic_contract(cfg)
    assert forced_spec.forced_symbol_sets and forced_spec.frozen_selected_thresholds_by_set
    assert forced_spec.forced_period_lock['offset_sessions'] == 63
    forced_manifest = repo.read_json(forced, end_to_end.PIPELINE_MANIFEST)
    fixed = next(s['child_run_id'] for s in forced_manifest['stages'] if s['stage_key'] == 'fixed_candidate_evaluation')
    fixed_config = repo.read_json(fixed, 'results/run_configuration.json')
    assert fixed_config['logistic_contract'] == logistic_contract(cfg)
    assert repo.read_json(fixed, 'results/selected_thresholds_by_set.json') == forced_spec.frozen_selected_thresholds_by_set
    fingerprints = {r: repo.configuration_fingerprint(r) for r in (temporal,forced)}
    import rstock.logistic as logistic
    monkeypatch.setattr(logistic,'fit_logistic',lambda *a,**k:pytest.fail('Recomputed completed temporal models'))
    from rstock.application.runner import RunService
    from test_forward_recovery import FakeBackend
    assert RunService(repo, backend=FakeBackend()).resume(root).run_id == root
    execute_run(repo,root,1)
    assert repo.status(root)['status'] == 'completed', repo.status(root).get('error')
    assert fingerprints == {r:repo.configuration_fingerprint(r) for r in fingerprints}
    results = repo.run_directory(root)/'results'
    comparison = json.loads((results/'temporal_validation_comparison.json').read_text())
    assert comparison['final_status'] == expected
    models = ProductionRepository(tmp_path).models()
    assert bool(models) == (expected == 'passed')
    if models:
        assert all(m.predictive_algorithm == 'logistic_l2' and m.source_xgboost_calibration_run is None for m in models)
        assert all(m.training_metadata['validation_provenance']['forced_candidate_validation_run_id'] == forced for m in models)
