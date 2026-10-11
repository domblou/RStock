"""SPY acquisition/extension tests use synthetic data only; no real workflows."""
import json

import numpy as np
import pandas as pd
import pytest

from rstock.application import market_context as mc
from rstock.application import market_context_runtime as runtime
from rstock.application.market_context_acquisition import acquire_spy, ContextAcquisitionError
from rstock.application.forward_diagnostic import _json, digest
from rstock.application.domain import JobType
from rstock.application.workflows import WorkflowRegistry
from test_market_context import setup_run, prices


def test_float32_rounding_is_audited_and_material_block_is_rejected():
    snap = mc.adjusted_snapshot(prices()).astype(np.float32).astype(float)
    frozen = snap.iloc[:600]
    incoming = snap.iloc[570:700].copy()
    # One representable price step is not a scientific return revision.
    incoming.iloc[8, 0] = float(np.nextafter(np.float32(incoming.iloc[8, 0]), np.float32(np.inf)))
    report = mc.extension_comparison(frozen, incoming)
    assert report.attrs['classification'] == 'precision_compatible'
    assert report.consistent.all()
    pd.testing.assert_frame_equal(mc.extend_adjusted_snapshot(frozen, incoming).iloc[:600], frozen)
    incoming.iloc[5:15, 0] *= .997523
    report = mc.extension_comparison(frozen, incoming)
    assert report.attrs['classification'] == 'material_revision'
    assert not report.consistent.all()
    with pytest.raises(ValueError, match='historical_returns_changed'):
        mc.extend_adjusted_snapshot(frozen, incoming)


def test_missing_overlap_session_is_not_silently_skipped():
    snap = mc.adjusted_snapshot(prices())
    incoming = snap.iloc[570:700].drop(snap.index[580])
    assert not mc.extension_comparison(snap.iloc[:600], incoming).consistent.all()
    with pytest.raises(ValueError): mc.extend_adjusted_snapshot(snap.iloc[:600], incoming)


def test_e2e_acquires_full_known_period_once_and_keeps_past_causal(tmp_path):
    spec, output, provider = setup_run(tmp_path)
    owner = tmp_path/'runs/e2e'
    (owner/'orchestration').mkdir(parents=True)
    (owner/'orchestration/pipeline.json').write_text(json.dumps({'stages':[{'stage_key':'walk_forward','child_run_id':'wf'}]}))
    (owner/'config.json').write_text(json.dumps({'historical_data_cutoff':'2023-11-30'}))
    spec.source_end_to_end_run = 'e2e'
    first = runtime.materialize_context_diagnostic(spec, output, provider=provider)
    _, calendar, metrics, _ = runtime.load_context_diagnostic(output,tmp_path/'runs')
    assert calendar.session_date.max() == '2023-11-30'
    assert metrics.unique_dates.max() <= 64
    acquisition = _json(tmp_path/'runs'/first['acquisition']['manifest'])
    assert acquisition['requested_end_exclusive'] == '2023-11-30'
    holdout = tmp_path/'runs/holdout/results';holdout.mkdir(parents=True)
    data = pd.read_csv(output/'predictions.csv').iloc[:6].assign(Date=['2023-11-20']*3+['2023-11-21']*3)
    data = data.iloc[[0,3]].copy() # one observation per session and identity
    data.to_csv(holdout/'holdout_predictions.csv',index=False)
    spec.job_type = JobType.HOLDOUT_EVALUATION
    second = runtime.materialize_context_diagnostic(spec, holdout, provider=provider)
    assert provider.calls == 1
    assert second['context_manifest'] == first['context_manifest']
    assert second['comparability'] == first['comparability']
    snapshot = mc.adjusted_snapshot(prices())
    dates=pd.DatetimeIndex(pd.to_datetime(calendar.session_date))
    altered=snapshot.copy();altered.loc[altered.index>'2023-10-02'] *= 2
    protocol=mc.ContextProtocol.from_config(spec.config)
    before,cuts=mc.build_context(snapshot,dates,protocol,'2023-01-03','2023-07-03')
    after,_=mc.build_context(altered,dates,protocol,'2023-01-03','2023-07-03',cuts)
    pd.testing.assert_frame_equal(before.loc[before.session_date.le('2023-10-02')],after.loc[after.session_date.le('2023-10-02')])


@pytest.mark.parametrize('job',[JobType.WALK_FORWARD,JobType.HOLDOUT_EVALUATION,JobType.FORWARD_SIMULATION])
def test_real_divergence_blocks_only_diagnostic_and_archives_rejection(tmp_path,monkeypatch,caplog,job):
    spec, wf, provider = setup_run(tmp_path)
    initial = runtime.materialize_context_diagnostic(spec, wf, provider=provider)
    runs=tmp_path/'runs'; parent=runs/initial['context_manifest']
    immutable={p:p.read_bytes() for p in parent.parent.iterdir() if p.is_file()}
    raw=prices();frozen=pd.read_csv(parent.parent/'spy_adjusted_snapshot.csv',index_col=0,parse_dates=True)
    bad_date=frozen.index[-5]
    class Divergent:
        source_name='synthetic divergent'
        def fetch(self,symbol,start,end):
            part=raw.loc[(raw.index>=pd.Timestamp(start))&(raw.index<pd.Timestamp(end))].copy()
            part.loc[bad_date,'Adjusted'] *= 1.01
            return part
    monkeypatch.setattr(runtime,'YahooFinanceProvider',Divergent)
    output=wf if job==JobType.WALK_FORWARD else runs/job.value/'results'
    output.mkdir(parents=True,exist_ok=True)
    spec.job_type=job;spec.source_walk_forward_run='wf'
    record=pd.read_csv(wf/'predictions.csv').iloc[[0]].assign(Date='2023-10-31')
    if job==JobType.FORWARD_SIMULATION:
        record=record.rename(columns={'Date':'session_date'}).assign(source_model_id='synthetic')
    file=output/runtime.INPUTS[job.value][0]
    record.to_csv(file,index=False);scientific=file.read_bytes()
    registry=WorkflowRegistry({job:lambda *_:{'science':'completed','scientific_score':.73}})
    result=registry.execute(spec,output,progress_callback=None,cancellation_check=None)
    assert result['science']=='completed' and result['scientific_score']==.73
    assert result['market_context_diagnostic']['status']=='unavailable'
    assert result['market_context_diagnostic']['reason']=='context_extension_historical_returns_changed'
    assert file.read_bytes()==scientific
    assert all(p.read_bytes()==value for p,value in immutable.items())
    attempt=_json(output/runtime.ATTEMPT_MANIFEST)
    path=runs/attempt['acquisition']['manifest'];evidence=_json(path)
    assert digest(path)==attempt['acquisition']['sha256']
    assert evidence['status']=='rejected'
    assert evidence['comparison']['classification']=='material_revision'
    assert {'response.csv','overlap_comparison.csv'} <= set(evidence['artifact_digests'])
    for name,sha in evidence['artifact_digests'].items(): assert digest(path.parent/name)==sha
    assert 'traitement scientifique conservé' in caplog.text
    if job==JobType.WALK_FORWARD: assert _json(output/mc.DIAGNOSTIC_MANIFEST)['status']=='available'
    else: assert _json(output/mc.DIAGNOSTIC_MANIFEST)['status']=='unavailable'


def test_diagnostic_warning_storage_failure_cannot_fail_science(tmp_path,monkeypatch):
    spec,output,provider=setup_run(tmp_path)
    monkeypatch.setattr(runtime,'materialize_context_diagnostic',lambda *_:(_ for _ in ()).throw(ValueError('material divergence')))
    monkeypatch.setattr(runtime,'_publish',lambda *_:(_ for _ in ()).throw(OSError('disk full')))
    registry=WorkflowRegistry({JobType.WALK_FORWARD:lambda *_:{'science':'completed'}})
    result=registry.execute(spec,output,progress_callback=None,cancellation_check=None)
    assert result['science']=='completed'
    assert result['market_context_diagnostic']['status']=='unavailable'


def test_rejected_raw_acquisition_survives_normalization_failure(tmp_path):
    class Invalid:
        source_name='synthetic invalid'
        def fetch(self,*_):return prices().rename(columns={'Adjusted':'Close'})
    with pytest.raises(ContextAcquisitionError) as exc:
        acquire_spy(Invalid(),pd.Timestamp('2023-01-01').date(),pd.Timestamp('2024-01-01').date(),tmp_path/'acquisitions',tmp_path)
    path=tmp_path/exc.value.evidence['manifest']
    assert _json(path)['status']=='rejected'
    assert (path.parent/'response.csv').is_file()


def test_clear_warning_for_material_divergence():
    from rstock.application.market_context_ui import _incomplete_message
    text=_incomplete_message('context_extension_historical_returns_changed')
    assert 'SPY incomplet' in text and 'extension' in text and 'scientifiques' in text


def test_e2e_without_explicit_cutoff_uses_persisted_prepared_dates(tmp_path):
    spec,output,provider=setup_run(tmp_path)
    owner=tmp_path/'runs/e2e';(owner/'orchestration').mkdir(parents=True)
    (owner/'orchestration/pipeline.json').write_text('{"stages": []}')
    (output/'run_configuration.json').write_text('{"traceability": {"prepared_market_last_date": "2023-11-30T00:00:00"}}')
    spec.source_end_to_end_run='e2e'
    assert runtime._acquisition_end(spec,owner,pd.Timestamp('2023-10-02'),output)==pd.Timestamp('2023-11-30')
    spec.job_type=JobType.FORWARD_SIMULATION
    assert runtime._acquisition_end(spec,owner,pd.Timestamp('2023-10-02'),output)==pd.Timestamp('2023-10-02')


def test_acquisition_interruption_keeps_raw_response_and_retry_creates_new_attempt(tmp_path,monkeypatch):
    from rstock.application import market_context_acquisition as acq
    spec,output,provider=setup_run(tmp_path)
    original=acq._publish
    def interrupt(path,payload,**kwargs):
        if path.name=='acquisition_manifest.json' and payload.get('status')=='validated':
            raise RuntimeError('interrupted before validation commit')
        return original(path,payload,**kwargs)
    monkeypatch.setattr(acq,'_publish',interrupt)
    args=(provider,pd.Timestamp('2023-01-01').date(),pd.Timestamp('2023-11-30').date(),tmp_path/'acquisitions',tmp_path)
    with pytest.raises(ContextAcquisitionError): acquire_spy(*args)
    first=next((tmp_path/'acquisitions').glob('*/acquisition_manifest.json'))
    before=first.read_bytes()
    assert (first.parent/'response.csv').exists()
    monkeypatch.setattr(acq,'_publish',original)
    _,reference=acquire_spy(*args)
    assert tmp_path/reference['manifest']!=first
    assert first.read_bytes()==before
    assert _json(tmp_path/reference['manifest'])['status']=='validated'


def test_export_evidence_preserves_acquisition_bytes_and_detects_changes(tmp_path):
    from rstock.application.forward_export import _Evidence
    spec,output,provider=setup_run(tmp_path)
    manifest=runtime.materialize_context_diagnostic(spec,output,provider=provider)
    runs=tmp_path/'runs';evidence=_Evidence(runs)
    evidence.acquisition(manifest['acquisition'])
    assert any(name.endswith('/response.csv') for name in evidence.attachments)
    for name,raw in evidence.attachments.items(): assert (runs/name.removeprefix('runs/')).read_bytes()==raw
    response=next(name for name in evidence.attachments if name.endswith('/response.csv'))
    with (runs/response.removeprefix('runs/')).open('a') as stream: stream.write('changed')
    with pytest.raises(ValueError,match='changed'): evidence.unchanged()
