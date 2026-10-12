"""Read archived promotion/quality evidence; no model fit, prediction or job."""
from pathlib import Path
import hashlib, json, pickle, sys
import numpy as np
import pandas as pd

ROOT=Path(__file__).resolve().parents[2]
OUT=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT))
from rstock.evaluation import probability_losses, _roc_auc
ORIGINS=['20260919T020736_28fa866cd2','20260919T213755_20f36d8467']
WF={'20260919T020736_28fa866cd2':'20260919T020736_c46d78d58f',
    '20260919T213755_20f36d8467':'20260919T213756_53f8770057'}
LIVE={'scheduled_live','legacy_inferred_live','watching_live'}

def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def clean(v):
    if isinstance(v,dict):return {str(k):clean(x) for k,x in v.items()}
    if isinstance(v,(list,tuple)):return [clean(x) for x in v]
    if isinstance(v,np.generic):return clean(v.item())
    if isinstance(v,float) and not np.isfinite(v):return None
    if isinstance(v,pd.Timestamp):return v.isoformat()
    return v
def origin(m):
    c=m.get('source_configuration',{})
    return c.get('source_end_to_end_run') or c.get('source_experiment_run')

def main():
    paths=[Path(__file__),ROOT/'rstock/evaluation.py',ROOT/'rstock/application/production_services.py',
        ROOT/'rstock/application/production_quality.py',ROOT/'production/model_registry.json',
        ROOT/'production/quality/current_generation.json',ROOT/'production/quality/manifest.json',
        ROOT/'production/history/predictions.csv',ROOT/'production/real_trades.json',
        ROOT/'runs'/ORIGINS[0]/'results/promotion_results.json']
    digests={}
    def track(p):
        key=str(p.relative_to(ROOT));digests[key]=sha(p);return p
    for p in paths:track(p)
    registry=json.loads((ROOT/'production/model_registry.json').read_text())['models']
    models=[m for m in registry if origin(m) in ORIGINS]
    assert len(models)==24
    promotion=json.loads((ROOT/'runs'/ORIGINS[0]/'results/promotion_results.json').read_text())
    assert {x['model_id'] for x in promotion['candidates']}=={m['model_id'] for m in models if origin(m)==ORIGINS[0]}
    current=json.loads((ROOT/'production/quality/current_generation.json').read_text())
    generation=current['generation']
    snapshots={}
    for o,wf in WF.items():
        p=track(ROOT/'runs'/wf/'checkpoints/artifacts/prepared_snapshot.pkl')
        snapshots[o]=pickle.loads(p.read_bytes())['prepared']
    hist=pd.read_csv(ROOT/'production/history/predictions.csv',float_precision='round_trip')
    assert not hist.prediction_id.duplicated().any()
    hi=hist.set_index('prediction_id')
    frames=[];population=[];baselines=[]; quality_rows=[]
    for m in models:
        mid=m['model_id'];o=origin(m)
        meta=json.loads(track(ROOT/'production/artifacts'/mid/'production.metadata.json').read_text())
        assert int(meta['artifact_version'])==int(m['artifact_version'])
        d=pd.read_parquet(track(ROOT/'production/quality/observations'/f'model_id={mid}'/'observations.parquet'))
        assert not d.duplicated(['prediction_id']).any()
        qpath=ROOT/'production/quality/generations'/generation/'snapshots'/f'{mid}.json'
        qs=json.loads(track(qpath).read_text())
        quality_rows.append({'Model':mid,'Target':m['target'],'Origin':o,'Keys':';'.join(qs.keys()),
            'PersistedProductionSinceActivation':json.dumps(qs.get('production_since_activation',{})),
            'PersistedSincePromotion':json.dumps(qs.get('since_promotion',{})),
            'AsOf':qs.get('as_of_session')})
        for row in d.itertuples():
            assert row.prediction_id in hi.index
            h=hi.loc[row.prediction_id]
            assert int(h.model_version)==int(row.model_version)
            assert pd.Timestamp(h.prediction_date)==pd.Timestamp(row.session_date)
            if row.prediction_status=='predicted':
                assert np.isclose(float(h.up_probability),float(row.up_probability),rtol=0,atol=1e-10)
            assert pd.Timestamp(h.created_at)==pd.Timestamp(row.prediction_created_at)
        d['session_date']=pd.to_datetime(d.session_date).dt.normalize()
        d['as_of_date']=pd.to_datetime(d.as_of_date).dt.normalize()
        d['prediction_created_at']=pd.to_datetime(d.prediction_created_at,utc=True)
        d['market_open']=pd.to_datetime(d.session_date.dt.strftime('%Y-%m-%d')+' 09:30').dt.tz_localize('America/New_York').dt.tz_convert('UTC')
        d['market_close']=pd.to_datetime(d.session_date.dt.strftime('%Y-%m-%d')+' 16:00').dt.tz_localize('America/New_York').dt.tz_convert('UTC')
        promoted=pd.Timestamp(m['created_at']);trained=pd.Timestamp(meta['trained_at'])
        train_end=pd.Timestamp(meta['train_end']).normalize()
        d['source_cohort']=o;d['current_registry_status']=m['status'];d['promotion_at']=promoted
        d['post_decision']=(d.market_open>promoted)&(d.prediction_created_at>=promoted)
        d['origin_live_tag']=d.prediction_origin.isin(LIVE)
        d['evaluated']=d.evaluation_status.eq('evaluated')&d.intraday_return.notna()&d.up_target.notna()
        d['preopen_timestamp']=d.prediction_created_at<d.market_open
        d['intraday_timestamp']=(d.prediction_created_at>=d.market_open)&(d.prediction_created_at<d.market_close)
        d['postclose_timestamp']=d.prediction_created_at>=d.market_close
        d['metadata_version_matched']=d.model_version.eq(meta['artifact_version'])
        d['prior_training']=(d.session_date>train_end)&(d.prediction_created_at>=trained)&d.metadata_version_matched
        d['prior_asof']=d.as_of_date<d.session_date
        d['strict_timing']=d.post_decision&d.origin_live_tag&d.preopen_timestamp&d.prior_training&d.prior_asof
        d['strict_original_version']=d.strict_timing&d.model_version.eq(1)
        d['timing_class']=np.select([~d.post_decision,~d.origin_live_tag,d.postclose_timestamp,d.intraday_timestamp,
            ~d.metadata_version_matched,~d.prior_training,~d.prior_asof,d.strict_timing],
            ['before_decision_or_historical','backfill_or_unverified_origin','after_close','intraday_not_preopen',
             'prior_version_metadata_unavailable','training_not_prior','asof_not_prior','preopen_matched_version'],default='timestamp_unavailable')
        p0=np.nan;baseline_status='training_population_unavailable'
        source=snapshots[o];cols=meta['predictor_columns']; labels=list(meta['outcomes'].values())
        if pd.Timestamp(meta['train_end'])<=source.index.max():
            train=source.loc[meta['train_start']:meta['train_end'],[*cols,*labels]].dropna()
            if len(train)==int(meta['observations']) and str(train.index.min())==meta['train_start'].replace('T',' ') and str(train.index.max())==meta['train_end'].replace('T',' '):
                p0=float(train[meta['outcomes']['up']].mean())
                baseline_status='reconstructed_discovery_snapshot_matching_train_bounds_count_not_production_train_hash'
        d['constant_probability_reconstructed']=np.where(d.metadata_version_matched,p0,np.nan)
        d['baseline_status']=np.where(d.metadata_version_matched,baseline_status,'historical_version_training_unavailable')
        population.append({'Model':mid,'Origin':o,'Target':m['target'],'Predictors':','.join(m['predictors']),
            'Created':m['created_at'],'RegistryStatus':m['status'],'CurrentArtifactVersion':m['artifact_version'],
            'TrainStart':meta['train_start'],'TrainEnd':meta['train_end'],'TrainedAt':meta['trained_at'],
            'TrainObservations':meta['observations'],'UpThreshold':m['up_threshold'],'DownThreshold':m['down_threshold'],
            'ReconstructedConstant':p0,'BaselineStatus':baseline_status,'ObservationRows':len(d),
            'PostDecisionEvaluated':int((d.post_decision&d.evaluated).sum()),
            'StrictOriginalEvaluated':int((d.strict_original_version&d.evaluated).sum()),
            'StrictOriginalSignals':int((d.strict_original_version&d.evaluated&d.is_bullish_signal).sum())})
        frames.append(d)
    obs=pd.concat(frames,ignore_index=True)
    eligible_prob=obs.up_probability.notna()&obs.down_probability.notna()
    signal=(obs.up_probability>=obs.up_threshold)&(obs.down_probability<obs.down_threshold)
    assert (signal[eligible_prob]==obs.loc[eligible_prob,'is_bullish_signal']).all()
    metricrows=[];aggrows=[];dates=[];scope_frames={}
    scopes={
        'post_decision_all_origins':obs.post_decision&obs.evaluated,
        'post_decision_live_tag':obs.post_decision&obs.evaluated&obs.origin_live_tag,
        'preopen_original_version_matched':obs.strict_original_version&obs.evaluated,
        'preopen_updated_version_matched':obs.strict_timing&obs.evaluated&obs.model_version.gt(1)}
    for name,mask in scopes.items():
        g=obs.loc[mask].copy();scope_frames[name]=g
        for (mid,ver),x in g.groupby(['model_id','model_version']):
            y=x.up_target.to_numpy(dtype=int);p=x.up_probability.to_numpy(dtype=float)
            ll,br=probability_losses(y,p);s=x.is_bullish_signal.to_numpy(dtype=bool);rets=x.intraday_return.to_numpy(dtype=float)
            baseline=x.constant_probability_reconstructed.notna().all()
            dll=dbr=np.nan
            if baseline:
                l0,b0=probability_losses(y,x.constant_probability_reconstructed.to_numpy(dtype=float));dll=float((ll-l0).mean());dbr=float((br-b0).mean())
            row={'Scope':name,'Origin':x.source_cohort.iloc[0],'Model':mid,'Version':int(ver),'Target':x.target.iloc[0],
                'Observations':len(x),'Dates':x.session_date.nunique(),'FirstDate':x.session_date.min().date().isoformat(),
                'LastDate':x.session_date.max().date().isoformat(),'Signals':int(s.sum()),'UpPrevalence':float(y.mean()),
                'UpSignalPrecision':float(y[s].mean()) if s.any() else np.nan,
                'PositiveReturnWinRate':float((rets[s]>0).mean()) if s.any() else np.nan,
                'SignalMeanReturn':float(rets[s].mean()) if s.any() else np.nan,
                'PnlNotional10000':float(rets[s].sum()*10000),'DailyBuyMeanReturnSameDates':float(rets.mean()),
                'SignalMinusDailyMeanReturn':float(rets[s].mean()-rets.mean()) if s.any() else np.nan,
                'UpLogLoss':float(ll.mean()),'UpBrier':float(br.mean()),'UpAUC':_roc_auc(y,p),
                'ReconstructedDeltaLL':dll,'ReconstructedDeltaBrier':dbr,'BaselineAvailableReconstructed':bool(baseline)}
            metricrows.append(row)
        for cohort,cg in [('all24',g),*((o,g[g.source_cohort==o]) for o in ORIGINS)]:
            s=cg.is_bullish_signal.fillna(False);r=cg.intraday_return.astype(float)
            daily=cg.loc[s].groupby('session_date').intraday_return.agg(['sum','size'])
            aggrows.append({'Scope':name,'Cohort':cohort,'Models':cg.model_id.nunique(),'Versions':len(cg[['model_id','model_version']].drop_duplicates()),
                'Observations':len(cg),'Dates':cg.session_date.nunique(),'FirstDate':cg.session_date.min().date().isoformat() if len(cg) else None,
                'LastDate':cg.session_date.max().date().isoformat() if len(cg) else None,'Signals':int(s.sum()),
                'SignalDates':cg.loc[s,'session_date'].nunique(),'PositiveReturnWinRate':float((r[s]>0).mean()) if s.any() else np.nan,
                'UpEventPrecision':float(cg.loc[s,'up_target'].astype(float).mean()) if s.any() else np.nan,
                'MeanSignalReturn':float(r[s].mean()) if s.any() else np.nan,'PnlNotional10000':float(r[s].sum()*10000),
                'MaxDailySignals':int(daily['size'].max()) if len(daily) else 0,
                'MaxDailyNotional':int(daily['size'].max()*10000) if len(daily) else 0})
            for date,v in daily.iterrows():dates.append({'Scope':name,'Cohort':cohort,'Date':date.date().isoformat(),'Signals':int(v['size']),'PnlNotional10000':float(v['sum']*10000)})
    # Record concentration without deleting losing models or days.
    metrics=pd.DataFrame(metricrows);aggregate=pd.DataFrame(aggrows);daymetrics=pd.DataFrame(dates)
    strict=metrics[metrics.Scope=='preopen_original_version_matched']
    strict_dates=scope_frames['preopen_original_version_matched']
    datesets={m:set(x.session_date) for m,x in strict_dates.groupby('model_id')}
    common=set.intersection(*datesets.values()) if datesets else set()
    common_frame=strict_dates[strict_dates.session_date.isin(common)]
    commonrows=[]
    for mid,x in common_frame.groupby('model_id'):
        y=x.up_target.to_numpy(dtype=int);p=x.up_probability.to_numpy(dtype=float);ll,br=probability_losses(y,p)
        dll=np.nan
        if x.constant_probability_reconstructed.notna().all():dll=float((ll-probability_losses(y,x.constant_probability_reconstructed.to_numpy(dtype=float))[0]).mean())
        commonrows.append({'Model':mid,'Target':x.target.iloc[0],'Dates':len(x),'UpLogLoss':float(ll.mean()),'UpAUC':_roc_auc(y,p),'ReconstructedDeltaLL':dll})
    rr=[]
    for name,g in metrics.groupby('Scope'):
        t=g.groupby('Target')[['UpLogLoss','UpBrier','ReconstructedDeltaLL','ReconstructedDeltaBrier']].mean()
        rr.append({'Scope':name,'Weighting':'equal target then equal model/version within target; each model dates equal',
            'Targets':len(t),'UpLogLoss':float(t.UpLogLoss.mean()),'UpBrier':float(t.UpBrier.mean()),
            'ReconstructedDeltaLL':float(t.ReconstructedDeltaLL.mean()),'ReconstructedDeltaBrier':float(t.ReconstructedDeltaBrier.mean()),
            'ModelsWithBaseline':int(g.BaselineAvailableReconstructed.sum()),'ModelsBeatReconstructedConstant':int((g.ReconstructedDeltaLL<0).sum())})
    trades=json.loads((ROOT/'production/real_trades.json').read_text()).get('trades',[])
    selectedtrades=[t for t in trades if t.get('model_id') in {m['model_id'] for m in models}]
    for name,frame in [('population',pd.DataFrame(population)),('observations_classified',obs),
        ('model_metrics',metrics),('cohort_metrics',aggregate),('daily_signal_contributions',daymetrics),
        ('equal_target_probability_metrics',pd.DataFrame(rr)),('common_calendar_probability_metrics',pd.DataFrame(commonrows)),
        ('persisted_quality_snapshot_comparison',pd.DataFrame(quality_rows))]:
        frame.to_csv(OUT/(name+'.csv'),index=False)
    assert all(sha(ROOT/k)==v for k,v in digests.items()),'Historical input changed'
    summary={'population':'all24 registered models from two source decisions, including inactive/watching, 16+8',
        'cohort_sizes':{o:sum(origin(m)==o for m in models) for o in ORIGINS},
        'current_quality_generation':generation,'registry_status_counts':pd.Series([m['status'] for m in models]).value_counts().to_dict(),
        'observations_total':len(obs),'origin_counts':obs.prediction_origin.value_counts().to_dict(),
        'timing_counts_post_decision_evaluated':obs[obs.post_decision&obs.evaluated].timing_class.value_counts().to_dict(),
        'aggregate':aggrows,'equal_target_probability':rr,'common_calendar_dates':[x.date().isoformat() for x in sorted(common)],
        'strict_original_models_missing':sorted({m['model_id'] for m in models}-set(strict.Model)),
        'strict_positive_pnl_models':int((strict.PnlNotional10000>0).sum()),
        'strict_negative_pnl_models':int((strict.PnlNotional10000<0).sum()),
        'strict_zero_pnl_models':int((strict.PnlNotional10000==0).sum()),
        'real_trades_all_records':len(trades),'real_trades_matching_cohort':len(selectedtrades),
        'baseline_limit':'constant reconstructed from discovery snapshot at matching production metadata train bounds/count; actual production training matrix/hash not persisted here, provider revisions not excluded; updated/historical version reference may be unavailable',
        'timing_limit':'preopen classification checks archived created_at, as_of and matching current-version training metadata; not a guarantee of immutable historical booster bytes or provider PIT',
        'historical_inputs_unchanged':True,'new_training_executed':False,'new_prediction_executed':False,
        'input_sha256':digests}
    (OUT/'summary.json').write_text(json.dumps(clean(summary),ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
    print(json.dumps(clean({k:v for k,v in summary.items() if k!='input_sha256'}),ensure_ascii=False,indent=2),flush=True)

if __name__=='__main__':main()
