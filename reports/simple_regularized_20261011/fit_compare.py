"""Report-only, frozen-population logistic ridge comparison; no application jobs."""
import os
for _name in ('OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS', 'MKL_NUM_THREADS'):
    os.environ[_name] = '1'
from pathlib import Path
import argparse
import hashlib
import json
import pickle
import sys
import time
import platform
import numpy as np
import pandas as pd
import scipy
from scipy.optimize import minimize
from scipy.special import expit

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from rstock.evaluation import probability_losses, _roc_auc
from rstock.features import intraday_target_column, intraday_down_target_column
OUT = Path(__file__).resolve().parent
RUNS = {'external': '20261011T032201_17ece38f7b',
        'mixed': '20261011T032130_4742d2f509', 'target': '20261011T031942_217eeee2c1'}

def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for chunk in iter(lambda: f.read(4*1024*1024), b''): h.update(chunk)
    return h.hexdigest()

def atomic_json(path, value):
    tmp = path.with_suffix('.tmp')
    tmp.write_text(json.dumps(value, indent=2, allow_nan=False), encoding='utf-8')
    tmp.replace(path)

def objective(theta, z, y, penalty):
    score = theta[0] + z @ theta[1:]
    residual = expit(score) - y
    loss = np.mean(np.logaddexp(0., score) - y*score) + .5*penalty*np.dot(theta[1:],theta[1:])
    gradient = np.r_[residual.mean(), z.T @ residual / len(y) + penalty*theta[1:]]
    return loss, gradient

def numerical_checks():
    # No optimizer or training: check analytic gradient, extreme logits, scaler contract.
    z = np.array([[1.,0.],[-1.,2.],[.5,-3.],[2.,1.]])
    y = np.array([0.,1.,0.,1.]); theta = np.array([.2,-.3,.4])
    loss, grad = objective(theta,z,y,.1)
    eps = 1e-6
    numeric = np.array([(objective(theta+np.eye(3)[i]*eps,z,y,.1)[0]-
                         objective(theta-np.eye(3)[i]*eps,z,y,.1)[0])/(2*eps) for i in range(3)])
    assert np.allclose(grad,numeric,rtol=1e-7,atol=1e-8)
    assert np.isfinite(objective(np.array([1000.,1000.,-1000.]),z,y,.1)[0])
    return {'analytic_gradient_max_error':float(np.max(abs(grad-numeric))), 'stable_extreme_logits':True}

def load_family(family):
    base = ROOT/'runs'/RUNS[family]
    cfg = json.loads((base/'config.json').read_text(encoding='utf-8'))
    assert json.loads((base/'status.json').read_text())['status']=='completed'
    rc = cfg['rstock_config']
    assert rc['predictor_prefilter_enabled'] is False and rc['date_feature_regex']==''
    p = base/'checkpoints/artifacts/prepared_snapshot.pkl'
    assert sha(p)==json.loads(p.with_suffix('.json').read_text())['sha256']
    frame = pickle.loads(p.read_bytes())['prepared']
    windows = pd.read_csv(base/'results/windows.csv')
    keys = sorted(windows.Set.unique()); targets = sorted(windows.Observation.unique())
    assert len(targets)==20 and len(keys)==(20 if family=='target' else 3800)
    dates = sorted(pd.read_csv(ROOT/'diagnostics/aligned_constant_20261011/aligned_constant_20261011T031914_c00cea6f22.csv',usecols=['Date']).Date.unique())
    assert len(dates)==433 and dates[0]=='2024-07-12' and dates[-1]=='2026-04-02'
    probabilities = np.full((len(keys),len(dates)),np.nan)
    keypos = {k:i for i,k in enumerate(keys)}; datepos={d:i for i,d in enumerate(dates)}
    labels = {t:frame.loc[pd.DatetimeIndex(dates),intraday_target_column(t)].to_numpy(dtype=int) for t in targets}
    expected_windows = np.empty(len(dates),dtype=int)
    ref = windows[windows.Set==keys[0]].sort_values('Window')
    for row in ref.itertuples():
        ix = [datepos[d] for d in dates if row.TestStart<=d<=row.TestEnd]
        expected_windows[ix]=row.Window
    for chunk in pd.read_csv(base/'results/predictions.csv',usecols=['Set','Observation','Window','Date','UpProbability','IntradayTarget'],chunksize=65536,float_precision='round_trip'):
        ki=chunk.Set.map(keypos).to_numpy(); di=chunk.Date.map(datepos).to_numpy()
        assert not pd.isna(ki).any() and not pd.isna(di).any()
        ki=ki.astype(int); di=di.astype(int)
        assert len(np.unique(ki*len(dates)+di))==len(chunk)
        assert np.isnan(probabilities[ki,di]).all()
        assert np.array_equal(chunk.Window.to_numpy(),expected_windows[di])
        assert all(json.loads(k)[0]==t for k,t in zip(chunk.Set,chunk.Observation))
        assert np.array_equal(chunk.IntradayTarget.to_numpy(),np.array([labels[t][i] for t,i in zip(chunk.Observation,di)]))
        probabilities[ki,di]=chunk.UpProbability.to_numpy()
    assert np.isfinite(probabilities).all() and ((probabilities>=0)&(probabilities<=1)).all()
    # Verify exact per-candidate masks, training/test boundaries and saved XGB scores.
    for number,row in enumerate(windows.itertuples(),start=1):
        names=json.loads(row.FeatureColumns); target=row.Observation
        data=frame[[*names,intraday_target_column(target),intraday_down_target_column(target)]].dropna()
        train=data.loc[row.TrainStart:row.TrainEnd]; test=data.loc[row.TestStart:row.TestEnd]
        assert len(train)==row.TrainObservations and len(test)==row.TestObservations
        assert train.index.max()<test.index.min() and train.index.max()<pd.Timestamp('2026-04-06')
        assert 0<train[intraday_target_column(target)].mean()<1, 'Single-class training window'
        assert np.isfinite(train[names].to_numpy(dtype=float)).all()
        assert np.isfinite(test[names].to_numpy(dtype=float)).all()
        ix=np.array([datepos[str(d.date())] for d in test.index])
        assert np.array_equal(expected_windows[ix],np.full(len(ix),row.Window))
        ll,br=probability_losses(test[intraday_target_column(target)].to_numpy(),probabilities[keypos[row.Set],ix])
        assert np.isclose(ll.mean(),row.UpLogLoss,rtol=0,atol=1e-10)
        assert np.isclose(br.mean(),row.UpBrier,rtol=0,atol=1e-10)
        if number%1330==0: print(f'{family}: checked {number}/{len(windows)} window contracts',flush=True)
    return frame,windows,keys,targets,dates,probabilities,cfg

def fit_target(frame, windows, keys, dates, penalty):
    p=np.full((len(keys),len(dates)),np.nan); audit=[]; datepos={d:i for i,d in enumerate(dates)}
    for i,key in enumerate(keys):
        rows=windows[windows.Set==key].sort_values('Window')
        for row in rows.itertuples():
            names=json.loads(row.FeatureColumns); label=intraday_target_column(row.Observation)
            data=frame[[*names,label,intraday_down_target_column(row.Observation)]].dropna()
            train=data.loc[row.TrainStart:row.TrainEnd]; test=data.loc[row.TestStart:row.TestEnd]
            x=train[names].to_numpy(dtype=float); mean=x.mean(axis=0); scale=x.std(axis=0,ddof=0)
            scale[scale==0]=1.; z=(x-mean)/scale; y=train[label].to_numpy(dtype=float)
            prevalence=y.mean(); assert 0<prevalence<1, 'Single-class train: stop; no substituted estimator'
            theta=np.r_[np.log(prevalence/(1-prevalence)),np.zeros(len(names))]
            started=time.perf_counter()
            result=minimize(objective,theta,args=(z,y,penalty),method='L-BFGS-B',jac=True,
                            options={'maxiter':1000,'gtol':1e-8,'ftol':1e-12})
            elapsed=time.perf_counter()-started
            record={'Set':key,'Window':row.Window,'TrainN':len(train),'TestN':len(test),
                    'success':bool(result.success),'message':str(result.message),'iterations':int(result.nit),
                    'gradient_max':float(np.max(abs(result.jac))),'seconds':elapsed,
                    'FeatureColumns':json.dumps(names),'mean':json.dumps(mean.tolist()),
                    'scale':json.dumps(scale.tolist()),'coefficients':json.dumps(result.x.tolist()),
                    'mean_penalty':penalty,'equivalent_sum_penalty':penalty*len(train)}
            if not result.success or not np.isfinite(result.x).all():
                atomic_json(OUT/'nonconvergence.json',record)
                raise RuntimeError('Optimizer failed; stopped without retry or fallback')
            ix=np.array([datepos[str(d.date())] for d in test.index])
            p[i,ix]=expit(result.x[0]+((test[names].to_numpy(dtype=float)-mean)/scale) @ result.x[1:])
            audit.append(record)
    assert np.isfinite(p).all()
    return p,pd.DataFrame(audit)

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--families',nargs='+',choices=['external'],default=['external'])
    parser.add_argument('--penalty',type=float,default=.1)
    parser.add_argument('--execute',action='store_true',help='Explicitly authorize fits; default only preflight')
    args=parser.parse_args(); assert args.penalty==.1 and args.families==['external'], 'Only the adopted diagnostic protocol is authorized'
    args.families=list(dict.fromkeys(args.families))
    checks=numerical_checks()
    inputs=[Path(__file__),ROOT/'rstock/evaluation.py',ROOT/'rstock/features.py',
            ROOT/'diagnostics/aligned_constant_20261011/aligned_constant_20261011T031914_c00cea6f22.csv']
    for family in args.families:
        inputs.extend(ROOT/'runs'/RUNS[family]/name for name in ['config.json','status.json','results/windows.csv',
             'results/predictions.csv','results/run_configuration.json','checkpoints/artifacts/prepared_snapshot.pkl',
             'checkpoints/artifacts/prepared_snapshot.json'])
    digests={str(p.relative_to(ROOT)):sha(p) for p in inputs}
    manifest={'families':args.families,'runs':{f:RUNS[f] for f in args.families},'direction':'Up',
      'objective':'mean(binary logloss) + penalty/2 * squared coefficient norm; intercept unpenalized',
      'penalty':args.penalty,'scaling':'train-only mean/std ddof=0; zero std replaced by 1',
      'solver':{'method':'L-BFGS-B','maxiter':1000,'gtol':1e-8,'ftol':1e-12},
      'initialization':'intercept logit(train prevalence); coefficients zero; no weights',
      'evaluation':'all historical WF candidates, no qualification selection; no holdout/forward',
      'weighting':'equal candidates within target, equal 20 targets; equal dates; candidate AUC never pooled',
      'versions':{'python':platform.python_version(),'numpy':np.__version__,'pandas':pd.__version__,'scipy':scipy.__version__},
      'historical_effective_configuration':{f:json.loads((ROOT/'runs'/RUNS[f]/'config.json').read_text(encoding='utf-8')) for f in args.families},
      'source_sha256':digests,'numerical_checks':checks}
    mp=OUT/'manifest.json'
    if mp.exists(): assert json.loads(mp.read_text())==manifest, 'Frozen manifest differs; no overwrite'
    elif args.execute: atomic_json(mp,manifest)
    allmetrics=[]; allaudit=[]; periods=[]; windowmetrics=[]
    for family in args.families:
        frame,windows,keys,targets,dates,xgb,cfg=load_family(family)
        print(f'{family}: preflight validated {len(keys)} candidates, 7 windows, 433 dates',flush=True)
        if not args.execute: continue
        for target in targets:
            targetkeys=[k for k in keys if json.loads(k)[0]==target]
            cp=OUT/f'{family}_{target}.npz'; ap=OUT/f'{family}_{target}_audit.csv'; commit=OUT/f'{family}_{target}_complete.json'
            if commit.exists():
                saved=json.loads(commit.read_text()); assert saved['manifest_sha256']==sha(mp)
                assert saved['probabilities_sha256']==sha(cp) and saved['audit_sha256']==sha(ap)
                with np.load(cp,allow_pickle=False) as archive:
                    assert archive['keys'].tolist()==targetkeys and archive['dates'].tolist()==dates
                    p=archive['probabilities'].copy()
                audit=pd.read_csv(ap)
            else:
                p,audit=fit_target(frame,windows,targetkeys,dates,args.penalty)
                tmp=cp.with_suffix('.tmp.npz'); np.savez_compressed(tmp,keys=np.array(targetkeys),dates=np.array(dates),probabilities=p,
                    labels=frame.loc[pd.DatetimeIndex(dates),intraday_target_column(target)].to_numpy(dtype=int))
                tmp.replace(cp); audit.to_csv(ap,index=False)
                atomic_json(commit,{'manifest_sha256':sha(mp),'probabilities_sha256':sha(cp),'audit_sha256':sha(ap)})
            allaudit.append(audit)
            y=frame.loc[pd.DatetimeIndex(dates),intraday_target_column(target)].to_numpy(dtype=int)
            for i,key in enumerate(targetkeys):
                rows=windows[windows.Set==key].sort_values('Window'); base=np.empty(len(dates))
                for row in rows.itertuples():
                    label=intraday_target_column(target); names=json.loads(row.FeatureColumns)
                    data=frame[[*names,label,intraday_down_target_column(target)]].dropna()
                    prevalence=data.loc[row.TrainStart:row.TrainEnd,label].mean()
                    mask=np.array([row.TestStart<=d<=row.TestEnd for d in dates]); base[mask]=prevalence
                for model,prob in [('logistic',p[i]),('xgboost',xgb[keys.index(key)]),('constant',base)]:
                    ll,br=probability_losses(y,prob)
                    allmetrics.append({'Family':family,'Model':model,'Observation':target,'Set':key,'N':len(y),
                                       'LogLoss':float(ll.mean()),'Brier':float(br.mean()),'AUC':_roc_auc(y,prob)})
                    for period,ix in enumerate(np.array_split(np.arange(len(dates)),3),start=1):
                        periods.append({'Family':family,'Model':model,'Observation':target,'Set':key,'Period':period,
                            'FirstDate':dates[ix[0]],'LastDate':dates[ix[-1]],'N':len(ix),
                            'LogLoss':float(ll[ix].mean()),'Brier':float(br[ix].mean()),'AUC':_roc_auc(y[ix],prob[ix])})
                    for row in rows.itertuples():
                        ix=np.array([j for j,d in enumerate(dates) if row.TestStart<=d<=row.TestEnd])
                        windowmetrics.append({'Family':family,'Model':model,'Observation':target,'Set':key,'Window':row.Window,
                            'N':len(ix),'LogLoss':float(ll[ix].mean()),'Brier':float(br[ix].mean()),'AUC':_roc_auc(y[ix],prob[ix])})
            print(f'{family} {target}: {len(targetkeys)*7} fits committed/reused',flush=True)
    assert all(sha(ROOT/path)==digest for path,digest in digests.items()), 'Historical source changed'
    if not args.execute:
        print('Preflight complete; no fits and no outputs written',flush=True); return
    metrics=pd.DataFrame(allmetrics); metrics.to_csv(OUT/'candidate_metrics.csv',index=False)
    bytarget=metrics.groupby(['Family','Model','Observation'])[['LogLoss','Brier','AUC']].mean().reset_index()
    bytarget.to_csv(OUT/'target_metrics.csv',index=False)
    overall=bytarget.groupby(['Family','Model'])[['LogLoss','Brier','AUC']].mean().reset_index()
    overall.to_csv(OUT/'overall_metrics.csv',index=False)
    periodframe=pd.DataFrame(periods)
    periodframe.to_csv(OUT/'candidate_period_metrics.csv',index=False)
    periodtargets=periodframe.groupby(['Family','Model','Observation','Period'])[['LogLoss','Brier','AUC']].mean().reset_index()
    periodtargets.to_csv(OUT/'target_period_metrics.csv',index=False)
    periodtargets.groupby(['Family','Model','Period'])[['LogLoss','Brier','AUC']].mean().reset_index().to_csv(OUT/'overall_period_metrics.csv',index=False)
    pd.DataFrame(windowmetrics).to_csv(OUT/'window_metrics.csv',index=False)
    paired=metrics[metrics.Model=='logistic'].merge(metrics[metrics.Model=='xgboost'],on=['Family','Observation','Set','N'],suffixes=('_logistic','_xgboost'),validate='one_to_one')
    paired=paired.merge(metrics[metrics.Model=='constant'][['Family','Observation','Set','LogLoss','Brier','AUC']],on=['Family','Observation','Set'],validate='one_to_one')
    for metric in ['LogLoss','Brier','AUC']:
        paired.rename(columns={metric:metric+'_constant'},inplace=True)
        paired['Delta_'+metric+'_vs_xgboost']=paired[metric+'_logistic']-paired[metric+'_xgboost']
        paired['Delta_'+metric+'_vs_constant']=paired[metric+'_logistic']-paired[metric+'_constant']
    paired.to_csv(OUT/'paired_candidate_metrics.csv',index=False)
    audits=pd.concat(allaudit,ignore_index=True)
    atomic_json(OUT/'summary.json',{'manifest_sha256':sha(mp),'fits':len(audits),'all_converged':bool(audits.success.all()),
      'optimizer_seconds_sum':float(audits.seconds.sum()),'max_iterations':int(audits.iterations.max()),
      'overall':overall.to_dict('records'),'historical_inputs_unchanged':True,
      'limitations':['retrospective previously consulted dates','dependent candidates/windows','no provider PIT guarantee',
                     'no holdout/forward or selection-procedure validation'],
      'output_sha256':{p.name:sha(p) for p in OUT.glob('*.npz')}})
    print(overall.to_string(index=False),flush=True)

if __name__=='__main__': main()
