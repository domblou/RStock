"""Evaluate archived WF-selected candidates on their later holdout; never fit."""
from pathlib import Path
import hashlib
import json
import pickle
import sys

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from rstock.evaluation import probability_losses, _roc_auc

OUT = Path(__file__).resolve().parent
RUNS = {
    'constant20': '20261011T031914_c00cea6f22',
    'target20': '20261011T031942_217eeee2c1',
    'mixed20': '20261011T032130_4742d2f509',
    'external20': '20261011T032201_17ece38f7b',
    'external150': '20261011T133446_bab008d129',
}

def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(4 * 1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()

def jsonable(value):
    if isinstance(value, dict): return {str(k): jsonable(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)): return [jsonable(v) for v in value]
    if isinstance(value, np.generic): return jsonable(value.item())
    if isinstance(value, float) and not np.isfinite(value): return None
    return value

def spearman(a, b):
    a, b = pd.Series(a, dtype=float), pd.Series(b, dtype=float)
    return float(a.rank().corr(b.rank())) if a.nunique() > 1 and b.nunique() > 1 else np.nan

def main():
    inputs = [Path(__file__), ROOT/'rstock/evaluation.py',
              ROOT/'rstock/qualification.py', ROOT/'rstock/walk_forward.py',
              ROOT/'reports/paired_predictors_20261011/candidate_metrics.csv',
              ROOT/'reports/paired_predictors_20261011/summary.json']
    inputs.append(ROOT/'reports/paired_predictors_20261011/window_metrics.csv')
    for family in ['mixed20','external20']:
        inputs.append(ROOT/'runs'/RUNS[family]/'results/windows.csv')
    for rid in RUNS.values():
        r = ROOT/'runs'/rid
        inputs.extend(r/n for n in ['config.json', 'status.json', 'results/qualification.csv',
            'results/final_holdout.csv', 'results/final_holdout_predictions.csv',
            'results/run_configuration.json', 'checkpoints/artifacts/prepared_snapshot.pkl',
            'checkpoints/artifacts/prepared_snapshot.json'])
    digests = {str(p.relative_to(ROOT)): sha(p) for p in inputs}
    old = pd.read_csv(ROOT/'reports/paired_predictors_20261011/candidate_metrics.csv')
    rows, daily, coverage, rare, rankrows = [], [], [], [], []
    primary_snapshot = None
    for family, rid in RUNS.items():
        r = ROOT/'runs'/rid
        assert json.loads((r/'status.json').read_text())['status'] == 'completed'
        cfg = json.loads((r/'config.json').read_text())['rstock_config']
        assert cfg['predictor_prefilter_enabled'] is False and cfg['date_feature_regex'] == ''
        p = r/'checkpoints/artifacts/prepared_snapshot.pkl'
        assert sha(p) == json.loads(p.with_suffix('.json').read_text())['sha256']
        snapshot = pickle.loads(p.read_bytes())['prepared']
        if family.endswith('20'):
            if primary_snapshot is None: primary_snapshot = snapshot
            assert snapshot.equals(primary_snapshot)
        q = pd.read_csv(r/'results/qualification.csv')
        h = pd.read_csv(r/'results/final_holdout.csv')
        pred = pd.read_csv(r/'results/final_holdout_predictions.csv', float_precision='round_trip')
        eligible = q[q.Eligible]
        assert q.Set.is_unique and h.Set.is_unique
        assert set(eligible.Set) == set(h.Set)
        assert set(h.Set) == set(pred.Set)
        assert h.FinalAvailable.all() and not pred.duplicated(['Set', 'Date']).any()
        cov = {'Family': family, 'Run': rid, 'WFPopulation': len(q),
            'WFQualified': len(eligible), 'HoldoutEvaluated': len(h),
            'HoldoutConfirmed': int(h.FinalConfirmed.sum()),
            'TargetsInWF': q.Observation.nunique(), 'TargetsWithHoldout': h.Observation.nunique(),
            'RejectedWithHoldout': len(set(h.Set)-set(eligible.Set)),
            'PredictiveModelType': cfg.get('predictive_model_type','external_only'),
            'XGBDepth': cfg['xgb_max_depth'], 'XGBEta': cfg['xgb_eta'],
            'XGBRounds': cfg['xgb_rounds'], 'XGBLambda': cfg['xgb_reg_lambda']}
        coverage.append(cov)
        if h.empty: continue
        qi, hi = q.set_index('Set'), h.set_index('Set')
        groups = dict(tuple(pred.groupby('Set', sort=True)))
        for key, g in groups.items():
            g = g.sort_values('Date'); hh, qq = hi.loc[key], qi.loc[key]
            syms = json.loads(key); target = syms[0]
            assert g.Observation.eq(target).all()
            source = syms if cfg.get('predictive_model_type','external_only') in ['target_only','target_and_external'] else syms[1:]
            features = [f'{s}_intraday_J-{lag}' for s in source for lag in range(1,cfg['lag_depth']+1)]
            labels = [f'{target}.intraday_target', f'{target}.intraday_down_target']
            data = snapshot[[*features,*labels]].dropna()
            train = data.loc[hh.FinalTrainStart:hh.FinalTrainEnd]
            test = data.loc[hh.FinalTestStart:hh.FinalTestEnd]
            assert len(train)==hh.FinalTrainObservations and len(test)==hh.FinalTestObservations==63
            assert str(train.index.max().date()) < g.Date.min()
            assert g.Date.tolist()==[str(x.date()) for x in test.index]
            assert hh.FinalTrainEnd == '2026-04-02' and hh.FinalTestStart == '2026-04-06' and hh.FinalTestEnd == '2026-07-06'
            assert np.isclose(_roc_auc(g.UpTarget.to_numpy(),g.UpProbability.to_numpy()),hh.FinalUpROCAUC,atol=1e-10)
            assert bool(hh.FinalConfirmed)==(hh.FinalUpROCAUC>cfg['final_confirmation_min_auc'])
            for direction, label in zip(['Up','Down'],labels):
                y = test[label].to_numpy(dtype=int)
                assert np.array_equal(y,g[direction+'Target'].to_numpy())
                probs = g[direction+'Probability'].to_numpy()
                base = float(train[label].mean())
                ll, br = probability_losses(y, probs)
                ll0, br0 = probability_losses(y,np.full(len(y),base))
                auc = _roc_auc(y,probs)
                signal = g[direction+'Prediction'].to_numpy(dtype=bool)
                returns = g.IntradayReturn.to_numpy()*(1 if direction=='Up' else -1)
                row = {'Family':family, 'Run':rid,'Direction':direction,'Observation':target,'Set':key,
                    'TrainStart':hh.FinalTrainStart,'TrainEnd':hh.FinalTrainEnd,'TrainN':len(train),
                    'TestStart':g.Date.min(),'TestEnd':g.Date.max(),'N':len(y),
                    'WFMedianAUC':qq.ROCAUCMedian,'WFRank':qq.EligibleRank,
                    'ConfirmedOnThisHoldout':bool(hh.FinalConfirmed),
                    'TrainPrevalence':base,'TestPrevalence':float(y.mean()),
                    'LogLoss':float(ll.mean()),'ConstantLogLoss':float(ll0.mean()),
                    'DeltaLogLoss':float((ll-ll0).mean()),'Brier':float(br.mean()),
                    'ConstantBrier':float(br0.mean()),'DeltaBrier':float((br-br0).mean()),
                    'AUC':auc,'NativeSignals':int(signal.sum()),
                    'NativeSignalPrecision':float(y[signal].mean()) if signal.any() else np.nan,
                    'NativeSignalReturn':float(returns[signal].mean()) if signal.any() else np.nan}
                rows.append(row)
                for date,j in zip(g.Date,range(len(y))):
                    daily.append({'Family':family,'Direction':direction,'Observation':target,'Set':key,
                        'Date':date,'DeltaLogLoss':float(ll[j]-ll0[j]),'DeltaBrier':float(br[j]-br0[j])})
        subset = pd.DataFrame([x for x in rows if x['Family']==family and x['Direction']=='Up'])
        rankrows.append({'Family':family,'Candidates':len(subset),
            'SpearmanWFMedianAUCHoldoutAUC':spearman(subset.WFMedianAUC,subset.AUC),
            'SpearmanWFRankHoldoutAUC':spearman(subset.WFRank,subset.AUC),
            'SpearmanWFMedianAUCHoldoutLossEdge':spearman(subset.WFMedianAUC,-subset.DeltaLogLoss),
            'WithinTargetMeanSpearmanWFMedianAUCHoldoutAUC':np.nanmean([spearman(x.WFMedianAUC,x.AUC) for _,x in subset.groupby('Observation') if len(x)>=3]) if any(len(x)>=3 for _,x in subset.groupby('Observation')) else np.nan})
        if family in ['external20','mixed20']:
            oldfamily=family[:-2]
            o=old[(old.Family==oldfamily)&(old.Direction=='Up')].copy()
            base=old[(old.Family=='constant')&(old.Direction=='Up')].set_index('Observation').LogLoss
            o['BetterThanConstant']=o.LogLoss<o.Observation.map(base)
            winners=set(o[o.BetterThanConstant].Set); selected=set(eligible.Set)
            historical_winner_selected=subset[subset.Set.isin(winners)]
            rare.append({'Family':family,'RetrospectiveDevWinners':len(winners),
                'WFSelectedAmongThose':len(winners&selected),
                'HoldoutLossWinnersAmongSelectedDevWinners':int((historical_winner_selected.DeltaLogLoss<0).sum()),
                'Warning':'retrospective dev winner subset is diagnostic only, never the selected universe'})
    metrics=pd.DataFrame(rows); day=pd.DataFrame(daily)
    target=metrics.groupby(['Family','Direction','Observation'],as_index=False).agg(
        Candidates=('Set','size'),LogLoss=('LogLoss','mean'),ConstantLogLoss=('ConstantLogLoss','mean'),
        DeltaLogLoss=('DeltaLogLoss','mean'),Brier=('Brier','mean'),ConstantBrier=('ConstantBrier','mean'),
        DeltaBrier=('DeltaBrier','mean'),AUC=('AUC','mean'))
    overall=target.groupby(['Family','Direction'],as_index=False).agg(Targets=('Observation','size'),
        LogLoss=('LogLoss','mean'),ConstantLogLoss=('ConstantLogLoss','mean'),DeltaLogLoss=('DeltaLogLoss','mean'),
        Brier=('Brier','mean'),ConstantBrier=('ConstantBrier','mean'),DeltaBrier=('DeltaBrier','mean'),AUC=('AUC','mean'))
    daily_target=day.groupby(['Family','Direction','Observation','Date'],as_index=False)[['DeltaLogLoss','DeltaBrier']].mean()
    inference=[]
    for (family,direction),g in daily_target.groupby(['Family','Direction']):
        sequence=g.groupby('Date').DeltaLogLoss.mean().sort_index().to_numpy(); n=len(sequence)
        assert n==63
        for block in [5,10]:
            rng=np.random.default_rng(20261011+block)
            starts=rng.integers(n,size=(2000,(n+block-1)//block))
            indices=((starts[:,:,None]+np.arange(block))%n).reshape(2000,-1)[:,:n]
            draws=sequence[indices].mean(axis=1)
            low,high=np.quantile(draws,[.025,.975])
            inference.append({'Family':family,'Direction':direction,'Dates':n,'BlockSessions':block,
                'ApproxBlocks':n/block,'Draws':2000,'MeanDeltaLogLoss':float(sequence.mean()),
                'ExploratoryPercentile95Low':float(low),'ExploratoryPercentile95High':float(high)})
    counts=[]
    for (f,d),g in metrics.groupby(['Family','Direction']):
        t=target[(target.Family==f)&(target.Direction==d)]
        counts.append({'Family':f,'Direction':d,'Candidates':len(g),'CandidatesBeatConstantLL':int((g.DeltaLogLoss<0).sum()),
            'CandidatesAUCAboveHalf':int((g.AUC>.5).sum()),'Targets':len(t),
            'TargetsBeatConstantLL':int((t.DeltaLogLoss<0).sum()),
            'NativeSignals':int(g.NativeSignals.sum()),
            'QualifiedConfirmed':int(g.ConfirmedOnThisHoldout.sum())})
    partitions=metrics.groupby(['Family','Direction','ConfirmedOnThisHoldout'],as_index=False)[['DeltaLogLoss','DeltaBrier','AUC']].mean()
    partitions['Weighting']='equal candidate; selected on this same holdout; descriptive only'
    # Same holdout used to confirm these candidates: this partition is descriptive only.
    for name,frame in [('candidate_holdout_metrics',metrics),('target_holdout_metrics',target),
        ('overall_holdout_metrics',overall),('daily_target_loss_edges',daily_target),
        ('coverage',pd.DataFrame(coverage)),('exploratory_block_intervals',pd.DataFrame(inference)),
        ('rank_diagnostics',pd.DataFrame(rankrows)),('confirmed_partition_descriptive',partitions)]:
        frame.to_csv(OUT/(name+'.csv'),index=False)
    # Earliest prefix satisfying the saved minimum-window rule, not a tuned split.
    # All later scores already exist for selected and rejected candidates.
    wm=pd.read_csv(ROOT/'reports/paired_predictors_20261011/window_metrics.csv')
    wm=wm[wm.Direction=='Up']
    basewm=wm[wm.Family=='constant'].set_index(['Observation','Window'])
    prefixrows=[]; prefixsummary=[]; prefixtarget=[]
    for family in ['external20','mixed20']:
        cfg=json.loads((ROOT/'runs'/RUNS[family]/'config.json').read_text())['rstock_config']
        original=pd.read_csv(ROOT/'runs'/RUNS[family]/'results/qualification.csv').set_index('Set')
        windows=pd.read_csv(ROOT/'runs'/RUNS[family]/'results/windows.csv')
        n=cfg['qualification_min_windows']; assert n==4
        def eligible(block):
            auc=block.UpROCAUC
            return (block.Window.nunique()>=n and auc.notna().sum()>=n
                and auc.median()>=cfg['qualification_min_median_auc']
                and (auc>.5).sum()/len(block)>=cfg['qualification_min_pct_windows_above_random']
                and auc.min()>=cfg['qualification_min_worst_window_auc']
                and block.UpPositiveOutcomes.sum()>=cfg['qualification_min_positive_observations']
                and auc.std(ddof=0)<=cfg['qualification_max_auc_std'])
        score=wm[wm.Family==family[:-2]].set_index(['Set','Window'])
        for key, block in windows.groupby('Set'):
            assert eligible(block)==bool(original.loc[key,'Eligible'])
            prefix=block[block.Window<=n]; future=block[block.Window>n]; chosen=eligible(prefix)
            targetname=block.Observation.iloc[0]
            assert prefix.TestEnd.max()=='2025-07-15' and future.TestStart.min()=='2025-07-16'
            assert future.TestEnd.max()=='2026-04-02'
            own=score.loc[[(key,int(x)) for x in future.Window]]
            baseline=basewm.loc[[(targetname,int(x)) for x in future.Window]]
            weights=own.N.to_numpy();assert weights.sum()==181
            prefixrows.append({'Family':family,'Observation':targetname,'Set':key,
                'SelectedAfterFirst4':chosen,'PrefixFirstTest':prefix.TestStart.min(),
                'SelectionAsOf':prefix.TestEnd.max(),'EvaluationStart':future.TestStart.min(),
                'EvaluationEnd':future.TestEnd.max(),'EvaluationDates':int(weights.sum()),
                'PrefixMedianAUC':float(prefix.UpROCAUC.median()),
                'LaterLogLoss':float(np.average(own.LogLoss,weights=weights)),
                'LaterConstantLogLoss':float(np.average(baseline.LogLoss,weights=weights)),
                'LaterDeltaLogLoss':float(np.average(own.LogLoss.to_numpy()-baseline.LogLoss.to_numpy(),weights=weights)),
                'LaterDeltaBrier':float(np.average(own.Brier.to_numpy()-baseline.Brier.to_numpy(),weights=weights)),
                'LaterMeanWindowAUC':float(np.average(own.AUC,weights=weights))})
        pf=pd.DataFrame([x for x in prefixrows if x['Family']==family])
        targetpf=pf.groupby(['Observation','SelectedAfterFirst4'],as_index=False).agg(
            Candidates=('Set','size'),LaterLogLoss=('LaterLogLoss','mean'),
            LaterConstantLogLoss=('LaterConstantLogLoss','mean'),LaterDeltaLogLoss=('LaterDeltaLogLoss','mean'),
            LaterDeltaBrier=('LaterDeltaBrier','mean'),LaterMeanWindowAUC=('LaterMeanWindowAUC','mean'))
        targetpf['Family']=family;prefixtarget.extend(targetpf.to_dict('records'))
        for chosen,g in targetpf.groupby('SelectedAfterFirst4'):
            prefixsummary.append({'Family':family,'SelectedAfterFirst4':bool(chosen),
                'Candidates':int(g.Candidates.sum()),'Targets':len(g),
                'EqualTargetDeltaLogLoss':float(g.LaterDeltaLogLoss.mean()),
                'EqualTargetDeltaBrier':float(g.LaterDeltaBrier.mean()),
                'EqualTargetMeanWindowAUC':float(g.LaterMeanWindowAUC.mean())})
        selectedpf=targetpf[targetpf.SelectedAfterFirst4].set_index('Observation')
        rejectedpf=targetpf[~targetpf.SelectedAfterFirst4].set_index('Observation')
        common=sorted(set(selectedpf.index)&set(rejectedpf.index))
        sametargetpf=pf[pf.Observation.isin(common)]
        samerejected=sametargetpf[~sametargetpf.SelectedAfterFirst4]
        sameselected=sametargetpf[sametargetpf.SelectedAfterFirst4]
        prefixsummary.append({'Family':family,'Comparison':'selected minus rejected on same target/date',
            'CommonTargets':len(common),
            'TargetsWithoutSelectedCandidates':sorted(set(pf.Observation)-set(selectedpf.index)),
            'MatchedTargetAllCandidateDeltaLL':float(sametargetpf.groupby('Observation').LaterDeltaLogLoss.mean().mean()),
            'MatchedTargetRejectedDeltaLL':float(samerejected.groupby('Observation').LaterDeltaLogLoss.mean().mean()),
            'EqualTargetDeltaLLSelectedMinusRejected':float((selectedpf.loc[common,'LaterDeltaLogLoss']-rejectedpf.loc[common,'LaterDeltaLogLoss']).mean()),
            'SelectedCandidatesBeatConstant':int(((pf.SelectedAfterFirst4)&(pf.LaterDeltaLogLoss<0)).sum()),
            'MatchedTargetsRejectedCandidates':len(samerejected),
            'MatchedTargetsRejectedCandidatesBeatConstant':int((samerejected.LaterDeltaLogLoss<0).sum()),
            'SelectedTargetsBeatConstant':int((selectedpf.LaterDeltaLogLoss<0).sum()),
            'AllCandidateSpearmanPrefixMedianAUCLaterMeanWindowAUC':spearman(pf.PrefixMedianAUC,pf.LaterMeanWindowAUC)})
    pd.DataFrame(prefixrows).to_csv(OUT/'prefix_selection_candidates.csv',index=False)
    pd.DataFrame(prefixtarget).to_csv(OUT/'prefix_selection_targets.csv',index=False)
    pd.DataFrame(prefixsummary).to_csv(OUT/'prefix_selection_summary.csv',index=False)
    assert all(sha(ROOT/p)==digest for p,digest in digests.items()),'Historical source changed'
    summary={'question':'Does recorded pre-holdout WF qualification select later probability skill?',
        'holdout':'2026-04-06 to 2026-07-06; 63 sessions','train_ends':'2026-04-02',
        'primary':'20 target families, all WF-qualified candidates; no holdout confirmation filter',
        'supplement':'150-target external-only run, distinct depth2/20-round specification',
        'weighting':'equal target; equal selected candidate within target; equal dates; no prediction ensemble',
        'coverage':coverage,'overall':overall.to_dict('records'),'counts':counts,'rare_dev_winners':rare,
        'rank_diagnostics':rankrows,'exploratory_intervals':inference,
        'prefix_check':prefixsummary,
        'prefix_method':'exact saved qualification criteria on first4 windows (earliest minimum); archived later3windows181dates; no new model or threshold, no prefilter; descriptive consulted-history check',
        'inference_limits':'conditional to archived selected population, consulted dates, only63days; 5/10day circular-block sensitivity, no independent multiple-search correction or prospective validation',
        'rejected_holdout_missing':True,'training_executed':False,'historical_inputs_unchanged':True,
        'input_sha256':digests}
    (OUT/'summary.json').write_text(json.dumps(jsonable(summary),ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
    print(json.dumps(jsonable({k:v for k,v in summary.items() if k!='input_sha256'}),ensure_ascii=False,indent=2),flush=True)

if __name__=='__main__':main()
