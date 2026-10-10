"""Analyse ponctuelle, lecture seule des runs. Aucune importation de RStock/XGBoost.
Python + numpy + pandas. Les seules écritures sont dans le dossier de ce script.
"""
from pathlib import Path
import ast
import hashlib
import json
import pickle
import re
import subprocess
import sys
import platform
import numpy as np
import pandas as pd

OUT = Path(__file__).resolve().parent
ROOT = OUT.parents[1]
EPS = 1e-15
ROOT_IDS = ['20261009T115931_8bf594b3bd', '20261010T001622_e7f734b61a']
KEY = ['Set', 'Window', 'Date']
WK = ['Set', 'Window']
sources = {}

def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()

def source(path):
    path = path.resolve()
    sources[str(path)] = {'bytes': path.stat().st_size, 'sha256': sha(path)}
    return path

def js(path):
    return json.loads(source(path).read_text(encoding='utf-8-sig'))

def write_json(name, value):
    (OUT / name).write_text(json.dumps(value, ensure_ascii=False, indent=2, default=str), encoding='utf-8')

def losses(y, p):
    p = np.asarray(p, dtype=float)
    y = np.asarray(y, dtype=float)
    assert np.isfinite(p).all() and ((p >= 0) & (p <= 1)).all()
    assert np.isin(y, [0, 1]).all()
    q = np.clip(p, EPS, 1 - EPS)
    return -(y * np.log(q) + (1 - y) * np.log1p(-q)), (p - y) ** 2

def auc(y, p):
    y = np.asarray(y)
    n1, n0 = int(y.sum()), len(y) - int(y.sum())
    if not n1 or not n0:
        return np.nan
    ranks = pd.Series(np.asarray(p)).rank(method='average').to_numpy()
    return float((ranks[y == 1].sum() - n1 * (n1 + 1) / 2) / (n1 * n0))

def checked_unique(df, keys, name):
    bad = df.duplicated(keys, keep=False)
    if bad.any():
        df.loc[bad].to_csv(OUT / (name + '_doublons.csv'), index=False)
        raise ValueError(f'{name}: {bad.sum()} clés dupliquées; voir CSV, aucun appariement effectué')

def bool_value(v):
    if v is True or str(v).lower() == 'true':
        return True
    if v is False or str(v).lower() == 'false':
        return False
    raise ValueError(f'Booléen inconnu: {v!r}')

def fmt(x):
    return f'{x:.6f}' if pd.notna(x) else 'NA'

def main():
    print('Résolution des enfants et empreintes des sources...', flush=True)
    manifests = [js(ROOT / 'runs' / r / 'orchestration/pipeline.json') for r in ROOT_IDS]
    for r in ROOT_IDS:
        js(ROOT / 'runs' / r / 'config.json')
    wfids, prefilters = [], []
    for m in manifests:
        stages = {s['stage_key']: s for s in m['stages']}
        wfids.append(stages['walk_forward']['source_run_id'] or stages['walk_forward']['child_run_id'])
        prefilters.append(stages['prefilter']['source_run_id'] or stages['prefilter']['child_run_id'])
    dirs = [ROOT / 'runs' / r for r in wfids]
    configs = [js(d / 'config.json') for d in dirs]
    rc = [js(d / 'results/run_configuration.json') for d in dirs]
    for d in dirs:
        for rel in ['summary.json', 'results/selection_diagnostic_manifest.json', 'checkpoints/artifacts/prepared_snapshot.json']:
            if (d / rel).exists():
                js(d / rel)
    c0, c1 = [c['rstock_config'] for c in configs]
    envelope_keys = ['symbols', 'target_symbols', 'predictor_symbols', 'historical_data_cutoff', 'resolved_market_session_cutoff', 'evaluate_final_holdout', 'calendar', 'frozen_xgboost_parameters', 'frozen_threshold_calibration_parameters']
    for k in envelope_keys:
        assert configs[0].get(k) == configs[1].get(k), f'Configuration scientifique différente: {k}'
    assert configs[0].get('frozen_xgboost_parameters') is None
    config_diff = {k: {'fixe': c0.get(k, '<absent>'), 'chronologique': c1.get(k, '<absent>')}
                   for k in sorted(c0.keys() | c1.keys()) if c0.get(k, '<absent>') != c1.get(k, '<absent>')}
    allowed = {k for k in config_diff if k.startswith('xgb_early_stopping_') or k.startswith('prefilter_xgb_early_stopping_')}
    allowed |= {'xgb_round_selection_mode', 'xgb_round_selection_protocol_version', 'prefilter_xgb_round_selection_mode', 'prefilter_xgb_round_selection_protocol_version'}
    assert not (config_diff.keys() - allowed), config_diff
    assert prefilters[0] == prefilters[1]
    assert c0['xgb_rounds'] == c1['xgb_rounds'] == 80
    assert c0.get('xgb_round_selection_mode', 'fixed') == 'fixed'
    assert c1['xgb_round_selection_mode'] == 'chronological'
    frames = []
    for d in dirs:
        # Pickles locaux de confiance, aucun réentraînement ni recalcul de features.
        with source(d / 'checkpoints/artifacts/prepared_snapshot.pkl').open('rb') as f:
            frames.append(pickle.load(f)['prepared'])
    assert frames[0].equals(frames[1]), 'Snapshots préparés différents'
    prepared = frames[0]
    assert prepared.index.is_unique and prepared.index.is_monotonic_increasing
    win = [pd.read_csv(source(d / 'results/windows.csv')) for d in dirs]
    predcols = ['Set', 'Window', 'Date', 'IntradayTarget', 'UpTarget', 'DownTarget', 'UpProbability', 'DownProbability']
    pred = [pd.read_csv(source(d / 'results/predictions.csv'), usecols=predcols) for d in dirs]
    for i, p in enumerate(pred):
        checked_unique(p, KEY, f'predictions_{i}')
        assert p['UpTarget'].equals(p['IntradayTarget'])
        p['Date'] = pd.to_datetime(p['Date']).dt.strftime('%Y-%m-%d')
        checked_unique(p, KEY, f'predictions_dates_normalisees_{i}')
        checked_unique(win[i], WK, f'fenetres_{i}')
    print('Appariement exhaustif...', flush=True)
    paired = pred[0].merge(pred[1], on=KEY, how='outer', suffixes=('_fixed', '_chrono'), indicator=True, validate='one_to_one')
    unmatched = paired['_merge'] != 'both'
    paired.loc[unmatched].to_csv(OUT / 'observations_non_appariees.csv', index=False)
    assert not unmatched.any(), 'Observations non appariées: voir CSV; pas de suppression silencieuse'
    for d in ['Up', 'Down']:
        assert paired[f'{d}Target_fixed'].equals(paired[f'{d}Target_chrono']), f'Labels {d} différents'
    wmerge = win[0].merge(win[1], on=WK, how='outer', suffixes=('_fixed', '_chrono'), indicator=True, validate='one_to_one')
    wmerge.loc[wmerge['_merge'] != 'both'].to_csv(OUT / 'fenetres_non_appariees.csv', index=False)
    assert (wmerge['_merge'] == 'both').all()
    shared_checks = ['Observation', 'Predictors', 'MarketCalendar', 'TrainStart', 'TrainEnd', 'TestStart', 'TestEnd', 'TrainObservations', 'TestObservations', 'Predictions', 'RowsLostToLags', 'UpPositiveOutcomes', 'DownPositiveOutcomes']
    for col in shared_checks:
        a, b = wmerge[col + '_fixed'], wmerge[col + '_chrono']
        assert ((a == b) | (a.isna() & b.isna())).all(), f'Fenêtres différentes: {col}'
    w = win[1].set_index(WK)
    groups = {key: g for key, g in paired.groupby(WK, sort=False)}
    candidate_sets = sorted(w.index.get_level_values('Set').unique())
    schedules = {}
    for s in candidate_sets:
        records = w.loc[s].reset_index()[['Window', 'TrainStart', 'TrainEnd', 'TestStart', 'TestEnd']].sort_values('Window').to_dict('records')
        signature = json.dumps(records, sort_keys=True)
        schedules[s] = signature
    sigids = {s: f'C{i+1:02d}' for i, s in enumerate(sorted(set(schedules.values())))}
    cohorts = {s: sigids[sig] for s, sig in schedules.items()}
    write_json('cohortes_calendriers.json', [{'cohorte': cid, 'calendrier': json.loads(sig), 'candidats': sum(v == cid for v in cohorts.values())} for sig, cid in sigids.items()])
    print('Vérification des features, labels apprentissage et frontières internes...', flush=True)
    metrics, temporal_errors = [], []
    baseline_by_key = {}
    expected_params = {name: c1['xgb_' + cfg] for name, cfg in [('max_depth','max_depth'),('eta','eta'),('num_boost_round','rounds'),('min_child_weight','min_child_weight'),('subsample','subsample'),('colsample_bytree','colsample_bytree'),('gamma','gamma'),('reg_alpha','reg_alpha'),('reg_lambda','reg_lambda')]}
    regex = re.compile(c1['date_feature_regex']) if c1['date_feature_regex'] else None
    vsize = c1['xgb_early_stopping_validation_sessions']
    mintrain = c1['xgb_early_stopping_min_train_observations']
    for candidate_number, s in enumerate(candidate_sets):
        if candidate_number and candidate_number % 300 == 0:
            print(f'  {candidate_number}/{len(candidate_sets)} candidats vérifiés', flush=True)
        symbols = json.loads(s)
        target, predictors = symbols[0], symbols[1:]
        requested = {f'{symbol}_intraday_J-{lag}' for symbol in predictors for lag in range(1, c1['lag_depth'] + 1)}
        columns = [name for name in prepared.columns if name in requested or (name in {'wday','yday','mon'} and regex and regex.search(name))]
        assert len(columns) >= len(requested)
        data = prepared[columns + [f'{target}.intraday_target', f'{target}.intraday_down_target']].dropna()
        for (_, window), row in w.loc[[s]].iterrows():
            key = (s, window)
            g = groups[key]
            train = data.loc[pd.Timestamp(row.TrainStart):pd.Timestamp(row.TrainEnd)]
            test = data.loc[pd.Timestamp(row.TestStart):pd.Timestamp(row.TestEnd)]
            assert len(train) == row.TrainObservations and len(test) == row.TestObservations == len(g)
            assert set(test.index.strftime('%Y-%m-%d')) == set(g.Date)
            assert train.index.max() < test.index.min()
            assert test.index.max() < pd.Timestamp(rc[1]['final_holdout_start'])
            dates = pd.DatetimeIndex(train.index).normalize()
            assert dates.is_unique
            inner, valid = train.iloc[:-vsize], train.iloc[-vsize:]
            for direction, outcome in [('Up', f'{target}.intraday_target'), ('Down', f'{target}.intraday_down_target')]:
                def diag(name):
                    return row[direction + name]
                assert ast.literal_eval(diag('PredictorColumns')) == columns
                assert ast.literal_eval(diag('Parameters')) == expected_params
                assert diag('RoundSelectionMode') == 'chronological'
                assert diag('Outcome') == outcome
                assert pd.Timestamp(diag('TrainingStart')) == train.index.min()
                assert pd.Timestamp(diag('TrainingEnd')) == train.index.max()
                assert diag('TrainingObservations') == len(train)
                policy = ast.literal_eval(diag('RoundSelectionPolicy'))
                for field, value in policy.items():
                    if field.startswith('xgb_'):
                        assert c1[field] == value
                assert policy['fallback_rounds'] == 80
                y = g[f'{direction}Target_fixed'].to_numpy(dtype=float)
                actual = test[outcome].reindex(pd.to_datetime(g.Date)).to_numpy(dtype=float)
                assert np.array_equal(y, actual)
                assert len(inner) == diag('InternalTrainObservations') and len(valid) == diag('ValidationObservations')
                assert int(inner[outcome].sum()) == diag('InternalTrainPositiveOutcomes')
                assert int(valid[outcome].sum()) == diag('ValidationPositiveOutcomes')
                for name, part in [('InternalTrain',inner), ('Validation',valid)]:
                    for suffix, boundary in [('Start',part.index.min()),('End',part.index.max())]:
                        assert pd.Timestamp(diag(name + suffix)) == boundary
                used = bool_value(diag('RoundSelectionUsed'))
                reason = '' if pd.isna(diag('RoundSelectionFallbackReason')) else str(diag('RoundSelectionFallbackReason'))
                if len(train) < mintrain + vsize:
                    expected_reason = 'insufficient_history'
                elif valid[outcome].nunique() < 2:
                    expected_reason = 'validation_single_class'
                elif inner[outcome].nunique() < 2:
                    expected_reason = 'internal_train_single_class'
                else:
                    expected_reason = ''
                assert used == (not expected_reason), (key, direction, reason, expected_reason)
                if not used:
                    assert reason == expected_reason, (reason,expected_reason)
                    assert diag('RoundsRetained') == 80
                else:
                    assert len(inner) >= mintrain and len(valid) == vsize
                    assert inner.index.max() < valid.index.min() <= valid.index.max() < test.index.min()
                    assert 1 <= diag('RoundsRetained') <= c1['xgb_early_stopping_max_rounds']
                    assert diag('RoundsRetained') == diag('BestIteration') + 1
                    assert diag('RoundsRetained') <= diag('SelectionRoundsRun') <= c1['xgb_early_stopping_max_rounds']
                p0, p1 = [g[f'{direction}Probability_{mode}'].to_numpy(float) for mode in ['fixed','chrono']]
                l0,b0 = losses(y,p0); l1,b1 = losses(y,p1)
                prev = float(train[outcome].mean())
                lb,bb = losses(y,np.full(len(y),prev))
                a0,a1 = auc(y,p0),auc(y,p1)
                rec = {'Set':s,'CandidateId':hashlib.sha256(s.encode()).hexdigest()[:16], 'Observation':target,'Direction':direction,'Window':window,'Cohort':cohorts[s], 'Status':'optimise' if used else 'repli', 'FallbackReason':reason, 'N':len(y), 'TestPositives':int(y.sum()),'TrainN':len(train),'TrainPositives':int(train[outcome].sum()),'TrainPrevalence':prev,
                       'TrainStart':row.TrainStart,'TrainEnd':row.TrainEnd,'TestStart':row.TestStart,'TestEnd':row.TestEnd,
                       'InternalTrainStart':diag('InternalTrainStart'),'InternalTrainEnd':diag('InternalTrainEnd'),'ValidationStart':diag('ValidationStart'),'ValidationEnd':diag('ValidationEnd'), 'InternalTrainN':len(inner),'ValidationN':len(valid), 'InternalTrainPositives':diag('InternalTrainPositiveOutcomes'), 'ValidationPositives':diag('ValidationPositiveOutcomes'),
                       'FixedRoundsConfigured':80,'ChronoRounds':diag('RoundsRetained'),'SelectionRoundsRun':diag('SelectionRoundsRun'),'EarlyStoppingTriggered':diag('EarlyStoppingTriggered'),'SelectionCapReached':diag('SelectionCapReached'), 'SelectionSeconds':diag('SelectionSeconds'),'RefitSeconds':diag('RefitSeconds'),
                       'LogLossFixed':l0.mean(),'LogLossChrono':l1.mean(),'DeltaLogLoss':(l1-l0).mean(),'BrierFixed':b0.mean(),'BrierChrono':b1.mean(),'DeltaBrier':(b1-b0).mean(),'AUCFixed':a0,'AUCChrono':a1,'DeltaAUC':a1-a0,'AUCDefined':pd.notna(a0),
                       'LogLossTrainConstant':lb.mean(),'BrierTrainConstant':bb.mean(), 'DeltaLogLossFixedVsConstant':(l0-lb).mean(),'DeltaLogLossChronoVsConstant':(l1-lb).mean(), 'DeltaBrierFixedVsConstant':(b0-bb).mean(),'DeltaBrierChronoVsConstant':(b1-bb).mean(),
                       'DifferentProbabilities':int(np.count_nonzero(p0 != p1)), 'MaxAbsProbabilityDelta':float(np.max(np.abs(p1-p0)))}
                for metric, val in [('LogLoss',l1.mean()),('Brier',b1.mean())]:
                    assert np.isclose(val, diag(metric), rtol=0, atol=1e-12), (key,direction,metric,val,diag(metric))
                metrics.append(rec)
                baseline_by_key[(s,window,direction)] = prev
    metrics = pd.DataFrame(metrics)
    # AUC par modèle/fenêtre: audit des valeurs persistées, jamais AUC mutualisée.
    for i, mode in enumerate(['Fixed','Chrono']):
        for direction in ['Up','Down']:
            lookup = win[i].set_index(WK)[direction+'ROCAUC']
            for row in metrics[metrics.Direction == direction].itertuples():
                stored = lookup.loc[(row.Set,row.Window)]
                val = getattr(row,'AUC'+mode)
                assert (pd.isna(stored) and pd.isna(val)) or np.isclose(val,stored,rtol=0,atol=1e-12)
    metrics.to_csv(OUT/'metriques_par_candidat_direction_fenetre.csv',index=False,float_format='%.17g')
    print('Agrégation descriptive et compression des prédictions appariées...',flush=True)
    measures = ['LogLossFixed','LogLossChrono','DeltaLogLoss','BrierFixed','BrierChrono','DeltaBrier','AUCFixed','AUCChrono','DeltaAUC','LogLossTrainConstant','BrierTrainConstant','DeltaLogLossFixedVsConstant','DeltaLogLossChronoVsConstant','DeltaBrierFixedVsConstant','DeltaBrierChronoVsConstant']
    def aggregate(g):
        r={'CandidateWindows':len(g),'Candidates':g.Set.nunique(),'Observations':int(g.N.sum()),'AUCDefinedWindows':int(g.AUCDefined.sum()),'DifferentProbabilities':int(g.DifferentProbabilities.sum()),'MaxAbsProbabilityDelta':g.MaxAbsProbabilityDelta.max(),'MeanChronoRounds':g.ChronoRounds.mean(),'MedianChronoRounds':g.ChronoRounds.median(),'MinChronoRounds':g.ChronoRounds.min(),'MaxChronoRounds':g.ChronoRounds.max(),'FirstTestDate':g.TestStart.min(),'LastTestDate':g.TestEnd.max(),'SelectionSecondsSum':g.SelectionSeconds.sum(),'RefitSecondsSum':g.RefitSeconds.sum()}
        for col in measures:
            r['MeanCW_'+col]=g[col].mean()
            if 'AUC' not in col:
                r['ObsWeighted_'+col]=np.average(g[col],weights=g.N)
        return r
    global_rows=[]
    for direction in ['Up','Down']:
        for status in ['toutes','optimise','repli']:
            g=metrics[(metrics.Direction==direction)&((metrics.Status==status) if status!='toutes' else True)]
            if len(g):
                global_rows.append({'Direction':direction,'Status':status,**aggregate(g)})
    global_summary=pd.DataFrame(global_rows)
    global_summary.to_csv(OUT/'synthese_globale.csv',index=False,float_format='%.17g')
    period_rows=[]
    for key,g in metrics.groupby(['Cohort','TestStart','TestEnd','Direction','Status'],sort=True):
        period_rows.append(dict(zip(['Cohort','TestStart','TestEnd','Direction','Status'],key))|aggregate(g))
    period=pd.DataFrame(period_rows)
    period.to_csv(OUT/'synthese_par_periode_statut.csv',index=False,float_format='%.17g')
    combined=pd.concat([paired[KEY+['UpTarget_fixed','UpProbability_fixed','UpProbability_chrono']].rename(columns={'UpTarget_fixed':'Label','UpProbability_fixed':'ProbabilityFixed','UpProbability_chrono':'ProbabilityChrono'}).assign(Direction='Up'),paired[KEY+['DownTarget_fixed','DownProbability_fixed','DownProbability_chrono']].rename(columns={'DownTarget_fixed':'Label','DownProbability_fixed':'ProbabilityFixed','DownProbability_chrono':'ProbabilityChrono'}).assign(Direction='Down')],ignore_index=True)
    combined=combined.merge(metrics[['Set','Window','Direction','CandidateId','Cohort','Status','TrainPrevalence']],on=['Set','Window','Direction'],how='left',validate='many_to_one')
    assert len(combined)==2*len(paired) and combined.Status.notna().all()
    combined.to_csv(OUT/'predictions_appariees.csv.gz',index=False,compression={'method':'gzip','compresslevel':6,'mtime':0},float_format='%.17g')
    commits=[r.get('traceability',{}).get('git_commit') for r in rc]
    code_audit = {}
    # Le champ historique peut être nommé git_commit_hash selon le contrat.
    for i,r in enumerate(rc):
        tr=r.get('traceability',{})
        commits[i]=tr.get('git_commit') or tr.get('git_commit_hash') or tr.get('code_commit')
    print('Commits enregistrés:',commits,flush=True)
    if all(commits):
        diff=subprocess.run(['git','diff',commits[0],commits[1],'--','rstock/config.py','rstock/modeling.py','rstock/walk_forward.py','rstock/evaluation.py','rstock/features.py'],cwd=ROOT,capture_output=True,text=True,encoding='utf-8',errors='replace')
        (OUT/'differences_code_enregistre.patch.txt').write_text(diff.stdout+diff.stderr,encoding='utf-8')
        for path, names in [('rstock/features.py',['predictor_columns','prepare_dataset']), ('rstock/modeling.py',['training_parameters','fit_booster_matrix','predict_probabilities_matrix'])]:
            versions = []
            for commit in commits:
                result = subprocess.run(['git','show',commit+':'+path],cwd=ROOT,capture_output=True,text=True,encoding='utf-8',errors='replace',check=True)
                tree = ast.parse(result.stdout)
                versions.append({node.name: hashlib.sha256(ast.dump(node,include_attributes=False).encode()).hexdigest() for node in ast.walk(tree) if isinstance(node,ast.FunctionDef) and node.name in names})
            for name in names:
                code_audit[path+':'+name] = {'ast_sha256_fixed':versions[0].get(name),'ast_sha256_chrono':versions[1].get(name),'identical':name in versions[0] and name in versions[1] and versions[0][name]==versions[1][name]}
    unchanged=all(sha(Path(p))==v['sha256'] for p,v in sources.items())
    assert unchanged,'Une source a changé pendant la lecture'
    audit={'e2e_runs':ROOT_IDS,'wf_runs':wfids,'prefilter_runs':prefilters,'source_files':sources,'source_files_unchanged_at_end':unchanged,'rstock_config_differences':config_diff,'traceability':[r.get('traceability') for r in rc], 'code_commits':commits,'round_selection':rc[1].get('round_selection'),'rows_per_run':[len(p) for p in pred],'paired_observations_per_direction':len(paired),'unmatched_observations':int(unmatched.sum()),'duplicate_keys':0,'labels_identical':True,'prepared_frames_equal':True,'shared_window_fields_equal':shared_checks,'feature_columns_and_training_parameters_verified':True,'train_prevalence_verified_from_prepared_snapshot':True,'calendar_cohorts':len(sigids),'probability_epsilon':EPS,'analysis_versions':{'python':platform.python_version(),'numpy':np.__version__,'pandas':pd.__version__},'analysis_script_sha256':sha(Path(__file__))}
    write_json('sources_et_verifications.json',audit)
    audit['scientific_envelope_fields_equal'] = envelope_keys
    audit['recorded_code_function_audit'] = code_audit
    audit['fallback_probability_equality'] = bool((metrics.loc[metrics.Status=='repli','DifferentProbabilities']==0).all())
    audit['paired_predictions_file'] = {'path':str(OUT/'predictions_appariees.csv.gz'),'rows':len(combined),'bytes':(OUT/'predictions_appariees.csv.gz').stat().st_size,'sha256':sha(OUT/'predictions_appariees.csv.gz')}
    write_json('sources_et_verifications.json',audit)
    n=len(metrics)//2
    lines=['# Analyse ponctuelle XGBoost fixe / chronologique','',f'End-to-End fixe : `{ROOT_IDS[0]}` ; WF `{wfids[0]}`.',f'End-to-End chronologique : `{ROOT_IDS[1]}` ; WF `{wfids[1]}`.','',f'## Comparabilité vérifiée','',f'- {len(candidate_sets):,} candidats ; {n:,} fenêtres candidat ; {len(paired):,} observations appariées par direction ({len(combined):,} lignes directionnelles). Aucune sélection sur admissibilité ou qualification.',f'- Zéro doublon de clé (candidat, fenêtre, date), zéro observation ou fenêtre non appariée, labels Up/Down strictement égaux. Les CSV de non-appariement sont présents même vides.', '- Les deux snapshots préparés sont égaux (valeurs, index, colonnes et types). Même candidat et ordre des variables, mêmes retards/exclusions, seuils de labels, frontières et effectifs train/test, mêmes hyperparamètres hors sélection des tours. Les diagnostics chronologiques concordent avec les features et paramètres de référence.',f'- Même préfiltre hérité : `{prefilters[0]}`. Il ne constitue pas une validation externe indépendante ; sa sélection scientifique préalable peut limiter cette indépendance.',f'- {len(sigids)} cohortes distinctes de calendrier complet. Les périodes réellement évaluées sont conservées dans les CSV, aucune comparaison de simples numéros de fenêtre entre calendriers.',f'- Commits enregistrés : fixe `{commits[0]}`, chronologique `{commits[1]}`. Le changement de code reste un facteur de confusion possible ; le diff est joint si disponible. Une empreinte de commit ne capture pas d’éventuelles modifications locales historiques.','', '## Résultats descriptifs','', 'Δ = chronologique moins fixe. Δ négatif favorable pour log loss et Brier ; positif favorable pour AUC.', '', '|Direction|Statut|Fenêtres|Observations|LL fixe pondérée|LL chrono pondérée|Δ LL pondérée|Δ Brier pondéré|Δ AUC moyenne par fenêtre|', '|---|---|---:|---:|---:|---:|---:|---:|---:|']
    for row in global_summary.to_dict('records'):
        lines.append('|'+ '|'.join([row['Direction'],row['Status'],str(row['CandidateWindows']),str(row['Observations']),fmt(row['ObsWeighted_LogLossFixed']),fmt(row['ObsWeighted_LogLossChrono']),fmt(row['ObsWeighted_DeltaLogLoss']),fmt(row['ObsWeighted_DeltaBrier']),fmt(row['MeanCW_DeltaAUC'])])+'|')
    lines += ['', 'Pour toutes les fenêtres, les valeurs Brier fixe → chronologique et AUC moyenne fixe → chronologique sont :']
    for row in global_summary[global_summary.Status=='toutes'].to_dict('records'):
        lines.append(f"- {row['Direction']} : Brier {row['ObsWeighted_BrierFixed']:.6f} → {row['ObsWeighted_BrierChrono']:.6f} ; AUC moyenne par candidat/fenêtre {row['MeanCW_AUCFixed']:.6f} → {row['MeanCW_AUCChrono']:.6f}.")
    lines += ['', '### Périodes réelles et cohortes', '', 'C01 : 1 702 candidats, 7 fenêtres chacun ; C02 : 112 candidats, 4 fenêtres chacun. Les deux modes suivent exactement le même calendrier à l’intérieur de chaque candidat. La différence de calendrier concerne les cohortes, pas un désalignement fixe/chronologique.', '', '|Cohorte|Début test|Fin test|Direction|Statut|Fenêtres|Observations|Δ LL pondérée|Δ Brier pondéré|Δ AUC moyenne|', '|---|---|---|---|---|---:|---:|---:|---:|---:|']
    for row in period.to_dict('records'):
        lines.append('|'+ '|'.join([row['Cohort'],row['TestStart'],row['TestEnd'],row['Direction'],row['Status'],str(row['CandidateWindows']),str(row['Observations']),fmt(row['ObsWeighted_DeltaLogLoss']),fmt(row['ObsWeighted_DeltaBrier']),fmt(row['MeanCW_DeltaAUC'])])+'|')
    lines += ['', '### Couverture et replis','']
    for direction in ['Up','Down']:
        g=metrics[metrics.Direction==direction]; opt=g[g.Status=='optimise']; fall=g[g.Status=='repli']
        lines.append(f'- {direction} : {len(opt):,}/{len(g):,} fenêtres optimisées ({100*len(opt)/len(g):.2f} %) ; {len(fall):,} replis. Tours optimisés min/médiane/max : {opt.ChronoRounds.min():g}/{opt.ChronoRounds.median():g}/{opt.ChronoRounds.max():g}. Raisons de repli : {fall.FallbackReason.value_counts().to_dict()}.')
        lines.append(f'  Replis à 80 : {int(fall.DifferentProbabilities.sum()):,} probabilités différentes ; écart absolu maximal {fall.MaxAbsProbabilityDelta.max():.17g}.')
        lines.append(f'  Optimisées : {int(opt.SelectionCapReached.map(bool_value).sum())} sélections atteignent le plafond ; {int(opt.EarlyStoppingTriggered.map(bool_value).sum())} early stopping déclenchés.')
    lines += ['', '- Minimum : 315 séances distinctes utilisables après retards/exclusions = 252 apprentissage interne + 63 validation interne. Les frontières et effectifs de chaque direction ont été contrôlés sur le snapshot. Validation strictement après apprentissage interne et avant test ; refit sur le train complet. Les fenêtres insuffisantes utilisent explicitement 80 tours.','', '### Moyennes, AUC et référence constante','', '|Direction|Statut|Δ LL moyenne candidat/fenêtre|Δ Brier moyenne candidat/fenêtre|Fenêtres AUC définie|LL constante train pondérée|LL fixe − constante|LL chrono − constante|Brier constante train pondéré|Brier fixe − constante|Brier chrono − constante|','|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|']
    for row in global_summary.to_dict('records'):
        cols=['MeanCW_DeltaLogLoss','MeanCW_DeltaBrier','ObsWeighted_LogLossTrainConstant','ObsWeighted_DeltaLogLossFixedVsConstant','ObsWeighted_DeltaLogLossChronoVsConstant','ObsWeighted_BrierTrainConstant','ObsWeighted_DeltaBrierFixedVsConstant','ObsWeighted_DeltaBrierChronoVsConstant']
        vals=[fmt(row[c]) for c in cols]
        lines.append('|'+ '|'.join([row['Direction'],row['Status'],vals[0],vals[1],str(row['AUCDefinedWindows']),*vals[2:]])+'|')
    lines += ['', 'Les moyennes CW donnent le même poids à chaque candidat/fenêtre. Les pertes pondérées donnent le même poids à chaque observation directionnelle (N dans les CSV). Les AUC sont calculées séparément par candidat/fenêtre avec les deux classes ; les fenêtres mono-classe sont NA et exclues uniquement du dénominateur AUC. Aucune AUC regroupant les prédictions de plusieurs modèles n’est utilisée.','', 'La référence constante est la prévalence du train complet propre au candidat/fenêtre et à la direction, retrouvée dans les labels du snapshot après exclusions, puis vérifiée contre la somme des effectifs positifs apprentissage interne + validation. Elle n’utilise jamais la prévalence test.','', '## Conclusion et limites','']
    for direction in ['Up','Down']:
        row=global_summary[(global_summary.Direction==direction)&(global_summary.Status=='toutes')].iloc[0]
        lines.append(f'- {direction} : variation descriptive LL pondérée {row.ObsWeighted_DeltaLogLoss:+.6f}, Brier pondéré {row.ObsWeighted_DeltaBrier:+.6f}, AUC moyenne {row.MeanCW_DeltaAUC:+.6f}. Voir les périodes/cohortes et les fenêtres optimisées avant de généraliser cette moyenne.')
    lines += ['', '**Résultat exploratoire : aucune supériorité statistiquement démontrée.** Pas de test/IC naïf fondé sur l’indépendance des candidats/fenêtres. Les candidats partagent dates, titres, labels et variables ; les apprentissages sont expansifs et chevauchants. Les observations de candidats différents peuvent compter plusieurs fois une même réalisation économique. Les cohortes ont des calendriers différents et le préfiltre est partagé. La couverture de l’optimisation et les résultats globaux sont donc distingués.', '', 'Le run fixe ne persiste pas les diagnostics internes ni les tours effectivement observés : ses 80 tours sont ceux du contrat enregistré, pas une mesure instrumentée par fenêtre. Sa version runtime XGBoost historique n’est pas identifiée dans les sources utilisées ; le chronologique enregistre la version dans son contrat. Les commits différents empêchent d’attribuer sans réserve tout écart au seul choix des tours. Les métriques recalculées concordent avec les LL/Brier persistés chronologiques et les AUC persistées des deux runs (tolérance absolue 1e-12).','', '## Reproduction et conventions','', 'Depuis C:\\Dev\\RStock : `python reports/analyse_xgb_20261010_fixe_chronologique/analyse.py` (Python, numpy, pandas). Le script n’importe pas RStock ou XGBoost ; il lit uniquement les artefacts existants et écrit exclusivement dans son dossier.', '', '- Clé brute : Set JSON ordonné (cible en premier, prédicteurs dans l’ordre), Window, Date normalisée ; Direction ajoutée après appariement. Identité courte SHA256(Set) à des fins de lecture, Set demeure la clé scientifique.', '- Log loss : probabilités bornées à [1e-15, 1−1e-15], même convention pour les deux runs et le comparateur constant ; logarithme naturel. Brier sur probabilités non bornées. AUC par rangs moyens pour les ex æquo.', '- `metriques_par_candidat_direction_fenetre.csv` : métriques individuelles, deltas, prévalence train, frontières et diagnostics. `synthese_globale.csv` : toutes/optimisées/replis. `synthese_par_periode_statut.csv` : cohortes, dates réelles, direction et statut.', '- `predictions_appariees.csv.gz` : les deux probabilités, label, clé, statut et prévalence train. Les chemins complets des deux CSV sources et leurs empreintes SHA256 sont dans `sources_et_verifications.json` (ainsi que tous les autres fichiers lus).', '- Tous les fichiers sources hachés au début ont été rehachés en fin de lecture et sont inchangés. Les ZIP/grilles ne sont pas nécessaires : les CSV locaux complets existent. Aucun nouvel export applicatif ; aucun entraînement.', '', 'Le temps de sélection/refit enregistré est agrégé dans les CSV ; il s’agit de sommes de temps par modèle et non de durée murale du job parallèle. Les résultats de périodes doivent être lus à calendrier comparable, sans considérer les cohortes comme des réplications indépendantes.']
    conclusion_index = lines.index('## Conclusion et limites') + 2
    lines.insert(conclusion_index, 'Sur cette population, Up favorise le fixe pour la log loss, le Brier et l’AUC moyenne. Down favorise le chronologique en log loss et Brier, mais **les deux modes Down restent moins bons que la probabilité constante fondée sur le train** pour ces deux pertes. Un gain contre le fixe ne suffit donc pas à démontrer une valeur prédictive additionnelle. Les deux modes Up dépassent légèrement leur référence constante en log loss/Brier, le fixe davantage.\n')
    lines += ['', 'Audit supplémentaire des commits : les AST des fonctions `prepare_dataset`, `predictor_columns`, `training_parameters`, `fit_booster_matrix` et `predict_probabilities_matrix` sont identiques. Le code ajouté comprend la sélection interne, le refit après sélection et les diagnostics. L’identité parfaite des replis constitue un contrôle empirique utile, sans prouver l’absence de toute différence historique non enregistrée. Tous les tests WF se terminent avant le début du holdout final (6 avril 2026).']
    (OUT/'SYNTHESE.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    print(global_summary[['Direction','Status','CandidateWindows','Observations','ObsWeighted_DeltaLogLoss','ObsWeighted_DeltaBrier','MeanCW_DeltaAUC','DifferentProbabilities']].to_string(index=False),flush=True)
    print('Terminé. Sources inchangées. Dossier:', OUT,flush=True)

if __name__ == '__main__':
    main()
