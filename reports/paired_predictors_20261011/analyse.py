"""Score archived predictions in chunks; never submit jobs or fit boosters."""
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
    'constant': '20261011T031914_c00cea6f22',
    'target': '20261011T031942_217eeee2c1',
    'mixed': '20261011T032130_4742d2f509',
    'external': '20261011T032201_17ece38f7b',
}
BASELINE = ROOT / 'diagnostics/aligned_constant_20261011/aligned_constant_20261011T031914_c00cea6f22.csv'


def sha(path):
    digest = hashlib.sha256()
    with path.open('rb') as handle:
        for chunk in iter(lambda: handle.read(4 * 1024 * 1024), b''):
            digest.update(chunk)
    return digest.hexdigest()


def main():
    inputs = [BASELINE, Path(__file__), ROOT / 'rstock/evaluation.py']
    configs, windows, frames = {}, {}, {}
    for family, ident in RUNS.items():
        root = ROOT / 'runs' / ident
        names = ['config.json', 'status.json', 'results/run_configuration.json',
                 'results/windows.csv', 'results/predictions.csv',
                 'checkpoints/artifacts/prepared_snapshot.pkl',
                 'checkpoints/artifacts/prepared_snapshot.json']
        inputs.extend(root / name for name in names)
        config = json.loads((root / 'config.json').read_text(encoding='utf-8'))
        assert json.loads((root / 'status.json').read_text())['status'] == 'completed'
        configs[family] = config
        windows[family] = pd.read_csv(root / 'results/windows.csv')
        path = root / 'checkpoints/artifacts/prepared_snapshot.pkl'
        assert sha(path) == json.loads(path.with_suffix('.json').read_text())['sha256']
        frames[family] = pickle.loads(path.read_bytes())['prepared']
    digests = {str(p.relative_to(ROOT)): sha(p) for p in inputs}
    print('Historical inputs hashed; validating common training/test population.', flush=True)
    reference = frames['target']
    assert all(frame.equals(reference) for frame in frames.values())
    science_fields = ['lag_depth', 'permutation_depth', 'intraday_target_threshold',
                      'intraday_down_threshold', 'xgb_max_depth', 'xgb_eta', 'xgb_rounds',
                      'xgb_seed', 'xgb_min_child_weight', 'xgb_subsample',
                      'xgb_colsample_bytree', 'xgb_gamma', 'xgb_reg_alpha', 'xgb_reg_lambda',
                      'walk_forward_window_mode', 'walk_forward_min_train_size',
                      'walk_forward_test_size', 'walk_forward_step_size',
                      'final_holdout_size', 'prediction_threshold', 'target_volatility_enabled',
                      'predictor_prefilter_enabled', 'date_feature_regex']
    assert all({k: c['rstock_config'].get(k) for k in science_fields} ==
               {k: configs['target']['rstock_config'].get(k) for k in science_fields}
               for c in configs.values())
    targets = sorted(windows['target'].Observation.unique())
    assert len(targets) == 20
    ref_windows = windows['target'].set_index(['Observation', 'Window'])
    baseline = pd.read_csv(BASELINE)
    dates = sorted(baseline.Date.unique())
    assert len(dates) == 433
    date_index = pd.DatetimeIndex(dates)
    pos = {date: i for i, date in enumerate(dates)}
    n_dates = len(dates)
    target_index = {t: i for i, t in enumerate(targets)}
    labels = {}
    for direction, suffix in [('Up', 'intraday_up'), ('Down', 'intraday_down')]:
        # Read exact label-column names from the known prepared contract.
        from rstock.features import intraday_target_column, intraday_down_target_column
        column = intraday_target_column if direction == 'Up' else intraday_down_target_column
        labels[direction] = np.stack([reference.loc[date_index, column(t)].to_numpy(dtype=int)
                                     for t in targets])
        for t in targets:
            b = baseline[(baseline.Observation == t) & (baseline.Direction == direction)].sort_values('Date')
            assert b.Date.tolist() == dates
            assert np.array_equal(b.Target.to_numpy(), labels[direction][target_index[t]])
            for number, group in b.groupby('Window'):
                window = ref_windows.loc[t, number]
                train = reference.loc[window.TrainStart:window.TrainEnd, column(t)]
                assert len(train) == window.TrainObservations and train.notna().all()
                assert np.allclose(group.Probability, train.mean(), rtol=0, atol=1e-14)
                assert (group.TrainEnd == window.TrainEnd).all()
    blocks = {int(n): np.array([pos[d] for d in group.sort_values('Date').Date])
              for n, group in baseline[(baseline.Observation == targets[0]) &
                                       (baseline.Direction == 'Up')].groupby('Window')}
    expected_window = np.empty(n_dates, dtype=int)
    for number, indices in blocks.items():
        expected_window[indices] = number
    scores, window_scores, period_scores, daily_scores = [], [], [], []
    for family in ['constant', 'target', 'external', 'mixed']:
        w = windows[family]
        saved_windows = w.set_index(['Set', 'Window'])
        sets = sorted(w.Set.unique())
        assert len(sets) == (3800 if family in {'external', 'mixed'} else 20)
        set_targets = [json.loads(key)[0] for key in sets]
        ti = np.array([target_index[t] for t in set_targets])
        if family != 'constant':
            required = set()
            for text in w.FeatureColumns.unique():
                required.update(json.loads(text))
            available = reference[sorted(required)].notna().all(axis=1)
            for key, group in w.groupby('Set'):
                target = json.loads(key)[0]
                for row in group.itertuples():
                    rw = ref_windows.loc[target, row.Window]
                    for field in ['TrainStart', 'TrainEnd', 'TestStart', 'TestEnd',
                                  'TrainObservations', 'TestObservations',
                                  'UpPositiveOutcomes', 'DownPositiveOutcomes']:
                        assert getattr(row, field) == rw[field], (family, key, field)
                    assert available.loc[row.TrainStart:row.TrainEnd].all()
                    assert available.loc[row.TestStart:row.TestEnd].all()
            matrices = {d: np.full((len(sets), n_dates), np.nan) for d in ['Up', 'Down']}
            lookup = {key: i for i, key in enumerate(sets)}
            count = 0
            path = ROOT / 'runs' / RUNS[family] / 'results/predictions.csv'
            columns = ['Set', 'Observation', 'Window', 'Date', 'UpProbability',
                       'DownProbability', 'IntradayTarget', 'DownTarget']
            for chunk in pd.read_csv(path, usecols=columns, chunksize=65536, float_precision='round_trip'):
                si = chunk.Set.map(lookup).to_numpy()
                di = chunk.Date.map(pos).to_numpy()
                assert not pd.isna(si).any() and not pd.isna(di).any()
                si, di = si.astype(int), di.astype(int)
                assert np.array_equal(chunk.Observation.to_numpy(), np.array(set_targets)[si])
                assert np.array_equal(chunk.Window.to_numpy(), expected_window[di])
                flat = si * n_dates + di
                assert len(np.unique(flat)) == len(flat)
                for direction, label in [('Up', 'IntradayTarget'), ('Down', 'DownTarget')]:
                    assert np.array_equal(chunk[label].to_numpy(), labels[direction][ti[si], di])
                    assert np.isnan(matrices[direction][si, di]).all(), 'Duplicate candidate/date'
                    probability = chunk[direction + 'Probability'].to_numpy()
                    assert np.isfinite(probability).all() and ((probability >= 0) & (probability <= 1)).all()
                    matrices[direction][si, di] = probability
                count += len(chunk)
            assert count == len(sets) * n_dates
            assert all(np.isfinite(matrix).all() for matrix in matrices.values())
        else:
            matrices = {}
            for direction in ['Up', 'Down']:
                matrices[direction] = np.stack([
                    baseline[(baseline.Observation == t) & (baseline.Direction == direction)]
                    .sort_values('Date').Probability.to_numpy() for t in set_targets])
        for direction in ['Up', 'Down']:
            y = labels[direction][ti]
            probabilities = matrices[direction]
            ll, br = probability_losses(y, probabilities)
            for i, key in enumerate(sets):
                scores.append({'Family': family, 'Run': RUNS[family], 'Direction': direction,
                               'Observation': set_targets[i], 'Set': key, 'N': n_dates,
                               'LogLoss': ll[i].mean(), 'Brier': br[i].mean(),
                               'AUC': _roc_auc(y[i], probabilities[i])})
                for number, indices in blocks.items():
                    wm = {'Family': family, 'Direction': direction, 'Observation': set_targets[i],
                          'Set': key, 'Window': number, 'N': len(indices),
                          'LogLoss': ll[i, indices].mean(), 'Brier': br[i, indices].mean(),
                          'AUC': _roc_auc(y[i, indices], probabilities[i, indices])}
                    if family != 'constant':
                        saved = saved_windows.loc[key, number]
                        for metric in ['LogLoss', 'Brier', 'AUC']:
                            field = direction + ('ROCAUC' if metric == 'AUC' else metric)
                            value = np.nan if wm[metric] is None else wm[metric]
                            assert np.isclose(value, saved[field], atol=1e-10, rtol=0, equal_nan=True), (family, key, number, metric)
                    window_scores.append(wm)
            for target, target_i in target_index.items():
                mask = ti == target_i
                for period, indices in enumerate(np.array_split(np.arange(n_dates), 3), start=1):
                    period_scores.append({'Family': family, 'Direction': direction, 'Observation': target,
                                          'Period': period, 'FirstDate': dates[indices[0]],
                                          'LastDate': dates[indices[-1]], 'N': len(indices),
                                          'LogLoss': ll[mask][:, indices].mean(),
                                          'Brier': br[mask][:, indices].mean()})
                for date_i, date in enumerate(dates):
                    daily_scores.append({'Family': family, 'Direction': direction, 'Observation': target,
                                         'Date': date, 'LogLoss': ll[mask, date_i].mean(),
                                         'Brier': br[mask, date_i].mean()})
        print(f'{family}: {len(sets)} candidates x {n_dates} dates; labels/keys/windows verified.', flush=True)
    scores = pd.DataFrame(scores)
    target_scores = scores.groupby(['Family', 'Direction', 'Observation'])[['LogLoss', 'Brier', 'AUC']].mean().reset_index()
    overall = target_scores.groupby(['Family', 'Direction'])[['LogLoss', 'Brier', 'AUC']].mean().reset_index()
    paired = scores[scores.Family == 'mixed'].merge(scores[scores.Family == 'external'],
        on=['Set', 'Observation', 'Direction'], suffixes=('_mixed', '_external'), validate='one_to_one')
    for metric in ['LogLoss', 'Brier', 'AUC']:
        paired['Delta_' + metric] = paired[metric + '_mixed'] - paired[metric + '_external']
    assert len(paired) == 7600
    for path, digest in digests.items():
        assert sha(ROOT / path) == digest, f'Historical input modified: {path}'
    for name, frame in [('candidate_metrics', scores), ('target_metrics', target_scores),
                        ('overall_metrics', overall), ('window_metrics', pd.DataFrame(window_scores)),
                        ('period_metrics', pd.DataFrame(period_scores)),
                        ('daily_target_losses', pd.DataFrame(daily_scores)),
                        ('paired_mixed_external', paired)]:
        frame.to_csv(OUT / (name + '.csv'), index=False)
    summary = {'run_ids': RUNS, 'population': 'development before qualification; all candidates',
               'targets': targets, 'common_dates': n_dates, 'first_date': dates[0], 'last_date': dates[-1],
               'weighting': 'equal target weight; equal candidate weight within target; equal dates',
               'auc': 'mean per-candidate AUC, never pooled across models',
               'input_sha256': digests, 'historical_inputs_unchanged': True,
               'training_executed': False, 'overall': overall.to_dict('records'),
               'snapshots_equal': True, 'prediction_keys_labels_windows_verified': True,
               'constant_train_prevalences_verified': True,
               'window_scores_match_saved_tolerance': 1e-10,
               'inference': 'descriptive paired evaluation; no significance test or independent validation'}
    (OUT / 'summary.json').write_text(json.dumps(summary, ensure_ascii=False, indent=2), encoding='utf-8')
    print(overall.to_string(index=False), flush=True)


if __name__ == '__main__':
    main()
