"""Frozen prefilter engine configuration and observed development-window audit."""
from dataclasses import replace
import hashlib
import json
import numpy as np
import pandas as pd

PREFILTER_MODEL_FIELDS = ('prefilter_algorithm', 'prefilter_logistic_l2_penalty',
    'prefilter_logistic_max_iterations', 'prefilter_logistic_gradient_tolerance',
    'prefilter_logistic_objective_tolerance')


def training_config(config):
    return replace(config, predictive_algorithm=config.prefilter_algorithm,
        predictive_model_type='external_only', target_volatility_enabled=False,
        xgb_seed=config.prefilter_xgb_seed,
        **{name.removeprefix('prefilter_'):getattr(config,name) for name in PREFILTER_MODEL_FIELDS[1:]})


def engine_contract(config):
    if config.prefilter_algorithm == 'logistic_l2':
        from .logistic import logistic_contract
        return {'algorithm':'logistic_l2','parameters':logistic_contract(training_config(config))}
    from .modeling import prefilter_xgboost_snapshot, RoundSelectionPolicy
    return {'algorithm':'xgboost','parameters':prefilter_xgboost_snapshot(config),
            'round_selection_policy':RoundSelectionPolicy.from_config(config,scope='prefilter').snapshot()}


def window_audit(train, test, names, outcome, probabilities, *, identity, window, holdout_start):
    from .evaluation import classification_metrics
    if train.index.max() >= test.index.min() or test.index.max() >= holdout_start:
        raise ValueError('Prefilter window crosses its training/test or Holdout boundary')
    def digest(frame):
        return hashlib.sha256(pd.util.hash_pandas_object(frame, index=True).values.tobytes()
            + json.dumps(list(frame.columns)).encode()).hexdigest()
    constant = float(train[outcome].mean())
    y = test[outcome].to_numpy(dtype=int)
    reference = np.full(len(test), constant)
    model_metrics = classification_metrics(y,(np.asarray(probabilities)>=.5).astype(int),np.asarray(probabilities))
    base_metrics = classification_metrics(y,(reference>=.5).astype(int),reference)
    return {'Set':identity,'Window':window,'Direction':'Up',
        'TrainStart':str(train.index.min()),'TrainEnd':str(train.index.max()),
        'TestStart':str(test.index.min()),'TestEnd':str(test.index.max()),
        'HoldoutStart':str(holdout_start),'TrainObservations':len(train),'TestObservations':len(test),
        'TrainInputSHA256':digest(train[[*names,outcome]]),'TestInputSHA256':digest(test[[*names,outcome]]),
        'Features':json.dumps(list(names)),'Outcome':outcome,'ConstantProbability':constant,
        'UpLogLoss':model_metrics.log_loss,'UpBrier':model_metrics.brier,
        'UpROCAUC':model_metrics.roc_auc,'UpPRAUC':model_metrics.pr_auc,
        'ConstantLogLoss':base_metrics.log_loss,'ConstantBrier':base_metrics.brier,
        'ConstantROCAUC':base_metrics.roc_auc,'Status':'evaluated'}


def persist_audit(output, checkpoints, config):
    frames = [manager.load_artifact('prefilter_round_selection_training') for manager in checkpoints
              if manager.artifact_exists('prefilter_round_selection_training')]
    audit = pd.concat(frames,ignore_index=True) if frames else pd.DataFrame()
    columns = ['Set','Window','OriginCutoff','TrainStart','TrainEnd','TestStart','TestEnd',
        'HoldoutStart','TrainObservations','TestObservations','TrainInputSHA256','TestInputSHA256','Features','Outcome','Status']
    valid = not audit.empty and set(columns) <= set(audit.columns) and audit[columns].notna().all().all()
    expected_windows = 0
    for manager in checkpoints:
        if manager.artifact_exists('prefilter_qualification'):
            qualification = manager.load_artifact('prefilter_qualification')
            expected_windows += int(qualification.get('WindowsEvaluated',pd.Series(dtype=int)).sum())
    valid = valid and expected_windows == len(audit)
    result = {'version':1,'engine':engine_contract(config),'status':'verified' if valid else 'unverifiable',
              'direction':'Up','evaluation_sha256':None,'windows':len(audit),'metrics':{}}
    if valid:
        ordered = audit.sort_values(['OriginCutoff','Set','Window'])
        identity = ordered[columns].to_json(orient='records',date_format='iso')
        result['evaluation_sha256'] = hashlib.sha256(identity.encode()).hexdigest()
        result.update(train_start=str(audit.TrainStart.min()),test_end=str(audit.TestEnd.max()),
            holdout_start=str(audit.HoldoutStart.max()), label_availability='intraday_label_known_after_session_close')
        if any(pd.Timestamp(row.TestEnd)>=pd.Timestamp(row.HoldoutStart) for row in audit.itertuples()):
            raise ValueError('Prefilter audit includes Holdout observations')
        for metric in ('UpLogLoss','UpBrier','ConstantLogLoss','ConstantBrier'):
            if metric in audit:
                weights = pd.to_numeric(audit.TestObservations)
                result['metrics'][metric] = float((pd.to_numeric(audit[metric])*weights).sum()/weights.sum())
    output.mkdir(parents=True,exist_ok=True)
    audit.to_csv(output/'prefilter_windows.csv',index=False)
    (output/'prefilter_evaluation_contract.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n',encoding='utf-8')
    result['artifact_sha256'] = hashlib.sha256((output/'prefilter_windows.csv').read_bytes()).hexdigest()
    return {'prefilter_evaluation':result, 'prefilter_algorithm':config.prefilter_algorithm}
