"""Persisted, descriptive selection evidence. Never trains or changes a decision."""
from __future__ import annotations
import hashlib
import json
from pathlib import Path
from time import perf_counter
import numpy as np
import pandas as pd
from .forward_diagnostic import _json, _clean, _publish, _stage_ids, _key, _bool, digest, _stage_expected

PROTOCOL = "selection_diagnostic_v1"
MANIFEST = "selection_diagnostic_manifest.json"
FILES = ("selection_funnel.csv", "selection_rejections.csv", "selection_candidates.csv",
         "selection_wf_windows.csv", "selection_context_population.csv", "selection_history_composition.csv")
FORWARD_FILES = ("selection_generalization.csv", "selection_t0_associations.csv",
                 "selection_forward_context.csv", "selection_forward_composition.csv")


def detail_name(key):
    return hashlib.sha256(str(key).encode()).hexdigest()[:24] + ".json"


def _canonical(frame):
    result = frame.copy()
    if "Set" in result:
        directions = result.get("Direction", pd.Series("Up", index=result.index))
        result["model_key"] = [_key(str(s), str(d)) for s, d in zip(result.Set, directions)]
    return result


def _frame(path, sources, unavailable, expected=None):
    if not path.is_file():
        unavailable[str(path)] = "missing_or_purged"
        return pd.DataFrame()
    sha = digest(path)
    if expected and sha != expected: raise ValueError(f"selection_source_integrity_error:{path}")
    sources[str(path)] = sha
    try: return pd.read_csv(path)
    except pd.errors.EmptyDataError: return pd.DataFrame()


def _reason_rows(frame, stage, column):
    records = []
    if column not in frame: return records
    for value in frame[column].dropna():
        try: reasons = json.loads(str(value))
        except ValueError: reasons = [str(value)]
        if not isinstance(reasons, list): reasons = [str(reasons)]
        for reason in reasons:
            records.append({"stage": stage, "reason": reason})
    if not records: return []
    counts = pd.DataFrame(records).value_counts(["stage", "reason"]).rename("candidates").reset_index()
    unique = {}
    for value in frame[column].dropna():
        try: items = json.loads(str(value))
        except ValueError: items = [str(value)]
        if isinstance(items, list) and len(items) == 1: unique[str(items[0])] = unique.get(str(items[0]), 0) + 1
    counts["sole_rejection_reason"] = counts.reason.map(unique).fillna(0).astype(int)
    counts["interpretation"] = "Motifs chevauchants; effet marginal observable, pas une cascade de filtres inventée"
    return counts.to_dict("records")


def _valid_commit(output, signature):
    current = _json(output / MANIFEST)
    return (current.get("protocol") == PROTOCOL and current.get("input_signature") == signature
            and all((output / name).is_file() and digest(output / name) == sha
                    for name, sha in current.get("artifact_digests", {}).items())
            and bool(current.get("artifact_digests"))
            and all((output / name).is_file() and digest(output / name) == sha
                    for name, sha in current.get("detail_digests", {}).items()))


def _wf_holdout_summary(candidates):
    if not {"ROCAUCMedian", "holdout_qualification_ROCAUC"}.issubset(candidates):
        return {"availability":"unavailable"}
    valid=candidates[["ROCAUCMedian", "holdout_qualification_ROCAUC"]].apply(pd.to_numeric,errors="coerce").dropna()
    return {"availability":"available" if len(valid) else "unavailable", "matched_candidates":len(valid),
            "wf_auc_median":valid.ROCAUCMedian.median(),"holdout_auc_median":valid.holdout_qualification_ROCAUC.median(),
            "lower_holdout_count":int(valid.holdout_qualification_ROCAUC.lt(valid.ROCAUCMedian).sum()),
            "interpretation":"Mêmes identités; AUC médiane de fenêtres WF et AUC Holdout ont des définitions différentes"}


def _context_compatibility_columns(table):
    """Legacy aggregates remain readable; never invent missing exposure evidence."""
    table = table.copy()
    for column in ("market_exposure_share", "exposure_adjusted_signal_concentration"):
        if column not in table: table[column] = np.nan
    return table


def materialize_selection(run: Path):
    """One compact sidecar per E2E, resolving physical inherited stages."""
    started = perf_counter(); run = Path(run); output = run / "results"
    stages = _stage_ids(run); runs = run.parent
    if not (run / "orchestration/pipeline.json").is_file():
        raise ValueError("selection_pipeline_manifest_missing")
    sources, missing = {}, {}
    pipeline = run / "orchestration/pipeline.json"
    sources[str(pipeline)] = digest(pipeline)
    saved = _json(run / "config.json")
    if (run / "config.json").exists(): sources[str(run / "config.json")] = digest(run / "config.json")
    def read(stage, file):
        if stage not in stages:
            missing[stage + "/" + file] = "stage_not_recorded"; return pd.DataFrame()
        return _frame(runs / stages[stage] / "results" / file, sources, missing,
                      _stage_expected(run, stage, "results/" + file))
    wf = read("walk_forward", "qualification.csv")
    windows = read("walk_forward", "windows.csv")
    selection = read("walk_forward", "selection_results.csv")
    prefilter_stage = "prefilter" if "prefilter" in stages else "walk_forward"
    prefilter = read(prefilter_stage, "predictor_prefilter.csv")
    holdout_stage = "holdout_evaluation" if "holdout_evaluation" in stages else "threshold_calibration"
    holdout = read(holdout_stage, "holdout_metrics.csv")
    qualification_path = runs / stages.get("promotion_qualification", run.name) / "results/qualification.json"
    qualification = _json(qualification_path)
    if qualification_path.exists():
        sha = digest(qualification_path)
        expected = _stage_expected(run, "promotion_qualification", "results/qualification.json")
        if expected and sha != expected: raise ValueError("selection_qualification_integrity_error")
        sources[str(qualification_path)] = sha
    else: missing["qualification_decisions"] = "historical_decision_table_not_persisted"
    decisions = pd.DataFrame(qualification.get("decisions", []))
    funnel, reasons = [], []
    def count(stage, frame, accepted=None, unit="combinaisons", note=""):
        known = accepted is not None
        retained = int(accepted.fillna(False).sum()) if known else None
        funnel.append(dict(stage=stage, evaluated=len(frame), retained=retained,
            rejected=len(frame)-retained if known else None,
            conversion=retained/len(frame) if known and len(frame) else None, unit=unit,
            availability="available" if len(frame) else "unavailable_or_empty", interpretation=note))
    accepted = (prefilter.PrefilterStatus.eq("retained") if "PrefilterStatus" in prefilter else None)
    count("Préfiltre", prefilter, accepted, "couples cible/prédicteur", "Population du CSV; origines distinctes non additionnées")
    if "PrefilterStatus" in prefilter:
        reasons.extend(dict(stage="Préfiltre", reason=k, candidates=int(v), sole_rejection_reason=int(v), interpretation="Statuts finaux exclusifs du CSV persisté") for k,v in prefilter.loc[~accepted,"PrefilterStatus"].value_counts().items())
    eligible = _bool(wf.Eligible) if "Eligible" in wf else None
    count("Walk-forward", wf, eligible)
    reasons.extend(_reason_rows(wf.loc[~eligible] if eligible is not None else wf, "Walk-forward", "IneligibilityReasons"))
    up_holdout = holdout.loc[holdout.Direction.eq("Up")] if "Direction" in holdout else holdout
    count("Holdout évalué", up_holdout, None, note="évaluation, pas un filtre autonome; décisions dans Qualification")
    qaccepted = _bool(decisions.candidate) if "candidate" in decisions else None
    count("Qualification promotion", decisions, qaccepted)
    reasons.extend(_reason_rows(decisions.loc[~qaccepted] if qaccepted is not None else decisions, "Qualification promotion", "reasons"))
    candidates = _canonical(wf)
    if not candidates.empty:
        candidates["wf_eligible"] = eligible.to_numpy() if eligible is not None else None
        candidates["qualification_candidate"] = None
        candidates["decision_availability"] = "unavailable"
        if not decisions.empty and "Combinaison" in decisions:
            keyed = decisions.assign(model_key=[_key(str(s), str(d)) for s,d in zip(decisions.Combinaison, decisions.get("Direction", pd.Series("Up",index=decisions.index)))])
            if keyed.model_key.duplicated().any(): raise ValueError("selection_ambiguous_qualification_identity")
            lookup = keyed.set_index("model_key")
            if qaccepted is not None:
                lookup["accepted"] = qaccepted.to_numpy()
                candidates["qualification_candidate"] = candidates.model_key.map(lookup.accepted)
            if "reasons" in lookup:
                candidates["qualification_reasons"] = candidates.model_key.map(lookup.reasons.map(lambda value: json.dumps(value,ensure_ascii=False)))
            candidates["decision_availability"] = np.where(candidates.model_key.isin(lookup.index), "persisted", "not_evaluated")
        if not up_holdout.empty and "Set" in up_holdout:
            h = _canonical(up_holdout)
            if h.model_key.duplicated().any(): raise ValueError("selection_ambiguous_holdout_identity")
            h = h.set_index("model_key")
            for column in ("ROCAUC", "Precision", "SignalCount", "Prevalence", "DirectionalReturnMean"):
                if column in h: candidates["holdout_qualification_"+column] = candidates.model_key.map(h[column])
        if not selection.empty and "Set" in selection:
            ranked = _canonical(selection).set_index("model_key")
            for column in ("model_selection_score", "model_selection_rank", "FinalStatus", "FinalUpROCAUC"):
                if column in ranked: candidates[column] = candidates.model_key.map(ranked[column])
    wf_rows = _canonical(windows)
    distribution = pd.DataFrame()
    if not windows.empty and {"Set","Window","UpROCAUC"}.issubset(windows):
        statistics = wf_rows.groupby("model_key").UpROCAUC.agg(wf_auc_mean="mean", wf_auc_std="std", wf_auc_worst="min", wf_auc_best="max", wf_window_count="count")
        statistics["wf_test_start"] = wf_rows.groupby("model_key").TestStart.min()
        statistics["wf_test_end"] = wf_rows.groupby("model_key").TestEnd.max()
        statistics["wf_auc_range"] = statistics.wf_auc_best-statistics.wf_auc_worst
        statistics["wf_good_window_share"] = wf_rows.assign(good=wf_rows.UpROCAUC.gt(.5).where(wf_rows.UpROCAUC.notna())).groupby("model_key").good.mean()
        candidates = candidates.merge(statistics, left_on="model_key", right_index=True, how="left", validate="one_to_one")
        wf_rows = wf_rows.merge(candidates[["model_key","wf_eligible","qualification_candidate"]], on="model_key", how="left", validate="many_to_one")
        wf_rows["population"] = np.where(wf_rows.wf_eligible.fillna(False),"Admissibles WF","Rejetés WF")
        wf_rows["qualification_population"] = np.where(wf_rows.qualification_candidate.eq(True),"Retenus qualification",np.where(wf_rows.qualification_candidate.eq(False),"Rejetés qualification","Non évalués en qualification"))
        distributions = []
        for level,column in (("Walk-forward","population"),("Qualification promotion","qualification_population")):
            view = wf_rows.copy()
            view["population"] = view[column]
            grouped = view.groupby(["Window","population"]).agg(candidates=("model_key","nunique"), auc_median=("UpROCAUC","median"), auc_q25=("UpROCAUC",lambda x:x.quantile(.25)), auc_q75=("UpROCAUC",lambda x:x.quantile(.75)), auc_min=("UpROCAUC","min"), auc_max=("UpROCAUC","max"), test_start=("TestStart","min"), test_end=("TestEnd","max")).reset_index()
            distributions.append(grouped.assign(selection_stage=level))
        distribution = pd.concat(distributions,ignore_index=True)
    contexts, compositions, context_refs = [], [], {}
    details = {str(row["model_key"]): {"candidate": row, "windows": [], "context": []} for row in candidates.to_dict("records")} if "model_key" in candidates else {}
    if not wf_rows.empty:
        for key, group in wf_rows.groupby("model_key"):
            if str(key) in details: details[str(key)]["windows"] = group.to_dict("records")
    comparability = None
    for stage in ("walk_forward", "threshold_calibration", "holdout_evaluation"):
        if stage not in stages: continue
        directory = runs / stages[stage] / "results"
        manifest = _json(directory / "context_diagnostic_manifest.json")
        if manifest.get("status") != "available":
            missing[stage+"/market_context"] = manifest.get("reason", "disabled_or_not_persisted"); continue
        from .market_context_runtime import load_context_diagnostic
        meta, context, table, _ = load_context_diagnostic(directory, runs)
        table = _context_compatibility_columns(table)
        if "regime" not in context:
            missing[stage+"/economic_regimes"] = "not_captured_by_historical_protocol"
        sources[str(directory / "context_diagnostic_manifest.json")] = digest(directory / "context_diagnostic_manifest.json")
        context_refs[stage] = {"run_id": stages[stage], "manifest_sha256": sources[str(directory / "context_diagnostic_manifest.json")]}
        signature = meta.get("comparability")
        if signature is None:
            old_context = _json(runs / meta["context_manifest"])
            signature = {"protocol_id": meta.get("protocol_id"), "legacy_shared_manifest": meta.get("context_manifest"), "boundaries": old_context.get("boundaries"), "reference_start": old_context.get("reference_start"), "reference_end_exclusive": old_context.get("reference_end_exclusive")}
        if signature is not None:
            if comparability is not None and signature != comparability: raise ValueError("selection_incompatible_context_references")
            comparability = signature
        if not table.empty and "model_key" in candidates:
            table = table.merge(candidates[["model_key","wf_eligible","qualification_candidate"]],on="model_key",how="left",validate="many_to_one")
            table["population"] = np.where(table.qualification_candidate.eq(True),"Retenus qualification",np.where(table.wf_eligible.eq(True),"Admissibles WF non retenus",np.where(table.wf_eligible.eq(False),"Rejetés WF","Décision indisponible")))
            for key, group in table.groupby("model_key"):
                if str(key) in details: details[str(key)]["context"].extend(group.to_dict("records"))
            # Keep raw per-model values in lazy details; population charts only
            # summarize supported comparisons, not single-class/tiny samples.
            table = table.copy()
            for column in ("auc", "brier"):
                if column in table: table[column] = table[column].where(table.auc_supported)
            for column in ("precision", "mean_return"):
                if column in table: table[column] = table[column].where(table.signal_supported)
            groupkeys = ["stage","population","axis","band","period_kind","horizon"]
            compact = table.groupby(groupkeys,dropna=False).agg(models=("model_key","nunique"), model_windows=("model_key","size"), observations=("observations","sum"), signals=("signal_count","sum"), median_model_window_auc=("auc","median"), median_model_window_brier=("brier","median"), median_model_window_precision=("precision","median"), median_model_window_return=("mean_return","median"), supported_auc_rows=("auc_supported","sum"), median_exposure_share=("market_exposure_share","median"), median_exposure_adjusted_concentration=("exposure_adjusted_signal_concentration","median")).reset_index()
            contexts.append(compact)
        if not windows.empty and {"TestStart","TestEnd"}.issubset(windows):
            starts, ends = pd.to_datetime(windows.TestStart), pd.to_datetime(windows.TestEnd)
            if stage == "walk_forward":
                date_mask = pd.Series(False,index=context.index)
                for start,end in windows[["TestStart","TestEnd"]].drop_duplicates().itertuples(index=False,name=None): date_mask |= context.session_date.between(str(start),str(end))
            else:
                # Restrict to actually observed dates, excluding initial train warmup.
                path = directory / ("holdout_predictions.csv" if stage == "holdout_evaluation" else "development_probabilities.csv")
                dates = set()
                if path.exists():
                    source_sha = digest(path)
                    expected_sha = meta.get("input_digests",{}).get(str(path.relative_to(runs)))
                    if expected_sha and source_sha != expected_sha: raise ValueError("selection_context_dates_integrity_error")
                    sources[str(path)] = source_sha
                    for part in pd.read_csv(path,usecols=["Date"],chunksize=100000): dates.update(part.Date.astype(str))
                date_mask = context.session_date.isin(dates)
            part = context.loc[date_mask]
            for axis in ("trend", "drawdown", "volatility", "regime"):
                field = axis if axis == "regime" else axis+"_band"
                if field not in part: continue
                for band,g in part.groupby(part[field].fillna("Indisponible")):
                    compositions.append(dict(stage=stage,axis=axis,band=band,sessions=len(g),share=len(g)/len(part) if len(part) else None,episodes=g.episode_id.nunique() if "episode_id" in g else None))
    artifacts = dict(zip(FILES,[pd.DataFrame(funnel),pd.DataFrame(reasons).reindex(columns=["stage","reason","candidates","sole_rejection_reason","interpretation"]),candidates,distribution,pd.concat(contexts,ignore_index=True) if contexts else pd.DataFrame(columns=["stage","population","axis","band"]),pd.DataFrame(compositions,columns=["stage","axis","band","sessions","share","episodes"])]))
    signature = hashlib.sha256(json.dumps(sources,sort_keys=True).encode()).hexdigest()
    if _valid_commit(output, signature): return _json(output / MANIFEST)
    from .runner import _try_submission_mutex
    with _try_submission_mutex(output / ".selection_diagnostic.lock") as acquired:
        if not acquired: raise ValueError("selection_diagnostic_already_building")
        if _valid_commit(output,signature): return _json(output / MANIFEST)
        for name,frame in artifacts.items(): _publish(output/name,frame)
        detail_digests = {}
        for key,record in details.items():
            name = "selection_details/"+detail_name(key)
            _publish(output/name,_clean(record)); detail_digests[name] = digest(output/name)
        # Inputs may have been purged/replaced by another component while computing.
        if any(not Path(name).exists() or digest(Path(name)) != sha for name,sha in sources.items()): raise ValueError("selection_sources_changed_during_generation")
        current = _json(output/MANIFEST)
        manifest = {**current,"schema_version":1,"protocol":PROTOCOL,"status":"available" if len(candidates) else "partial",
            "source_e2e_run_id":run.name,"stage_run_ids":stages,"input_signature":signature,
            "sources":{str(Path(name).relative_to(runs)):sha for name,sha in sources.items()},
            "artifact_digests":{name:digest(output/name) for name in artifacts},"detail_digests":detail_digests,
            "context_references":context_refs,"comparability":comparability,
            "missing":missing,"candidate_count":len(candidates),"build_seconds":perf_counter()-started,
            "wf_holdout_summary": _wf_holdout_summary(candidates),
            "qualification_policy": qualification.get("policy_parameters"),
            "criteria":{key:saved.get("rstock_config",{}).get(key) for key in saved.get("rstock_config",{}) if any(token in key for token in ("qualification","promotion_min","promotion_max","predictor_prefilter","prefilter_selection","threshold_calibration","threshold_parameter_calibration","model_selection","walk_forward","final_holdout","final_confirmation"))},
            "limitations":["Associations descriptives, aucune causalité démontrée","Performances Train non persistées","Holdout de qualification distinct du Holdout comparable","Les fenêtres, séances et épisodes ne sont pas des essais indépendants","Forward des rejetés absent sauf simulation explicite"]}
        _publish(output/MANIFEST,manifest)
    return manifest


def materialize_generalization(output: Path):
    """Forward-owned compact links; reconcile a tiny parent index under mutex."""
    output=Path(output); analysis=_json(output/"forward_analysis_manifest.json")
    source_id=analysis.get("source_e2e_run_id")
    if not source_id: raise ValueError("selection_forward_source_missing")
    source=output.parent.parent/str(source_id); runs=source.parent
    candidates_path=source/"results/selection_candidates.csv"
    if not candidates_path.exists(): return {"status":"unavailable","reason":"selection_t0_not_persisted"}
    manifest=_json(source/"results"/MANIFEST)
    expected=manifest.get("artifact_digests",{}).get("selection_candidates.csv")
    if not expected or digest(candidates_path)!=expected: raise ValueError("selection_t0_integrity_error")
    source_inputs = {candidates_path: expected, source/"results"/MANIFEST: digest(source/"results"/MANIFEST),
                     output/"forward_analysis_manifest.json": digest(output/"forward_analysis_manifest.json")}
    candidates=pd.read_csv(candidates_path)
    diagnostic=output/"forward_diagnostic_metrics.csv"
    if not diagnostic.exists(): return {"status":"unavailable","reason":"forward_diagnostic_not_persisted"}
    meta=_json(output/"forward_diagnostic_manifest.json")
    if digest(diagnostic)!=meta.get("artifact_digests",{}).get(diagnostic.name): raise ValueError("selection_forward_diagnostic_integrity_error")
    source_inputs[diagnostic] = meta["artifact_digests"][diagnostic.name]
    source_inputs[output/"forward_diagnostic_manifest.json"] = digest(output/"forward_diagnostic_manifest.json")
    forward=pd.read_csv(diagnostic)
    merged=forward.merge(candidates,left_on="canonical_combination_id",right_on="model_key",how="left",validate="many_to_one",suffixes=("","_wf"))
    merged["forward_run_id"]=output.parent.name
    associations=[]
    criteria=[c for c in ("ROCAUCMedian","ROCAUCWorst","ROCAUCStd","wf_auc_range","wf_good_window_share","model_selection_score","holdout_qualification_ROCAUC","holdout_qualification_Precision") if c in merged]
    for (kind,horizon),part in merged.groupby(["period_kind","horizon"],dropna=False):
        for criterion in criteria:
            for outcome in ("auc","brier","delta_auc","mean_return"):
                if outcome not in part: continue
                x=pd.to_numeric(part[criterion],errors="coerce");y=pd.to_numeric(part[outcome],errors="coerce");valid=x.notna()&y.notna();n=int(valid.sum())
                supported=n>=10 and x[valid].nunique()>1 and y[valid].nunique()>1
                rho=float(x[valid].rank().corr(y[valid].rank())) if supported else None
                associations.append(dict(period_kind=kind,horizon=horizon,criterion=criterion,outcome=outcome,models=n,spearman=rho,status="exploratory_association" if supported else "insufficient_support",interpretation="Non causal; cohorte sélectionnée; horizons dépendants"))
    context_population=pd.DataFrame(columns=["stage","population","axis","band","forward_run_id"])
    composition=pd.DataFrame(columns=["stage","axis","band","sessions","share","episodes","forward_run_id"])
    context_status="unavailable"
    context_meta=_json(output/"context_diagnostic_manifest.json")
    if context_meta.get("status")=="available":
        from .market_context_runtime import load_context_diagnostic
        expected_context=manifest.get("comparability")
        if expected_context is not None and context_meta.get("comparability")!=expected_context:
            raise ValueError("selection_forward_context_reference_mismatch")
        source_inputs[output/"context_diagnostic_manifest.json"] = digest(output/"context_diagnostic_manifest.json")
        period_path = output/"forward_period_metrics.csv"
        period_sha = digest(period_path)
        expected_period = context_meta.get("input_digests",{}).get(str(period_path.relative_to(runs)))
        if expected_period and period_sha != expected_period: raise ValueError("selection_context_period_integrity_error")
        source_inputs[period_path] = period_sha
        _,calendar,table,_=load_context_diagnostic(output,runs)
        table = _context_compatibility_columns(table)
        context_status="available" if expected_context is not None else "upstream_reference_unavailable"
        if not table.empty:
            table=table.merge(candidates[["model_key","wf_eligible","qualification_candidate"]],on="model_key",how="left",validate="many_to_one")
            table["population"]=np.where(table.qualification_candidate.eq(True),"Retenus qualification","Décision indisponible")
            for column in ("auc","brier"): table[column]=table[column].where(table.auc_supported)
            for column in ("precision","mean_return"): table[column]=table[column].where(table.signal_supported)
            keys=["stage","population","axis","band","period_kind","horizon"]
            context_population=table.groupby(keys,dropna=False).agg(models=("model_key","nunique"),observations=("observations","sum"),signals=("signal_count","sum"),median_model_window_auc=("auc","median"),median_model_window_brier=("brier","median"),median_model_window_precision=("precision","median"),median_model_window_return=("mean_return","median"),supported_auc_rows=("auc_supported","sum"),median_exposure_share=("market_exposure_share","median"),median_exposure_adjusted_concentration=("exposure_adjusted_signal_concentration","median")).reset_index()
            context_population["forward_run_id"]=output.parent.name
        periods=pd.read_csv(output/"forward_period_metrics.csv")
        period_ranges=periods.loc[periods.scope.eq("model"),["period_kind","horizon","session_start","session_end"]].drop_duplicates()
        records=[]
        for period in period_ranges.to_dict("records"):
            part=calendar.loc[calendar.session_date.between(str(period["session_start"]),str(period["session_end"]))]
            for axis in ("trend","drawdown","volatility","regime"):
                field=axis if axis=="regime" else axis+"_band"
                if field not in part:continue
                for band,group in part.groupby(part[field].fillna("Indisponible")):
                    records.append(dict(stage="forward",axis=axis,band=band,sessions=len(group),share=len(group)/len(part) if len(part) else None,episodes=group.episode_id.nunique() if "episode_id" in group else None,forward_run_id=output.parent.name,period_kind=period["period_kind"],horizon=period["horizon"]))
        composition=pd.DataFrame(records)
    from .runner import _try_submission_mutex
    with _try_submission_mutex(output/".selection_diagnostic.lock") as acquired:
        if not acquired: raise ValueError("selection_diagnostic_already_building")
        _publish(output/FORWARD_FILES[0],merged)
        _publish(output/FORWARD_FILES[1],pd.DataFrame(associations,columns=["period_kind","horizon","criterion","outcome","models","spearman","status","interpretation"]))
        _publish(output/FORWARD_FILES[2],context_population)
        _publish(output/FORWARD_FILES[3],composition)
        if any(not path.exists() or digest(path) != sha for path,sha in source_inputs.items()):
            raise ValueError("selection_forward_sources_changed_during_generation")
        current = _json(output/"selection_generalization_manifest.json")
        payload={**current,"sources":{str(path.relative_to(runs)):sha for path,sha in source_inputs.items()},"context_status":context_status,"schema_version":1,"protocol":PROTOCOL,"source_e2e_run_id":source_id,"forward_run_id":output.parent.name,"source_snapshot_sha256":analysis.get("source_snapshot_sha256"),"t0_manifest_sha256":source_inputs[source/"results"/MANIFEST],"artifact_digests":{n:digest(output/n) for n in FORWARD_FILES}}
        _publish(output/"selection_generalization_manifest.json",payload)
    with _try_submission_mutex(source/"results/.selection_forward_index.lock") as acquired:
        if not acquired: raise ValueError("selection_forward_index_busy_retry")
        path=source/"results/selection_forward_index.json"
        index=_json(path);refs=dict(index.get("runs",{}))
        refs[output.parent.name]={"manifest":str((output/"selection_generalization_manifest.json").relative_to(runs)),"sha256":digest(output/"selection_generalization_manifest.json")}
        _publish(path,{**index,"source_e2e_run_id":source_id,"runs":refs})
    return {"status":"available"}


def optional_selection_diagnostic(spec, output):
    try:
        if spec.job_type.value=="end_to_end": return materialize_selection(Path(output).parent)
        if spec.job_type.value=="forward_simulation": return materialize_generalization(Path(output))
    except Exception as exc:
        # Diagnostic failures never alter original scientific results. Persistent
        # available records are preserved and an explicit attempt status is kept.
        path=Path(output)/"selection_diagnostic_attempt.json"
        _publish(path,{"status":"unavailable","reason":str(exc)})
        return {"status":"unavailable","reason":str(exc)}
