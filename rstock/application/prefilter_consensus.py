"""Occurrence consensus over complete autonomous univariate selections."""
from __future__ import annotations

import hashlib
import json
import pickle
from dataclasses import replace

import pandas as pd

from rstock.calendars import offset_market_session
from rstock.checkpoints import CheckpointManager
from rstock.combinations import canonical_combination_id
from rstock.progress import check_cancellation
from .prefilter_experiments import prefilter_checkpoint_batch_sizes
from .repository import RunRepository


def resolve_consensus_origins(cutoff, calendar, count, step):
    origins = tuple(pd.Timestamp(offset_market_session(cutoff, calendar, i * step)).normalize()
                    for i in range(count))
    current = pd.Timestamp(cutoff).normalize()
    if (not origins or origins[0] != current or len(set(origins)) != count
            or any(origin >= current for origin in origins[1:])):
        raise ValueError("Consensus requires current cutoff and strictly earlier unique origins")
    return origins


def aggregate_consensus(tables, *, origins, targets, predictors, min_occurrences):
    """Only final retained status contributes; scores/ranks are diagnostics."""
    if len(tables) != len(origins) or not 1 <= min_occurrences <= len(origins):
        raise ValueError("Incomplete consensus denominator")
    by_origin = []
    for table in tables:
        if table.duplicated(["Observation", "Predictor"]).any():
            raise ValueError("Duplicate consensus candidate identity")
        by_origin.append({(str(row["Observation"]), str(row["Predictor"])): row
                          for row in table.to_dict("records")})
    rows = []
    retained = {target: [] for target in sorted(targets)}
    for target in sorted(targets):
        for predictor in sorted(predictors):
            if target == predictor:
                continue
            row = {"Observation": target, "Predictor": predictor,
                   "candidate_id": canonical_combination_id(target, "Up", [predictor])}
            count = 0
            for index, (origin, records) in enumerate(zip(origins, by_origin, strict=True), 1):
                item = records.get((target, predictor), {})
                selected = item.get("PrefilterStatus") == "retained"
                count += int(selected)
                row.update({f"cutoff_origin_{index}": pd.Timestamp(origin).date().isoformat(),
                            f"selected_origin_{index}": selected,
                            f"rank_origin_{index}": item.get("PrefilterRank", pd.NA),
                            f"score_origin_{index}": item.get("PrefilterScore", pd.NA),
                            f"status_origin_{index}": item.get("PrefilterStatus", "missing_origin_result"),
                            f"reason_origin_{index}": item.get("PrefilterSkipReason", "")})
            selected = count >= min_occurrences
            row.update(selection_count=count, selection_frequency=count / len(origins),
                       consensus_selected=selected,
                       PrefilterStatus="retained" if selected else "rejected_consensus")
            rows.append(row)
            if selected:
                retained[target].append(predictor)
    return pd.DataFrame(rows), retained


def execute_consensus(spec, output, checkpoint, prepared, sets, predictors, targets,
                      calendars, progress_callback, cancellation_check):
    # Import orchestration helpers lazily; scientific selection stays shared.
    from . import workflows as wf
    config = spec.config
    origins = resolve_consensus_origins(spec.historical_data_cutoff, spec.calendar,
                                      config.temporal_consensus_origins,
                                      config.temporal_consensus_step_sessions)
    repository = RunRepository(spec.config.project_root / "runs")
    run_id = output.parent.name
    fingerprint = repository.configuration_fingerprint(run_id, fallback=spec.fingerprint)
    from .prefilter_progress import TemporalPrefilterProgress
    origin_checkpoints = []
    for origin in origins:
        check_cancellation(cancellation_check)
        date = origin.date().isoformat()
        identity = hashlib.sha256(f"{fingerprint}:{date}:consensus_v1".encode()).hexdigest()
        origin_checkpoint = CheckpointManager(
            output.parent / "checkpoints/consensus_origins" / date,
            run_id=f"{run_id}:origin:{date}", job_type="prefilter_consensus_origin",
            configuration_fingerprint=identity,
            batch_sizes=prefilter_checkpoint_batch_sizes(config),
        )
        wf._ensure_prefilter_checkpoint_protocol(origin_checkpoint, config)
        origin_checkpoints.append(origin_checkpoint)
    progress = TemporalPrefilterProgress(
        progress_callback, [o.date().isoformat() for o in origins], len(sets),
        origin_checkpoints, config.predictor_prefilter_batch_size,
    )
    tables, qualifications, provenance = [], [], []
    for index, origin in enumerate(origins):
        check_cancellation(cancellation_check)
        date = origin.date().isoformat()
        origin_checkpoint = origin_checkpoints[index]
        origin_progress = progress.origin_callback(index)
        try:
            pending_snapshot = False
            if origin_checkpoint.artifact_exists("prepared_snapshot"):
                view, info = origin_checkpoint.load_snapshot()
            else:
                source = _inherited_origin(spec, repository, date)
                if source is not None:
                    view, info = source
                elif index == 0:
                    view = prepared.copy()
                    info = {"predictor_symbols": predictors, "target_symbols": targets,
                            "calendars": calendars, "effective_end_date": view.attrs.get("effective_end_date")}
                else:
                    origin_spec = replace(
                        spec, prefilter_method="single_origin", historical_data_cutoff=date,
                        resolved_market_session_cutoff=date, requested_historical_cutoff=date,
                        prefilter_derivation=None, prepared_snapshot_required=False,
                        prepared_dataset_digest_required=False, source_prepared_dataset_sha256=None,
                    )
                    view, actual_predictors, actual_targets, actual_calendars = wf._prepared_inputs(
                        origin_spec, origin_progress, cancellation_check)
                    info = {"predictor_symbols": actual_predictors, "target_symbols": actual_targets,
                            "calendars": actual_calendars, "effective_end_date": view.attrs.get("effective_end_date")}
                pending_snapshot = True
            if (view.empty or view.index.max() != origin or
                    info["predictor_symbols"] != predictors or info["target_symbols"] != targets
                    or info["calendars"] != calendars):
                raise ValueError("Autonomous origin cutoff or universe differs")
            if pending_snapshot:
                origin_checkpoint.commit_snapshot(view, info)
            if origin_checkpoint.artifact_exists("prefilter_selection"):
                selection = origin_checkpoint.load_artifact("prefilter_selection")
                qualification = origin_checkpoint.load_artifact("prefilter_qualification")
            else:
                result = wf.evaluate_prefilter_walk_forward(
                    view, sets, wf._prefilter_qualification_config(config), market_calendars=calendars,
                    progress_callback=origin_progress, cancellation_check=cancellation_check,
                    checkpoint_manager=origin_checkpoint,
                )
                wf._require_exploitable_prefilter(result)
                qualification = result.qualification
                selection = wf.select_predictors(
                    qualification, view.iloc[:-config.final_holdout_size], targets=targets,
                    candidate_symbols=predictors, config=config,
                    excluded_targets=getattr(result, "excluded_targets", {}),
                )
                origin_checkpoint.commit_artifact("prefilter_qualification", qualification)
                origin_checkpoint.commit_artifact("prefilter_selection", selection)
        except ValueError as error:
            raise ValueError(f"Consensus origin {date} is not executable: {error}") from error
        progress.origin_completed(index)
        snapshot = output.parent / "checkpoints/consensus_origins" / date / "checkpoints/artifacts/prepared_snapshot.pkl"
        provenance.append({"cutoff": date, "prepared_snapshot_sha256": hashlib.sha256(snapshot.read_bytes()).hexdigest(),
                           "prepared_dataset_sha256": wf._persist_prepared_traceability({}, view, spec)["prepared_dataset_sha256"],
                           "history_days": config.model_history_days,
                           "first_date": view.index.min().isoformat(), "last_date": view.index.max().isoformat()})
        table = selection.metrics.copy()
        table["OriginCutoff"] = date
        tables.append(table)
        qualified = qualification.copy()
        qualified["OriginCutoff"] = date
        qualifications.append(qualified)
    progress.finish()
    aggregate, retained = aggregate_consensus(tables, origins=origins, targets=targets,
                                             predictors=predictors,
                                             min_occurrences=config.temporal_consensus_min_occurrences)
    output.mkdir(parents=True, exist_ok=True)
    aggregate.to_csv(output / "temporal_consensus_candidates.csv", index=False)
    aggregate.to_csv(output / "predictor_prefilter.csv", index=False)
    pd.concat(tables, ignore_index=True).to_csv(output / "predictor_prefilter_origins.csv", index=False)
    pd.concat(qualifications, ignore_index=True).to_csv(output / "prefilter_qualification.csv", index=False)
    counts = aggregate["selection_count"].value_counts().to_dict()
    distribution = {str(n): int(counts.get(n, 0)) for n in range(len(origins) + 1)}
    trace = wf._persist_prepared_traceability({}, prepared, spec)
    manifest = {
        "schema_version": 2, "prefilter_method": "temporal_consensus",
        **wf._persist_prefilter_training(output, origin_checkpoints, config),
        "prepared_dataset_as_of": spec.historical_data_cutoff,
        "prepared_dataset_sha256": trace["prepared_dataset_sha256"],
        "xgboost_parameters": wf.prefilter_xgboost_snapshot(config),
        "origin_cutoffs": [origin.date().isoformat() for origin in origins],
        "origins": provenance, "step_sessions": config.temporal_consensus_step_sessions,
        "min_occurrences": config.temporal_consensus_min_occurrences,
        "selection_definition": "final retained after origin qualification, Top N and correlation",
        "rank_definition": "PrefilterRank among eligible univariate predictors",
        "selection_frequency_definition": "final selection_count / requested origins",
        "predictors_by_target": retained, "occurrence_distribution": distribution,
        "distinct_observed_candidates": len(aggregate),
        "distinct_selected_at_least_once": int((aggregate["selection_count"] > 0).sum()),
        "evaluated_candidates": len(aggregate),
        "retained_predictors": sum(map(len, retained.values())),
    }
    (output / "predictor_prefilter.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    checkpoint.phase_completed("predictor_prefilter_walk_forward")
    checkpoint.phase_completed("predictor_prefilter_selection")
    return {"job_type": spec.job_type.value, "traceability": trace, **manifest,
            "result_files": sorted(path.name for path in output.iterdir()),
            "checkpoint_manifest": "checkpoints/manifest.json"}


def _inherited_origin(spec, repository, date):
    """Reuse verified origin data from a derived consensus's frozen source."""
    if not spec.prefilter_derivation:
        return None
    source = spec.prefilter_derivation["source_run_id"]
    source_spec = repository.load_spec(source)
    if source_spec.prefilter_method != "temporal_consensus":
        return None
    path = repository.run_directory(source) / "results/predictor_prefilter.json"
    report = json.loads(path.read_text(encoding="utf-8"))
    reference = next((item for item in report["origins"] if item["cutoff"] == date), None)
    if reference is None:
        return None
    root = repository.run_directory(source) / "checkpoints/consensus_origins" / date
    raw = root / "checkpoints/artifacts/prepared_snapshot.pkl"
    if hashlib.sha256(raw.read_bytes()).hexdigest() != reference["prepared_snapshot_sha256"]:
        raise ValueError("Inherited consensus origin snapshot changed")
    fingerprint = repository.configuration_fingerprint(source)
    metadata = json.loads((root / "checkpoints/artifacts/prepared_snapshot.json").read_text(encoding="utf-8"))
    expected = hashlib.sha256(f"{fingerprint}:{date}:consensus_v1".encode()).hexdigest()
    if metadata.get("configuration_fingerprint") != expected or metadata.get("sha256") != reference["prepared_snapshot_sha256"]:
        raise ValueError("Inherited consensus origin provenance changed")
    payload = pickle.loads(raw.read_bytes())
    return payload["prepared"], payload["metadata"]
