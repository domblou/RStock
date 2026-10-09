"""Optional post-science context diagnostics; immutable shared benchmark evidence."""
from __future__ import annotations

import logging
from dataclasses import asdict
from pathlib import Path

import pandas as pd
import exchange_calendars as xcals

from .forward_diagnostic import _json, _key, _publish, _stage_ids, digest, REFERENCE, ANALYSIS
from .market_context import (
    ContextProtocol, DIAGNOSTIC_MANIFEST, CONTEXT_MANIFEST, METRICS_FILE,
    ROBUSTNESS_FILE, adjusted_snapshot, aggregate_context, normalize_predictions,
    publish_context, validate_context,
)
from rstock.data import YahooFinanceProvider


LOGGER = logging.getLogger(__name__)
ATTEMPT_MANIFEST = "context_diagnostic_attempt.json"


INPUTS = {
    "walk_forward": ("predictions.csv", "walk_forward", "persisted_wf_up"),
    "threshold_calibration": ("development_probabilities.csv", "development_calibrated", "frozen_combined_up_down"),
    "holdout_evaluation": ("holdout_predictions.csv", "holdout", "frozen_combined_up_down"),
    "forward_simulation": ("forward_observations.csv", "forward", "persisted_forward_combined"),
}


def _paths(spec, output):
    runs = spec.config.project_root / "runs"
    owner = runs / (spec.source_end_to_end_run or spec.source_experiment_run or output.parent.name)
    stages = _stage_ids(owner)
    wf = output if spec.job_type.value == "walk_forward" else runs / (
        spec.source_walk_forward_run or stages.get("walk_forward", owner.name)
    ) / "results"
    # The physical inherited WF owns the context, so qualification derivatives
    # share their parent's benchmark and fixed development-only boundaries.
    store_owner = wf.parent if (wf / "windows.csv").exists() else owner
    return runs, owner, stages, wf, store_owner / "diagnostics/market_context"


def prediction_groups(path, chunksize=100_000):
    """Read bounded contiguous scientific sets; reject ambiguous historical layout.

    RStock writers concatenate complete sets. Carry across CSV chunk boundaries,
    including both directions, so no probability pairing is lost.
    """
    column = "source_model_id" if "source_model_id" in pd.read_csv(path, nrows=0) else "Set"
    pending = pd.DataFrame()
    seen = set()
    for chunk in pd.read_csv(path, chunksize=chunksize):
        chunk = pd.concat([pending, chunk], ignore_index=True) if not pending.empty else chunk
        blocks = chunk[column].ne(chunk[column].shift()).cumsum()
        groups = list(chunk.groupby(blocks, sort=False))
        for _, group in groups[:-1]:
            key = str(group[column].iloc[0])
            if key in seen: raise ValueError("context_noncontiguous_model_predictions")
            seen.add(key)
            yield group
        pending = groups[-1][1] if groups else pd.DataFrame()
    if not pending.empty:
        key = str(pending[column].iloc[0])
        if key in seen: raise ValueError("context_noncontiguous_model_predictions")
        yield pending


def _acquisition_end(spec, owner, observed_end, wf):
    """Prefetch the known E2E period; aggregates still use only their own dates."""
    if spec.job_type.value == "forward_simulation" or not (owner / "orchestration/pipeline.json").is_file():
        return observed_end
    saved = _json(owner / "config.json")
    cutoff = (getattr(spec, "resolved_market_session_cutoff", None)
              or getattr(spec, "historical_data_cutoff", None)
              or saved.get("resolved_market_session_cutoff") or saved.get("historical_data_cutoff"))
    if not cutoff:
        cutoff = _json(wf / "run_configuration.json").get("traceability", {}).get("prepared_market_last_date")
    return max(observed_end, pd.Timestamp(cutoff).normalize()) if cutoff else observed_end


def materialize_context_diagnostic(spec, output: Path, *, provider=None):
    if not spec.config.market_context_enabled or spec.job_type.value not in INPUTS:
        return None
    protocol = ContextProtocol.from_config(spec.config)
    filename, stage, rule = INPUTS[spec.job_type.value]
    source = output / filename
    if not source.exists():
        raise ValueError("context_predictions_unavailable")
    runs, owner, stages, wf, store = _paths(spec, output)
    windows_path = wf / "windows.csv"
    if not windows_path.exists():
        raise ValueError("context_development_reference_unavailable")
    windows = pd.read_csv(windows_path)
    if windows.empty or not {"TrainStart", "TestStart"}.issubset(windows):
        raise ValueError("context_development_reference_unavailable")
    reference_start = pd.to_datetime(windows.TrainStart).min().strftime("%Y-%m-%d")
    reference_end = pd.to_datetime(windows.TestStart).min().strftime("%Y-%m-%d")
    date_column = "session_date" if stage == "forward" else "Date"
    ranges = [pd.to_datetime(chunk[date_column]).agg(["min", "max"]) for chunk in pd.read_csv(source, usecols=[date_column], chunksize=100_000)]
    if not ranges: raise ValueError("context_predictions_empty")
    last = max(part["max"] for part in ranges).normalize()
    first = min(pd.Timestamp(reference_start), min(part["min"] for part in ranges).normalize())
    inputs = {str(source.relative_to(runs)): digest(source), str(windows_path.relative_to(runs)): digest(windows_path)}
    threshold_path = output / "selected_thresholds_by_set.json"
    if not threshold_path.exists():
        threshold_path = runs / (spec.source_threshold_calibration_run or stages.get("threshold_calibration", owner.name)) / "results/selected_thresholds_by_set.json"
    thresholds = None
    if rule == "frozen_combined_up_down" and threshold_path.exists():
        thresholds = _json(threshold_path)
        inputs[str(threshold_path.relative_to(runs))] = digest(threshold_path)
    periods_path = output / "forward_period_metrics.csv"
    persisted_periods = None
    if stage == "forward" and periods_path.exists():
        persisted_periods = pd.read_csv(periods_path)
        inputs[str(periods_path.relative_to(runs))] = digest(periods_path)
    manifest_path = output / DIAGNOSTIC_MANIFEST
    acquisition_end = _acquisition_end(spec, owner, last, wf)
    candidates = []
    for path in (store / protocol.identifier / "revisions").glob("*/" + CONTEXT_MANIFEST):
        manifest = validate_context(path)
        if manifest["reference_start"] == reference_start and manifest["reference_end_exclusive"] == reference_end:
            candidates.append((manifest["last_session"], path, manifest))
    candidates.sort(key=lambda row: (row[0], str(row[1])))
    containing = next((row for row in candidates if row[0] >= acquisition_end.strftime("%Y-%m-%d") and row[2]["first_session"] <= first.strftime("%Y-%m-%d")), None)
    if containing:
        context_manifest = containing[1]
    else:
        parent = candidates[-1] if candidates else None
        if parent:
            frozen = pd.read_csv(parent[1].parent / "spy_adjusted_snapshot.csv", index_col="Date", parse_dates=True, float_precision="round_trip")
            acquisition_start = frozen.index.max() - pd.Timedelta(days=30)
            first = min(first, pd.Timestamp(parent[2]["first_session"]))
        else:
            acquisition_start = first - pd.Timedelta(days=2 * max(protocol.trend_sessions, protocol.drawdown_sessions, protocol.volatility_sessions, 252 if protocol.regime_version else 0) + 100)
        service = provider or YahooFinanceProvider()
        # Explicit dedicated SPY acquisition, independent of the predictor universe
        # and of the mutable market cache. No raw-price fallback.
        from .market_context_acquisition import acquire_spy
        snapshot, acquisition = acquire_spy(service, acquisition_start.date(), acquisition_end.date(),
            store / protocol.identifier / "acquisitions", runs, parent=parent[1] if parent else None)
        sessions = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range(first, acquisition_end)).tz_localize(None)
        context_manifest = publish_context(store, snapshot, sessions, protocol, reference_start, reference_end,
            parent_manifest=parent[1] if parent else None, provider=service.source_name, acquisition=acquisition)
    context_meta = validate_context(context_manifest)
    context = pd.read_csv(context_manifest.parent / "market_context.csv")
    snapshot_path = owner / "results/forward_model_snapshot.json"
    reference_path = owner / "results" / REFERENCE
    origins = {}
    t0_reference = None
    if stage == "forward":
        if not snapshot_path.exists():
            raise ValueError("context_forward_model_identity_unavailable")
        analysis = _json(output / ANALYSIS)
        if analysis.get("source_snapshot_sha256") != digest(snapshot_path):
            raise ValueError("context_forward_model_snapshot_integrity_error")
        models = _json(snapshot_path).get("models", [])
        identities = {model["source_model_id"]: _key(model["set"], model["direction"]) for model in models}
        inputs[str(snapshot_path.relative_to(runs))] = digest(snapshot_path)
    if reference_path.exists() and snapshot_path.exists():
        reference = _json(reference_path)
        if reference.get("source_snapshot_sha256") == digest(snapshot_path):
            origins = {model["canonical_combination_id"]: model.get("origin", "unavailable") for model in reference.get("models", [])}
            inputs[str(reference_path.relative_to(runs))] = digest(reference_path)
            t0_reference = {"path": str(reference_path.relative_to(runs)), "sha256": digest(reference_path)}
    if manifest_path.exists():
        prior = _json(manifest_path)
        if prior.get("status") == "available" and prior.get("input_digests") == inputs and prior.get("protocol_id") == protocol.identifier:
            try:
                return load_context_diagnostic(output, runs)[0]
            except (ValueError, OSError):
                pass  # Interrupted publication: rebuild from verified evidence.
    metric_parts, summary_parts = [], []
    for raw in prediction_groups(source):
        rows = normalize_predictions(raw, signal_rule=rule, thresholds=thresholds)
        if stage == "forward":
            rows["model_key"] = rows.source_model_id.map(identities)
            if rows.model_key.isna().any(): raise ValueError("context_forward_model_identity_unavailable")
        if persisted_periods is None:
            parts = [("full_run", 0, rows)]
        else:
            model_id = str(raw.source_model_id.iloc[0])
            table = persisted_periods.loc[persisted_periods.scope.eq("model") & persisted_periods.source_model_id.astype(str).eq(model_id)]
            parts = []
            for record in table.to_dict("records"):
                start, end = record.get("session_start"), record.get("session_end")
                subset = rows.loc[pd.to_datetime(rows.Date).between(pd.Timestamp(start), pd.Timestamp(end))] if pd.notna(start) and pd.notna(end) else rows.iloc[:0]
                if not subset.empty: parts.append((record["period_kind"], record["horizon"], subset))
        for kind, horizon, subset in parts:
            metric, summary = aggregate_context(subset, context, stage=stage, signal_rule=rule)
            for table in (metric, summary):
                if not table.empty: table["period_kind"], table["horizon"] = kind, horizon
            metric_parts.append(metric); summary_parts.append(summary)
    metrics = pd.concat(metric_parts, ignore_index=True) if metric_parts else pd.DataFrame()
    robustness = pd.concat(summary_parts, ignore_index=True) if summary_parts else pd.DataFrame()
    if metrics.empty:
        raise ValueError("context_no_evaluable_models")
    metrics["qualification_origin"] = metrics.model_key.map(origins).fillna("unavailable")
    robustness["qualification_origin"] = robustness.model_key.map(origins).fillna("unavailable")
    references = {}
    for stage_key, run_id in stages.items():
        path = runs / run_id / "results" / DIAGNOSTIC_MANIFEST
        if stage_key in INPUTS and path.exists() and path != manifest_path:
            references[stage_key] = {"path": str(path.relative_to(runs)), "sha256": digest(path)}
    from .runner import _try_submission_mutex
    with _try_submission_mutex(output / ".context_diagnostic.lock") as acquired:
        if not acquired:
            raise ValueError("context_diagnostic_already_building")
        # Re-read the persistent record before final publication. Never replace
        # another writer's available evidence using an old in-memory manifest.
        existing = _json(manifest_path) if manifest_path.exists() else {}
        if existing.get("status") == "available" and existing.get("input_digests") == inputs and existing.get("protocol_id") == protocol.identifier:
            try:
                return load_context_diagnostic(output, runs)[0]
            except (ValueError, OSError):
                pass
        _publish(output / METRICS_FILE, metrics)
        _publish(output / ROBUSTNESS_FILE, robustness)
        manifest = {**existing, "schema_version": 1, "status": "available", "stage": stage, "protocol_id": protocol.identifier,
            "protocol": asdict(protocol), "standard_protocol": protocol.standard,
            "context_manifest": str(context_manifest.relative_to(runs)), "context_manifest_sha256": digest(context_manifest),
            "context_revision": context_meta["revision"], "acquisition": context_meta.get("acquisition"),
            "comparability": {"context_store": str(store.relative_to(runs)), "reference_context_sha256": context_meta.get("reference_context_sha256"), "protocol_id": context_meta["protocol_id"], "reference_start": context_meta["reference_start"], "reference_end_exclusive": context_meta["reference_end_exclusive"], "boundaries": context_meta["boundaries"], "regime_version": protocol.regime_version},
            "input_digests": inputs,
            "stage_references": references, "source_e2e_run_id": owner.name,
            "t0_reference": t0_reference,
            "artifact_digests": {name: digest(output / name) for name in (METRICS_FILE, ROBUSTNESS_FILE)},
            "interpretation": "Descriptive terciles, not economic regimes. Calibration-selected thresholds are not independent WF evaluation."}
        if any(not (runs / name).is_file() or digest(runs / name) != sha for name,sha in inputs.items()):
            raise ValueError("context_inputs_changed_during_generation")
        _publish(manifest_path, manifest)
    return manifest


def optional_context_diagnostic(spec, output):
    """Every diagnostic failure is nonfatal, including failure to save its warning."""
    try:
        result = materialize_context_diagnostic(spec, output)
        if result is not None:
            from .runner import _try_submission_mutex
            with _try_submission_mutex(Path(output) / ".context_diagnostic.lock") as acquired:
                if not acquired: raise ValueError("context_diagnostic_already_building")
                path = Path(output) / ATTEMPT_MANIFEST
                _publish(path, {**_json(path), "status": "available", "reason": None,
                    "protocol_id": result.get("protocol_id"), "acquisition": result.get("acquisition")})
        return result
    except Exception as exc:
        reason = str(exc)
        LOGGER.warning("Diagnostic SPY incomplet; traitement scientifique conservé: %s", reason)
        result = {"schema_version": 1, "status": "unavailable", "reason": reason,
            "warning": "SPY diagnostic incomplete; scientific results are unaffected",
            "protocol_id": ContextProtocol.from_config(spec.config).identifier,
            "acquisition": getattr(exc, "evidence", None)}
        try:
            from .runner import _try_submission_mutex
            with _try_submission_mutex(Path(output) / ".context_diagnostic.lock") as acquired:
                if acquired:
                    path = Path(output) / DIAGNOSTIC_MANIFEST
                    current = _json(path)
                    _publish(Path(output) / ATTEMPT_MANIFEST, {**_json(Path(output) / ATTEMPT_MANIFEST), **result})
                    if current.get("status") != "available":
                        _publish(path, {**current, **result})
        except Exception:
            LOGGER.warning("Unable to persist SPY diagnostic warning", exc_info=True)
        return result


def _load_context_diagnostic(output: Path, runs: Path):
    manifest = _json(output / DIAGNOSTIC_MANIFEST)
    if manifest.get("status") != "available":
        return manifest, pd.DataFrame(), pd.DataFrame(), pd.DataFrame()
    path = (runs / manifest["context_manifest"]).resolve()
    if not path.is_relative_to(runs.resolve()) or digest(path) != manifest["context_manifest_sha256"]:
        raise ValueError("context_manifest_integrity_error")
    validate_context(path)
    for name in (METRICS_FILE, ROBUSTNESS_FILE):
        if digest(output / name) != manifest.get("artifact_digests", {}).get(name):
            raise ValueError("context_diagnostic_integrity_error")
    return manifest, pd.read_csv(path.parent / "market_context.csv"), pd.read_csv(output / METRICS_FILE), pd.read_csv(output / ROBUSTNESS_FILE)



from functools import lru_cache

@lru_cache(maxsize=8)
def _cached_context(output, runs, stamps):
    return _load_context_diagnostic(Path(output), Path(runs))


def load_context_diagnostic(output: Path, runs: Path):
    """Validate once per persisted revision; never load predictions for display."""
    output, runs = Path(output), Path(runs)
    meta = _json(output / DIAGNOSTIC_MANIFEST)
    paths = [output / DIAGNOSTIC_MANIFEST]
    if meta.get("status") == "available":
        context_path = (runs / meta["context_manifest"]).resolve()
        if not context_path.is_relative_to(runs.resolve()): raise ValueError("context_manifest_integrity_error")
        paths += [context_path, *[output / name for name in (METRICS_FILE, ROBUSTNESS_FILE)]]
        context_meta = _json(context_path)
        paths += [context_path.parent / name for name in context_meta.get("artifact_digests",{})]
    stamps = tuple((str(path),path.stat().st_mtime_ns,path.stat().st_size) for path in paths)
    return _cached_context(str(output),str(runs),stamps)
