"""Derived T0/Forward diagnostics. No fitting, downloads or qualification decisions.

The shared T0 sidecar is immutable. Forward economics remain owned by the
temporal analysis; this module only joins them for display/comparison.
"""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from typing import Any
from uuid import uuid4

import numpy as np
import pandas as pd

from rstock.combinations import canonical_combination_id_from_set
from rstock.evaluation import classification_metrics


PROTOCOL = "forward_diagnostic_v1"
REFERENCE = "forward_t0_reference.json"
MANIFEST = "forward_diagnostic_manifest.json"
METRICS = "forward_diagnostic_metrics.csv"
BINS = "forward_probability_bins.csv"
ANALYSIS = "forward_analysis_manifest.json"
ORIGINS = {"normal": "E2E normal", "common": "Commun au parent",
           "additional": "Supplémentaire admis", "removed": "Retiré",
           "unavailable": "Origine indisponible"}
BIN_EDGES = (0.0, 0.2, 0.4, 0.6, 0.8, 1.0)


def digest(path: Path) -> str:
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def _json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        return value if isinstance(value, dict) else {}
    except (OSError, ValueError):
        return {}


def _clean(value: Any) -> Any:
    if isinstance(value, dict):
        return {str(k): _clean(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_clean(v) for v in value]
    if isinstance(value, np.generic):
        value = value.item()
    if isinstance(value, float) and not np.isfinite(value):
        return None
    return value


def _publish(path: Path, value: Any, *, immutable: bool = False) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{uuid4().hex}.tmp")
    try:
        if isinstance(value, pd.DataFrame):
            value.to_csv(temporary, index=False)
        else:
            temporary.write_text(json.dumps(_clean(value), ensure_ascii=False,
                                            indent=2, allow_nan=False) + "\n", encoding="utf-8")
        if immutable:
            try:
                os.link(temporary, path)  # atomic create; never overwrite another writer
            except FileExistsError:
                pass
        else:
            temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def _stage_ids(run: Path) -> dict[str, str]:
    from .end_to_end import effective_stage_run_id

    manifest = _json(run / "orchestration/pipeline.json")
    result = {}
    for stage in manifest.get("stages", []):
        try:
            key = stage["stage_key"]
            resolved = effective_stage_run_id(manifest, key)
            if resolved:
                result[key] = resolved
        except (KeyError, ValueError, TypeError):
            continue
    return result


def _key(set_name: str, direction: str) -> str:
    return canonical_combination_id_from_set(set_name, direction)


def _decisions(run: Path, stages: dict[str, str]) -> dict[str, dict[str, Any]] | None:
    stage = stages.get("promotion_qualification")
    if not stage:
        return None
    path = run.parent / stage / "results/qualification.json"
    expected = _stage_expected(run, "promotion_qualification", "results/qualification.json")
    if expected and (not path.is_file() or digest(path) != expected):
        return None
    payload = _json(path)
    if not isinstance(payload.get("decisions"), list):
        return None
    result = {}
    for row in payload["decisions"]:
        try:
            key = _key(row["Combinaison"], row["Direction"])
        except (KeyError, TypeError, ValueError):
            return None
        if key in result:
            return None
        result[key] = row
    return result


def _stage_expected(run: Path, key: str, relative: str) -> str | None:
    manifest = _json(run / "orchestration/pipeline.json")
    stage = next((s for s in manifest.get("stages", []) if s.get("stage_key") == key), {})
    return stage.get("artifact_digests", {}).get(relative)


def _candidate(row: dict[str, Any]) -> bool:
    return row.get("candidate") is True or row.get("Statut promotion") == "Candidat"


def _source_file(run: Path, relative: str, sources: dict[str, str],
                 expected: str | None = None) -> Path | None:
    path = run / relative
    if not path.is_file():
        return None
    actual = digest(path)
    sources[str(path.relative_to(run.parent)).replace("\\", "/")] = actual
    return path if expected is None or actual == expected else None


def _csv(path: Path | None) -> pd.DataFrame:
    if path is None:
        return pd.DataFrame()
    try:
        return pd.read_csv(path)
    except (OSError, ValueError, pd.errors.ParserError):
        return pd.DataFrame()


def _select(frame: pd.DataFrame, set_name: str, direction: str | None = None) -> pd.DataFrame:
    if "Set" not in frame:
        return frame.iloc[:0]
    # Match scientific identity, while retaining original feature order in the snapshot.
    wanted = _key(set_name, direction or "Up")
    def matches(value: Any) -> bool:
        try:
            return _key(str(value), direction or "Up") == wanted
        except (ValueError, TypeError):
            return False
    matching_sets = [value for value in frame["Set"].unique() if matches(value)]
    result = frame.loc[frame["Set"].isin(matching_sets)]
    if direction is not None:
        if "Direction" not in result:
            return result.iloc[:0]
        result = result.loc[result["Direction"].eq(direction)]
    return result


def _bool(series: pd.Series) -> pd.Series:
    return series.map(lambda v: v is True or str(v).lower() in {"true", "1"})


def probability_metrics(frame: pd.DataFrame) -> dict[str, Any]:
    """All evaluable rows for probability quality; the actual combined signal for recall."""
    required = {"prediction_probability", "outcome", "signal"}
    if not required.issubset(frame) or frame.empty:
        return {"availability": "unavailable_missing_probabilities"}
    scores = pd.to_numeric(frame["prediction_probability"], errors="coerce")
    actual = pd.to_numeric(frame["outcome"], errors="coerce")
    if (not np.isfinite(scores).all() or not scores.between(0, 1).all()
            or not actual.isin([0, 1]).all()):
        return {"availability": "unavailable_invalid_probabilities"}
    signal = _bool(frame["signal"])
    metrics = classification_metrics(actual, signal.astype(int), scores)
    return {
        "availability": "available", "observations": len(frame),
        "positives": int(actual.sum()), "negatives": int((actual == 0).sum()),
        "auc": metrics.roc_auc, "auc_status": "available" if metrics.roc_auc is not None else "unavailable_single_class",
        "brier": float(((scores - actual) ** 2).mean()),
        "prevalence": metrics.prevalence,
        "precision": metrics.precision if signal.any() else None,
        "recall": metrics.recall if actual.sum() else None,
        "f1": metrics.f1 if actual.sum() or signal.any() else None,
        "signal_rate": float(signal.mean()), "signals": int(signal.sum()),
        "mean_probability": float(scores.mean()),
        "signal_mean_probability": float(scores.loc[signal].mean()) if signal.any() else None,
        "probability_bias": float((scores - actual).mean()),
    }


def probability_bins(frame: pd.DataFrame, availability: str | None = None) -> list[dict[str, Any]]:
    if (availability or probability_metrics(frame)["availability"]) != "available":
        return []
    scores = pd.to_numeric(frame["prediction_probability"])
    result = []
    for index, (low, high) in enumerate(zip(BIN_EDGES[:-1], BIN_EDGES[1:])):
        selected = frame.loc[(scores >= low) & ((scores <= high) if high == 1 else (scores < high))]
        signals = selected.loc[_bool(selected["signal"])]
        returns = pd.to_numeric(signals.get("directional_return", pd.Series(dtype=float)), errors="coerce")
        result.append({"bin": index, "lower": low, "upper": high,
                       "observations": len(selected), "signals": len(signals),
                       "mean_probability": pd.to_numeric(selected["prediction_probability"]).mean(),
                       "observed_frequency": pd.to_numeric(selected["outcome"]).mean(),
                       "signal_precision": pd.to_numeric(signals["outcome"]).mean(),
                       "signal_mean_return": returns.mean()})
    return _clean(result)


def _holdout_reference(frame: pd.DataFrame, model: dict[str, Any]) -> dict[str, Any]:
    if {"UpProbability", "DownProbability", "UpTarget", "DownTarget", "Date", "Set"}.issubset(frame):
        # Explicit adapter for persisted wide historical prediction records.
        frame = pd.concat([frame.assign(Direction=d, Probability=frame[f"{d}Probability"], Target=frame[f"{d}Target"])
                           for d in ("Up", "Down")], ignore_index=True)
    subset = _select(frame, model["set"])
    required = {"Date", "Direction", "Probability", "Target"}
    if subset.empty or not required.issubset(subset):
        return {"availability": "unavailable_missing_holdout_predictions"}
    if ("Window" in subset and subset["Window"].nunique() != 1
            or subset.duplicated(["Date", "Direction"]).any()):
        return {"availability": "unavailable_ambiguous_holdout"}
    up = subset.loc[subset["Direction"].eq("Up")].set_index("Date")
    down = subset.loc[subset["Direction"].eq("Down")].set_index("Date")
    if up.empty or set(up.index) != set(down.index):
        return {"availability": "unavailable_unpaired_holdout"}
    if pd.to_datetime(up.index, errors="coerce").isna().any():
        return {"availability": "unavailable_invalid_holdout_dates"}
    if model.get("direction") != "Up":
        return {"availability": "unavailable_unsupported_direction"}
    if model.get("up_threshold") is None or model.get("down_threshold") is None:
        return {"availability": "unavailable_missing_thresholds"}
    down_scores = pd.to_numeric(down["Probability"].reindex(up.index), errors="coerce")
    if not down_scores.between(0, 1).all():
        return {"availability": "unavailable_invalid_probabilities"}
    rows = pd.DataFrame({"prediction_probability": up["Probability"], "outcome": up["Target"],
                         "signal": (pd.to_numeric(up["Probability"], errors="coerce") >= model["up_threshold"])
                         & (down_scores < model["down_threshold"])})
    if "IntradayReturn" in up:
        rows["directional_return"] = pd.to_numeric(up["IntradayReturn"], errors="coerce")
    values = probability_metrics(rows)
    signal_returns = rows.loc[rows["signal"], "directional_return"] if "directional_return" in rows else pd.Series(dtype=float)
    economic_available = "directional_return" in rows and np.isfinite(rows["directional_return"]).all()
    values.update({"mean_return": float(signal_returns.mean()) if economic_available and len(signal_returns) else None,
                   "economic_availability": "available" if economic_available else "unavailable_missing_returns",
                   "start": str(up.index.min()), "end": str(up.index.max()),
                   "probability_bins": probability_bins(rows, values["availability"]),
                   "signal_rule": "up>=up_threshold and down<down_threshold"})
    return _clean(values)


def build_t0_reference(run: Path) -> dict[str, Any]:
    """Read persisted evidence only, including parent decisions for derived cohorts."""
    sources: dict[str, str] = {}
    snapshot_path = _source_file(run, "results/forward_model_snapshot.json", sources)
    if snapshot_path is None:
        raise ValueError("forward_snapshot_missing")
    snapshot = _json(snapshot_path)
    config = _json(_source_file(run, "config.json", sources) or run / "config.json")
    _source_file(run, "orchestration/pipeline.json", sources)
    stages = _stage_ids(run)
    decisions = _decisions(run, stages)
    qualification_run = run.parent / stages.get("promotion_qualification", run.name)
    qualification_path = _source_file(qualification_run, "results/qualification.json", sources,
                                      _stage_expected(run, "promotion_qualification", "results/qualification.json"))
    qualification = _json(qualification_path) if qualification_path else {}
    expected = qualification.get("source_artifact_digests", {})
    wf_run = run.parent / stages.get("walk_forward", run.name)
    holdout_run = run.parent / stages.get("holdout_evaluation", stages.get("threshold_calibration", run.name))
    wf = _csv(_source_file(wf_run, "results/qualification.csv", sources,
                           _stage_expected(run, "walk_forward", "results/qualification.csv")))
    holdout = _csv(_source_file(holdout_run, "results/holdout_metrics.csv", sources, expected.get("holdout_metrics.csv")))
    predictions = _csv(_source_file(holdout_run, "results/holdout_predictions.csv", sources, expected.get("holdout_predictions.csv")))
    if stages.get("promotion_qualification") and qualification_path is None and (qualification_run / "results/qualification.json").exists():
        # A failed qualification digest cannot authorize unverified downstream evidence.
        holdout, predictions = pd.DataFrame(), pd.DataFrame()
    parent_id = (config.get("derivation") or {}).get("source_end_to_end_run_id")
    parent_decisions = None
    parent_models: dict[str, Any] = {}
    if parent_id:
        parent = run.parent / parent_id
        _source_file(parent, "config.json", sources)
        _source_file(parent, "orchestration/pipeline.json", sources)
        parent_stages = _stage_ids(parent)
        parent_decisions = _decisions(parent, parent_stages)
        _source_file(run.parent / parent_stages.get("promotion_qualification", parent_id), "results/qualification.json", sources)
        parent_snapshot = _source_file(parent, "results/forward_model_snapshot.json", sources)
        for model in (_json(parent_snapshot).get("models", []) if parent_snapshot else []):
            parent_models[_key(model["set"], model["direction"])] = model
    entries = []
    models = list(snapshot.get("models", []))
    current_keys = {_key(m["set"], m["direction"]) for m in models}
    removed_keys = {key for key, row in (parent_decisions or {}).items() if _candidate(row)} - current_keys
    for key in sorted(removed_keys):
        # Removed models are never assigned invented Forward observations.
        parent_model = parent_models.get(key)
        if parent_model:
            models.append(dict(parent_model, source_model_id=None, removed=True))
        else:
            parts = json.loads(key)
            models.append({"set": json.dumps([parts[0], *parts[2:]], separators=(",", ":")),
                           "target": parts[0], "direction": parts[1], "source_model_id": None, "removed": True})
    for model in models:
        key = _key(model["set"], model["direction"])
        origin = "normal" if not parent_id else "unavailable"
        if model.get("removed"):
            origin = "removed"
        elif parent_decisions is not None:
            origin = "common" if _candidate(parent_decisions.get(key, {})) else "additional"
        wf_rows = _select(wf, model["set"])
        holdout_rows = _select(holdout, model["set"], model["direction"])
        # A removed candidate can use current T0 data only when the data stages
        # are inherited unchanged; otherwise its parent evidence is separate.
        evidence_same = not model.get("removed") or (
            stages.get("holdout_evaluation") == parent_stages.get("holdout_evaluation")
            and stages.get("threshold_calibration") == parent_stages.get("threshold_calibration")
            and stages.get("walk_forward") == parent_stages.get("walk_forward"))
        comparable = _holdout_reference(predictions, model) if evidence_same else {"availability": "unavailable_different_parent_evidence"}
        entries.append({"source_model_id": model.get("source_model_id"), "canonical_combination_id": key,
                        "set": model["set"], "target": model["target"], "direction": model["direction"],
                        "origin": origin, "forward_available": not model.get("removed", False),
                        "up_threshold": model.get("up_threshold"), "down_threshold": model.get("down_threshold"),
                        "wf": wf_rows.iloc[0].to_dict() if len(wf_rows) == 1 and evidence_same else {},
                        "holdout_qualification": holdout_rows.iloc[0].to_dict() if len(holdout_rows) == 1 and evidence_same else {},
                        "holdout_comparable": comparable,
                        "qualification_decision": (decisions or {}).get(key),
                        "parent_decision": (parent_decisions or {}).get(key)})
    return _clean({"protocol": PROTOCOL, "schema_version": 1, "source_e2e_run_id": run.name,
                   "source_snapshot_sha256": digest(snapshot_path), "cutoff_t0": snapshot.get("resolved_market_session_cutoff"),
                   "parent_e2e_run_id": parent_id, "stages": stages, "source_artifact_digests": sources,
                   "qualification_overrides": (config.get("derivation") or {}).get("overrides", []),
                   "interpretation": "Qualification evidence, not independent evaluation of the final refitted booster",
                   "models": entries})


def ensure_t0_reference(run: Path) -> tuple[dict[str, Any], Path]:
    path = run / "results" / REFERENCE
    if not path.exists():
        _publish(path, build_t0_reference(run), immutable=True)
    # Re-read the winner of a concurrent publication, never a stale local object.
    reference = _json(path)
    if (reference.get("protocol") != PROTOCOL or reference.get("source_e2e_run_id") != run.name
            or reference.get("source_snapshot_sha256") != digest(run / "results/forward_model_snapshot.json")):
        raise ValueError("forward_t0_reference_mismatch")
    return reference, path


def analyze_diagnostic(observations: pd.DataFrame, periods: pd.DataFrame,
                       reference: dict[str, Any]) -> tuple[pd.DataFrame, pd.DataFrame]:
    rows, bins = [], []
    concentration = {}
    observations = observations.copy()
    observations["session_date"] = pd.to_datetime(observations["session_date"], errors="coerce")
    by_id = {str(m["source_model_id"]): m for m in reference["models"] if m.get("source_model_id")}
    for _, period in periods.loc[periods["scope"].eq("model")].iterrows():
        model_id = str(period["source_model_id"])
        model = by_id.get(model_id)
        if model is None:
            raise ValueError("forward_diagnostic_unknown_model")
        subset = observations.loc[observations["source_model_id"].astype(str).eq(model_id)
                                  & observations["session_date"].between(pd.Timestamp(period["session_start"]), pd.Timestamp(period["session_end"]))]
        values = probability_metrics(subset)
        counts = subset.loc[_bool(subset["signal"])].groupby("session_date").size() if "signal" in subset else pd.Series(dtype=int)
        same_period = periods.loc[periods["period_kind"].eq(period["period_kind"]) & periods["horizon"].eq(period["horizon"])]
        total_signals = same_period.loc[same_period["scope"].eq("run"), "signals"].sum()
        target_signals = same_period.loc[same_period["scope"].eq("model") & same_period["target"].eq(model["target"]), "signals"].sum()
        period_key = (period["period_kind"], int(period["horizon"]))
        if period_key not in concentration:
            all_rows = observations.loc[observations["session_date"].between(pd.Timestamp(period["session_start"]), pd.Timestamp(period["session_end"]))]
            daily_signals = all_rows.loc[_bool(all_rows["signal"])].groupby("session_date").size()
            concentration[period_key] = float(daily_signals.max() / daily_signals.sum()) if len(daily_signals) else None
        baseline = model["holdout_comparable"]
        row = {"source_model_id": model_id, "canonical_combination_id": model["canonical_combination_id"],
               "target": model["target"], "origin": model["origin"],
               "period_kind": period["period_kind"], "horizon": period["horizon"],
               **{k: v for k, v in values.items() if k not in {"precision", "signals"}},
               "t0_availability": baseline["availability"],
               "active_signal_sessions": len(counts),
               "signal_share_population": float(period["signals"] / total_signals) if total_signals else None,
               "target_signal_share_population": float(target_signals / total_signals) if total_signals else None,
               "population_largest_session_signal_share": concentration[period_key],
               "largest_session_signal_share": float(counts.max() / counts.sum()) if len(counts) else None}
        for name in ("auc", "brier", "recall", "f1", "positives", "negatives", "prevalence", "signal_rate", "mean_probability", "signal_mean_probability", "probability_bias", "auc_status"):
            row.setdefault(name, None)
        for name in ("auc", "brier", "precision", "recall", "f1", "prevalence", "mean_return", "signal_rate", "mean_probability", "probability_bias"):
            t0 = baseline.get(name) if baseline["availability"] == "available" else None
            forward = period["mean_return"] if name == "mean_return" else values.get(name)
            row[f"t0_{name}"] = t0
            row[f"delta_{name}"] = float(forward - t0) if t0 is not None and forward is not None and pd.notna(forward) else None
        row["wf_auc_median"] = model["wf"].get("ROCAUCMedian")
        row["qualification_holdout_auc"] = model["holdout_qualification"].get("ROCAUC")
        rows.append(row)
        for bucket in probability_bins(subset, values["availability"]):
            bins.append({"source_model_id": model_id, "period_kind": period["period_kind"],
                         "horizon": period["horizon"], **bucket})
    return pd.DataFrame(rows), pd.DataFrame(bins, columns=["source_model_id", "period_kind", "horizon", "bin", "lower", "upper", "observations", "signals", "mean_probability", "observed_frequency", "signal_precision", "signal_mean_return"])


def materialize_forward_diagnostic(output: Path) -> dict[str, Any]:
    from .runner import _try_submission_mutex

    with _try_submission_mutex(output / ".forward_diagnostic.lock") as acquired:
        if not acquired:
            raise ValueError("forward_diagnostic_already_building")
        return _materialize_forward_diagnostic(output)


def _materialize_forward_diagnostic(output: Path) -> dict[str, Any]:
    """Post-Forward entry point, also usable explicitly for historical runs.

    Publish the commit manifest last; interruption cannot authorize partial files.
    The existing analysis manifest is reloaded before adding our references.
    """
    analysis = _json(output / ANALYSIS)
    source_id = analysis.get("source_e2e_run_id")
    if not source_id or analysis.get("forward_run_id") != output.parent.name:
        raise ValueError("forward_analysis_unavailable")
    inputs = ("forward_observations.csv", "forward_period_metrics.csv", "forward_exclusions.csv")
    for name in inputs:
        path = output / name
        if not path.is_file() or digest(path) != analysis.get("artifact_digests", {}).get(name):
            raise ValueError("forward_diagnostic_input_changed")
    source = output.parent.parent / str(source_id)
    if not source.resolve().is_relative_to(output.parent.parent.resolve()):
        raise ValueError("forward_diagnostic_source_outside_runs")
    reference, reference_path = ensure_t0_reference(source)
    expected_snapshot = analysis.get("source_snapshot_sha256")
    if expected_snapshot and expected_snapshot != reference["source_snapshot_sha256"]:
        raise ValueError("forward_diagnostic_snapshot_changed")
    input_digests = {name: digest(output / name) for name in inputs}
    reference_digest = digest(reference_path)
    existing = _json(output / MANIFEST)
    if (existing.get("protocol") == PROTOCOL and existing.get("input_digests") == input_digests
            and existing.get("t0_reference", {}).get("sha256") == reference_digest
            and all((output / name).is_file() and digest(output / name) == value
                    for name, value in existing.get("artifact_digests", {}).items())
            and set(existing.get("artifact_digests", {})) == {METRICS, BINS}):
        manifest = existing
    else:
        metrics, bins = analyze_diagnostic(pd.read_csv(output / inputs[0]), pd.read_csv(output / inputs[1]), reference)
        _publish(output / METRICS, metrics)
        _publish(output / BINS, bins)
        manifest = {"protocol": PROTOCOL, "schema_version": 1, "forward_run_id": output.parent.name,
                    "source_e2e_run_id": source_id, "input_digests": input_digests,
                    "t0_reference": {"path": f"{source_id}/results/{REFERENCE}", "sha256": reference_digest},
                    "artifact_digests": {name: digest(output / name) for name in (METRICS, BINS)}}
        if any(digest(output / name) != value for name, value in input_digests.items()):
            raise ValueError("forward_diagnostic_input_changed")
        _publish(output / MANIFEST, manifest)
    current = _json(output / ANALYSIS)
    if current.get("source_e2e_run_id") != source_id or current.get("artifact_digests") != analysis.get("artifact_digests"):
        raise ValueError("forward_analysis_changed_during_diagnostic")
    current["t0_reference"] = manifest["t0_reference"]
    current["diagnostic"] = {"path": MANIFEST, "sha256": digest(output / MANIFEST), "protocol": PROTOCOL}
    _publish(output / ANALYSIS, current)
    return manifest


def load_forward_diagnostic(output: Path) -> tuple[dict[str, Any], pd.DataFrame, pd.DataFrame]:
    """Read and validate committed derived artifacts; never calculate on display."""
    manifest = _json(output / MANIFEST)
    if not manifest:
        return {}, pd.DataFrame(), pd.DataFrame()
    if manifest.get("protocol") != PROTOCOL or manifest.get("forward_run_id") != output.parent.name:
        raise ValueError("forward_diagnostic_manifest_mismatch")
    if (set(manifest.get("input_digests", {})) != {"forward_observations.csv", "forward_period_metrics.csv", "forward_exclusions.csv"}
            or set(manifest.get("artifact_digests", {})) != {METRICS, BINS}):
        raise ValueError("forward_diagnostic_manifest_incomplete")
    reference_path = output.parent.parent / manifest["t0_reference"]["path"]
    if not reference_path.resolve().is_relative_to(output.parent.parent.resolve()):
        raise ValueError("forward_diagnostic_reference_outside_runs")
    for name, expected in {**manifest["input_digests"], **manifest["artifact_digests"]}.items():
        if not (output / name).is_file() or digest(output / name) != expected:
            raise ValueError("forward_diagnostic_artifact_changed")
    if not reference_path.is_file() or digest(reference_path) != manifest["t0_reference"]["sha256"]:
        raise ValueError("forward_t0_reference_changed")
    reference = _json(reference_path)
    periods = pd.read_csv(output / "forward_period_metrics.csv")
    metrics = pd.read_csv(output / METRICS).merge(
        periods.drop(columns=["target"]), on=["source_model_id", "period_kind", "horizon"],
        how="left", validate="one_to_one")
    return reference, metrics, pd.read_csv(output / BINS)


def forward_comparison_table(runs_root: Path, run_ids: list[str]) -> pd.DataFrame:
    frames = []
    for run_id in run_ids:
        reference, metrics, _ = load_forward_diagnostic(runs_root / run_id / "results")
        if metrics.empty:
            frames.append(pd.DataFrame([{"run_id": run_id, "availability": "unavailable_legacy"}]))
            continue
        selected = metrics.loc[metrics["period_kind"].eq("cumulative") & metrics["horizon"].isin([21, 42, 63])].copy()
        if selected.empty:
            frames.append(pd.DataFrame([{"run_id": run_id, "cutoff_t0": reference.get("cutoff_t0"),
                                         "availability": "unavailable_horizon_not_reached"}]))
            continue
        selected.insert(0, "run_id", run_id)
        selected["cutoff_t0"] = reference.get("cutoff_t0")
        frames.append(selected)
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
