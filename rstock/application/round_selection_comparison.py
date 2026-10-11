"""Read-only paired WF analysis on the entire frozen input population.

Intervals describe this selected population, not independent validation of the
upstream discovery process. Temporal blocks are resampled jointly for every
candidate and direction; candidate rows are never treated as independent draws.
"""
from __future__ import annotations

import hashlib
import json
from dataclasses import fields
from pathlib import Path

import numpy as np
import pandas as pd

from rstock.combinations import canonical_combination_id_from_set, symbol_set_id
from rstock.config import RStockConfig
from rstock.evaluation import probability_losses
from rstock.modeling import ROUND_SELECTION_FIELDS, round_selection_coverage


def _configuration(root: Path) -> dict:
    return json.loads((root / "config.json").read_text(encoding="utf8"))


def _window_losses(root: Path, windows: pd.DataFrame) -> pd.DataFrame:
    """Recover historical losses and verify exact test labels/dates in chunks."""
    accumulators = {}
    columns = ["Set", "Window", "Date", "IntradayTarget", "DownTarget", "UpProbability", "DownProbability"]
    path = root / "results" / "predictions.csv"
    for chunk in pd.read_csv(path, usecols=columns, chunksize=65536):
        for (set_id, window), group in chunk.groupby(["Set", "Window"], sort=False):
            entry = accumulators.setdefault((set_id, window), {"digest": hashlib.sha256(), "count": 0,
                "UpLogLoss": 0., "DownLogLoss": 0., "UpBrier": 0., "DownBrier": 0.})
            dates = pd.to_datetime(group["Date"]).dt.strftime("%Y-%m-%d")
            for date, up, down in zip(dates, group["IntradayTarget"], group["DownTarget"]):
                entry["digest"].update(f"{date}|{int(up)}|{int(down)}\n".encode())
            entry["count"] += len(group)
            for direction, label in (("Up", "IntradayTarget"), ("Down", "DownTarget")):
                losses, briers = probability_losses(group[label], group[f"{direction}Probability"])
                entry[f"{direction}LogLoss"] += float(losses.sum())
                entry[f"{direction}Brier"] += float(briers.sum())
    records = []
    for (set_id, window), entry in accumulators.items():
        records.append({"Set": set_id, "Window": window, "TestDigest": entry["digest"].hexdigest(),
            "ObservedTestRows": entry["count"], **{name: entry[name] / entry["count"]
            for name in ("UpLogLoss", "DownLogLoss", "UpBrier", "DownBrier")}})
    if not records:
        raise ValueError("No comparable WF predictions are available")
    result = windows.drop(columns=["UpLogLoss", "DownLogLoss", "UpBrier", "DownBrier"], errors="ignore").merge(
        pd.DataFrame(records), on=["Set", "Window"], how="outer", validate="one_to_one", indicator=True)
    if not result["_merge"].eq("both").all() or not result["TestObservations"].eq(result["ObservedTestRows"]).all():
        raise ValueError("WF windows and prediction population differ")
    return result.drop(columns="_merge")


def _equal_weight_mean(frame: pd.DataFrame, metric: str) -> float:
    by_candidate = frame.groupby(["Direction", "CanonicalId"])[metric].mean()
    return float(by_candidate.groupby(level="Direction").mean().mean())


def temporal_block_interval(pairs: pd.DataFrame, *, draws: int = 2000, seed: int = 1234) -> dict:
    """Synchronous moving-block bootstrap, with conservative adequacy checks.

    Both default and doubled block lengths must have enough effective blocks.
    A CI is withheld for sparse panels or unstable block-length sensitivity.
    These checks are adequacy diagnostics, not a proof of independence.
    """
    result = {"status": "résultat exploratoire", "confidence_level": .95,
              "interval": None, "method": "synchronous_temporal_moving_blocks_v1"}
    if pairs.empty:
        return {**result, "reason": "no_optimized_or_comparable_windows"}
    starts = pd.DatetimeIndex(sorted(pd.to_datetime(pairs["TestStart"]).unique()))
    ends = pd.to_datetime(pairs["TestEnd"])
    begin_positions = starts.searchsorted(pd.to_datetime(pairs["TestStart"]))
    end_positions = starts.searchsorted(ends, side="right")
    overlap = int(np.max(end_positions - begin_positions))
    block_length = max(overlap, int(np.ceil(len(starts) ** (1 / 3))), 2)
    result.update(temporal_origins=len(starts), overlap_span=overlap, block_length=block_length,
                  effective_blocks=len(starts) // block_length)
    panel = pairs.pivot(index=["Direction", "CanonicalId"], columns="TestStart", values="DeltaLogLoss")
    panel.columns = pd.to_datetime(panel.columns)
    panel = panel.reindex(columns=starts).sort_index()
    if len(starts) // block_length < 20 or len(starts) // (2 * block_length) < 10:
        return {**result, "reason": "insufficient_effective_temporal_blocks"}
    if panel.notna().mean(axis=1).min() < .8:
        return {**result, "reason": "sparse_candidate_temporal_panel"}
    values = panel.to_numpy(dtype=float)
    direction_masks = [panel.index.get_level_values("Direction") == d for d in ("Up", "Down")]
    rng = np.random.default_rng(seed)
    intervals = []
    for length in (block_length, 2 * block_length):
        estimates = []
        for _ in range(draws):
            origins = rng.integers(0, len(starts) - length + 1, size=int(np.ceil(len(starts) / length)))
            sampled = (origins[:, None] + np.arange(length)).ravel()[:len(starts)]
            selected = values[:, sampled]
            if np.any(np.isfinite(selected).sum(axis=1) == 0):
                continue
            candidate_means = np.nanmean(selected, axis=1)
            estimates.append(np.mean([candidate_means[mask].mean() for mask in direction_masks if mask.any()]))
        if len(estimates) < .95 * draws:
            return {**result, "reason": "insufficient_supported_resamples"}
        intervals.append(np.quantile(estimates, [.025, .975]).tolist())
    result["sensitivity_intervals"] = intervals
    # Report the conservative envelope, never select the favorable block size.
    interval = [min(v[0] for v in intervals), max(v[1] for v in intervals)]
    if (intervals[0][1] < 0) != (intervals[1][1] < 0):
        return {**result, "reason": "unstable_block_length_sensitivity"}
    result.update(status="estimation conditionnelle par blocs", interval=interval,
                  reason="selected_population_only; additional_independent_validation_required")
    return result


def _summarize(pairs: pd.DataFrame) -> dict:
    if pairs.empty:
        return {"candidate_direction_windows": 0, "inference": temporal_block_interval(pairs)}
    summary = {"candidate_direction_windows": len(pairs), "candidates": int(pairs["SetFixed"].nunique())}
    for metric in ("LogLoss", "Brier", "ROCAUC"):
        summary[f"delta_{metric}"] = _equal_weight_mean(pairs, f"Delta{metric}")
    baseline = _equal_weight_mean(pairs, "FixedLogLoss")
    summary["relative_log_loss_improvement"] = -summary["delta_LogLoss"] / baseline if baseline > 0 else None
    summary["directions"] = {direction: {
        "windows": len(group), "delta_log_loss": _equal_weight_mean(group, "DeltaLogLoss"),
        "delta_brier": _equal_weight_mean(group, "DeltaBrier"),
        "delta_auc": _equal_weight_mean(group, "DeltaROCAUC"),
    } for direction, group in pairs.groupby("Direction")}
    temporal = pairs.groupby("TestStart")["DeltaLogLoss"].mean().sort_index()
    summary["temporal_delta_std"] = float(temporal.std()) if len(temporal) > 1 else None
    summary["performance_dispersion"] = {
        mode: {metric: float(pairs.groupby(["Direction", "CanonicalId"])[f"{metric}{mode}"].std().mean())
               for metric in ("LogLoss", "Brier", "ROCAUC")}
        for mode in ("Fixed", "Chronological")
    }
    segments = np.array_split(np.arange(len(temporal)), 3)
    segment_values = []
    for segment in segments:
        dates = temporal.index[segment]
        selected = pairs[pairs["TestStart"].isin(dates)]
        segment_values.append(_equal_weight_mean(selected, "DeltaLogLoss") if len(selected) else None)
    summary["temporal_thirds_delta_log_loss"] = segment_values
    if all(v is not None for v in segment_values):
        best_dates = temporal.index[segments[int(np.argmin(segment_values))]]
        remaining = pairs[~pairs["TestStart"].isin(best_dates)]
        summary["delta_without_best_third"] = _equal_weight_mean(remaining, "DeltaLogLoss")
    summary["inference"] = temporal_block_interval(pairs)
    ci = summary["inference"]["interval"]
    summary["additional_validation_warranted"] = bool(
        ci is not None and ci[1] < 0 and summary["relative_log_loss_improvement"] >= .01
        and summary["delta_ROCAUC"] >= -.01 and summary["delta_Brier"] <= .005
        and sum(v is not None and v < 0 for v in segment_values) >= 2
        and summary.get("delta_without_best_third", float("inf")) < 0)
    return summary


def compare_round_selection(project_root: Path, fixed_id: str, chronological_id: str) -> tuple[dict, pd.DataFrame]:
    roots = [Path(project_root) / "runs" / run_id for run_id in (fixed_id, chronological_id)]
    configs = [_configuration(root) for root in roots]
    fixed, chronological = configs
    if fixed["rstock_config"].get("xgb_round_selection_mode", "fixed") != "fixed" or chronological["rstock_config"].get("xgb_round_selection_mode") != "chronological":
        raise ValueError("Select a fixed reference and a chronological derived WF")
    provenance = chronological.get("walk_forward_derivation") or {}
    if provenance.get("source_run_id") != fixed_id:
        raise ValueError("Chronological run must derive from the selected reference")
    from .repository import RunRepository
    from .walk_forward_experiments import load_frozen_walk_forward_candidates
    repository = RunRepository(Path(project_root) / "runs")
    for run_id in (fixed_id, chronological_id):
        if repository.status(run_id).get("status") != "completed":
            raise ValueError("Paired comparison requires completed WF runs")
    fixed_spec, chronological_spec = (repository.load_spec(run_id) for run_id in (fixed_id, chronological_id))
    population = load_frozen_walk_forward_candidates(repository, chronological_spec)
    for field in fields(RStockConfig):
        if field.name not in ROUND_SELECTION_FIELDS and getattr(fixed_spec.config, field.name) != getattr(chronological_spec.config, field.name):
            raise ValueError(f"Scientific configuration differs: {field.name}")
    windows = [_window_losses(root, pd.read_csv(root / "results" / "windows.csv")) for root in roots]
    observed = set(windows[0]["Set"])
    from rstock.combination_planning import CombinationPlan
    count = population.count() if isinstance(population, CombinationPlan) else len(population)
    for start in range(0, count, 65536):
        chunk = population.slice(start, min(start + 65536, count)) if isinstance(population, CombinationPlan) else population.iloc[start:start + 65536]
        for _, row in chunk.iterrows():
            expected_id = symbol_set_id(row)
            if expected_id not in observed:
                raise ValueError("Reference results omit a candidate from the frozen input population")
            observed.remove(expected_id)
    if observed:
        raise ValueError("Reference results contain candidates outside the frozen input population")
    rows = []
    for frame in windows:
        directional = []
        for direction in ("Up", "Down"):
            work = frame[["Set", "Window", "TrainStart", "TrainEnd", "TestStart", "TestEnd", "TestObservations", "TestDigest"]].copy()
            work["Direction"] = direction
            work["CanonicalId"] = work["Set"].map(lambda value: canonical_combination_id_from_set(value, direction))
            if work["CanonicalId"].isna().any():
                raise ValueError("Ambiguous historical candidate identity")
            for metric in ("LogLoss", "Brier", "ROCAUC"):
                work[metric] = frame[f"{direction}{metric}"].to_numpy()
            used = frame.get(f"{direction}RoundSelectionUsed", pd.Series(False, index=frame.index))
            work["Optimized"] = used.map(lambda value: str(value).lower() == "true").to_numpy()
            directional.append(work)
        rows.append(pd.concat(directional, ignore_index=True))
    keys = ["CanonicalId", "Direction", "Window", "TrainStart", "TrainEnd", "TestStart", "TestEnd"]
    pairs = rows[0].merge(rows[1], on=keys, how="outer", suffixes=("Fixed", "Chronological"), validate="one_to_one", indicator=True)
    if not pairs["_merge"].eq("both").all():
        raise ValueError("Candidate/window population differs; paired inference is unavailable")
    if not pairs["SetFixed"].eq(pairs["SetChronological"]).all() or not pairs["TestDigestFixed"].eq(pairs["TestDigestChronological"]).all():
        raise ValueError("Candidate feature order or test dates/labels differ")
    pairs = pairs.drop(columns="_merge")
    for metric in ("LogLoss", "Brier", "ROCAUC"):
        pairs[f"Delta{metric}"] = pairs[f"{metric}Chronological"] - pairs[f"{metric}Fixed"]
    pairs["FixedLogLoss"] = pairs["LogLossFixed"]
    summary = {
        "reference": fixed_id, "chronological": chronological_id,
        "scope": "Comparaison conditionnelle sur une population déjà sélectionnée ; aucune supériorité indépendante démontrée.",
        "coverage": round_selection_coverage(windows[1]),
        "all_windows": _summarize(pairs),
        "optimized_windows": _summarize(pairs[pairs["OptimizedChronological"]]),
        "optimized_subset_caveat": "Sous-ensemble disposant d'un historique et de conditions de classes suffisants ; non représentatif de toute la population.",
        "decision_criteria": {"relative_log_loss_gain": .01, "max_auc_degradation": .01,
                              "max_brier_degradation": .005, "confidence": .95},
    }
    return summary, pairs
