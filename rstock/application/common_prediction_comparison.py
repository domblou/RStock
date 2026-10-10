"""Compare existing runs on observed common dates, never on aggregate substitutes."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Sequence

import numpy as np
import pandas as pd

from rstock.evaluation import binary_predictions, classification_metrics


STATE_LABELS = {
    "no_qualified_candidates": "Aucun candidat qualifié", "qualified_candidates": "Candidats qualifiés",
    "executed": "Holdout exécuté", "not_executed": "Holdout non exécuté",
    "available": "Données disponibles", "unavailable": "Données indisponibles",
    "unknown": "État historique non déterminé",
}


def _json(path: Path) -> dict:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return value if isinstance(value, dict) else {}


def result_availability(root: Path) -> dict[str, str]:
    summary = _json(root / "summary.json")
    pipeline = _json(root / "results/pipeline_summary.json")
    results = _json(root / "results/run_configuration.json")
    config = _json(root / "config.json")
    values = {**summary, **results, **pipeline}
    count = values.get("eligible_combinations", values.get("qualified_candidates"))
    candidate = values.get("candidate_status")
    if candidate is None and count is not None:
        candidate = "qualified_candidates" if int(count) else "no_qualified_candidates"
    holdout = values.get("holdout_status")
    if holdout is None:
        if values.get("holdout_evaluated") is True:
            holdout = "executed"
        elif values.get("holdout_evaluated") is False or values.get("final_holdout_evaluated") is False or config.get("evaluate_final_holdout") is False:
            holdout = "not_executed"
        else:
            for filename in ("holdout_predictions.csv", "final_holdout_predictions.csv"):
                path = root / "results" / filename
                if path.is_file():
                    try:
                        if not pd.read_csv(path, nrows=1).empty:
                            holdout = "executed"
                    except (ValueError, pd.errors.EmptyDataError):
                        pass
    try:
        prediction_root = _prediction_root(root.parent, root.name)
    except (ValueError, KeyError):
        prediction_root = root
    return {"candidate_status": candidate or "unknown", "holdout_status": holdout or "unknown",
            "data_status": "available" if (prediction_root / "results/predictions.csv").is_file() else "unavailable"}


def _prediction_root(runs: Path, run_id: str, phase: str = "development") -> Path:
    root = runs / run_id
    config = _json(root / "config.json")
    if config.get("job_type") == "end_to_end":
        from .end_to_end import effective_stage_run_id
        manifest = _json(root / "orchestration/pipeline.json")
        stage = "walk_forward" if phase == "development" else "holdout_evaluation"
        source = effective_stage_run_id(manifest, stage) if manifest else None
        if not source and phase == "holdout" and manifest:
            source = effective_stage_run_id(manifest, "threshold_calibration")
        if not source:
            summary = _json(root / "results/pipeline_summary.json")
            source = next((item.get("child_run_id") for item in summary.get("stages", [])
                           if item.get("stage_key") == stage), None)
        if not source:
            raise ValueError("Walk-forward source is unavailable")
        return runs / str(source)
    return root


def _read_predictions(root: Path, phase: str = "development") -> pd.DataFrame:
    path = root / "results/predictions.csv"
    if phase == "holdout":
        path = root / "results/holdout_predictions.csv"
        if not path.is_file():
            path = root / "results/final_holdout_predictions.csv"
    columns = {"Set", "Observation", "Window", "Date", "TrainEnd", "UpProbability", "DownProbability",
               "UpPrediction", "DownPrediction", "IntradayTarget", "DownTarget", "IntradayReturn",
               "Direction", "Probability", "Target", "Prediction"}
    chunks = pd.read_csv(path, usecols=lambda name: name in columns, chunksize=65536)
    wide = pd.concat(chunks, ignore_index=True)
    if wide.empty:
        raise ValueError("Individual predictions are empty")
    if "Observation" not in wide:
        wide["Observation"] = wide["Set"].map(lambda value: json.loads(value)[0])
    wide["Date"] = pd.to_datetime(wide["Date"], errors="raise").dt.normalize()
    if "TrainEnd" not in wide:
        windows = pd.read_csv(root / "results/windows.csv", usecols=["Set", "Window", "TrainEnd"])
        wide = wide.merge(windows, on=["Set", "Window"], how="left", validate="many_to_one")
    wide["TrainEnd"] = pd.to_datetime(wide["TrainEnd"], errors="raise")
    if wide["TrainEnd"].isna().any() or (wide["TrainEnd"] >= wide["Date"]).any():
        raise ValueError("Training origins are missing or not strictly before predictions")
    settings = _json(root / "config.json").get("rstock_config", {})
    threshold = settings.get("prediction_threshold", .5)
    records = []
    for direction, label in (("Up", "IntradayTarget"), ("Down", "DownTarget")):
        if "Direction" in wide:
            part = wide[wide["Direction"] == direction].copy()
            if "Probability" not in part or "Target" not in part:
                raise ValueError("Directional probabilities or observed labels are unavailable")
            if "Prediction" not in part:
                raise ValueError("Frozen holdout decisions are unavailable")
            if part.empty:
                continue
        else:
            probability = direction + "Probability"
            if probability not in wide or label not in wide:
                raise ValueError("Directional probabilities or observed labels are unavailable")
            part = wide[["Set", "Observation", "Date", "TrainEnd", probability, label]].rename(
                columns={probability: "Probability", label: "Target"}).copy()
            part["Direction"] = direction
            part["Prediction"] = wide[direction + "Prediction"] if direction + "Prediction" in wide else binary_predictions(part["Probability"], threshold)
        # Labels are required; never impute unknown outcomes or probabilities.
        part = part.dropna(subset=["Target", "Probability", "Prediction"])
        if not part["Target"].isin([0, 1]).all() or not part["Prediction"].isin([0, 1]).all():
            raise ValueError("Non-binary labels or predictions")
        if not np.isfinite(part["Probability"]).all() or not part["Probability"].between(0, 1).all():
            raise ValueError("Invalid probabilities")
        keys = ["Set", "Observation", "Direction", "Date"]
        same_origin = part.duplicated([*keys, "TrainEnd"], keep=False)
        if same_origin.any():
            duplicates = part.loc[same_origin].groupby([*keys, "TrainEnd"])[["Target", "Probability", "Prediction"]].nunique()
            if duplicates.gt(1).any().any():
                raise ValueError("Conflicting predictions at the same training origin")
        # One prediction per candidate/date: the latest strictly historical origin.
        records.append(part.sort_values("TrainEnd").drop_duplicates(keys, keep="last"))
    if not records:
        raise ValueError("Individual directional predictions are unavailable")
    return pd.concat(records, ignore_index=True)


def compare_common_predictions(project_root: Path, run_ids: Sequence[str], *, phase: str = "development") -> tuple[dict, pd.DataFrame]:
    """Intersect observations across every run and every candidate per target/direction.

    Scores are evaluated separately per candidate, never averaged into an ensemble.
    Overlapping WF blocks use the latest available training origin per date.
    """
    if phase not in {"development", "holdout"}:
        raise ValueError("Unsupported prediction comparison phase")
    if len(run_ids) < 2 or len(set(run_ids)) != len(run_ids):
        raise ValueError("Select at least two distinct runs")
    runs = Path(project_root) / "runs"
    frames = {}
    availability = {run_id: result_availability(runs / run_id) for run_id in run_ids}
    unavailable = {}
    for run_id in run_ids:
        try:
            frames[run_id] = _read_predictions(_prediction_root(runs, run_id, phase), phase)
            availability[run_id]["data_status"] = "available"
        except (OSError, ValueError, KeyError, pd.errors.ParserError) as error:
            unavailable[run_id] = str(error)
            availability[run_id]["data_status"] = "unavailable"
    audit = {"availability": availability, "duplicate_policy": "latest_historical_origin_per_candidate_date",
             "population": "development_before_qualification" if phase == "development" else "holdout_evaluated_population", "unavailable_runs": unavailable}
    if unavailable:
        return {**audit, "status": "unavailable", "common_observations": 0}, pd.DataFrame()
    common_keys = set.intersection(*(set(zip(frame["Observation"], frame["Direction"])) for frame in frames.values()))
    rows = []
    common_count = 0
    for observation, direction in sorted(common_keys):
        groups = {run_id: frame[(frame["Observation"] == observation) & (frame["Direction"] == direction)]
                  for run_id, frame in frames.items()}
        # Candidate completeness is required as well as run completeness.
        dates = set.intersection(*(set(group["Date"]) for frame in groups.values()
                                  for _, group in frame.groupby("Set")))
        if not dates:
            continue
        common_count += len(dates)
        labels = pd.concat([frame[frame["Date"].isin(dates)][["Date", "Target"]] for frame in groups.values()])
        if labels.groupby("Date")["Target"].nunique().gt(1).any():
            return {**audit, "status": "label_mismatch", "common_observations": 0}, pd.DataFrame()
        for run_id, frame in groups.items():
            config = _json(runs / run_id / "config.json").get("rstock_config", {})
            for set_id, candidate in frame.groupby("Set", sort=False):
                paired = candidate[candidate["Date"].isin(dates)].sort_values("Date")
                metrics = classification_metrics(paired["Target"], paired["Prediction"], paired["Probability"])
                rows.append({"Run": run_id, "PredictiveModelType": config.get("predictive_model_type", "external_only"),
                    "Observation": observation, "Direction": direction, "Set": set_id,
                    "CommonObservations": len(paired), "ExcludedObservations": len(candidate) - len(paired),
                    "FirstDate": paired["Date"].min(), "LastDate": paired["Date"].max(), **metrics.as_columns()})
    return {**audit, "status": "available" if rows else "no_common_observations", "common_observations": common_count}, pd.DataFrame(rows)
