"""Reconstruct a controlled WF baseline from local snapshots, without training."""
from __future__ import annotations

import hashlib
import json
import pickle
from pathlib import Path

import pandas as pd

from rstock.evaluation import binary_predictions
from rstock.features import intraday_target_column, intraday_down_target_column, model_predictor_columns
from rstock.modeling import fit_constant_probability
from rstock.walk_forward import walk_forward_windows
from .repository import RunRepository


def _dates_digest(index):
    return hashlib.sha256("\n".join(index.strftime("%Y-%m-%d")).encode()).hexdigest()


def _diagnostic(run_id, set_id, direction, number, train, test, label, protocol):
    return {"Run": run_id, "Set": set_id, "Direction": direction, "Window": number,
            "Protocol": protocol, "TrainStart": train.index.min(), "TrainEnd": train.index.max(),
            "TestStart": test.index.min(), "TestEnd": test.index.max(),
            "TrainObservations": len(train), "TestObservations": len(test),
            "TrainDatesDigest": _dates_digest(train.index), "TestDatesDigest": _dates_digest(test.index),
            "TrainLabelsDigest": hashlib.sha256(train[label].astype(int).to_numpy().tobytes()).hexdigest(),
            "TrainPositiveOutcomes": int(train[label].sum())}


def reconstruct_aligned_constants(roots: dict[str, Path]) -> tuple[dict[str, pd.DataFrame], pd.DataFrame]:
    """Require existing XGB windows to match the common mask exactly.

    Constant windows are deliberately rebuilt, rather than filtered afterwards.
    If XGB variants used different training populations, no aligned claim is made.
    """
    specs, prepared, windows, predictions, model_data = {}, {}, {}, {}, {}
    for run_id, root in roots.items():
        specs[run_id] = RunRepository(root.parent).load_spec(root.name)
        path = root / "checkpoints/artifacts/prepared_snapshot.pkl"
        raw = path.read_bytes()
        metadata = json.loads(path.with_suffix(".json").read_text(encoding="utf-8"))
        if hashlib.sha256(raw).hexdigest() != metadata["sha256"]:
            raise ValueError(f"Snapshot digest differs: {run_id}")
        frame = pickle.loads(raw)["prepared"].sort_index()
        if not isinstance(frame.index, pd.DatetimeIndex) or frame.index.has_duplicates or frame.index.hasnans:
            raise ValueError(f"Invalid snapshot calendar: {run_id}")
        prepared[run_id] = frame
        windows[run_id] = pd.read_csv(root / "results/windows.csv")
        predictions[run_id] = pd.read_csv(root / "results/predictions.csv",
            usecols=["Set", "Window", "Date", "IntradayTarget", "DownTarget"])
        predictions[run_id]["Date"] = pd.to_datetime(predictions[run_id]["Date"])
    constants = [r for r, s in specs.items() if s.config.predictive_model_type == "constant_probability"]
    variants = [r for r in specs if r not in constants]
    if not constants or not variants:
        raise ValueError("A constant and a predictive variant are required")
    targets = set.intersection(*(set(w.Observation) for w in windows.values()))
    records = {r: [] for r in constants}
    diagnostics = []
    for target in sorted(targets):
        labels = [intraday_target_column(target), intraday_down_target_column(target)]
        candidates = {}
        # Union of required columns across variants; intersection of valid dates.
        # This handles interior missing observations as well as initial lags.
        for run_id, spec in specs.items():
            for set_id in windows[run_id].loc[windows[run_id].Observation == target, "Set"].unique():
                symbols = json.loads(set_id)
                names = model_predictor_columns(prepared[run_id], target, symbols[1:], spec.config)
                data = prepared[run_id][list(dict.fromkeys([*names, *labels]))].dropna()
                model_data[run_id, set_id] = data
                candidates[run_id, set_id] = data.index
        calendar = pd.DatetimeIndex(sorted(set.intersection(*(set(v) for v in candidates.values()))))
        if calendar.empty:
            raise ValueError(f"No common model observations: {target}")
        reference = variants[0]
        settings = json.loads((roots[reference] / "results/run_configuration.json").read_text(encoding="utf-8"))
        cutoff = pd.Timestamp(settings["final_holdout_start"])
        calendar = calendar[calendar < cutoff]
        ref = prepared[reference].loc[calendar, labels]
        for run_id in specs:
            if not prepared[run_id].loc[calendar, labels].equals(ref):
                raise ValueError(f"Training/test labels differ: {run_id}/{target}")
        common_windows = walk_forward_windows(len(calendar), specs[reference].config)
        if not common_windows:
            raise ValueError(f"No common WF windows: {target}")
        for run_id in variants:
            other_settings = json.loads((roots[run_id] / "results/run_configuration.json").read_text(encoding="utf-8"))
            if pd.Timestamp(other_settings["final_holdout_start"]) != cutoff:
                raise ValueError("Holdout boundaries differ")
            for (candidate_run, set_id), own_index in candidates.items():
                if candidate_run != run_id:
                    continue
                own = model_data[run_id, set_id]
                own = own.loc[own.index < cutoff]
                saved = windows[run_id][windows[run_id].Set == set_id]
                own_windows = walk_forward_windows(len(own), specs[run_id].config)
                if len(saved) != len(common_windows) or len(own_windows) != len(common_windows):
                    raise ValueError(f"Training/test windows differ: {run_id}/{set_id}")
                for window, own_window in zip(common_windows, own_windows):
                    train, test = ref.iloc[window.train_slice], ref.iloc[window.test_slice]
                    if (not own.iloc[own_window.train_slice].index.equals(train.index)
                            or not own.iloc[own_window.test_slice].index.equals(test.index)):
                        raise ValueError(f"Training observations or retraining boundaries differ: {run_id}/{set_id}")
                    row = saved[saved.Window == window.number]
                    if len(row) != 1:
                        raise ValueError("Stored window identity differs")
                    row = row.iloc[0]
                    for name, value in {"TrainStart": train.index.min(), "TrainEnd": train.index.max(),
                                        "TestStart": test.index.min(), "TestEnd": test.index.max()}.items():
                        if pd.Timestamp(row[name]) != value:
                            raise ValueError(f"Stored {name} differs: {run_id}/{set_id}")
                    if row.TrainObservations != len(train) or row.TestObservations != len(test):
                        raise ValueError("Stored training/test counts differ")
                    observed = predictions[run_id]
                    observed = observed[(observed.Set == set_id) & (observed.Window == window.number)].sort_values("Date")
                    if (not pd.DatetimeIndex(observed.Date).equals(test.index)
                            or not (observed[["IntradayTarget", "DownTarget"]].to_numpy() == test[labels].to_numpy()).all()):
                        raise ValueError("Stored test dates/labels differ")
                    for direction, label in zip(("Up", "Down"), labels):
                        diagnostics.append(_diagnostic(run_id, set_id, direction, window.number, train, test, label, "verified_historical"))
        for window in common_windows:
            train, test = ref.iloc[window.train_slice], ref.iloc[window.test_slice]
            if train.index.max() >= test.index.min():
                raise ValueError("Training labels are not strictly historical")
            for run_id in constants:
                for candidate_run, set_id in candidates:
                    if candidate_run != run_id:
                        continue
                    for direction, label in zip(("Up", "Down"), labels):
                        model = fit_constant_probability(train, label)
                        diagnostic = _diagnostic(run_id, set_id, direction, window.number, train, test, label, "aligned_constant_recalculated")
                        diagnostic["Probability"] = model.probability
                        diagnostics.append(diagnostic)
                        records[run_id].append(pd.DataFrame({"Set": set_id, "Observation": target,
                            "Direction": direction, "Window": window.number, "Date": test.index,
                            "TrainEnd": train.index.max(), "Target": test[label].astype(int).to_numpy(),
                            "Probability": model.probability,
                            "Prediction": binary_predictions([model.probability] * len(test), specs[run_id].config.prediction_threshold)}))
    if any(not parts for parts in records.values()):
        raise ValueError("No shared targets for controlled comparison")
    frames = {r: pd.concat(parts, ignore_index=True).sort_values("TrainEnd").drop_duplicates(
        ["Set", "Observation", "Direction", "Date"], keep="last") for r, parts in records.items()}
    return frames, pd.DataFrame(diagnostics)
