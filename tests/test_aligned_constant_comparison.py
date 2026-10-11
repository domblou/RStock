import hashlib
import json
import pickle
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.application.aligned_constant_comparison import reconstruct_aligned_constants
from rstock.application.common_prediction_comparison import compare_common_predictions
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.config import DEFAULT_CONFIG
from rstock.features import model_predictor_columns
from rstock.walk_forward import walk_forward_windows


def fixture_runs(tmp_path, *, rolling=False, different_training=False):
    dates = pd.bdate_range("2024-01-01", periods=25)
    returns = pd.Series(np.arange(25, dtype=float), index=dates)
    returns.iloc[8] = np.nan
    prepared = pd.DataFrame({"AAA.intraday_target": (np.arange(25) % 3 == 0).astype(int),
                             "AAA.intraday_down_target": (np.arange(25) % 4 == 0).astype(int)}, index=dates)
    for lag in range(1, 4):
        prepared[f"AAA_intraday_J-{lag}"] = returns.shift(lag)
    # Missing current labels also exclude that observation, alongside its later lags.
    prepared.loc[dates[8], ["AAA.intraday_target", "AAA.intraday_down_target"]] = np.nan
    repository = RunRepository(tmp_path / "runs")
    roots = {}
    kinds = ["constant_probability", "target_only"] + (["target_shorter_training"] if different_training else [])
    for kind in kinds:
        shorter = kind == "target_shorter_training"
        config = replace(DEFAULT_CONFIG, project_root=tmp_path,
                         predictive_model_type="target_only" if shorter else kind,
                         lag_depth=2 if shorter else 3,
                         walk_forward_min_train_size=5 if shorter else 4, walk_forward_train_size=4,
                         walk_forward_test_size=3, walk_forward_step_size=2,
                         walk_forward_window_mode="rolling" if rolling else "expanding")
        run_id = repository.create(ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config,
                                                 symbols=("AAA",), combinations_per_target=1))
        root = repository.run_directory(run_id)
        roots[kind] = root
        artifacts = root / "checkpoints/artifacts"
        artifacts.mkdir(parents=True)
        raw = pickle.dumps({"prepared": prepared})
        (artifacts / "prepared_snapshot.pkl").write_bytes(raw)
        (artifacts / "prepared_snapshot.json").write_text(json.dumps({"sha256": hashlib.sha256(raw).hexdigest()}))
        results = root / "results"
        results.mkdir()
        (results / "run_configuration.json").write_text(json.dumps({"final_holdout_start": str(dates[-2])}))
        names = model_predictor_columns(prepared, "AAA", [], config)
        data = prepared[[*names, "AAA.intraday_target", "AAA.intraday_down_target"]].dropna()
        data = data[data.index < dates[-2]]
        windows, predictions = [], []
        for window in walk_forward_windows(len(data), config):
            train, test = data.iloc[window.train_slice], data.iloc[window.test_slice]
            windows.append({"Set": '["AAA"]', "Observation": "AAA", "Window": window.number,
                            "TrainStart": train.index.min(), "TrainEnd": train.index.max(),
                            "TestStart": test.index.min(), "TestEnd": test.index.max(),
                            "TrainObservations": len(train), "TestObservations": len(test)})
            for date, row in test.iterrows():
                predictions.append({"Set": '["AAA"]', "Observation": "AAA", "Window": window.number,
                                    "Date": date, "IntradayTarget": row['AAA.intraday_target'],
                                    "DownTarget": row['AAA.intraday_down_target'],
                                    "UpProbability": .25, "DownProbability": .25})
        pd.DataFrame(windows).to_csv(results / "windows.csv", index=False)
        pd.DataFrame(predictions).to_csv(results / "predictions.csv", index=False)
    return roots, prepared


@pytest.mark.parametrize("rolling", [False, True])
def test_lags_and_interior_gap_use_exact_training_labels_and_boundaries(tmp_path, rolling):
    roots, prepared = fixture_runs(tmp_path, rolling=rolling)
    mapped = {kind: root for kind, root in roots.items()}
    frames, diagnostic = reconstruct_aligned_constants(mapped)
    target_windows = pd.read_csv(roots["target_only"] / "results/windows.csv")
    constant_windows = pd.read_csv(roots["constant_probability"] / "results/windows.csv")
    assert target_windows.iloc[0].TestStart != constant_windows.iloc[0].TestStart
    data = prepared.dropna()
    assert len(prepared) - len(data) == 7  # Initial three + missing row + three affected lags.
    for row in diagnostic[diagnostic.Run == "constant_probability"].itertuples():
        label = "AAA.intraday_target" if row.Direction == "Up" else "AAA.intraday_down_target"
        train = data.loc[row.TrainStart:row.TrainEnd]
        assert row.Probability == pytest.approx(train[label].mean())
        assert row.TrainObservations == len(train)
        assert row.TrainEnd < row.TestStart
        paired = diagnostic[(diagnostic.Run == "target_only") & (diagnostic.Window == row.Window)
                            & (diagnostic.Direction == row.Direction)].iloc[0]
        for column in ("TrainDatesDigest", "TrainLabelsDigest", "TestDatesDigest", "TrainObservations", "TestObservations"):
            assert getattr(row, column) == paired[column]
    frame = frames["constant_probability"]
    assert frame.groupby(["Window", "Direction"]).Probability.nunique().eq(1).all()
    # Test labels cannot affect the first block's baseline, even when they change its scores.
    first = target_windows.iloc[0]
    changed = prepared.copy()
    changed.loc[first.TestStart:first.TestEnd, "AAA.intraday_target"] = 1
    for root in roots.values():
        path = root / "checkpoints/artifacts/prepared_snapshot.pkl"
        raw = pickle.dumps({"prepared": changed})
        path.write_bytes(raw)
        path.with_suffix(".json").write_text(json.dumps({"sha256": hashlib.sha256(raw).hexdigest()}))
        predictions = pd.read_csv(root / "results/predictions.csv")
        selected = predictions.Date.between(first.TestStart, first.TestEnd)
        predictions.loc[selected, "IntradayTarget"] = 1
        predictions.to_csv(root / "results/predictions.csv", index=False)
    _, altered = reconstruct_aligned_constants(mapped)
    original = diagnostic[(diagnostic.Run == "constant_probability") & (diagnostic.Window == 1) & (diagnostic.Direction == "Up")]
    new = altered[(altered.Run == "constant_probability") & (altered.Window == 1) & (altered.Direction == "Up")]
    assert original.Probability.tolist() == new.Probability.tolist()


def test_existing_comparison_recalculates_only_in_memory_and_reports_mismatch(tmp_path):
    roots, _ = fixture_runs(tmp_path)
    files = {path: path.read_bytes() for root in roots.values() for path in root.rglob("*") if path.is_file()}
    ids = [root.name for root in roots.values()]
    audit, metrics = compare_common_predictions(tmp_path, ids)
    assert audit["training_alignment"]["status"] == "verified"
    assert metrics.TrainingAlignment.eq("common_training_and_test_verified").all()
    assert all(path.read_bytes() == content for path, content in files.items())
    historical, _ = compare_common_predictions(tmp_path, ids, align_constant=False)
    assert historical["training_alignment"]["status"] == "unverified"
    path = roots["target_only"] / "results/windows.csv"
    windows = pd.read_csv(path)
    windows.loc[0, "TrainObservations"] += 1
    windows.to_csv(path, index=False)
    audit, metrics = compare_common_predictions(tmp_path, ids)
    assert audit["training_alignment"]["status"] == "unavailable"
    assert "counts differ" in audit["training_alignment"]["reason"]
    assert metrics.TrainingAlignment.eq("test_dates_only_training_unverified").all()


def test_same_test_start_with_different_training_is_not_claimed_aligned(tmp_path):
    roots, _ = fixture_runs(tmp_path, different_training=True)
    long = pd.read_csv(roots["target_only"] / "results/windows.csv").iloc[0]
    short = pd.read_csv(roots["target_shorter_training"] / "results/windows.csv").iloc[0]
    assert long.TestStart == short.TestStart
    assert long.TrainStart != short.TrainStart
    assert long.TrainObservations != short.TrainObservations
    audit, metrics = compare_common_predictions(tmp_path, [root.name for root in roots.values()])
    assert audit["status"] == "available"
    assert audit["training_alignment"]["status"] == "unavailable"
    assert "differ" in audit["training_alignment"]["reason"]
    assert metrics.TrainingAlignment.eq("test_dates_only_training_unverified").all()
