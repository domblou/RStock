import hashlib
import json
import pickle
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from rstock.features import intraday_target_column, intraday_down_target_column
from rstock.application import rare_relations_analysis as rare


def fixture_source(tmp_path):
    index = pd.bdate_range("2025-07-07", periods=10)
    up, down = intraday_target_column("AAA"), intraday_down_target_column("AAA")
    frame = pd.DataFrame({"feature": np.arange(10, dtype=float),
        up: [0, 1, 1, 1, 0, 1, 0, 1, 0, 1], down: [0, 0, 0, 0, 1, 0, 1, 0, 1, 0]}, index=index)
    frame.loc[index[1], "feature"] = np.nan
    frame.loc[index[2], down] = np.nan
    key = '["AAA", "BBB"]'
    windows = pd.DataFrame([
        {"Set": key, "Window": 1, "Observation": "AAA", "FeatureColumns": '["feature"]',
         "TrainStart": index[0], "TrainEnd": index[3], "TrainObservations": 2,
         "TestStart": index[4], "TestEnd": index[6], "TestObservations": 3},
        {"Set": key, "Window": 2, "Observation": "AAA", "FeatureColumns": '["feature"]',
         "TrainStart": index[0], "TrainEnd": index[6], "TrainObservations": 5,
         "TestStart": index[7], "TestEnd": index[9], "TestObservations": 3},
    ])
    predictions = pd.DataFrame({"Set": key, "Window": [1]*3+[2]*3,
        "Date": index[4:].strftime("%Y-%m-%d"), "Observation": "AAA",
        "IntradayTarget": frame[up].iloc[4:].to_numpy(), "UpProbability": [.3, .7, .2, .6, .4, .6]})
    source = tmp_path / "source"
    (source / "results").mkdir(parents=True)
    (source / "checkpoints/artifacts").mkdir(parents=True)
    windows.to_csv(source / "results/windows.csv", index=False)
    predictions.to_csv(source / "results/predictions.csv", index=False)
    raw = pickle.dumps({"prepared": frame})
    (source / "checkpoints/artifacts/prepared_snapshot.pkl").write_bytes(raw)
    (source / "checkpoints/artifacts/prepared_snapshot.json").write_text(json.dumps({"sha256": hashlib.sha256(raw).hexdigest()}))
    return source, frame, windows, predictions


def test_mask_and_constant_never_use_test_labels(tmp_path):
    _, frame, windows, predictions = fixture_source(tmp_path)
    refs, size = rare.build_references(frame, windows)
    ref = refs[(windows.Set.iloc[0], 1)]
    assert ref.probability == .5  # both feature and Down-label masks are applied
    changed = frame.copy()
    changed.loc[changed.index[4:], intraday_target_column("AAA")] = 1
    other, _ = rare.build_references(changed, windows)
    assert other[(windows.Set.iloc[0], 1)].probability == ref.probability
    scored = rare.score_chunk(predictions, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    assert scored.Period.tolist() == ["selection"]*3 + ["evaluation"]*3
    assert np.allclose(scored.GainLogLoss, scored.ConstantLogLoss - scored.LogLoss)


def test_archive_corruption_and_duplicate_rows_rejected(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    path = source / "checkpoints/artifacts/prepared_snapshot.pkl"
    path.write_bytes(path.read_bytes()+b"changed")
    with pytest.raises(ValueError, match="Empreinte"):
        rare.load_snapshot(path, path.with_suffix(".json"))
    refs, size = rare.build_references(frame, windows)
    seen = np.zeros(size, bool)
    rare.score_chunk(predictions.iloc[:2], frame, refs, seen, "2025-07-15", "2025-07-18")
    with pytest.raises(ValueError, match="déjà rencontrée"):
        rare.score_chunk(predictions.iloc[:1], frame, refs, seen, "2025-07-15", "2025-07-18")


def test_bad_labels_dates_and_overlapping_windows_rejected(tmp_path):
    _, frame, windows, predictions = fixture_source(tmp_path)
    refs, size = rare.build_references(frame, windows)
    bad = predictions.copy()
    bad.loc[0, "IntradayTarget"] = 1
    with pytest.raises(ValueError, match="Labels"):
        rare.score_chunk(bad, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    bad = predictions.copy()
    bad.loc[0, "Date"] = "2025-07-07"
    with pytest.raises(ValueError, match="calendrier"):
        rare.score_chunk(bad, frame, refs, np.zeros(size, bool), "2025-07-15", "2025-07-18")
    duplicate = windows.iloc[[0, 0]].copy()
    duplicate.iloc[1, duplicate.columns.get_loc("Window")] = 2
    with pytest.raises(ValueError, match="chevauchantes"):
        rare.build_references(frame, duplicate)


def test_missing_scientific_rules_cannot_be_bypassed_or_execute(tmp_path):
    source, _, _, _ = fixture_source(tmp_path)
    parameters = rare.validate_parameters({})
    preview = rare.preflight(SimpleNamespace(directory=source), parameters)
    assert not preview["ready"] and "critères de sélection" in preview["reasons"][0]
    assert preview["temporal_summary"]["selection_sessions"] == 3
    with pytest.raises(ValueError, match="Paramètres inconnus"):
        rare.validate_parameters({"approved": True})
    with pytest.raises(ValueError, match="Lancement bloqué"):
        rare.execute(SimpleNamespace(parameters=parameters))


def test_calendar_default_and_session_mode_resolve_same_common_boundary(tmp_path):
    source, _, _, _ = fixture_source(tmp_path)
    before = {path: path.read_bytes() for path in source.rglob("*") if path.is_file()}
    calendar = rare.source_calendar(source)
    split = rare.resolve_split(calendar, {})
    assert split["total_sessions"] == 6 and split["selection_sessions"] == 3
    assert split["wf_start"] == "2025-07-11" and split["wf_end"] == "2025-07-18"
    assert split["selection_end"] == "2025-07-15" and split["evaluation_start"] == "2025-07-16"
    assert split == rare.resolve_split(calendar, {"selection_mode": "sessions", "selection_sessions": 3})
    assert before == {path: path.read_bytes() for path in before}
    frozen = rare.preflight(SimpleNamespace(directory=source), {})["resolved_parameters"]
    assert rare.resolve_split(calendar, frozen) == split
    changed = {**frozen, "selection_fraction": .8}
    with pytest.raises(ValueError, match="découpage figé"):
        rare.resolve_split(calendar, changed)


def test_partial_windows_and_missing_candidate_dates_preserve_union_calendar(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    other = windows.copy()
    other["Set"] = '["AAA", "CCC"]'
    windows = pd.concat([windows, other], ignore_index=True)
    rows = predictions.iloc[[0, 2, 4]].copy()
    rows["Set"] = other.Set.iloc[0]
    windows.to_csv(source / "results/windows.csv", index=False)
    pd.concat([predictions, rows], ignore_index=True).to_csv(source / "results/predictions.csv", index=False)
    calendar = rare.source_calendar(source)
    split = rare.resolve_split(calendar, {})
    assert split["total_sessions"] == 6 and split["selection_end"] == "2025-07-15"
    assert split["coverage"][0]["selection_sessions"] == 3
    assert split["coverage"][1]["selection_sessions"] == 2
    assert split["coverage"][1]["evaluation_sessions"] == 1
    assert len(split["warnings"]) == 2
    # A date absent from every archived prediction is not manufactured from the snapshot.
    archived = pd.concat([predictions, rows], ignore_index=True)
    archived = archived.loc[archived.Date.ne(predictions.Date.iloc[1])]
    archived.to_csv(source / "results/predictions.csv", index=False)
    updated = rare.source_calendar(source)
    assert len(updated["dates"]) == 5 and pd.Timestamp(predictions.Date.iloc[1]) not in updated["dates"]


@pytest.mark.parametrize("parameters", [
    {"selection_mode": "sessions", "selection_sessions": 6},
    {"selection_fraction": .01},
    {"selection_mode": "sessions", "selection_sessions": 3, "selection_fraction": .6},
    {"selection_fraction": 1.}, {"selection_fraction": True},
    {"selection_mode": "sessions", "selection_sessions": 2.5},
])
def test_empty_periods_and_exclusive_modes_rejected(tmp_path, parameters):
    source, _, _, _ = fixture_source(tmp_path)
    with pytest.raises(ValueError):
        rare.resolve_split(rare.source_calendar(source), parameters)


def test_calendar_does_not_depend_on_future_probabilities_or_loss_values(tmp_path):
    source, frame, windows, predictions = fixture_source(tmp_path)
    initial = rare.resolve_split(rare.source_calendar(source), {})
    predictions.loc[3:, "UpProbability"] = [.99, .01, .99]
    predictions.to_csv(source / "results/predictions.csv", index=False)
    assert rare.resolve_split(rare.source_calendar(source), {}) == initial
    assert not hasattr(rare, "ApprovedProtocol") and not hasattr(rare, "APPROVED_PROTOCOLS")


def test_invalid_or_missing_prediction_calendar_blocks_preview(tmp_path):
    source, _, _, predictions = fixture_source(tmp_path)
    predictions.loc[0, "Date"] = "2025-07-07"
    predictions.to_csv(source / "results/predictions.csv", index=False)
    with pytest.raises(ValueError, match="calendrier"):
        rare.source_calendar(source)
    predictions.iloc[:0].to_csv(source / "results/predictions.csv", index=False)
    with pytest.raises(ValueError, match="deux séances"):
        rare.source_calendar(source)
