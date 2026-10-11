import json
from pathlib import Path

import pandas as pd
import pytest

from rstock.application.common_prediction_comparison import compare_common_predictions, result_availability


def run(tmp_path, run_id, dates, *, set_id='["AAA"]', kind="constant_probability", probabilities=None):
    root = tmp_path / "runs" / run_id
    (root / "results").mkdir(parents=True)
    (root / "config.json").write_text(json.dumps({"job_type": "walk_forward", "rstock_config": {"predictive_model_type": kind}}))
    frame = pd.DataFrame({"Set": set_id, "Observation": "AAA", "Date": dates, "Window": 0,
        "TrainEnd": "2025-01-01", "IntradayTarget": [int(d[-2:]) % 2 for d in dates],
        "DownTarget": [1 - int(d[-2:]) % 2 for d in dates],
        "UpProbability": probabilities or [.25] * len(dates), "DownProbability": .2})
    frame.to_csv(root / "results/predictions.csv", index=False)
    return root


def test_four_types_use_actual_intersection_and_individual_metrics(tmp_path):
    kinds = ("constant_probability", "target_only", "target_and_external", "external_only")
    for i, kind in enumerate(kinds):
        dates = [f"2025-01-{d:02d}" for d in range(2 + i, 9)]
        run(tmp_path, str(i), dates, kind=kind)
    audit, metrics = compare_common_predictions(tmp_path, ["0", "1", "2", "3"])
    assert audit["status"] == "available"
    assert audit["common_observations"] == 8  # Four dates for each direction.
    assert len(metrics) == 8
    assert metrics["CommonObservations"].eq(4).all()
    assert metrics["ROCAUC"].eq(.5).all()
    assert metrics["Brier"].notna().all()
    assert metrics["LogLoss"].notna().all()
    assert metrics["FirstDate"].eq(pd.Timestamp("2025-01-05")).all()


def test_candidate_completeness_without_probability_ensemble(tmp_path):
    a = run(tmp_path, "a", ["2025-01-02", "2025-01-03", "2025-01-04"], set_id='["AAA","BBB"]')
    second = pd.read_csv(a / "results/predictions.csv").iloc[1:].copy()
    second["Set"] = '["AAA","CCC"]'
    second["UpProbability"] = .9
    first = pd.read_csv(a / "results/predictions.csv")
    pd.concat([first, second]).to_csv(a / "results/predictions.csv", index=False)
    run(tmp_path, "b", ["2025-01-02", "2025-01-03", "2025-01-04"])
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "available"
    assert metrics["CommonObservations"].eq(2).all()
    assert len(metrics) == 6
    a_up = metrics[(metrics["Run"] == "a") & (metrics["Direction"] == "Up")]
    assert a_up["Brier"].nunique() == 2


def test_overlap_uses_latest_historical_origin(tmp_path):
    a = run(tmp_path, "a", ["2025-01-03", "2025-01-04"])
    first = pd.read_csv(a / "results/predictions.csv")
    latest = first.copy(); latest["TrainEnd"] = "2025-01-02"; latest["UpProbability"] = .8
    pd.concat([first, latest]).to_csv(a / "results/predictions.csv", index=False)
    run(tmp_path, "b", ["2025-01-03", "2025-01-04"])
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "available"
    assert metrics.loc[(metrics.Run == "a") & (metrics.Direction == "Up"), "Brier"].iloc[0] == pytest.approx(.34)


def test_mismatched_labels_and_missing_predictions_are_explicit(tmp_path):
    run(tmp_path, "a", ["2025-01-02", "2025-01-03"])
    b = run(tmp_path, "b", ["2025-01-02", "2025-01-03"])
    frame = pd.read_csv(b / "results/predictions.csv"); frame["IntradayTarget"] = 1 - frame["IntradayTarget"]
    frame.to_csv(b / "results/predictions.csv", index=False)
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "label_mismatch" and metrics.empty
    (b / "results/predictions.csv").unlink()
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "unavailable" and metrics.empty
    assert "b" in audit["unavailable_runs"]


def test_empty_intersection_and_independent_result_states(tmp_path):
    a = run(tmp_path, "a", ["2025-01-02"])
    run(tmp_path, "b", ["2025-01-03"])
    (a / "results/run_configuration.json").write_text(json.dumps({"candidate_status": "no_qualified_candidates", "holdout_status": "not_executed"}))
    assert result_availability(a) == {"candidate_status": "no_qualified_candidates", "holdout_status": "not_executed", "data_status": "available"}
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "no_common_observations" and metrics.empty


def test_conflicting_same_origin_is_unavailable_not_averaged(tmp_path):
    a = run(tmp_path, "a", ["2025-01-02"])
    frame = pd.read_csv(a / "results/predictions.csv"); conflict = frame.copy(); conflict["UpProbability"] = .9
    pd.concat([frame, conflict]).to_csv(a / "results/predictions.csv", index=False)
    run(tmp_path, "b", ["2025-01-02"])
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "unavailable" and metrics.empty



def test_long_holdout_predictions_use_frozen_decisions(tmp_path):
    for run_id in ("a", "b"):
        root = run(tmp_path, run_id, ["2025-01-02", "2025-01-03"])
        pd.DataFrame({"Set": '["AAA"]', "Observation": "AAA", "Direction": "Up",
            "Date": ["2025-01-04", "2025-01-05"], "TrainEnd": "2025-01-03",
            "Target": [0, 1], "Probability": [.4, .4], "Prediction": [1, 1]}).to_csv(root / "results/holdout_predictions.csv", index=False)
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"], phase="holdout")
    assert audit["status"] == "available"
    assert metrics["CommonObservations"].eq(2).all()
    assert metrics["TP"].eq(1).all() and metrics["FP"].eq(1).all()


def test_e2e_comparison_reads_wf_individual_predictions(tmp_path):
    for run_id in ("a", "b"):
        wf_root = run(tmp_path, run_id + "-wf", ["2025-01-02", "2025-01-03"])
        root = tmp_path / "runs" / run_id
        (root / "results").mkdir(parents=True)
        (root / "config.json").write_text(json.dumps({"job_type": "end_to_end"}))
        (root / "results/pipeline_summary.json").write_text(json.dumps({"candidate_status": "no_qualified_candidates", "holdout_status": "not_executed",
            "stages": [{"stage_key": "walk_forward", "child_run_id": run_id + "-wf"}]}))
    audit, metrics = compare_common_predictions(tmp_path, ["a", "b"])
    assert audit["status"] == "available"
    assert all(values["data_status"] == "available" for values in audit["availability"].values())
    assert metrics["CommonObservations"].eq(2).all()
