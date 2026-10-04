"""History comparison of real persisted Predictor prefilter artifacts."""

from __future__ import annotations

import csv
import io
import json
from pathlib import Path

import pytest

from rstock.application.history_analysis import selected_run_action
from rstock.application.prefilter_comparison import (
    ND, compare_prefilter_candidates, load_prefilter_comparison,
    prefilter_comparison_csv, prefilter_profile_differences,
)
from rstock.application.run_comparison import comparison_types
from rstock.combinations import canonical_combination_id


def _json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding="utf-8")


def _table(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def _run(
    root: Path, run_id: str, *, method: str, cutoff: str,
    profile: str, worst_auc: float, selected: tuple[tuple[str, str], ...],
) -> None:
    directory = root / "runs" / run_id
    config = {
        "job_type": "predictor_prefilter",
        "requested_historical_cutoff": cutoff,
        "resolved_market_session_cutoff": cutoff,
        "primary_universe_id": "SP500",
        "prefilter_method": method,
        "prefilter_profile": profile,
        "rstock_config": {
            "predictor_prefilter_top_n": 2,
            "predictor_prefilter_min_worst_auc": worst_auc,
            "predictor_prefilter_min_pct_above_random": 0.6,
            "predictor_prefilter_min_median_auc": 0.55,
            "predictor_prefilter_max_auc_std": 0.12,
            "predictor_prefilter_correlation_threshold": 0.9,
        },
    }
    _json(directory / "config.json", config)
    _json(directory / "summary.json", {"prepared_dataset_as_of": cutoff})
    by_target: dict[str, list[str]] = {}
    for target, predictor in selected:
        by_target.setdefault(target, []).append(predictor)
    _json(directory / "results/predictor_prefilter.json", {
        "predictors_by_target": by_target,
        "prepared_dataset_as_of": cutoff,
    })
    identities = (("AAA", "BBB"), ("AAA", "CCC"), ("CCC", "BBB"))
    if method == "temporal_stability":
        _table(directory / "results/predictor_prefilter.csv", [{
            "Observation": target, "Predictor": predictor,
            "EligibleFrequency": 1.0 if index < 2 else 0.0,
            "PrefilterStatus": "retained" if (target, predictor) in selected else
                "rejected_top_n" if index < 2 else "rejected_ineligible",
        } for index, (target, predictor) in enumerate(identities)])
        _table(directory / "results/predictor_prefilter_origins.csv", [{
            "OriginCutoff": cutoff, "Observation": target, "Predictor": predictor,
        } for target, predictor in identities])
    else:
        _table(directory / "results/predictor_prefilter.csv", [{
            "Observation": target, "Predictor": predictor,
            "Eligible": index < 2,
            "PrefilterStatus": "retained" if (target, predictor) in selected else
                "rejected_top_n" if index < 2 else "rejected_threshold",
        } for index, (target, predictor) in enumerate(identities)])


def test_compare_mode_accepts_two_to_six_homogeneous_prefilters_and_rejects_mixed():
    for count in range(2, 7):
        assert comparison_types(["predictor_prefilter"] * count) == "predictor_prefilter"
        assert selected_run_action([str(index) for index in range(count)]) == "comparison"
    assert comparison_types(["predictor_prefilter"] * 7) is None
    assert comparison_types(["predictor_prefilter", "walk_forward"]) is None
    assert comparison_types(["predictor_prefilter", "end_to_end"]) is None
    assert selected_run_action([str(index) for index in range(7)]) is None


def test_prefilter_loader_reads_profiles_funnel_and_cutoffs(tmp_path):
    _run(tmp_path, "a", method="single_origin", cutoff="2026-07-06",
         profile="profile-A", worst_auc=.35,
         selected=(("AAA", "BBB"), ("CCC", "BBB")))
    _run(tmp_path, "b", method="temporal_stability", cutoff="2026-07-07",
         profile="profile-B", worst_auc=.45,
         selected=(("AAA", "BBB"), ("AAA", "CCC")))
    a = load_prefilter_comparison(tmp_path, "a")
    b = load_prefilter_comparison(tmp_path, "b")
    assert (a.requested_cutoff, a.resolved_cutoff) == ("2026-07-06", "2026-07-06")
    assert (a.before, a.passed, a.after_top_n, a.retained, a.survival_rate) == (3, 2, 2, 2, 2 / 3)
    assert (b.before, b.passed, b.retained, b.origin_evaluations) == (3, 2, 2, 3)
    assert a.display_row()["Worst AUC min"] == .35
    assert b.display_row()["Worst AUC min"] == .45
    assert a.universe == "SP500"
    differences = prefilter_profile_differences((a, b))
    assert "Profil / preset" in differences
    assert "Méthode" in differences
    assert "predictor_prefilter_min_worst_auc" in differences


def test_prefilter_candidate_overlap_uses_canonical_identity(tmp_path):
    for run_id, selected in (
        ("a", (("AAA", "BBB"), ("CCC", "BBB"))),
        ("b", (("AAA", "BBB"), ("AAA", "CCC"))),
        ("c", (("AAA", "BBB"),)),
    ):
        _run(tmp_path, run_id, method="single_origin", cutoff="2026-07-06",
             profile="profile-A", worst_auc=.35, selected=selected)
    items = [load_prefilter_comparison(tmp_path, run_id) for run_id in ("a", "b", "c")]
    overlap = compare_prefilter_candidates(items)
    shared = canonical_combination_id("AAA", "Up", ["BBB"])
    assert overlap.common == frozenset({shared})
    assert {run_id: len(values) for run_id, values in overlap.own.items()} == {
        "a": 1, "b": 1, "c": 0,
    }
    assert overlap.overlap_rate == pytest.approx(1 / 3)


def test_prefilter_comparison_csv_is_self_contained(tmp_path):
    _run(tmp_path, "a", method="single_origin", cutoff="2026-07-06",
         profile="profile-A", worst_auc=.35, selected=(("AAA", "BBB"),))
    _run(tmp_path, "b", method="temporal_stability", cutoff="2026-07-07",
         profile="profile-B", worst_auc=.45, selected=(("AAA", "BBB"), ("AAA", "CCC")))
    items = [load_prefilter_comparison(tmp_path, run_id) for run_id in ("a", "b")]
    exported = list(csv.DictReader(io.StringIO(prefilter_comparison_csv(items))))
    summary = [row for row in exported if row["record_type"] == "run_summary"]
    candidates = [row for row in exported if row["record_type"] == "candidate"]
    assert len(summary) == 2
    assert summary[0]["Run ID"] == "a"
    assert summary[0]["Cutoff demandé"] == "2026-07-06"
    assert summary[0]["Worst AUC min"] == "0.35"
    assert summary[0]["Combinaisons avant filtre"] == "3"
    assert json.loads(summary[0]["profile_parameters_json"])["predictor_prefilter_top_n"] == 2
    assert len(candidates) == 2
    assert any(row["present_in_a"] == "1" and row["present_in_b"] == "1"
               and row["present_in_all"] == "1" for row in candidates)
    assert any(row["present_in_a"] == "0" and row["present_in_b"] == "1"
               for row in candidates)


def test_six_prefilter_runs_export_six_presence_columns(tmp_path):
    run_ids = [f"run-{index}" for index in range(6)]
    for run_id in run_ids:
        _run(tmp_path, run_id, method="single_origin", cutoff="2026-07-06",
             profile="profile-A", worst_auc=.35, selected=(("AAA", "BBB"),))
    items = [load_prefilter_comparison(tmp_path, run_id) for run_id in run_ids]
    exported = list(csv.DictReader(io.StringIO(prefilter_comparison_csv(items))))
    candidate = next(row for row in exported if row["record_type"] == "candidate")
    assert all(candidate[f"present_in_{run_id}"] == "1" for run_id in run_ids)
    assert candidate["present_in_all"] == "1"


def test_old_prefilter_missing_fields_and_artifacts_display_nd(tmp_path):
    _json(tmp_path / "runs/old/config.json", {"job_type": "predictor_prefilter"})
    item = load_prefilter_comparison(tmp_path, "old")
    row = item.display_row()
    assert row["Cutoff demandé"] == ND
    assert row["Profil / preset"] == ND
    assert row["Worst AUC min"] == ND
    assert row["Combinaisons avant filtre"] == ND
    assert item.candidates is None
    _run(tmp_path, "new", method="single_origin", cutoff="2026-07-06",
         profile="profile-A", worst_auc=.35, selected=(("AAA", "BBB"),))
    other = load_prefilter_comparison(tmp_path, "new")
    assert compare_prefilter_candidates((item, other)).overlap_rate is None
    exported = list(csv.DictReader(io.StringIO(prefilter_comparison_csv((item, other)))))
    assert len(exported) == 3
    candidate = next(row for row in exported if row["record_type"] == "candidate")
    assert candidate["present_in_old"] == ND
    assert candidate["present_in_new"] == "1"
