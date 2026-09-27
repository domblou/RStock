import csv
import json
from pathlib import Path

import pytest

from rstock.application.run_comparison import comparison_types, load_end_to_end_comparison


def _write(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding="utf-8")


def _run(
    root: Path, *, candidates: bool, enabled: bool = True,
    forward: str | None = None, signals: int | None = None,
) -> Path:
    runs = root / "runs"
    parent = runs / "ete"
    _write(parent / "config.json", {"resolved_market_session_cutoff": "2025-09-30", "forward_simulation_enabled": enabled})
    _write(parent / "status.json", {"status": "completed"})
    _write(parent / "results/pipeline_summary.json", {
        "stages": [
            {"stage_key": "walk_forward", "child_run_id": "wf"},
            {"stage_key": "threshold_calibration", "child_run_id": "threshold"},
        ],
        "forward_simulation": (
            {"child_run_id": "forward", "status": "launched"} if forward else
            {"status": "skipped_no_models"} if enabled and not candidates else {}
        ),
    })
    _write(runs / "wf/summary.json", {
        "raw_combination_count": 200, "total_combinations": 100,
        "eligible_combinations": 20, "metrics": {"FinalConfirmedSets": 5.0},
    })
    _write(runs / "threshold/summary.json", {
        "holdout_combination_counts": {"Up": {"evaluated_combinations": 12}}
    })
    models = [{"set": '["AAA","BBB"]', "target": "AAA", "direction": "Up"}] if candidates else []
    _write(parent / "results/forward_model_snapshot.json", {"models": models, "candidate_count": len(models)})
    metrics = runs / "threshold/results/holdout_metrics.csv"
    metrics.parent.mkdir(parents=True, exist_ok=True)
    with metrics.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=["Set", "Direction", "ROCAUC", "Precision", "DirectionalReturnMean", "SignalCount"])
        writer.writeheader()
        writer.writerow({"Set": '["AAA","BBB"]', "Direction": "Up", "ROCAUC": .72, "Precision": .5, "DirectionalReturnMean": .01, "SignalCount": 25})
        writer.writerow({"Set": '["CCC","DDD"]', "Direction": "Up", "ROCAUC": .99, "Precision": 1, "DirectionalReturnMean": .5, "SignalCount": 500})
    if forward:
        _write(runs / "forward/status.json", {"status": forward})
        if forward == "completed":
            _write(runs / "forward/summary.json", {
                "source_model_count": 1, "total_signals": signals,
                "precision": .4 if signals else None,
                "directional_return_mean": .02 if signals else None,
                "sessions": 63, "first_session": "2025-10-01", "last_session": "2025-12-30",
            })
    return parent


def _historical_threshold_artifacts(root: Path, threshold_id: str = "threshold") -> None:
    result = root / "runs" / threshold_id / "results"
    _write(result / "selected_thresholds_by_set.json", {
        '["AAA","BBB"]': {"Up": {"status": "selected", "threshold": 0.6}}
    })
    (result / "threshold_metrics_by_set.csv").write_text(
        "Set,Direction,Selected\n", encoding="utf-8"
    )
    with (result / "holdout_metrics.csv").open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=[
            "Set", "Direction", "Threshold", "SignalCount", "ROCAUC", "Precision",
            "DirectionalReturnMean", "OppositeMoveFrequency",
        ])
        writer.writeheader()
        writer.writerow({
            "Set": '["AAA","BBB"]', "Direction": "Up", "Threshold": 0.6,
            "SignalCount": 25, "ROCAUC": 0.72, "Precision": 0.5,
            "DirectionalReturnMean": 0.01, "OppositeMoveFrequency": 0.1,
        })


def test_historical_v1_without_forward_snapshot_reads_frozen_threshold_candidates(tmp_path):
    parent = _run(tmp_path, candidates=True)
    (parent / "results/forward_model_snapshot.json").unlink()
    _write(parent / "orchestration/pipeline.json", {
        "schema_version": 1,
        "stages": [
            {"stage_key": "walk_forward", "child_run_id": "wf"},
            {"stage_key": "threshold_calibration", "child_run_id": "threshold"},
        ],
    })
    _historical_threshold_artifacts(tmp_path)

    item = load_end_to_end_comparison(tmp_path, "ete")

    assert (item.up_evaluable, item.candidates, item.targets) == (12, 1, 1)
    assert item.candidate_yield == 0.01
    assert (item.holdout_auc, item.holdout_precision, item.holdout_return, item.holdout_signals) == (
        0.72, 0.5, 0.01, 25,
    )


def test_historical_fallback_requires_valid_threshold_artifacts(tmp_path):
    parent = _run(tmp_path, candidates=True)
    (parent / "results/forward_model_snapshot.json").unlink()
    _historical_threshold_artifacts(tmp_path)
    (tmp_path / "runs/threshold/results/selected_thresholds_by_set.json").write_text(
        "not json", encoding="utf-8"
    )

    item = load_end_to_end_comparison(tmp_path, "ete")

    assert item.candidates is None
    assert item.holdout_auc is None


@pytest.mark.parametrize("threshold_mode", ["inherited", "recomputed"])
def test_derived_v2_resolves_effective_threshold_stage(tmp_path, threshold_mode):
    _run(tmp_path, candidates=True)
    _historical_threshold_artifacts(tmp_path)
    derived = tmp_path / "runs" / "derived"
    _write(derived / "config.json", {"forward_simulation_enabled": False})
    _write(derived / "status.json", {"status": "completed"})
    threshold_stage = (
        {"stage_key": "threshold_calibration", "mode": "inherited", "source_run_id": "threshold"}
        if threshold_mode == "inherited" else
        {"stage_key": "threshold_calibration", "mode": "recomputed", "child_run_id": "threshold_derived"}
    )
    if threshold_mode == "recomputed":
        _historical_threshold_artifacts(tmp_path, "threshold_derived")
        _write(tmp_path / "runs/threshold_derived/summary.json", {
            "holdout_combination_counts": {"Up": {"evaluated_combinations": 12}}
        })
    _write(derived / "orchestration/pipeline.json", {
        "schema_version": 2,
        "stages": [
            {"stage_key": "walk_forward", "mode": "inherited", "source_run_id": "wf"},
            threshold_stage,
        ],
    })

    item = load_end_to_end_comparison(tmp_path, "derived")

    assert (item.evaluated, item.candidates, item.targets) == (100, 1, 1)
    assert item.up_evaluable == 12
    assert item.holdout_auc == 0.72
    assert item.holdout_signals == 25


@pytest.mark.parametrize("count", [2, 5])
def test_homogeneous_comparison_modes_and_mixed_rejection(count):
    assert comparison_types(["walk_forward"] * count) == "walk_forward"
    assert comparison_types(["end_to_end"] * count) == "end_to_end"
    assert comparison_types(["walk_forward", "end_to_end"]) is None
    assert comparison_types(["end_to_end"] * 6) is None


def test_candidate_cohort_and_funnel_use_distinct_populations(tmp_path):
    _run(tmp_path, candidates=True, forward="completed", signals=37)
    item = load_end_to_end_comparison(tmp_path, "ete")
    assert (item.raw, item.evaluated, item.qualified, item.confirmed) == (200, 100, 20, 5)
    assert item.confirmation_rate == .25
    assert item.up_evaluable == 12
    assert item.candidates == item.targets == 1
    assert item.candidate_yield == .01
    assert (item.holdout_auc, item.holdout_precision, item.holdout_return, item.holdout_signals) == (.72, .5, .01, 25)
    assert item.forward_status == "terminée avec signaux"
    assert (item.forward_signals, item.forward_sessions) == (37, 63)


@pytest.mark.parametrize(
    ("candidates", "enabled", "forward", "signals", "expected"),
    [
        (False, True, None, None, "skipped_no_models"),
        (True, False, None, None, "désactivée"),
        (True, True, None, None, "non lancée"),
        (True, True, "pending", None, "en attente"),
        (True, True, "running", None, "en cours"),
        (True, True, "failed", None, "échouée"),
        (True, True, "cancelled", None, "annulée"),
        (True, True, "interrupted", None, "interrompue"),
        (True, True, "completed", 0, "terminée sans signaux"),
        (True, True, "completed", 4, "terminée avec signaux"),
    ],
)
def test_forward_states(tmp_path, candidates, enabled, forward, signals, expected):
    _run(tmp_path, candidates=candidates, enabled=enabled, forward=forward, signals=signals)
    item = load_end_to_end_comparison(tmp_path, "ete")
    assert item.forward_status == expected
    if not candidates:
        assert item.candidates == 0
        assert item.holdout_auc is None
        assert item.forward_precision is None
    if forward == "completed" and signals == 0:
        assert item.forward_signals == 0
        assert item.forward_precision is None
    if forward in {"running", "failed"}:
        assert item.forward_signals is None


def test_missing_artifacts_are_not_zero(tmp_path):
    _write(tmp_path / "runs/ete/config.json", {"forward_simulation_enabled": True})
    item = load_end_to_end_comparison(tmp_path, "ete")
    assert item.evaluated is None
    assert item.candidates is None
    assert item.candidate_yield is None
    assert item.forward_status == "non lancée"


def test_completed_forward_with_no_models_keeps_business_result_visible(tmp_path):
    _run(tmp_path, candidates=False, forward="completed", signals=0)
    _write(tmp_path / "runs/forward/summary.json", {
        "result": "skipped_no_models", "candidate_count": 0,
        "source_model_count": 0, "simulation_executed": False,
        "reason": "no_eligible_models", "total_signals": 0,
        "precision": None, "directional_return_mean": None,
    })
    item = load_end_to_end_comparison(tmp_path, "ete")
    assert item.forward_status == "completed · skipped_no_models"
    assert item.forward_models == 0
    assert item.forward_signals == 0
    assert item.forward_precision is None


def test_comparison_view_has_critical_help_and_no_end_to_end_chart():
    source = Path("rstock/application/streamlit_app.py").read_text(encoding="utf-8")
    end_to_end = source.split("def _render_end_to_end_comparison", 1)[1].split("def _render_run_comparison_view", 1)[0]
    for label in (
        "Confirmées holdout WF", "Up évaluables", "Candidats finaux", "Candidate yield",
        "Signaux holdout", "Statut Forward", "Précision Forward", "Période Forward",
    ):
        assert label in end_to_end
    assert "st.column_config.TextColumn(help=description)" in end_to_end
    assert "st.altair_chart" not in end_to_end
    assert "Aucun modèle admissible — Forward Simulation non exécutée." in source
