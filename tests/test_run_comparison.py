import csv
import json
from contextlib import nullcontext
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace

import pytest

from rstock.application.run_comparison import (
    QualificationRejectionAnalysis, RejectionProximity, _qualification_rejection_analysis,
    comparison_types, load_end_to_end_comparison,
)


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


@pytest.mark.parametrize("schema_version", [3, 4])
def test_split_comparison_reads_qualification_and_holdout_from_their_own_runs(
    tmp_path, schema_version,
):
    parent = _run(tmp_path, candidates=False)
    (parent / "results/forward_model_snapshot.json").unlink()
    _write(parent / "orchestration/pipeline.json", {
        "schema_version": schema_version,
        "stages": [
            ({"stage_key": "walk_forward", "child_run_id": "wf"}
             if schema_version == 3 else
             {"stage_key": "walk_forward", "mode": "inherited", "source_run_id": "wf"}),
            ({"stage_key": "threshold_calibration", "child_run_id": "threshold"}
             if schema_version == 3 else
             {"stage_key": "threshold_calibration", "mode": "inherited", "source_run_id": "threshold"}),
            ({"stage_key": "holdout_evaluation", "child_run_id": "holdout"}
             if schema_version == 3 else
             {"stage_key": "holdout_evaluation", "mode": "inherited", "source_run_id": "holdout"}),
            {"stage_key": "promotion_qualification", "child_run_id": "qualification",
             **({"mode": "recomputed"} if schema_version == 4 else {})},
        ],
    })
    _write(tmp_path / "runs/holdout/summary.json", {
        "run_configuration": {"holdout_combination_counts": {
            "Up": {"evaluated_combinations": 4}
        }}
    })
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "decisions": [{"Combinaison": '["AAA","BBB"]', "Cible": "AAA",
                       "candidate": True}]
    })
    source = tmp_path / "runs/threshold/results/holdout_metrics.csv"
    destination = tmp_path / "runs/holdout/results/holdout_metrics.csv"
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(source.read_bytes())
    source.unlink()

    item = load_end_to_end_comparison(tmp_path, "ete")

    assert (item.up_evaluable, item.candidates, item.targets) == (4, 1, 1)
    assert (item.holdout_auc, item.holdout_signals) == (0.72, 25)


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


@pytest.mark.parametrize("count", [2, 5, 6])
def test_homogeneous_comparison_modes_and_mixed_rejection(count):
    assert comparison_types(["walk_forward"] * count) == "walk_forward"
    assert comparison_types(["end_to_end"] * count) == "end_to_end"
    assert comparison_types(["predictor_prefilter"] * count) == "predictor_prefilter"
    assert comparison_types(["forward_simulation"] * count) == "forward_simulation"
    assert comparison_types(["walk_forward", "end_to_end"]) is None
    assert comparison_types(["predictor_prefilter", "walk_forward"]) is None
    assert comparison_types(["predictor_prefilter", "end_to_end"]) is None
    assert comparison_types(["end_to_end"] * 7) is None


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
        "Comparabilité", "Funnel scientifique", "Confirmées holdout WF", "Non calculé",
        "Holdout E2E — population évaluable", "Candidats finaux", "Candidate yield",
        "Analyse des rejets de qualification",
    ):
        assert label in end_to_end
    assert "Statut Forward" not in end_to_end
    assert "st.altair_chart" not in end_to_end
    assert "Aucun modèle admissible — Forward Simulation non exécutée." in source


def test_split_holdout_population_is_visible_without_final_candidates(tmp_path):
    parent = _run(tmp_path, candidates=False)
    _write(parent / "orchestration/pipeline.json", {"stages": [
        {"stage_key": "walk_forward", "child_run_id": "wf"},
        {"stage_key": "threshold_calibration", "child_run_id": "threshold"},
        {"stage_key": "holdout_evaluation", "child_run_id": "holdout"},
        {"stage_key": "promotion_qualification", "child_run_id": "qualification"},
    ]})
    _write(tmp_path / "runs/wf/config.json", {"evaluate_final_holdout": False})
    _write(tmp_path / "runs/wf/results/run_configuration.json", {"final_holdout_evaluated": False})
    _write(tmp_path / "runs/threshold/summary.json", {"source_qualified_combinations": 20})
    _write(tmp_path / "runs/holdout/summary.json", {"run_configuration": {
        "holdout_combination_counts": {"Up": {"evaluated_combinations": 2}}
    }})
    metrics = tmp_path / "runs/holdout/results/holdout_metrics.csv"
    metrics.parent.mkdir(parents=True, exist_ok=True)
    with metrics.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=["Set", "Direction", "ROCAUC", "Precision",
                                                   "DirectionalReturnMean", "SignalCount"])
        writer.writeheader()
        writer.writerow({"Set": "a", "Direction": "Up", "ROCAUC": .4,
                         "Precision": .2, "DirectionalReturnMean": .01, "SignalCount": 10})
        writer.writerow({"Set": "b", "Direction": "Up", "ROCAUC": .6,
                         "Precision": .4, "DirectionalReturnMean": "", "SignalCount": 0})
        writer.writerow({"Set": "a", "Direction": "Down", "ROCAUC": .9,
                         "Precision": .9, "DirectionalReturnMean": .9, "SignalCount": 50})
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": [], "decisions": [
            {"Cible": "a", "Combinaison": "a", "Direction": "Up", "candidate": False,
             "reasons": ["AUC 0,4 < 0,6", "Précision 20 % < 40 %"]},
            {"Cible": "b", "Combinaison": "b", "Direction": "Up", "candidate": False,
             "reasons": ["10 signaux < 20", "Rendement <= 0"]},
        ],
    })

    item = load_end_to_end_comparison(tmp_path, "ete")

    assert item.wf_final_holdout_evaluated is False
    assert item.confirmed is None and item.confirmation_rate is None
    assert item.calibration_entries == 20
    assert item.up_evaluable == 2 and item.qualification_passed == 0
    assert item.holdout_population == {
        "count": 2, "auc_median": .5, "precision_median": .30000000000000004,
        "return_mean": .01, "return_median": .01, "return_models": 1,
        "signals_median": 5.0, "signals_total": 10.0,
    }
    assert item.candidate_population is None and item.final_candidates == 0
    assert item.rejection_qualification_source == "Normale"
    assert item.rejection_counts == {"AUC insuffisante": 1, "Précision insuffisante": 1,
                                     "Signaux insuffisants": 1, "Rendement insuffisant": 1}
    assert (item.rejection_analysis.rejected, item.rejection_analysis.one_criterion,
            item.rejection_analysis.two_criteria, item.rejection_analysis.three_or_more_criteria) == (2, 0, 2, 0)


def test_qualification_rejections_count_unique_combinations_and_criteria(tmp_path):
    parent = _run(tmp_path, candidates=False)
    _write(parent / "orchestration/pipeline.json", {"stages": [
        {"stage_key": "promotion_qualification", "child_run_id": "qualification"},
    ]})
    reasons_by_set = {
        "a": ["Précision 30 % < 40 %"],
        "b": ["AUC 0,50 < 0,60"],
        "c": ["5 signaux < 20", "Précision 30 % < 40 %"],
        "d": ["Rendement <= 0", "Mouvements opposés 40 % > 30 %", "AUC 0,50 < 0,60"],
        "e": ["Seuil Up requis absent"],
        "f": ["Rendement directionnel moyen absente"],
    }
    decisions = [
        {"Cible": name, "Combinaison": name, "Direction": "Up",
         "candidate": False, "reasons": reasons}
        for name, reasons in reasons_by_set.items()
    ]
    decisions.append(dict(decisions[0]))  # Repeated artifact row is still one combination.
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": [], "decisions": decisions,
    })

    analysis = load_end_to_end_comparison(tmp_path, "ete").rejection_analysis

    assert (analysis.rejected, analysis.one_criterion, analysis.two_criteria,
            analysis.three_or_more_criteria) == (6, 4, 1, 1)
    assert analysis.rejected == analysis.one_criterion + analysis.two_criteria + analysis.three_or_more_criteria
    assert analysis.single == {"Précision": 1, "AUC": 1, "Seuil": 1, "Rendement": 1}
    assert analysis.involved == {
        "Précision": 2, "AUC": 2, "Signaux": 1, "Rendement": 2,
        "Mouvement opposé": 1, "Seuil": 1,
    }


def test_qualification_rejection_analysis_distinguishes_empty_from_missing_artifact(tmp_path):
    parent = _run(tmp_path, candidates=False)
    _write(parent / "orchestration/pipeline.json", {"stages": [
        {"stage_key": "promotion_qualification", "child_run_id": "qualification"},
    ]})
    assert load_end_to_end_comparison(tmp_path, "ete").rejection_analysis is None
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": [], "decisions": [],
    })
    analysis = load_end_to_end_comparison(tmp_path, "ete").rejection_analysis
    assert analysis.rejected == analysis.one_criterion == analysis.two_criteria == analysis.three_or_more_criteria == 0
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": ["a"], "decisions": [],
    })
    assert load_end_to_end_comparison(tmp_path, "ete").rejection_analysis is None


def test_qualification_rejection_total_excludes_candidates(tmp_path):
    parent = _run(tmp_path, candidates=False)
    _write(parent / "orchestration/pipeline.json", {"stages": [
        {"stage_key": "promotion_qualification", "child_run_id": "qualification"},
    ]})
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": ["a"], "decisions": [
            {"Cible": "a", "Combinaison": "a", "Direction": "Up", "candidate": True, "reasons": []},
            {"Cible": "b", "Combinaison": "b", "Direction": "Up", "candidate": False,
             "reasons": ["Précision 30 % < 40 %"]},
        ],
    })
    analysis = load_end_to_end_comparison(tmp_path, "ete").rejection_analysis
    assert (analysis.rejected, analysis.one_criterion, analysis.single["Précision"]) == (1, 1, 1)


@pytest.mark.parametrize(
    ("category", "metric", "policy_key", "limit", "values", "reason", "labels"),
    [
        ("Précision", "Précision holdout", "promotion_min_holdout_precision", .55,
         [.545, .535, .51, .49], "Précision sous le minimum", ("≤1 pt", "≤2 pts", "≤5 pts")),
        ("AUC", "AUC holdout", "promotion_min_holdout_auc", .60,
         [.595, .585, .56, .54], "AUC sous le minimum", ("≤0,01", "≤0,02", "≤0,05")),
        ("Signaux", "Signaux holdout", "promotion_min_holdout_signals", 10,
         [9, 8, 5, 4], "signaux insuffisants", ("manque de 1", "≤2", "≤5")),
        ("Rendement", "Rendement directionnel moyen", "promotion_min_mean_directional_return", .002,
         [.0015, 0, -.002, -.004], "Rendement insuffisant", ("≤10 pb", "≤25 pb", "≤50 pb")),
        ("Mouvement opposé", "Fréquence mouvement opposé", "promotion_max_opposite_movement_frequency", .30,
         [.305, .315, .34, .36], "Mouvements opposés trop fréquents", ("≤1 pt", "≤2 pts", "≤5 pts")),
    ],
)
def test_single_rejection_proximity_uses_persisted_policy_and_cumulative_bands(
    category, metric, policy_key, limit, values, reason, labels,
):
    decisions = [
        {"Cible": str(index), "Combinaison": str(index), "Direction": "Up",
         "candidate": False, "reasons": [reason], metric: value}
        for index, value in enumerate(values)
    ]
    # Multi-criterion rejection is excluded from every « seul » proximity band.
    decisions.append({"Cible": "multi", "Combinaison": "multi", "Direction": "Up",
                      "candidate": False, "reasons": [reason, "Seuil calibré absent"], metric: values[0]})
    analysis = _qualification_rejection_analysis({
        "candidate_sets": [], "policy_parameters": {policy_key: limit}, "decisions": decisions,
    })
    assert analysis.single[category] == 4
    assert analysis.proximity[category] == RejectionProximity(labels, (1, 2, 3), 0)
    assert analysis.rejected == 5 and analysis.one_criterion == 4 and analysis.two_criteria == 1


def test_single_rejection_missing_value_or_policy_is_not_counted_as_near_threshold():
    decision = {"Cible": "a", "Combinaison": "a", "Direction": "Up", "candidate": False,
                "reasons": ["Précision holdout absente"], "Précision holdout": None}
    with_policy = _qualification_rejection_analysis({
        "candidate_sets": [], "policy_parameters": {"promotion_min_holdout_precision": .55},
        "decisions": [decision],
    })
    assert with_policy.single["Précision"] == 1
    assert with_policy.proximity["Précision"].counts == (0, 0, 0)
    assert with_policy.proximity["Précision"].unavailable == 1
    without_policy = _qualification_rejection_analysis({"candidate_sets": [], "decisions": [decision]})
    assert "Précision" not in without_policy.proximity


def test_same_precision_uses_each_qualifications_own_persisted_threshold():
    decisions = [{"Cible": "a", "Combinaison": "a", "Direction": "Up", "candidate": False,
                  "reasons": ["Précision insuffisante"], "Précision holdout": .49}]
    near = _qualification_rejection_analysis({
        "candidate_sets": [], "policy_parameters": {"promotion_min_holdout_precision": .50},
        "decisions": decisions,
    })
    far = _qualification_rejection_analysis({
        "candidate_sets": [], "policy_parameters": {"promotion_min_holdout_precision": .55},
        "decisions": decisions,
    })
    assert near.proximity["Précision"].counts == (1, 1, 1)
    assert far.proximity["Précision"].counts == (0, 0, 0)


def test_non_numeric_threshold_rejection_keeps_total_without_proximity():
    analysis = _qualification_rejection_analysis({
        "candidate_sets": [], "policy_parameters": {"promotion_min_holdout_auc": .6},
        "decisions": [{"Cible": "a", "Combinaison": "a", "Direction": "Up",
                       "candidate": False, "reasons": ["Seuil Up requis absent"],
                       "Seuil calibré": None}],
    })
    assert analysis.single["Seuil"] == 1
    assert "Seuil" not in analysis.proximity


def test_temporal_funnel_uses_forced_qualification_not_temporal_child(tmp_path):
    parent = _run(tmp_path, candidates=True)
    _write(parent / "orchestration/pipeline.json", {"stages": [
        {"stage_key": "walk_forward", "child_run_id": "wf"},
        {"stage_key": "threshold_calibration", "child_run_id": "threshold"},
        {"stage_key": "promotion_qualification", "child_run_id": "qualification"},
        {"stage_key": "temporal_validation_end_to_end", "child_run_id": "temporal"},
        {"stage_key": "forced_candidate_validation_end_to_end", "child_run_id": "forced"},
    ]})
    _write(tmp_path / "runs/qualification/results/qualification.json", {
        "candidate_sets": ["a", "b"], "decisions": []})
    _write(tmp_path / "runs/forced/orchestration/pipeline.json", {"stages": [
        {"stage_key": "promotion_qualification", "child_run_id": "forced_qualification"}]})
    _write(tmp_path / "runs/forced_qualification/results/qualification.json", {
        "candidate_sets": ["a"], "decisions": [
            {"Cible": "a", "Combinaison": "a", "Direction": "Up", "candidate": True, "reasons": []},
            {"Cible": "b", "Combinaison": "b", "Direction": "Up", "candidate": False,
             "reasons": ["AUC 0,50 < 0,60"]},
        ]})
    _write(parent / "results/temporal_validation_comparison.json", {"final_status": "failed"})

    item = load_end_to_end_comparison(tmp_path, "ete")

    assert item.qualification_passed == 2
    assert item.temporal_entering == 2 and item.temporal_passed == 1
    assert item.final_candidates == 1 and item.temporal_status == "failed"
    assert item.rejection_qualification_source == "Forcée"
    assert item.rejection_analysis.rejected == 1
    assert item.rejection_analysis.single == {"AUC": 1}


def test_scientific_comparability_ignores_workers_and_batch_sizes(tmp_path):
    parent = _run(tmp_path, candidates=False)
    settings = {"permutation_depth": 2, "qualification_min_median_auc": .55,
                "promotion_min_holdout_auc": .6, "threshold_calibration_min_signals_per_window": 5,
                "combination_workers": 2, "walk_forward_batch_size": 25,
                "predictor_prefilter_batch_size": 25}
    config = {"primary_universe_id": "universe", "rstock_config": settings}
    _write(parent / "config.json", config)
    first = load_end_to_end_comparison(tmp_path, "ete").scientific_profile
    config["rstock_config"] = {**settings, "combination_workers": 4,
                                "walk_forward_batch_size": 50, "predictor_prefilter_batch_size": 50}
    _write(parent / "config.json", config)
    second = load_end_to_end_comparison(tmp_path, "ete").scientific_profile
    assert first == second
    config["rstock_config"]["promotion_min_holdout_auc"] = .7
    _write(parent / "config.json", config)
    third = load_end_to_end_comparison(tmp_path, "ete").scientific_profile
    assert third["Qualification"] != first["Qualification"]


def test_comparison_ui_keeps_mixed_wf_contract_separate_from_holdout_population(tmp_path, monkeypatch):
    from rstock.application import streamlit_app

    _run(tmp_path, candidates=False)
    base = load_end_to_end_comparison(tmp_path, "ete")
    first = replace(base, run_id="pit-old", wf_final_holdout_evaluated=True,
                    confirmed=5, holdout_population={"count": 2, "auc_median": .55,
                    "precision_median": .4, "return_mean": .01, "return_median": .01,
                    "return_models": 2, "signals_median": 10},
                    qualification_passed=0, final_candidates=0,
                    rejection_qualification_source="Normale",
                    rejection_analysis=QualificationRejectionAnalysis(
                        2, 1, 1, 0, {"Précision": 1}, {"Précision": 2, "AUC": 1},
                        {"Précision": RejectionProximity(("≤1 pt", "≤2 pts", "≤5 pts"), (0, 1, 1), 0)}))
    second = replace(first, run_id="pit-new", wf_final_holdout_evaluated=False, confirmed=None,
                     rejection_qualification_source="Forcée",
                     rejection_analysis=QualificationRejectionAnalysis(0, 0, 0, 0, {}, {}))
    analyses = {item.run_id: item for item in (first, second)}
    frames = []
    sections = []
    warnings = []
    fake_st = SimpleNamespace(
        session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        subheader=sections.append, warning=warnings.append, success=lambda _: None,
        dataframe=lambda frame, **_: frames.append(frame),
        expander=lambda _: nullcontext(), caption=lambda _: None,
        selectbox=lambda _label, choices, **_: choices[0], button=lambda *_args, **_kwargs: False,
    )
    monkeypatch.setattr(streamlit_app, "st", fake_st)
    monkeypatch.setattr(streamlit_app, "load_end_to_end_comparison", lambda _root, run_id: analyses[run_id])
    monkeypatch.setattr(streamlit_app, "render_dataframe", fake_st.dataframe)

    streamlit_app._render_end_to_end_comparison(["pit-old", "pit-new"])

    assert sections == ["Comparabilité", "Funnel scientifique", "Analyse des rejets de qualification",
                        "Holdout E2E — population évaluable", "Candidats finaux"]
    assert any("ne sont pas comparables" in warning for warning in warnings)
    wf_diagnostic = next(frame for frame in frames if "Confirmées holdout WF" in frame.columns)
    assert wf_diagnostic["Confirmées holdout WF"].tolist() == ["5", "Non calculé"]
    population = next(frame for frame in frames if "Rendement directionnel moyen" in frame.columns)
    assert population["Up évaluables"].tolist() == ["2", "2"]
    final = next(frame for frame in frames if "Candidate yield / WF évaluées" in frame.columns)
    assert final["Candidats"].tolist() == ["0", "0"]
    rejection = next(frame for frame in frames if "Mesure" in frame.columns)
    assert rejection.set_index("Mesure").loc["Qualification source"].tolist() == ["Normale", "Forcée"]
    assert rejection.set_index("Mesure").loc["Combinaisons rejetées"].tolist() == ["2", "0"]
    assert rejection.set_index("Mesure").loc["Précision seule"].tolist() == [
        "1 (≤1 pt: 0 · ≤2 pts: 1 · ≤5 pts: 1)", "0",
    ]
    assert rejection.set_index("Mesure").loc["Précision impliquée"].tolist() == ["2", "0"]
