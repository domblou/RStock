from datetime import datetime, timedelta, timezone
from types import SimpleNamespace

import pandas as pd

from rstock.application.history_ui import (
    EXPERIMENT_JOB_TYPES,
    PRODUCTION_JOB_TYPES,
    already_promoted,
    filter_runs,
    history_row,
    paginate_runs,
    qualified_combinations_table,
)


NOW = datetime(2026, 9, 14, 12, tzinfo=timezone.utc)


def _run(identifier, job_type, status="completed", days=0, duration=12):
    return {
        "run_id": identifier,
        "job_type": job_type,
        "status": status,
        "created_at": (NOW - timedelta(days=days)).isoformat(),
        "duration_seconds": duration,
    }


def _detail(*, model_id=None, symbols=("AAA", "BBB"), summary=None):
    return {
        "configuration": {
            "model_id": model_id,
            "symbols": list(symbols),
            "rstock_config": {"permutation_depth": 2},
        },
        "summary": summary or {},
    }


def test_filters_cover_type_status_period_and_model():
    runs = [
        _run("wf-today", "walk_forward"),
        _run("train-week", "production_training", days=3),
        _run("prediction-old", "daily_prediction", status="failed", days=31),
    ]
    details = {
        "wf-today": _detail(),
        "train-week": _detail(model_id="model-1"),
        "prediction-old": _detail(model_id="model-2"),
    }

    today = filter_runs(
        runs, allowed_types=EXPERIMENT_JOB_TYPES, period="Aujourd’hui", now=NOW
    )
    model = filter_runs(
        runs,
        allowed_types=PRODUCTION_JOB_TYPES,
        status="completed",
        period="7 jours",
        model_id="model-1",
        detail_loader=lambda identifier: details[identifier],
        now=NOW,
    )

    assert [run["run_id"] for run in today] == ["wf-today"]
    assert [run["run_id"] for run in model] == ["train-week"]


def test_storage_filter_defaults_to_complete_and_can_show_summaries_only():
    runs = [
        _run("complete", "walk_forward"),
        _run("summary-only", "walk_forward"),
    ]
    details = {
        "complete": _detail(),
        "summary-only": {**_detail(), "storage": {"state": "purged"}},
    }

    complete = filter_runs(
        runs,
        allowed_types=EXPERIMENT_JOB_TYPES,
        detail_loader=lambda identifier: details[identifier],
    )
    summaries = filter_runs(
        runs,
        allowed_types=EXPERIMENT_JOB_TYPES,
        storage="Résumé seulement",
        detail_loader=lambda identifier: details[identifier],
    )

    assert [run["run_id"] for run in complete] == ["complete"]
    assert [run["run_id"] for run in summaries] == ["summary-only"]


def test_today_uses_the_local_calendar_day_for_utc_persisted_runs():
    eastern = timezone(timedelta(hours=-4))
    local_now = datetime(2026, 9, 12, 23, 41, tzinfo=eastern)
    runs = [{
        "run_id": "late-local-run",
        "job_type": "walk_forward",
        "status": "completed",
        # 23:20 on September 12 in Eastern time, but September 13 in UTC.
        "created_at": "2026-09-13T03:20:09+00:00",
    }]

    filtered = filter_runs(
        runs, allowed_types=EXPERIMENT_JOB_TYPES, period="Aujourd’hui", now=local_now
    )

    assert [run["run_id"] for run in filtered] == ["late-local-run"]


def test_today_excludes_utc_timestamp_that_is_still_previous_local_day():
    eastern = timezone(timedelta(hours=-4))
    local_now = datetime(2026, 9, 12, 0, 30, tzinfo=eastern)
    runs = [
        {
            "run_id": "previous-local-day",
            "job_type": "walk_forward",
            "status": "completed",
            # 23:00 on September 11 in Eastern time.
            "created_at": "2026-09-12T03:00:00+00:00",
        },
        {
            "run_id": "current-local-day",
            "job_type": "walk_forward",
            "status": "completed",
            # 00:30 on September 12 in Eastern time.
            "created_at": "2026-09-12T04:30:00+00:00",
        },
    ]

    filtered = filter_runs(
        runs, allowed_types=EXPERIMENT_JOB_TYPES, period="Aujourd’hui", now=local_now
    )

    assert [run["run_id"] for run in filtered] == ["current-local-day"]


def test_history_rows_display_run_id_and_direct_walk_forward_source():
    normal = history_row(
        _run("opaque-guid", "market_update"),
        _detail(summary={"requested_symbols": ["AAA", "BBB", "CCC"], "updated_symbols": ["AAA", "BBB", "CCC"]}),
        {},
    )
    duplicated_detail = _detail()
    duplicated_detail["configuration"]["source_walk_forward_run"] = "parent-run"
    duplicated = history_row(_run("child-run", "walk_forward"), duplicated_detail, {})

    assert normal.context == "3 symboles"
    assert normal.summary == "3 symboles mis à jour"
    assert normal.display()["Run ID"] == "opaque-guid"
    assert normal.display()["Run source"] == "—"
    assert duplicated.display()["Run ID"] == "child-run"
    assert duplicated.display()["Run source"] == "parent-run"
    assert set(normal.display()) == {
        "Run ID",
        "Run source",
        "Date / heure",
        "Type",
        "Contexte",
        "Statut",
        "Stockage",
        "Durée",
        "Résumé",
    }


def test_threshold_parameter_calibration_appears_with_direct_parent_and_winner():
    detail = _detail(summary={
        "selected_configuration": {"configuration": "candidate_03"}
    })
    detail["configuration"]["source_walk_forward_run"] = "wf-parent"
    detail["configuration"]["source_xgboost_calibration_run"] = "xgb-parent"

    row = history_row(
        _run("parameter-run", "threshold_parameter_calibration"), detail, {}
    )

    assert "threshold_parameter_calibration" in EXPERIMENT_JOB_TYPES
    assert row.display()["Type"] == "Calibration des paramètres de seuils"
    assert row.display()["Run source"] == "xgb-parent"
    assert row.summary == "Configuration gagnante : candidate_03"


def test_threshold_parameter_calibration_summary_matches_nonterminal_status():
    detail = _detail()

    running = history_row(
        _run("parameter-running", "threshold_parameter_calibration", status="running"),
        detail,
        {},
    )
    pending = history_row(
        _run("parameter-pending", "threshold_parameter_calibration", status="pending"),
        detail,
        {},
    )

    assert running.summary == "Calibration des paramètres en cours"
    assert pending.summary == "Calibration des paramètres en attente"


def test_threshold_parameter_calibration_completed_legacy_run_keeps_success_fallback():
    row = history_row(
        _run("parameter-completed", "threshold_parameter_calibration"), _detail(), {}
    )

    assert row.summary == "Calibration des paramètres terminée"


def test_history_uses_persisted_run_description_and_keeps_legacy_summary_fallback():
    described = _detail(summary={"outcome": "completed"})
    described["configuration"]["run_description"] = "profondeur 2"

    calibration = history_row(
        _run("calibration", "threshold_calibration"), described, {}
    )
    legacy = history_row(
        _run("legacy", "threshold_calibration"), _detail(), {}
    )

    assert calibration.summary == "Calibration des seuils — profondeur 2"
    assert legacy.summary == "Calibration terminée"


def test_threshold_calibration_keeps_the_walk_forward_context_after_duplication():
    symbols = tuple(f"SYM{index:03d}" for index in range(115))
    walk_forward = _detail(symbols=symbols)
    calibration = _detail(symbols=symbols)
    calibration["configuration"]["source_walk_forward_run"] = "walk-forward-source"
    calibration["configuration"]["run_description"] = "Rejeu historique"

    source_row = history_row(_run("walk-forward-source", "walk_forward"), walk_forward, {})
    calibration_row = history_row(
        _run("threshold-calibration", "threshold_calibration"), calibration, {}
    )

    assert source_row.context == "115 symboles · profondeur 2"
    assert calibration_row.context == source_row.context
    assert calibration_row.summary == "Calibration des seuils — Rejeu historique"


def test_legacy_calibration_without_inherited_depth_has_a_safe_context_fallback():
    detail = _detail(symbols=("AAA", "BBB"))
    detail["configuration"]["rstock_config"] = {}

    row = history_row(_run("legacy-calibration", "threshold_calibration"), detail, {})

    assert row.context == "2 symboles · profondeur —"


def test_production_rows_have_human_context_and_summary():
    models = {"model-1": "DIS ← PFE + WMT"}
    training = history_row(
        _run("training", "production_training"),
        _detail(model_id="model-1", summary={"model_id": "model-1"}),
        models,
    )
    screening = history_row(
        _run("screening", "daily_screening"),
        _detail(summary={"predictions": 2, "categories": {"no_signal": 2}}),
        models,
    )

    assert training.context == "DIS ← PFE + WMT"
    assert training.summary == "1 modèle entraîné"
    assert screening.context == "2 modèles actifs"
    assert screening.summary == "0 signaux · 2 sans signal"


def test_pagination_bounds_visible_runs():
    runs = [_run(f"run-{index}", "walk_forward") for index in range(60)]

    page, pages = paginate_runs(runs, page=1, page_size=25)

    assert pages == 3
    assert len(page) == 25
    assert page[0]["run_id"] == "run-25"


def test_qualified_table_combines_development_holdout_and_is_sortable():
    qualification = pd.DataFrame([
        {"Set": "DIS<-PFE+WMT", "Observation": "DIS", "Predictors": '["PFE","WMT"]', "Eligible": True, "ROCAUCMedian": 0.61, "IneligibilityReasons": "[]"},
        {"Set": "AAA<-BBB", "Observation": "AAA", "Predictors": '["BBB"]', "Eligible": False, "ROCAUCMedian": 0.9, "IneligibilityReasons": '["median_auc"]'},
    ])
    holdout = pd.DataFrame([{"Set": "DIS<-PFE+WMT", "FinalUpROCAUC": 0.58}])

    table = qualified_combinations_table(qualification, holdout)

    assert table.to_dict("records") == [{
        "Combinaison": "DIS<-PFE+WMT", "Cible": "DIS", "Predictors": "PFE + WMT",
        "AUC dev médiane": 0.61, "AUC holdout": 0.58,
        "Stabilité / qualification": "Qualifiée",
    }]


def test_qualified_table_prefers_persisted_final_score_when_available():
    qualification = pd.DataFrame([
        {"Set": "A<-B", "Observation": "A", "Predictors": '["B"]', "Eligible": True, "ROCAUCMedian": 0.80},
        {"Set": "C<-D", "Observation": "C", "Predictors": '["D"]', "Eligible": True, "ROCAUCMedian": 0.70},
    ])
    scores = pd.DataFrame([
        {"Set": "A<-B", "model_selection_score": 65.0, "model_selection_rank": 2},
        {"Set": "C<-D", "model_selection_score": 82.0, "model_selection_rank": 1},
    ])

    table = qualified_combinations_table(qualification, selection_results=scores)

    assert table["Combinaison"].tolist() == ["C<-D", "A<-B"]
    assert table[["Score", "Rang"]].to_dict("records") == [
        {"Score": 82.0, "Rang": 1}, {"Score": 65.0, "Rang": 2}
    ]


def test_threshold_calibration_history_summary_explains_missing_frozen_threshold():
    row = history_row(
        _run("threshold-run", "threshold_calibration"),
        {"configuration": {}, "summary": {
            "outcome": "completed_no_eligible_threshold",
            "missing_frozen_thresholds": [{"direction": "Up"}],
        }},
        {},
    )

    assert row.summary == "Holdout ignoré — 1 seuil gelé manquant (Up)"


def test_threshold_calibration_history_summary_reports_partial_holdout_by_direction():
    row = history_row(
        _run("threshold-run", "threshold_calibration"),
        {"configuration": {}, "summary": {
            "outcome": "completed_partial_holdout",
            "holdout_combination_counts": {
                "Up": {
                    "total_combinations": 300,
                    "evaluated_combinations": 267,
                    "skipped_combinations": 33,
                    "exclusion_reasons": {"no_eligible_threshold": 33},
                },
                "Down": {
                    "total_combinations": 300,
                    "evaluated_combinations": 300,
                    "skipped_combinations": 0,
                    "exclusion_reasons": {},
                },
            },
        }},
        {},
    )

    assert row.summary == (
        "Holdout partiel — Up : 267/300 combinaisons évaluées · "
        "33 ignorées sans seuil admissible · "
        "Down : 300/300 combinaisons évaluées"
    )


def test_threshold_calibration_history_summary_reports_when_no_pair_is_eligible():
    row = history_row(
        _run("threshold-run", "threshold_calibration"),
        {"configuration": {}, "summary": {
            "outcome": "completed_no_eligible_threshold",
            "holdout_combination_counts": {
                direction: {
                    "total_combinations": 2,
                    "evaluated_combinations": 0,
                    "skipped_combinations": 2,
                    "exclusion_reasons": {"no_eligible_threshold": 2},
                }
                for direction in ("Up", "Down")
            },
        }},
        {},
    )

    assert row.summary == (
        "Aucune combinaison admissible (Up, Down) · holdout ignoré"
    )


def test_existing_promotion_is_detected_without_changing_registry_logic():
    model = SimpleNamespace(
        source_walk_forward_run="wf-run", target="DIS", predictors=("PFE", "WMT"), status="candidate"
    )

    existing = already_promoted(
        walk_forward_run="wf-run", set_name="DIS<-PFE+WMT", models=[model]
    )

    assert existing is model
