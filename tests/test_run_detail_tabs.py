from __future__ import annotations

from dataclasses import dataclass
from contextlib import nullcontext
from pathlib import Path
from types import SimpleNamespace

import pytest

from rstock.application import streamlit_app
from rstock.application.domain import JobType
from rstock.application.history_ui import EXPERIMENT_JOB_TYPES
from rstock.application.run_detail_tabs import (
    PIPELINE_CHILD_TABS,
    PIPELINE_STAGE_LABEL_COLUMN,
    batch_status_counts,
    pipeline_stage_rows,
    render_lazy_tabs,
    tabs_for_job,
    walk_forward_batch_rows,
)


def _keys(job_type: JobType, *, batched: bool = False) -> list[str]:
    return [
        item.key
        for item in tabs_for_job(
            job_type, has_walk_forward_batches=batched
        )
    ]


def test_walk_forward_batches_are_hidden_from_history_and_parent_tab_is_conditional():
    assert JobType.WALK_FORWARD_BATCH.value not in EXPERIMENT_JOB_TYPES
    assert "walk_forward_batches" not in _keys(JobType.WALK_FORWARD)
    assert _keys(JobType.WALK_FORWARD, batched=True) == [
        "summary",
        "resources",
        "analysis",
        "combinations",
        "validation",
        "walk_forward_batches",
        "technical",
    ]


def test_end_to_end_exposes_every_integrated_pipeline_view():
    assert JobType.END_TO_END.value in EXPERIMENT_JOB_TYPES
    assert _keys(JobType.END_TO_END) == [
        "summary",
        "resources",
        "walk_forward",
        "xgboost",
        "threshold_parameters",
        "thresholds",
        "holdout_evaluation",
        "promotion_qualification",
        "temporal_validation",
        "promotion",
        "technical",
    ]


def test_split_jobs_have_standalone_history_and_results_tabs():
    assert JobType.HOLDOUT_EVALUATION.value in EXPERIMENT_JOB_TYPES
    assert _keys(JobType.HOLDOUT_EVALUATION) == [
        "results", "resources", "configuration", "files", "logs",
    ]
    assert JobType.PROMOTION_QUALIFICATION.value in EXPERIMENT_JOB_TYPES
    assert _keys(JobType.PROMOTION_QUALIFICATION) == [
        "results", "configuration", "files", "logs",
    ]
    assert _keys(JobType.FORCED_CANDIDATE_VALIDATION) == [
        "summary", "resources", "walk_forward", "fixed_candidate_evaluation",
        "promotion_qualification", "technical",
    ]


def test_split_stage_row_exposes_run_timestamps_and_inherited_provenance():
    row = pipeline_stage_rows([{
        "stage_key": "holdout_evaluation", "mode": "inherited",
        "source_run_id": "source-holdout", "child_run_id": "source-holdout",
        "status": "completed", "started_at": "2026-09-01T12:00:00+00:00",
        "finished_at": "2026-09-01T12:05:00+00:00",
    }])[0]
    assert row["Run ID enfant"] == "source-holdout"
    assert row["Run source"] == "source-holdout"
    assert row["Début"] == "2026-09-01T12:00:00+00:00"
    assert row["Fin"] == "2026-09-01T12:05:00+00:00"


def test_each_end_to_end_scientific_tab_targets_the_expected_child_stage():
    assert PIPELINE_CHILD_TABS == {
        "child_walk_forward": "walk_forward",
        "child_xgboost": "xgboost_calibration",
        "child_threshold_parameters": "threshold_parameter_calibration",
        "child_thresholds": "threshold_calibration",
        "child_holdout_evaluation": "holdout_evaluation",
        "child_promotion_qualification": "promotion_qualification",
        "child_fixed_candidate_evaluation": "fixed_candidate_evaluation",
    }


def test_pipeline_summary_maps_disabled_failed_and_preserves_completed_stages():
    rows = pipeline_stage_rows(
        [
            {"stage_key": "walk_forward", "status": "completed", "progress": 100.0},
            {"stage_key": "xgboost_calibration", "status": "completed", "progress": 100.0},
            {
                "stage_key": "threshold_parameter_calibration",
                "status": "failed",
                "progress": 42.0,
                "error": "boom",
            },
            {"stage_key": "threshold_calibration", "status": "pending"},
            {"stage_key": "promotion", "status": "not_requested"},
        ]
    )

    assert [row["Statut"] for row in rows] == [
        "completed",
        "completed",
        "failed",
        "pending",
        "disabled",
    ]
    assert rows[2]["Erreur"] == "boom"
    assert PIPELINE_STAGE_LABEL_COLUMN == "Étape"
    assert rows[2][PIPELINE_STAGE_LABEL_COLUMN] == "Paramètres seuils"


@dataclass
class _FakeTab:
    open: bool

    def __enter__(self):
        return self

    def __exit__(self, *args):
        return False


class _FakeStreamlit:
    def __init__(self, active_index: int):
        self.active_index = active_index
        self.labels = []
        self.options = {}

    def tabs(self, labels, **options):
        self.labels = list(labels)
        self.options = options
        return [
            _FakeTab(index == self.active_index) for index, _ in enumerate(labels)
        ]


def test_lazy_tabs_executes_only_the_visible_renderer():
    ui = _FakeStreamlit(active_index=3)
    called: list[str] = []
    tabs = tabs_for_job(JobType.END_TO_END)
    renderers = {
        item.renderer_key: (
            lambda renderer_key=item.renderer_key: called.append(renderer_key)
        )
        for item in tabs
    }

    selected = render_lazy_tabs(ui, tabs, renderers, key="end-to-end-test")

    assert selected == "xgboost"
    assert called == ["child_xgboost"]
    assert ui.options == {"key": "end-to-end-test", "on_change": "rerun"}


def test_summary_open_does_not_run_scientific_renderers():
    ui = _FakeStreamlit(active_index=0)
    called: list[str] = []
    tabs = tabs_for_job(JobType.END_TO_END)
    renderers = {
        item.renderer_key: (
            lambda renderer_key=item.renderer_key: called.append(renderer_key)
        )
        for item in tabs
    }

    render_lazy_tabs(ui, tabs, renderers, key="summary-only")

    assert called == ["summary"]


def test_individual_scientific_tabs_keep_their_existing_layout():
    expected = ["results", "resources", "configuration", "files", "logs"]
    assert _keys(JobType.XGBOOST_CALIBRATION) == expected
    assert _keys(JobType.THRESHOLD_PARAMETER_CALIBRATION) == expected
    assert _keys(JobType.THRESHOLD_CALIBRATION) == expected


def test_batch_view_model_keeps_runtime_state_separate_from_manifest_ranges():
    batches = [
            {
                "batch_id": "000000",
                "range_start": 0,
                "range_stop": 10,
                "combination_count": 10,
                "child_run_id": "child-0",
                "status": "completed",
                "progress": 100.0,
            },
            {
                "batch_id": "000001",
                "range_start": 10,
                "range_stop": 15,
                "combination_count": 5,
                "child_run_id": "child-1",
                "status": "failed",
                "error": "worker failed",
            },
        ]
    rows = walk_forward_batch_rows(batches)

    assert rows[1]["Range start"] == 10
    assert rows[1]["Run ID"] == "child-1"
    assert rows[1]["Erreur"] == "worker failed"
    assert batch_status_counts(batches) == {
        "completed": 1,
        "running": 0,
        "failed": 1,
        "pending": 0,
    }


def test_pipeline_children_and_individual_runs_use_the_same_tab_registry():
    xgboost = tabs_for_job(JobType.XGBOOST_CALIBRATION)
    assert [item.renderer_key for item in xgboost] == [
        "results",
        "resources",
        "configuration",
        "files",
        "logs",
    ]


@pytest.mark.parametrize("renderer_key", tuple(PIPELINE_CHILD_TABS))
def test_pipeline_child_results_precede_technical_block(monkeypatch, renderer_key):
    events = []
    stage_key = PIPELINE_CHILD_TABS[renderer_key]
    stage = {"child_run_id": "child", "status": "completed", "artifact_digests": {}}
    detail = {"status": {"status": "completed"}, "metadata": {"run_role": "pipeline_stage"}}
    fake_st = SimpleNamespace(
        caption=lambda *_: events.append("caption"),
        expander=lambda *_: (events.append("technical") or nullcontext()),
        json=lambda *_: events.append("json"),
        button=lambda *_args, **_kwargs: (events.append("button") or False),
    )
    monkeypatch.setattr(streamlit_app, "st", fake_st)
    monkeypatch.setattr(streamlit_app, "pipeline_stage_by_key", lambda *_: stage)
    monkeypatch.setattr(
        streamlit_app, "_render_job_detail_tabs",
        lambda *_args, **_kwargs: events.append("results"),
    )

    streamlit_app._render_pipeline_child(
        SimpleNamespace(run=lambda _: detail), "parent", {}, renderer_key
    )

    assert events.index("results") < events.index("technical")
    assert events.index("results") < events.index("json")
    assert events.index("results") < events.index("button")


@pytest.mark.parametrize("inherited", (False, True))
def test_pipeline_child_standalone_navigation_remains_available_at_bottom(monkeypatch, inherited):
    events = []
    stage = {"child_run_id": "child", "status": "completed", "artifact_digests": {}}
    if inherited:
        stage.update(mode="inherited", source_run_id="source")
    detail = {"status": {"status": "completed"}, "metadata": {}}
    monkeypatch.setattr(streamlit_app, "st", SimpleNamespace(
        caption=lambda *_: None,
        expander=lambda *_: nullcontext(),
        json=lambda *_: None,
        button=lambda *_args, **_kwargs: (events.append("button") or True),
    ))
    monkeypatch.setattr(streamlit_app, "pipeline_stage_by_key", lambda *_: stage)
    monkeypatch.setattr(streamlit_app, "_render_job_detail_tabs", lambda *_args, **_kwargs: events.append("results"))
    monkeypatch.setattr(streamlit_app, "_history_navigation", lambda *args: events.append(args))

    streamlit_app._render_pipeline_child(
        SimpleNamespace(run=lambda _: detail), "parent", {}, "child_walk_forward"
    )

    assert events == (
        ["results", "button", ("detail", ["source"]), "button", ("detail", ["source"])]
        if inherited else ["results", "button", ("detail", ["child"])]
    )


def test_temporal_and_split_result_technical_blocks_follow_scientific_content():
    source = Path(streamlit_app.__file__).read_text(encoding="utf-8")
    temporal = source.split("def _render_temporal_validation(", 1)[1].split("def _read_light_json(", 1)[0]
    assert temporal.index("_render_pipeline_summary(child_run_id, child_detail)") < temporal.rindex(
        'with st.expander("Provenance technique et navigation")'
    )
    standard = source.split("def _render_standard_results(", 1)[1].split("def _render_standard_job_tabs(", 1)[0]
    holdout = standard.split("elif job_type is JobType.HOLDOUT_EVALUATION", 1)[1].split(
        "elif job_type is JobType.PROMOTION_QUALIFICATION", 1
    )[0]
    qualification = standard.split("elif job_type is JobType.PROMOTION_QUALIFICATION", 1)[1].split(
        "elif (", 1
    )[0]
    assert holdout.index("st.dataframe(") < holdout.index("st.json(configuration)")
    assert qualification.index("_render_qualification_decision_grid(") < qualification.index("st.json(")
