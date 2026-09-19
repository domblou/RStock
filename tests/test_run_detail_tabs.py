from __future__ import annotations

from dataclasses import dataclass

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
        "walk_forward",
        "xgboost",
        "threshold_parameters",
        "thresholds",
        "temporal_validation",
        "promotion",
        "technical",
    ]


def test_each_end_to_end_scientific_tab_targets_the_expected_child_stage():
    assert PIPELINE_CHILD_TABS == {
        "child_walk_forward": "walk_forward",
        "child_xgboost": "xgboost_calibration",
        "child_threshold_parameters": "threshold_parameter_calibration",
        "child_thresholds": "threshold_calibration",
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
    ui = _FakeStreamlit(active_index=2)
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
    expected = ["results", "configuration", "files", "logs"]
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
        "configuration",
        "files",
        "logs",
    ]
