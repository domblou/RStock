import json
import sys
import time
from types import SimpleNamespace

import pytest

from rstock.application import resource_telemetry, streamlit_app
from rstock.application.runner import ProgressReporter
from rstock.telemetry import ProcessSample, process_rss_bytes


@pytest.mark.skipif(sys.platform != "win32", reason="Windows process counter")
def test_windows_rss_counter_uses_a_valid_process_handle():
    assert isinstance(process_rss_bytes(), int)
    assert process_rss_bytes() > 0


def test_resource_recorder_keeps_simultaneous_peak_and_previous_attempt(tmp_path, monkeypatch):
    assert resource_telemetry.SAMPLE_SECONDS == 5.0
    readings = iter([(100, 50), (200, 50), (100, 200), (100, 50),
                     (100, 50), (100, 50), (100, 50)])
    current = [100, 50]

    def sample(pid):
        if pid == 1:
            current[:] = next(readings, current)
            return ProcessSample(1, 10, time.process_time(), current[0], None)
        return ProcessSample(2, 20, time.process_time(), current[1], None)

    monkeypatch.setattr(resource_telemetry.os, "getpid", lambda: 1)
    monkeypatch.setattr(resource_telemetry, "descendant_pids", lambda _pid: {2})
    monkeypatch.setattr(resource_telemetry, "process_sample", sample)
    run = tmp_path / "run"
    first = resource_telemetry.ResourceRecorder(run, "run", {"combination_workers": 3})
    first.phase_started("metrics")
    first.phase_completed("metrics", {"combinations_processed": 7})
    first.batch_completed("walk_forward", {
        "batch_id": 0, "checkpoint_written": True, "combinations": 7,
        "elapsed_seconds": 1.0,
    }, 7)
    first.close("interrupted")
    document = json.loads((run / "telemetry/resource_summary.json").read_text(encoding="utf-8"))
    attempt = document["attempts"][0]
    assert attempt["rss_peak_sampled_bytes"] == 300
    assert attempt["parent_rss_peak_sampled_bytes"] == 200
    assert attempt["children_rss_peak_sampled_bytes"] == 200
    assert attempt["phase_rows"][0]["completed_items"] == 7
    assert attempt["phase_rows"][0]["duration_seconds"] >= 0
    assert len((run / "telemetry/batches.jsonl").read_text(encoding="utf-8").splitlines()) == 1

    second = resource_telemetry.ResourceRecorder(run, "run", {"combination_workers": 3})
    second.close("completed")
    resumed = json.loads((run / "telemetry/resource_summary.json").read_text(encoding="utf-8"))
    assert [attempt["status"] for attempt in resumed["attempts"]] == ["interrupted", "completed"]
    assert resumed["attempts"][0]["phase_rows"][0]["completed_items"] == 7


def test_resource_resume_reconciles_an_unfinished_attempt(tmp_path, monkeypatch):
    monkeypatch.setattr(resource_telemetry, "descendant_pids", lambda _pid: set())
    root = tmp_path / "run" / "telemetry"
    root.mkdir(parents=True)
    (root / "resource_summary.json").write_text(json.dumps({
        "schema_version": 1, "run_id": "run", "attempts": [{
            "attempt_id": "old", "status": "running", "finished_at": None,
            "phase_rows": [{"name": "walk_forward", "finished_at": None}],
        }],
    }), encoding="utf-8")
    recorder = resource_telemetry.ResourceRecorder(tmp_path / "run", "run", {})
    recorder.close("completed")
    attempts = json.loads((root / "resource_summary.json").read_text(encoding="utf-8"))["attempts"]
    assert [item["status"] for item in attempts] == ["interrupted", "completed"]
    assert attempts[0]["finished_at"] is None
    assert attempts[0]["phase_rows"][0]["finished_at"] is None


def test_phase_duration_uses_its_own_start_after_another_phase_starts(tmp_path, monkeypatch):
    from rstock.application import runner

    class Repository:
        def __init__(self):
            self.progress = None

        def write_json(self, _run_id, _name, value):
            self.progress = value

        def append_log(self, *_args):
            pass

    repository = Repository()
    clock = [0.0]
    monkeypatch.setattr(runner.time, "monotonic", lambda: clock[0])
    reporter = ProgressReporter(repository, "run")
    clock[0] = 10.0
    reporter.phase_started("metrics")
    clock[0] = 100.0
    reporter.phase_started("final_holdout")
    clock[0] = 102.0
    reporter.phase_completed("final_holdout")
    clock[0] = 110.0
    reporter.phase_completed("metrics")
    history = repository.progress["phase_history"]
    metrics = next(row for row in history if row["name"] == "metrics")
    holdout = next(row for row in history if row["name"] == "final_holdout")
    assert metrics["duration_seconds"] > holdout["duration_seconds"]
    assert metrics["duration_seconds"] >= 90


def test_resource_view_keeps_legacy_run_unavailable(tmp_path, monkeypatch):
    messages = []
    monkeypatch.setattr(streamlit_app, "st", SimpleNamespace(
        session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        info=messages.append,
    ))
    streamlit_app._render_run_resources("old-run")
    assert messages == ["Télémétrie indisponible pour ce run"]


def test_resource_view_shows_measured_cpu_phases_and_two_charts(tmp_path, monkeypatch):
    folder = tmp_path / "runs" / "run" / "telemetry"
    folder.mkdir(parents=True)
    (folder / "resource_summary.json").write_text(json.dumps({
        "schema_version": 1, "attempts": [{
            "attempt_id": "one", "status": "completed", "elapsed_seconds": 100,
            "logical_processors": 16, "cpu_mean_cores": 3.1,
            "cpu_max_sampled_cores": 5.0, "cpu_covered_seconds": 95,
            "rss_peak_sampled_bytes": 2**30,
            "max_child_processes_observed": 3,
            "wait_seconds": {"heavy_slot": 2.0},
            "configuration": {"combination_workers": 3, "xgb_nthread": 2},
            "phase_rows": [{
                "name": "metrics", "duration_seconds": 40,
                "cpu_mean_cores": 1.0, "cpu_max_sampled_cores": 2.0,
                "rss_peak_sampled_bytes": 2**29,
                "max_child_processes_observed": 0,
                "completed_items": 20, "item_kind": "combinaisons",
            }],
        }],
    }), encoding="utf-8")
    (folder / "samples.jsonl").write_text(json.dumps({
        "attempt_id": "one", "elapsed_seconds": 5,
        "cpu_cores": 3.1, "total_rss_bytes": 2**30,
    }) + "\n", encoding="utf-8")
    metrics, frames, charts = [], [], []
    column = SimpleNamespace(metric=lambda label, value: metrics.append((label, value)))
    fake = SimpleNamespace(
        session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        caption=lambda *_args: None, subheader=lambda *_args: None,
        columns=lambda count: [column] * count,
        dataframe=lambda frame, **_kwargs: frames.append(frame),
        line_chart=lambda frame, **_kwargs: charts.append(frame),
        column_config=SimpleNamespace(
            TextColumn=lambda **kwargs: kwargs,
            NumberColumn=lambda **kwargs: kwargs,
        ),
    )
    monkeypatch.setattr(streamlit_app, "st", fake)
    streamlit_app._render_run_resources("run")
    assert ("CPU moyen", "3.1 / 16 cœurs — 19 %") in metrics
    assert ("CPU max échantillonné", "5.0 / 16 cœurs — 31 %") in metrics
    assert frames[0].iloc[0]["Durée (s)"] == 40
    assert frames[0].iloc[0]["Débit"] == "0.50 combinaisons/s"
    assert len(charts) == 2
