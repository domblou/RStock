import json

from rstock.application.run_timing import observed_run_timing


def _status(**changes):
    return {"status": "interrupted", "started_at": "2026-10-10T13:26:32+00:00",
            "finished_at": "2026-10-11T02:29:15+00:00", "duration_seconds": 46963,
            **changes}


def test_historical_interruption_uses_last_valid_sample_without_rewriting_status(tmp_path):
    directory = tmp_path / "telemetry"
    directory.mkdir()
    path = directory / "samples.jsonl"
    path.write_text(json.dumps({"at": "2026-10-10T19:46:35+00:00"}) + '\n{"at":')
    status = _status()
    before = status.copy()
    timing = observed_run_timing(tmp_path, status, {"updated_at": "2026-10-10T13:26:33+00:00"})
    assert timing["observed_duration_seconds"] == 22803
    assert timing["detection_delay_seconds"] == 24160
    assert timing["wall_duration_seconds"] == 46963
    assert timing["observation_source"] == "telemetry"
    assert status == before
    from rstock.application.run_detail_tabs import pipeline_stage_rows
    row = pipeline_stage_rows([{"stage_key": "threshold_parameter_calibration",
                                "status": "interrupted", **timing}])[0]
    assert row["Durée (s)"] == 22803
    assert row["Délai détection (s)"] == 24160
    assert row["Fin"] is None
    assert row["Détection interruption"] == status["finished_at"]


def test_old_attempts_and_future_samples_do_not_inflate_current_observation(tmp_path):
    (tmp_path / "telemetry").mkdir()
    (tmp_path / "telemetry/samples.jsonl").write_text('\n'.join(
        json.dumps({"at": stamp}) for stamp in
        ("2026-10-09T19:00:00+00:00", "2026-10-12T19:00:00+00:00")))
    timing = observed_run_timing(tmp_path, _status(), {"updated_at": "2026-10-10T13:27:32+00:00"})
    assert timing["duration_seconds"] == 60
    assert timing["observation_source"] == "progress"


def test_missing_evidence_leaves_observed_duration_unknown(tmp_path):
    timing = observed_run_timing(tmp_path, _status())
    assert timing["duration_seconds"] is None
    assert timing["wall_duration_seconds"] == 46963


def test_completed_duration_keeps_existing_semantics(tmp_path):
    assert observed_run_timing(tmp_path, _status(status="completed"))["duration_seconds"] == 46963
