from dataclasses import replace
import inspect
from pathlib import Path

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.forward_simulation import run_forward_simulation
from rstock.calendars import forward_market_sessions, resolve_market_session_on_or_before
from rstock.config import DEFAULT_CONFIG


def _spec(**values):
    base = ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=replace(DEFAULT_CONFIG, project_root=Path(".")),
        symbols=("AAA", "BBB"),
        target_symbols=("AAA",),
        context_symbols=("BBB",),
        predictor_symbols=("AAA", "BBB"),
    )
    return replace(base, **values)


def test_historical_cutoff_provenance_round_trips_without_changing_legacy_fields():
    spec = _spec(
        historical_data_cutoff="2026-03-24",
        requested_historical_cutoff="2026-03-22",
        resolved_market_session_cutoff="2026-03-24",
        forward_simulation_enabled=True,
        forward_simulation_mode="63_sessions",
        forward_simulation_start_date="2026-03-25",
        forward_simulation_end_date="2026-06-23",
    )
    restored = ExperimentSpec.from_dict(spec.to_dict())
    assert restored.requested_historical_cutoff == "2026-03-22"
    assert restored.resolved_market_session_cutoff == "2026-03-24"
    assert restored.historical_data_cutoff == "2026-03-24"
    assert restored.forward_simulation_start_date == "2026-03-25"


def test_historical_snapshot_without_forward_fields_keeps_legacy_semantics():
    values = _spec(historical_data_cutoff="2026-03-24").to_dict()
    for field in (
        "requested_historical_cutoff",
        "resolved_market_session_cutoff",
        "forward_simulation_enabled",
        "forward_simulation_mode",
        "forward_simulation_start_date",
        "forward_simulation_end_date",
        "source_forward_model_snapshot_sha256",
    ):
        values.pop(field)
    restored = ExperimentSpec.from_dict(values)
    assert restored.historical_data_cutoff == "2026-03-24"
    assert restored.forward_simulation_enabled is False
    assert restored.resolved_market_session_cutoff is None


def test_forward_sessions_start_strictly_after_resolved_cutoff():
    cutoff = resolve_market_session_on_or_before("2026-03-22", "XNYS")
    sessions = forward_market_sessions(cutoff, "XNYS", 63)
    assert len(sessions) == 63
    assert sessions.min() > cutoff


def test_forward_spec_requires_source_and_resolved_dates():
    try:
        _spec(job_type=JobType.FORWARD_SIMULATION)
    except ValueError as error:
        assert "forward_simulation" in str(error)
    else:  # pragma: no cover
        raise AssertionError("A forward source and dates are mandatory")


def test_forward_execution_contains_no_booster_fit_path():
    assert "fit_booster(" not in inspect.getsource(run_forward_simulation)
