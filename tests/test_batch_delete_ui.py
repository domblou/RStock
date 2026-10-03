from contextlib import nullcontext
from types import SimpleNamespace

from rstock.application import streamlit_app
from rstock.application.batch_delete import preview_batch_delete
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.services import ExperimentService
from rstock.config import DEFAULT_CONFIG


def test_batch_delete_confirmation_is_explicit_and_refreshes_once(tmp_path, monkeypatch):
    from dataclasses import replace

    repository = RunRepository(tmp_path / "runs")
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"), combinations_per_target=1,
    )
    run_id = repository.create(spec)
    repository.transition(run_id, JobStatus.FAILED)
    service = ExperimentService(RunService(repository))
    review = preview_batch_delete(service, [run_id])
    events: list[str] = []
    state: dict[str, object] = {"history-pending-batch-delete": review}

    def button(label: str, **_kwargs: object) -> bool:
        events.append(label)
        return label == "Confirmer la suppression définitive groupée"

    fake_st = SimpleNamespace(
        session_state=state,
        container=lambda **_kwargs: nullcontext(),
        warning=lambda message: events.append(message),
        caption=lambda message: events.append(message),
        info=lambda message: events.append(message),
        error=lambda message: events.append(message),
        columns=lambda _widths: (SimpleNamespace(button=button), SimpleNamespace(button=button), None),
        rerun=lambda: events.append("rerun"),
    )
    monkeypatch.setattr(streamlit_app, "st", fake_st)

    streamlit_app._render_batch_delete_confirmation(service, review, key_prefix="history")

    assert not repository.run_directory(run_id).exists()
    assert events.count("rerun") == 1
    assert any("irréversible" in event and "1 runs à supprimer" in event for event in events)
    assert any("Par type :" in event for event in events)
    assert "history-pending-batch-delete" not in state
    assert state["history-batch-delete-result"].succeeded == (run_id,)


def test_individual_delete_confirmation_removes_run_and_refreshes_once(tmp_path, monkeypatch):
    from dataclasses import replace

    repository = RunRepository(tmp_path / "runs")
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"), combinations_per_target=1,
    )
    run_id = repository.create(spec)
    repository.transition(run_id, JobStatus.CANCELLED)
    service = ExperimentService(RunService(repository))
    plan = service.delete_preview(run_id)
    state: dict[str, object] = {"pending-run-delete": plan}
    events: list[str] = []

    def button(label: str, **_kwargs: object) -> bool:
        events.append(label)
        return label == "Confirmer la suppression définitive"

    fake_st = SimpleNamespace(
        session_state=state,
        container=lambda **_kwargs: nullcontext(),
        warning=lambda message: events.append(message),
        caption=lambda message: events.append(message),
        error=lambda message: events.append(message),
        columns=lambda _widths: (SimpleNamespace(button=button), SimpleNamespace(button=button), None),
        rerun=lambda: events.append("rerun"),
    )
    monkeypatch.setattr(streamlit_app, "st", fake_st)
    streamlit_app._render_run_delete_confirmation(service, plan)

    assert not repository.run_directory(run_id).exists()
    assert events.count("rerun") == 1
    assert "pending-run-delete" not in state
