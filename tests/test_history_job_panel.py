"""Global active-job display; no real services or jobs are started."""
from contextlib import nullcontext
from types import SimpleNamespace

import pytest

from rstock.application import streamlit_app as ui
from rstock.application.domain import JobType


class FakeUI:
    def __init__(self):
        self.titles, self.cards, self.buttons = [], [], []
        self.session_state = {}

    def subheader(self, title): self.titles.append(title)
    def container(self, **kwargs): return nullcontext()
    def columns(self, *args): return [self] * 4
    def markdown(self, value): self.cards.append(value)
    def code(self, *args): pass
    def metric(self, *args): pass
    def caption(self, *args): pass
    def button(self, label, **kwargs):
        self.buttons.append((label, kwargs))
        return False


def test_global_panel_includes_every_active_type_and_excludes_finished(monkeypatch):
    fake = FakeUI()
    monkeypatch.setattr(ui, "st", fake)
    runs = [dict(run_id=f"active-{job.value}", job_type=job.value, status="running") for job in JobType]
    runs += [dict(run_id="pending", job_type="scientific_analysis", status="pending")]
    runs += [dict(run_id=status, job_type="forward_simulation", status=status)
             for status in ("completed", "failed", "cancelled", "interrupted")]
    requested = []
    service = SimpleNamespace(runs=lambda: runs, run=lambda run_id: requested.append(run_id) or dict(progress={}, log_tail=[]))
    monkeypatch.setattr(ui, "running_duration", lambda _: 0)
    assert ui._job_panel(service, domain=None)
    assert fake.titles == ["Jobs actifs — Tous les types"]
    assert len(requested) == len(JobType) + 1
    assert "**scientific_analysis**" in fake.cards and "**forward_simulation**" in fake.cards
    assert len(fake.buttons) == len(requested) and all(label == "Annuler" for label, _ in fake.buttons)


def test_no_active_jobs_has_no_heading_or_detail_reads(monkeypatch):
    fake = FakeUI()
    monkeypatch.setattr(ui, "st", fake)
    service = SimpleNamespace(runs=lambda: [dict(job_type="walk_forward", status="completed")],
                              run=lambda _: pytest.fail("Finished job detail read"))
    assert not ui._job_panel(service, domain=None)
    assert not fake.titles and not fake.cards


@pytest.mark.parametrize("tab", [0, 1, 2])
def test_history_adds_global_panel_after_selected_tab(monkeypatch, tab):
    fake = FakeUI()
    fake.tabs = lambda *a, **kw: [SimpleNamespace(open=i == tab) for i in range(3)]
    fake.session_state = SimpleNamespace(get=lambda _: None, lab_config=SimpleNamespace(project_root="fixture"))
    # Selected-tab rendering is isolated from data services; the global panel
    # must still be invoked once after the body, with no domain/type filter.
    import pandas as pd
    fake.expander = lambda *a, **kw: nullcontext()
    fake.divider = lambda: None
    events = []
    monkeypatch.setattr(ui, "st", fake)
    service = object()
    monkeypatch.setattr(ui, "_service", lambda: service)
    monkeypatch.setattr(ui, "_page_header", lambda *_: events.append("header"))
    monkeypatch.setattr(ui, "_history_runs_panel", lambda *a, **kw: events.append("grid"))
    monkeypatch.setattr(ui, "render_dataframe", lambda *a, **kw: events.append("data"))
    fake.info = lambda *_: None
    monkeypatch.setattr(ui, "ModelService", lambda *_: SimpleNamespace(models=lambda: []))
    monkeypatch.setattr(ui, "SignalService", lambda *_: SimpleNamespace(history=lambda: pd.DataFrame(columns=["category"]), realized_results=lambda: pd.DataFrame()))
    monkeypatch.setattr(ui, "PredictionService", lambda *_: SimpleNamespace(history=lambda: pd.DataFrame()))
    def panel(actual_service, **kwargs):
        assert actual_service is service and kwargs == {"domain": None}
        events.append("global")
    monkeypatch.setattr(ui, "_live_job_panel", panel)
    ui._history_page()
    assert events[-1] == "global" and events.count("global") == 1
