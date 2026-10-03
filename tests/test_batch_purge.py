"""Batch purge coordinates the existing per-run storage policy."""

from __future__ import annotations

from contextlib import nullcontext
from types import SimpleNamespace

from rstock.application.batch_purge import execute_batch_purge, preview_batch_purge
from rstock.application.run_storage import PurgeArtifact, PurgeEligibility, PurgePlan
from rstock.application import streamlit_app


class FakePurgeService:
    def __init__(self) -> None:
        self.states = {
            "parent": "eligible",
            "child": "eligible",
            "purged": "already purged",
            "missing": "run missing",
            "incomplete": "run incomplete",
            "error": "eligible",
        }
        self.calls: list[tuple[str, str]] = []

    def purge_eligibility(self, run_id: str) -> PurgeEligibility:
        self.calls.append(("eligibility", run_id))
        state = self.states[run_id]
        return PurgeEligibility(state == "eligible", None if state == "eligible" else state)

    def purge_preview(self, run_id: str) -> PurgePlan:
        self.calls.append(("preview", run_id))
        return PurgePlan(
            run_id,
            (PurgeArtifact(run_id, "heavy.bin", 10),)
            + ((PurgeArtifact("child", "heavy.bin", 20),) if run_id == "parent" else ()),
            ("child",) if run_id == "parent" else (),
            30 if run_id == "parent" else 20 if run_id == "child" else 10,
        )

    def purge_run_type(self, run_id: str) -> str:
        return "end_to_end" if run_id == "parent" else "walk_forward"

    def purge(self, run_id: str) -> dict[str, object]:
        self.calls.append(("purge", run_id))
        if run_id == "error":
            raise OSError("disk error")
        self.states[run_id] = "already purged"
        if run_id == "parent":
            self.states["child"] = "already purged"
        return {"state": "purged", "reclaimed_bytes": 30 if run_id == "parent" else 20}


def test_batch_preview_checks_existing_policy_and_deduplicates_parent_artifacts():
    service = FakePurgeService()

    review = preview_batch_purge(
        service, ["child", "purged", "parent", "missing", "incomplete", "parent"]
    )

    assert review.requested_run_ids == (
        "child", "purged", "parent", "missing", "incomplete"
    )
    assert review.eligible_run_ids == ("parent", "child")
    assert review.affected_run_ids == ("parent", "child")
    assert review.affected_by_type == (("end_to_end", 1), ("walk_forward", 1))
    assert review.skipped == (
        ("purged", "already purged"),
        ("missing", "run missing"),
        ("incomplete", "run incomplete"),
    )
    assert review.reclaimable_bytes == 30
    assert not any(call[0] == "purge" for call in service.calls)
    assert not any(call == ("preview", run_id) for run_id in ("purged", "missing", "incomplete") for call in service.calls)


def test_batch_execution_continues_after_error_and_rechecks_each_run():
    service = FakePurgeService()
    review = preview_batch_purge(service, ["error", "parent", "child", "missing"])
    service.calls.clear()

    result = execute_batch_purge(service, review)

    assert result.succeeded == ("parent", "child")
    assert result.skipped == (("missing", "run missing"),)
    assert result.errors == (("error", "disk error"),)
    assert result.reclaimed_bytes == 30
    assert service.calls == [
        ("eligibility", "error"), ("purge", "error"),
        ("eligibility", "parent"), ("purge", "parent"),
    ]


def test_batch_execution_skips_run_that_became_ineligible_after_preview():
    service = FakePurgeService()
    review = preview_batch_purge(service, ["parent"])
    service.states["parent"] = "run active"
    service.calls.clear()

    result = execute_batch_purge(service, review)

    assert result.succeeded == ()
    assert result.skipped == (("parent", "run active"),)
    assert result.errors == ()
    assert service.calls == [("eligibility", "parent")]


def test_batch_preview_error_does_not_block_other_runs():
    service = FakePurgeService()
    original_preview = service.purge_preview

    def preview(run_id: str) -> PurgePlan:
        if run_id == "error":
            raise OSError("preview unavailable")
        return original_preview(run_id)

    service.purge_preview = preview  # type: ignore[method-assign]
    review = preview_batch_purge(service, ["error", "parent"])

    assert review.eligible_run_ids == ("parent",)
    assert review.errors == (("error", "preview unavailable"),)
    assert review.reclaimable_bytes == 30


def test_batch_confirmation_refreshes_once_after_all_purges(monkeypatch):
    service = FakePurgeService()
    review = preview_batch_purge(service, ["error", "parent", "missing"])
    events: list[str] = []
    state: dict[str, object] = {"history-pending-batch-purge": review}

    def button(label: str, **_kwargs: object) -> bool:
        events.append(label)
        return label == "Confirmer la purge groupée"

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

    streamlit_app._render_batch_purge_confirmation(
        service, review, key_prefix="history"
    )

    assert service.states["parent"] == "already purged"
    assert events.count("rerun") == 1
    assert events.index("rerun") > events.index("Confirmer la purge groupée")
    assert any("3 runs sélectionnés" in event and "3 runs concernés" in event for event in events)
    assert any("Par type :" in event and "1" in event for event in events)
    assert "history-pending-batch-purge" not in state
    result = state["history-batch-purge-result"]
    assert result.succeeded == ("parent",)
    assert len(result.errors) == 1
