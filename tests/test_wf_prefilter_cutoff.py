"""Frozen Prefilter cutoffs across launch, copying and worker resume."""
from dataclasses import replace

import pytest

from test_prefilter_pipeline import _source
from test_prefilter_experiments import _Backend
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.prefilter_contract import load, inherit_reference
from rstock.application.runner import RunService
from rstock.application.worker import execute_run
from rstock.application.workflows import WorkflowRegistry, _validate_walk_forward_prefilter_input
from rstock.application.experiment_duplication import (
    walk_forward_duplication_draft, experiment_spec_from_duplication,
)


@pytest.mark.parametrize("field", ["historical_data_cutoff", "resolved_market_session_cutoff"])
def test_backend_rejects_conflicting_cutoff_before_launch(tmp_path, monkeypatch, field):
    repository, _, wf, _ = _source(tmp_path, monkeypatch)
    invalid = replace(wf, **{field: "2026-09-18"})
    before = repository.list_run_ids()
    with pytest.raises(ValueError, match="cutoff.*match.*Prefilter"):
        RunService(repository, backend=_Backend()).submit(invalid)
    assert repository.list_run_ids() == before
    with pytest.raises(ValueError, match="cutoff.*match.*Prefilter"):
        _validate_walk_forward_prefilter_input(invalid)


def test_duplication_inherits_contract_instead_of_trace_cutoff(tmp_path, monkeypatch):
    repository, source, wf, _ = _source(tmp_path, monkeypatch)
    detail = dict(configuration=wf.to_dict(), summary=dict(traceability={
        "prepared_market_last_date": "2026-09-18",
        "prepared_dataset_sha256": wf.source_prepared_dataset_sha256,
    }))
    draft = walk_forward_duplication_draft("old-wf", detail)
    copied = experiment_spec_from_duplication(draft, current_config=wf.config, use_run_config=True)
    assert copied.source_prefilter_run == source
    assert copied.historical_data_cutoff == copied.resolved_market_session_cutoff == "2026-09-25"
    assert copied.requested_historical_cutoff is None
    assert load(repository, copied)["cutoff"] == "2026-09-25"


def test_same_reference_resume_preserves_cutoff_and_rejects_corruption(tmp_path, monkeypatch):
    repository, _, wf, _ = _source(tmp_path, monkeypatch)
    wf = inherit_reference(repository, wf)
    run_id = repository.create(wf)
    def fail(*args):
        raise RuntimeError("interrupted")
    registry = WorkflowRegistry({JobType.WALK_FORWARD: fail})
    execute_run(repository, run_id, 1, registry=registry)
    assert repository.status(run_id)["status"] == "failed"
    service = RunService(repository, backend=_Backend())
    result = service.resume(run_id)
    assert result.run_id == run_id
    assert repository.load_spec(run_id) == wf
    execute_run(repository, run_id, 1, registry=registry)
    invalid = replace(wf, historical_data_cutoff="2026-09-18")
    repository.write_json(run_id, "config.json", invalid.to_dict())
    with pytest.raises(ValueError, match="cutoff.*match.*Prefilter"):
        service.resume(run_id)
    assert repository.status(run_id)["status"] == "failed"


def test_historical_wf_without_prefilter_reference_keeps_its_cutoff(tmp_path, monkeypatch):
    repository, _, wf, _ = _source(tmp_path, monkeypatch)
    snapshot = replace(
        wf, source_prefilter_run=None, source_prefilter_contract_sha256=None,
        historical_data_cutoff="2026-09-18",
    ).to_dict()
    snapshot.pop("prefilter_execution_version")
    historical = ExperimentSpec.from_dict(snapshot)
    assert historical.prefilter_execution_version == 1
    submitted = RunService(repository, backend=_Backend()).submit(historical)
    restored = repository.load_spec(submitted.run_id)
    assert restored.historical_data_cutoff == "2026-09-18"
