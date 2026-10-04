"""Standalone Predictor prefilter and frozen-snapshot derivation contracts."""

from __future__ import annotations

import hashlib
from dataclasses import replace

import pandas as pd
import pytest

from rstock.application import workflows
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.history_ui import EXPERIMENT_JOB_TYPES, JOB_LABELS
from rstock.application.model_ui import job_domain
from rstock.application.prefilter_experiments import (
    PREFILTER_DERIVATION_FIELDS, build_derived_prefilter_spec,
)
from rstock.application.repository import RunRepository
from rstock.application.run_detail_tabs import tabs_for_job
from rstock.application.runner import RunService
from rstock.config import DEFAULT_CONFIG
from rstock.walk_forward import PrefilterWalkForwardResult


class _Backend:
    def launch(self, *_args):
        return 4321


def _fixture(tmp_path, monkeypatch):
    original_prepared_inputs = workflows._prepared_inputs
    repository = RunRepository(tmp_path / "runs")
    config = replace(
        DEFAULT_CONFIG, project_root=tmp_path,
        predictor_prefilter_enabled=True, predictor_prefilter_top_n=1,
        final_holdout_size=1, walk_forward_end_offset_sessions=0,
    )
    spec = ExperimentSpec(
        job_type=JobType.PREDICTOR_PREFILTER, config=config,
        symbols=("AAA", "BBB", "CCC", "DDD"), target_symbols=("AAA",),
        context_symbols=("BBB", "CCC", "DDD"),
        historical_data_cutoff="2026-09-25",
    )
    prepared = pd.DataFrame(
        {"AAA_Close": range(10), "BBB_Close": range(10)},
        index=pd.date_range("2026-09-14", periods=10, freq="B"),
    )
    prepared.attrs["effective_end_date"] = "2026-09-25T00:00:00"
    prepared.attrs["symbols_used"] = 4
    prepared.attrs["walk_forward_end_offset_sessions"] = 0
    preparation_calls = []
    qualifications = []

    def prepare(*_args):
        preparation_calls.append(True)
        return prepared.copy(), ["AAA", "BBB", "CCC", "DDD"], ["AAA"], {
            symbol: "XNYS" for symbol in ("AAA", "BBB", "CCC", "DDD")
        }

    def evaluate(_prepared, _sets, effective_config, **_kwargs):
        qualifications.append(effective_config.qualification_min_median_auc)
        rows = []
        for symbol, median in (("BBB", 0.75), ("CCC", 0.70), ("DDD", 0.65)):
            rows.append({
                "Observation": "AAA", "Predictors": f'["{symbol}"]',
                "Eligible": True, "ROCAUCMedian": median,
                "PctWindowsAboveRandom": 0.8, "ROCAUCWorst": 0.6,
                "ROCAUCStd": 0.03,
            })
        return PrefilterWalkForwardResult(
            pd.DataFrame(rows), {"pairs_admissible": 3}, ("AAA",), {},
        )

    monkeypatch.setattr(workflows, "_prepared_inputs", prepare)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", evaluate)
    monkeypatch.setattr(workflows, "generate_symbol_sets", lambda *_args, **_kwargs: pd.DataFrame({"V1": ["BBB", "CCC", "DDD"]}))
    return repository, spec, preparation_calls, qualifications, original_prepared_inputs


def _complete(repository, run_id, summary):
    repository.write_json(run_id, "summary.json", summary)
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.COMPLETED)


def test_prefilter_job_and_derived_run_share_frozen_source_without_market(tmp_path, monkeypatch):
    repository, spec, preparation_calls, qualifications, original_prepared_inputs = _fixture(tmp_path, monkeypatch)
    service = RunService(repository, backend=_Backend())
    parent = service.submit(spec).run_id
    parent_summary = workflows._predictor_prefilter(
        repository.load_spec(parent), repository.run_directory(parent) / "results", None, None,
    )
    _complete(repository, parent, parent_summary)
    parent_snapshot = repository.run_directory(parent) / "checkpoints/artifacts/prepared_snapshot.pkl"
    parent_sha = hashlib.sha256(parent_snapshot.read_bytes()).hexdigest()
    assert preparation_calls == [True]
    assert pd.read_csv(repository.run_directory(parent) / "results/predictor_prefilter.csv").query(
        "PrefilterStatus == 'retained'"
    )["Predictor"].tolist() == ["BBB"]

    monkeypatch.setattr(workflows, "_prepared_inputs", original_prepared_inputs)
    monkeypatch.setattr(workflows.MarketDataService, "load", lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("market downloaded")))
    monkeypatch.setattr(workflows, "prepare_dataset", lambda *_args: (_ for _ in ()).throw(AssertionError("market re-prepared")))
    child = service.create_derived(parent, "predictor_prefilter", {
        "predictor_prefilter_top_n": 2,
        "predictor_prefilter_min_median_auc": 0.6,
    }).run_id
    child_spec = repository.load_spec(child)
    assert child_spec.config.predictor_prefilter_top_n == 2
    assert child_spec.config.predictor_prefilter_min_median_auc == 0.6
    assert child_spec.source_experiment_run == parent
    assert child_spec.prepared_snapshot_required
    assert child_spec.historical_data_cutoff == spec.historical_data_cutoff
    assert child_spec.prefilter_derivation["prepared_snapshot_sha256"] == parent_sha
    assert child_spec.source_prepared_dataset_sha256 == parent_summary["traceability"]["prepared_dataset_sha256"]
    child_summary = workflows._predictor_prefilter(
        child_spec, repository.run_directory(child) / "results", None, None,
    )
    assert child_summary["traceability"]["prepared_dataset_sha256"] == child_spec.source_prepared_dataset_sha256
    assert child_summary["prepared_dataset_as_of"] == parent_summary["prepared_dataset_as_of"]
    assert pd.read_csv(repository.run_directory(child) / "results/predictor_prefilter.csv").query(
        "PrefilterStatus == 'retained'"
    )["Predictor"].tolist() == ["BBB", "CCC"]
    assert qualifications == [spec.config.predictor_prefilter_min_median_auc, 0.6]
    assert preparation_calls == [True]
    assert not (repository.run_directory(child) / "results/walk_forward.csv").exists()


def test_prefilter_derivation_rejects_missing_or_changed_source_snapshot(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    parent = repository.create(spec)
    summary = workflows._predictor_prefilter(
        spec, repository.run_directory(parent) / "results", None, None,
    )
    _complete(repository, parent, summary)
    path = repository.run_directory(parent) / "checkpoints/artifacts/prepared_snapshot.pkl"
    changes = {"predictor_prefilter_top_n": 2}
    derived = build_derived_prefilter_spec(repository, parent, changes)
    path.write_bytes(path.read_bytes() + b"changed")
    with pytest.raises(ValueError, match="corrupt or changed"):
        workflows._predictor_prefilter(
            derived, repository.run_directory(parent) / "unused", None, None,
        )
    path.unlink()
    with pytest.raises(ValueError, match="missing"):
        build_derived_prefilter_spec(repository, parent, changes)


def test_prefilter_derivation_rejects_source_digest_mismatch(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    parent = repository.create(spec)
    summary = workflows._predictor_prefilter(
        spec, repository.run_directory(parent) / "results", None, None,
    )
    summary["traceability"]["prepared_dataset_sha256"] = "0" * 64
    _complete(repository, parent, summary)
    with pytest.raises(ValueError, match="digest"):
        build_derived_prefilter_spec(repository, parent, {"predictor_prefilter_top_n": 2})


def test_prefilter_resume_uses_committed_preparation_after_interruption(tmp_path, monkeypatch):
    repository, spec, preparation_calls, _, _ = _fixture(tmp_path, monkeypatch)
    run_id = repository.create(spec)
    evaluator = workflows.evaluate_prefilter_walk_forward

    def interrupt_once(*_args, **_kwargs):
        raise InterruptedError("interrupted after snapshot")

    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", interrupt_once)
    output = repository.run_directory(run_id) / "results"
    with pytest.raises(InterruptedError):
        workflows._predictor_prefilter(spec, output, None, None)
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", evaluator)
    monkeypatch.setattr(
        workflows, "_prepared_inputs",
        lambda *_args: (_ for _ in ()).throw(AssertionError("preparation repeated")),
    )
    summary = workflows._predictor_prefilter(spec, output, None, None)
    assert summary["prepared_dataset_as_of"] == spec.historical_data_cutoff
    assert preparation_calls == [True]


def test_prefilter_job_visible_in_history_and_derived_fields_are_bounded(tmp_path):
    assert "predictor_prefilter" in EXPERIMENT_JOB_TYPES
    assert JOB_LABELS["predictor_prefilter"] == "Préfiltre prédicteurs"
    assert job_domain(JobType.PREDICTOR_PREFILTER) == "experiment"
    assert {tab.key for tab in tabs_for_job(JobType.PREDICTOR_PREFILTER)} >= {
        "results", "resources", "configuration", "files", "logs",
    }
    assert "predictor_prefilter_top_n" in PREFILTER_DERIVATION_FIELDS
