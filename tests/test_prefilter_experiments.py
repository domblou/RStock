"""Standalone Predictor prefilter and frozen-snapshot derivation contracts."""

from __future__ import annotations

import hashlib
from contextlib import nullcontext
from dataclasses import replace
from types import SimpleNamespace

import pandas as pd
import pytest

from rstock.application import workflows
from rstock.application import streamlit_app
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.history_ui import EXPERIMENT_JOB_TYPES, JOB_LABELS
from rstock.application.model_ui import job_domain
from rstock.application.prefilter_experiments import (
    PREFILTER_DERIVATION_FIELDS, PREFILTER_XGBOOST_FIELDS,
    build_derived_prefilter_spec,
)
from rstock.application.prefilter_stability import (
    aggregate_temporal_prefilter, resolve_stability_origins,
)
from rstock.application.repository import RunRepository
from rstock.application.run_detail_tabs import tabs_for_job
from rstock.application.runner import RunService
from rstock.application.worker import execute_run
from rstock.checkpoints import CheckpointIncompatibleError
from rstock.config import DEFAULT_CONFIG
from rstock.modeling import historical_xgboost_parameters, prefilter_xgboost_parameters
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
        {
            "AAA_Close": range(10), "BBB_Close": range(10),
            **{f"{symbol}.intraday_return": [0.01] * 10
               for symbol in ("AAA", "BBB", "CCC", "DDD")},
        },
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


def test_prefilter_derived_xgboost_values_are_inherited_and_used(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    source_values = {
        "prefilter_xgb_max_depth": 7, "prefilter_xgb_eta": 0.3, "prefilter_xgb_num_boost_round": 11,
        "prefilter_xgb_min_child_weight": 1.5, "prefilter_xgb_subsample": 0.9,
        "prefilter_xgb_colsample_bytree": 0.8, "prefilter_xgb_gamma": 0.1,
        "prefilter_xgb_reg_alpha": 0.2, "prefilter_xgb_reg_lambda": 2.0,
        "prefilter_xgb_seed": 4321,
    }
    overrides = {
        "prefilter_xgb_max_depth": 3, "prefilter_xgb_eta": 0.2, "prefilter_xgb_num_boost_round": 20,
        "prefilter_xgb_min_child_weight": 2.5, "prefilter_xgb_subsample": 0.75,
        "prefilter_xgb_colsample_bytree": 0.6, "prefilter_xgb_gamma": 0.3,
        "prefilter_xgb_reg_alpha": 0.4, "prefilter_xgb_reg_lambda": 3.0,
        "prefilter_xgb_seed": 5678,
    }
    spec = replace(spec, config=replace(spec.config, **source_values))
    parent = repository.create(spec)
    parent_summary = workflows._predictor_prefilter(
        spec, repository.run_directory(parent) / "results", None, None,
    )
    _complete(repository, parent, parent_summary)

    inherited = build_derived_prefilter_spec(
        repository, parent, {"predictor_prefilter_top_n": 2},
    )
    assert {field: getattr(inherited.config, field)
            for field in PREFILTER_XGBOOST_FIELDS} == source_values

    received = []
    evaluate = workflows.evaluate_prefilter_walk_forward

    def capture(prepared, sets, config, **kwargs):
        received.append({field: getattr(config, field)
                         for field in PREFILTER_XGBOOST_FIELDS})
        return evaluate(prepared, sets, config, **kwargs)

    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", capture)
    child = RunService(repository, backend=_Backend()).create_derived(
        parent, "predictor_prefilter",
        overrides,
    ).run_id
    child_spec = repository.load_spec(child)
    assert {field: getattr(child_spec.config, field)
            for field in PREFILTER_XGBOOST_FIELDS} == overrides
    parameters = prefilter_xgboost_parameters(child_spec.config)
    assert parameters.as_dict() == {
        "max_depth": 3, "eta": 0.2, "num_boost_round": 20,
        "min_child_weight": 2.5, "subsample": 0.75,
        "colsample_bytree": 0.6, "gamma": 0.3,
        "reg_alpha": 0.4, "reg_lambda": 3.0,
    }
    assert set(child_spec.prefilter_derivation["overrides"]) == set(overrides)
    workflows._predictor_prefilter(
        child_spec, repository.run_directory(child) / "results", None, None,
    )
    assert received == [overrides]
    assert historical_xgboost_parameters(child_spec.config) == historical_xgboost_parameters(spec.config)
    assert child_spec.config.xgb_seed == spec.config.xgb_seed


def test_prefilter_derivation_ui_prefills_and_submits_xgboost_values(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    source_values = {
        "prefilter_xgb_max_depth": 7, "prefilter_xgb_eta": 0.3, "prefilter_xgb_num_boost_round": 11,
        "prefilter_xgb_min_child_weight": 1.5, "prefilter_xgb_subsample": 0.9,
        "prefilter_xgb_colsample_bytree": 0.8, "prefilter_xgb_gamma": 0.1,
        "prefilter_xgb_reg_alpha": 0.2, "prefilter_xgb_reg_lambda": 2.0,
        "prefilter_xgb_seed": 4321,
    }
    spec = replace(spec, config=replace(spec.config, **source_values))
    run_id = repository.create(spec)
    shown = {}
    submitted = []
    overrides = {
        "prefilter_xgb_max_depth": 3, "prefilter_xgb_eta": 0.2, "prefilter_xgb_num_boost_round": 20,
        "prefilter_xgb_min_child_weight": 2.5, "prefilter_xgb_subsample": 0.75,
        "prefilter_xgb_colsample_bytree": 0.6, "prefilter_xgb_gamma": 0.3,
        "prefilter_xgb_reg_alpha": 0.4, "prefilter_xgb_reg_lambda": 3.0,
        "prefilter_xgb_seed": 5678,
    }

    class FakeStreamlit:
        session_state = {}

        def button(self, _label, *, key, **_kwargs):
            return key in {f"prefilter-derive-button-{run_id}",
                           f"prefilter-derive-submit-{run_id}"}

        def number_input(self, _label, *, key, value, **_kwargs):
            field = key.removeprefix(f"prefilter-derive-{run_id}-")
            shown[field] = value
            return overrides.get(field, value)

        def selectbox(self, _label, values, *, index, **_kwargs):
            return values[index]

        def container(self, **_kwargs):
            return nullcontext()

        def caption(self, *_args):
            pass

        def subheader(self, *_args):
            pass

        def columns(self, count):
            return [self] * count

        def success(self, *_args):
            pass

    service = SimpleNamespace(
        run_service=SimpleNamespace(repository=repository),
        create_derived=lambda parent, kind, changes: (
            submitted.append((parent, kind, changes.copy()))
            or SimpleNamespace(run_id="child")
        ),
    )
    monkeypatch.setattr(streamlit_app, "st", FakeStreamlit())
    streamlit_app._render_prefilter_derived_creation(
        run_id, {"configuration": {}, "status": {"status": "completed"}}, service,
    )
    assert {field: shown[field] for field in overrides} == source_values
    assert submitted == [(run_id, "predictor_prefilter", overrides)]


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


def test_historical_prefilter_derivation_keeps_legacy_xgboost_override(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    parent = repository.create(spec)
    snapshot = spec.to_dict()
    for field in PREFILTER_XGBOOST_FIELDS:
        snapshot["rstock_config"].pop(field)
    repository.write_json(parent, "config.json", snapshot)
    source = repository.load_spec(parent)
    summary = workflows._predictor_prefilter(
        source, repository.run_directory(parent) / "results", None, None,
    )
    _complete(repository, parent, summary)
    derived = build_derived_prefilter_spec(repository, parent, {"prefilter_xgb_eta": 0.3})
    child = repository.create(derived)
    legacy = derived.to_dict()
    for field in PREFILTER_XGBOOST_FIELDS:
        legacy["rstock_config"].pop(field)
    legacy["rstock_config"]["xgb_eta"] = 0.3
    legacy["prefilter_derivation"]["overrides"] = {
        "xgb_eta": {"old_value": source.config.xgb_eta, "new_value": 0.3},
    }
    repository.write_json(child, "config.json", legacy)
    restored = repository.load_spec(child)
    assert restored.config.prefilter_xgb_eta == 0.3
    assert restored.config.prefilter_xgb_num_boost_round == source.config.xgb_rounds
    result = workflows._predictor_prefilter(
        restored, repository.run_directory(child) / "results", None, None,
    )
    assert result["traceability"]["prepared_dataset_sha256"] == summary["traceability"]["prepared_dataset_sha256"]
    with pytest.raises(ValueError, match="Unsupported"):
        build_derived_prefilter_spec(repository, parent, {"xgb_eta": 0.4})


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


def test_new_prefilter_worker_creates_matching_checkpoint(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    run_id = repository.create(spec)
    manifest_path = repository.run_directory(run_id) / "checkpoints/manifest.json"
    assert not manifest_path.exists()

    execute_run(repository, run_id, 1)

    assert repository.status(run_id)["status"] == "completed"
    manifest = repository.read_json(run_id, "checkpoints/manifest.json")
    assert manifest["run_id"] == run_id
    assert manifest["job_type"] == JobType.PREDICTOR_PREFILTER.value
    assert manifest["configuration_fingerprint"] == repository.configuration_fingerprint(run_id)
    assert manifest["batch_sizes"] == {
        "predictor_prefilter_walk_forward": spec.config.predictor_prefilter_batch_size,
    }


def test_prefilter_worker_resumes_compatible_checkpoint(tmp_path, monkeypatch):
    repository, spec, preparation_calls, _, _ = _fixture(tmp_path, monkeypatch)
    run_id = repository.create(spec)
    evaluator = workflows.evaluate_prefilter_walk_forward

    def interrupted(*_args, **_kwargs):
        raise InterruptedError("stop after preparation")

    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", interrupted)
    execute_run(repository, run_id, 1)
    assert repository.status(run_id)["status"] == "failed"
    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", evaluator)
    monkeypatch.setattr(
        workflows, "_prepared_inputs",
        lambda *_args: (_ for _ in ()).throw(AssertionError("preparation repeated")),
    )

    RunService(repository, backend=_Backend()).resume(run_id)
    execute_run(repository, run_id, 1)

    assert repository.status(run_id)["status"] == "completed"
    assert preparation_calls == [True]
    manifest = repository.read_json(run_id, "checkpoints/manifest.json")
    assert manifest["attempt_count"] == 2
    assert manifest["resume_count"] == 1


@pytest.mark.parametrize("changed_field", [
    "configuration_fingerprint", "batch_sizes", "job_type",
])
def test_prefilter_resume_rejects_only_incompatible_checkpoint(
    tmp_path, monkeypatch, changed_field,
):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    run_id = repository.create(spec)
    monkeypatch.setattr(
        workflows, "evaluate_prefilter_walk_forward",
        lambda *_args, **_kwargs: (_ for _ in ()).throw(InterruptedError("stop")),
    )
    execute_run(repository, run_id, 1)
    assert repository.status(run_id)["status"] == "failed"
    manifest = repository.read_json(run_id, "checkpoints/manifest.json")
    if changed_field == "configuration_fingerprint":
        manifest[changed_field] = "0" * 64
    elif changed_field == "batch_sizes":
        manifest[changed_field]["final_holdout"] = 25
    else:
        manifest[changed_field] = JobType.WALK_FORWARD.value
    repository.write_json(run_id, "checkpoints/manifest.json", manifest)

    with pytest.raises(CheckpointIncompatibleError, match="configuration"):
        RunService(repository, backend=_Backend()).resume(run_id)
    assert repository.status(run_id)["status"] == "failed"


def test_historical_prefilter_run_defaults_to_single_origin(tmp_path, monkeypatch):
    repository, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    values = spec.to_dict()
    for field in ("prefilter_method", "stability_origin_count", "stability_step_sessions"):
        values.pop(field)
    restored = ExperimentSpec.from_dict(values)
    assert restored.prefilter_method == "single_origin"
    assert restored.stability_origin_count == 5
    assert restored.stability_step_sessions == 1
    current_id = repository.create(spec)
    historical_id = repository.create(restored)
    for run_id, configuration in ((current_id, spec), (historical_id, restored)):
        workflows._predictor_prefilter(
            configuration, repository.run_directory(run_id) / "results", None, None,
        )
    pd.testing.assert_frame_equal(
        pd.read_csv(repository.run_directory(current_id) / "results/predictor_prefilter.csv"),
        pd.read_csv(repository.run_directory(historical_id) / "results/predictor_prefilter.csv"),
    )


def test_temporal_origins_resolve_market_sessions_and_reject_missing_or_incomplete(tmp_path, monkeypatch):
    _, spec, _, _, _ = _fixture(tmp_path, monkeypatch)
    prepared, *_ = workflows._prepared_inputs(spec, None, None)
    origins = resolve_stability_origins(
        prepared, cutoff=spec.historical_data_cutoff, calendar="XNYS",
        origin_count=3, step_sessions=1, symbols=spec.predictor_symbols,
    )
    assert [value.date().isoformat() for value in origins] == [
        "2026-09-25", "2026-09-24", "2026-09-23",
    ]
    missing = prepared.drop(pd.Timestamp("2026-09-24"))
    with pytest.raises(ValueError, match="lacks temporal origin"):
        resolve_stability_origins(
            missing, cutoff=spec.historical_data_cutoff, calendar="XNYS",
            origin_count=3, step_sessions=1, symbols=spec.predictor_symbols,
        )
    incomplete = prepared.copy()
    incomplete.at[pd.Timestamp("2026-09-24"), "BBB.intraday_return"] = float("nan")
    with pytest.raises(ValueError, match="incompl"):
        resolve_stability_origins(
            incomplete, cutoff=spec.historical_data_cutoff, calendar="XNYS",
            origin_count=3, step_sessions=1, symbols=spec.predictor_symbols,
        )


def test_temporal_aggregate_is_deterministic_and_redundancy_follows_top_n(tmp_path):
    config = replace(
        DEFAULT_CONFIG, project_root=tmp_path, predictor_prefilter_top_n=2,
        predictor_prefilter_correlation_threshold=0.8, lag_depth=1,
    )
    frames = []
    for cutoff in ("2026-09-25", "2026-09-24", "2026-09-23"):
        rows = []
        for rank, symbol in enumerate(("BBB", "CCC", "DDD"), start=1):
            rows.append({
                "OriginCutoff": cutoff, "Observation": "AAA", "Predictor": symbol,
                "PrefilterScoreRank": rank, "PrefilterRank": rank,
                "PrefilterScore": 1.0 - rank / 10,
                "ROCAUCMedian": 0.8 - rank / 20,
                "PctWindowsAboveRandom": 0.8, "ROCAUCWorst": 0.6,
                "ROCAUCStd": 0.03, "Eligible": True,
                "PrefilterStatus": "retained" if rank <= 2 else "rejected_top_n",
            })
        frames.append(pd.DataFrame(rows))
    development = pd.DataFrame({
        "BBB_intraday_J-1": [1, 2, 3, 4],
        "CCC_intraday_J-1": [2, 4, 6, 8],
        "DDD_intraday_J-1": [1, 0, 1, 0],
    })
    aggregate, details, retained = aggregate_temporal_prefilter(
        frames, targets=["AAA"], predictors=["AAA", "BBB", "CCC", "DDD"],
        config=config, principal_development=development,
    )
    reversed_aggregate, _, _ = aggregate_temporal_prefilter(
        [frame.iloc[::-1] for frame in frames[::-1]], targets=["AAA"],
        predictors=["AAA", "BBB", "CCC", "DDD"],
        config=config, principal_development=development,
    )
    pd.testing.assert_frame_equal(aggregate, reversed_aggregate)
    assert aggregate["Predictor"].tolist() == ["BBB", "CCC", "DDD"]
    assert aggregate["AggregateRank"].tolist() == [1, 2, 3]
    assert aggregate["TopNFrequency"].tolist() == [1.0, 1.0, 0.0]
    assert aggregate["PrefilterStatus"].tolist() == [
        "retained", "removed_redundancy", "rejected_top_n",
    ]
    assert retained["AAA"] == ("BBB",)
    assert len(details) == 9


def test_temporal_top_n_uses_aggregate_eligibility_before_score(tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, predictor_prefilter_top_n=1)
    frames = []
    for cutoff, bbb_eligible in (("2026-09-25", True), ("2026-09-24", False)):
        frames.append(pd.DataFrame([
            {
                "OriginCutoff": cutoff, "Observation": "AAA", "Predictor": "BBB",
                "PrefilterScoreRank": 1, "PrefilterRank": 1 if bbb_eligible else pd.NA,
                "PrefilterScore": 100.0, "ROCAUCMedian": 0.9,
                "PctWindowsAboveRandom": 1.0, "ROCAUCWorst": 0.9,
                "ROCAUCStd": 0.0, "Eligible": bbb_eligible,
                "PrefilterStatus": "retained" if bbb_eligible else "rejected_threshold",
            },
            {
                "OriginCutoff": cutoff, "Observation": "AAA", "Predictor": "CCC",
                "PrefilterScoreRank": 2, "PrefilterRank": 2 if bbb_eligible else 1,
                "PrefilterScore": 1.0, "ROCAUCMedian": 0.7,
                "PctWindowsAboveRandom": 0.8, "ROCAUCWorst": 0.6,
                "ROCAUCStd": 0.1, "Eligible": True,
                "PrefilterStatus": "rejected_top_n" if bbb_eligible else "retained",
            },
        ]))
    aggregate, _, retained = aggregate_temporal_prefilter(
        frames, targets=["AAA"], predictors=["AAA", "BBB", "CCC"],
        config=config, principal_development=pd.DataFrame(),
    )
    assert aggregate["Predictor"].tolist() == ["CCC", "BBB"]
    assert aggregate["EligibleFrequency"].tolist() == [1.0, 0.5]
    assert aggregate["TopNFrequency"].tolist() == [0.5, 0.5]
    assert retained["AAA"] == ("CCC",)


def test_single_origin_can_derive_temporal_on_exact_source_snapshot(tmp_path, monkeypatch):
    repository, spec, preparation_calls, _, original_prepared_inputs = _fixture(tmp_path, monkeypatch)
    parent = repository.create(spec)
    parent_summary = workflows._predictor_prefilter(
        spec, repository.run_directory(parent) / "results", None, None,
    )
    _complete(repository, parent, parent_summary)
    parent_sha = hashlib.sha256((
        repository.run_directory(parent) / "checkpoints/artifacts/prepared_snapshot.pkl"
    ).read_bytes()).hexdigest()
    monkeypatch.setattr(workflows, "_prepared_inputs", original_prepared_inputs)
    monkeypatch.setattr(workflows.MarketDataService, "load", lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("market downloaded")))
    monkeypatch.setattr(workflows, "prepare_dataset", lambda *_args: (_ for _ in ()).throw(AssertionError("market re-prepared")))
    evaluator = workflows.evaluate_prefilter_walk_forward
    observed = []

    def evaluate(origin_view, *args, **kwargs):
        observed.append((origin_view.index.max().date().isoformat(), len(origin_view)))
        return evaluator(origin_view, *args, **kwargs)

    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", evaluate)
    child = RunService(repository, backend=_Backend()).create_derived(
        parent, "predictor_prefilter", {
            "prefilter_method": "temporal_stability",
            "stability_origin_count": 3,
            "stability_step_sessions": 2,
        },
    ).run_id
    child_spec = repository.load_spec(child)
    assert child_spec.prefilter_method == "temporal_stability"
    assert child_spec.stability_origin_count == 3
    assert child_spec.stability_step_sessions == 2
    assert child_spec.prefilter_derivation["prepared_snapshot_sha256"] == parent_sha
    summary = workflows._predictor_prefilter(
        child_spec, repository.run_directory(child) / "results", None, None,
    )
    assert observed == [("2026-09-25", 10), ("2026-09-23", 8), ("2026-09-21", 6)]
    assert preparation_calls == [True]
    assert summary["traceability"]["prepared_dataset_sha256"] == parent_summary["traceability"]["prepared_dataset_sha256"]
    assert summary["origin_cutoffs"] == [date for date, _ in observed]
    results = repository.run_directory(child) / "results"
    assert pd.read_csv(results / "predictor_prefilter_origins.csv")["OriginCutoff"].nunique() == 3
    assert "AggregateRank" in pd.read_csv(results / "predictor_prefilter.csv")


def test_temporal_prefilter_resumes_from_completed_origin(tmp_path, monkeypatch):
    repository, base, preparation_calls, _, _ = _fixture(tmp_path, monkeypatch)
    spec = replace(base, prefilter_method="temporal_stability", stability_origin_count=3)
    run_id = repository.create(spec)
    output = repository.run_directory(run_id) / "results"
    evaluator = workflows.evaluate_prefilter_walk_forward
    observed = []
    failed = False

    def interrupt_second(origin_view, *args, **kwargs):
        nonlocal failed
        date = origin_view.index.max().date().isoformat()
        observed.append(date)
        if date == "2026-09-24" and not failed:
            failed = True
            raise InterruptedError("second origin interrupted")
        return evaluator(origin_view, *args, **kwargs)

    monkeypatch.setattr(workflows, "evaluate_prefilter_walk_forward", interrupt_second)
    with pytest.raises(InterruptedError):
        workflows._predictor_prefilter(spec, output, None, None)
    monkeypatch.setattr(
        workflows, "_prepared_inputs",
        lambda *_args: (_ for _ in ()).throw(AssertionError("snapshot prepared twice")),
    )
    summary = workflows._predictor_prefilter(spec, output, None, None)
    assert summary["origin_cutoffs"] == ["2026-09-25", "2026-09-24", "2026-09-23"]
    assert observed == ["2026-09-25", "2026-09-24", "2026-09-24", "2026-09-23"]
    assert preparation_calls == [True]
