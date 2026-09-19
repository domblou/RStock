import importlib
import hashlib
import json
import os
import sys
import threading
import time
from dataclasses import replace
from datetime import datetime, timedelta, timezone
from types import SimpleNamespace

import pandas as pd
import pytest

from rstock.application.domain import (
    ExperimentSpec,
    JobStatus,
    JobType,
    RunMetadata,
    RunRole,
)
import rstock.application.repository as repository_module
import rstock.application.runner as runner_module
from rstock.application.repository import RunRepository
from rstock.application.runner import ProgressReporter, RunService
from rstock.application.worker import RunLease, SlotLease, execute_run
from rstock.application.workflows import (
    WorkflowRegistry,
    _prepared_experiment,
    _require_exploitable_prefilter,
    _resumable_walk_forward,
    _walk_forward,
)
from rstock.config import DEFAULT_CONFIG
from rstock.progress import ProgressEvent, check_cancellation


class FakeBackend:
    def __init__(self, *, error=None):
        self.error = error
        self.launches = []

    def launch(self, runs_root, run_id, max_concurrent_jobs):
        if self.error:
            raise self.error
        self.launches.append((runs_root, run_id, max_concurrent_jobs))
        return 4321


def _spec(tmp_path, job_type=JobType.WALK_FORWARD, **config_changes):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, **config_changes)
    return ExperimentSpec(
        job_type=job_type,
        config=config,
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
    )


def test_run_creation_persists_required_files_and_reloadable_configuration(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    spec = _spec(tmp_path, xgb_seed=987)
    run_id = repository.create(spec)
    directory = repository.run_directory(run_id)

    assert {
        "config.json",
        "metadata.json",
        "status.json",
        "progress.json",
        "summary.json",
        "run.log",
    } <= {path.name for path in directory.iterdir()}
    assert repository.status(run_id)["status"] == "pending"
    restored = RunRepository(tmp_path / "runs").load_spec(run_id)
    assert restored == spec
    assert restored.config.xgb_seed == 987
    snapshot = repository.read_json(run_id, "config.json")
    assert snapshot["schema_version"] == 1
    assert snapshot["pipeline_version"] == 1
    assert snapshot["calibration_sampling_policy_version"] == 2


def test_phase_five_enables_walk_forward_batch_and_end_to_end():
    assert JobType.WALK_FORWARD_BATCH.value == "walk_forward_batch"
    assert JobType.END_TO_END.value == "end_to_end"
    assert JobType.WALK_FORWARD_BATCH.implemented
    assert JobType.END_TO_END.implemented


def test_repository_persists_queryable_parent_child_metadata(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent_id = repository.create(
        _spec(tmp_path), metadata=RunMetadata(run_role=RunRole.PIPELINE_PARENT)
    )
    relation_key = "walk_forward_batch:0003"
    child_id = repository.deterministic_child_run_id(parent_id, relation_key)
    metadata = RunMetadata(
        run_role=RunRole.TECHNICAL_BATCH,
        visible_in_history=False,
        parent_run_id=parent_id,
        relation_key=relation_key,
        relation_type="walk_forward_batch",
        stage_key="walk_forward",
        stage_index=0,
        batch_id="0003",
        batch_index=3,
        batch_count=8,
    )

    created_id = repository.create(_spec(tmp_path), run_id=child_id, metadata=metadata)

    persisted = repository.run_metadata(child_id)
    assert created_id == child_id
    assert persisted.parent_run_id == parent_id
    assert persisted.root_run_id == parent_id
    assert persisted.created_by_run_id == parent_id
    assert persisted.visible_in_history is False
    assert repository.list_children(parent_id) == [child_id]
    assert repository.child_for_relation(parent_id, relation_key) == child_id
    assert repository.deterministic_child_run_id(parent_id, relation_key) == child_id
    with pytest.raises(FileExistsError):
        repository.create(_spec(tmp_path), run_id=child_id, metadata=metadata)


def test_repository_reads_runs_without_metadata_as_visible_standalone_runs(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    (repository.run_directory(run_id) / "metadata.json").unlink()

    metadata = repository.run_metadata(run_id)

    assert metadata.run_role is RunRole.STANDALONE
    assert metadata.visible_in_history is True
    assert metadata.parent_run_id is None


def test_experiment_snapshot_uses_explicit_historical_defaults_and_raw_fingerprint(
    tmp_path,
):
    values = _spec(tmp_path).to_dict()
    values["schema_version"] = 1
    for name in (
        "source_end_to_end_run",
        "source_threshold_calibration_run",
        "market_benchmark_symbol",
        "auto_promote_candidates",
        "pipeline_version",
        "calibration_sampling_policy_version",
        "combination_plan_version",
        "combination_plan_sha256",
        "combination_range_start",
        "combination_range_stop",
    ):
        values.pop(name)
    for name in (
        "walk_forward_max_combinations_per_batch",
        "xgboost_global_max_qualified_combinations",
        "threshold_parameter_calibration_max_models",
        "temporal_min_candidate_yield_ratio",
        "temporal_max_auc_degradation",
        "temporal_min_precision_edge",
        "temporal_min_mean_directional_return",
        "temporal_confidence_level",
        "temporal_max_ci_width",
    ):
        values["rstock_config"].pop(name)
    canonical = json.dumps(values, sort_keys=True, separators=(",", ":"))

    restored = ExperimentSpec.from_dict(values)

    assert restored.source_end_to_end_run is None
    assert restored.source_threshold_calibration_run is None
    assert restored.market_benchmark_symbol is None
    assert restored.auto_promote_candidates is False
    assert restored.pipeline_version == 0
    assert restored.calibration_sampling_policy_version == 1
    assert restored.combination_plan_version is None
    assert restored.combination_plan_sha256 is None
    assert restored.combination_range_start is None
    assert restored.combination_range_stop is None
    assert restored.config.walk_forward_max_combinations_per_batch == 2_200_000
    assert restored.config.xgboost_global_max_qualified_combinations is None
    assert restored.config.threshold_parameter_calibration_max_models is None
    assert restored.config.temporal_min_candidate_yield_ratio == 0.25
    assert restored.config.temporal_max_auc_degradation == 0.03
    assert restored.config.temporal_min_precision_edge == 0.00
    assert restored.config.temporal_min_mean_directional_return == 0.00
    assert restored.config.temporal_confidence_level == 0.95
    assert restored.config.temporal_max_ci_width == 0.20
    assert restored.fingerprint == hashlib.sha256(canonical.encode()).hexdigest()
    assert restored.to_dict()["schema_version"] == 1


def test_walk_forward_batch_capacity_is_serialized_with_the_experiment(tmp_path):
    spec = replace(
        _spec(tmp_path),
        config=replace(
            _spec(tmp_path).config,
            walk_forward_max_combinations_per_batch=10_000,
        ),
    )

    restored = ExperimentSpec.from_dict(spec.to_dict())

    assert restored.config.walk_forward_max_combinations_per_batch == 10_000
    assert restored.to_dict()["rstock_config"][
        "walk_forward_max_combinations_per_batch"
    ] == 10_000


def test_new_pipeline_fields_and_combination_range_are_part_of_snapshot_identity(
    tmp_path,
):
    baseline = _spec(tmp_path)
    pipeline_spec = replace(
        baseline,
        source_end_to_end_run="end-to-end-parent",
        auto_promote_candidates=True,
        combination_plan_version=2,
        combination_plan_sha256="plan-sha256",
        combination_range_start=100,
        combination_range_stop=200,
    )

    snapshot = pipeline_spec.to_dict()

    assert snapshot["schema_version"] == 1
    assert snapshot["source_end_to_end_run"] == "end-to-end-parent"
    assert snapshot["auto_promote_candidates"] is True
    assert snapshot["combination_range_start"] == 100
    assert snapshot["combination_range_stop"] == 200
    assert pipeline_spec.fingerprint != baseline.fingerprint


def test_empty_market_context_remains_a_valid_predictor_population(tmp_path):
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
    )

    assert spec.context_symbols == ()
    assert spec.predictor_symbols == ("AAA", "BBB")


@pytest.mark.parametrize(
    ("pairs_admissible", "targets"),
    [(0, ("T",)), (1, ())],
)
def test_prefilter_fails_clearly_without_exploitable_population(
    pairs_admissible, targets
):
    result = SimpleNamespace(
        telemetry={"pairs_admissible": pairs_admissible},
        exploitable_targets=targets,
    )

    with pytest.raises(ValueError, match="no exploitable pairs"):
        _require_exploitable_prefilter(result)


def test_legacy_experiment_config_defaults_missing_batch_sizes(tmp_path):
    values = _spec(tmp_path).to_dict()
    for name in (
        "predictor_prefilter_batch_size",
        "walk_forward_batch_size",
        "final_holdout_batch_size",
    ):
        values["rstock_config"].pop(name)

    restored = ExperimentSpec.from_dict(values)

    assert restored.config.predictor_prefilter_batch_size == 25
    assert restored.config.walk_forward_batch_size == 25
    assert restored.config.final_holdout_batch_size == 25


def test_walk_forward_preparation_fetches_predictor_union_but_limits_targets(
    monkeypatch, tmp_path
):
    index = pd.bdate_range("2026-01-05", periods=4)
    prices = pd.DataFrame(index=index)
    for symbol in ("AAA", "BBB", "CONTEXT"):
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.High"] = 102.0
        prices[f"{symbol}.Low"] = 99.0
        prices[f"{symbol}.Close"] = 101.0
    captured_symbols = []

    def fake_load(self, spec, **kwargs):
        captured_symbols.append(spec.symbols)
        return SimpleNamespace(
            prices=prices,
            symbols=list(spec.symbols),
            failed_symbols=[],
        ), {}

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, permutation_depth=1),
        symbols=("AAA", "BBB", "CONTEXT"),
        target_symbols=("AAA", "BBB"),
        context_symbols=("CONTEXT",),
    )

    prepared, generated, _ = _prepared_experiment(spec, None, None)

    assert captured_symbols == [("AAA", "BBB", "CONTEXT")]
    assert "CONTEXT_intraday_J-1" in prepared
    assert set(generated["V0"]) == {"AAA", "BBB"}
    assert "CONTEXT" in set(generated["V1"])


def test_walk_forward_persists_prepared_traceability(monkeypatch, tmp_path):
    prepared = pd.DataFrame(
        {"AAA.Open": [100.0, 101.0]},
        index=pd.DatetimeIndex(["2026-01-02", "2026-01-05"]),
    )
    prepared.attrs["symbols_used"] = 2
    generated = pd.DataFrame({"V0": ["AAA"], "V1": ["BBB"]})
    result = SimpleNamespace(
        aggregate_global=pd.DataFrame([{"Sets": 1}]),
        qualification=pd.DataFrame({"Eligible": [True]}),
        run_configuration={},
    )
    captured = {}

    monkeypatch.setattr(
        "rstock.traceability.current_git_commit", lambda project_root: "deadbeef"
    )

    monkeypatch.setattr(
        "rstock.application.workflows._prepared_experiment",
        lambda *args, **kwargs: (prepared, generated, {}),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_walk_forward",
        lambda *args, **kwargs: result,
    )

    def write_result(result, output):
        output.mkdir(parents=True, exist_ok=True)
        captured.update(result.run_configuration)

    monkeypatch.setattr(
        "rstock.application.workflows.write_walk_forward_results", write_result
    )

    summary = _walk_forward(
        _spec(tmp_path, permutation_depth=1), tmp_path / "output", None, None
    )

    assert summary["traceability"] == captured["traceability"]
    assert captured["traceability"]["git_commit"] == "deadbeef"
    assert captured["traceability"]["prepared_market_last_date"] == "2026-01-05T00:00:00"
    assert captured["traceability"]["symbols_used"] == 2
    assert len(captured["traceability"]["prepared_dataset_sha256"]) == 64


def test_disabled_predictor_prefilter_keeps_the_existing_walk_forward_path(
    monkeypatch, tmp_path
):
    generated = pd.DataFrame([{"V0": "AAA", "V1": "BBB"}])
    result = SimpleNamespace(
        aggregate_global=pd.DataFrame([{"Sets": 1}]),
        qualification=pd.DataFrame([{"Eligible": True}]),
        run_configuration={},
    )
    calls = []

    def fake_prepared(spec, progress, cancellation):
        calls.append("existing-preparation")
        return pd.DataFrame(), generated, {}

    monkeypatch.setattr("rstock.application.workflows._prepared_experiment", fake_prepared)
    monkeypatch.setattr(
        "rstock.application.workflows.select_predictors",
        lambda *args, **kwargs: pytest.fail("disabled prefilter must not run"),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_walk_forward",
        lambda *args, **kwargs: result,
    )
    monkeypatch.setattr(
        "rstock.application.workflows.write_walk_forward_results",
        lambda *args, **kwargs: None,
    )

    summary = _walk_forward(_spec(tmp_path), tmp_path, None, None)

    assert calls == ["existing-preparation"]
    assert "predictor_prefilter" not in summary
    assert "total_combinations" not in summary


def test_enabled_prefilter_feeds_only_retained_predictors_to_final_generation(
    monkeypatch, tmp_path
):
    index = pd.bdate_range("2026-01-01", periods=10)
    prepared = pd.DataFrame(index=index)
    for symbol, offset in (("A", 0.0), ("B", 10.0)):
        for lag in range(1, 4):
            prepared[f"{symbol}_intraday_J-{lag}"] = range(
                lag, lag + len(index)
            )
            prepared[f"{symbol}_intraday_J-{lag}"] += offset
    qualification = pd.DataFrame([
        {
            "Set": "T<-A", "Observation": "T", "Predictors": '["A"]',
            "ROCAUCMedian": 0.65, "PctWindowsAboveRandom": 1.0,
            "ROCAUCStd": 0.02, "ROCAUCWorst": 0.55, "Eligible": True,
            "IneligibilityReasons": "[]",
        },
        {
            "Set": "T<-B", "Observation": "T", "Predictors": '["B"]',
            "ROCAUCMedian": 0.45, "PctWindowsAboveRandom": 0.25,
            "ROCAUCStd": 0.20, "ROCAUCWorst": 0.30, "Eligible": False,
            "IneligibilityReasons": '["median_auc"]',
        },
    ])
    final_result = SimpleNamespace(
        aggregate_global=pd.DataFrame([{"Sets": 1}]),
        qualification=pd.DataFrame([{"Eligible": True}]),
        run_configuration={},
    )
    evaluated_sets = []
    evaluated_configs = []
    evaluated_options = []

    monkeypatch.setattr(
        "rstock.application.workflows._prepared_inputs",
        lambda *args: (prepared, ["T", "A", "B"], ["T"], {}),
    )

    def fake_evaluate(data, generated, config, **kwargs):
        evaluated_sets.append(generated.copy())
        evaluated_configs.append(config)
        evaluated_options.append(kwargs)
        if len(evaluated_sets) == 1:
            return SimpleNamespace(
                qualification=qualification,
                telemetry={"pairs_admissible": 2},
                exploitable_targets=("T",),
                excluded_targets={},
            )
        return final_result

    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_prefilter_walk_forward", fake_evaluate
    )
    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_walk_forward", fake_evaluate
    )
    monkeypatch.setattr(
        "rstock.application.workflows.write_walk_forward_results",
        lambda *args, **kwargs: None,
    )
    spec = _spec(
        tmp_path,
        predictor_prefilter_enabled=True,
        predictor_prefilter_top_n=1,
        permutation_depth=2,
        final_holdout_size=2,
    )
    spec = replace(
        spec,
        symbols=("T", "A", "B"),
        target_symbols=("T",),
        context_symbols=("A", "B"),
    )

    summary = _walk_forward(spec, tmp_path, None, None)

    assert len(evaluated_sets) == 2
    assert evaluated_sets[0]["V1"].tolist() == ["A", "B"]
    assert evaluated_sets[1]["V1"].tolist() == ["A"]
    assert evaluated_sets[1]["V2"].isna().all()
    assert evaluated_configs[0].qualification_min_median_auc == (
        spec.config.predictor_prefilter_min_median_auc
    )
    assert "evaluate_holdout" not in evaluated_options[0]
    assert "evaluate_holdout" not in evaluated_options[1]
    assert summary["total_combinations"] == 1
    assert summary["predictor_prefilter"][0]["retained_predictors"] == ["A"]
    assert {
        "rejected_median_auc",
        "rejected_pct_above_random",
        "rejected_worst_auc",
        "rejected_auc_std",
    } <= set(summary["predictor_prefilter"][0])
    assert (tmp_path / "predictor_prefilter.csv").exists()
    assert (tmp_path / "predictor_prefilter.json").exists()


def test_status_transitions_and_duration_are_persisted(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    running = repository.transition(run_id, JobStatus.RUNNING, pid=123)
    completed = repository.transition(run_id, JobStatus.COMPLETED)

    assert running["started_at"] is not None
    assert running["pid"] == 123
    assert completed["finished_at"] is not None
    assert completed["duration_seconds"] >= 0
    with pytest.raises(ValueError, match="Invalid job transition"):
        repository.transition(run_id, JobStatus.RUNNING)


def test_progress_persists_real_percentage_and_observed_eta(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.started -= 1.0

    reporter(ProgressEvent("simulated", "unit", 2, 4, {"direction": "Up"}))
    progress = RunRepository(tmp_path / "runs").progress(run_id)

    assert progress["percent"] == 50.0
    assert progress["completed_units"] == 2
    assert progress["total_units"] == 4
    assert progress["eta_seconds"] is not None
    assert progress["details"] == {"direction": "Up"}


def test_phase_progress_never_reaches_global_100_before_final_publication(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.configure_phases([("walk_forward", 90), ("publishing", 10)])

    reporter.phase_started("walk_forward")
    reporter(ProgressEvent("walk_forward", "last", 10, 10))
    assert repository.progress(run_id)["stage_percent"] == 100.0
    assert repository.progress(run_id)["workflow_percent"] == 90.0
    reporter.phase_completed("walk_forward")
    assert repository.progress(run_id)["workflow_percent"] == 90.0
    with pytest.raises(RuntimeError, match="before publishing"):
        reporter.complete_workflow()

    reporter.phase_started("publishing")
    assert repository.progress(run_id)["workflow_percent"] < 100.0
    reporter.phase_completed("publishing")
    assert repository.progress(run_id)["workflow_percent"] == 99.9
    reporter.complete_workflow()
    assert repository.progress(run_id)["workflow_percent"] == 100.0


def test_unmeasurable_phase_has_no_eta(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    reporter = ProgressReporter(repository, run_id)
    reporter.configure_phases([("aggregation", 1)])
    reporter.phase_started("aggregation")

    progress = repository.progress(run_id)
    assert progress["stage_percent"] is None
    assert progress["eta_seconds"] is None


def test_json_write_retries_temporary_windows_permission_error(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    original_replace = os.replace
    calls = 0

    def temporarily_locked(source, destination):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise PermissionError(5, "Access denied", str(destination))
        return original_replace(source, destination)

    monkeypatch.setattr(repository_module.os, "replace", temporarily_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    assert repository.write_json(run_id, "status.json", {"status": "updated"})
    assert calls == 2
    assert repository.read_json(run_id, "status.json") == {"status": "updated"}
    assert not list(repository.run_directory(run_id).glob(".status.json.*.tmp"))


def test_persistent_lock_skips_only_progress_and_allows_later_recovery(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    original_replace = os.replace
    previous_progress = repository.progress(run_id)
    previous_status = repository.status(run_id)

    def persistently_locked(source, destination):
        raise PermissionError(5, "Access denied", str(destination))

    monkeypatch.setattr(repository_module.os, "replace", persistently_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    reporter = ProgressReporter(repository, run_id)
    reporter(ProgressEvent("blocked", "progress", 1, 2))
    assert repository.write_json(run_id, "progress.json", {"stage": "blocked"}) is False
    assert repository.progress(run_id) == previous_progress
    assert "Progress update skipped" in "\n".join(repository.log_tail(run_id))
    assert not list(repository.run_directory(run_id).glob(".progress.json.*.tmp"))
    with pytest.raises(PermissionError):
        repository.write_json(run_id, "status.json", {"status": "blocked"})
    assert repository.status(run_id) == previous_status
    assert not list(repository.run_directory(run_id).glob(".status.json.*.tmp"))

    monkeypatch.setattr(repository_module.os, "replace", original_replace)
    assert repository.write_json(run_id, "progress.json", {"stage": "recovered"})
    assert repository.progress(run_id) == {"stage": "recovered"}


def test_progress_read_retries_a_temporary_windows_permission_error(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    original_read = repository._read_json_path
    calls = 0

    def temporarily_locked(path):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise PermissionError(32, "Sharing violation", str(path))
        return original_read(path)

    monkeypatch.setattr(repository, "_read_json_path", temporarily_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    assert repository.progress(run_id)["stage"] == "pending"
    assert calls == 2


def test_persistent_progress_read_lock_returns_cached_or_safe_display_state(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    cached = repository.progress(run_id)

    def persistently_locked(path):
        raise PermissionError(32, "Sharing violation", str(path))

    monkeypatch.setattr(repository, "_read_json_path", persistently_locked)
    monkeypatch.setattr(repository_module.time, "sleep", lambda _: None)

    assert repository.progress(run_id) == cached
    fresh = RunRepository(tmp_path / "runs")
    monkeypatch.setattr(fresh, "_read_json_path", persistently_locked)
    assert fresh.progress(run_id)["temporarily_unavailable"] is True


def test_invalid_progress_json_is_not_hidden_by_lock_tolerance(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    (repository.run_directory(run_id) / "progress.json").write_text(
        "{not valid json", encoding="utf-8"
    )

    with pytest.raises(json.JSONDecodeError):
        repository.progress(run_id)


def test_pending_cancellation_is_durable(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    backend = FakeBackend()
    service = RunService(repository, backend=backend)
    submitted = service.submit(_spec(tmp_path))

    cancelled = service.cancel(submitted.run_id)

    assert cancelled["status"] == "cancelled"
    assert repository.cancellation_requested(submitted.run_id)
    assert RunRepository(tmp_path / "runs").status(submitted.run_id)["status"] == "cancelled"


def test_duplicate_active_submission_returns_existing_run(tmp_path):
    backend = FakeBackend()
    service = RunService(RunRepository(tmp_path / "runs"), backend=backend)
    spec = _spec(tmp_path)

    first = service.submit(spec)
    second = RunService(
        RunRepository(tmp_path / "runs"), backend=backend
    ).submit(spec)

    assert first.created is True
    assert second.created is False
    assert second.run_id == first.run_id
    assert len(backend.launches) == 1


def test_worker_failure_marks_failed_and_does_not_publish_results(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def fail(spec, output, progress, cancellation):
        (output / "partial.csv").write_text("partial", encoding="utf-8")
        raise RuntimeError("simulated worker failure")

    registry = WorkflowRegistry({JobType.WALK_FORWARD: fail})
    execute_run(repository, run_id, 1, registry=registry)

    status = repository.status(run_id)
    assert status["status"] == "failed"
    assert status["error"] == "simulated worker failure"
    assert repository.result_files(run_id) == []
    assert "simulated worker failure" in "\n".join(repository.log_tail(run_id))


def test_worker_cooperative_cancellation_keeps_partial_results_unpublished(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def cancel(spec, output, progress, cancellation):
        (output / "partial.csv").write_text("partial", encoding="utf-8")
        repository.request_cancellation(run_id)
        check_cancellation(cancellation)
        return {}

    registry = WorkflowRegistry({JobType.WALK_FORWARD: cancel})
    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == "cancelled"
    assert repository.result_files(run_id) == []
    assert (repository.run_directory(run_id) / "run.log").exists()


def test_completed_workers_publish_results_in_isolated_run_directories(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    first = repository.create(_spec(tmp_path))
    second = repository.create(_spec(tmp_path, job_type=JobType.THRESHOLD_CALIBRATION))

    def succeed(spec, output, progress, cancellation):
        (output / "result.txt").write_text(spec.job_type.value, encoding="utf-8")
        return {"job_type": spec.job_type.value, "metric": 1}

    registry = WorkflowRegistry(
        {
            JobType.WALK_FORWARD: succeed,
            JobType.THRESHOLD_CALIBRATION: succeed,
        }
    )
    execute_run(repository, first, 1, registry=registry)
    execute_run(repository, second, 1, registry=registry)

    assert repository.status(first)["status"] == "completed"
    assert repository.status(second)["status"] == "completed"
    assert repository.summary(first)["job_type"] == "walk_forward"
    assert repository.summary(second)["job_type"] == "threshold_calibration"
    assert (
        repository.run_directory(first) / "results" / "result.txt"
    ).read_text(encoding="utf-8") == "walk_forward"
    assert (
        repository.run_directory(second) / "results" / "result.txt"
    ).read_text(encoding="utf-8") == "threshold_calibration"


def test_worker_persists_phase_logs_and_completes_only_after_publishing(tmp_path, monkeypatch):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    completed_progress = []
    original_transition = repository.transition

    def observed_transition(identifier, target, **kwargs):
        if target is JobStatus.COMPLETED:
            completed_progress.append(repository.progress(identifier).copy())
        return original_transition(identifier, target, **kwargs)

    monkeypatch.setattr(repository, "transition", observed_transition)

    def succeed(spec, output, progress, cancellation):
        for phase in (
            "data_preparation", "combination_generation", "walk_forward",
            "aggregation", "qualification", "final_holdout", "metrics", "result_writing",
        ):
            progress(ProgressEvent(phase, "started", details={"phase_event": "started"}))
            progress(ProgressEvent(phase, "completed", details={"phase_event": "completed"}))
        (output / "result.txt").write_text("ready", encoding="utf-8")
        return {"job_type": spec.job_type.value}

    execute_run(repository, run_id, 1, registry=WorkflowRegistry({JobType.WALK_FORWARD: succeed}))

    assert repository.status(run_id)["status"] == "completed"
    assert completed_progress[0]["workflow_percent"] == 100.0
    assert completed_progress[0]["phase_history"][-1]["name"] == "publishing"
    assert completed_progress[0]["phase_history"][-1]["status"] == "completed"
    log = "\n".join(repository.log_tail(run_id, lines=100))
    for phase in ("data_preparation", "walk_forward", "final_holdout", "result_writing", "publishing"):
        assert f"Phase started: {phase}" in log
        assert f"Phase completed: {phase}" in log


def test_slot_lease_enforces_concurrency_limit(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    repository.root.mkdir(parents=True)
    first = SlotLease(repository, "first", 1, poll_seconds=0.01)
    second = SlotLease(repository, "second", 1, poll_seconds=0.01)
    first.acquire()
    acquired = threading.Event()

    def acquire_second():
        second.acquire()
        acquired.set()

    thread = threading.Thread(target=acquire_second)
    thread.start()
    time.sleep(0.05)
    assert not acquired.is_set()
    first.release()
    assert acquired.wait(timeout=1)
    second.release()
    thread.join(timeout=1)


def test_failed_walk_forward_can_resume_same_run_only_once(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def fail(spec, output, progress, cancellation):
        raise RuntimeError("boom")

    execute_run(
        repository,
        run_id,
        1,
        registry=WorkflowRegistry({JobType.WALK_FORWARD: fail}),
    )
    backend = FakeBackend()
    service = RunService(repository, backend=backend)

    resumed = service.resume(run_id)

    assert resumed.run_id == run_id
    assert resumed.created is False
    assert repository.status(run_id)["status"] == "pending"
    assert backend.launches[0][1] == run_id
    with pytest.raises(ValueError, match="déjà"):
        service.resume(run_id)


def test_resume_uses_persisted_fingerprint_for_a_historical_snapshot(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def fail(spec, output, progress, cancellation):
        raise RuntimeError("boom")

    execute_run(
        repository,
        run_id,
        1,
        registry=WorkflowRegistry({JobType.WALK_FORWARD: fail}),
    )
    authoritative_fingerprint = repository.status(run_id)[
        "configuration_fingerprint"
    ]
    historical = repository.read_json(run_id, "config.json")
    historical["schema_version"] = 1
    for name in (
        "source_end_to_end_run",
        "source_threshold_calibration_run",
        "auto_promote_candidates",
        "pipeline_version",
        "calibration_sampling_policy_version",
        "combination_plan_version",
        "combination_plan_sha256",
        "combination_range_start",
        "combination_range_stop",
    ):
        historical.pop(name)
    repository.write_json(run_id, "config.json", historical)

    assert repository.load_spec(run_id).fingerprint != authoritative_fingerprint
    resumed = RunService(repository, backend=FakeBackend()).resume(run_id)

    assert resumed.run_id == run_id
    assert repository.status(run_id)["status"] == JobStatus.PENDING.value
    assert repository.read_json(run_id, "config.json")["schema_version"] == 1


def test_restart_creates_new_run_and_keeps_failed_source_intact(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = repository.create(_spec(tmp_path))
    repository.transition(source, JobStatus.FAILED, error="original failure")
    backend = FakeBackend()

    restarted = RunService(repository, backend=backend).restart(source)

    assert restarted.run_id != source
    assert repository.status(source)["status"] == "failed"
    assert repository.status(restarted.run_id)["restarted_from_run"] == source
    assert repository.load_spec(restarted.run_id).source_walk_forward_run == source


def test_restart_preserves_frozen_xgboost_calibration_provenance(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source_spec = replace(
        _spec(tmp_path),
        source_xgboost_calibration_run="xgb-parent",
        frozen_xgboost_parameters={
            "Up": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120},
            "Down": {"max_depth": 3, "eta": 0.1, "num_boost_round": 80},
        },
    )
    source = repository.create(source_spec)
    repository.transition(source, JobStatus.FAILED, error="original failure")

    restarted = RunService(repository, backend=FakeBackend()).restart(source)
    replayed = repository.load_spec(restarted.run_id)

    assert replayed.source_xgboost_calibration_run == "xgb-parent"
    assert replayed.frozen_xgboost_parameters == source_spec.frozen_xgboost_parameters
    assert replayed.frozen_xgboost_parameters_sha256 == (
        source_spec.frozen_xgboost_parameters_sha256
    )


def test_restart_reuses_historical_traceability_cutoff(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = repository.create(_spec(tmp_path))
    repository.write_json(
        source,
        "summary.json",
        {
            "traceability": {
                "prepared_market_last_date": "2025-01-31T00:00:00",
                "prepared_dataset_sha256": "source-hash",
            }
        },
    )
    repository.transition(source, JobStatus.FAILED, error="original failure")

    restarted = RunService(repository, backend=FakeBackend()).restart(source)
    spec = repository.load_spec(restarted.run_id)

    assert spec.historical_data_cutoff == "2025-01-31T00:00:00"
    assert spec.source_prepared_dataset_sha256 == "source-hash"
    assert spec.source_walk_forward_run == source


def test_per_run_lease_refuses_a_second_live_worker(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    first = RunLease(repository, run_id)
    second = RunLease(repository, run_id)
    first.acquire()
    try:
        with pytest.raises(RuntimeError, match="verrou"):
            second.acquire()
    finally:
        first.release()


def test_resume_refuses_legacy_run_without_checkpoint(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.FAILED, error="legacy")

    with pytest.raises(ValueError, match="Aucun checkpoint"):
        RunService(repository, backend=FakeBackend()).resume(run_id)


def _age_running_run(repository, run_id, *, seconds=120):
    old = (datetime.now(timezone.utc) - timedelta(seconds=seconds)).isoformat()
    status = repository.status(run_id)
    status["started_at"] = old
    repository.write_json(run_id, "status.json", status)
    progress = repository.progress(run_id)
    progress["updated_at"] = old
    repository.write_json(run_id, "progress.json", progress)


def test_transient_negative_pid_probe_just_after_running_is_ignored(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=4321)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda _: False)

    detail = RunService(repository, backend=FakeBackend()).get(run_id)

    assert detail["status"]["status"] == "running"


def test_dead_running_worker_is_interrupted_only_after_durable_confirmation(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    _age_running_run(repository, run_id)
    monkeypatch.setattr(runner_module, "INTERRUPTION_CONFIRMATION_SECONDS", 0.0)
    service = RunService(repository, backend=FakeBackend())

    first = service.get(run_id)
    second = service.get(run_id)

    assert first["status"]["status"] == "running"
    assert second["status"]["status"] == "interrupted"


def test_recent_heartbeat_prevents_interruption_of_pid_negative_run(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    status = repository.status(run_id)
    status["started_at"] = (
        datetime.now(timezone.utc) - timedelta(minutes=2)
    ).isoformat()
    repository.write_json(run_id, "status.json", status)
    monkeypatch.setattr(runner_module, "_pid_alive", lambda _: False)
    monkeypatch.setattr(runner_module, "INTERRUPTION_CONFIRMATION_SECONDS", 0.0)
    service = RunService(repository, backend=FakeBackend())

    service.get(run_id)
    detail = service.get(run_id)

    assert detail["status"]["status"] == "running"


def test_live_worker_lease_protects_long_phase_without_progress(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=os.getpid())
    _age_running_run(repository, run_id)
    lease = RunLease(repository, run_id)
    lease.acquire()
    probes = 0

    def transient_primary_failure(_):
        nonlocal probes
        probes += 1
        return probes % 2 == 0

    monkeypatch.setattr(runner_module, "_pid_alive", transient_primary_failure)
    service = RunService(repository, backend=FakeBackend())
    try:
        assert service.get(run_id)["status"]["status"] == "running"
        assert service.list()[0]["status"] == "running"
    finally:
        lease.release()


@pytest.mark.parametrize(
    "job_type",
    [JobType.THRESHOLD_PARAMETER_CALIBRATION, JobType.THRESHOLD_CALIBRATION],
)
def test_calibration_worker_completes_under_concurrent_polling(
    monkeypatch, tmp_path, job_type
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, job_type=job_type))
    entered = threading.Event()
    release = threading.Event()

    def calibration(spec, output, progress, cancellation):
        entered.set()
        assert release.wait(timeout=2)
        (output / "result.txt").write_text(spec.job_type.value, encoding="utf-8")
        return {"job_type": spec.job_type.value}

    worker = threading.Thread(
        target=execute_run,
        args=(repository, run_id, 1),
        kwargs={"registry": WorkflowRegistry({job_type: calibration})},
    )
    worker.start()
    assert entered.wait(timeout=2)
    real_pid_alive = runner_module._pid_alive
    first_probe = True

    def one_false_negative(pid):
        nonlocal first_probe
        if first_probe:
            first_probe = False
            return False
        return real_pid_alive(pid)

    monkeypatch.setattr(runner_module, "_pid_alive", one_false_negative)
    service = RunService(RunRepository(repository.root), backend=FakeBackend())
    assert service.get(run_id)["status"]["status"] == "running"
    assert service.list()[0]["status"] == "running"
    release.set()
    worker.join(timeout=3)

    assert not worker.is_alive()
    assert repository.status(run_id)["status"] == "completed"


def test_worker_recovers_false_interrupted_status_after_publishing(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def succeed_after_false_interruption(spec, output, progress, cancellation):
        (output / "result.txt").write_text("ready", encoding="utf-8")
        repository.transition(
            run_id,
            JobStatus.INTERRUPTED,
            error="Le processus worker n’est plus actif.",
        )
        return {"job_type": spec.job_type.value}

    execute_run(
        repository,
        run_id,
        1,
        registry=WorkflowRegistry(
            {JobType.WALK_FORWARD: succeed_after_false_interruption}
        ),
    )

    status = repository.status(run_id)
    assert status["status"] == "completed"
    assert status["error"] is None
    assert repository.progress(run_id)["workflow_percent"] == 100.0
    assert "Recovered inconsistent interrupted status" in "\n".join(
        repository.log_tail(run_id, lines=100)
    )


def test_repository_does_not_generally_allow_interrupted_to_completed(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=123)
    repository.transition(run_id, JobStatus.INTERRUPTED)

    with pytest.raises(ValueError, match="Invalid job transition"):
        repository.transition(run_id, JobStatus.COMPLETED)


def test_polling_reader_tolerates_worker_completion_during_interruption_decision(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))
    repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    _age_running_run(repository, run_id)
    monkeypatch.setattr(runner_module, "INTERRUPTION_CONFIRMATION_SECONDS", 0.0)
    service = RunService(repository, backend=FakeBackend())
    service.get(run_id)
    original_transition = repository.transition

    def worker_wins(identifier, target, **kwargs):
        if target is JobStatus.INTERRUPTED:
            original_transition(identifier, JobStatus.COMPLETED)
        return original_transition(identifier, target, **kwargs)

    monkeypatch.setattr(repository, "transition", worker_wins)

    detail = service.get(run_id)

    assert detail["status"]["status"] == "completed"


def test_attempt_history_survives_failed_then_successful_resume(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path))

    def fail(spec, output, progress, cancellation):
        raise RuntimeError("first attempt")

    execute_run(
        repository,
        run_id,
        1,
        registry=WorkflowRegistry({JobType.WALK_FORWARD: fail}),
    )
    RunService(repository, backend=FakeBackend()).resume(run_id)

    def succeed(spec, output, progress, cancellation):
        (output / "result.txt").write_text("ok", encoding="utf-8")
        return {"job_type": spec.job_type.value}

    execute_run(
        repository,
        run_id,
        1,
        registry=WorkflowRegistry({JobType.WALK_FORWARD: succeed}),
    )
    manifest = repository.read_json(run_id, "checkpoints/manifest.json")

    assert repository.status(run_id)["status"] == "completed"
    assert manifest["attempt_count"] == 2
    assert manifest["resume_count"] == 1
    assert [attempt["status"] for attempt in manifest["attempts"]] == [
        "failed",
        "completed",
    ]


def test_resumable_workflow_reuses_prepared_and_generated_snapshots(monkeypatch, tmp_path):
    run = tmp_path / "runs" / "run-fixture"
    output = run / "_working"
    prepared = pd.DataFrame(index=pd.bdate_range("2026-01-01", periods=12))
    loads = 0

    def prepare(*args, **kwargs):
        nonlocal loads
        loads += 1
        return prepared, ["AAA", "BBB"], ["AAA"], {"AAA": "XNYS"}

    monkeypatch.setattr("rstock.application.workflows._prepared_inputs", prepare)
    monkeypatch.setattr(
        "rstock.application.workflows.run_streamed_walk_forward",
        lambda *args, **kwargs: (_ for _ in ()).throw(RuntimeError("walk crash")),
    )
    with pytest.raises(RuntimeError, match="walk crash"):
        _resumable_walk_forward(_spec(tmp_path, permutation_depth=1), output, None, None)

    monkeypatch.setattr(
        "rstock.application.workflows._prepared_inputs",
        lambda *args, **kwargs: pytest.fail("prepared snapshot was not reused"),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.generate_symbol_sets",
        lambda *args, **kwargs: pytest.fail("generated-set checkpoint was not reused"),
    )

    def resumed(*args, **kwargs):
        output.mkdir(parents=True, exist_ok=True)
        return SimpleNamespace(
            aggregate_global=pd.DataFrame([{"Sets": 1}]),
            qualification=pd.DataFrame([{"Eligible": True}]),
            run_configuration={},
            telemetry={},
        )

    monkeypatch.setattr(
        "rstock.application.workflows.run_streamed_walk_forward", resumed
    )
    summary = _resumable_walk_forward(
        _spec(tmp_path, permutation_depth=1), output, None, None
    )

    assert loads == 1
    assert summary["eligible_combinations"] == 1


def test_services_import_without_streamlit(monkeypatch):
    monkeypatch.setitem(sys.modules, "streamlit", None)
    module = importlib.import_module("rstock.application.services")
    importlib.reload(module)
    assert module.ExperimentService is not None
