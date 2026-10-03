import json
from collections import Counter
from dataclasses import replace

import pandas as pd
import pytest

import rstock.application.end_to_end as end_to_end
import rstock.application.worker as worker_module
from rstock.application.auto_promotion import AutoPromotionRunner, PROMOTION_CHECKPOINT
from rstock.application.promotion_pipeline import (
    PROMOTION_TRIGGER_CHECKPOINT, PromotionTriggerPolicy,
)
from rstock.application.domain import (
    ExperimentSpec,
    JobStatus,
    JobType,
    RunMetadata,
    RunPurpose,
    RunRole,
)
from rstock.application.end_to_end import (
    CHILD_ID_POLICY_RESERVED,
    PIPELINE_MANIFEST,
    SCIENTIFIC_STAGES,
    TEMPORAL_VALIDATION_STAGE,
    build_pipeline_manifest,
    load_pipeline_manifest,
    persist_or_validate_pipeline_manifest,
)
from rstock.application.repository import RunRepository
from rstock.application.forced_candidate_validation import (
    load_forced_validation_manifest,
)
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import PromotionService
from rstock.application.runner import RunService
from rstock.application.worker import execute_run
from rstock.application.workflows import (
    WorkflowRegistry,
    _end_to_end,
    _forced_candidate_validation,
    _promotion_qualification,
)
from rstock.config import DEFAULT_CONFIG
from rstock.progress import check_cancellation


def _spec(tmp_path, **values):
    pipeline_version = values.pop("pipeline_version", 2)
    return ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
        combinations_per_target=1,
        pipeline_version=pipeline_version,
        **values,
    )


def _write_json(path, values):
    path.write_text(json.dumps(values), encoding="utf-8")


def test_historical_forced_spec_without_period_lock_keeps_legacy_contract(tmp_path):
    raw = _spec(tmp_path).to_dict()
    raw.pop("forced_period_lock", None)
    assert ExperimentSpec.from_dict(raw).forced_period_lock is None
    raw["forced_period_lock"] = {"schema_version": 1, "effective_cutoff": "2026-06-01"}
    assert ExperimentSpec.from_dict(raw).forced_period_lock == raw["forced_period_lock"]


def test_split_forced_validation_without_candidates_persists_period_lock(
    tmp_path, monkeypatch,
):
    from rstock.application import forced_candidate_validation as forced_module

    repository = RunRepository(tmp_path / "runs")
    lock = {"schema_version": 1, "temporal_run_id": "temporal-source"}
    spec = replace(
        _spec(tmp_path), job_type=JobType.FORCED_CANDIDATE_VALIDATION,
        forced_symbol_sets=(), forced_candidate_identities=(), forced_period_lock=lock,
    )
    run_id = repository.create(spec)
    monkeypatch.setattr(forced_module, "build_forced_period_lock", lambda *args: lock)
    for _ in range(2):
        summary = forced_module.run_forced_candidate_validation(
            spec, repository.run_directory(run_id) / "results", None, None,
            execute_reserved_child=lambda *args: pytest.fail("unexpected child"),
            phase_callback=lambda *args, **kwargs: None,
        )
        assert summary["candidate_count"] == 0
        manifest = load_forced_validation_manifest(repository, run_id)
        assert manifest["period_lock"] == lock
        assert manifest["child_id_policy_version"] == CHILD_ID_POLICY_RESERVED
        assert manifest["stages"] == []
    parent = _spec(
        tmp_path, pipeline_version=3, temporal_validation_enabled=True,
        auto_promote_candidates=True,
    )
    parent_id = repository.create(parent)
    decision = PromotionTriggerPolicy(repository, parent_id).decide(
        parent, {"schema_version": 3, "stages": []},
        {"final_status": "passed"}, run_id,
    )
    assert decision["authorized"] is False
    assert decision["reason"] == "no_reference_candidates"
    assert decision["promotion_qualification_run_id"] is None


def test_end_to_end_child_preserves_frozen_walk_forward_window_geometry(tmp_path):
    parent = replace(
        _spec(tmp_path),
        config=replace(
            DEFAULT_CONFIG,
            project_root=tmp_path,
            walk_forward_window_mode="rolling",
            walk_forward_train_size=504,
        ),
    )

    child = end_to_end._base_child_spec(
        parent,
        root_run_id="root-run",
        job_type=JobType.WALK_FORWARD,
    )

    assert child.config.walk_forward_window_mode == "rolling"
    assert child.config.walk_forward_train_size == 504

    temporal = end_to_end._temporal_validation_spec(parent)
    assert temporal.config.walk_forward_window_mode == "rolling"
    assert temporal.config.walk_forward_train_size == 504


@pytest.mark.parametrize("explicit_cutoff", [None, "2026-09-24"])
def test_end_to_end_freezes_the_same_market_request_window_for_descendants(
    tmp_path, monkeypatch, explicit_cutoff
):
    from datetime import date, timedelta

    from rstock.application import workflows
    from rstock.checkpoints import CheckpointManager
    from rstock.data import prefix_symbol_columns
    from rstock.market_cache import MarketDataResult
    from rstock.traceability import prepared_dataset_hash

    class FixedDate(date):
        @classmethod
        def today(cls):
            return cls(2026, 9, 26)

    monkeypatch.setattr(end_to_end, "date", FixedDate)
    calls = []
    dates = pd.date_range("2026-09-10", "2026-09-25")
    prices = pd.DataFrame({
        "Open": range(100, 116), "High": range(101, 117),
        "Low": range(99, 115), "Close": range(100, 116),
    }, index=dates)

    class FakeMarketDataService:
        def load(self, spec, *, as_of=None, **kwargs):
            calls.append(as_of)
            end = as_of or FixedDate.today()
            start = end - timedelta(days=spec.config.model_history_days)
            selected = prices.loc[
                (prices.index.date >= start) & (prices.index.date <= end)
            ]
            return MarketDataResult(
                prices=pd.concat([
                    prefix_symbol_columns(selected, symbol)
                    for symbol in ("AAA", "BBB")
                ], axis=1),
                symbols=["AAA", "BBB"], failed_symbols=[], events=[],
            ), {"AAA": "XNYS", "BBB": "XNYS"}

    monkeypatch.setattr(workflows, "MarketDataService", FakeMarketDataService)
    repository = RunRepository(tmp_path / "runs")
    parent = replace(
        _spec(tmp_path, pipeline_version=3, historical_data_cutoff=explicit_cutoff),
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, model_history_days=10),
    )
    root_id = repository.create(parent)
    manifest = build_pipeline_manifest(repository, root_id, parent)
    expected_as_of = explicit_cutoff or "2026-09-26"
    assert manifest["prepared_dataset_as_of"] == expected_as_of
    (repository.run_directory(root_id) / "orchestration").mkdir()
    repository.write_json(root_id, PIPELINE_MANIFEST, manifest)
    monkeypatch.setattr(
        end_to_end, "date",
        type("NextDate", (date,), {"today": classmethod(lambda cls: cls(2026, 9, 27))}),
    )
    manifest = persist_or_validate_pipeline_manifest(repository, root_id, parent)
    assert manifest["prepared_dataset_as_of"] == expected_as_of

    source = end_to_end.build_stage_spec(
        repository, root_id, parent, "walk_forward", manifest
    )
    assert source.historical_data_cutoff == expected_as_of
    prepared_source, *_ = workflows._prepared_inputs(source, None, None)
    digest = prepared_dataset_hash(prepared_source)
    source_id = manifest["stages"][0]["child_run_id"]
    repository.create(source, run_id=source_id)
    CheckpointManager(
        repository.run_directory(source_id), run_id=source_id,
        job_type=JobType.WALK_FORWARD.value,
        configuration_fingerprint=source.fingerprint, batch_sizes={},
    ).commit_snapshot(prepared_source, {
        "predictor_symbols": ["AAA", "BBB"],
        "target_symbols": ["AAA", "BBB"],
        "calendars": {"AAA": "XNYS", "BBB": "XNYS"},
        "effective_end_date": prepared_source.attrs.get("effective_end_date"),
    })
    repository.write_json(source_id, "summary.json", {
        "traceability": {
            "prepared_market_last_date": prepared_source.index.max().isoformat(),
            "prepared_dataset_sha256": digest,
        }
    })
    source_status = repository.status(source_id)
    source_status["status"] = "completed"
    repository.write_json(source_id, "status.json", source_status)
    prices.loc[prices.index.max(), "Close"] = -999.0

    descendant = end_to_end.build_stage_spec(
        repository, root_id, parent, "xgboost_calibration", manifest
    )
    assert descendant.historical_data_cutoff == expected_as_of
    assert descendant.prepared_snapshot_required
    assert descendant.source_prepared_dataset_sha256 == digest
    prepared_descendant, *_ = workflows._prepared_inputs(descendant, None, None)
    assert prepared_descendant.index.equals(prepared_source.index)
    assert prepared_dataset_hash(prepared_descendant) == digest
    assert calls == [date.fromisoformat(expected_as_of)]

    xgboost_id = manifest["stages"][1]["child_run_id"]
    (repository.run_directory(xgboost_id) / "results").mkdir(parents=True)
    repository.write_json(xgboost_id, "results/selected_configurations.json", {
        "Up": {"parameters": {"max_depth": 2, "eta": 0.05, "num_boost_round": 20}},
        "Down": {"parameters": {"max_depth": 2, "eta": 0.05, "num_boost_round": 20}},
    })
    threshold_parameters = end_to_end.build_stage_spec(
        repository, root_id, parent, "threshold_parameter_calibration", manifest
    )
    assert threshold_parameters.historical_data_cutoff == expected_as_of
    assert threshold_parameters.prepared_snapshot_required
    assert threshold_parameters.source_prepared_dataset_sha256 == digest
    assert prepared_dataset_hash(workflows._prepared_inputs(threshold_parameters, None, None)[0]) == digest

    threshold_parameters_id = manifest["stages"][2]["child_run_id"]
    (repository.run_directory(threshold_parameters_id) / "results").mkdir(parents=True)
    repository.write_json(
        threshold_parameters_id,
        "results/selected_threshold_calibration_configuration.json",
        {"parameters": {"threshold_calibration_min_robust_signals": 20}},
    )
    threshold = end_to_end.build_stage_spec(
        repository, root_id, parent, "threshold_calibration", manifest
    )
    assert threshold.historical_data_cutoff == expected_as_of
    assert threshold.prepared_snapshot_required
    assert threshold.source_prepared_dataset_sha256 == digest
    assert prepared_dataset_hash(workflows._prepared_inputs(threshold, None, None)[0]) == digest

    threshold_id = manifest["stages"][3]["child_run_id"]
    (repository.run_directory(threshold_id) / "results").mkdir(parents=True)
    repository.write_json(threshold_id, "results/selected_thresholds_by_set.json", {})
    holdout = end_to_end.build_stage_spec(
        repository, root_id, parent, "holdout_evaluation", manifest
    )
    assert holdout.prepared_snapshot_required
    assert holdout.source_prepared_dataset_sha256 == digest
    assert prepared_dataset_hash(workflows._prepared_inputs(holdout, None, None)[0]) == digest
    assert calls == [date.fromisoformat(expected_as_of)]


def test_walk_forward_refuses_incomplete_latest_session_before_snapshot(tmp_path, monkeypatch):
    from rstock.application import workflows
    from rstock.data import prefix_symbol_columns
    from rstock.market_cache import MarketDataResult

    dates = pd.to_datetime(["2026-10-01", "2026-10-02"])
    prices = pd.DataFrame({
        "Open": [100.0, 101.0], "High": [101.0, 102.0],
        "Low": [99.0, 100.0], "Close": [100.5, 101.5],
    }, index=dates)
    incomplete = prices.copy()
    incomplete.loc[dates[-1], "Close"] = float("nan")

    class FakeMarketDataService:
        def load(self, spec, **kwargs):
            return MarketDataResult(
                prices=pd.concat([
                    prefix_symbol_columns(prices, "AAA"),
                    prefix_symbol_columns(incomplete, "BBB"),
                ], axis=1),
                symbols=["AAA", "BBB"], failed_symbols=[], events=[],
            ), {"AAA": "XNYS", "BBB": "XNYS"}

    monkeypatch.setattr(workflows, "MarketDataService", FakeMarketDataService)
    spec = end_to_end._base_child_spec(
        _spec(tmp_path, historical_data_cutoff="2026-10-02"),
        root_run_id="root-run", job_type=JobType.WALK_FORWARD,
    )
    with pytest.raises(ValueError, match="Dernière séance marché incomplète.*BBB"):
        workflows._prepared_inputs(spec, None, None)


def test_standalone_calibration_still_prepares_market_data(tmp_path, monkeypatch):
    from rstock.application import workflows
    from rstock.data import prefix_symbol_columns
    from rstock.market_cache import MarketDataResult

    calls = []
    prices = pd.DataFrame({
        "Open": [100.0, 101.0], "High": [101.0, 102.0],
        "Low": [99.0, 100.0], "Close": [100.5, 101.5],
    }, index=pd.to_datetime(["2026-10-01", "2026-10-02"]))

    class FakeMarketDataService:
        def load(self, spec, **kwargs):
            calls.append(spec.job_type)
            return MarketDataResult(
                prices=pd.concat([
                    prefix_symbol_columns(prices, symbol)
                    for symbol in ("AAA", "BBB")
                ], axis=1),
                symbols=["AAA", "BBB"], failed_symbols=[], events=[],
            ), {"AAA": "XNYS", "BBB": "XNYS"}

    monkeypatch.setattr(workflows, "MarketDataService", FakeMarketDataService)
    spec = ExperimentSpec(
        job_type=JobType.XGBOOST_CALIBRATION,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"), target_symbols=("AAA", "BBB"), context_symbols=(),
        combinations_per_target=1, historical_data_cutoff="2026-10-02",
    )
    prepared, *_ = workflows._prepared_inputs(spec, None, None)
    assert calls == [JobType.XGBOOST_CALIBRATION]
    assert prepared.index.max() == pd.Timestamp("2026-10-02")


def test_existing_legacy_child_retains_its_persisted_preparation_contract(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent = _spec(tmp_path, pipeline_version=3)
    root_id = repository.create(parent)
    manifest = build_pipeline_manifest(repository, root_id, parent)
    walk_id = manifest["stages"][0]["child_run_id"]
    repository.run_directory(walk_id).mkdir()
    repository.write_json(walk_id, "summary.json", {
        "traceability": {
            "prepared_market_last_date": "2026-10-02T00:00:00",
            "prepared_dataset_sha256": "a" * 64,
        },
    })
    new_spec = end_to_end.build_stage_spec(
        repository, root_id, parent, "xgboost_calibration", manifest
    )
    assert new_spec.prepared_snapshot_required
    old_spec = replace(new_spec, prepared_snapshot_required=False)
    child_id = manifest["stages"][1]["child_run_id"]
    repository.create(old_spec, run_id=child_id)

    resumed = end_to_end.build_stage_spec(
        repository, root_id, parent, "xgboost_calibration", manifest
    )
    assert resumed.fingerprint == old_spec.fingerprint
    assert not resumed.prepared_snapshot_required


def _fake_registry(
    repository,
    calls,
    fail_once=None,
    *,
    cancel_on=None,
    promotion_rows=None,
):
    failed = set()

    def handler(job_type):
        def run(spec, output, progress_callback, cancellation_check):
            calls[job_type] += 1
            if cancel_on is job_type:
                repository.request_cancellation(str(spec.source_end_to_end_run))
                check_cancellation(cancellation_check)
            if fail_once is job_type and job_type not in failed:
                failed.add(job_type)
                raise RuntimeError(f"injected failure: {job_type.value}")
            output.mkdir(parents=True, exist_ok=True)
            if job_type is JobType.WALK_FORWARD:
                rows = promotion_rows or [
                    {
                        "Set": "AAA<-BBB",
                        "Observation": "AAA",
                        "Predictors": '["BBB"]',
                    }
                ]
                pd.DataFrame(
                    [
                        {
                            "Set": row["Set"],
                            "Observation": row.get(
                                "Observation", row["Set"].split("<-", 1)[0]
                            ),
                            "Predictors": row.get(
                                "Predictors"
                            ) or json.dumps(row["Set"].split("<-", 1)[1].split("+")),
                            "Eligible": True,
                            "ROCAUCMedian": 0.65,
                        }
                        for row in rows
                    ]
                ).to_csv(output / "qualification.csv", index=False)
                _write_json(output / "run_configuration.json", {"job_type": "walk_forward"})
                return {
                    "job_type": job_type.value,
                    "traceability": {
                        "prepared_market_last_date": "2026-09-15T00:00:00",
                        "prepared_dataset_sha256": "d" * 64,
                    },
                }
            if job_type is JobType.XGBOOST_CALIBRATION:
                selected = {
                    "Up": {
                        "parameters": {
                            "max_depth": 2,
                            "eta": 0.05,
                            "num_boost_round": 20,
                        }
                    },
                    "Down": {
                        "parameters": {
                            "max_depth": 3,
                            "eta": 0.05,
                            "num_boost_round": 20,
                        }
                    },
                }
                _write_json(output / "selected_configurations.json", selected)
                _write_json(output / "sampling_manifest.json", {"policy_version": 2})
            elif job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION:
                _write_json(
                    output / "selected_threshold_calibration_configuration.json",
                    {
                        "parameters": {
                            "threshold_calibration_min_signals_per_window": 5,
                            "threshold_calibration_quantiles": [0.5, 0.75],
                        }
                    },
                )
                _write_json(output / "sampling_manifest.json", {"policy_version": 2})
            else:
                rows = promotion_rows or []
                _write_json(
                    output / "selected_thresholds_by_set.json",
                    {
                        row["Set"]: {
                            "Up": {"status": "selected", "threshold": 0.62},
                            "Down": {"status": "selected", "threshold": 0.38},
                        }
                        for row in rows
                    },
                )
                if rows:
                    pd.DataFrame(
                        [
                            {
                                "Set": row["Set"],
                                "Observation": row.get(
                                    "Observation", row["Set"].split("<-", 1)[0]
                                ),
                                "Direction": "Up",
                                "Threshold": 0.62,
                                "SignalCount": row.get("SignalCount", 20),
                                "ROCAUC": row.get("ROCAUC", 0.60),
                                "Precision": row.get("Precision", 0.40),
                                "DirectionalReturnMean": row.get(
                                    "DirectionalReturnMean", 0.01
                                ),
                                "OppositeMoveFrequency": row.get(
                                    "OppositeMoveFrequency", 0.30
                                ),
                            }
                            for row in rows
                        ]
                    ).to_csv(output / "holdout_metrics.csv", index=False)
                _write_json(output / "run_configuration.json", {"outcome": "completed"})
            return {"job_type": job_type.value}

        return run

    return WorkflowRegistry(
        {
            JobType.END_TO_END: _end_to_end,
            JobType.FORCED_CANDIDATE_VALIDATION: _forced_candidate_validation,
            JobType.FIXED_CANDIDATE_EVALUATION: handler(
                JobType.FIXED_CANDIDATE_EVALUATION
            ),
            **{
                job_type: handler(job_type)
                for _, job_type, _ in SCIENTIFIC_STAGES
            },
        }
    )


def _create_parent(repository, spec):
    return repository.create(
        spec,
        metadata=RunMetadata(
            run_role=RunRole.PIPELINE_PARENT,
            run_purpose=(
                RunPurpose.REFERENCE
                if spec.temporal_validation_enabled
                else RunPurpose.STANDARD
            ),
        ),
    )


def _resume(repository, run_id, registry):
    status = repository.prepare_resume(run_id)
    status["resume_requested"] = True
    repository.write_json(run_id, "status.json", status)
    execute_run(repository, run_id, 1, registry=registry)


@pytest.mark.parametrize(
    "failed_job_type",
    [
        JobType.WALK_FORWARD,
        JobType.XGBOOST_CALIBRATION,
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.THRESHOLD_CALIBRATION,
    ],
)
def test_end_to_end_resume_reuses_completed_stages_and_failed_child_id(
    tmp_path, failed_job_type
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))
    calls = Counter()
    registry = _fake_registry(repository, calls, fail_once=failed_job_type)

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.FAILED.value
    manifest_before = load_pipeline_manifest(repository, run_id)
    assert manifest_before is not None
    reserved_ids = {
        item["stage_key"]: item["child_run_id"]
        for item in manifest_before["stages"]
        if item["child_run_id"] is not None
    }
    failed_stage = next(
        key
        for key, job_type, _ in SCIENTIFIC_STAGES
        if job_type is failed_job_type
    )
    assert repository.status(reserved_ids[failed_stage])["status"] == "failed"
    assert reserved_ids[failed_stage] in repository.status(run_id)["error"]

    _resume(repository, run_id, registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    manifest_after = load_pipeline_manifest(repository, run_id)
    assert manifest_after is not None
    assert {
        item["stage_key"]: item["child_run_id"]
        for item in manifest_after["stages"]
        if item["child_run_id"] is not None
    } == reserved_ids
    failed_index = [item[1] for item in SCIENTIFIC_STAGES].index(failed_job_type)
    for index, (_, job_type, _) in enumerate(SCIENTIFIC_STAGES):
        assert calls[job_type] == (2 if index == failed_index else 1)
    assert all(
        item["expected_fingerprint"] and item["artifact_digests"]
        for item in manifest_after["stages"][:-1]
    )
    assert all(
        repository.run_metadata(child_id).run_role is RunRole.PIPELINE_STAGE
        and repository.run_metadata(child_id).visible_in_history
        for child_id in reserved_ids.values()
    )


def test_end_to_end_reserves_every_child_before_materializing_the_first(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))
    registry = _fake_registry(
        repository, Counter(), fail_once=JobType.WALK_FORWARD
    )

    execute_run(repository, run_id, 1, registry=registry)

    manifest = repository.read_json(run_id, PIPELINE_MANIFEST)
    child_ids = [item["child_run_id"] for item in manifest["stages"][:-1]]
    assert all(child_ids)
    assert repository.run_directory(child_ids[0]).exists()
    assert all(not repository.run_directory(item).exists() for item in child_ids[1:])


def test_end_to_end_temporal_validation_runs_a_real_child_pipeline(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path, temporal_validation_enabled=True))
    calls = Counter()
    execute_run(repository, run_id, 1, registry=_fake_registry(repository, calls))

    manifest = load_pipeline_manifest(repository, run_id)
    temporal = next(
        item for item in manifest["stages"]
        if item["stage_key"] == TEMPORAL_VALIDATION_STAGE
    )
    child_id = temporal["child_run_id"]
    child_spec = repository.load_spec(child_id)
    child_metadata = repository.run_metadata(child_id)
    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    assert repository.status(child_id)["status"] == JobStatus.COMPLETED.value
    assert child_spec.job_type is JobType.END_TO_END
    assert child_spec.config.walk_forward_end_offset_sessions == 63
    assert child_spec.temporal_validation_enabled is False
    assert child_spec.auto_promote_candidates is False
    assert child_metadata.run_purpose is RunPurpose.TEMPORAL_VALIDATION
    assert child_metadata.visible_in_history is True
    assert child_metadata.reference_run_id == run_id
    assert load_pipeline_manifest(repository, child_id)["temporal_validation_enabled"] is False


def test_three_pass_child_freezes_exact_reference_candidates_and_relations(monkeypatch, tmp_path):
    class FakeTemporalValidationRunner:
        def __init__(self, *_args, **_kwargs):
            pass

        def execute(self):
            return {"final_status": "passed"}

    monkeypatch.setattr(end_to_end, "TemporalValidationRunner", FakeTemporalValidationRunner)
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path, temporal_validation_enabled=True))
    rows = [
        {"Set": '["DDOG","REGN"]', "Observation": "DDOG", "Predictors": '["REGN"]'},
        {"Set": '["VLO","TMO","MMM"]', "Observation": "VLO", "Predictors": '["TMO","MMM"]'},
    ]
    calls = Counter()
    execute_run(
        repository,
        run_id,
        1,
        registry=_fake_registry(repository, calls, promotion_rows=rows),
    )

    manifest = load_pipeline_manifest(repository, run_id)
    temporal_id = next(item["child_run_id"] for item in manifest["stages"] if item["stage_key"] == TEMPORAL_VALIDATION_STAGE)
    forced_id = next(item["child_run_id"] for item in manifest["stages"] if item["stage_key"] == end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE)
    forced_spec = repository.load_spec(forced_id)
    metadata = repository.run_metadata(forced_id)
    assert forced_spec.job_type is JobType.FORCED_CANDIDATE_VALIDATION
    assert forced_spec.forced_symbol_sets == (("DDOG", "REGN"), ("VLO", "TMO", "MMM"))
    assert forced_spec.forced_candidate_identities == (
        ('["DDOG","REGN"]', "Up"),
        ('["VLO","TMO","MMM"]', "Up"),
    )
    assert forced_spec.frozen_xgboost_parameters["Up"]["max_depth"] == 2
    assert forced_spec.frozen_xgboost_parameters["Down"]["max_depth"] == 3
    assert forced_spec.frozen_xgboost_parameters["Up"]["eta"] == 0.05
    assert forced_spec.frozen_xgboost_parameters["Up"]["num_boost_round"] == 20
    assert forced_spec.frozen_selected_thresholds_by_set == {
        '["DDOG","REGN"]': {
            "Up": {"status": "selected", "threshold": 0.62},
            "Down": {"status": "selected", "threshold": 0.38},
        },
        '["VLO","TMO","MMM"]': {
            "Up": {"status": "selected", "threshold": 0.62},
            "Down": {"status": "selected", "threshold": 0.38},
        },
    }
    assert forced_spec.config.walk_forward_end_offset_sessions == 63
    assert metadata.run_role is RunRole.FORCED_CANDIDATE_VALIDATION
    assert metadata.run_purpose is RunPurpose.FORCED_CANDIDATE_VALIDATION
    assert metadata.parent_run_id == run_id
    assert metadata.reference_run_id == run_id
    assert metadata.validation_run_id == temporal_id
    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    forced_manifest = load_forced_validation_manifest(repository, forced_id)
    assert [item["stage_key"] for item in forced_manifest["stages"]] == [
        "walk_forward",
        "fixed_candidate_evaluation",
    ]
    assert calls[JobType.XGBOOST_CALIBRATION] == 2
    assert calls[JobType.THRESHOLD_PARAMETER_CALIBRATION] == 2
    assert calls[JobType.THRESHOLD_CALIBRATION] == 2
    assert calls[JobType.FIXED_CANDIDATE_EVALUATION] == 1
    fixed_id = next(
        item["child_run_id"]
        for item in forced_manifest["stages"]
        if item["stage_key"] == "fixed_candidate_evaluation"
    )
    fixed_spec = repository.load_spec(fixed_id)
    assert fixed_spec.frozen_xgboost_parameters == (
        forced_spec.frozen_xgboost_parameters
    )
    assert fixed_spec.frozen_selected_thresholds_by_set == (
        forced_spec.frozen_selected_thresholds_by_set
    )
    assert fixed_spec.config.walk_forward_end_offset_sessions == 63


def test_three_pass_resume_reuses_forced_walk_forward_after_fixed_evaluation_failure(
    monkeypatch, tmp_path
):
    class FakeTemporalValidationRunner:
        def __init__(self, *_args, **_kwargs):
            pass

        def execute(self):
            return {"final_status": "passed"}

    monkeypatch.setattr(end_to_end, "TemporalValidationRunner", FakeTemporalValidationRunner)
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository, _spec(tmp_path, temporal_validation_enabled=True)
    )
    calls = Counter()
    registry = _fake_registry(
        repository,
        calls,
        fail_once=JobType.FIXED_CANDIDATE_EVALUATION,
        promotion_rows=[
            {
                "Set": '["AAA","BBB"]',
                "Observation": "AAA",
                "Predictors": '["BBB"]',
            }
        ],
    )

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.FAILED.value
    outer = load_pipeline_manifest(repository, run_id)
    forced_id = next(
        item["child_run_id"]
        for item in outer["stages"]
        if item["stage_key"] == end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE
    )
    forced_manifest = load_forced_validation_manifest(repository, forced_id)
    forced_walk_id = next(
        item["child_run_id"]
        for item in forced_manifest["stages"]
        if item["stage_key"] == "walk_forward"
    )
    fixed_id = next(
        item["child_run_id"]
        for item in forced_manifest["stages"]
        if item["stage_key"] == "fixed_candidate_evaluation"
    )
    assert repository.status(forced_walk_id)["status"] == "completed"
    assert repository.status(fixed_id)["status"] == "failed"

    _resume(repository, run_id, registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    assert calls[JobType.WALK_FORWARD] == 3
    assert calls[JobType.XGBOOST_CALIBRATION] == 2
    assert calls[JobType.THRESHOLD_PARAMETER_CALIBRATION] == 2
    assert calls[JobType.THRESHOLD_CALIBRATION] == 2
    assert calls[JobType.FIXED_CANDIDATE_EVALUATION] == 2


def test_historical_pipeline_v1_keeps_two_pass_shape(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    spec = replace(_spec(tmp_path, temporal_validation_enabled=True), pipeline_version=1)
    run_id = _create_parent(repository, spec)
    manifest = build_pipeline_manifest(repository, run_id, spec)
    keys = [item["stage_key"] for item in manifest["stages"]]
    assert end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE not in keys
    assert manifest["stages"][-1]["dependency_run_ids"] == [
        next(item["child_run_id"] for item in manifest["stages"] if item["stage_key"] == "threshold_calibration")
    ]


def test_temporal_validation_rejects_nonzero_reference_offset_and_allows_auto_promotion(tmp_path):
    with pytest.raises(ValueError, match="offset 0"):
        replace(
            _spec(tmp_path, temporal_validation_enabled=True),
            config=replace(
                DEFAULT_CONFIG,
                project_root=tmp_path,
                walk_forward_end_offset_sessions=1,
            ),
        )
    spec = _spec(
        tmp_path,
        temporal_validation_enabled=True,
        auto_promote_candidates=True,
    )
    assert spec.auto_promote_candidates is True


@pytest.mark.parametrize("comparison_status", ["failed", "inconclusive", "invalid"])
def test_temporal_validation_blocks_promotion_unless_comparison_passes(
    monkeypatch, tmp_path, comparison_status
):
    class FakeTemporalValidationRunner:
        def __init__(self, *_args, **_kwargs):
            pass

        def execute(self):
            return {"final_status": comparison_status}

    monkeypatch.setattr(end_to_end, "TemporalValidationRunner", FakeTemporalValidationRunner)
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository,
        _spec(
            tmp_path,
            temporal_validation_enabled=True,
            auto_promote_candidates=True,
        ),
    )
    registry = _fake_registry(
        repository, Counter(), promotion_rows=[{"Set": "AAA<-BBB"}]
    )

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    assert not (repository.run_directory(run_id) / PROMOTION_CHECKPOINT).exists()
    blocked = repository.read_json(run_id, PROMOTION_TRIGGER_CHECKPOINT)
    assert blocked["authorized"] is False
    assert blocked["reason"] == "temporal_validation_not_passed"
    promotion_stage = RunService(repository).get(run_id)["pipeline_stages"][-1]
    assert promotion_stage["status"] == "blocked"
    assert promotion_stage["promotion_trigger"] == blocked
    assert ProductionRepository(tmp_path).models() == []


def test_temporal_validation_passed_promotes_forced_revalidation_only(monkeypatch, tmp_path):
    class FakeTemporalValidationRunner:
        def __init__(self, *_args, **_kwargs):
            pass

        def execute(self):
            return {"final_status": "passed"}

    monkeypatch.setattr(end_to_end, "TemporalValidationRunner", FakeTemporalValidationRunner)
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository,
        _spec(
            tmp_path,
            temporal_validation_enabled=True,
            auto_promote_candidates=True,
        ),
    )
    execute_run(
        repository,
        run_id,
        1,
        registry=_fake_registry(
            repository, Counter(), promotion_rows=[{"Set": "AAA<-BBB"}]
        ),
    )

    models = ProductionRepository(tmp_path).models()
    assert len(models) == 1
    threshold_parent = repository.run_metadata(
        models[0].source_threshold_calibration_run
    ).parent_run_id
    forced = next(
        item for item in load_pipeline_manifest(repository, run_id)["stages"]
        if item["stage_key"] == end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE
    )
    reference_xgboost = next(
        item["child_run_id"]
        for item in load_pipeline_manifest(repository, run_id)["stages"]
        if item["stage_key"] == "xgboost_calibration"
    )
    assert threshold_parent == forced["child_run_id"]
    assert models[0].source_xgboost_calibration_run == reference_xgboost
    assert models[0].up_threshold == 0.62
    assert models[0].training_metadata["validation_provenance"] == {
        "reference_run_id": run_id,
        "temporal_validation_run_id": next(
            item["child_run_id"] for item in load_pipeline_manifest(repository, run_id)["stages"]
            if item["stage_key"] == TEMPORAL_VALIDATION_STAGE
        ),
        "forced_candidate_validation_run_id": forced["child_run_id"],
        "promotion_policy_version": 1,
    }
    assert (repository.run_directory(run_id) / PROMOTION_CHECKPOINT).is_file()


def test_pre_split_three_pass_pipeline_reference_contract(tmp_path):
    """Persist the composite pipeline's observable scientific and promotion contract.

    Scientific workers emit fixed small artifacts; the real End-to-End,
    temporal comparison, forced runner, candidate guidance and promotion run.
    The numerical calibration contract has a separate deterministic test.
    """
    repository = RunRepository(tmp_path / "runs")
    root_id = _create_parent(
        repository,
        _spec(tmp_path, temporal_validation_enabled=True, auto_promote_candidates=True),
    )
    rows = [
        {"Set": "AAA<-BBB", "ROCAUC": 0.65, "Precision": 0.65,
         "DirectionalReturnMean": 0.02, "OppositeMoveFrequency": 0.20},
        {"Set": "BBB<-AAA", "ROCAUC": 0.59, "Precision": 0.65,
         "DirectionalReturnMean": 0.02, "OppositeMoveFrequency": 0.20},
    ]
    calls = Counter()
    registry = _fake_registry(repository, calls, promotion_rows=rows)
    for job_type in (JobType.THRESHOLD_CALIBRATION, JobType.FIXED_CANDIDATE_EVALUATION):
        original = registry.handlers[job_type]

        def with_temporal_artifacts(spec, output, progress, cancellation, *, original=original):
            summary = original(spec, output, progress, cancellation)
            metrics = pd.read_csv(output / "holdout_metrics.csv")
            if spec.job_type is JobType.FIXED_CANDIDATE_EVALUATION:
                metrics = metrics[metrics["Set"] == "AAA<-BBB"].copy()
                selected_path = output / "selected_thresholds_by_set.json"
                selected = json.loads(selected_path.read_text(encoding="utf-8"))
                _write_json(selected_path, {"AAA<-BBB": selected["AAA<-BBB"]})
            if spec.config.walk_forward_end_offset_sessions:
                metrics.loc[metrics["Set"] == "AAA<-BBB", "ROCAUC"] = 0.64
            metrics.to_csv(output / "holdout_metrics.csv", index=False)
            metrics.to_csv(output / "threshold_metrics_by_set.csv", index=False)
            start = "2025-09-01" if spec.config.walk_forward_end_offset_sessions else "2026-01-01"
            dates = pd.bdate_range(start, periods=40)
            pd.DataFrame([
                {
                    "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up",
                    "Date": day.date().isoformat(), "Probability": 0.8 if index % 2 == 0 else 0.2,
                    "Target": 1 if index % 2 == 0 else 0,
                    "IntradayReturn": 0.02 if index % 2 == 0 else -0.01,
                    "MFE": 0.02, "MAE": -0.01, "Threshold": 0.62,
                    "Prediction": 1 if index % 2 == 0 else 0,
                }
                for index, day in enumerate(dates)
            ]).to_csv(output / "holdout_predictions.csv", index=False)
            return summary

        registry.handlers[job_type] = with_temporal_artifacts

    execute_run(repository, root_id, 1, registry=registry)

    assert repository.status(root_id)["status"] == JobStatus.COMPLETED.value, repository.status(root_id).get("error")
    manifest = load_pipeline_manifest(repository, root_id)
    stages = {item["stage_key"]: item for item in manifest["stages"]}
    assert [item["stage_key"] for item in manifest["stages"]] == [
        "walk_forward", "xgboost_calibration", "threshold_parameter_calibration",
        "threshold_calibration", TEMPORAL_VALIDATION_STAGE,
        end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE, "promotion",
    ]
    reference_threshold_id = stages["threshold_calibration"]["child_run_id"]
    reference_results = repository.run_directory(reference_threshold_id) / "results"
    selected = json.loads((reference_results / "selected_thresholds_by_set.json").read_text())
    assert selected["AAA<-BBB"] == {
        "Up": {"status": "selected", "threshold": 0.62},
        "Down": {"status": "selected", "threshold": 0.38},
    }
    reference_holdout = pd.read_csv(reference_results / "holdout_metrics.csv").set_index("Set")
    assert reference_holdout.loc["AAA<-BBB", ["SignalCount", "ROCAUC", "Precision"]].tolist() == [20, 0.65, 0.65]
    assert reference_holdout.loc["BBB<-AAA", "ROCAUC"] == pytest.approx(0.59)
    _, reference_guidance, reference_candidates = AutoPromotionRunner(
        repository,
        root_run_id=root_id,
        walk_forward_run_id=stages["walk_forward"]["child_run_id"],
        xgboost_calibration_run_id=stages["xgboost_calibration"]["child_run_id"],
        threshold_calibration_run_id=reference_threshold_id,
    ).source_candidates()
    assert reference_candidates == ["AAA<-BBB"]
    assert dict(zip(reference_guidance["Combinaison"], reference_guidance["Statut promotion"])) == {
        "AAA<-BBB": "Candidat", "BBB<-AAA": "Non candidat",
    }
    for stage_key in ("walk_forward", "xgboost_calibration", "threshold_parameter_calibration", "threshold_calibration"):
        stage = stages[stage_key]
        assert stage["expected_fingerprint"] == repository.configuration_fingerprint(stage["child_run_id"])
        assert stage["artifact_digests"]
        assert repository.run_metadata(stage["child_run_id"]).parent_run_id == root_id

    temporal_id = stages[TEMPORAL_VALIDATION_STAGE]["child_run_id"]
    temporal = repository.read_json(root_id, "results/temporal_validation_comparison.json")
    assert temporal["final_status"] == "passed"
    assert {key: gate["status"] for key, gate in temporal["gates"].items()} == {
        "candidate_yield": "passed", "holdout_auc": "passed",
        "precision_edge": "passed", "directional_return": "passed",
    }
    assert temporal["gates"]["holdout_auc"]["metrics"]["validation"]["median"] == pytest.approx(0.615)
    assert temporal["reference_threshold_run_id"] == reference_threshold_id
    assert temporal["source_artifact_digests"]["reference"]["holdout_metrics.csv"]
    temporal_manifest = load_pipeline_manifest(repository, temporal_id)
    temporal_stages = {item["stage_key"]: item for item in temporal_manifest["stages"]}
    _, _, temporal_candidates = AutoPromotionRunner(
        repository,
        root_run_id=temporal_id,
        walk_forward_run_id=temporal_stages["walk_forward"]["child_run_id"],
        xgboost_calibration_run_id=temporal_stages["xgboost_calibration"]["child_run_id"],
        threshold_calibration_run_id=temporal_stages["threshold_calibration"]["child_run_id"],
    ).source_candidates()
    assert temporal_candidates == ["AAA<-BBB"]
    assert repository.run_metadata(temporal_id).reference_run_id == root_id

    forced_id = stages[end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE]["child_run_id"]
    forced_spec = repository.load_spec(forced_id)
    assert forced_spec.forced_candidate_identities == (( '["AAA","BBB"]', "Up"),)
    assert forced_spec.config.walk_forward_end_offset_sessions == 63
    assert forced_spec.frozen_xgboost_parameters["Up"]["max_depth"] == 2
    assert forced_spec.frozen_selected_thresholds_by_set['["AAA","BBB"]']["Up"]["threshold"] == 0.62
    assert forced_spec.frozen_threshold_calibration_parameters["threshold_calibration_min_signals_per_window"] == 5
    assert forced_spec.source_xgboost_calibration_run == stages["xgboost_calibration"]["child_run_id"]
    assert forced_spec.source_threshold_calibration_run == reference_threshold_id
    forced_manifest = load_forced_validation_manifest(repository, forced_id)
    fixed_id = next(item["child_run_id"] for item in forced_manifest["stages"] if item["stage_key"] == "fixed_candidate_evaluation")
    assert repository.status(fixed_id)["status"] == JobStatus.COMPLETED.value
    assert repository.run_metadata(fixed_id).parent_run_id == forced_id

    promotion = repository.read_json(root_id, PROMOTION_CHECKPOINT)
    assert promotion["candidate_sets"] == ["AAA<-BBB"]
    legacy_trigger = repository.read_json(root_id, PROMOTION_TRIGGER_CHECKPOINT)
    assert legacy_trigger["source_selection_reason"] == "legacy_forced_composite"
    assert legacy_trigger["promotion_qualification_run_id"] is None
    assert promotion["candidate_count"] == promotion["created_count"] == 1
    assert {row["Combinaison"]: row["Statut promotion"] for row in promotion["diagnostics"]} == {
        "AAA<-BBB": "Candidat",
    }
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 1
    assert models[0].source_walk_forward_run == next(
        item["child_run_id"] for item in forced_manifest["stages"] if item["stage_key"] == "walk_forward"
    )
    assert models[0].source_xgboost_calibration_run == stages["xgboost_calibration"]["child_run_id"]
    assert models[0].source_threshold_calibration_run == fixed_id
    assert models[0].training_metadata["validation_provenance"] == {
        "reference_run_id": root_id,
        "temporal_validation_run_id": temporal_id,
        "forced_candidate_validation_run_id": forced_id,
        "promotion_policy_version": 1,
    }


def test_split_pipeline_creates_three_distinct_runs_with_equivalent_decisions(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    rows = [
        {"Set": "AAA<-BBB", "ROCAUC": 0.65},
        {"Set": "BBB<-AAA", "ROCAUC": 0.59},
    ]
    legacy_id = _create_parent(repository, _spec(tmp_path))
    legacy_registry = _fake_registry(repository, Counter(), promotion_rows=rows)
    execute_run(repository, legacy_id, 1, registry=legacy_registry)
    legacy_manifest = load_pipeline_manifest(repository, legacy_id)
    legacy_threshold_id = next(
        item["child_run_id"] for item in legacy_manifest["stages"]
        if item["stage_key"] == "threshold_calibration"
    )
    old_results = repository.run_directory(legacy_threshold_id) / "results"
    expected_holdout = pd.read_csv(old_results / "holdout_metrics.csv")
    _, legacy_guidance, legacy_candidates = AutoPromotionRunner(
        repository, root_run_id=legacy_id,
        walk_forward_run_id=legacy_manifest["stages"][0]["child_run_id"],
        xgboost_calibration_run_id=legacy_manifest["stages"][1]["child_run_id"],
        threshold_calibration_run_id=legacy_threshold_id,
    ).source_candidates()

    root_id = _create_parent(repository, _spec(
        tmp_path, pipeline_version=3, auto_promote_candidates=True
    ))
    registry = _fake_registry(repository, Counter(), promotion_rows=rows)
    threshold_handler = registry.handlers[JobType.THRESHOLD_CALIBRATION]

    def split_threshold(spec, output, progress, cancellation):
        assert spec.evaluate_final_holdout is False
        summary = threshold_handler(spec, output, progress, cancellation)
        (output / "holdout_metrics.csv").unlink()
        pd.DataFrame([{"Set": row["Set"], "Direction": "Up"} for row in rows]).to_csv(
            output / "threshold_metrics_by_set.csv", index=False
        )
        pd.DataFrame([{"Observation": "AAA", "Predictors": '["BBB"]'}]).to_csv(
            output / "sampled_combinations.csv", index=False
        )
        return summary

    def split_holdout(spec, output, _progress, _cancellation):
        assert spec.source_threshold_calibration_run
        assert spec.frozen_selected_thresholds_by_set
        output.mkdir(parents=True, exist_ok=True)
        expected_holdout.to_csv(output / "holdout_metrics.csv", index=False)
        _write_json(output / "run_configuration.json", {
            "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        })
        return {"job_type": JobType.HOLDOUT_EVALUATION.value}

    registry.handlers[JobType.THRESHOLD_CALIBRATION] = split_threshold
    registry.handlers[JobType.HOLDOUT_EVALUATION] = split_holdout
    registry.handlers[JobType.PROMOTION_QUALIFICATION] = _promotion_qualification
    execute_run(repository, root_id, 1, registry=registry)

    assert repository.status(root_id)["status"] == JobStatus.COMPLETED.value, repository.status(root_id).get("error")
    manifest = load_pipeline_manifest(repository, root_id)
    assert manifest["schema_version"] == end_to_end.SPLIT_PIPELINE_SCHEMA_VERSION
    assert [item["stage_key"] for item in manifest["stages"][-4:]] == [
        "threshold_calibration", "holdout_evaluation", "promotion_qualification", "promotion",
    ]
    stages = {item["stage_key"]: item for item in manifest["stages"]}
    threshold_id = stages["threshold_calibration"]["child_run_id"]
    holdout_id = stages["holdout_evaluation"]["child_run_id"]
    qualification_id = stages["promotion_qualification"]["child_run_id"]
    assert len({threshold_id, holdout_id, qualification_id}) == 3
    assert stages["holdout_evaluation"]["dependency_run_ids"] == [threshold_id]
    assert stages["promotion_qualification"]["dependency_run_ids"] == [holdout_id]
    assert "promotion_min_holdout_signals" not in stages["holdout_evaluation"]["parameter_contract"]
    assert stages["promotion_qualification"]["parameter_contract"]["promotion_min_holdout_signals"] == 20
    assert stages["threshold_calibration"]["parameter_contract"]["final_holdout_size"] == 63
    assert all(stages[key]["artifact_digests"] for key in (
        "threshold_calibration", "holdout_evaluation", "promotion_qualification"
    ))
    assert not (repository.run_directory(threshold_id) / "results" / "holdout_metrics.csv").exists()
    assert repository.load_spec(holdout_id).source_threshold_calibration_run == threshold_id
    assert repository.load_spec(qualification_id).source_holdout_evaluation_run == holdout_id
    assert all(repository.run_metadata(run_id).parent_run_id == root_id for run_id in (
        threshold_id, holdout_id, qualification_id
    ))
    new_threshold = repository.run_directory(threshold_id) / "results"
    new_holdout = repository.run_directory(holdout_id) / "results"
    assert json.loads((new_threshold / "selected_thresholds_by_set.json").read_text()) == json.loads(
        (old_results / "selected_thresholds_by_set.json").read_text()
    )
    pd.testing.assert_frame_equal(
        pd.read_csv(new_holdout / "holdout_metrics.csv"),
        pd.read_csv(old_results / "holdout_metrics.csv"),
    )
    qualification = repository.read_json(qualification_id, "results/qualification.json")
    assert qualification["candidate_sets"] == legacy_candidates == ["AAA<-BBB"]
    assert qualification["policy_parameters"]["promotion_min_holdout_auc"] == 0.6
    assert all(isinstance(row["reasons"], list) for row in qualification["decisions"])
    assert {row["Combinaison"]: row["Statut promotion"] for row in qualification["decisions"]} == {
        row["Combinaison"]: row["Statut promotion"]
        for _, row in legacy_guidance.iterrows()
    }
    promotion = repository.read_json(root_id, PROMOTION_CHECKPOINT)
    assert promotion["source_promotion_qualification_run"] == qualification_id
    trigger = repository.read_json(root_id, PROMOTION_TRIGGER_CHECKPOINT)
    assert trigger["promotion_qualification_run_id"] == qualification_id
    assert trigger["source_selection_reason"] == "normal_pipeline"
    assert trigger["authorized"] is True
    assert RunService(repository).get(root_id)["pipeline_stages"][-1]["promotion_trigger"] == trigger
    assert promotion["source_holdout_evaluation_run"] == holdout_id
    assert promotion["candidate_sets"] == ["AAA<-BBB"]
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 1
    assert models[0].source_threshold_calibration_run == threshold_id
    assert models[0].training_metadata["source_holdout_evaluation_run"] == holdout_id
    stricter = replace(
        repository.load_spec(qualification_id),
        config=replace(
            repository.load_spec(qualification_id).config,
            promotion_min_holdout_signals=21,
        ),
    )
    _promotion_qualification(stricter, tmp_path / "stricter_qualification", None, None)
    changed = json.loads((tmp_path / "stricter_qualification" / "qualification.json").read_text())
    assert changed["candidate_sets"] == []
    assert changed["source_holdout_evaluation_run"] == holdout_id


@pytest.mark.parametrize("interrupt_fixed", [False, True])
def test_split_pipeline_temporal_comparison_forced_candidates_and_promotion(
    tmp_path, interrupt_fixed,
):
    import pickle
    import exchange_calendars as xcals
    from rstock.checkpoints import CheckpointManager
    from rstock.calendars import offset_market_session
    from rstock.traceability import prepared_dataset_hash
    from rstock.application.forced_period import validate_forced_period
    from rstock.application.workflows import _prepared_inputs

    repository = RunRepository(tmp_path / "runs")
    root_id = _create_parent(repository, _spec(
        tmp_path, pipeline_version=3, temporal_validation_enabled=True,
        auto_promote_candidates=True,
    ))
    registry = _fake_registry(
        repository, Counter(), promotion_rows=[{"Set": "AAA<-BBB", "ROCAUC": 0.65}],
        fail_once=JobType.FIXED_CANDIDATE_EVALUATION if interrupt_fixed else None,
    )
    threshold_handler = registry.handlers[JobType.THRESHOLD_CALIBRATION]
    walk_handler = registry.handlers[JobType.WALK_FORWARD]
    fixed_handler = registry.handlers[JobType.FIXED_CANDIDATE_EVALUATION]

    def period(spec):
        end = offset_market_session("2026-09-15", "XNYS", 63) if spec.config.walk_forward_end_offset_sessions else pd.Timestamp("2026-09-15")
        return pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range(
            "2025-01-01", end
        )[-190:]).tz_localize(None)

    def walk_forward(spec, output, progress, cancellation):
        summary = walk_handler(spec, output, progress, cancellation)
        if spec.forced_period_lock is not None:
            prepared, predictors, targets, calendars = _prepared_inputs(spec, None, None)
            pd.DataFrame([{
                "Set": identity[0], "Observation": "AAA", "Predictors": '["BBB"]',
                "Eligible": True, "ROCAUCMedian": 0.65,
            } for identity in spec.forced_candidate_identities or ()]).to_csv(
                output / "qualification.csv", index=False
            )
        elif spec.pipeline_version >= 3:
            dates = period(spec)
            prepared = pd.DataFrame({"AAA_Close": range(len(dates)),
                                     "BBB_Close": range(len(dates))}, index=dates)
            prepared.attrs["effective_end_date"] = dates[-1].isoformat()
            prepared.attrs["symbols_used"] = 2
            predictors, targets = list(spec.predictor_symbols), list(spec.target_symbols)
            calendars = {"AAA": "XNYS", "BBB": "XNYS"}
        else:
            return summary
        checkpoint = CheckpointManager(
            output.parent, run_id=output.parent.name, job_type=spec.job_type.value,
            configuration_fingerprint=spec.fingerprint,
            batch_sizes={"predictor_prefilter_walk_forward": spec.config.predictor_prefilter_batch_size,
                         "walk_forward": spec.config.walk_forward_batch_size,
                         "final_holdout": spec.config.final_holdout_batch_size},
        )
        checkpoint.commit_snapshot(prepared, {
            "predictor_symbols": predictors, "target_symbols": targets,
            "calendars": calendars,
            "effective_end_date": prepared.attrs["effective_end_date"],
        })
        summary["traceability"] = {
            "prepared_market_last_date": prepared.index.max().isoformat(),
            "prepared_dataset_sha256": prepared_dataset_hash(prepared),
        }
        return summary

    def fixed(spec, output, progress, cancellation):
        result = fixed_handler(spec, output, progress, cancellation)
        if spec.forced_period_lock is not None:
            selected = spec.frozen_selected_thresholds_by_set
            _write_json(output / "selected_thresholds_by_set.json", selected)
            pd.DataFrame([{
                "Set": identity[0], "Observation": "AAA", "Direction": identity[1],
                "Threshold": selected[identity[0]][identity[1]]["threshold"],
                "SignalCount": 20, "ROCAUC": 0.64, "Precision": 0.65,
                "DirectionalReturnMean": 0.02, "OppositeMoveFrequency": 0.2,
            } for identity in spec.forced_candidate_identities or ()]).to_csv(
                output / "holdout_metrics.csv", index=False
            )
            lock = spec.forced_period_lock
            _write_json(output / "run_configuration.json", {
                "development_end": lock["development_end"],
                "final_holdout_start": lock["holdout_first_session"],
                "final_holdout_size": lock["final_holdout_size"],
                "candidate_errors": {},
            })
        return result

    def threshold(spec, output, progress, cancellation):
        summary = threshold_handler(spec, output, progress, cancellation)
        (output / "holdout_metrics.csv").unlink()
        pd.DataFrame([{"Set": "AAA<-BBB", "Direction": "Up"}]).to_csv(
            output / "threshold_metrics_by_set.csv", index=False
        )
        pd.DataFrame([{"Observation": "AAA", "Predictors": '["BBB"]'}]).to_csv(
            output / "sampled_combinations.csv", index=False
        )
        return summary

    def holdout(spec, output, _progress, _cancellation):
        output.mkdir(parents=True, exist_ok=True)
        auc = 0.64 if spec.config.walk_forward_end_offset_sessions else 0.65
        pd.DataFrame([{
            "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up",
            "Threshold": 0.62, "SignalCount": 20, "ROCAUC": auc,
            "Precision": 0.65, "DirectionalReturnMean": 0.02,
            "OppositeMoveFrequency": 0.2,
        }]).to_csv(output / "holdout_metrics.csv", index=False)
        dates = period(spec)[-40:]
        pd.DataFrame([{
            "Set": "AAA<-BBB", "Direction": "Up", "Date": day.date().isoformat(),
            "Target": 1 if index % 2 == 0 else 0,
            "Prediction": 1 if index % 2 == 0 else 0,
            "IntradayReturn": 0.02 if index % 2 == 0 else -0.01,
        } for index, day in enumerate(dates)]).to_csv(
            output / "holdout_predictions.csv", index=False
        )
        _write_json(output / "run_configuration.json", {
            "source_threshold_calibration_run": spec.source_threshold_calibration_run,
        })
        return {"job_type": JobType.HOLDOUT_EVALUATION.value}

    registry.handlers[JobType.THRESHOLD_CALIBRATION] = threshold
    registry.handlers[JobType.WALK_FORWARD] = walk_forward
    registry.handlers[JobType.FIXED_CANDIDATE_EVALUATION] = fixed
    registry.handlers[JobType.HOLDOUT_EVALUATION] = holdout
    registry.handlers[JobType.PROMOTION_QUALIFICATION] = _promotion_qualification
    execute_run(repository, root_id, 1, registry=registry)
    if interrupt_fixed:
        assert repository.status(root_id)["status"] == JobStatus.FAILED.value
        failed_manifest = load_pipeline_manifest(repository, root_id)
        failed_forced_id = next(item["child_run_id"] for item in failed_manifest["stages"]
                                if item["stage_key"] == end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE)
        failed_forced_manifest = load_forced_validation_manifest(repository, failed_forced_id)
        failed_child_ids = [item["child_run_id"] for item in failed_forced_manifest["stages"]]
        assert repository.status(failed_child_ids[0])["status"] == "completed"
        assert repository.status(failed_child_ids[1])["status"] == "failed"
        _resume(repository, root_id, registry)
        resumed_manifest = load_forced_validation_manifest(repository, failed_forced_id)
        assert [item["child_run_id"] for item in resumed_manifest["stages"]] == failed_child_ids

    assert repository.status(root_id)["status"] == JobStatus.COMPLETED.value, repository.status(root_id).get("error")
    manifest = load_pipeline_manifest(repository, root_id)
    stages = {item["stage_key"]: item for item in manifest["stages"]}
    temporal_id = stages[TEMPORAL_VALIDATION_STAGE]["child_run_id"]
    temporal = repository.read_json(root_id, "results/temporal_validation_comparison.json")
    assert temporal["final_status"] == "passed"
    assert temporal["reference_holdout_run_id"] == stages["holdout_evaluation"]["child_run_id"]
    validation_manifest = load_pipeline_manifest(repository, temporal_id)
    assert temporal["validation_holdout_run_id"] == next(
        item["child_run_id"] for item in validation_manifest["stages"]
        if item["stage_key"] == "holdout_evaluation"
    )
    forced_id = stages[end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE]["child_run_id"]
    forced_spec = repository.load_spec(forced_id)
    assert forced_spec.forced_candidate_identities == (( '["AAA","BBB"]', "Up"),)
    assert forced_spec.frozen_selected_thresholds_by_set['["AAA","BBB"]']["Up"]["threshold"] == 0.62
    forced_manifest = load_forced_validation_manifest(repository, forced_id)
    assert forced_manifest["schema_version"] == 2
    assert repository.run_metadata(temporal_id).stage_index == 6
    assert repository.run_metadata(forced_id).stage_index == 7
    assert [item["stage_key"] for item in forced_manifest["stages"]] == [
        "walk_forward", "fixed_candidate_evaluation", "promotion_qualification",
    ]
    lock = forced_manifest["period_lock"]
    assert lock == forced_spec.forced_period_lock
    assert lock == validation_manifest["resolved_period"]
    assert lock == stages[end_to_end.FORCED_CANDIDATE_VALIDATION_STAGE]["period_lock"]
    assert lock["offset_sessions"] == 63
    assert lock["holdout_last_session"] == lock["effective_cutoff"]
    assert len(lock["holdout_sessions"]) == lock["final_holdout_size"]
    temporal_wf = lock["temporal_walk_forward_run_id"]
    with (repository.run_directory(temporal_wf) / "checkpoints/artifacts/prepared_snapshot.pkl").open("rb") as stream:
        prepared = pickle.load(stream)["prepared"]
    validate_forced_period(prepared, lock)
    shifted = prepared.copy()
    shifted.index = pd.DatetimeIndex([
        xcals.get_calendar("XNYS").next_session(day).tz_localize(None)
        for day in shifted.index
    ])
    with pytest.raises(ValueError, match="period differs"):
        validate_forced_period(shifted, lock)
    extra_session = xcals.get_calendar("XNYS").sessions_window(
        pd.Timestamp(lock["effective_cutoff"]), 2
    )[-1].tz_localize(None)
    extended = pd.concat([prepared, pd.DataFrame(
        [{column: 0 for column in prepared.columns}], index=[extra_session]
    )])
    with pytest.raises(ValueError, match="period differs"):
        validate_forced_period(extended, lock)
    forced_stages = {item["stage_key"]: item for item in forced_manifest["stages"]}
    forced_wf_id = forced_stages["walk_forward"]["child_run_id"]
    fixed_id = forced_stages["fixed_candidate_evaluation"]["child_run_id"]
    qualification_id = forced_stages["promotion_qualification"]["child_run_id"]
    assert repository.load_spec(forced_wf_id).source_walk_forward_run == temporal_wf
    assert repository.load_spec(fixed_id).prepared_snapshot_required
    assert repository.load_spec(fixed_id).frozen_xgboost_parameters == forced_spec.frozen_xgboost_parameters
    assert repository.load_spec(fixed_id).frozen_selected_thresholds_by_set == forced_spec.frozen_selected_thresholds_by_set
    qualification = repository.read_json(qualification_id, "results/qualification.json")
    assert qualification["candidate_sets"] == ['["AAA","BBB"]']
    assert qualification["period_lock"] == lock
    assert repository.read_json(root_id, PROMOTION_CHECKPOINT)["source_promotion_qualification_run"] == qualification_id
    trigger = repository.read_json(root_id, PROMOTION_TRIGGER_CHECKPOINT)
    assert trigger["promotion_qualification_run_id"] == qualification_id
    assert trigger["source_selection_reason"] == "forced_reference_candidates"
    before = load_pipeline_manifest(repository, root_id)
    end_to_end.run_end_to_end(
        repository.load_spec(root_id), repository.run_directory(root_id) / "results",
        None, None,
        execute_reserved_child=lambda repo, child_id: (
            None if repo.status(child_id)["status"] == "completed"
            else pytest.fail("resume attempted an unfinished child")
        ),
        phase_callback=lambda *args, **kwargs: None,
    )
    after = load_pipeline_manifest(repository, root_id)
    assert after["stages"] == before["stages"]
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 1
    assert models[0].training_metadata["validation_provenance"]["forced_candidate_validation_run_id"] == forced_id
    # A completed forced run must still reject a changed temporal source on retry.
    from rstock.application.forced_candidate_validation import run_forced_candidate_validation

    sidecar = repository.read_json(temporal_wf, "checkpoints/artifacts/prepared_snapshot.json")
    sidecar["sha256"] = "0" * 64
    repository.write_json(temporal_wf, "checkpoints/artifacts/prepared_snapshot.json", sidecar)
    with pytest.raises(ValueError, match="Temporal prepared snapshot digest differs"):
        run_forced_candidate_validation(
            forced_spec, repository.run_directory(forced_id) / "results", None, None,
            execute_reserved_child=lambda *args: pytest.fail("unexpected child replay"),
            phase_callback=lambda *args, **kwargs: None,
        )


@pytest.mark.parametrize(("wf_eligible", "down_selected", "expected_reason"), [
    (False, True, "Walk-forward forcé non qualifié"),
    (True, False, "Seuil Down requis absent"),
])
def test_forced_qualification_persists_individual_blockers(
    tmp_path, wf_eligible, down_selected, expected_reason,
):
    repository = RunRepository(tmp_path / "runs")
    base = _spec(tmp_path)
    wf_spec = replace(base, job_type=JobType.WALK_FORWARD,
                      temporal_validation_enabled=False)
    fixed_spec = replace(base, job_type=JobType.FIXED_CANDIDATE_EVALUATION,
                         temporal_validation_enabled=False)
    wf_id = repository.create(wf_spec)
    fixed_id = repository.create(fixed_spec)
    set_name = '["AAA","BBB"]'
    wf_results = repository.run_directory(wf_id) / "results"
    fixed_results = repository.run_directory(fixed_id) / "results"
    wf_results.mkdir()
    fixed_results.mkdir()
    pd.DataFrame([{"Set": set_name, "Observation": "AAA",
                   "Eligible": wf_eligible}]).to_csv(wf_results / "qualification.csv", index=False)
    selected = {set_name: {
        "Up": {"status": "selected", "threshold": 0.62},
        "Down": ({"status": "selected", "threshold": 0.38} if down_selected
                 else {"status": "missing", "threshold": None}),
    }}
    _write_json(fixed_results / "selected_thresholds_by_set.json", selected)
    pd.DataFrame([{
        "Set": set_name, "Direction": "Up", "SignalCount": 30,
        "ROCAUC": 0.7, "Precision": 0.7, "DirectionalReturnMean": 0.03,
        "OppositeMoveFrequency": 0.1,
    }]).to_csv(fixed_results / "holdout_metrics.csv", index=False)
    _write_json(fixed_results / "run_configuration.json", {"candidate_errors": {}})
    for run_id in (wf_id, fixed_id):
        repository.transition(run_id, JobStatus.RUNNING)
        repository.transition(run_id, JobStatus.COMPLETED)
    qualification_spec = replace(
        base, job_type=JobType.PROMOTION_QUALIFICATION,
        forced_period_lock={"schema_version": 1},
        forced_candidate_identities=((set_name, "Up"),),
        forced_symbol_sets=(("AAA", "BBB"),),
        source_walk_forward_run=wf_id,
        source_threshold_calibration_run=fixed_id,
        source_holdout_evaluation_run=fixed_id,
        frozen_selected_thresholds_by_set=selected,
    )
    summary = _promotion_qualification(
        qualification_spec, tmp_path / "qualification", None, None
    )
    assert summary["candidate_count"] == 0
    decision = summary["decisions"][0]
    assert decision["candidate"] is False
    assert expected_reason in decision["reasons"]
    assert summary["source_fixed_candidate_evaluation_run"] == fixed_id


def test_split_normal_qualification_rejects_missing_down_threshold(tmp_path, monkeypatch):
    from rstock.application import auto_promotion

    repository = RunRepository(tmp_path / "runs")
    base = _spec(tmp_path, pipeline_version=3)
    calibration_id = repository.create(replace(base, job_type=JobType.THRESHOLD_CALIBRATION))
    holdout_id = repository.create(replace(base, job_type=JobType.HOLDOUT_EVALUATION))
    calibration = repository.run_directory(calibration_id) / "results"
    holdout = repository.run_directory(holdout_id) / "results"
    calibration.mkdir()
    holdout.mkdir()
    _write_json(calibration / "selected_thresholds_by_set.json", {
        "AAA<-BBB": {
            "Up": {"status": "selected", "threshold": 0.62},
            "Down": {"status": "missing", "threshold": None},
        }
    })
    pd.DataFrame([{"Set": "AAA<-BBB", "Direction": "Up"}]).to_csv(
        calibration / "threshold_metrics_by_set.csv", index=False
    )
    pd.DataFrame([{"Set": "AAA<-BBB", "Direction": "Up"}]).to_csv(
        holdout / "holdout_metrics.csv", index=False
    )
    for run_id in (calibration_id, holdout_id):
        repository.transition(run_id, JobStatus.RUNNING)
        repository.transition(run_id, JobStatus.COMPLETED)
    monkeypatch.setattr(auto_promotion, "_promotion_guidance", lambda *args, **kwargs: pd.DataFrame([{
        "Combinaison": "AAA<-BBB", "Cible": "AAA", "Direction": "Up",
        "Seuil calibré": 0.62, "Signaux holdout": 30,
        "AUC holdout": 0.7, "Précision holdout": 0.7,
        "Rendement directionnel moyen": 0.03,
        "Fréquence mouvement opposé": 0.1,
        "Statut promotion": "Candidat",
    }]))
    summary = _promotion_qualification(replace(
        base, job_type=JobType.PROMOTION_QUALIFICATION,
        source_threshold_calibration_run=calibration_id,
        source_holdout_evaluation_run=holdout_id,
    ), tmp_path / "normal_qualification", None, None)
    assert summary["candidate_sets"] == []
    assert "Seuil Down requis absent" in summary["decisions"][0]["reasons"]


@pytest.mark.parametrize("failed_stage", [
    JobType.HOLDOUT_EVALUATION, JobType.PROMOTION_QUALIFICATION,
])
def test_split_pipeline_retry_reuses_scientific_children(tmp_path, failed_stage):
    repository = RunRepository(tmp_path / "runs")
    root_id = _create_parent(repository, _spec(tmp_path, pipeline_version=3))
    calls = Counter()
    failed = False
    registry = _fake_registry(repository, calls)
    original_threshold = registry.handlers[JobType.THRESHOLD_CALIBRATION]

    def threshold(spec, output, progress, cancellation):
        result = original_threshold(spec, output, progress, cancellation)
        pd.DataFrame(columns=["Set", "Direction"]).to_csv(
            output / "threshold_metrics_by_set.csv", index=False
        )
        pd.DataFrame([{"Observation": "AAA", "Predictors": '["BBB"]'}]).to_csv(
            output / "sampled_combinations.csv", index=False
        )
        return result

    def child(spec, output, _progress, _cancellation):
        nonlocal failed
        calls[spec.job_type] += 1
        if spec.job_type is failed_stage and not failed:
            failed = True
            raise RuntimeError("injected split stage failure")
        output.mkdir(parents=True, exist_ok=True)
        if spec.job_type is JobType.HOLDOUT_EVALUATION:
            pd.DataFrame(columns=["Set", "Direction"]).to_csv(
                output / "holdout_metrics.csv", index=False
            )
            _write_json(output / "run_configuration.json", {"holdout_evaluated": False})
        else:
            _write_json(output / "qualification.json", {"candidate_sets": [], "decisions": []})
        return {"job_type": spec.job_type.value}

    registry.handlers[JobType.THRESHOLD_CALIBRATION] = threshold
    registry.handlers[JobType.HOLDOUT_EVALUATION] = child
    registry.handlers[JobType.PROMOTION_QUALIFICATION] = child
    execute_run(repository, root_id, 1, registry=registry)
    assert repository.status(root_id)["status"] == JobStatus.FAILED.value
    before = load_pipeline_manifest(repository, root_id)
    ids = {stage["stage_key"]: stage["child_run_id"] for stage in before["stages"][:-1]}

    _resume(repository, root_id, registry)

    assert repository.status(root_id)["status"] == JobStatus.COMPLETED.value
    after = load_pipeline_manifest(repository, root_id)
    assert {stage["stage_key"]: stage["child_run_id"] for stage in after["stages"][:-1]} == ids
    assert all(calls[job_type] == 1 for _, job_type, _ in SCIENTIFIC_STAGES)
    assert calls[JobType.HOLDOUT_EVALUATION] == (
        2 if failed_stage is JobType.HOLDOUT_EVALUATION else 1
    )
    assert calls[JobType.PROMOTION_QUALIFICATION] == (
        2 if failed_stage is JobType.PROMOTION_QUALIFICATION else 1
    )
    assert all(stage["artifact_digests"] for stage in after["stages"][:-1])


def test_split_qualification_does_not_promote_without_holdout_observations(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    base = _spec(tmp_path, pipeline_version=3)
    calibration_id = repository.create(replace(base, job_type=JobType.THRESHOLD_CALIBRATION))
    holdout_id = repository.create(replace(base, job_type=JobType.HOLDOUT_EVALUATION))
    calibration = repository.run_directory(calibration_id) / "results"
    holdout = repository.run_directory(holdout_id) / "results"
    calibration.mkdir()
    holdout.mkdir()
    _write_json(calibration / "selected_thresholds_by_set.json", {
        "AAA<-BBB": {
            "Up": {"status": "selected", "threshold": 0.62},
            "Down": {"status": "selected", "threshold": 0.38},
        },
    })
    pd.DataFrame([{
        "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up",
        "Threshold": 0.62, "SignalCount": 20, "ROCAUC": 0.65,
        "Precision": 0.65, "DirectionalReturnMean": 0.02,
        "OppositeMoveFrequency": 0.2,
    }]).to_csv(calibration / "threshold_metrics_by_set.csv", index=False)
    pd.DataFrame(columns=["Set", "Direction"]).to_csv(
        holdout / "holdout_metrics.csv", index=False
    )
    for run_id in (calibration_id, holdout_id):
        repository.transition(run_id, JobStatus.RUNNING)
        repository.transition(run_id, JobStatus.COMPLETED)
    spec = replace(
        base, job_type=JobType.PROMOTION_QUALIFICATION,
        source_threshold_calibration_run=calibration_id,
        source_holdout_evaluation_run=holdout_id,
    )
    output = tmp_path / "qualification"
    _promotion_qualification(spec, output, None, None)
    result = json.loads((output / "qualification.json").read_text(encoding="utf-8"))
    assert result["candidate_sets"] == []
    assert result["decisions"][0]["Statut promotion"] == "Non candidat"


def test_temporal_validation_passed_without_promotion_keeps_production_empty(
    monkeypatch, tmp_path
):
    class FakeTemporalValidationRunner:
        def __init__(self, *_args, **_kwargs):
            pass

        def execute(self):
            return {"final_status": "passed"}

    monkeypatch.setattr(end_to_end, "TemporalValidationRunner", FakeTemporalValidationRunner)
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path, temporal_validation_enabled=True))
    execute_run(repository, run_id, 1, registry=_fake_registry(repository, Counter()))

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    assert ProductionRepository(tmp_path).models() == []


def test_temporal_validation_reservation_is_reused_before_materialization(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path, temporal_validation_enabled=True))
    first = persist_or_validate_pipeline_manifest(
        repository, run_id, repository.load_spec(run_id)
    )
    child_id = next(
        item["child_run_id"] for item in first["stages"]
        if item["stage_key"] == TEMPORAL_VALIDATION_STAGE
    )
    monkeypatch.setattr(
        repository, "generate_run_id", lambda: (_ for _ in ()).throw(AssertionError())
    )
    resumed = persist_or_validate_pipeline_manifest(
        repository, run_id, repository.load_spec(run_id)
    )
    assert next(
        item["child_run_id"] for item in resumed["stages"]
        if item["stage_key"] == TEMPORAL_VALIDATION_STAGE
    ) == child_id
    assert not repository.run_directory(child_id).exists()


def test_end_to_end_v2_reservation_is_reused_after_crash_before_materialization(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))

    reserved = persist_or_validate_pipeline_manifest(
        repository, run_id, repository.load_spec(run_id)
    )
    child_ids = [item["child_run_id"] for item in reserved["stages"][:-1]]
    assert reserved["child_id_policy_version"] == CHILD_ID_POLICY_RESERVED
    assert all(
        child_id
        != repository.deterministic_child_run_id(
            run_id, f"pipeline_stage:{stage_key}"
        )
        for child_id, (stage_key, _, _) in zip(child_ids, SCIENTIFIC_STAGES)
    )
    assert all(not repository.run_directory(child_id).exists() for child_id in child_ids)

    monkeypatch.setattr(
        repository, "generate_run_id", lambda: (_ for _ in ()).throw(AssertionError())
    )
    resumed = persist_or_validate_pipeline_manifest(
        repository, run_id, repository.load_spec(run_id)
    )
    assert [item["child_run_id"] for item in resumed["stages"][:-1]] == child_ids
    assert repository.list_children(run_id) == []


def test_end_to_end_v1_manifest_coexists_without_migration(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    historical_id = _create_parent(repository, _spec(tmp_path))
    new_id = _create_parent(repository, _spec(tmp_path))
    historical = build_pipeline_manifest(
        repository,
        historical_id,
        repository.load_spec(historical_id),
        child_id_policy_version=1,
    )
    historical.pop("child_id_policy_version")
    (repository.run_directory(historical_id) / "orchestration").mkdir()
    repository.write_json(historical_id, PIPELINE_MANIFEST, historical)

    loaded = persist_or_validate_pipeline_manifest(
        repository, historical_id, repository.load_spec(historical_id)
    )
    fresh = persist_or_validate_pipeline_manifest(
        repository, new_id, repository.load_spec(new_id)
    )
    assert "child_id_policy_version" not in loaded
    assert fresh["child_id_policy_version"] == CHILD_ID_POLICY_RESERVED
    assert loaded["stages"][0]["child_run_id"] == repository.deterministic_child_run_id(
        historical_id, "pipeline_stage:walk_forward"
    )


def test_end_to_end_resume_rejects_changed_completed_artifact(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))
    registry = _fake_registry(
        repository, Counter(), fail_once=JobType.XGBOOST_CALIBRATION
    )
    execute_run(repository, run_id, 1, registry=registry)
    manifest = load_pipeline_manifest(repository, run_id)
    walk_forward_id = manifest["stages"][0]["child_run_id"]
    qualification = (
        repository.run_directory(walk_forward_id) / "results" / "qualification.csv"
    )
    qualification.write_text("tampered", encoding="utf-8")

    _resume(repository, run_id, registry)

    status = repository.status(run_id)
    assert status["status"] == JobStatus.FAILED.value
    assert "ont changé" in status["error"]
    assert repository.status(walk_forward_id)["status"] == JobStatus.COMPLETED.value


def test_end_to_end_parent_cancellation_cancels_active_child_and_stops_pipeline(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))
    registry = _fake_registry(
        repository, Counter(), cancel_on=JobType.WALK_FORWARD
    )

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.CANCELLED.value
    manifest = load_pipeline_manifest(repository, run_id)
    first = manifest["stages"][0]["child_run_id"]
    assert repository.status(first)["status"] == JobStatus.CANCELLED.value
    assert all(
        not repository.run_directory(item["child_run_id"]).exists()
        for item in manifest["stages"][1:-1]
    )


def test_end_to_end_auto_promotion_requires_holdout(tmp_path):
    with pytest.raises(ValueError, match="exige le holdout"):
        _spec(
            tmp_path,
            evaluate_final_holdout=False,
            auto_promote_candidates=True,
        )


def test_end_to_end_auto_promotes_each_unique_up_candidate_once(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository, _spec(tmp_path, auto_promote_candidates=True)
    )
    rows = [
        {"Set": "AAA<-BBB"},
        {"Set": "BBB<-AAA", "ROCAUC": 0.59},
    ]
    calls = Counter()
    registry = _fake_registry(repository, calls, promotion_rows=rows)

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 1
    assert models[0].target == "AAA"
    assert models[0].predictors == ("BBB",)
    assert models[0].status.value == "candidate"
    assert models[0].source_walk_forward_run
    assert models[0].source_xgboost_calibration_run
    assert models[0].source_threshold_calibration_run
    assert models[0].training_metadata["selected_threshold_direction"] == "Up"

    promotion = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    assert promotion["status"] == "completed"
    assert promotion["policy_parameters"] == {
        "minimum_holdout_signals": 20,
        "minimum_holdout_auc": 0.6,
        "minimum_holdout_precision": 0.4,
        "minimum_directional_return_exclusive": 0.0,
        "maximum_opposite_movement_frequency": 0.3,
        "required_direction": "Up",
    }
    assert promotion["candidate_sets"] == ["AAA<-BBB"]
    assert promotion["candidate_count"] == 1
    assert promotion["created_count"] == 1
    assert promotion["completed_count"] == 1
    assert {
        row["Combinaison"]: row["Statut promotion"]
        for row in promotion["diagnostics"]
    } == {"AAA<-BBB": "Candidat", "BBB<-AAA": "Non candidat"}
    manifest = load_pipeline_manifest(repository, run_id)
    promotion_stage = manifest["stages"][-1]
    assert promotion_stage["expected_job_type"] is None
    assert promotion_stage["child_run_id"] is None
    assert promotion_stage["expected_fingerprint"] == promotion["plan_sha256"]
    assert promotion_stage["artifact_digests"].keys() == {PROMOTION_CHECKPOINT}
    assert repository.summary(run_id)["promotion"]["created_count"] == 1
    assert all(calls[job_type] == 1 for _, job_type, _ in SCIENTIFIC_STAGES)
    detail = RunService(repository).get(run_id)
    assert detail["pipeline_stages"][-1]["status"] == "completed"
    assert detail["pipeline_stages"][-1]["progress"] == 100.0
    assert detail["pipeline_stages"][-1]["promotion"]["created_count"] == 1


def test_end_to_end_auto_promotion_uses_the_frozen_snapshot_policy(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    spec = _spec(tmp_path, auto_promote_candidates=True)
    spec = replace(
        spec,
        config=replace(spec.config, promotion_min_holdout_auc=0.70),
    )
    run_id = _create_parent(repository, spec)
    registry = _fake_registry(
        repository, Counter(), promotion_rows=[{"Set": "AAA<-BBB", "ROCAUC": 0.65}]
    )

    execute_run(repository, run_id, 1, registry=registry)

    promotion = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    assert promotion["candidate_count"] == 0
    assert promotion["policy_parameters"]["minimum_holdout_auc"] == 0.70


def test_end_to_end_promotion_resume_reconciles_model_created_before_checkpoint(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository, _spec(tmp_path, auto_promote_candidates=True)
    )
    rows = [{"Set": "AAA<-BBB"}, {"Set": "BBB<-AAA"}]
    calls = Counter()
    registry = _fake_registry(repository, calls, promotion_rows=rows)
    original = PromotionService.promote
    injected = {"raised": False}

    def create_then_fail(self, *args, **kwargs):
        result = original(self, *args, **kwargs)
        if not injected["raised"]:
            injected["raised"] = True
            raise RuntimeError("injected failure after registry publication")
        return result

    with monkeypatch.context() as patcher:
        patcher.setattr(PromotionService, "promote", create_then_fail)
        execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.FAILED.value
    failed = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    trigger_before = repository.read_json(run_id, PROMOTION_TRIGGER_CHECKPOINT)
    tampered = {**trigger_before, "promotion_qualification_run_id": "wrong-run"}
    repository.write_json(run_id, PROMOTION_TRIGGER_CHECKPOINT, tampered)
    with pytest.raises(ValueError, match="decision changed on resume"):
        PromotionTriggerPolicy(repository, run_id).decide(
            repository.load_spec(run_id), load_pipeline_manifest(repository, run_id),
            None, None,
        )
    repository.write_json(run_id, PROMOTION_TRIGGER_CHECKPOINT, trigger_before)
    assert failed["status"] == "failed"
    assert failed["candidates"][0]["status"] == "failed"
    assert len(ProductionRepository(tmp_path).models()) == 1

    _resume(repository, run_id, registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 2
    assert len({model.training_metadata["promotion_fingerprint"] for model in models}) == 2
    completed = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    assert repository.read_json(run_id, PROMOTION_TRIGGER_CHECKPOINT) == trigger_before
    assert completed["status"] == "completed"
    assert completed["completed_count"] == 2
    assert completed["created_count"] == 1
    assert completed["reused_count"] == 1
    assert all(item["status"] == "completed" for item in completed["candidates"])
    stages = {item["stage_key"]: item for item in load_pipeline_manifest(repository, run_id)["stages"]}
    coordinator = AutoPromotionRunner(
        repository, root_run_id=run_id,
        walk_forward_run_id=stages["walk_forward"]["child_run_id"],
        xgboost_calibration_run_id=stages["xgboost_calibration"]["child_run_id"],
        threshold_calibration_run_id=stages["threshold_calibration"]["child_run_id"],
    )
    assert coordinator._persist_candidate("AAA<-BBB", status="running")["candidates"][0]["status"] == "completed"
    assert coordinator._persist_stage("running")["status"] == "completed"
    assert repository.read_json(run_id, PROMOTION_CHECKPOINT) == completed
    assert all(calls[job_type] == 1 for _, job_type, _ in SCIENTIFIC_STAGES)


def test_end_to_end_auto_promotion_completes_without_candidates(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(
        repository, _spec(tmp_path, auto_promote_candidates=True)
    )
    registry = _fake_registry(
        repository,
        Counter(),
        promotion_rows=[{"Set": "AAA<-BBB", "SignalCount": 19}],
    )

    execute_run(repository, run_id, 1, registry=registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    promotion = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    assert promotion["status"] == "completed"
    assert promotion["candidate_count"] == 0
    assert ProductionRepository(tmp_path).models() == []


def test_run_service_marks_parent_and_exposes_live_pipeline_children(tmp_path):
    class FakeBackend:
        def launch(self, runs_root, run_id, max_concurrent_jobs):
            return 1234

    repository = RunRepository(tmp_path / "runs")
    service = RunService(repository, backend=FakeBackend())
    submitted = service.submit(_spec(tmp_path))
    assert repository.run_metadata(submitted.run_id).run_role is RunRole.PIPELINE_PARENT

    # The worker normally creates this manifest. Building it here isolates the
    # read-side contract used later by the UI without launching scientific work.
    from rstock.application.end_to_end import persist_or_validate_pipeline_manifest

    persist_or_validate_pipeline_manifest(
        repository, submitted.run_id, repository.load_spec(submitted.run_id)
    )
    detail = service.get(submitted.run_id)
    assert [item["status"] for item in detail["pipeline_stages"]] == [
        "reserved",
        "reserved",
        "reserved",
        "reserved",
        "not_requested",
    ]


def test_end_to_end_metadata_is_canonical_for_temporal_and_standard_runs(tmp_path):
    repository = RunRepository(tmp_path / "runs")

    standard_id = repository.create(_spec(tmp_path))
    temporal_id = repository.create(
        _spec(tmp_path, temporal_validation_enabled=True)
    )

    standard = repository.run_metadata(standard_id)
    temporal = repository.run_metadata(temporal_id)
    assert standard.run_role is RunRole.PIPELINE_PARENT
    assert standard.run_purpose is RunPurpose.STANDARD
    assert temporal.run_role is RunRole.PIPELINE_PARENT
    assert temporal.run_purpose is RunPurpose.REFERENCE


def test_temporal_parent_rejects_explicit_noncanonical_metadata(tmp_path):
    repository = RunRepository(tmp_path / "runs")

    with pytest.raises(ValueError, match="pipeline_parent/reference"):
        repository.create(
            _spec(tmp_path, temporal_validation_enabled=True),
            metadata=RunMetadata(),
        )


def test_temporal_parent_guard_runs_before_manifest_or_children(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    spec = _spec(tmp_path, temporal_validation_enabled=True)
    run_id = repository.create(spec)
    repository.write_json(run_id, "metadata.json", RunMetadata().to_dict())
    child_calls = []

    with pytest.raises(ValueError, match="pipeline_parent/reference"):
        end_to_end.run_end_to_end(
            spec,
            repository.run_directory(run_id) / "_working",
            None,
            None,
            execute_reserved_child=lambda *_: child_calls.append(True),
            phase_callback=lambda *_args, **_kwargs: None,
        )

    assert child_calls == []
    assert repository.list_children(run_id) == []
    assert not (
        repository.run_directory(run_id) / "orchestration" / "pipeline.json"
    ).exists()
    assert not (repository.run_directory(run_id) / "results").exists()


def test_historical_failed_end_to_end_backfill_reserves_new_dedicated_job(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    parent_id = _create_parent(repository, _spec(tmp_path))
    expected_description = "Revalidation des candidats de r\u00e9f\u00e9rence"
    repository.transition(parent_id, JobStatus.RUNNING)
    repository.transition(parent_id, JobStatus.COMPLETED)
    dedicated = replace(
        _spec(tmp_path),
        job_type=JobType.FORCED_CANDIDATE_VALIDATION,
        forced_symbol_sets=(("AAA", "BBB"),),
        historical_forced_validation_backfill=True,
        auto_promote_candidates=False,
        run_description=expected_description,
    )
    legacy = replace(dedicated, job_type=JobType.END_TO_END)
    legacy_id = repository.create(legacy)
    repository.transition(legacy_id, JobStatus.RUNNING)
    repository.transition(legacy_id, JobStatus.FAILED, error="legacy failure")
    (repository.run_directory(parent_id) / "orchestration").mkdir(exist_ok=True)
    repository.write_json(
        parent_id,
        end_to_end.HISTORICAL_FORCED_VALIDATION_CHECKPOINT,
        {
            "schema_version": 1,
            "parent_run_id": parent_id,
            "child_run_id": legacy_id,
            "expected_fingerprint": legacy.fingerprint,
            "created_at": "2026-09-20T00:00:00+00:00",
        },
    )
    monkeypatch.setattr(
        end_to_end,
        "_historical_forced_validation_spec",
        lambda *_args: (dedicated, "offset-63"),
    )

    new_id, new_spec, created = (
        end_to_end.materialize_historical_forced_candidate_validation(
            repository, parent_id
        )
    )

    checkpoint = repository.read_json(
        parent_id, end_to_end.HISTORICAL_FORCED_VALIDATION_CHECKPOINT
    )
    assert created is True
    assert new_id != legacy_id
    assert new_spec.job_type is JobType.FORCED_CANDIDATE_VALIDATION
    assert repository.status(legacy_id)["status"] == JobStatus.FAILED.value
    assert repository.load_spec(new_id).job_type is JobType.FORCED_CANDIDATE_VALIDATION
    assert repository.load_spec(new_id).run_description == expected_description
    assert checkpoint["active_child_run_id"] == new_id
    assert checkpoint["previous_child_run_ids"] == [legacy_id]


def test_end_to_end_restart_creates_a_new_parent_and_new_child_chain(tmp_path):
    class FakeBackend:
        def launch(self, runs_root, run_id, max_concurrent_jobs):
            return 1234

    repository = RunRepository(tmp_path / "runs")
    source_id = _create_parent(repository, _spec(tmp_path))
    source_manifest = build_pipeline_manifest(
        repository, source_id, repository.load_spec(source_id)
    )
    service = RunService(repository, backend=FakeBackend())

    restarted = service.restart(source_id)
    restarted_manifest = build_pipeline_manifest(
        repository, restarted.run_id, repository.load_spec(restarted.run_id)
    )

    assert restarted.run_id != source_id
    assert repository.run_metadata(restarted.run_id).run_role is RunRole.PIPELINE_PARENT
    assert {
        item["child_run_id"] for item in source_manifest["stages"][:-1]
    }.isdisjoint(
        {item["child_run_id"] for item in restarted_manifest["stages"][:-1]}
    )


def test_temporal_end_to_end_restart_and_resume_keep_reference_metadata(tmp_path):
    class FakeBackend:
        def launch(self, runs_root, run_id, max_concurrent_jobs):
            return 1234

    repository = RunRepository(tmp_path / "runs")
    source_id = repository.create(
        _spec(tmp_path, temporal_validation_enabled=True)
    )
    repository.transition(source_id, JobStatus.FAILED, error="interrupted")
    service = RunService(repository, backend=FakeBackend())

    resumed = service.resume(source_id)
    resumed_metadata = repository.run_metadata(resumed.run_id)
    assert resumed_metadata.run_role is RunRole.PIPELINE_PARENT
    assert resumed_metadata.run_purpose is RunPurpose.REFERENCE

    repository.transition(source_id, JobStatus.FAILED, error="interrupted again")
    restarted = service.restart(source_id)
    restarted_metadata = repository.run_metadata(restarted.run_id)
    assert restarted_metadata.run_role is RunRole.PIPELINE_PARENT
    assert restarted_metadata.run_purpose is RunPurpose.REFERENCE


def test_end_to_end_auto_forward_returns_and_dispatches_the_reserved_child(
    tmp_path, monkeypatch
):
    class RecordingBackend:
        launches = []

        def launch(self, _runs_root, run_id, _max_concurrent_jobs):
            self.launches.append(run_id)
            return 7654

    backend = RecordingBackend()
    repository = RunRepository(tmp_path / "runs")
    parent = _spec(
        tmp_path,
        historical_data_cutoff="2026-06-22",
        requested_historical_cutoff="2026-06-22",
        resolved_market_session_cutoff="2026-06-22",
        forward_simulation_enabled=True,
        forward_simulation_mode="63_sessions",
    )
    run_id = _create_parent(repository, parent)
    monkeypatch.setattr(
        end_to_end,
        "build_forward_model_snapshot",
        lambda *_args, **_kwargs: {
            "candidate_count": 1,
            "resolved_market_session_cutoff": "2026-06-22",
            "snapshot_sha256": "a" * 64,
        },
    )
    monkeypatch.setattr(worker_module, "LocalProcessBackend", lambda: backend)

    execute_run(repository, run_id, 1, registry=_fake_registry(repository, Counter()))

    forward = repository.summary(run_id)["forward_simulation"]
    child_id = forward["child_run_id"]
    assert forward["status"] == "launched"
    assert backend.launches == [child_id]
    assert repository.status(child_id)["launcher_pid"] == 7654
    assert repository.status(child_id)["dispatch_state"] == "launched"
    assert repository.status(child_id)["dispatch_attempt_count"] == 1
    child_log = "\n".join(repository.log_tail(child_id, lines=20))
    assert "Forward created pending by End-to-End" in child_log
    assert "Forward initial dispatch requested" in child_log
    assert "Forward initial dispatch launched" in child_log
    forward_children = [
        item
        for item in repository.list_children(run_id)
        if repository.load_spec(item).job_type is JobType.FORWARD_SIMULATION
    ]
    assert forward_children == [child_id]
    persisted_forward = repository.read_json(
        run_id, "results/pipeline_summary.json"
    )["forward_simulation"]
    assert persisted_forward["child_run_id"] == child_id
    assert persisted_forward["status"] == "launched"


def test_end_to_end_auto_forward_without_candidates_completes_child_without_simulation(
    tmp_path, monkeypatch
):
    import hashlib
    import rstock.application.forward_simulation as forward_module

    class RecordingBackend:
        launches = []

        def launch(self, _runs_root, run_id, _max_concurrent_jobs):
            self.launches.append(run_id)
            return 7654

    backend = RecordingBackend()
    repository = RunRepository(tmp_path / "runs")
    parent = _spec(
        tmp_path,
        historical_data_cutoff="2026-06-22",
        requested_historical_cutoff="2026-06-22",
        resolved_market_session_cutoff="2026-06-22",
        forward_simulation_enabled=True,
        forward_simulation_mode="63_sessions",
    )
    run_id = _create_parent(repository, parent)

    def empty_snapshot(_repository, _root_id, _spec, *, result_directory, **_kwargs):
        path = result_directory / "forward_model_snapshot.json"
        path.write_text(json.dumps({
            "source_end_to_end_run_id": run_id,
            "resolved_market_session_cutoff": "2026-06-22",
            "candidate_count": 0,
            "models": [],
        }), encoding="utf-8")
        return {
            "candidate_count": 0,
            "resolved_market_session_cutoff": "2026-06-22",
            "snapshot_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        }

    class ForbiddenMarketData:
        def load(self, *_args, **_kwargs):
            raise AssertionError("Forward market data must not load without models")

    monkeypatch.setattr(end_to_end, "build_forward_model_snapshot", empty_snapshot)
    monkeypatch.setattr(worker_module, "LocalProcessBackend", lambda: backend)
    monkeypatch.setattr(forward_module, "MarketDataService", ForbiddenMarketData)

    execute_run(repository, run_id, 1, registry=_fake_registry(repository, Counter()))

    parent_forward = repository.summary(run_id)["forward_simulation"]
    child_id = parent_forward["child_run_id"]
    assert parent_forward["status"] == "launched"
    assert backend.launches == [child_id]
    assert repository.read_json(run_id, "results/pipeline_summary.json")["forward_simulation"]["child_run_id"] == child_id
    assert repository.run_metadata(child_id).visible_in_history is True
    assert any(item.status["run_id"] == child_id for item in RunService(repository).history_summaries())

    execute_run(repository, child_id, 1)

    assert repository.status(child_id)["status"] == "completed"
    summary = repository.summary(child_id)
    assert summary["result"] == "skipped_no_models"
    assert summary["candidate_count"] == 0
    assert summary["simulation_executed"] is False
    assert summary["reason"] == "no_eligible_models"
    assert summary["precision"] is None
    assert repository.read_json(child_id, "results/forward_summary.json")["result"] == "skipped_no_models"
    assert not (repository.run_directory(child_id) / "results/forward_observations.csv").exists()


def test_end_to_end_without_auto_forward_never_dispatches_a_forward_child(
    tmp_path, monkeypatch
):
    class RecordingBackend:
        launches = []

        def launch(self, _runs_root, run_id, _max_concurrent_jobs):
            self.launches.append(run_id)
            return 7654

    backend = RecordingBackend()
    repository = RunRepository(tmp_path / "runs")
    run_id = _create_parent(repository, _spec(tmp_path))
    monkeypatch.setattr(worker_module, "LocalProcessBackend", lambda: backend)

    execute_run(repository, run_id, 1, registry=_fake_registry(repository, Counter()))

    assert repository.summary(run_id)["forward_simulation"] is None
    assert backend.launches == []
    assert not [
        item
        for item in repository.list_children(run_id)
        if repository.load_spec(item).job_type is JobType.FORWARD_SIMULATION
    ]
