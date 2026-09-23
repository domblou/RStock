import json
from collections import Counter
from dataclasses import replace

import pandas as pd
import pytest

import rstock.application.end_to_end as end_to_end
from rstock.application.auto_promotion import PROMOTION_CHECKPOINT
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
)
from rstock.config import DEFAULT_CONFIG
from rstock.progress import check_cancellation


def _spec(tmp_path, **values):
    return ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
        combinations_per_target=1,
        **values,
    )


def _write_json(path, values):
    path.write_text(json.dumps(values), encoding="utf-8")


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
    assert failed["status"] == "failed"
    assert failed["candidates"][0]["status"] == "failed"
    assert len(ProductionRepository(tmp_path).models()) == 1

    _resume(repository, run_id, registry)

    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value
    models = ProductionRepository(tmp_path).models()
    assert len(models) == 2
    assert len({model.training_metadata["promotion_fingerprint"] for model in models}) == 2
    completed = repository.read_json(run_id, PROMOTION_CHECKPOINT)
    assert completed["status"] == "completed"
    assert completed["completed_count"] == 2
    assert completed["created_count"] == 1
    assert completed["reused_count"] == 1
    assert all(item["status"] == "completed" for item in completed["candidates"])
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
