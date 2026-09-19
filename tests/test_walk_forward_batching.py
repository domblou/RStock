from dataclasses import replace

import numpy as np
import pandas as pd

import rstock.application.workflows as workflows
import rstock.streaming_walk_forward as streaming
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.orchestration_runtime import execute_child
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.walk_forward_batches import (
    CHILD_ID_POLICY_RESERVED,
    MANIFEST_NAME,
    build_manifest,
    materialize_reservations,
    persist_or_validate_manifest,
    prefilter_digest,
)
from rstock.application.worker import execute_run
from rstock.application.workflows import WorkflowRegistry
from rstock.combination_planning import CombinationPlan
from rstock.config import DEFAULT_CONFIG
from rstock.features import prepare_dataset


def _spec(tmp_path, job_type=JobType.WALK_FORWARD):
    return ExperimentSpec(
        job_type=job_type,
        config=replace(
            DEFAULT_CONFIG,
            project_root=tmp_path,
            predictor_prefilter_enabled=False,
            permutation_depth=1,
        ),
        symbols=("AAA", "BBB", "CCC"),
        target_symbols=("AAA", "BBB", "CCC"),
        context_symbols=(),
    )


def test_manifest_reserves_ids_before_materializing_children_and_is_idempotent(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent_spec = _spec(tmp_path)
    parent_id = repository.create(
        parent_spec, metadata=RunMetadata(run_role=RunRole.PIPELINE_PARENT)
    )
    plan = CombinationPlan(parent_spec.symbols, 1)
    proposed, reservations = build_manifest(
        repository,
        parent_run_id=parent_id,
        parent_spec=parent_spec,
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
    )

    manifest = persist_or_validate_manifest(repository, parent_id, proposed)

    assert len(manifest["batches"]) == 3
    assert repository.list_children(parent_id) == []
    assert manifest["raw_combination_count"] == 6
    assert manifest["preview_batch_count"] == 3
    assert manifest["prefiltered_combination_count"] == 6
    assert manifest["effective_batch_count"] == 3
    assert manifest["max_combinations_per_batch"] == 2
    batch_indices = [
        combination_index
        for batch in manifest["batches"]
        for combination_index in range(batch["range_start"], batch["range_stop"])
    ]
    assert batch_indices == list(range(plan.count()))
    assert len(batch_indices) == len(set(batch_indices))

    materialize_reservations(
        repository, manifest=manifest, reservations=reservations
    )
    first_ids = [item["child_run_id"] for item in manifest["batches"]]
    materialize_reservations(
        repository, manifest=manifest, reservations=reservations
    )

    assert sorted(repository.list_children(parent_id)) == sorted(first_ids)
    for index, run_id in enumerate(first_ids):
        child = repository.load_spec(run_id)
        assert child.job_type is JobType.WALK_FORWARD_BATCH
        assert child.combination_range_start == index * 2
        assert child.combination_range_stop == (index + 1) * 2
        assert repository.run_metadata(run_id).visible_in_history is False


def test_v2_batch_reservations_survive_crash_before_materialization(monkeypatch, tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent_spec = _spec(tmp_path)
    parent_id = repository.create(parent_spec)
    plan = CombinationPlan(parent_spec.symbols, 1)
    proposed, _ = build_manifest(
        repository,
        parent_run_id=parent_id,
        parent_spec=parent_spec,
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
    )
    manifest = persist_or_validate_manifest(repository, parent_id, proposed)
    child_ids = [item["child_run_id"] for item in manifest["batches"]]
    assert manifest["child_id_policy_version"] == CHILD_ID_POLICY_RESERVED
    assert repository.list_children(parent_id) == []

    monkeypatch.setattr(
        repository, "generate_run_id", lambda: (_ for _ in ()).throw(AssertionError())
    )
    resumed, reservations = build_manifest(
        repository,
        parent_run_id=parent_id,
        parent_spec=parent_spec,
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
        child_id_policy_version=CHILD_ID_POLICY_RESERVED,
        reserved_child_ids=child_ids,
    )
    persisted = persist_or_validate_manifest(repository, parent_id, resumed)
    materialize_reservations(repository, manifest=persisted, reservations=reservations)
    materialize_reservations(repository, manifest=persisted, reservations=reservations)
    assert sorted(repository.list_children(parent_id)) == sorted(child_ids)


def test_v1_and_v2_batch_manifests_coexist_without_migration(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    plan = CombinationPlan(_spec(tmp_path).symbols, 1)
    historical_id = repository.create(_spec(tmp_path))
    fresh_id = repository.create(_spec(tmp_path))
    historical, _ = build_manifest(
        repository,
        parent_run_id=historical_id,
        parent_spec=repository.load_spec(historical_id),
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
        child_id_policy_version=1,
    )
    historical.pop("child_id_policy_version")
    (repository.run_directory(historical_id) / "orchestration").mkdir()
    repository.write_json(historical_id, MANIFEST_NAME, historical)
    resumed, _ = build_manifest(
        repository,
        parent_run_id=historical_id,
        parent_spec=repository.load_spec(historical_id),
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
        child_id_policy_version=1,
        reserved_child_ids=[item["child_run_id"] for item in historical["batches"]],
    )
    assert persist_or_validate_manifest(repository, historical_id, resumed) == historical
    fresh, _ = build_manifest(
        repository,
        parent_run_id=fresh_id,
        parent_spec=repository.load_spec(fresh_id),
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
    )
    assert fresh["child_id_policy_version"] == CHILD_ID_POLICY_RESERVED


def test_run_service_hides_technical_batches_and_exposes_them_on_parent(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent_spec = _spec(tmp_path)
    parent_id = repository.create(parent_spec)
    plan = CombinationPlan(parent_spec.symbols, 1)
    proposed, reservations = build_manifest(
        repository,
        parent_run_id=parent_id,
        parent_spec=parent_spec,
        raw_plan=plan,
        effective_plan=plan,
        max_combinations_per_batch=2,
        prefilter_policy_version="disabled_v1",
        prefilter_sha256=prefilter_digest(plan.predictors_by_target),
    )
    manifest = persist_or_validate_manifest(repository, parent_id, proposed)
    materialize_reservations(repository, manifest=manifest, reservations=reservations)

    service = RunService(repository)

    assert [item["run_id"] for item in service.list()] == [parent_id]
    detail = service.get(parent_id)
    assert detail["walk_forward_batch_manifest"]["planned_batch_count"] == 3
    assert len(detail["walk_forward_batches"]) == 3
    assert {item["status"] for item in detail["walk_forward_batches"]} == {"pending"}


def test_parent_releases_single_heavy_slot_while_child_executes(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    parent_spec = _spec(tmp_path)
    parent_id = repository.create(parent_spec)
    child_id_holder = []

    def parent_handler(spec, output, progress, cancellation):
        relation = "walk_forward_batch:000000"
        child_id = repository.deterministic_child_run_id(parent_id, relation)
        child_spec = replace(
            spec,
            job_type=JobType.WALK_FORWARD_BATCH,
            source_walk_forward_run=parent_id,
            combination_plan_version=2,
            combination_plan_sha256="plan",
            combination_range_start=0,
            combination_range_stop=1,
        )
        repository.create(
            child_spec,
            run_id=child_id,
            metadata=RunMetadata(
                run_role=RunRole.TECHNICAL_BATCH,
                visible_in_history=False,
                parent_run_id=parent_id,
                relation_key=relation,
                relation_type="walk_forward_batch",
                stage_key="walk_forward",
                batch_id="000000",
                batch_index=0,
                batch_count=1,
            ),
        )
        child_id_holder.append(child_id)
        execute_child(child_id)
        return {"parent": "done"}

    def child_handler(spec, output, progress, cancellation):
        return {"child": "done"}

    registry = WorkflowRegistry(
        {
            JobType.WALK_FORWARD: parent_handler,
            JobType.WALK_FORWARD_BATCH: child_handler,
        }
    )

    execute_run(repository, parent_id, 1, registry=registry)

    assert repository.status(parent_id)["status"] == JobStatus.COMPLETED.value
    assert repository.status(child_id_holder[0])["status"] == JobStatus.COMPLETED.value


def test_batched_parent_runs_children_and_publishes_canonical_results(
    monkeypatch, tmp_path
):
    index = pd.bdate_range("2024-01-01", periods=24)
    signal = np.arange(len(index)) % 2
    prices = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB")):
        shifted = np.roll(signal, offset)
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.Close"] = np.where(shifted, 102.0, 100.0)
        prices[f"{symbol}.High"] = np.maximum(prices[f"{symbol}.Close"], 100.0) + 1.0
        prices[f"{symbol}.Low"] = np.minimum(prices[f"{symbol}.Close"], 100.0) - 1.0
    prepared = prepare_dataset(prices, ["AAA", "BBB"])
    config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        permutation_depth=1,
        predictor_prefilter_enabled=False,
        walk_forward_max_combinations_per_batch=1,
        combination_workers=1,
        walk_forward_batch_size=1,
        final_holdout_batch_size=1,
        xgb_rounds=1,
        xgb_nthread=1,
        walk_forward_min_train_size=8,
        walk_forward_test_size=4,
        walk_forward_step_size=4,
        final_holdout_size=4,
        qualification_min_windows=2,
        qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0,
        qualification_min_positive_observations=1,
        qualification_max_auc_std=1.0,
    )
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=config,
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
    )
    monkeypatch.setattr(
        workflows,
        "_prepared_inputs",
        lambda *args, **kwargs: (
            prepared,
            ["AAA", "BBB"],
            ["AAA", "BBB"],
            {},
        ),
    )
    repository = RunRepository(tmp_path / "runs")
    parent_id = repository.create(spec)

    original_insert = streaming._insert_predictions
    monkeypatch.setattr(
        streaming,
        "_insert_predictions",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            RuntimeError("aggregation crash")
        ),
    )
    execute_run(repository, parent_id, 1)

    assert repository.status(parent_id)["status"] == "failed"
    children = repository.list_children(parent_id)
    assert len(children) == 2
    assert all(repository.status(run_id)["status"] == "completed" for run_id in children)
    child_ids = tuple(children)

    monkeypatch.setattr(streaming, "_insert_predictions", original_insert)
    monkeypatch.setattr(
        workflows,
        "_walk_forward_batch",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            AssertionError("a completed child was relaunched")
        ),
    )
    resumed = repository.prepare_resume(parent_id)
    resumed["resume_requested"] = True
    repository.write_json(parent_id, "status.json", resumed)
    execute_run(repository, parent_id, 1)

    assert repository.status(parent_id)["status"] == "completed"
    assert tuple(repository.list_children(parent_id)) == child_ids
    results = repository.run_directory(parent_id) / "results"
    assert (results / "qualification.csv").exists()
    assert (results / "final_holdout.csv").exists()
    detail = RunService(repository).get(parent_id)
    assert len(detail["walk_forward_batches"]) == 2
    manifest = detail["walk_forward_batch_manifest"]
    assert manifest["max_combinations_per_batch"] == 1
    assert [
        (batch["range_start"], batch["range_stop"])
        for batch in manifest["batches"]
    ] == [(0, 1), (1, 2)]
