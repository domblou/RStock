from __future__ import annotations

from dataclasses import replace
import hashlib
from pathlib import Path

import pytest

from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.end_to_end import REQUIRED_ARTIFACTS, build_pipeline_manifest
from rstock.application.history_ui import history_row
from rstock.application.history_analysis import (
    load_threshold_calibration_artifacts,
    load_threshold_holdout_predictions,
    load_walk_forward_artifacts,
)
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import PromotionService
from rstock.application.repository import RunRepository
from rstock.application.run_storage import RunStorageService
from rstock.application.runner import RunService
from rstock.config import DEFAULT_CONFIG


ESSENTIALS = {
    JobType.WALK_FORWARD: (
        "qualification.csv",
        "final_holdout.csv",
        "selection_results.csv",
        "run_configuration.json",
    ),
    JobType.XGBOOST_CALIBRATION: (
        "selected_configurations.json",
        "development_metrics_by_configuration.csv",
        "holdout_metrics.csv",
        "run_configuration.json",
        "sampling_manifest.json",
    ),
    JobType.THRESHOLD_PARAMETER_CALIBRATION: (
        "selected_threshold_calibration_configuration.json",
        "development_metrics_by_configuration.csv",
        "tested_threshold_parameter_configurations.csv",
        "run_configuration.json",
        "sampling_manifest.json",
    ),
    JobType.THRESHOLD_CALIBRATION: (
        "selected_thresholds.json",
        "selected_thresholds_by_set.json",
        "threshold_metrics_by_set.csv",
        "run_configuration.json",
        "sampling_manifest.json",
    ),
}

PURGE_SAMPLE = {
    JobType.WALK_FORWARD: "aggregate_by_set.csv",
    JobType.XGBOOST_CALIBRATION: "development_metrics_by_window.csv",
    JobType.THRESHOLD_PARAMETER_CALIBRATION: "development_predictions.csv",
    JobType.THRESHOLD_CALIBRATION: "development_probabilities.csv",
}


class FakeBackend:
    def launch(self, runs_root, run_id, max_concurrent_jobs):
        return 999_999_999


def _spec(tmp_path: Path, job_type: JobType) -> ExperimentSpec:
    return ExperimentSpec(
        job_type=job_type,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
    )


def _complete(repository: RunRepository, run_id: str) -> None:
    repository.transition(run_id, JobStatus.RUNNING, pid=999_999_999)
    repository.transition(run_id, JobStatus.COMPLETED)


def _write_essential_results(
    repository: RunRepository, run_id: str, job_type: JobType
) -> None:
    results = repository.run_directory(run_id) / "results"
    results.mkdir(parents=True, exist_ok=True)
    for name in ESSENTIALS.get(job_type, ()):
        path = results / name
        path.write_text(
            "{}\n" if path.suffix == ".json" else "value\n1\n",
            encoding="utf-8",
        )


def _completed_run(
    repository: RunRepository, tmp_path: Path, job_type: JobType
) -> str:
    run_id = repository.create(_spec(tmp_path, job_type))
    _write_essential_results(repository, run_id, job_type)
    _complete(repository, run_id)
    return run_id


@pytest.mark.parametrize(
    "job_type",
    [
        JobType.WALK_FORWARD,
        JobType.XGBOOST_CALIBRATION,
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.THRESHOLD_CALIBRATION,
    ],
)
def test_completed_scientific_runs_purge_only_explicit_heavy_artifacts(
    tmp_path, job_type
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _completed_run(repository, tmp_path, job_type)
    directory = repository.run_directory(run_id)
    heavy = directory / "results" / PURGE_SAMPLE[job_type]
    heavy.write_bytes(b"x" * 37)
    unknown = directory / "results" / "future_unknown_artifact.bin"
    unknown.write_bytes(b"keep")
    checkpoint = directory / "checkpoints" / "batches" / "payload.pkl"
    checkpoint.parent.mkdir(parents=True)
    checkpoint.write_bytes(b"checkpoint")
    working = directory / "_working" / "temporary.csv"
    working.parent.mkdir()
    working.write_bytes(b"temporary")

    service = RunStorageService(repository)
    plan = service.preview(run_id)

    assert plan.reclaimable_bytes == 37 + len(b"checkpoint") + len(b"temporary")
    assert not (directory / "storage.json").exists()

    storage = service.purge(run_id)

    assert storage["state"] == "purged"
    assert storage["policy_version"] == "heavy-artifacts-v1"
    assert storage["reclaimed_bytes"] == plan.reclaimable_bytes
    assert not heavy.exists()
    assert not (directory / "checkpoints").exists()
    assert not (directory / "_working").exists()
    assert unknown.read_bytes() == b"keep"
    for name in ESSENTIALS[job_type]:
        assert (directory / "results" / name).exists()
    assert (directory / "config.json").exists()
    assert (directory / "metadata.json").exists()
    assert (directory / "summary.json").exists()
    assert (directory / "run.log").exists()


def test_walk_forward_policy_removes_window_and_set_aggregates(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    results = repository.run_directory(run_id) / "results"
    removed = (
        "aggregate_by_window.csv",
        "aggregate_by_set.csv",
        "aggregate_global.csv",
        "risk_by_window.csv",
        "risk_by_set.csv",
        "risk_global.csv",
    )
    for name in removed:
        (results / name).write_text("value\n1\n", encoding="utf-8")

    RunStorageService(repository).purge(run_id)

    assert all(not (results / name).exists() for name in removed)
    assert (results / "qualification.csv").exists()


def test_historical_run_without_storage_manifest_is_full(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))

    assert repository.storage(run_id) == {
        "schema_version": 1,
        "state": "full",
        "policy_version": None,
        "purged_at": None,
        "reclaimed_bytes": 0,
        "deleted_artifacts": [],
    }
    assert not (repository.run_directory(run_id) / "storage.json").exists()


@pytest.mark.parametrize(
    "status",
    ["pending", "running", "failed", "cancelled", "interrupted"],
)
def test_non_completed_run_is_not_purgeable(tmp_path, status):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))
    values = repository.status(run_id)
    values["status"] = status
    values["pid"] = None
    repository.write_json(run_id, "status.json", values)

    eligibility = RunStorageService(repository).eligibility(run_id)

    assert not eligibility.eligible
    assert "terminés" in str(eligibility.reason)


def test_walk_forward_parent_waits_for_technical_children_then_purges_them(
    tmp_path,
):
    repository = RunRepository(tmp_path / "runs")
    parent = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    child = repository.create(
        _spec(tmp_path, JobType.WALK_FORWARD_BATCH),
        metadata=RunMetadata(
            run_role=RunRole.TECHNICAL_BATCH,
            visible_in_history=False,
            parent_run_id=parent,
            relation_key="walk_forward_batch:000000",
            relation_type="walk_forward_batch",
            batch_id="000000",
            batch_index=0,
            batch_count=1,
        ),
    )
    payload = repository.run_directory(child) / "checkpoints" / "payload.pkl"
    payload.parent.mkdir()
    payload.write_bytes(b"batch")
    service = RunStorageService(repository)

    assert not service.eligibility(parent).eligible

    _complete(repository, child)
    assert service.eligibility(parent).eligible
    plan = service.preview(parent)
    assert plan.related_run_ids == (child,)
    assert plan.reclaimable_bytes == len(b"batch")

    service.purge(parent)

    assert not payload.exists()
    assert repository.storage(child)["state"] == "purged"
    assert repository.storage(parent)["state"] == "purged"


def test_unfinished_descendant_blocks_purge(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    dependent_spec = replace(
        _spec(tmp_path, JobType.XGBOOST_CALIBRATION),
        source_walk_forward_run=source,
    )
    dependent = repository.create(dependent_spec)

    eligibility = RunStorageService(repository).eligibility(source)

    assert not eligibility.eligible
    assert dependent in str(eligibility.reason)


def test_active_worker_and_missing_essential_artifact_block_purge(
    tmp_path, monkeypatch
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    status = repository.status(run_id)
    status["pid"] = 123
    repository.write_json(run_id, "status.json", status)
    monkeypatch.setattr(
        "rstock.application.run_storage.process_alive",
        lambda pid: pid == 123,
    )

    active = RunStorageService(repository).eligibility(run_id)
    assert not active.eligible
    assert "worker" in str(active.reason)

    status["pid"] = None
    repository.write_json(run_id, "status.json", status)
    (
        repository.run_directory(run_id) / "results" / "qualification.csv"
    ).unlink()
    missing = RunStorageService(repository).eligibility(run_id)
    assert not missing.eligible
    assert "qualification.csv" in str(missing.reason)


def test_interrupted_purge_is_reconciled_and_second_purge_is_idempotent(
    tmp_path, monkeypatch
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    results = repository.run_directory(run_id) / "results"
    for name in ("predictions.csv", "windows.csv"):
        (results / name).write_bytes(name.encode())
    service = RunStorageService(repository)
    original = service._delete_artifact
    calls = 0

    def interrupt(artifact):
        nonlocal calls
        calls += 1
        original(artifact)
        if calls == 1:
            raise OSError("simulated interruption")

    monkeypatch.setattr(service, "_delete_artifact", interrupt)
    with pytest.raises(OSError, match="simulated interruption"):
        service.purge(run_id)
    assert repository.storage(run_id)["state"] == "purging"

    recovered = RunStorageService(repository).purge(run_id)
    repeated = RunStorageService(repository).purge(run_id)

    assert recovered["state"] == "purged"
    assert repeated == recovered
    assert not (results / "predictions.csv").exists()
    assert not (results / "windows.csv").exists()


def test_purged_run_cannot_be_resumed_or_restarted(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))
    storage = repository.storage(run_id)
    storage.update(state="purged", policy_version="heavy-artifacts-v1")
    repository.write_storage(run_id, storage)
    service = RunService(repository, backend=FakeBackend())

    with pytest.raises(ValueError, match="purgé"):
        service.resume(run_id)
    with pytest.raises(ValueError, match="purgé"):
        service.restart(run_id)


def test_history_labels_purged_run_as_summary_only(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))
    status = repository.status(run_id)
    detail = {
        "configuration": repository.load_spec(run_id).to_dict(),
        "summary": {},
        "storage": {"state": "purged"},
    }

    row = history_row(status, detail, {})

    assert row.storage == "Résumé seulement"
    assert row.display()["Stockage"] == "Résumé seulement"


def test_end_to_end_waits_for_all_children_and_preserves_manifest_artifacts(
    tmp_path,
):
    repository = RunRepository(tmp_path / "runs")
    parent_spec = _spec(tmp_path, JobType.END_TO_END)
    parent = repository.create(
        parent_spec,
        metadata=RunMetadata(run_role=RunRole.PIPELINE_PARENT),
    )
    manifest = build_pipeline_manifest(repository, parent, parent_spec)
    children: list[str] = []
    protected_heavy: tuple[str, str] | None = None
    for stage in manifest["stages"][:-1]:
        stage_key = str(stage["stage_key"])
        job_type = JobType(str(stage["expected_job_type"]))
        child_id = str(stage["child_run_id"])
        repository.create(
            _spec(tmp_path, job_type),
            run_id=child_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE,
                visible_in_history=False,
                parent_run_id=parent,
                relation_key=f"pipeline_stage:{stage_key}",
                relation_type="pipeline_stage",
                stage_key=stage_key,
            ),
        )
        _write_essential_results(repository, child_id, job_type)
        heavy_name = PURGE_SAMPLE[job_type]
        (repository.run_directory(child_id) / "results" / heavy_name).write_bytes(
            stage_key.encode()
        )
        stage["artifact_digests"] = {}
        for relative in REQUIRED_ARTIFACTS[stage_key]:
            path = repository.run_directory(child_id) / relative
            stage["artifact_digests"][relative] = hashlib.sha256(
                path.read_bytes()
            ).hexdigest()
        if job_type is JobType.XGBOOST_CALIBRATION:
            protected_relative = f"results/{heavy_name}"
            stage["artifact_digests"][protected_relative] = hashlib.sha256(
                (
                    repository.run_directory(child_id) / protected_relative
                ).read_bytes()
            ).hexdigest()
            protected_heavy = (child_id, protected_relative)
        children.append(child_id)
    orchestration = repository.run_directory(parent) / "orchestration"
    orchestration.mkdir()
    repository.write_json(parent, "orchestration/pipeline.json", manifest)
    _complete(repository, parent)
    for child_id in children[:-1]:
        _complete(repository, child_id)

    service = RunStorageService(repository)
    assert not service.eligibility(parent).eligible

    _complete(repository, children[-1])
    manifest_before = (
        repository.run_directory(parent) / "orchestration" / "pipeline.json"
    ).read_bytes()
    required_before = {
        (child_id, relative): (repository.run_directory(child_id) / relative).read_bytes()
        for stage, child_id in zip(manifest["stages"][:-1], children, strict=True)
        for relative in REQUIRED_ARTIFACTS[str(stage["stage_key"])]
    }

    plan = service.preview(parent)
    expected_reclaimable = sum(
        len(str(stage["stage_key"]).encode())
        for stage in manifest["stages"][:-1]
        if str(stage["stage_key"]) != "xgboost_calibration"
    )
    assert set(plan.related_run_ids) == set(children)
    assert plan.reclaimable_bytes == expected_reclaimable

    service.purge(parent)

    assert (
        repository.run_directory(parent) / "orchestration" / "pipeline.json"
    ).read_bytes() == manifest_before
    for key, value in required_before.items():
        child_id, relative = key
        assert (repository.run_directory(child_id) / relative).read_bytes() == value
    for stage, child_id in zip(manifest["stages"][:-1], children, strict=True):
        job_type = JobType(str(stage["expected_job_type"]))
        heavy_path = (
            repository.run_directory(child_id) / "results" / PURGE_SAMPLE[job_type]
        )
        if protected_heavy == (
            child_id,
            f"results/{PURGE_SAMPLE[job_type]}",
        ):
            assert heavy_path.exists()
        else:
            assert not heavy_path.exists()
        assert repository.storage(child_id)["state"] == "purged"
    assert repository.storage(parent)["state"] == "purged"


def test_preview_stats_heavy_files_without_reading_their_contents(
    tmp_path, monkeypatch
):
    repository = RunRepository(tmp_path / "runs")
    run_id = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    heavy = repository.run_directory(run_id) / "results" / "predictions.csv"
    heavy.write_bytes(b"x" * 123)
    original = Path.read_text

    def guarded_read_text(path, *args, **kwargs):
        if path == heavy:
            raise AssertionError("The preview must not read a heavy artifact")
        return original(path, *args, **kwargs)

    monkeypatch.setattr(Path, "read_text", guarded_read_text)

    assert RunStorageService(repository).preview(run_id).reclaimable_bytes == 123


def test_promoted_model_and_persisted_simulation_remain_valid_after_purge(
    tmp_path,
):
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(_spec(tmp_path, JobType.WALK_FORWARD))
    results = repository.run_directory(run_id) / "results"
    results.mkdir()
    (results / "qualification.csv").write_text(
        'Set,Observation,Predictors,Eligible,ROCAUCMedian\n'
        'AAA<-BBB,AAA,"[""BBB""]",True,0.61\n',
        encoding="utf-8",
    )
    (results / "final_holdout.csv").write_text(
        "Set,FinalUpROCAUC\nAAA<-BBB,0.58\n",
        encoding="utf-8",
    )
    (results / "selection_results.csv").write_text(
        "Set,model_selection_score\nAAA<-BBB,75.0\n",
        encoding="utf-8",
    )
    (results / "run_configuration.json").write_text("{}\n", encoding="utf-8")
    (results / "predictions.csv").write_bytes(b"heavy")
    _complete(repository, run_id)
    production = ProductionRepository(tmp_path)
    promoted, created = PromotionService(repository, production).promote(
        run_id, "AAA<-BBB"
    )
    simulation = tmp_path / "simulations" / "simulation.json"
    simulation.parent.mkdir()
    simulation.write_text('{"simulation": true}\n', encoding="utf-8")

    RunStorageService(repository).purge(run_id)

    reloaded = next(
        model
        for model in ProductionRepository(tmp_path).models()
        if model.model_id == promoted.model_id
    )
    assert created
    assert reloaded.source_walk_forward_run == run_id
    assert reloaded.target == "AAA"
    assert reloaded.predictors == ("BBB",)
    assert simulation.read_text(encoding="utf-8") == '{"simulation": true}\n'


def test_retained_history_views_load_and_purged_detail_returns_empty(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    walk_forward = _completed_run(repository, tmp_path, JobType.WALK_FORWARD)
    threshold = _completed_run(
        repository, tmp_path, JobType.THRESHOLD_CALIBRATION
    )
    threshold_predictions = (
        repository.run_directory(threshold) / "results" / "holdout_predictions.csv"
    )
    threshold_predictions.write_text("Set,Direction\nAAA<-BBB,Up\n", encoding="utf-8")
    (
        repository.run_directory(threshold) / "results" / "holdout_metrics.csv"
    ).write_text("Set,Direction\nAAA<-BBB,Up\n", encoding="utf-8")

    storage = RunStorageService(repository)
    storage.purge(walk_forward)
    storage.purge(threshold)

    qualification, holdout = load_walk_forward_artifacts(tmp_path, walk_forward)
    metrics, threshold_holdout, selected = load_threshold_calibration_artifacts(
        tmp_path, threshold
    )
    purged_predictions = load_threshold_holdout_predictions(tmp_path, threshold)
    assert not qualification.empty
    assert not holdout.empty
    assert not metrics.empty
    assert not threshold_holdout.empty
    assert selected == {}
    assert purged_predictions.empty
