"""Phase-one contracts for derived End-to-End plans (no execution)."""

from __future__ import annotations

import hashlib
from dataclasses import replace

import pytest

from rstock.application.derivation import (
    Derivation,
    InheritedStage,
    ParameterOverride,
    SCIENTIFIC_STAGE_KEYS,
    stage_modes,
)
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.end_to_end import (
    DERIVED_PIPELINE_SCHEMA_VERSION,
    PIPELINE_MANIFEST,
    SCIENTIFIC_STAGES,
    build_pipeline_manifest,
    effective_stage_run_id,
    load_pipeline_manifest,
    persist_or_validate_pipeline_manifest,
    validate_pipeline_manifest,
    run_end_to_end,
)
from rstock.application.repository import RunRepository
from rstock.config import DEFAULT_CONFIG


def _source(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source_spec = ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=replace(
            DEFAULT_CONFIG, project_root=tmp_path,
            threshold_calibration_min_robust_signals=20,
        ),
        symbols=("AAA", "BBB"),
        historical_data_cutoff="2026-09-26",
    )
    source_id = repository.create(source_spec)
    source_manifest = build_pipeline_manifest(repository, source_id, source_spec)
    for index, (stage_key, job_type, _) in enumerate(SCIENTIFIC_STAGES):
        child_id = source_manifest["stages"][index]["child_run_id"]
        child_spec = replace(source_spec, job_type=job_type)
        repository.create(
            child_spec, run_id=child_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE,
                parent_run_id=source_id,
                relation_key=f"pipeline_stage:{stage_key}",
            ),
        )
        source_manifest["stages"][index]["expected_fingerprint"] = (
            child_spec.fingerprint
        )
        source_manifest["stages"][index]["artifact_digests"] = {
            "summary.json": hashlib.sha256(
                repository.run_directory(child_id).joinpath("summary.json").read_bytes()
            ).hexdigest()
        }
        repository.transition(child_id, JobStatus.RUNNING)
        repository.transition(child_id, JobStatus.COMPLETED)
    repository.run_directory(source_id).joinpath("orchestration").mkdir()
    repository.write_json(source_id, PIPELINE_MANIFEST, source_manifest)
    repository.transition(source_id, JobStatus.RUNNING)
    repository.transition(source_id, JobStatus.COMPLETED)
    manifest_sha = hashlib.sha256(
        repository.run_directory(source_id).joinpath(PIPELINE_MANIFEST).read_bytes()
    ).hexdigest()
    return repository, source_spec, source_id, source_manifest, manifest_sha


def _derived(repository, source_spec, source_id, source_manifest, manifest_sha,
             fork_stage, *, overrides=(), forward=False):
    modes = stage_modes(fork_stage, forward_enabled=forward)
    inherited = {}
    for stage in source_manifest["stages"]:
        stage_key = stage["stage_key"]
        if stage_key in SCIENTIFIC_STAGE_KEYS and modes[stage_key] == "inherited":
            child_id = stage["child_run_id"]
            inherited[stage_key] = InheritedStage(
                source_run_id=child_id,
                configuration_fingerprint=repository.configuration_fingerprint(child_id),
                required_artifact_digests=stage["artifact_digests"],
            )
    derivation = Derivation(
        source_end_to_end_run_id=source_id,
        fork_stage=fork_stage,
        source_manifest_sha256=manifest_sha,
        inherited_stages=inherited,
        overrides=tuple(overrides),
        created_at="2026-09-26T18:00:00+00:00",
    )
    config = source_spec.config
    if any(item.field == "threshold_calibration_min_robust_signals" for item in overrides):
        config = replace(config, threshold_calibration_min_robust_signals=15)
    return replace(
        source_spec, config=config, derivation=derivation,
        forward_simulation_enabled=forward,
    )


@pytest.mark.parametrize(
    ("fork_stage", "inherited_keys"),
    [
        ("xgboost_calibration", ("walk_forward",)),
        ("threshold_parameter_calibration", ("walk_forward", "xgboost_calibration")),
        ("threshold_calibration", (
            "walk_forward", "xgboost_calibration", "threshold_parameter_calibration"
        )),
    ],
)
def test_derived_manifest_resolves_shared_sources_and_new_children(
    tmp_path, fork_stage, inherited_keys,
):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    spec = _derived(repository, source_spec, source_id, source, sha, fork_stage)
    root_id = repository.create(spec)
    manifest = persist_or_validate_pipeline_manifest(repository, root_id, spec)

    assert manifest["schema_version"] == DERIVED_PIPELINE_SCHEMA_VERSION
    assert manifest["derivation"]["source_end_to_end_run_id"] == source_id
    assert repository.run_metadata(root_id).parent_run_id is None
    assert repository.run_metadata(root_id).root_run_id == root_id
    assert load_pipeline_manifest(repository, root_id) == manifest
    assert persist_or_validate_pipeline_manifest(repository, root_id, spec) == manifest
    for stage in manifest["stages"]:
        key = stage["stage_key"]
        if key in inherited_keys:
            assert stage["mode"] == "inherited"
            assert stage["child_run_id"] is None
            assert effective_stage_run_id(manifest, key) == stage["source_run_id"]
        elif key in {"promotion", "forward_simulation"}:
            assert stage["mode"] == "not_executed"
            assert effective_stage_run_id(manifest, key) is None
        else:
            assert stage["mode"] == "recomputed"
            assert stage["source_run_id"] is None
            assert effective_stage_run_id(manifest, key) == stage["child_run_id"]
    assert all(
        stage["child_run_id"] not in {
            source_stage["child_run_id"] for source_stage in source["stages"]
        }
        for stage in manifest["stages"] if stage["child_run_id"]
    )


def test_two_derived_roots_can_share_one_source(tmp_path):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    spec = _derived(repository, source_spec, source_id, source, sha,
                    "threshold_calibration")
    first = repository.create(spec)
    second = repository.create(spec)
    first_manifest = persist_or_validate_pipeline_manifest(repository, first, spec)
    second_manifest = persist_or_validate_pipeline_manifest(repository, second, spec)
    assert first != second
    assert effective_stage_run_id(first_manifest, "walk_forward") == (
        effective_stage_run_id(second_manifest, "walk_forward")
    )
    assert effective_stage_run_id(first_manifest, "threshold_calibration") != (
        effective_stage_run_id(second_manifest, "threshold_calibration")
    )


def test_derivation_round_trip_and_override_ownership(tmp_path):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    override = ParameterOverride(
        "threshold_calibration_min_robust_signals", 20, 15
    )
    spec = _derived(
        repository, source_spec, source_id, source, sha,
        "threshold_calibration", overrides=(override,), forward=True,
    )
    restored = ExperimentSpec.from_dict(spec.to_dict())
    assert restored.derivation == spec.derivation
    assert restored.fingerprint == spec.fingerprint
    assert stage_modes("threshold_calibration", forward_enabled=True)[
        "forward_simulation"
    ] == "recomputed"
    manifest = build_pipeline_manifest(repository, "new-root", restored)
    validate_pipeline_manifest(manifest, root_run_id="new-root")
    assert effective_stage_run_id(manifest, "forward_simulation") is not None

    with pytest.raises(ValueError, match="inherited or disabled"):
        _derived(
            repository, source_spec, source_id, source, sha,
            "threshold_calibration",
            overrides=(ParameterOverride(
                "xgboost_global_max_qualified_combinations", 500, 100
            ),),
        )


def test_ordinary_manifest_and_historical_config_stay_v1(tmp_path):
    repository, source_spec, source_id, source, _ = _source(tmp_path)
    assert "derivation" not in source_spec.to_dict()
    historical = ExperimentSpec.from_dict(source_spec.to_dict())
    assert historical.derivation is None
    assert historical.fingerprint == source_spec.fingerprint
    assert source["schema_version"] == 1
    assert load_pipeline_manifest(repository, source_id) == source
    assert effective_stage_run_id(source, "walk_forward") == (
        source["stages"][0]["child_run_id"]
    )
    assert build_pipeline_manifest(repository, "another-root", historical)[
        "schema_version"
    ] == 1


def test_derived_source_manifest_mutation_is_rejected(tmp_path):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    spec = _derived(repository, source_spec, source_id, source, sha,
                    "threshold_calibration")
    repository.write_json(source_id, PIPELINE_MANIFEST, {**source, "changed": True})
    with pytest.raises(ValueError, match="missing or has changed"):
        build_pipeline_manifest(repository, "new-root", spec)


def test_derived_pipeline_requires_a_pinned_prepared_snapshot(tmp_path):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    spec = _derived(repository, source_spec, source_id, source, sha,
                    "threshold_calibration")
    root_id = repository.create(spec)
    with pytest.raises(ValueError, match="snapshot digest is required"):
        run_end_to_end(
            spec, repository.run_directory(root_id) / "results", None, None,
            execute_reserved_child=lambda *_: pytest.fail("child was started"),
            phase_callback=lambda **_: None,
        )
    assert not repository.run_directory(root_id).joinpath(PIPELINE_MANIFEST).exists()


def test_derived_manifest_rejects_changed_lineage_and_dependencies(tmp_path):
    repository, source_spec, source_id, source, sha = _source(tmp_path)
    spec = _derived(repository, source_spec, source_id, source, sha,
                    "threshold_calibration")
    manifest = build_pipeline_manifest(repository, "new-root", spec)
    manifest["stages"][0]["source_run_id"] = "different-source"
    with pytest.raises(ValueError, match="Invalid inherited stage"):
        validate_pipeline_manifest(manifest, root_run_id="new-root")
    manifest = build_pipeline_manifest(repository, "new-root", spec)
    manifest["stages"][3]["dependency_run_ids"] = []
    with pytest.raises(ValueError, match="dependencies mismatch"):
        validate_pipeline_manifest(manifest, root_run_id="new-root")
