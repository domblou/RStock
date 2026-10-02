import hashlib
import json
import pickle
from dataclasses import replace
from types import SimpleNamespace
from threading import Event, Thread

import pandas as pd
import pytest

from rstock.application.derived_experiments import build_derived_spec
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.end_to_end import (
    PIPELINE_MANIFEST, SCIENTIFIC_STAGES, artifact_digests,
    build_pipeline_manifest, persist_or_validate_pipeline_manifest,
    run_end_to_end, validate_pipeline_manifest,
)
from rstock.application.derivation import Derivation
from rstock.application.repository import RunRepository
from rstock.application.run_storage import RunStorageService
from rstock.application.runner import RunService
from rstock.application.run_detail_tabs import pipeline_stage_rows
from rstock.application import workflows
from rstock.application import forward_simulation as forward_module
from rstock.checkpoints import CheckpointManager
from rstock.config import DEFAULT_CONFIG
from rstock.threshold_parameter_calibration import ThresholdCalibrationParameters
from rstock.traceability import prepared_dataset_hash


def _completed(repository, run_id):
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.COMPLETED)


def _source(tmp_path, *, temporal=False, legacy_manifest=False):
    repository = RunRepository(tmp_path / "runs")
    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    source = ExperimentSpec(
        job_type=JobType.END_TO_END, config=config, symbols=("AAA", "BBB"),
        historical_data_cutoff=None if temporal else "2026-09-26",
        temporal_validation_enabled=temporal,
    )
    root_id = repository.create(source)
    manifest = build_pipeline_manifest(repository, root_id, source)
    prepared = pd.DataFrame(
        {"AAA_Close": [1.0, 2.0, 3.0], "BBB_Close": [2.0, 3.0, 4.0]},
        index=pd.date_range("2026-09-23", periods=3),
    )
    prepared.attrs["effective_end_date"] = "2026-09-26T00:00:00"
    prepared.attrs["symbols_used"] = 2
    digest = prepared_dataset_hash(prepared)
    for index, (key, job_type, _) in enumerate(SCIENTIFIC_STAGES):
        child_id = manifest["stages"][index]["child_run_id"]
        child_spec = replace(source, job_type=job_type, temporal_validation_enabled=False)
        repository.create(
            child_spec, run_id=child_id,
            metadata=RunMetadata(
                run_role=RunRole.PIPELINE_STAGE,
                parent_run_id=root_id,
                relation_key=f"pipeline_stage:{key}",
            ),
        )
        results = repository.run_directory(child_id) / "results"
        results.mkdir()
        from rstock.application.end_to_end import REQUIRED_ARTIFACTS
        for relative in REQUIRED_ARTIFACTS[key]:
            path = repository.run_directory(child_id) / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("{}" if path.suffix == ".json" else "x\n", encoding="utf-8")
        if key == "walk_forward":
            repository.write_json(child_id, "summary.json", {
                "traceability": {
                    "prepared_dataset_sha256": digest,
                    "prepared_market_last_date": prepared.index.max().isoformat(),
                }
            })
            checkpoint = CheckpointManager(
                repository.run_directory(child_id), run_id=child_id,
                job_type=job_type.value, configuration_fingerprint=child_spec.fingerprint,
                batch_sizes={
                    "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
                    "walk_forward": config.walk_forward_batch_size,
                    "final_holdout": config.final_holdout_batch_size,
                },
            )
            checkpoint.commit_snapshot(prepared, {
                "predictor_symbols": ["AAA", "BBB"],
                "target_symbols": ["AAA", "BBB"],
                "calendars": {"AAA": "XNYS", "BBB": "XNYS"},
                "effective_end_date": prepared.attrs["effective_end_date"],
            })
        elif key == "xgboost_calibration":
            repository.write_json(child_id, "results/selected_configurations.json", {
                direction: {"parameters": {
                    "max_depth": 2, "eta": 0.05, "num_boost_round": 20,
                }} for direction in ("Up", "Down")
            })
        elif key == "threshold_parameter_calibration":
            parameters = ThresholdCalibrationParameters.from_config(
                replace(config, threshold_calibration_min_robust_signals=20)
            ).as_dict()
            repository.write_json(
                child_id, "results/selected_threshold_calibration_configuration.json",
                {"parameters": parameters},
            )
        manifest["stages"][index]["expected_fingerprint"] = child_spec.fingerprint
        manifest["stages"][index]["artifact_digests"] = artifact_digests(
            repository, child_id, key
        )
        _completed(repository, child_id)
    repository.run_directory(root_id).joinpath("orchestration").mkdir()
    if legacy_manifest:
        manifest.pop("prepared_dataset_as_of")
    repository.write_json(root_id, PIPELINE_MANIFEST, manifest)
    _completed(repository, root_id)
    return repository, root_id, manifest, digest


def test_temporal_source_creates_scientific_only_derivation_from_verified_legacy_snapshot(tmp_path):
    repository, source_id, source_manifest, _ = _source(
        tmp_path, temporal=True, legacy_manifest=True
    )
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )

    assert spec.historical_data_cutoff == "2026-09-25"
    assert spec.temporal_validation_enabled is False
    assert spec.derivation.source_temporal_validation_enabled is True
    assert [stage["stage_key"] for stage in source_manifest["stages"] if "temporal" in stage["stage_key"]]

    derived_id = repository.create(spec)
    manifest = build_pipeline_manifest(repository, derived_id, spec)
    validate_pipeline_manifest(manifest, root_run_id=derived_id)
    assert manifest["prepared_dataset_as_of"] == "2026-09-25"
    assert manifest["temporal_validation_enabled"] is False
    assert manifest["temporal_validation_provenance"] == {
        "source_enabled": True, "inherited": False, "replayed": False,
    }
    assert all("temporal_validation" not in stage["stage_key"] for stage in manifest["stages"])
    manifest["temporal_validation_provenance"]["inherited"] = True
    with pytest.raises(ValueError, match="temporal validation provenance"):
        validate_pipeline_manifest(manifest, root_run_id=derived_id)
    persisted = persist_or_validate_pipeline_manifest(repository, derived_id, spec)
    assert persist_or_validate_pipeline_manifest(repository, derived_id, spec) == persisted


def test_legacy_derivation_without_temporal_provenance_keeps_its_serialized_contract(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    previous = spec.derivation.to_dict()
    previous.pop("source_temporal_validation_enabled")

    restored = Derivation.from_dict(previous)
    assert restored.source_temporal_validation_enabled is None
    assert restored.to_dict() == previous
    old_spec = replace(spec, derivation=restored)
    old_id = repository.create(old_spec)
    old_manifest = build_pipeline_manifest(repository, old_id, old_spec)
    assert "temporal_validation_provenance" not in old_manifest
    validate_pipeline_manifest(old_manifest, root_run_id=old_id)


def test_legacy_source_without_unambiguous_walk_forward_session_is_rejected(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path, temporal=True, legacy_manifest=True)
    walk_forward_id = manifest["stages"][0]["child_run_id"]
    summary = repository.summary(walk_forward_id)
    summary["traceability"]["prepared_market_last_date"] = "2026-09-24T00:00:00"
    repository.write_json(walk_forward_id, "summary.json", summary)
    manifest["stages"][0]["artifact_digests"] = artifact_digests(
        repository, walk_forward_id, "walk_forward"
    )
    repository.write_json(source_id, PIPELINE_MANIFEST, manifest)

    with pytest.raises(ValueError, match="prepared dates differ|session is ambiguous"):
        build_derived_spec(
            repository, source_id, "threshold_calibration",
            {"threshold_calibration_min_robust_signals": 15},
        )


def test_domain_rejects_replaying_temporal_validation_on_a_derivative(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    with pytest.raises(ValueError, match="cannot inherit or replay temporal validation"):
        replace(spec, temporal_validation_enabled=True, historical_data_cutoff=None)


def _threshold_selection(set_name):
    return {set_name: {
        "Up": {"threshold": 0.6},
        "Down": {"threshold": 0.4},
    }}


def test_derived_forward_has_own_frozen_models_candidates_and_reserved_child(
    tmp_path, monkeypatch,
):
    repository, source_id, source_manifest, digest = _source(tmp_path)
    source_threshold = source_manifest["stages"][3]["child_run_id"]
    source_set = json.dumps(["AAA", "BBB"])
    derived_set = json.dumps(["BBB", "AAA"])
    repository.write_json(
        source_threshold, "results/selected_thresholds_by_set.json",
        _threshold_selection(source_set),
    )

    class FrozenMarket:
        def load(self, *_args, **_kwargs):
            snapshot = repository.run_directory(
                source_manifest["stages"][0]["child_run_id"]
            ) / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
            prepared = pickle.loads(snapshot.read_bytes())["prepared"]
            return SimpleNamespace(prices=prepared, symbols=("AAA", "BBB")), None

    class FakeBooster:
        def save_model(self, path):
            path.write_bytes(f"booster:{path.parent.name}:{path.name}".encode())

    monkeypatch.setattr(forward_module, "MarketDataService", FrozenMarket)
    monkeypatch.setattr(forward_module, "prepare_dataset", lambda prices, *_: prices)
    monkeypatch.setattr(forward_module, "predictor_columns", lambda *_: ["AAA_Close"])
    monkeypatch.setattr(forward_module, "intraday_target_column", lambda *_: "AAA_Close")
    monkeypatch.setattr(forward_module, "intraday_down_target_column", lambda *_: "BBB_Close")
    monkeypatch.setattr(forward_module, "fit_booster", lambda *_args, **_kwargs: FakeBooster())
    monkeypatch.setattr(
        forward_module, "_promotion_guidance",
        lambda _root, selected, _config: pd.DataFrame([{
            "Statut promotion": "Candidat",
            "Combinaison": next(iter(selected)), "Direction": "Up",
        }]),
    )
    source_spec = repository.load_spec(source_id)
    source_snapshot = forward_module.build_forward_model_snapshot(
        repository, source_id, source_spec
    )
    class RecordingBackend:
        def launch(self, _runs_root, _run_id, _limit):
            return 7654

    parent_forward = RunService(
        repository, backend=RecordingBackend()
    ).start_forward_simulation(
        source_id, start_date="2026-09-28", end_date="2026-10-02"
    )

    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {
            "threshold_calibration_min_robust_signals": 15,
            "forward_simulation_mode": "custom_end_date",
            "forward_simulation_end_date": "2026-09-28",
        },
        forward_enabled=True,
    )
    derived_id = repository.create(spec)
    reserved = build_pipeline_manifest(repository, derived_id, spec)
    repository.run_directory(derived_id).joinpath("orchestration").mkdir()
    repository.write_json(derived_id, PIPELINE_MANIFEST, reserved)
    forward_id = reserved["stages"][-1]["child_run_id"]
    assert reserved["stages"][-1]["mode"] == "recomputed"
    assert reserved["stages"][-2]["mode"] == "not_executed"

    class ForbiddenMarket:
        def load(self, *_args, **_kwargs):
            pytest.fail("derived Forward snapshot consulted the market cache")

    monkeypatch.setattr(forward_module, "MarketDataService", ForbiddenMarket)
    calls = []

    def execute(repo, child_id):
        calls.append(child_id)
        repo.run_directory(child_id).joinpath("results").mkdir(exist_ok=True)
        repo.write_json(
            child_id, "results/selected_thresholds_by_set.json",
            _threshold_selection(derived_set),
        )
        repo.write_json(child_id, "results/run_configuration.json", {})
        _completed(repo, child_id)

    output = repository.run_directory(derived_id) / "results"
    result = run_end_to_end(
        spec, output, None, None,
        execute_reserved_child=execute, phase_callback=lambda *a, **k: None,
    )
    derived_snapshot = forward_module.validate_forward_snapshot(
        repository, repository.load_spec(forward_id)
    )
    assert result["forward_simulation"]["child_run_id"] == forward_id
    assert parent_forward.run_id != forward_id
    assert repository.load_spec(parent_forward.run_id).source_end_to_end_run == source_id
    assert repository.load_spec(forward_id).derivation is None
    assert repository.load_spec(forward_id).source_end_to_end_run == derived_id
    with pytest.raises(ValueError, match="forward_snapshot_sha256_missing"):
        forward_module.validate_forward_snapshot(
            repository,
            replace(
                repository.load_spec(forward_id),
                source_forward_model_snapshot_sha256=None,
            ),
            require_expected_hash=False,
        )
    assert source_snapshot["models"][0]["set"] == source_set
    assert derived_snapshot["models"][0]["set"] == derived_set
    assert source_snapshot["models"][0]["source_model_id"] != (
        derived_snapshot["models"][0]["source_model_id"]
    )
    assert derived_snapshot["prepared_dataset_sha256"] == digest
    assert derived_snapshot["source_end_to_end_run_id"] == derived_id
    assert len(calls) == 1
    resumed = run_end_to_end(
        spec, output, None, None,
        execute_reserved_child=lambda *_: None,
        phase_callback=lambda *a, **k: None,
    )
    assert resumed["forward_simulation"]["child_run_id"] == forward_id
    assert set(repository.list_children(derived_id)) == {calls[0], forward_id}
    _completed(repository, derived_id)
    assert not RunStorageService(repository).eligibility(source_id).eligible

    manual = RunService(
        repository, backend=RecordingBackend()
    ).start_forward_simulation(
        derived_id, start_date="2026-09-28", end_date="2026-10-02"
    )
    assert manual.run_id != forward_id
    assert repository.load_spec(manual.run_id).source_end_to_end_run == derived_id
    assert repository.load_spec(manual.run_id).derivation is None
    source_wf = source_manifest["stages"][0]["child_run_id"]
    historical = pickle.loads((
        repository.run_directory(source_wf)
        / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
    ).read_bytes())["prepared"]
    future = pd.concat([
        historical,
        pd.DataFrame(
            {"AAA_Close": [4.0], "BBB_Close": [5.0]},
            index=pd.DatetimeIndex(["2026-09-28"]),
        ),
    ])

    class FutureMarket:
        def load(self, *_args, **_kwargs):
            return SimpleNamespace(prices=future, symbols=("AAA", "BBB")), None

    monkeypatch.setattr(forward_module, "MarketDataService", FutureMarket)
    monkeypatch.setattr(
        forward_module, "prepare_prediction_row",
        lambda *_args, **_kwargs: pd.DataFrame({"AAA_Close": [4.0]}),
    )
    monkeypatch.setattr(forward_module, "load_booster", lambda path: path.name)
    monkeypatch.setattr(
        forward_module, "predict_probabilities",
        lambda booster, *_args: [0.8 if booster == "up.ubj" else 0.2],
    )
    monkeypatch.setattr(forward_module, "_target_evaluation_validity", lambda *_: (None, []))
    monkeypatch.setattr(forward_module, "intraday_return_column", lambda *_: "BBB_Close")
    monkeypatch.setattr(forward_module, "mfe_column", lambda *_: "BBB_Close")
    monkeypatch.setattr(forward_module, "mae_column", lambda *_: "BBB_Close")
    forward_output = repository.run_directory(forward_id) / "results"
    forward_output.mkdir(exist_ok=True)
    forward_summary = forward_module.run_forward_simulation(
        repository.load_spec(forward_id), forward_output
    )
    assert forward_summary["total_signals"] == 1
    observations = pd.read_csv(forward_output / "forward_observations.csv")
    assert observations["source_end_to_end_run_id"].tolist() == [derived_id]
    assert observations["source_model_id"].tolist() == [
        derived_snapshot["models"][0]["source_model_id"]
    ]
    monkeypatch.setattr(
        forward_module, "predict_probabilities",
        lambda *_args: pytest.fail("completed Forward model was evaluated again"),
    )
    assert forward_module.run_forward_simulation(
        repository.load_spec(forward_id), forward_output
    )["total_signals"] == 1
    model = derived_snapshot["models"][0]
    booster = (
        output / "forward_model_snapshot" / model["source_model_id"] / "up.ubj"
    )
    booster.write_bytes(b"corrupt")
    with pytest.raises(ValueError, match="model artifact has changed"):
        forward_module.validate_forward_snapshot(
            repository, repository.load_spec(forward_id)
        )
    assert repository.status(derived_id)["status"] == "completed"


def test_derived_forward_without_candidates_completes_its_own_child(
    tmp_path, monkeypatch,
):
    repository, source_id, _, _ = _source(tmp_path)
    with pytest.raises(ValueError, match="requires an end date"):
        build_derived_spec(
            repository, source_id, "threshold_calibration",
            {
                "threshold_calibration_min_robust_signals": 15,
                "forward_simulation_mode": "custom_end_date",
            },
            forward_enabled=True,
        )
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
        forward_enabled=True,
    )
    derived_id = repository.create(spec)
    monkeypatch.setattr(
        forward_module, "_promotion_guidance",
        lambda *_: pd.DataFrame(columns=["Statut promotion"]),
    )
    monkeypatch.setattr(
        forward_module, "MarketDataService",
        lambda: pytest.fail("derived Forward consulted the market cache"),
    )

    def execute(repo, child_id):
        repo.run_directory(child_id).joinpath("results").mkdir(exist_ok=True)
        repo.write_json(child_id, "results/selected_thresholds_by_set.json", {})
        repo.write_json(child_id, "results/run_configuration.json", {})
        from rstock.application.run_storage import _ESSENTIAL_FILES
        for relative in _ESSENTIAL_FILES[JobType.THRESHOLD_CALIBRATION]:
            path = repo.run_directory(child_id) / relative
            if not path.exists():
                path.write_text("{}" if path.suffix == ".json" else "x\n")
        _completed(repo, child_id)

    result = run_end_to_end(
        spec, repository.run_directory(derived_id) / "results", None, None,
        execute_reserved_child=execute, phase_callback=lambda *a, **k: None,
    )
    child_id = result["forward_simulation"]["child_run_id"]
    child_output = repository.run_directory(child_id) / "results"
    child_output.mkdir(exist_ok=True)
    summary = forward_module.run_forward_simulation(
        repository.load_spec(child_id), child_output
    )
    assert summary["source_end_to_end_run_id"] == derived_id
    assert summary["result"] == "skipped_no_models"
    _completed(repository, child_id)
    _completed(repository, derived_id)
    source_wf = repository.load_spec(derived_id).derivation.inherited_stages[
        "walk_forward"
    ].source_run_id
    assert RunStorageService(repository).purge(derived_id)["state"] == "purged"
    assert repository.storage(source_wf)["state"] == "full"


def test_derived_forward_setup_error_keeps_parent_purgeable(tmp_path, monkeypatch):
    repository, source_id, _, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {
            "threshold_calibration_min_robust_signals": 15,
            "forward_simulation_mode": "custom_end_date",
            "forward_simulation_end_date": "2026-09-25",
        },
        forward_enabled=True,
    )
    derived_id = repository.create(spec)
    monkeypatch.setattr(
        forward_module, "_promotion_guidance",
        lambda *_: pd.DataFrame(columns=["Statut promotion"]),
    )

    def execute(repo, child_id):
        from rstock.application.run_storage import _ESSENTIAL_FILES
        for relative in _ESSENTIAL_FILES[JobType.THRESHOLD_CALIBRATION]:
            path = repo.run_directory(child_id) / relative
            path.parent.mkdir(exist_ok=True)
            path.write_text("{}" if path.suffix == ".json" else "x\n")
        _completed(repo, child_id)

    result = run_end_to_end(
        spec, repository.run_directory(derived_id) / "results", None, None,
        execute_reserved_child=execute, phase_callback=lambda *a, **k: None,
    )
    assert result["forward_simulation"]["status"] == "not_started"
    manifest = repository.read_json(derived_id, PIPELINE_MANIFEST)
    forward_id = manifest["stages"][-1]["child_run_id"]
    assert not repository.run_directory(forward_id).exists()
    _completed(repository, derived_id)
    assert RunStorageService(repository).purge(derived_id)["state"] == "purged"


def test_derived_threshold_uses_frozen_snapshot_override_and_resume(tmp_path, monkeypatch):
    repository, source_id, source_manifest, digest = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    assert spec.derivation.overrides[0].old_value == 20
    assert spec.derivation.overrides[0].new_value == 15
    derived_id = repository.create(spec)
    source_wf = source_manifest["stages"][0]["child_run_id"]
    source_before = hashlib.sha256(
        repository.run_directory(source_wf).joinpath("summary.json").read_bytes()
    ).hexdigest()

    class NoMarket:
        def load(self, *args, **kwargs):
            pytest.fail("derived calibration consulted the current market cache")

    monkeypatch.setattr(workflows, "MarketDataService", NoMarket)
    calls = []

    def execute(repo, child_id):
        calls.append(child_id)
        child = repo.load_spec(child_id)
        assert child.prepared_snapshot_required
        assert child.source_walk_forward_run == source_wf
        assert child.frozen_threshold_calibration_parameters[
            "threshold_calibration_min_robust_signals"
        ] == 15
        assert child.experimental_overrides[0]["old_value"] == 20
        prepared, *_ = workflows._prepared_inputs(child, None, None)
        assert prepared_dataset_hash(prepared) == digest
        results = repo.run_directory(child_id) / "results"
        results.mkdir(exist_ok=True)
        (results / "selected_thresholds_by_set.json").write_text("{}")
        (results / "run_configuration.json").write_text("{}")
        from rstock.application.run_storage import _ESSENTIAL_FILES
        for relative in _ESSENTIAL_FILES[JobType.THRESHOLD_CALIBRATION]:
            path = repo.run_directory(child_id) / relative
            if not path.exists():
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("{}" if path.suffix == ".json" else "x\n")
        _completed(repo, child_id)

    output = repository.run_directory(derived_id) / "results"
    result = run_end_to_end(
        spec, output, None, None,
        execute_reserved_child=execute, phase_callback=lambda *a, **k: None,
    )
    assert len(calls) == 1
    assert result["stage_run_ids"]["walk_forward"] == source_wf
    assert result["stage_run_ids"]["threshold_calibration"] == calls[0]
    assert hashlib.sha256(
        repository.run_directory(source_wf).joinpath("summary.json").read_bytes()
    ).hexdigest() == source_before
    child_spec = repository.load_spec(calls[0])
    monkeypatch.setattr(
        workflows, "_prepared_calibration_population",
        lambda *args: (pd.DataFrame(), pd.DataFrame(), True),
    )
    monkeypatch.setattr(
        workflows, "_resolve_threshold_xgboost_parameters",
        lambda *args: SimpleNamespace(up={}, down={}, source="frozen_snapshot"),
    )

    def capture_calibrator(prepared, generated, effective_config, **kwargs):
        assert effective_config.threshold_calibration_min_robust_signals == 15
        raise RuntimeError("calibrator received 15")

    monkeypatch.setattr(
        workflows, "run_controlled_threshold_calibration", capture_calibrator
    )
    with pytest.raises(RuntimeError, match="calibrator received 15"):
        workflows._threshold_calibration(child_spec, output, None, None)
    run_end_to_end(
        spec, output, None, None,
        execute_reserved_child=lambda repo, child_id: None,
        phase_callback=lambda *a, **k: None,
    )
    assert len(calls) == 1
    _completed(repository, derived_id)
    assert RunStorageService(repository).purge(derived_id)["state"] == "purged"
    assert repository.storage(source_wf)["state"] == "full"
    assert repository.run_directory(source_wf).joinpath(
        "checkpoints", "artifacts", "prepared_snapshot.pkl"
    ).is_file()


def test_derived_preflight_rejects_missing_or_corrupt_snapshot(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path)
    source_wf = manifest["stages"][0]["child_run_id"]
    snapshot = (
        repository.run_directory(source_wf)
        / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
    )
    snapshot.write_bytes(snapshot.read_bytes() + b"corrupt")
    with pytest.raises(ValueError, match="corrupt"):
        build_derived_spec(
            repository, source_id, "threshold_calibration",
            {"threshold_calibration_min_robust_signals": 15},
        )
    snapshot.unlink()
    with pytest.raises(ValueError, match="missing or unreadable"):
        build_derived_spec(
            repository, source_id, "threshold_calibration",
            {"threshold_calibration_min_robust_signals": 15},
        )


def test_preflight_rejects_logically_changed_snapshot_even_with_valid_sidecar(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path)
    source_wf = manifest["stages"][0]["child_run_id"]
    snapshot = (
        repository.run_directory(source_wf)
        / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
    )
    payload = pickle.loads(snapshot.read_bytes())
    payload["prepared"].iloc[0, 0] = 999.0
    raw = pickle.dumps(payload, protocol=pickle.HIGHEST_PROTOCOL)
    snapshot.write_bytes(raw)
    sidecar = snapshot.with_suffix(".json")
    metadata = json.loads(sidecar.read_text())
    metadata["sha256"] = hashlib.sha256(raw).hexdigest()
    sidecar.write_text(json.dumps(metadata))
    with pytest.raises(ValueError, match="prepared_dataset_digest_mismatch"):
        build_derived_spec(
            repository, source_id, "threshold_calibration",
            {"threshold_calibration_min_robust_signals": 15},
        )


def test_derived_reference_blocks_source_purge_and_preserves_own_root(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    derived_id = repository.create(spec)
    storage = RunStorageService(repository)
    assert not storage.eligibility(source_id).eligible
    assert "dérivé" in storage.eligibility(source_id).reason
    source_wf = manifest["stages"][0]["child_run_id"]
    assert not storage.eligibility(source_wf).eligible
    assert repository.run_metadata(derived_id).parent_run_id is None


def test_derived_resume_reuses_reserved_child_after_interruption(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    derived_id = repository.create(spec)
    output = repository.run_directory(derived_id) / "results"
    first_child = []

    def interrupted(repo, child_id):
        first_child.append(child_id)
        raise RuntimeError("interrupted after child reservation")

    with pytest.raises(RuntimeError, match="interrupted"):
        run_end_to_end(
            spec, output, None, None,
            execute_reserved_child=interrupted,
            phase_callback=lambda *a, **k: None,
        )
    completed = []

    def finish(repo, child_id):
        completed.append(child_id)
        results = repo.run_directory(child_id) / "results"
        results.mkdir(exist_ok=True)
        (results / "selected_thresholds_by_set.json").write_text("{}")
        (results / "run_configuration.json").write_text("{}")
        _completed(repo, child_id)

    run_end_to_end(
        spec, output, None, None,
        execute_reserved_child=finish,
        phase_callback=lambda *a, **k: None,
    )
    assert completed == first_child
    assert repository.load_spec(first_child[0]).fingerprint == (
        repository.configuration_fingerprint(first_child[0])
    )


def test_snapshot_change_after_derivation_is_rejected(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    derived_id = repository.create(spec)
    source_wf = manifest["stages"][0]["child_run_id"]
    snapshot = (
        repository.run_directory(source_wf)
        / "checkpoints" / "artifacts" / "prepared_snapshot.pkl"
    )
    snapshot.write_bytes(snapshot.read_bytes() + b"changed")
    with pytest.raises(ValueError, match="snapshot has changed"):
        run_end_to_end(
            spec, repository.run_directory(derived_id) / "results", None, None,
            execute_reserved_child=lambda *a: pytest.fail("child started"),
            phase_callback=lambda *a, **k: None,
        )


def test_inherited_artifact_change_never_starts_recomputed_child(tmp_path):
    repository, source_id, manifest, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    derived_id = repository.create(spec)
    source_wf = manifest["stages"][0]["child_run_id"]
    artifact = repository.run_directory(source_wf) / "results" / "qualification.csv"
    artifact.write_text("changed\n")
    with pytest.raises(ValueError, match="artefacts|artifacts|changed"):
        run_end_to_end(
            spec, repository.run_directory(derived_id) / "results", None, None,
            execute_reserved_child=lambda *a: pytest.fail("child started"),
            phase_callback=lambda *a, **k: None,
        )


def test_derived_submission_creates_new_root_with_independent_identity(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)

    class Backend:
        def launch(self, root, run_id, max_jobs):
            return 12345

    service = RunService(repository, backend=Backend())
    result = service.create_derived(
        source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    assert result.created
    assert result.run_id != source_id
    assert repository.run_metadata(result.run_id).root_run_id == result.run_id
    assert repository.run_metadata(result.run_id).parent_run_id is None
    assert repository.load_spec(result.run_id).derivation.source_end_to_end_run_id == source_id
    assert not RunStorageService(repository).eligibility(source_id).eligible
    detail = service.get(result.run_id)
    rows = pipeline_stage_rows(detail["pipeline_stages"])
    assert [row["Origine"] for row in rows[:4]] == [
        "Héritée", "Héritée", "Héritée", "Recalculée"
    ]
    assert rows[-1]["Origine"] == "Non exécutée"


def test_derived_parent_resume_keeps_same_identity(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)
    spec = build_derived_spec(
        repository, source_id, "threshold_calibration",
        {"threshold_calibration_min_robust_signals": 15},
    )
    derived_id = repository.create(spec)
    repository.transition(derived_id, JobStatus.RUNNING)
    repository.transition(derived_id, JobStatus.FAILED, error="interrupted")

    class Backend:
        def launch(self, root, run_id, max_jobs):
            assert run_id == derived_id
            return 12345

    service = RunService(repository, backend=Backend())
    resumed = service.resume(derived_id)
    assert resumed.run_id == derived_id
    assert repository.status(derived_id)["status"] == "pending"
    assert repository.status(source_id)["status"] == "completed"


def test_creation_and_purge_share_one_lock(tmp_path):
    repository, source_id, _, _ = _source(tmp_path)
    launch_entered = Event()
    release_launch = Event()
    purge_finished = Event()
    outcomes = {}

    class Backend:
        def launch(self, root, run_id, max_jobs):
            launch_entered.set()
            assert release_launch.wait(5)
            return 12345

    service = RunService(repository, backend=Backend())

    def create():
        outcomes["created"] = service.create_derived(
            source_id, "threshold_calibration",
            {"threshold_calibration_min_robust_signals": 15},
        ).run_id

    def purge():
        try:
            RunStorageService(repository).purge(source_id)
        except ValueError as error:
            outcomes["purge_error"] = str(error)
        finally:
            purge_finished.set()

    creator = Thread(target=create)
    remover = Thread(target=purge)
    creator.start()
    assert launch_entered.wait(5)
    remover.start()
    assert not purge_finished.wait(0.1)
    release_launch.set()
    creator.join(timeout=5)
    remover.join(timeout=5)
    assert outcomes["created"] != source_id
    assert "dérivé" in outcomes["purge_error"]


@pytest.mark.parametrize(
    ("fork", "changes", "expected_new"),
    [
        ("xgboost_calibration", {"combinations_per_target": 2}, 3),
        ("threshold_parameter_calibration",
         {"threshold_parameter_calibration_max_models": 100}, 2),
    ],
)
def test_derived_upstream_fork_executes_only_its_downstream_graph(
    tmp_path, fork, changes, expected_new,
):
    repository, source_id, source_manifest, _ = _source(tmp_path)
    spec = build_derived_spec(repository, source_id, fork, changes)
    derived_id = repository.create(spec)
    called = []

    def execute(repo, child_id):
        called.append(child_id)
        child = repo.load_spec(child_id)
        assert child.prepared_snapshot_required
        result_dir = repo.run_directory(child_id) / "results"
        result_dir.mkdir(exist_ok=True)
        from rstock.application.end_to_end import REQUIRED_ARTIFACTS
        key = next(
            stage for stage, job_type, _ in SCIENTIFIC_STAGES
            if job_type is child.job_type
        )
        for relative in REQUIRED_ARTIFACTS[key]:
            path = repo.run_directory(child_id) / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("{}" if path.suffix == ".json" else "x\n")
        if child.job_type is JobType.XGBOOST_CALIBRATION:
            repo.write_json(child_id, "results/selected_configurations.json", {
                direction: {"parameters": {
                    "max_depth": 2, "eta": 0.05, "num_boost_round": 20,
                }} for direction in ("Up", "Down")
            })
        if child.job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION:
            repo.write_json(
                child_id, "results/selected_threshold_calibration_configuration.json",
                {"parameters": ThresholdCalibrationParameters.from_config(
                    child.config
                ).as_dict()},
            )
        _completed(repo, child_id)

    result = run_end_to_end(
        spec, repository.run_directory(derived_id) / "results", None, None,
        execute_reserved_child=execute, phase_callback=lambda *a, **k: None,
    )
    assert len(called) == expected_new
    assert result["stage_run_ids"]["walk_forward"] == (
        source_manifest["stages"][0]["child_run_id"]
    )
    assert all(child_id not in {
        stage["child_run_id"] for stage in source_manifest["stages"]
    } for child_id in called)
