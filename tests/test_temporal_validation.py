import json
from dataclasses import replace

import pandas as pd
import pytest

import rstock.application.temporal_validation as temporal_validation
from rstock.application.domain import ExperimentSpec, JobType, RunMetadata, RunPurpose, RunRole
from rstock.application.repository import RunRepository
from rstock.application.temporal_validation import (
    BOOTSTRAP_METHOD_VERSION,
    TEMPORAL_VALIDATION_CHECKPOINT,
    TEMPORAL_VALIDATION_RESULT,
    TemporalValidationRunner,
    _auc_gate_status,
    _bootstrap,
    _bootstrap_seed,
    _ci_gate_status,
    _directional_returns,
    _final_status,
    _gate_yield,
    _precision_metrics,
    _single_candidate_yield,
    _yield_gate_status,
)
from rstock.config import DEFAULT_CONFIG


def _spec(tmp_path, **changes):
    config = changes.pop("config", replace(DEFAULT_CONFIG, project_root=tmp_path))
    return ExperimentSpec(
        job_type=JobType.END_TO_END,
        config=config,
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        **changes,
    )


def _write_threshold_results(repository, run_id, *, auc):
    directory = repository.run_directory(run_id) / "results"
    directory.mkdir(parents=True)
    selected = {"AAA<-BBB": {"Up": {"status": "selected", "threshold": 0.62}}}
    (directory / "selected_thresholds_by_set.json").write_text(json.dumps(selected), encoding="utf-8")
    metrics = pd.DataFrame([{
        "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up", "Threshold": 0.62,
        "SignalCount": 20, "ROCAUC": auc, "Precision": 0.65,
        "DirectionalReturnMean": 0.02, "OppositeMoveFrequency": 0.2,
    }])
    metrics.to_csv(directory / "threshold_metrics_by_set.csv", index=False)
    metrics.to_csv(directory / "holdout_metrics.csv", index=False)
    rows = []
    for index in range(8):
        rows.append({
            "Set": "AAA<-BBB", "Observation": "AAA", "Direction": "Up",
            "Date": f"2026-01-{index + 1:02d}", "Probability": 0.8,
            "Target": 1 if index % 2 == 0 else 0, "IntradayReturn": 0.02 if index % 2 == 0 else -0.01,
            "MFE": 0.02, "MAE": -0.01, "Threshold": 0.62,
            "Prediction": 1 if index % 2 == 0 else 0,
        })
    pd.DataFrame(rows).to_csv(directory / "holdout_predictions.csv", index=False)


def _fixture(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    reference_id = repository.create(
        _spec(tmp_path, temporal_validation_enabled=True),
        metadata=RunMetadata(run_role=RunRole.PIPELINE_PARENT, run_purpose=RunPurpose.REFERENCE),
    )
    validation_id = repository.create(
        _spec(
            tmp_path,
            config=replace(DEFAULT_CONFIG, project_root=tmp_path, walk_forward_end_offset_sessions=63),
            temporal_validation_enabled=False,
            auto_promote_candidates=False,
        ),
        metadata=RunMetadata(
            run_role=RunRole.PIPELINE_PARENT, run_purpose=RunPurpose.TEMPORAL_VALIDATION,
            visible_in_history=False, parent_run_id=reference_id, relation_key="temporal_validation_end_to_end",
            relation_type="temporal_validation_end_to_end", reference_run_id=reference_id,
        ),
    )
    threshold_spec = replace(_spec(tmp_path), job_type=JobType.THRESHOLD_CALIBRATION)
    reference_threshold = repository.create(
        threshold_spec,
        metadata=RunMetadata(run_role=RunRole.PIPELINE_STAGE, parent_run_id=reference_id, relation_key="pipeline_stage:threshold_calibration"),
    )
    validation_threshold = repository.create(
        threshold_spec,
        metadata=RunMetadata(run_role=RunRole.PIPELINE_STAGE, parent_run_id=validation_id, relation_key="pipeline_stage:threshold_calibration"),
    )
    for run_id, threshold_id in ((reference_id, reference_threshold), (validation_id, validation_threshold)):
        (repository.run_directory(run_id) / "orchestration").mkdir()
        repository.write_json(run_id, "orchestration/pipeline.json", {"stages": [{"stage_key": "threshold_calibration", "child_run_id": threshold_id}]})
    _write_threshold_results(repository, reference_threshold, auc=0.65)
    _write_threshold_results(repository, validation_threshold, auc=0.64)
    return repository, reference_id, validation_id, validation_threshold


def test_temporal_comparison_persists_four_passed_gates_and_reuses_checkpoint(tmp_path):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)

    first = runner.execute()
    second = runner.execute()

    assert first == second
    assert first["final_status"] == "passed"
    assert set(first["gates"]) == {"candidate_yield", "holdout_auc", "precision_edge", "directional_return"}
    assert first["gates"]["holdout_auc"]["metrics"]["validation"]["median"] == 0.64
    assert first["gates"]["precision_edge"]["metrics"]["bootstrap"]["method_version"] == BOOTSTRAP_METHOD_VERSION
    assert repository.read_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT) == first
    assert repository.read_json(reference_id, TEMPORAL_VALIDATION_RESULT) == first


def test_temporal_comparison_marks_invalid_relationship_without_promotion_data(tmp_path):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    metadata = repository.run_metadata(reference_id).to_dict()
    metadata["run_purpose"] = RunPurpose.STANDARD.value
    repository.write_json(reference_id, "metadata.json", metadata)

    result = TemporalValidationRunner(
        repository, root_run_id=reference_id, validation_run_id=validation_id
    ).execute()

    assert result["final_status"] == "invalid"
    assert "purpose reference" in result["error"]


def test_temporal_comparison_rejects_changed_inputs_after_checkpoint(tmp_path):
    repository, reference_id, validation_id, validation_threshold = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)
    first = runner.execute()
    path = repository.run_directory(validation_threshold) / "results" / "holdout_metrics.csv"
    frame = pd.read_csv(path)
    frame.loc[0, "ROCAUC"] = 0.61
    frame.to_csv(path, index=False)

    refreshed = runner.execute()

    assert refreshed["input_sha256"] != first["input_sha256"]
    assert refreshed["supersedes_input_sha256"] == first["input_sha256"]


@pytest.mark.parametrize(
    ("ratio", "expected"),
    [(1.0, "passed"), (0.24, "failed"), (None, "inconclusive")],
)
def test_candidate_yield_gate_decisions(ratio, expected):
    assert _yield_gate_status(ratio, 0.25) == expected


@pytest.mark.parametrize(
    ("reference", "validation", "expected"),
    [
        (0.60, 0.65, "passed"),
        (0.60, 0.57, "passed"),
        (0.60, 0.56, "failed"),
        (0.60, 0.50, "failed"),
        (None, 0.60, "inconclusive"),
    ],
)
def test_auc_gate_decisions(reference, validation, expected):
    assert _auc_gate_status(reference, validation, 0.03) == expected


def test_candidate_yield_compares_reference_and_validation(monkeypatch, tmp_path):
    responses = iter(
        [
            {"status": "passed", "metrics": {"ratio": 0.50}},
            {"status": "passed", "metrics": {"ratio": 0.25}},
        ]
    )
    monkeypatch.setattr(temporal_validation, "_single_candidate_yield", lambda *_: next(responses))

    gate = _gate_yield({}, tmp_path, {}, tmp_path)

    assert gate["metrics"]["ratio"] == 0.50
    assert gate["metrics"]["reference"]["candidate_yield"] == 0.50
    assert gate["metrics"]["validation"]["candidate_yield"] == 0.25


@pytest.mark.parametrize(
    ("lower", "upper", "max_width", "expected"),
    [
        (0.01, 0.10, 0.20, "passed"),
        (-0.10, -0.01, 0.20, "failed"),
        (-0.01, 0.01, 0.20, "inconclusive"),
        (0.01, 0.30, 0.20, "inconclusive"),
    ],
)
def test_precision_edge_ci_gate_decisions(lower, upper, max_width, expected):
    assert _ci_gate_status(lower, upper, 0.0, max_width=max_width) == expected


@pytest.mark.parametrize(
    ("lower", "upper", "expected"),
    [(0.01, 0.10, "passed"), (-0.10, -0.01, "failed"), (-0.01, 0.01, "inconclusive")],
)
def test_directional_return_ci_gate_has_no_width_rule(lower, upper, expected):
    assert _ci_gate_status(lower, upper, 0.0) == expected


def test_directional_return_respects_existing_up_down_convention():
    frame = pd.DataFrame(
        {"Direction": ["Up", "Down"], "IntradayReturn": [0.02, 0.02]}
    )

    assert _directional_returns(frame).tolist() == [0.02, -0.02]


def test_date_block_bootstrap_is_deterministic():
    frame = pd.DataFrame(
        {"Date": ["2026-01-01", "2026-01-01", "2026-01-02", "2026-01-03"], "value": [0.0, 1.0, 1.0, 1.0]}
    )
    statistic = lambda values: float(values["value"].mean())

    first = _bootstrap(frame, statistic, seed_material="stable", confidence_level=0.95)
    second = _bootstrap(frame, statistic, seed_material="stable", confidence_level=0.95)

    assert first == second
    assert first["block_size"] == 2


@pytest.mark.parametrize(
    ("statuses", "expected"),
    [
        (["passed"] * 4, "passed"),
        (["passed", "failed", "passed", "passed"], "failed"),
        (["passed", "inconclusive", "passed", "passed"], "inconclusive"),
        (["invalid", "failed", "inconclusive", "passed"], "invalid"),
    ],
)
def test_global_decision_priority(statuses, expected):
    gates = {str(index): {"status": status} for index, status in enumerate(statuses)}
    assert _final_status(gates) == expected


def test_interrupted_comparison_resumes_from_pending_checkpoint(tmp_path, monkeypatch):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)
    original = temporal_validation._gate_auc
    monkeypatch.setattr(temporal_validation, "_gate_auc", lambda *_: (_ for _ in ()).throw(RuntimeError("crash")))

    with pytest.raises(RuntimeError, match="crash"):
        runner.execute()
    assert repository.read_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT)["status"] == "pending"

    monkeypatch.setattr(temporal_validation, "_gate_auc", original)
    assert runner.execute()["final_status"] == "passed"


def test_candidate_yield_deduplicates_selected_holdout_pairs(monkeypatch, tmp_path):
    results = tmp_path / "results"
    results.mkdir()
    pd.DataFrame(
        [
            {"Set": "AAA<-BBB", "Direction": "Up", "Threshold": 0.62, "ROCAUC": 0.61},
            {"Set": "AAA<-BBB", "Direction": "Up", "Threshold": 0.62, "ROCAUC": 0.62},
        ]
    ).to_csv(results / "holdout_metrics.csv", index=False)
    monkeypatch.setattr(temporal_validation, "_candidate_sets", lambda *_: {"AAA<-BBB"})

    value = _single_candidate_yield(
        {"AAA<-BBB": {"Up": {"status": "selected", "threshold": 0.62}}},
        results,
    )

    assert value["metrics"]["eligible_pair_count"] == 1
    assert value["metrics"]["ratio"] == 1.0


def test_precision_edge_averages_pair_statistics_without_row_weighting():
    rows = [
        {"Set": "AAA<-BBB", "Direction": "Up", "_target": 1, "_signal": True}
        for _ in range(50)
    ] + [
        {"Set": "AAA<-BBB", "Direction": "Up", "_target": 0, "_signal": False}
        for _ in range(50)
    ] + [
        {"Set": "CCC<-DDD", "Direction": "Up", "_target": 0, "_signal": True},
        {"Set": "CCC<-DDD", "Direction": "Up", "_target": 1, "_signal": False},
    ]

    metrics = _precision_metrics(pd.DataFrame(rows))

    assert metrics == {
        "precision_signal": 0.5,
        "baseline_direction_rate": 0.5,
        "precision_edge": 0.0,
        "pair_count": 2,
    }


def test_missing_validation_predictions_makes_only_prediction_gates_inconclusive(tmp_path):
    repository, reference_id, validation_id, validation_threshold = _fixture(tmp_path)
    (repository.run_directory(validation_threshold) / "results" / "holdout_predictions.csv").unlink()

    result = TemporalValidationRunner(
        repository, root_run_id=reference_id, validation_run_id=validation_id
    ).execute()

    assert result["final_status"] == "inconclusive"
    assert result["gates"]["precision_edge"]["status"] == "inconclusive"
    assert result["gates"]["directional_return"]["status"] == "inconclusive"


def test_corrupt_validation_predictions_is_invalid(tmp_path):
    repository, reference_id, validation_id, validation_threshold = _fixture(tmp_path)
    path = repository.run_directory(validation_threshold) / "results" / "holdout_predictions.csv"
    path.write_text("invalid_column\nvalue\n", encoding="utf-8")

    result = TemporalValidationRunner(
        repository, root_run_id=reference_id, validation_run_id=validation_id
    ).execute()

    assert result["final_status"] == "invalid"


def test_checkpoint_persists_and_verifies_resolved_temporal_context(tmp_path):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)
    result = runner.execute()

    assert result["temporal_context"]["reference"] == {
        "run_id": reference_id,
        "purpose": "reference",
        "end_offset_sessions": 0,
        "effective_period": {"start": "2026-01-01", "end": "2026-01-08"},
        "effective_start": "2026-01-01",
        "effective_end": "2026-01-08",
    }
    state = repository.read_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT)
    state["temporal_context"]["validation"]["end_offset_sessions"] = 0
    repository.write_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT, state)

    with pytest.raises(ValueError, match="context is incompatible"):
        runner.execute()


def test_bootstrap_seed_uses_only_prescribed_components():
    first = _bootstrap_seed("reference", "validation", "ref-digest", "validation-digest")
    same = _bootstrap_seed("reference", "validation", "ref-digest", "validation-digest")
    changed = _bootstrap_seed("reference", "validation", "ref-digest", "changed-digest")

    assert first == same
    assert first != changed


def test_completed_legacy_checkpoint_is_not_migrated_or_recalculated(tmp_path):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)
    completed = runner.execute()
    legacy = dict(completed)
    legacy.pop("temporal_context")
    repository.write_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT, legacy)

    assert runner.execute() == legacy
    assert repository.read_json(reference_id, TEMPORAL_VALIDATION_CHECKPOINT) == legacy


def test_bootstrap_seed_ignores_non_prescribed_validation_parameters(tmp_path):
    repository, reference_id, validation_id, _ = _fixture(tmp_path)
    runner = TemporalValidationRunner(repository, root_run_id=reference_id, validation_run_id=validation_id)
    first = runner.execute()
    for run_id in (reference_id, validation_id):
        snapshot = repository.read_json(run_id, "config.json")
        snapshot["rstock_config"]["temporal_max_ci_width"] = 0.19
        repository.write_json(run_id, "config.json", snapshot)

    refreshed = runner.execute()

    assert refreshed["input_sha256"] != first["input_sha256"]
    assert (
        refreshed["gates"]["precision_edge"]["metrics"]["bootstrap"]["seed"]
        == first["gates"]["precision_edge"]["metrics"]["bootstrap"]["seed"]
    )
