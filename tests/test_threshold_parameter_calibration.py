from dataclasses import replace
import json

import numpy as np
import pandas as pd

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.workflows import _resolve_threshold_calibration_config
from rstock.combinations import generate_symbol_sets
from rstock.calibration_sampling import GLOBAL_STRATIFIED_V2
from rstock.config import DEFAULT_CONFIG
from rstock.modeling import XGBoostParameters
from rstock.threshold_parameter_calibration import (
    ThresholdCalibrationParameters,
    default_threshold_parameter_candidates,
    frozen_threshold_parameter_candidates,
    rank_threshold_parameter_configurations,
    run_threshold_parameter_calibration,
    threshold_parameter_table,
    write_threshold_parameter_calibration_results,
)


def _config(tmp_path, **changes):
    values = {
        "project_root": tmp_path,
        "threshold_calibration_min_signals_per_window": 1,
        "threshold_calibration_min_robust_signals": 1,
        "threshold_calibration_min_window_fraction": 0.5,
        "final_holdout_size": 3,
        **changes,
    }
    return replace(DEFAULT_CONFIG, **values)


def _predictions() -> pd.DataFrame:
    rows = []
    dates = pd.bdate_range("2025-01-01", periods=8)
    for direction in ("Up", "Down"):
        for window in (1, 2):
            for position, date in enumerate(dates[(window - 1) * 4 : window * 4]):
                favorable = position % 2 == 0
                realised = 0.02 if favorable else -0.02
                if direction == "Down":
                    realised = -realised
                rows.append({
                    "Set": "AAA<-BBB",
                    "Observation": "AAA",
                    "Direction": direction,
                    "Window": window,
                    "TrainEnd": date - pd.Timedelta(days=1),
                    "Date": date,
                    "Probability": 0.8 if favorable else 0.2,
                    "Target": int(favorable),
                    "IntradayReturn": realised,
                    "MFE": 0.03,
                    "MAE": -0.01,
                })
    return pd.DataFrame(rows)


def test_new_job_type_and_threshold_lineage_round_trip(tmp_path):
    config = _config(tmp_path, threshold_parameter_calibration_max_models=248)
    frozen = ThresholdCalibrationParameters.from_config(config).as_dict()
    spec = ExperimentSpec(
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
        source_walk_forward_run="wf-parent",
        source_xgboost_calibration_run="xgb-parent",
        source_threshold_parameter_calibration_run="threshold-parameter-parent",
        frozen_threshold_calibration_parameters=frozen,
    )

    restored = ExperimentSpec.from_dict(spec.to_dict())

    assert restored.job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION
    assert restored.source_threshold_parameter_calibration_run == (
        "threshold-parameter-parent"
    )
    assert restored.frozen_threshold_calibration_parameters == frozen
    assert len(restored.frozen_threshold_calibration_parameters_sha256) == 64
    assert restored.config.threshold_parameter_calibration_max_models == 248


def test_historical_snapshot_without_threshold_parameter_lineage_stays_valid(tmp_path):
    snapshot = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        _config(tmp_path),
        symbols=("AAA", "BBB"),
    ).to_dict()
    snapshot.pop("source_threshold_parameter_calibration_run")
    snapshot.pop("frozen_threshold_calibration_parameters")
    snapshot.pop("frozen_threshold_calibration_parameters_sha256")
    snapshot.pop("source_experiment_run")

    restored = ExperimentSpec.from_dict(snapshot)

    assert restored.source_threshold_parameter_calibration_run is None
    assert restored.source_experiment_run is None
    assert restored.frozen_threshold_calibration_parameters is None
    assert restored.frozen_threshold_calibration_parameters_sha256 is None


def test_candidate_generation_is_stable_contains_baseline_and_has_no_duplicates(tmp_path):
    config = _config(tmp_path)

    first = default_threshold_parameter_candidates(config)
    second = default_threshold_parameter_candidates(config)
    table = threshold_parameter_table(first)

    assert first == second
    assert first[0] == ThresholdCalibrationParameters.from_config(config)
    assert table.iloc[0]["Configuration"] == "baseline"
    assert table["ConfigurationDigest"].is_unique
    assert len(table) <= 11


def test_resumed_candidate_search_reuses_the_frozen_candidate_snapshot(
    monkeypatch, tmp_path
):
    config = _config(tmp_path)
    first = frozen_threshold_parameter_candidates(tmp_path, config)
    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.default_threshold_parameter_candidates",
        lambda config: (_ for _ in ()).throw(
            AssertionError("resume must not regenerate candidates")
        ),
    )

    resumed = frozen_threshold_parameter_candidates(
        tmp_path, replace(config, threshold_calibration_min_signals_per_window=99)
    )

    assert resumed == first


def test_ranking_is_deterministic_and_uses_stable_configuration_tie_break():
    rows = pd.DataFrame([
        {
            "Configuration": name,
            "EligibleModelPct": 0.5,
            "PrecisionMedian": 0.6,
            "F1Median": 0.55,
            "WindowCoverageMedian": 1.0,
            "PrecisionStdMedian": 0.1,
            "DirectionalReturnMeanMedian": 0.01,
            "DirectionalReturnStdMedian": 0.02,
            "OppositeMoveFrequencyMedian": 0.2,
            "TotalSignals": 20,
            "EligibleConfiguration": True,
            "Selected": False,
            "Rank": 0,
            "SelectionReason": None,
            "RejectionReason": None,
        }
        for name in ("candidate_b", "candidate_a")
    ])

    first = rank_threshold_parameter_configurations(rows)
    second = rank_threshold_parameter_configurations(rows.iloc[::-1])

    assert first.loc[0, "Configuration"] == "candidate_a"
    assert second.loc[0, "Configuration"] == "candidate_a"
    assert first.loc[0, "Selected"]


def test_runner_reuses_probabilities_and_never_uses_holdout_for_selection(
    monkeypatch, tmp_path
):
    calls = []
    prepared = pd.DataFrame(
        {"placeholder": np.arange(20)},
        index=pd.bdate_range("2025-01-01", periods=20),
    )

    def fake_probabilities(development, *args, **kwargs):
        calls.append(development.index.max())
        return _predictions()

    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.generate_development_probabilities",
        fake_probabilities,
    )
    config = _config(tmp_path)
    baseline = ThresholdCalibrationParameters.from_config(config)
    result = run_threshold_parameter_calibration(
        prepared,
        generate_symbol_sets(["AAA", "BBB"], 1),
        config,
        xgboost_parameters_by_direction={
            "Up": XGBoostParameters(2, 0.05, 20),
            "Down": XGBoostParameters(3, 0.1, 30),
        },
        xgboost_parameter_source="frozen_snapshot",
        combinations_per_target=1,
        candidates=[baseline, replace(baseline, precision_tolerance=0.0)],
        source_parent_run="wf-parent",
        source_walk_forward_run="wf-parent",
    )

    assert calls == [prepared.index[-4]]
    assert result.run_configuration["holdout_used_for_selection"] is False
    assert result.run_configuration["holdout_evaluated"] is False
    assert result.run_configuration["development_probability_rows"] == len(
        _predictions()
    )
    assert len(result.development_by_configuration) == 2
    assert result.selected_configuration["parent_run"] == "wf-parent"


def test_v2_directional_model_cap_counts_complete_up_down_pairs(
    monkeypatch, tmp_path
):
    sampled_sizes = []

    def fake_probabilities(development, sampled, *args, **kwargs):
        sampled_sizes.append(len(sampled))
        return _predictions()

    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.generate_development_probabilities",
        fake_probabilities,
    )
    result = run_threshold_parameter_calibration(
        pd.DataFrame(
            {"placeholder": np.arange(20)},
            index=pd.bdate_range("2025-01-01", periods=20),
        ),
        generate_symbol_sets(["AAA", "BBB", "CCC"], 1),
        _config(tmp_path),
        xgboost_parameters_by_direction={
            "Up": XGBoostParameters(2, 0.05, 20),
            "Down": XGBoostParameters(3, 0.1, 30),
        },
        xgboost_parameter_source="frozen_snapshot",
        combinations_per_target=1,
        sampling_policy=GLOBAL_STRATIFIED_V2,
        max_directional_models=4,
    )

    assert sampled_sizes == [2]
    manifest = result.run_configuration["sampling_manifest"]
    assert manifest["sampled_directional_models"] == 4
    assert manifest["sample_size"] == 2
    assert manifest["complete_direction_pairs"] is True


def test_holdout_values_cannot_affect_threshold_parameter_ranking(monkeypatch, tmp_path):
    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.generate_development_probabilities",
        lambda development, *args, **kwargs: _predictions(),
    )
    index = pd.bdate_range("2025-01-01", periods=20)
    first = pd.DataFrame({"value": np.arange(20, dtype=float)}, index=index)
    second = first.copy()
    second.iloc[-3:, 0] = [-1_000_000.0, 0.0, 1_000_000.0]
    config = _config(tmp_path)
    arguments = {
        "generated_sets": generate_symbol_sets(["AAA", "BBB"], 1),
        "config": config,
        "xgboost_parameters_by_direction": {
            "Up": XGBoostParameters(2, 0.05, 20),
            "Down": XGBoostParameters(3, 0.1, 30),
        },
        "xgboost_parameter_source": "rstock_config",
        "combinations_per_target": 1,
    }

    first_result = run_threshold_parameter_calibration(first, **arguments)
    second_result = run_threshold_parameter_calibration(second, **arguments)

    assert first_result.selected_configuration["configuration"] == (
        second_result.selected_configuration["configuration"]
    )
    assert first_result.selected_configuration["configuration_sha256"] == (
        second_result.selected_configuration["configuration_sha256"]
    )
    pd.testing.assert_frame_equal(
        first_result.development_by_configuration,
        second_result.development_by_configuration,
    )


def test_artifacts_persist_winner_metrics_lineage_and_no_holdout(monkeypatch, tmp_path):
    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.generate_development_probabilities",
        lambda *args, **kwargs: _predictions(),
    )
    config = _config(tmp_path)
    result = run_threshold_parameter_calibration(
        pd.DataFrame(
            {"placeholder": np.arange(20)},
            index=pd.bdate_range("2025-01-01", periods=20),
        ),
        generate_symbol_sets(["AAA", "BBB"], 1),
        config,
        xgboost_parameters_by_direction={
            "Up": XGBoostParameters(2, 0.05, 20),
            "Down": XGBoostParameters(3, 0.1, 30),
        },
        xgboost_parameter_source="frozen_snapshot",
        combinations_per_target=1,
        source_parent_run="xgb-parent",
        source_walk_forward_run="wf-parent",
        source_xgboost_calibration_run="xgb-parent",
        frozen_xgboost_parameters_sha256="xgb-digest",
    )
    result.run_configuration["traceability"] = {"prepared_dataset_sha256": "data"}
    write_threshold_parameter_calibration_results(result, tmp_path)

    selected = json.loads(
        (tmp_path / "selected_threshold_calibration_configuration.json").read_text(
            encoding="utf-8"
        )
    )
    configuration = json.loads(
        (tmp_path / "run_configuration.json").read_text(encoding="utf-8")
    )
    assert selected["parent_run"] == "xgb-parent"
    assert selected["configuration_sha256"]
    assert configuration["holdout_used_for_selection"] is False
    assert configuration["traceability"]["prepared_dataset_sha256"] == "data"
    assert (tmp_path / "tested_threshold_parameter_configurations.csv").exists()
    assert (tmp_path / "development_metrics_by_configuration.csv").exists()
    assert (tmp_path / "development_metrics_by_window.csv").exists()


def test_threshold_calibration_consumes_frozen_auto_calibration_not_current_config(tmp_path):
    current = _config(
        tmp_path,
        threshold_calibration_min_signals_per_window=99,
        threshold_calibration_precision_tolerance=0.5,
    )
    frozen = ThresholdCalibrationParameters.from_config(
        _config(
            tmp_path,
            threshold_calibration_min_signals_per_window=7,
            threshold_calibration_precision_tolerance=0.02,
        )
    ).as_dict()
    spec = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        current,
        symbols=("AAA", "BBB"),
        source_threshold_parameter_calibration_run="auto-parent",
        frozen_threshold_calibration_parameters=frozen,
        frozen_xgboost_parameters={
            "Up": XGBoostParameters(2, 0.05, 20).as_dict(),
            "Down": XGBoostParameters(3, 0.1, 30).as_dict(),
        },
    )

    effective, source = _resolve_threshold_calibration_config(spec)

    assert source == "frozen_snapshot"
    assert effective.threshold_calibration_min_signals_per_window == 7
    assert effective.threshold_calibration_precision_tolerance == 0.02
    assert spec.frozen_xgboost_parameters["Up"]["max_depth"] == 2


def test_threshold_calibration_can_load_referenced_auto_calibration_artifact(tmp_path):
    config = _config(tmp_path)
    repository = RunRepository(tmp_path / "runs")
    parent = repository.create(ExperimentSpec(
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
    ))
    results = repository.run_directory(parent) / "results"
    results.mkdir()
    parameters = ThresholdCalibrationParameters.from_config(
        replace(config, threshold_calibration_min_signals_per_window=6)
    )
    (results / "selected_threshold_calibration_configuration.json").write_text(
        json.dumps({"parameters": parameters.as_dict()}), encoding="utf-8"
    )
    repository.transition(parent, JobStatus.RUNNING)
    repository.transition(parent, JobStatus.COMPLETED)
    child = ExperimentSpec(
        JobType.THRESHOLD_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
        source_threshold_parameter_calibration_run=parent,
    )

    effective, source = _resolve_threshold_calibration_config(child)

    assert source == "referenced_calibration"
    assert effective.threshold_calibration_min_signals_per_window == 6


def test_restart_and_resume_keep_the_same_threshold_parameter_spec(tmp_path):
    class Backend:
        def __init__(self):
            self.launches = []

        def launch(self, root, run_id, maximum):
            self.launches.append(run_id)
            return 123

    repository = RunRepository(tmp_path / "runs")
    frozen = ThresholdCalibrationParameters.from_config(_config(tmp_path)).as_dict()
    source_spec = ExperimentSpec(
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        _config(tmp_path),
        symbols=("AAA", "BBB"),
        source_walk_forward_run="wf-parent",
        source_xgboost_calibration_run="xgb-parent",
        frozen_threshold_calibration_parameters=frozen,
    )
    source = repository.create(source_spec)
    repository.transition(source, JobStatus.RUNNING)
    repository.transition(source, JobStatus.FAILED, error="interrupted")
    backend = Backend()
    service = RunService(repository, backend=backend)

    resumed = service.resume(source)

    assert resumed.run_id == source
    assert repository.load_spec(source).to_dict() == source_spec.to_dict()
    repository.transition(source, JobStatus.FAILED, error="again")
    restarted = service.restart(source)
    assert restarted.run_id != source
    assert repository.load_spec(restarted.run_id).to_dict() == source_spec.to_dict()


def test_application_workflow_persists_traceability_and_selected_artifacts(
    monkeypatch, tmp_path
):
    from rstock.application import workflows

    captured: dict[str, object] = {}
    original_runner = workflows.run_threshold_parameter_calibration

    def capture_runner(*args, **kwargs):
        captured.update(kwargs)
        return original_runner(*args, **kwargs)

    prepared = pd.DataFrame(
        {"placeholder": np.arange(20)},
        index=pd.bdate_range("2025-01-01", periods=20),
    )
    prepared.attrs["effective_end_date"] = prepared.index[-1].isoformat()
    prepared.attrs["symbols_used"] = 2
    generated = generate_symbol_sets(["AAA", "BBB"], 1)
    monkeypatch.setattr(
        workflows,
        "_prepared_experiment",
        lambda *args, **kwargs: (prepared, generated, {}),
    )
    monkeypatch.setattr(
        workflows, "_qualified_sets_from_walk_forward_source", lambda spec: None
    )
    monkeypatch.setattr(
        "rstock.threshold_parameter_calibration.generate_development_probabilities",
        lambda *args, **kwargs: _predictions(),
    )
    monkeypatch.setattr(workflows, "run_threshold_parameter_calibration", capture_runner)
    spec = ExperimentSpec(
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        _config(tmp_path, threshold_parameter_calibration_max_models=2),
        symbols=("AAA", "BBB"),
        combinations_per_target=1,
    )
    output = tmp_path / "output"

    summary = workflows._threshold_parameter_calibration(spec, output, None, None)

    run_configuration = json.loads(
        (output / "run_configuration.json").read_text(encoding="utf-8")
    )
    assert summary["job_type"] == "threshold_parameter_calibration"
    assert summary["selected_configuration"]["configuration"]
    assert run_configuration["traceability"]["prepared_dataset_sha256"]
    assert run_configuration["holdout_used_for_selection"] is False
    assert captured["max_directional_models"] == 2
    assert (output / "selected_threshold_calibration_configuration.json").exists()
