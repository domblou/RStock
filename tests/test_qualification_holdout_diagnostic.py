from __future__ import annotations

import json
from dataclasses import replace
from types import SimpleNamespace

import pandas as pd

from rstock.application import workflows
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.qualification_holdout_diagnostic import (
    PROTOCOL,
    diagnostic_state,
    materialize_diagnostic,
    rejected_identities,
)
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.worker import WORKFLOW_PHASES
from rstock.combinations import symbol_set_id
from rstock.config import RStockConfig


APP = (
    __import__("pathlib").Path(__file__).parents[1]
    / "rstock"
    / "application"
    / "streamlit_app.py"
)


def _source(tmp_path, *, second_rejected: bool = False):
    repository = RunRepository(tmp_path / "runs")
    set_a = '["AAA","BBB"]'
    set_b = '["AAA","CCC"]'
    selected = {
        set_a: {"Up": {"status": "selected", "threshold": 0.4}},
        set_b: {"Up": {"status": "selected", "threshold": 0.45}},
    }
    source = ExperimentSpec(
        job_type=JobType.FORCED_CANDIDATE_VALIDATION,
        config=RStockConfig(project_root=tmp_path),
        symbols=("AAA", "BBB", "CCC"),
        forced_symbol_sets=(("AAA", "BBB"), ("AAA", "CCC")),
        forced_candidate_identities=((set_a, "Up"), (set_b, "Up")),
        frozen_xgboost_parameters={"Up": {"max_depth": 2}, "Down": {"max_depth": 2}},
        frozen_threshold_calibration_parameters={"threshold_calibration_step": 0.1},
        frozen_selected_thresholds_by_set=selected,
        source_end_to_end_run="end-to-end",
        source_xgboost_calibration_run="xgb-reference",
        source_threshold_calibration_run="threshold-reference",
    )
    forced_id = repository.create(source)
    repository.transition(forced_id, JobStatus.RUNNING)
    repository.transition(forced_id, JobStatus.COMPLETED)
    wf_id = repository.create(replace(source, job_type=JobType.WALK_FORWARD))
    results = repository.run_directory(wf_id) / "results"
    results.mkdir()
    pd.DataFrame([
        {"Set": set_a, "Observation": "AAA", "Predictors": '["BBB"]', "Eligible": False, "IneligibilityReasons": '["worst_window_auc"]', "ROCAUCMedian": .58, "ROCAUCWorst": .43, "ROCAUCStd": .06, "PctWindowsAboveRandom": .71},
        {"Set": set_b, "Observation": "AAA", "Predictors": '["CCC"]', "Eligible": not second_rejected, "IneligibilityReasons": '["median_auc"]' if second_rejected else '[]', "ROCAUCMedian": .52, "ROCAUCWorst": .46, "ROCAUCStd": .05, "PctWindowsAboveRandom": .71},
    ]).to_csv(results / "qualification.csv", index=False)
    (repository.run_directory(forced_id) / "orchestration").mkdir()
    repository.write_json(forced_id, "orchestration/pipeline.json", {
        "schema_version": 1,
        "workflow_type": JobType.FORCED_CANDIDATE_VALIDATION.value,
        "child_id_policy_version": 2,
        "root_run_id": forced_id,
        "stages": [
            {"stage_key": "walk_forward", "expected_job_type": "walk_forward", "child_run_id": wf_id, "expected_fingerprint": "wf", "dependency_run_ids": [], "artifact_digests": {}},
            {"stage_key": "fixed_candidate_evaluation", "expected_job_type": "fixed_candidate_evaluation", "child_run_id": "fixed", "expected_fingerprint": "fixed", "dependency_run_ids": [wf_id], "artifact_digests": {}},
        ],
    })
    return repository, forced_id, wf_id, set_a, set_b


def test_job_type_and_historical_snapshot_compatibility(tmp_path):
    assert JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC.implemented
    repository, forced_id, *_ = _source(tmp_path)
    values = repository.load_spec(forced_id).to_dict()
    for key in ("diagnostic_protocol", "diagnostic_only", "xgb_recalibration"):
        values.pop(key, None)
    historical = ExperimentSpec.from_dict(values)
    assert historical.diagnostic_protocol is None
    assert historical.diagnostic_only is None
    assert historical.xgb_recalibration is None


def test_selection_uses_only_persisted_wf_rejects(tmp_path):
    repository, forced_id, _, set_a, _ = _source(tmp_path)
    sets, identities = rejected_identities(repository, forced_id)
    assert sets == (("AAA", "BBB"),)
    assert identities == ((set_a, "Up"),)


def test_materialization_is_idempotent_and_freezes_diagnostic_contract(tmp_path):
    repository, forced_id, wf_id, set_a, _ = _source(tmp_path)
    first, spec, created = materialize_diagnostic(repository, forced_id)
    second, repeated, created_again = materialize_diagnostic(repository, forced_id)
    assert (first, created) == (second, True)
    assert created_again is False
    assert repeated.fingerprint == spec.fingerprint
    assert spec.job_type is JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC
    assert spec.forced_candidate_identities == ((set_a, "Up"),)
    assert spec.source_walk_forward_run == wf_id
    assert spec.source_forced_candidate_validation_run == forced_id
    assert spec.source_end_to_end_run == forced_id
    assert spec.diagnostic_protocol == PROTOCOL
    assert spec.diagnostic_only is True
    assert spec.xgb_recalibration is False
    assert spec.threshold_recalibration is False
    assert spec.walk_forward_rerun is False
    assert spec.prefilter_rerun is False
    assert spec.promotion_enabled is False
    state = diagnostic_state(repository, forced_id)
    assert state["run_id"] == first
    assert state["candidate_count"] == 1


def test_diagnostic_progress_contract_contains_every_emitted_phase():
    assert WORKFLOW_PHASES[JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC] == [
        ("data_preparation", 25),
        ("combination_generation", 5),
        ("final_holdout", 60),
        ("result_writing", 8),
        ("publishing", 2),
    ]


def test_failed_diagnostic_resumes_same_run_without_creating_a_duplicate(tmp_path):
    class Backend:
        def launch(self, _root, run_id, _limit):
            self.run_id = run_id
            return 4321

    repository, forced_id, *_ = _source(tmp_path)
    run_id, _, _ = materialize_diagnostic(repository, forced_id)
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.FAILED, error="combination_generation")
    children_before = repository.list_children(forced_id)
    backend = Backend()

    resumed = RunService(repository, backend=backend).resume(run_id)

    assert resumed.run_id == run_id
    assert backend.run_id == run_id
    assert repository.status(run_id)["status"] == JobStatus.PENDING.value
    assert repository.list_children(forced_id) == children_before == [run_id]
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.COMPLETED)
    assert repository.status(run_id)["status"] == JobStatus.COMPLETED.value


def test_fixed_diagnostic_evaluates_all_six_forced_sets_not_two_wf_qualified(
    monkeypatch, tmp_path
):
    forced_sets = tuple((f"TARGET{index}", f"PRED{index}") for index in range(6))
    set_ids = tuple(
        symbol_set_id(pd.Series({"V0": target, "V1": predictor}))
        for target, predictor in forced_sets
    )
    selected = {
        set_name: {"Up": {"status": "selected", "threshold": 0.5}}
        for set_name in set_ids
    }
    spec = ExperimentSpec(
        job_type=JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC,
        config=RStockConfig(project_root=tmp_path, final_holdout_size=2),
        symbols=tuple(symbol for item in forced_sets for symbol in item),
        forced_symbol_sets=forced_sets,
        forced_candidate_identities=tuple((set_name, "Up") for set_name in set_ids),
        frozen_xgboost_parameters={"Up": {"max_depth": 2}, "Down": {"max_depth": 2}},
        frozen_threshold_calibration_parameters={"threshold_calibration_step": 0.1},
        frozen_selected_thresholds_by_set=selected,
    )
    prepared = pd.DataFrame(
        {"value": range(8)}, index=pd.date_range("2026-01-01", periods=8)
    )
    qualified_calls = []
    evaluated_sets = []
    applied_directions = []
    progress = []

    monkeypatch.setattr(
        workflows,
        "_prepared_inputs",
        lambda *_args, **_kwargs: (prepared, (), (), {}),
    )
    monkeypatch.setattr(
        workflows,
        "_qualified_sets_from_walk_forward_source",
        lambda *_args, **_kwargs: qualified_calls.append(True)
        or pd.DataFrame([{"Set": set_ids[0]}, {"Set": set_ids[1]}]),
    )
    monkeypatch.setattr(
        workflows,
        "_resolve_threshold_xgboost_parameters",
        lambda _spec: SimpleNamespace(up={}, down={}, source="frozen_snapshot"),
    )
    monkeypatch.setattr(
        workflows,
        "_resolve_threshold_calibration_config",
        lambda _spec: (_spec.config, "frozen_snapshot"),
    )
    monkeypatch.setattr(
        workflows,
        "split_development_holdout",
        lambda frame, _size: (frame.iloc[:-2], frame.iloc[-2:], frame.index[-2]),
    )
    for forbidden in (
        "select_predictors",
        "evaluate_walk_forward",
        "run_controlled_calibration",
        "run_controlled_threshold_calibration",
        "run_threshold_parameter_calibration",
    ):
        monkeypatch.setattr(
            workflows,
            forbidden,
            lambda *_args, _name=forbidden, **_kwargs: (_ for _ in ()).throw(
                AssertionError(f"unexpected scientific stage: {_name}")
            ),
        )

    def fake_probabilities(_development, holdout, combinations, *_args, **_kwargs):
        set_name = symbol_set_id(combinations.iloc[0])
        evaluated_sets.append(set_name)
        return pd.DataFrame([
            {
                "Set": set_name,
                "Observation": json.loads(set_name)[0],
                "Direction": direction,
                "Window": 0,
                "TrainEnd": holdout.index[0],
                "Date": holdout.index[0],
                "Probability": 0.6,
                "Target": 1,
                "IntradayReturn": 0.01,
                "MFE": 0.02,
                "MAE": -0.01,
            }
            for direction in ("Up", "Down")
        ])

    def fake_apply(frame, _selected):
        applied_directions.extend(frame["Direction"].tolist())
        return frame.assign(Threshold=0.5, Prediction=1)

    monkeypatch.setattr(workflows, "generate_holdout_probabilities", fake_probabilities)
    monkeypatch.setattr(
        workflows,
        "apply_frozen_thresholds_by_set",
        fake_apply,
    )
    monkeypatch.setattr(
        workflows,
        "evaluate_applied_thresholds",
        lambda frame, _config: frame[["Set", "Observation", "Direction"]].assign(
            SignalCount=1, Precision=1.0, DirectionalReturnMean=0.01
        ),
    )
    monkeypatch.setattr(workflows, "_persist_walk_forward_period", lambda *_args: {})
    monkeypatch.setattr(workflows, "_persist_prepared_traceability", lambda *_args: {})

    workflows._fixed_candidate_evaluation(
        spec,
        tmp_path / "results",
        lambda event: progress.append(event.stage),
        None,
    )

    assert qualified_calls == []
    assert evaluated_sets == list(set_ids)
    assert len(evaluated_sets) == 6
    assert applied_directions == ["Up"] * 6
    configured = {
        phase for phase, _ in WORKFLOW_PHASES[JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC]
    }
    assert set(progress) <= configured
    assert {
        "combination_generation",
        "final_holdout",
        "result_writing",
    } <= set(progress)


def test_workflow_persists_diagnostics_and_keeps_invalid_candidate_local(
    monkeypatch, tmp_path
):
    repository, forced_id, _, set_a, set_b = _source(tmp_path, second_rejected=True)
    _, spec, _ = materialize_diagnostic(repository, forced_id)
    broken = json.loads(json.dumps(spec.frozen_selected_thresholds_by_set))
    broken[set_b]["Up"] = {"status": "missing", "threshold": None}
    spec = replace(spec, frozen_selected_thresholds_by_set=broken)
    captured = {}

    def fake_fixed(evaluation_spec, output, *_args):
        captured["identities"] = evaluation_spec.forced_candidate_identities
        output.mkdir(parents=True, exist_ok=True)
        pd.DataFrame([{
            "Set": set_a, "Observation": "AAA", "Direction": "Up", "Threshold": .4,
            "SignalCount": 30, "DirectionalReturnMean": .01,
            "OppositeMoveFrequency": .10, "Precision": .60, "ROCAUC": .65,
        }]).to_csv(output / "holdout_metrics.csv", index=False)
        pd.DataFrame().to_csv(output / "holdout_predictions.csv", index=False)
        pd.DataFrame().to_csv(output / "sampled_combinations.csv", index=False)
        return {"result_files": []}

    monkeypatch.setattr(workflows, "_fixed_candidate_evaluation", fake_fixed)
    output = tmp_path / "diagnostic"
    summary = workflows._qualification_holdout_diagnostic(spec, output, None, None)
    results = pd.read_csv(output / "diagnostic_results.csv")
    config = json.loads((output / "run_configuration.json").read_text(encoding="utf-8"))

    assert captured["identities"] == ((set_a, "Up"),)
    assert summary["candidate_count"] == 2
    assert summary["non_evaluable_count"] == 1
    assert set(results["Statut diagnostique"]) == {"Holdout favorable", "Non évaluable"}
    assert (output / "diagnostic_manifest.json").is_file()
    assert config["xgb_recalibration"] is False
    assert config["threshold_recalibration"] is False
    assert config["walk_forward_rerun"] is False
    assert config["prefilter_rerun"] is False
    assert config["promotion_enabled"] is False


def test_master_page_is_lightweight_and_detail_page_is_autonomous():
    source = APP.read_text(encoding="utf-8")
    temporal = source.split("def _render_temporal_validation", 1)[1].split(
        "def _read_light_json", 1
    )[0]
    detail = source.split(
        "def _render_qualification_holdout_diagnostic", 1
    )[1].split("def _render_job_detail_tabs", 1)[0]
    assert "diagnostic_state(" in temporal
    assert "diagnostic_results.csv" not in temporal
    assert "Lancer le diagnostic holdout des rejets WF" in temporal
    assert detail.count('root / "diagnostic_results.csv"') == 1
    assert '"worst_auc_sensitivity"' in detail
    assert '"diagnostic_candidate"' in detail
