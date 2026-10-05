"""Walk-forward derivation keeps its frozen upstream population."""

from dataclasses import replace
from contextlib import nullcontext
from types import SimpleNamespace
import hashlib

import pandas as pd
import pytest

from rstock.application import workflows
from rstock.application import streamlit_app
from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService
from rstock.application.walk_forward_experiments import (
    WF_DERIVATION_CONFIG_FIELDS, build_derived_walk_forward_spec,
    load_frozen_walk_forward_candidates,
)
from rstock.checkpoints import CheckpointManager
from rstock.combination_planning import CombinationPlan, build_combination_plan
from rstock.config import DEFAULT_CONFIG
from rstock.modeling import historical_xgboost_parameters, prefilter_xgboost_snapshot
from rstock.application.prefilter_experiments import PREFILTER_XGBOOST_FIELDS
from rstock.traceability import prepared_dataset_hash


class _Backend:
    def launch(self, *_args):
        return 4321


def _parent(tmp_path, *, planned=True):
    repository = RunRepository(tmp_path / "runs")
    config = replace(
        DEFAULT_CONFIG, project_root=tmp_path, permutation_depth=1,
        predictor_prefilter_enabled=True, xgb_eta=0.5, xgb_reg_lambda=1.0,
        walk_forward_max_combinations_per_batch=100 if planned else None,
    )
    source = ExperimentSpec(
        job_type=JobType.WALK_FORWARD, config=config,
        symbols=("AAA", "BBB", "CCC"), target_symbols=("AAA", "BBB"),
        context_symbols=("CCC",),
        predictor_symbols=("AAA", "BBB", "CCC"),
        historical_data_cutoff="2026-07-07",
        requested_historical_cutoff="2026-07-07",
        resolved_market_session_cutoff="2026-07-07",
    )
    run_id = repository.create(source)
    prepared = pd.DataFrame(
        {"AAA.Close": [1.0, 2.0, 3.0], "BBB.Close": [2.0, 3.0, 4.0]},
        index=pd.to_datetime(["2026-07-02", "2026-07-06", "2026-07-07"]),
    )
    prepared.attrs["effective_end_date"] = "2026-07-07T00:00:00"
    prepared.attrs["symbols_used"] = 3
    digest = prepared_dataset_hash(prepared)
    checkpoint = CheckpointManager(
        repository.run_directory(run_id), run_id=run_id,
        job_type=JobType.WALK_FORWARD.value,
        configuration_fingerprint=repository.configuration_fingerprint(run_id),
        batch_sizes={
            "predictor_prefilter_walk_forward": config.predictor_prefilter_batch_size,
            "walk_forward": config.walk_forward_batch_size,
            "final_holdout": config.final_holdout_batch_size,
        },
    )
    checkpoint.commit_snapshot(prepared, {
        "predictor_symbols": ["AAA", "BBB", "CCC"],
        "target_symbols": ["AAA"],
        "calendars": {symbol: "XNYS" for symbol in ("AAA", "BBB", "CCC")},
        "effective_end_date": prepared.attrs["effective_end_date"],
    })
    if planned:
        raw = build_combination_plan(
            target_symbols=["AAA"], predictor_symbols=["AAA", "BBB", "CCC"],
            permutation_depth=1,
        )
        effective = CombinationPlan.from_target_predictors({"AAA": ["BBB"]}, 1)
        checkpoint.commit_artifact("raw_combination_plan", raw.to_dict())
        checkpoint.commit_artifact("effective_combination_plan", effective.to_dict())
        expected = effective.slice(0, effective.count())
    else:
        expected = pd.DataFrame({"V0": ["AAA"], "V1": ["BBB"]})
        checkpoint.commit_artifact("generated_sets", expected)
    repository.write_json(run_id, "summary.json", {
        "traceability": {
            "prepared_dataset_sha256": digest,
            "prepared_market_last_date": prepared.index.max().isoformat(),
        },
    })
    repository.transition(run_id, JobStatus.RUNNING)
    repository.transition(run_id, JobStatus.COMPLETED)
    return repository, run_id, expected, digest


@pytest.mark.parametrize("planned", [True, False])
def test_derived_walk_forward_reuses_snapshot_candidates_and_recalculates(tmp_path, monkeypatch, planned):
    repository, parent, expected, digest = _parent(tmp_path, planned=planned)
    source = repository.load_spec(parent)
    source_results = repository.run_directory(parent) / "results"
    source_results.mkdir(exist_ok=True)
    prefilter_csv = b"Observation,Predictor,PrefilterStatus\nAAA,BBB,retained\n"
    repository.write_json(parent, "results/predictor_prefilter.json", {
        "xgboost_parameters": prefilter_xgboost_snapshot(source.config),
        "predictors_by_target": {"AAA": ["BBB"]},
    })
    (source_results / "predictor_prefilter.csv").write_bytes(prefilter_csv)
    repository.write_json(parent, "results/run_configuration.json", {
        "predictor_prefilter": {
            "enabled": True, "top_n": source.config.predictor_prefilter_top_n,
            "xgboost_parameters": prefilter_xgboost_snapshot(source.config),
        },
    })
    inherited = build_derived_walk_forward_spec(
        repository, parent, {"xgb_eta": 0.2},
    )
    assert all(getattr(inherited.config, field) == getattr(source.config, field)
               for field in WF_DERIVATION_CONFIG_FIELDS - {"xgb_eta"})
    child = RunService(repository, backend=_Backend()).create_derived(
        parent, "walk_forward", {"xgb_eta": 0.2, "xgb_reg_lambda": 2.0},
    ).run_id
    spec = repository.load_spec(child)
    assert spec.source_walk_forward_run == spec.source_experiment_run == parent
    assert spec.requested_historical_cutoff == source.requested_historical_cutoff
    assert spec.resolved_market_session_cutoff == source.resolved_market_session_cutoff
    assert spec.historical_data_cutoff == "2026-07-07"
    assert spec.source_prepared_dataset_sha256 == digest
    assert spec.prepared_snapshot_required and spec.prepared_dataset_digest_required
    assert spec.walk_forward_derivation["source_run_id"] == parent
    snapshot = (repository.run_directory(parent) / "checkpoints" / "artifacts"
                / "prepared_snapshot.pkl")
    assert spec.walk_forward_derivation["prepared_snapshot_sha256"] == hashlib.sha256(
        snapshot.read_bytes()
    ).hexdigest()
    assert spec.walk_forward_derivation["prepared_dataset_sha256"] == digest
    assert set(spec.walk_forward_derivation["overrides"]) == {"xgb_eta", "xgb_reg_lambda"}
    assert spec.config.predictor_prefilter_enabled
    assert all(getattr(spec.config, field) == getattr(source.config, field)
               for field in PREFILTER_XGBOOST_FIELDS)
    assert historical_xgboost_parameters(spec.config).reg_lambda == 2.0
    frozen = load_frozen_walk_forward_candidates(repository, spec)
    actual = frozen.slice(0, frozen.count()) if isinstance(frozen, CombinationPlan) else frozen
    pd.testing.assert_frame_equal(actual.reset_index(drop=True), expected.reset_index(drop=True))

    for name in ("MarketDataService", "prepare_dataset", "evaluate_prefilter_walk_forward",
                 "select_predictors", "generate_symbol_sets", "generate_target_symbol_sets"):
        if name == "MarketDataService":
            monkeypatch.setattr(workflows.MarketDataService, "load", lambda *_a, **_k: (_ for _ in ()).throw(AssertionError("market reloaded")))
        else:
            monkeypatch.setattr(workflows, name, lambda *_a, **_k: (_ for _ in ()).throw(AssertionError(f"{name} recalculated")))
    captured = []

    def evaluate(prepared, generated, config, _checkpoint, output, **kwargs):
        output.mkdir(parents=True, exist_ok=True)
        captured.append((prepared, generated, config, kwargs))
        return SimpleNamespace(
            aggregate_global=pd.DataFrame([{"AUC": 0.6}]),
            qualification=pd.DataFrame({"Eligible": [True]}), telemetry={},
        )

    monkeypatch.setattr(workflows, "run_streamed_walk_forward", evaluate)
    output = repository.run_directory(child) / "_working"
    result = workflows._walk_forward(spec, output, None, None)
    assert len(captured) == 1
    prepared, generated, effective, kwargs = captured[0]
    assert prepared_dataset_hash(prepared) == digest
    pd.testing.assert_frame_equal(generated.reset_index(drop=True), expected.reset_index(drop=True))
    assert (effective.xgb_eta, effective.xgb_reg_lambda) == (0.2, 2.0)
    assert kwargs["evaluate_holdout"] == source.evaluate_final_holdout
    inherited_prefilter = kwargs["run_configuration_extras"]["predictor_prefilter"]
    assert inherited_prefilter["source_walk_forward_run"] == parent
    assert inherited_prefilter["xgboost_parameters"] == prefilter_xgboost_snapshot(source.config)
    assert (output / "predictor_prefilter.csv").read_bytes() == prefilter_csv
    assert (output / "predictor_prefilter.json").read_bytes() == (
        source_results / "predictor_prefilter.json"
    ).read_bytes()
    assert result["walk_forward_derivation"]["source_run_id"] == parent
    assert result["eligible_combinations"] == 1


def test_derived_walk_forward_rejects_changed_source_candidates(tmp_path):
    repository, parent, _, _ = _parent(tmp_path)
    spec = build_derived_walk_forward_spec(repository, parent, {"xgb_eta": 0.2})
    path = repository.run_directory(parent) / "checkpoints/artifacts/effective_combination_plan.pkl"
    path.write_bytes(path.read_bytes() + b"changed")
    with pytest.raises(ValueError, match="candidates changed"):
        load_frozen_walk_forward_candidates(repository, spec)


def test_walk_forward_derivation_ui_inherits_and_submits_scientific_fields(tmp_path, monkeypatch):
    repository, parent, _, _ = _parent(tmp_path)
    source = repository.load_spec(parent)
    shown = {}
    submitted = []

    class FakeStreamlit:
        session_state = {}

        def button(self, _label, *, key, **_kwargs):
            return key in {f"wf-derive-button-{parent}", f"wf-derive-submit-{parent}"}

        def number_input(self, _label, *, key, value, **_kwargs):
            field = key.removeprefix(f"wf-derive-{parent}-")
            shown[field] = value
            return 0.2 if field == "xgb_eta" else value

        def selectbox(self, _label, values, *, index, **_kwargs):
            return values[index]

        def checkbox(self, _label, *, value, **_kwargs):
            return value

        def container(self, **_kwargs):
            return nullcontext()

        def columns(self, count):
            return [self] * count

        def caption(self, *_args):
            pass

        def subheader(self, *_args):
            pass

        def success(self, *_args):
            pass

    service = SimpleNamespace(
        run_service=SimpleNamespace(repository=repository),
        create_derived=lambda run_id, kind, changes: (
            submitted.append((run_id, kind, changes.copy()))
            or SimpleNamespace(run_id="child")
        ),
    )
    monkeypatch.setattr(streamlit_app, "st", FakeStreamlit())
    streamlit_app._render_walk_forward_derived_creation(
        parent, {"status": {"status": "completed"}, "summary": {}}, service,
    )
    assert set(shown) == WF_DERIVATION_CONFIG_FIELDS - {"walk_forward_window_mode"}
    assert all(shown[field] == getattr(source.config, field) for field in shown)
    assert submitted == [(parent, "walk_forward", {"xgb_eta": 0.2})]


def test_walk_forward_derivation_rejects_missing_source_snapshot(tmp_path):
    repository, parent, _, _ = _parent(tmp_path)
    snapshot = (repository.run_directory(parent) / "checkpoints" / "artifacts"
                / "prepared_snapshot.pkl")
    snapshot.unlink()
    with pytest.raises(ValueError, match="snapshot is missing"):
        build_derived_walk_forward_spec(repository, parent, {"xgb_eta": 0.2})


def test_walk_forward_derivation_rejects_changed_source_snapshot(tmp_path):
    repository, parent, _, _ = _parent(tmp_path)
    snapshot = (repository.run_directory(parent) / "checkpoints" / "artifacts"
                / "prepared_snapshot.pkl")
    snapshot.write_bytes(snapshot.read_bytes() + b"changed")
    with pytest.raises(ValueError, match="snapshot is corrupt or changed"):
        build_derived_walk_forward_spec(repository, parent, {"xgb_eta": 0.2})
