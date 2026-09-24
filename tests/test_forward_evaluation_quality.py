import hashlib
import json
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

import rstock.application.forward_simulation as forward
from rstock.application.domain import ExperimentSpec, JobStatus, JobType, RunMetadata, RunRole
from rstock.application.forward_simulation import (
    EXCLUSIONS_FILENAME, SNAPSHOT_DIRECTORY, SNAPSHOT_FILENAME,
    _target_evaluation_validity, run_forward_simulation,
)
from rstock.application.repository import RunRepository
from rstock.config import DEFAULT_CONFIG


def _prices(*, target_open=100.0, target_high=102.0, target_low=99.0, target_close=101.0):
    index = pd.bdate_range("2026-09-08", "2026-09-15")
    values = {}
    for symbol in ("ADM", "P"):
        values[f"{symbol}.Open"] = [100.0] * len(index)
        values[f"{symbol}.High"] = [102.0] * len(index)
        values[f"{symbol}.Low"] = [99.0] * len(index)
        values[f"{symbol}.Close"] = [101.0] * len(index)
    values["ADM.Open"][-1] = target_open
    values["ADM.High"][-1] = target_high
    values["ADM.Low"][-1] = target_low
    values["ADM.Close"][-1] = target_close
    return pd.DataFrame(values, index=index)


def _forward_fixture(tmp_path, monkeypatch, prices, *, model_count=1):
    repository = RunRepository(tmp_path / "runs")
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, lag_depth=1)
    parent = ExperimentSpec(
        JobType.END_TO_END, config, symbols=("ADM", "P"),
        historical_data_cutoff="2026-09-14", requested_historical_cutoff="2026-09-14",
        resolved_market_session_cutoff="2026-09-14",
    )
    parent_id = repository.create(parent)
    repository.transition(parent_id, JobStatus.RUNNING)
    repository.transition(parent_id, JobStatus.COMPLETED)
    result_dir = repository.run_directory(parent_id) / "results"
    result_dir.mkdir()
    models = []
    for number in range(model_count):
        model_id = f"model-{number}"
        directory = result_dir / SNAPSHOT_DIRECTORY / model_id
        directory.mkdir(parents=True)
        for name in ("up.ubj", "down.ubj", "metadata.json"):
            (directory / name).write_text("fixture", encoding="utf-8")
        models.append({
            "source_model_id": model_id, "set": '["ADM","P"]', "target": "ADM",
            "predictors": ["P"], "direction": "Up", "feature_names": ["P_intraday_J-1"],
            "lag_depth": 1, "up_threshold": 0.5, "down_threshold": 0.5,
        })
    snapshot = {
        "source_end_to_end_run_id": parent_id,
        "resolved_market_session_cutoff": "2026-09-14",
        "requested_historical_cutoff": "2026-09-14", "models": models,
    }
    snapshot_path = result_dir / SNAPSHOT_FILENAME
    snapshot_path.write_text(json.dumps(snapshot), encoding="utf-8")
    forward_spec = replace(
        parent, job_type=JobType.FORWARD_SIMULATION, source_end_to_end_run=parent_id,
        source_forward_model_snapshot_sha256=hashlib.sha256(snapshot_path.read_bytes()).hexdigest(),
        forward_simulation_start_date="2026-09-15", forward_simulation_end_date="2026-09-15",
        forward_simulation_enabled=False,
    )
    run_id = repository.create(forward_spec, metadata=RunMetadata(
        run_role=RunRole.PIPELINE_STAGE, parent_run_id=parent_id,
        relation_key="forward_simulation", relation_type="forward_simulation",
        stage_key="forward_simulation",
    ))

    class FakeMarketDataService:
        def load(self, *_args, **_kwargs):
            return SimpleNamespace(prices=prices, symbols=["ADM", "P"]), {}

    monkeypatch.setattr(forward, "MarketDataService", FakeMarketDataService)
    monkeypatch.setattr(forward, "load_booster", lambda _path: object())
    monkeypatch.setattr(forward, "predict_probabilities", lambda *_args: np.array([0.9]))
    output = repository.run_directory(run_id) / "_working"
    output.mkdir()
    return repository, run_id, forward_spec, output


@pytest.mark.parametrize("field", ["Open", "High", "Low", "Close"])
def test_incomplete_target_ohlc_is_an_exclusion_not_a_binary_outcome(tmp_path, monkeypatch, field):
    values = _prices()
    values.loc["2026-09-15", f"ADM.{field}"] = np.nan
    _repository, run_id, spec, output = _forward_fixture(tmp_path, monkeypatch, values)

    summary = run_forward_simulation(spec, output)
    exclusions = pd.read_csv(output / EXCLUSIONS_FILENAME)

    assert summary["evaluated_observations"] == 0
    assert summary["skipped_observations"] == 1
    assert summary["unique_data_quality_issues"] == 1
    assert summary["skipped_missing_target_ohlc"] == 1
    assert summary["evaluability_rate"] == 0.0
    assert exclusions.loc[0, "exclusion_reason"] == "missing_target_ohlc"
    assert exclusions.loc[0, "invalid_fields"] == field
    assert exclusions.loc[0, "forward_simulation_run_id"] == run_id


@pytest.mark.parametrize("value", [np.inf, -np.inf])
def test_infinite_target_ohlc_is_excluded(tmp_path, monkeypatch, value):
    values = _prices(target_close=value)
    _repository, _run_id, spec, output = _forward_fixture(tmp_path, monkeypatch, values)

    summary = run_forward_simulation(spec, output)

    assert summary["skipped_missing_target_ohlc"] == 1


@pytest.mark.parametrize("close, expected", [(102.0, 1), (100.5, 0)])
def test_complete_target_observation_remains_binary_and_evaluated(tmp_path, monkeypatch, close, expected):
    _repository, _run_id, spec, output = _forward_fixture(
        tmp_path, monkeypatch, _prices(target_close=close)
    )

    summary = run_forward_simulation(spec, output)
    observations = pd.read_csv(output / "forward_observations.csv")

    assert summary["evaluated_observations"] == 1
    assert summary["skipped_observations"] == 0
    assert summary["evaluability_rate"] == 1.0
    assert observations.loc[0, "outcome"] == expected
    assert bool(observations.loc[0, "correct_direction"]) is (expected == 1)


def test_non_finite_calculated_metric_has_a_distinct_reason():
    returns = pd.Series({
        "ADM.intraday_return": np.nan, "ADM.intraday_target": np.nan,
        "ADM.mfe": 0.01, "ADM.mae": -0.01,
    })
    prices = pd.Series({
        "ADM.Open": 100.0, "ADM.High": 101.0, "ADM.Low": 99.0, "ADM.Close": 100.5,
    })

    reason, invalid = _target_evaluation_validity(prices, returns, "ADM")

    assert reason == "non_finite_target_metric"
    assert invalid == ["ADM.intraday_return", "ADM.intraday_target"]


def test_same_target_session_is_one_data_issue_but_multiple_model_exclusions(tmp_path, monkeypatch):
    _repository, _run_id, spec, output = _forward_fixture(
        tmp_path, monkeypatch, _prices(target_close=np.nan), model_count=2
    )

    summary = run_forward_simulation(spec, output)
    exclusions = pd.read_csv(output / EXCLUSIONS_FILENAME)

    assert len(exclusions) == 2
    assert summary["skipped_observations"] == 2
    assert summary["unique_data_quality_issues"] == 1


def test_checkpoint_reuses_exclusions_without_duplication(tmp_path, monkeypatch):
    _repository, _run_id, spec, output = _forward_fixture(
        tmp_path, monkeypatch, _prices(target_close=np.nan)
    )

    first = run_forward_simulation(spec, output)
    second = run_forward_simulation(spec, output)

    assert first == second
    assert len(pd.read_csv(output / EXCLUSIONS_FILENAME)) == 1
