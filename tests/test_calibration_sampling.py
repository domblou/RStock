import hashlib

import pandas as pd

from dataclasses import replace

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.repository import RunRepository
from rstock.application import workflows
from rstock.calibration_sampling import (
    GLOBAL_STRATIFIED_V2,
    PER_TARGET_V1,
    exhaustive_v2_sample,
    global_stratified_v2_sample,
    policy_name,
)
from rstock.config import DEFAULT_CONFIG


def _population(counts):
    rows = [
        {"V0": target, "V1": f"P{target}{index}"}
        for target, count in counts.items()
        for index in range(count)
    ]
    return pd.DataFrame(rows)


def test_policy_is_selected_from_provenance_and_persisted_version():
    assert policy_name(2, qualified_walk_forward_source=True) == GLOBAL_STRATIFIED_V2
    assert policy_name(2, qualified_walk_forward_source=False) == PER_TARGET_V1
    assert policy_name(1, qualified_walk_forward_source=True) == PER_TARGET_V1


def test_global_stratified_v2_uses_largest_remainders_and_is_order_independent():
    population = _population({"AAA": 5, "BBB": 3, "CCC": 2})
    first = global_stratified_v2_sample(population, cap=5, seed=17)
    shuffled = population.sample(frac=1, random_state=9).reset_index(drop=True)
    second = global_stratified_v2_sample(shuffled, cap=5, seed=17)

    pd.testing.assert_frame_equal(first.combinations, second.combinations)
    assert first.manifest == second.manifest
    distribution = first.manifest["target_distribution"]
    assert sum(item["allocated"] for item in distribution.values()) == 5
    assert distribution["CCC"]["allocated"] == 1
    tie_winner = min(
        ("AAA", "BBB"),
        key=lambda target: hashlib.sha256(
            f"17{GLOBAL_STRATIFIED_V2}{target}".encode()
        ).hexdigest(),
    )
    assert distribution[tie_winner]["allocated"] == (
        3 if tie_winner == "AAA" else 2
    )


def test_cap_below_target_count_allows_zero_allocations_without_minimum_one():
    sample = global_stratified_v2_sample(
        _population({"AAA": 2, "BBB": 2, "CCC": 2, "DDD": 2}),
        cap=2,
        seed=1234,
    )

    allocations = [
        values["allocated"]
        for values in sample.manifest["target_distribution"].values()
    ]
    assert sorted(allocations) == [0, 0, 1, 1]
    assert len(sample.combinations) == 2


def test_threshold_directional_cap_keeps_complete_up_down_pairs():
    population = _population({"AAA": 4, "BBB": 4})
    sample = global_stratified_v2_sample(
        population,
        cap=2,
        seed=1,
        directional_model_cap=4,
    )

    assert len(sample.combinations) == 2
    assert sample.manifest["sampled_directional_models"] == 4
    assert sample.manifest["complete_direction_pairs"] is True


def test_final_threshold_v2_is_exhaustive_and_deduplicated():
    population = pd.concat(
        [_population({"AAA": 3, "BBB": 2}), _population({"AAA": 3})],
        ignore_index=True,
    )

    sample = exhaustive_v2_sample(population, seed=7)

    assert len(sample.combinations) == 5
    assert sample.manifest["population_size"] == 5
    assert sample.manifest["sample_size"] == 5
    assert sample.manifest["cap"] is None


def test_new_calibration_with_wf_source_uses_global_qualified_population(
    monkeypatch, tmp_path
):
    repository = RunRepository(tmp_path / "runs")
    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    source_spec = ExperimentSpec(
        JobType.WALK_FORWARD,
        config,
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
    )
    source_id = repository.create(source_spec)
    repository.transition(source_id, JobStatus.RUNNING, pid=1)
    repository.transition(source_id, JobStatus.COMPLETED)
    results = repository.run_directory(source_id) / "results"
    results.mkdir()
    pd.DataFrame(
        {
            "Set": ['["AAA","BBB"]', '["AAA","BBB"]', '["BBB","AAA"]'],
            "Eligible": [True, True, False],
        }
    ).to_csv(results / "qualification.csv", index=False)
    spec = ExperimentSpec(
        JobType.XGBOOST_CALIBRATION,
        config,
        symbols=("AAA", "BBB"),
        target_symbols=("AAA", "BBB"),
        context_symbols=(),
        source_walk_forward_run=source_id,
    )
    prepared = pd.DataFrame(index=pd.bdate_range("2025-01-01", periods=5))
    monkeypatch.setattr(
        workflows,
        "_prepared_inputs",
        lambda *args, **kwargs: (prepared, ["AAA", "BBB"], ["AAA", "BBB"], {}),
    )

    loaded, population, qualified_source = workflows._prepared_calibration_population(
        spec, None, None
    )

    assert loaded is prepared
    assert qualified_source is True
    assert population.to_dict("records") == [{"V0": "AAA", "V1": "BBB"}]
    assert spec.calibration_sampling_policy_version == 2
    assert policy_name(
        spec.calibration_sampling_policy_version,
        qualified_walk_forward_source=qualified_source,
    ) == GLOBAL_STRATIFIED_V2

    historical_snapshot = spec.to_dict()
    historical_snapshot.pop("calibration_sampling_policy_version")
    historical = ExperimentSpec.from_dict(historical_snapshot)
    assert historical.calibration_sampling_policy_version == 1
    assert policy_name(
        historical.calibration_sampling_policy_version,
        qualified_walk_forward_source=True,
    ) == PER_TARGET_V1
