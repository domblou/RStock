from dataclasses import replace
import json

import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_duplication import (
    JOB_TYPE_LABELS,
    config_from_historical_snapshot,
    duplication_combination_count,
    duplication_job_label,
    duplication_submission_values,
    experiment_spec_from_duplication,
    normalize_duplication_job_type,
    validate_duplication_job,
    walk_forward_duplication_draft,
)
from rstock.application.repository import RunRepository
from rstock.application.universes import (
    CONTEXT_UNIVERSE_TYPE,
    SAMPLE_SOURCE,
    TOP_N,
    UniverseSelection,
    UniverseService,
)
from rstock.combinations import generate_symbol_sets
from rstock.config import DEFAULT_CONFIG


def _detail(*, job_type="walk_forward", snapshot=None):
    return {
        "configuration": {
            "job_type": job_type,
            "symbols": ["DIS", "AMZN", "NVDA"],
            "primary_universe_id": "primary",
            "market_benchmark_symbol": "SPY",
            "context_universe_ids": ["market"],
            "context_sample_size": 1,
            "context_selection_method": "top_n",
            "context_seed": None,
            "universe_selection": {
                "source": "universe_sample",
                "universe": "primary",
                "sample_size": 2,
                "selection_method": "top_n",
                "seed": None,
            },
            "target_symbols": ["DIS", "AMZN"],
            "context_symbols": ["NVDA"],
            "predictor_symbols": ["DIS", "AMZN", "NVDA"],
            "calendar": "XNYS",
            "combinations_per_target": 7,
            "evaluate_final_holdout": False,
            "rstock_config": snapshot or {"xgb_seed": 99, "lag_depth": 4},
        }
    }


def test_walk_forward_duplication_draft_copies_all_experiment_inputs():
    detail = _detail()

    draft = walk_forward_duplication_draft("run_original", detail)

    assert draft["source_run_id"] == "run_original"
    assert draft["job_type"] == "walk_forward"
    assert draft["primary_universe_id"] == "primary"
    assert draft["market_benchmark_symbol"] == "SPY"
    assert draft["context_universe_ids"] == ["market"]
    assert draft["context_sample_size"] == 1
    assert draft["context_selection_method"] == "top_n"
    assert draft["target_symbols"] == ["DIS", "AMZN"]
    assert draft["context_symbols"] == ["NVDA"]
    assert draft["predictor_symbols"] == ["DIS", "AMZN", "NVDA"]
    assert draft["calendar"] == "XNYS"
    assert draft["combinations_per_target"] == 7
    assert draft["evaluate_final_holdout"] is False
    assert draft["rstock_config"] == {"xgb_seed": 99, "lag_depth": 4}

    draft["rstock_config"]["xgb_seed"] = 12
    assert detail["configuration"]["rstock_config"]["xgb_seed"] == 99


def test_duplication_carries_the_source_traceability_cutoff(tmp_path):
    detail = _detail()
    detail["summary"] = {
        "traceability": {
            "prepared_market_last_date": "2025-01-31T00:00:00",
            "prepared_dataset_sha256": "source-hash",
        }
    }

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", detail),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.historical_data_cutoff == "2025-01-31T00:00:00"
    assert duplicated.source_prepared_dataset_sha256 == "source-hash"
    assert duplicated.market_benchmark_symbol == "SPY"


def test_legacy_duplication_without_traceability_keeps_no_data_cutoff(tmp_path):
    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", _detail()),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.historical_data_cutoff is None
    assert duplicated.source_prepared_dataset_sha256 is None


def test_new_and_legacy_specs_have_no_xgboost_calibration_provenance(tmp_path):
    spec = ExperimentSpec(
        JobType.WALK_FORWARD,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("DIS", "AMZN"),
    )
    historical = spec.to_dict()
    historical.pop("source_xgboost_calibration_run")
    historical.pop("frozen_xgboost_parameters")
    historical.pop("frozen_xgboost_parameters_sha256")
    historical.pop("xgboost_resolution_version")

    assert spec.source_xgboost_calibration_run is None
    assert spec.frozen_xgboost_parameters is None
    assert ExperimentSpec.from_dict(historical).source_xgboost_calibration_run is None
    assert ExperimentSpec.from_dict(historical).xgboost_resolution_version == 0


def test_duplication_from_xgboost_calibration_freezes_directional_parameters(tmp_path):
    run_id = "xgb-parent"
    results = tmp_path / "runs" / run_id / "results"
    results.mkdir(parents=True)
    selected = {
        "Up": {"parameters": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120}},
        "Down": {"parameters": {"max_depth": 3, "eta": 0.1, "num_boost_round": 80}},
    }
    (results / "selected_configurations.json").write_text(
        json.dumps(selected), encoding="utf-8"
    )

    draft = walk_forward_duplication_draft(
        run_id, _detail(job_type="xgboost_calibration"), project_root=tmp_path
    )
    derived = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
        job_type=JobType.WALK_FORWARD,
    )
    repository = RunRepository(tmp_path / "persisted-runs")
    reloaded = repository.load_spec(repository.create(derived))

    assert reloaded.source_xgboost_calibration_run == run_id
    assert reloaded.frozen_xgboost_parameters == {
        "Up": selected["Up"]["parameters"],
        "Down": selected["Down"]["parameters"],
    }
    assert len(reloaded.frozen_xgboost_parameters_sha256) == 64


def test_duplication_config_choice_preserves_or_detaches_xgboost_provenance(tmp_path):
    from rstock.application.workflows import _resolve_threshold_xgboost_parameters

    detail = _detail()
    detail["configuration"].update({
        "source_xgboost_calibration_run": "xgb-parent",
        "frozen_xgboost_parameters": {
            "Up": {"max_depth": 2, "eta": 0.05, "num_boost_round": 60},
            "Down": {"max_depth": 3, "eta": 0.1, "num_boost_round": 90},
        },
    })
    draft = walk_forward_duplication_draft("wf-parent", detail)
    current = replace(DEFAULT_CONFIG, project_root=tmp_path)

    preserved = experiment_spec_from_duplication(
        draft, current_config=current, use_run_config=True
    )
    detached = experiment_spec_from_duplication(
        draft, current_config=current, use_run_config=False
    )

    assert preserved.source_xgboost_calibration_run == "xgb-parent"
    assert preserved.frozen_xgboost_parameters["Down"]["max_depth"] == 3
    assert detached.source_xgboost_calibration_run is None
    assert detached.frozen_xgboost_parameters is None
    assert detached.frozen_xgboost_parameters_sha256 is None
    assert _resolve_threshold_xgboost_parameters(preserved).source == "frozen_snapshot"
    assert _resolve_threshold_xgboost_parameters(detached).source == "rstock_config"


@pytest.mark.parametrize(
    ("job_type", "expected_label"),
    [
        (JobType.WALK_FORWARD, "Walk-forward — profondeur 2"),
        (JobType.XGBOOST_CALIBRATION, "Calibration XGBoost — profondeur 2"),
        (
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            "Calibration des paramètres de seuils — profondeur 2",
        ),
        (JobType.THRESHOLD_CALIBRATION, "Calibration des seuils — profondeur 2"),
    ],
)
def test_duplication_persists_the_source_description_with_the_selected_job_label(
    job_type, expected_label, tmp_path
):
    detail = _detail()
    detail["configuration"]["run_description"] = "Walk-forward — profondeur 2"
    draft = walk_forward_duplication_draft("run_original", detail)

    duplicated = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
        job_type=job_type,
    )

    assert draft["run_description"] == "profondeur 2"
    assert duplicated.run_description == "profondeur 2"
    assert duplicated.to_dict()["run_description"] == "profondeur 2"
    assert f"{JOB_TYPE_LABELS[job_type]} — {duplicated.run_description}" == expected_label


def test_legacy_duplication_without_a_description_remains_compatible(tmp_path):
    detail = _detail(snapshot={"xgb_seed": 99})
    draft = walk_forward_duplication_draft("run_original", detail)

    duplicated = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
        job_type=JobType.THRESHOLD_CALIBRATION,
    )

    assert draft["run_description"] is None
    assert duplicated.run_description is None


def test_duplication_preserves_walk_forward_batch_sizes(tmp_path):
    detail = _detail(snapshot={
        "predictor_prefilter_batch_size": 7,
        "walk_forward_batch_size": 11,
        "final_holdout_batch_size": 3,
    })
    draft = walk_forward_duplication_draft("run_original", detail)

    duplicated = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.config.predictor_prefilter_batch_size == 7
    assert duplicated.config.walk_forward_batch_size == 11
    assert duplicated.config.final_holdout_batch_size == 3


def test_recent_duplication_preserves_explicit_modern_compatibility_fields(tmp_path):
    detail = _detail(snapshot={
        "walk_forward_end_offset_sessions": 0,
        "max_generated_sets": 100_000,
    })

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", detail),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.config.walk_forward_end_offset_sessions == 0
    assert duplicated.config.max_generated_sets == 100_000


def test_legacy_duplication_restores_historical_walk_forward_end_offset(tmp_path):
    detail = _detail(snapshot={"max_generated_sets": 100_000})

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", detail),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.config.walk_forward_end_offset_sessions == 63


def test_legacy_duplication_restores_historical_generated_sets_limit(tmp_path):
    detail = _detail(snapshot={"walk_forward_end_offset_sessions": 63})

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", detail),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.config.max_generated_sets == 1_000_000_000


def test_new_runs_keep_current_defaults(tmp_path):
    current = replace(DEFAULT_CONFIG, project_root=tmp_path)

    assert current.walk_forward_end_offset_sessions == 0
    assert current.max_generated_sets == 100_000


def test_historical_walk_forward_string_has_the_safe_ui_label():
    draft = walk_forward_duplication_draft("run_original", _detail())

    assert draft["job_type"] == "walk_forward"
    assert normalize_duplication_job_type(draft["job_type"]) is JobType.WALK_FORWARD
    assert duplication_job_label(draft["job_type"]) == "Walk-forward"


@pytest.mark.parametrize(
    ("job_type", "label"),
    [
        (JobType.WALK_FORWARD, "Walk-forward"),
        (JobType.XGBOOST_CALIBRATION, "Calibration XGBoost"),
        (
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            "Calibration des paramètres de seuils",
        ),
        (JobType.THRESHOLD_CALIBRATION, "Calibration des seuils"),
    ],
)
def test_duplication_job_labels_accept_enums_and_ui_choices(job_type, label):
    assert normalize_duplication_job_type(job_type) is job_type
    assert duplication_job_label(job_type) == label
    assert JOB_TYPE_LABELS[job_type] == label


def test_unknown_historical_job_type_is_logged_and_uses_a_safe_label(caplog):
    with caplog.at_level("WARNING"):
        label = duplication_job_label("legacy_walk_forward_v0")

    assert label == "Walk-forward"
    assert "Unknown duplication job type" in caplog.text


def test_only_walk_forward_runs_can_be_duplicated():
    with pytest.raises(ValueError, match="walk-forward"):
        walk_forward_duplication_draft("run_other", _detail(job_type="daily_prediction"))


def test_historical_partial_config_uses_defaults_and_current_project_root(tmp_path):
    config = config_from_historical_snapshot(
        {"xgb_seed": 321, "selected_symbols": ["DIS", "AMZN"]},
        current_project_root=tmp_path,
    )

    assert config.project_root == tmp_path
    assert config.xgb_seed == 321
    assert config.selected_symbols == ("DIS", "AMZN")
    assert config.lag_depth == DEFAULT_CONFIG.lag_depth
    assert config.temporal_min_candidate_yield_ratio == 0.25
    assert config.temporal_max_auc_degradation == 0.03
    assert config.temporal_min_precision_edge == 0.00
    assert config.temporal_min_mean_directional_return == 0.00
    assert config.temporal_confidence_level == 0.95
    assert config.temporal_max_ci_width == 0.20


def test_run_or_current_config_choice_preserves_current_project_root(tmp_path):
    current = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_seed=17)
    draft = walk_forward_duplication_draft(
        "run_original", _detail(snapshot={"project_root": "C:/old/RStock", "xgb_seed": 99})
    )

    from_run = duplication_submission_values(
        draft, current_config=current, use_run_config=True
    )
    from_current = duplication_submission_values(
        draft, current_config=current, use_run_config=False
    )

    assert from_run["config"].project_root == tmp_path
    assert from_run["config"].xgb_seed == 99
    assert from_current["config"] is current
    assert from_current["config"].xgb_seed == 17
    assert from_run["target_symbols"] == ["DIS", "AMZN"]


def test_duplicated_spec_preserves_all_frozen_run_inputs(tmp_path):
    source = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_seed=99),
        symbols=("DIS", "AMZN", "NVDA"),
        calendar="XNYS",
        combinations_per_target=7,
        evaluate_final_holdout=False,
        universe_selection=UniverseSelection(
            source="universe_sample",
            universe="primary",
            sample_size=2,
            selection_method="top_n",
        ),
        primary_universe_id="primary",
        context_universe_ids=("market",),
        context_sample_size=1,
        context_selection_method="top_n",
        target_symbols=("DIS", "AMZN"),
        context_symbols=("NVDA",),
        predictor_symbols=("DIS", "AMZN", "NVDA"),
    )
    draft = walk_forward_duplication_draft(
        "run_original", {"configuration": source.to_dict()}
    )

    duplicated = experiment_spec_from_duplication(
        draft, current_config=source.config, use_run_config=True
    )

    expected = source.to_dict()
    expected["source_experiment_run"] = "run_original"
    expected["source_walk_forward_run"] = "run_original"
    assert duplicated.to_dict() == expected


def test_walk_forward_duplication_persists_its_direct_source_run(tmp_path):
    repository = RunRepository(tmp_path / "runs")
    source = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("DIS", "AMZN", "NVDA"),
        target_symbols=("DIS", "AMZN"),
        context_symbols=("NVDA",),
    )
    run_a = repository.create(source)
    assert repository.load_spec(run_a).source_walk_forward_run is None

    run_b = repository.create(
        experiment_spec_from_duplication(
            walk_forward_duplication_draft(
                run_a, {"configuration": repository.load_spec(run_a).to_dict()}
            ),
            current_config=source.config,
            use_run_config=True,
        )
    )
    run_c = repository.create(
        experiment_spec_from_duplication(
            walk_forward_duplication_draft(
                run_b, {"configuration": repository.load_spec(run_b).to_dict()}
            ),
            current_config=source.config,
            use_run_config=True,
        )
    )

    assert repository.load_spec(run_b).source_walk_forward_run == run_a
    assert repository.load_spec(run_c).source_walk_forward_run == run_b


def test_historical_snapshot_without_walk_forward_provenance_remains_valid(tmp_path):
    snapshot = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=("DIS", "AMZN"),
    ).to_dict()
    snapshot.pop("source_walk_forward_run")

    assert ExperimentSpec.from_dict(snapshot).source_walk_forward_run is None


def test_current_parameters_replace_only_the_technical_configuration(tmp_path):
    draft = walk_forward_duplication_draft("run_original", _detail())
    current = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_seed=17, lag_depth=8)

    from_run = experiment_spec_from_duplication(
        draft, current_config=current, use_run_config=True
    )
    from_current = experiment_spec_from_duplication(
        draft, current_config=current, use_run_config=False
    )

    assert from_current.config is current
    assert from_run.config.xgb_seed == 99
    assert from_current.target_symbols == from_run.target_symbols
    assert from_current.context_symbols == from_run.context_symbols
    assert from_current.predictor_symbols == from_run.predictor_symbols
    assert from_current.universe_selection == from_run.universe_selection
    assert from_current.evaluate_final_holdout == from_run.evaluate_final_holdout


def test_current_walk_forward_parameters_keep_offset_of_an_inherited_cutoff(tmp_path):
    detail = _detail(snapshot={
        "walk_forward_end_offset_sessions": 63,
        "walk_forward_min_train_size": 252,
    })
    detail["summary"] = {
        "traceability": {"prepared_market_last_date": "2025-01-31T00:00:00"}
    }
    current = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        walk_forward_end_offset_sessions=0,
        walk_forward_min_train_size=111,
    )

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("run_original", detail),
        current_config=current,
        use_run_config=False,
        job_type=JobType.WALK_FORWARD,
    )

    assert duplicated.historical_data_cutoff == "2025-01-31T00:00:00"
    assert duplicated.config.walk_forward_end_offset_sessions == 63
    assert duplicated.config.walk_forward_min_train_size == 111


def test_current_parameters_are_read_at_submission_after_the_draft_was_created(
    tmp_path,
):
    draft = walk_forward_duplication_draft(
        "run_original",
        _detail(snapshot={"predictor_prefilter_enabled": False}),
    )
    updated_current_config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        predictor_prefilter_enabled=True,
    )

    current_parameters = experiment_spec_from_duplication(
        draft,
        current_config=updated_current_config,
        use_run_config=False,
    )
    run_parameters = experiment_spec_from_duplication(
        draft,
        current_config=updated_current_config,
        use_run_config=True,
    )

    assert current_parameters.config.predictor_prefilter_enabled is True
    assert run_parameters.config.predictor_prefilter_enabled is False
    assert current_parameters.target_symbols == run_parameters.target_symbols
    assert current_parameters.context_symbols == run_parameters.context_symbols
    assert current_parameters.predictor_symbols == run_parameters.predictor_symbols


@pytest.mark.parametrize(
    "job_type",
    [
        JobType.THRESHOLD_CALIBRATION,
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.XGBOOST_CALIBRATION,
    ],
)
def test_duplication_can_change_walk_forward_to_a_compatible_experimental_job(
    tmp_path, job_type
):
    detail = _detail(snapshot={"xgb_seed": 99})
    draft = walk_forward_duplication_draft("run_original", detail)
    current = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_seed=17)

    from_run = experiment_spec_from_duplication(
        draft,
        current_config=current,
        use_run_config=True,
        job_type=job_type,
    )
    from_current = experiment_spec_from_duplication(
        draft,
        current_config=current,
        use_run_config=False,
        job_type=job_type.value,
    )

    assert from_run.job_type is job_type
    assert from_current.job_type is job_type
    assert from_run.config.xgb_seed == 99
    assert from_current.config.xgb_seed == 99
    assert from_run.source_walk_forward_run == "run_original"
    assert from_current.source_walk_forward_run == "run_original"
    assert from_run.calibration_sampling_policy_version == 2
    assert from_current.calibration_sampling_policy_version == 2
    for field in ("target_symbols", "context_symbols", "predictor_symbols"):
        assert getattr(from_run, field) == tuple(draft[field])
        assert getattr(from_current, field) == tuple(draft[field])
    assert detail == _detail(snapshot={"xgb_seed": 99})


def test_same_stage_historical_calibration_replay_keeps_v1_sampling(tmp_path):
    draft = walk_forward_duplication_draft("historical-xgb", _detail())
    draft.update(
        job_type="xgboost_calibration",
        source_walk_forward_run="historical-wf",
        calibration_sampling_policy_version=1,
    )
    duplicated = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
        job_type=JobType.XGBOOST_CALIBRATION,
    )

    assert duplicated.calibration_sampling_policy_version == 1


def test_walk_forward_to_xgboost_current_parameters_keep_upstream_baseline(tmp_path):
    from rstock.calibration import default_parameter_candidates

    source_config = {
        "xgb_max_depth": 2,
        "xgb_eta": 0.05,
        "xgb_rounds": 80,
        "walk_forward_min_train_size": 300,
        "threshold_calibration_min_signals_per_window": 11,
    }
    draft = walk_forward_duplication_draft(
        "wf-parent", _detail(snapshot=source_config)
    )
    current = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_max_depth=7,
        xgb_eta=0.3,
        xgb_rounds=12,
        walk_forward_min_train_size=120,
    )

    derived = experiment_spec_from_duplication(
        draft,
        current_config=current,
        use_run_config=False,
        job_type=JobType.XGBOOST_CALIBRATION,
        current_combinations_per_target=13,
    )

    assert derived.config.xgb_max_depth == 2
    assert derived.config.xgb_eta == 0.05
    assert derived.config.xgb_rounds == 80
    assert derived.config.walk_forward_min_train_size == 300
    assert derived.combinations_per_target == 13
    assert derived.source_walk_forward_run == "wf-parent"
    assert default_parameter_candidates(derived.config)[0].max_depth == 2
    assert default_parameter_candidates(derived.config)[0].eta == 0.05
    assert default_parameter_candidates(derived.config)[0].num_boost_round == 80


@pytest.mark.parametrize(
    "target_job_type",
    [
        JobType.THRESHOLD_PARAMETER_CALIBRATION,
        JobType.THRESHOLD_CALIBRATION,
    ],
)
def test_xgboost_to_threshold_current_parameters_keep_directional_winners(
    tmp_path, target_job_type
):
    from rstock.application.workflows import _resolve_threshold_xgboost_parameters

    run_id = "xgb-parent"
    selected = {
        "Up": {"parameters": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120}},
        "Down": {"parameters": {"max_depth": 4, "eta": 0.1, "num_boost_round": 80}},
    }
    results = tmp_path / "runs" / run_id / "results"
    results.mkdir(parents=True)
    (results / "selected_configurations.json").write_text(
        json.dumps(selected), encoding="utf-8"
    )
    detail = _detail(
        job_type="xgboost_calibration",
        snapshot={
            "xgb_max_depth": 3,
            "threshold_calibration_min_signals_per_window": 40,
            "threshold_calibration_precision_tolerance": 0.02,
        },
    )
    detail["configuration"]["source_walk_forward_run"] = "wf-parent"
    current = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_max_depth=9,
        threshold_calibration_min_signals_per_window=7,
        threshold_calibration_precision_tolerance=0.005,
    )

    derived = experiment_spec_from_duplication(
        walk_forward_duplication_draft(run_id, detail, project_root=tmp_path),
        current_config=current,
        use_run_config=False,
        job_type=target_job_type,
        current_combinations_per_target=5,
    )

    assert derived.source_xgboost_calibration_run == run_id
    assert derived.frozen_xgboost_parameters == {
        direction: values["parameters"] for direction, values in selected.items()
    }
    assert derived.config.xgb_max_depth == 3
    assert derived.config.threshold_calibration_min_signals_per_window == 7
    assert derived.config.threshold_calibration_precision_tolerance == 0.005
    assert derived.combinations_per_target == 5
    resolved = _resolve_threshold_xgboost_parameters(derived)
    assert resolved.source == "frozen_snapshot"
    assert resolved.up.max_depth == 2
    assert resolved.down.max_depth == 4


def test_walk_forward_to_threshold_current_parameters_keep_walk_forward_xgboost(tmp_path):
    from rstock.application.workflows import _resolve_threshold_xgboost_parameters

    draft = walk_forward_duplication_draft(
        "wf-parent",
        _detail(snapshot={
            "xgb_max_depth": 2,
            "xgb_eta": 0.04,
            "xgb_rounds": 90,
            "walk_forward_test_size": 42,
            "threshold_calibration_min_robust_signals": 30,
        }),
    )
    current = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_max_depth=8,
        xgb_eta=0.4,
        xgb_rounds=10,
        walk_forward_test_size=10,
        threshold_calibration_min_robust_signals=6,
    )

    derived = experiment_spec_from_duplication(
        draft,
        current_config=current,
        use_run_config=False,
        job_type=JobType.THRESHOLD_CALIBRATION,
    )

    assert derived.config.xgb_max_depth == 2
    assert derived.config.xgb_eta == 0.04
    assert derived.config.xgb_rounds == 90
    assert derived.config.walk_forward_test_size == 42
    assert derived.config.threshold_calibration_min_robust_signals == 6
    assert derived.source_xgboost_calibration_run is None
    assert derived.xgboost_resolution_version == 1
    resolved = _resolve_threshold_xgboost_parameters(derived)
    assert resolved.source == "rstock_config"
    assert resolved.up.max_depth == 2
    assert resolved.down.max_depth == 2


def test_threshold_duplication_with_run_parameters_preserves_both_stages(tmp_path):
    from rstock.application.workflows import _resolve_threshold_xgboost_parameters

    detail = _detail(
        job_type="threshold_calibration",
        snapshot={
            "xgb_max_depth": 3,
            "threshold_calibration_min_signals_per_window": 25,
        },
    )
    detail["configuration"].update({
        "source_walk_forward_run": "wf-parent",
        "source_xgboost_calibration_run": "xgb-parent",
        "frozen_xgboost_parameters": {
            "Up": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120},
            "Down": {"max_depth": 4, "eta": 0.1, "num_boost_round": 80},
        },
        "xgboost_resolution_version": 1,
    })

    duplicated = experiment_spec_from_duplication(
        walk_forward_duplication_draft("threshold-parent", detail),
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.job_type is JobType.THRESHOLD_CALIBRATION
    assert duplicated.source_walk_forward_run == "wf-parent"
    assert duplicated.source_xgboost_calibration_run == "xgb-parent"
    assert duplicated.frozen_xgboost_parameters["Down"]["max_depth"] == 4
    assert duplicated.config.xgb_max_depth == 3
    assert duplicated.config.threshold_calibration_min_signals_per_window == 25
    assert _resolve_threshold_xgboost_parameters(duplicated).source == "frozen_snapshot"


def test_walk_forward_to_threshold_parameter_calibration_owns_only_threshold_fields(
    tmp_path,
):
    detail = _detail(snapshot={
            "xgb_max_depth": 2,
            "walk_forward_test_size": 42,
            "threshold_calibration_min_signals_per_window": 30,
        })
    detail["configuration"].update({
        "source_xgboost_calibration_run": "older-xgb-ancestor",
        "frozen_xgboost_parameters": {
            "Up": {"max_depth": 2, "eta": 0.05, "num_boost_round": 80},
            "Down": {"max_depth": 3, "eta": 0.1, "num_boost_round": 60},
        },
    })
    draft = walk_forward_duplication_draft("wf-parent", detail)
    current = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        xgb_max_depth=9,
        walk_forward_test_size=10,
        threshold_calibration_min_signals_per_window=6,
    )

    derived = experiment_spec_from_duplication(
        draft,
        current_config=current,
        use_run_config=False,
        job_type=JobType.THRESHOLD_PARAMETER_CALIBRATION,
        current_combinations_per_target=4,
    )

    assert derived.config.xgb_max_depth == 2
    assert derived.config.walk_forward_test_size == 42
    assert derived.config.threshold_calibration_min_signals_per_window == 6
    assert derived.combinations_per_target == 4
    assert derived.source_walk_forward_run == "wf-parent"
    assert derived.source_experiment_run == "wf-parent"
    from rstock.application.workflows import _threshold_parameter_parent
    assert _threshold_parameter_parent(derived) == "wf-parent"


def test_threshold_parameter_result_is_frozen_for_threshold_descendant(tmp_path):
    from rstock.application.workflows import _resolve_threshold_calibration_config

    run_id = "threshold-parameter-parent"
    parameters = {
        "threshold_calibration_min_signals_per_window": 7,
        "threshold_calibration_min_robust_signals": 12,
        "threshold_calibration_min_window_fraction": 0.75,
        "threshold_calibration_precision_tolerance": 0.02,
        "threshold_calibration_quantiles": [0.5, 0.75, 0.9],
        "threshold_calibration_grid_decimals": 6,
    }
    results = tmp_path / "runs" / run_id / "results"
    results.mkdir(parents=True)
    (results / "selected_threshold_calibration_configuration.json").write_text(
        json.dumps({"parameters": parameters}), encoding="utf-8"
    )
    detail = _detail(
        job_type="threshold_parameter_calibration",
        snapshot={
            "xgb_max_depth": 3,
            "threshold_calibration_min_signals_per_window": 40,
        },
    )
    detail["configuration"].update({
        "source_walk_forward_run": "wf-parent",
        "source_xgboost_calibration_run": "xgb-parent",
        "frozen_xgboost_parameters": {
            "Up": {"max_depth": 2, "eta": 0.05, "num_boost_round": 120},
            "Down": {"max_depth": 4, "eta": 0.1, "num_boost_round": 80},
        },
    })

    derived = experiment_spec_from_duplication(
        walk_forward_duplication_draft(run_id, detail, project_root=tmp_path),
        current_config=replace(
            DEFAULT_CONFIG,
            project_root=tmp_path,
            xgb_max_depth=9,
            threshold_calibration_min_signals_per_window=2,
        ),
        use_run_config=False,
        job_type=JobType.THRESHOLD_CALIBRATION,
    )

    assert derived.source_threshold_parameter_calibration_run == run_id
    assert derived.frozen_threshold_calibration_parameters == parameters
    assert derived.config.threshold_calibration_min_signals_per_window == 40
    assert derived.config.xgb_max_depth == 3
    assert derived.source_xgboost_calibration_run == "xgb-parent"
    assert derived.frozen_xgboost_parameters["Down"]["max_depth"] == 4
    effective, source = _resolve_threshold_calibration_config(derived)
    assert source == "frozen_snapshot"
    assert effective.threshold_calibration_min_signals_per_window == 7


def test_duplication_defaults_to_the_source_job_type():
    draft = walk_forward_duplication_draft("run_original", _detail())

    assert validate_duplication_job(draft) is JobType.WALK_FORWARD


def test_duplication_rejects_an_incompatible_job_type_without_mutating_draft(tmp_path):
    draft = walk_forward_duplication_draft("run_original", _detail())
    original = {key: value.copy() if isinstance(value, list) else value for key, value in draft.items()}

    with pytest.raises(ValueError, match="supports only"):
        experiment_spec_from_duplication(
            draft,
            current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
            use_run_config=True,
            job_type=JobType.DAILY_PREDICTION,
        )

    assert draft == original


def test_submission_rejects_unknown_job_type_cleanly(tmp_path):
    draft = walk_forward_duplication_draft("run_original", _detail())

    with pytest.raises(ValueError, match="Unknown duplication job type"):
        experiment_spec_from_duplication(
            draft,
            current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
            use_run_config=True,
            job_type="legacy_walk_forward_v0",
        )


def test_threshold_calibration_duplication_rejects_an_incomplete_frozen_population(
    tmp_path,
):
    draft = walk_forward_duplication_draft("run_original", _detail())
    draft["predictor_symbols"] = ["DIS", "NVDA"]

    with pytest.raises(ValueError, match="inconsistent"):
        experiment_spec_from_duplication(
            draft,
            current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
            use_run_config=True,
            job_type=JobType.THRESHOLD_CALIBRATION,
        )


def test_historical_null_context_metadata_remains_null_without_changing_symbols(tmp_path):
    detail = _detail()
    configuration = detail["configuration"]
    configuration["context_sample_size"] = None
    configuration["context_selection_method"] = None
    configuration["context_seed"] = None
    draft = walk_forward_duplication_draft("run_original", detail)
    duplicated = experiment_spec_from_duplication(
        draft,
        current_config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        use_run_config=True,
    )

    assert duplicated.context_sample_size is None
    assert duplicated.context_selection_method is None
    assert duplicated.context_seed is None
    assert duplicated.target_symbols == ("DIS", "AMZN")
    assert duplicated.context_symbols == ("NVDA",)
    assert duplicated.predictor_symbols == ("DIS", "AMZN", "NVDA")


def test_duplicate_keeps_top_ten_context_and_1900_combinations_after_universe_change(
    tmp_path,
):
    targets = tuple(f"T{i:02d}" for i in range(10))
    market_context = tuple(f"C{i:02d}" for i in range(15))
    universes = UniverseService({"PRIMARY": targets}, root=tmp_path)
    context = universes.create(
        "Market Context", market_context, universe_type=CONTEXT_UNIVERSE_TYPE
    )
    selection = UniverseSelection(
        source=SAMPLE_SOURCE,
        universe="PRIMARY",
        sample_size=10,
        selection_method=TOP_N,
    )
    resolved = universes.resolve_experiment(
        selection,
        (context.universe_id,),
        context_sample_size=10,
        context_selection_method=TOP_N,
    )
    source = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path, permutation_depth=2),
        symbols=resolved.predictor_symbols,
        universe_selection=selection,
        primary_universe_id=resolved.primary_universe_id,
        context_universe_ids=resolved.context_universe_ids,
        context_sample_size=10,
        context_selection_method=TOP_N,
        target_symbols=resolved.target_symbols,
        context_symbols=resolved.context_symbols,
        predictor_symbols=resolved.predictor_symbols,
    )
    draft = walk_forward_duplication_draft(
        "run_source", {"configuration": source.to_dict()}
    )

    universes.update(
        context.universe_id,
        name="Market Context",
        symbols=tuple(f"NEW{i:02d}" for i in range(15)),
        universe_type=CONTEXT_UNIVERSE_TYPE,
    )
    duplicated = experiment_spec_from_duplication(
        draft, current_config=source.config, use_run_config=True
    )
    generated = generate_symbol_sets(
        list(duplicated.predictor_symbols),
        duplicated.config.permutation_depth,
        target_symbols=list(duplicated.target_symbols),
    )

    assert duplicated.target_symbols == targets
    assert duplicated.context_symbols == market_context[:10]
    assert duplicated.predictor_symbols == (*targets, *market_context[:10])
    assert duplication_combination_count(draft, duplicated.config) == 1900
    assert len(generated) == 1900
