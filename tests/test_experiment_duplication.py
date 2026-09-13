from dataclasses import replace

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

    assert duplicated.to_dict() == source.to_dict()


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
    [JobType.THRESHOLD_CALIBRATION, JobType.XGBOOST_CALIBRATION],
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
    assert from_current.config is current
    for field in ("target_symbols", "context_symbols", "predictor_symbols"):
        assert getattr(from_run, field) == tuple(draft[field])
        assert getattr(from_current, field) == tuple(draft[field])
    assert detail == _detail(snapshot={"xgb_seed": 99})


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
