from dataclasses import replace

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_launch import (
    launch_walk_forward_config,
    walk_forward_confirmation_text,
    walk_forward_launch_controls_visible,
)
from rstock.config import DEFAULT_CONFIG


def test_walk_forward_launch_config_is_local_and_supports_expanding(tmp_path):
    global_config = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        walk_forward_window_mode="rolling",
        walk_forward_train_size=504,
    )

    local = launch_walk_forward_config(global_config, "expanding")

    assert local.walk_forward_window_mode == "expanding"
    assert local.walk_forward_train_size == 504
    assert global_config.walk_forward_window_mode == "rolling"
    assert global_config.walk_forward_train_size == 504


def test_walk_forward_launch_config_supports_rolling_252_and_504(tmp_path):
    global_config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    larger_global_config = replace(global_config, walk_forward_train_size=504)

    rolling_252 = launch_walk_forward_config(global_config, "rolling")
    rolling_504 = launch_walk_forward_config(larger_global_config, "rolling")

    assert rolling_252.walk_forward_window_mode == "rolling"
    assert rolling_252.walk_forward_train_size == 252
    assert rolling_504.walk_forward_window_mode == "rolling"
    assert rolling_504.walk_forward_train_size == 504


def test_walk_forward_launch_controls_are_limited_to_relevant_job_types():
    assert walk_forward_launch_controls_visible(JobType.WALK_FORWARD)
    assert walk_forward_launch_controls_visible(JobType.END_TO_END)
    assert not walk_forward_launch_controls_visible(JobType.XGBOOST_CALIBRATION)
    assert not walk_forward_launch_controls_visible(JobType.THRESHOLD_CALIBRATION)


def test_walk_forward_confirmation_text_uses_the_local_protocol(tmp_path):
    expanding = replace(
        DEFAULT_CONFIG,
        project_root=tmp_path,
        walk_forward_window_mode="expanding",
        walk_forward_min_train_size=252,
        walk_forward_test_size=63,
        walk_forward_step_size=63,
    )
    rolling = replace(
        expanding,
        walk_forward_window_mode="rolling",
        walk_forward_train_size=252,
    )

    assert walk_forward_confirmation_text(expanding) == (
        "Walk-forward : Expansive · train min 252 · test 63 · step 63"
    )
    assert walk_forward_confirmation_text(rolling) == (
        "Walk-forward : Glissante 252 · test 63 · step 63"
    )


def test_walk_forward_and_end_to_end_snapshots_freeze_the_local_choice(tmp_path):
    global_config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    local = launch_walk_forward_config(global_config, "rolling")

    for job_type in (JobType.WALK_FORWARD, JobType.END_TO_END):
        spec = ExperimentSpec(job_type, local, symbols=("AAA", "BBB"))
        restored = ExperimentSpec.from_dict(spec.to_dict())

        assert restored.config.walk_forward_window_mode == "rolling"
        assert restored.config.walk_forward_train_size == 252

    changed_global = replace(
        global_config,
        walk_forward_window_mode="expanding",
        walk_forward_train_size=504,
    )
    assert changed_global.walk_forward_window_mode == "expanding"
    assert local.walk_forward_window_mode == "rolling"
    assert local.walk_forward_train_size == 252
