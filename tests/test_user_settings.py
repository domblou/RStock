from dataclasses import replace

import pytest

from rstock.config import (
    DEFAULT_CONFIG,
    UI_SETTINGS_DEFAULTS,
    load_user_settings,
    save_user_settings,
)


def test_sensitivity_threshold_settings_are_persisted_as_ui_settings(tmp_path):
    default_config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    config = replace(
        default_config,
        threshold_calibration_precision_tolerance=0.005,
        predictor_prefilter_batch_size=7,
        walk_forward_batch_size=11,
        final_holdout_batch_size=3,
        walk_forward_max_combinations_per_batch=12_345,
        xgboost_global_max_qualified_combinations=321,
        threshold_parameter_calibration_max_models=248,
    )
    ui_settings = {
        **UI_SETTINGS_DEFAULTS,
        "sensitivity_threshold_min": 0.15,
        "sensitivity_threshold_max": 0.55,
        "sensitivity_threshold_step": 0.02,
    }

    save_user_settings(config, ui_settings, default_config=default_config)
    loaded_config, loaded_ui, warning = load_user_settings(default_config)

    assert loaded_config == config
    assert loaded_config.threshold_calibration_precision_tolerance == 0.005
    assert loaded_config.predictor_prefilter_batch_size == 7
    assert loaded_config.walk_forward_batch_size == 11
    assert loaded_config.final_holdout_batch_size == 3
    assert loaded_config.walk_forward_max_combinations_per_batch == 12_345
    assert loaded_config.xgboost_global_max_qualified_combinations == 321
    assert loaded_config.threshold_parameter_calibration_max_models == 248
    assert warning is None
    assert loaded_ui["sensitivity_threshold_min"] == 0.15
    assert loaded_ui["sensitivity_threshold_max"] == 0.55
    assert loaded_ui["sensitivity_threshold_step"] == 0.02


def test_walk_forward_batch_capacity_default_is_2_200_000():
    assert DEFAULT_CONFIG.walk_forward_max_combinations_per_batch == 2_200_000


@pytest.mark.parametrize(
    ("name", "value"),
    [
        ("walk_forward_max_combinations_per_batch", 0),
        ("walk_forward_max_combinations_per_batch", -1),
        ("xgboost_global_max_qualified_combinations", 0),
        ("threshold_parameter_calibration_max_models", 1),
        ("threshold_parameter_calibration_max_models", 501),
    ],
)
def test_new_capacity_limits_reject_invalid_values(name, value):
    with pytest.raises(ValueError):
        replace(DEFAULT_CONFIG, **{name: value})


def test_new_capacity_limits_explicitly_support_unbounded_historical_values(tmp_path):
    default_config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    config = replace(
        default_config,
        walk_forward_max_combinations_per_batch=None,
        xgboost_global_max_qualified_combinations=None,
        threshold_parameter_calibration_max_models=None,
    )

    save_user_settings(config, UI_SETTINGS_DEFAULTS, default_config=default_config)
    loaded, _, warning = load_user_settings(default_config)

    assert loaded == config
    assert warning is None
