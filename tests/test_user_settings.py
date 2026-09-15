from dataclasses import replace

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
    assert warning is None
    assert loaded_ui["sensitivity_threshold_min"] == 0.15
    assert loaded_ui["sensitivity_threshold_max"] == 0.55
    assert loaded_ui["sensitivity_threshold_step"] == 0.02
