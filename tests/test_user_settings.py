from dataclasses import replace

from rstock.config import (
    DEFAULT_CONFIG,
    UI_SETTINGS_DEFAULTS,
    load_user_settings,
    save_user_settings,
)


def test_sensitivity_threshold_settings_are_persisted_as_ui_settings(tmp_path):
    config = replace(DEFAULT_CONFIG, project_root=tmp_path)
    ui_settings = {
        **UI_SETTINGS_DEFAULTS,
        "sensitivity_threshold_min": 0.15,
        "sensitivity_threshold_max": 0.55,
        "sensitivity_threshold_step": 0.02,
    }

    save_user_settings(config, ui_settings, default_config=config)
    loaded_config, loaded_ui, warning = load_user_settings(config)

    assert loaded_config == config
    assert warning is None
    assert loaded_ui["sensitivity_threshold_min"] == 0.15
    assert loaded_ui["sensitivity_threshold_max"] == 0.55
    assert loaded_ui["sensitivity_threshold_step"] == 0.02
