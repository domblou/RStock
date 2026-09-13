from dataclasses import replace

import pytest

from rstock.application.experiment_duplication import (
    config_from_historical_snapshot,
    duplication_submission_values,
    walk_forward_duplication_draft,
)
from rstock.config import DEFAULT_CONFIG


def _detail(*, job_type="walk_forward", snapshot=None):
    return {
        "configuration": {
            "job_type": job_type,
            "symbols": ["DIS", "AMZN", "NVDA"],
            "primary_universe_id": "primary",
            "context_universe_ids": ["market"],
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
    assert draft["target_symbols"] == ["DIS", "AMZN"]
    assert draft["context_symbols"] == ["NVDA"]
    assert draft["predictor_symbols"] == ["DIS", "AMZN", "NVDA"]
    assert draft["calendar"] == "XNYS"
    assert draft["combinations_per_target"] == 7
    assert draft["evaluate_final_holdout"] is False
    assert draft["rstock_config"] == {"xgb_seed": 99, "lag_depth": 4}

    draft["rstock_config"]["xgb_seed"] = 12
    assert detail["configuration"]["rstock_config"]["xgb_seed"] == 99


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
