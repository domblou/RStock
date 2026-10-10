"""Dedicated prefilter training, persistence and historical execution contracts."""

from contextlib import nullcontext
from dataclasses import replace
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from rstock.application import streamlit_app, workflows
from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_duplication import config_from_historical_snapshot
from rstock.application.prefilter_experiments import PREFILTER_XGBOOST_FIELDS
from rstock.application.repository import RunRepository
from rstock.config import (
    DEFAULT_CONFIG, PREFILTER_XGBOOST_LEGACY_FIELDS, UI_SETTINGS_DEFAULTS,
    load_user_settings, save_user_settings,
)
from rstock.features import prepare_dataset
from rstock.modeling import historical_xgboost_parameters, prefilter_xgboost_snapshot
import rstock.walk_forward as walk_forward


DEFAULTS = {
    "prefilter_xgb_max_depth": 3, "prefilter_xgb_eta": 0.2,
    "prefilter_xgb_num_boost_round": 20, "prefilter_xgb_min_child_weight": 1.0,
    "prefilter_xgb_subsample": 1.0, "prefilter_xgb_colsample_bytree": 1.0,
    "prefilter_xgb_gamma": 0.0, "prefilter_xgb_reg_alpha": 0.0,
    "prefilter_xgb_reg_lambda": 5.0, "prefilter_xgb_seed": 1234,
}
CUSTOM = {
    "prefilter_xgb_max_depth": 2, "prefilter_xgb_eta": 0.15,
    "prefilter_xgb_num_boost_round": 7, "prefilter_xgb_min_child_weight": 2.0,
    "prefilter_xgb_subsample": 0.8, "prefilter_xgb_colsample_bytree": 0.7,
    "prefilter_xgb_gamma": 0.1, "prefilter_xgb_reg_alpha": 0.2,
    "prefilter_xgb_reg_lambda": 4.0, "prefilter_xgb_seed": 5678,
}


def test_prefilter_defaults_and_settings_roundtrip(tmp_path):
    assert {field: getattr(DEFAULT_CONFIG, field) for field in DEFAULTS} == DEFAULTS
    base = replace(DEFAULT_CONFIG, project_root=tmp_path)
    config = replace(base, **CUSTOM, predictor_prefilter_enabled=True)
    save_user_settings(config, UI_SETTINGS_DEFAULTS, default_config=base)
    loaded, _, warning = load_user_settings(base)
    assert warning is None
    assert loaded == config
    spec = ExperimentSpec(job_type=JobType.PREDICTOR_PREFILTER, config=config, symbols=("AAA", "BBB"),
                          historical_data_cutoff="2026-09-25")
    snapshot = spec.to_dict()
    assert {field: snapshot["rstock_config"][field] for field in CUSTOM} == CUSTOM
    assert ExperimentSpec.from_dict(snapshot).config == config


@pytest.mark.parametrize("partial", [False, True])
def test_historical_snapshot_and_duplication_preserve_old_prefilter_parameters(tmp_path, partial):
    base = replace(DEFAULT_CONFIG, project_root=tmp_path, xgb_max_depth=8,
                   xgb_eta=0.33, xgb_rounds=13, xgb_reg_lambda=2.5, xgb_seed=999)
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=base, symbols=("AAA", "BBB"))
    snapshot = spec.to_dict()
    old = snapshot["rstock_config"]
    for field in DEFAULTS:
        del old[field]
    if partial:
        old["prefilter_xgb_eta"] = 0.12
        old["prefilter_xgb_seed"] = 777
    expected = {
        field: old.get(field, old[general])
        for field, general in PREFILTER_XGBOOST_LEGACY_FIELDS.items()
    }
    restored = ExperimentSpec.from_dict(snapshot)
    duplicated = config_from_historical_snapshot(old, current_project_root=tmp_path)
    for config in (restored.config, duplicated):
        assert {field: getattr(config, field) for field in DEFAULTS} == expected
        assert historical_xgboost_parameters(config) == historical_xgboost_parameters(base)
    assert ExperimentSpec.from_dict(restored.to_dict()).config == restored.config
    # Also cover partial snapshots that predate the general regularization fields.
    sparse = config_from_historical_snapshot({"xgb_eta": 0.4}, current_project_root=tmp_path)
    assert sparse.prefilter_xgb_eta == 0.4
    assert sparse.prefilter_xgb_reg_lambda == DEFAULT_CONFIG.xgb_reg_lambda


def test_old_user_settings_use_modern_prefilter_defaults_for_new_runs(tmp_path):
    import json
    from rstock.config import user_settings_path
    path = user_settings_path(tmp_path)
    path.parent.mkdir(parents=True)
    path.write_text(json.dumps({"config": {"xgb_eta": 0.8, "xgb_seed": 999}}))
    config, _, warning = load_user_settings(replace(DEFAULT_CONFIG, project_root=tmp_path))
    assert warning is None
    assert config.xgb_eta == 0.8 and config.xgb_seed == 999
    assert {field: getattr(config, field) for field in DEFAULTS} == DEFAULTS


def test_sparse_historical_defaults_are_independent_of_modern_general_defaults(tmp_path, monkeypatch):
    import rstock.config as config_module
    monkeypatch.setattr(config_module, "DEFAULT_CONFIG", replace(
        DEFAULT_CONFIG, xgb_max_depth=9, xgb_eta=0.07, xgb_rounds=100, xgb_reg_lambda=8.0,
    ))
    values = config_module.historical_prefilter_config_values({})
    assert values["prefilter_xgb_max_depth"] == 6
    assert values["prefilter_xgb_eta"] == 1.0
    assert values["prefilter_xgb_num_boost_round"] == 4
    assert values["prefilter_xgb_reg_lambda"] == 1.0


@pytest.mark.parametrize("field,value", [
    ("prefilter_xgb_max_depth", 0), ("prefilter_xgb_num_boost_round", 0),
    ("prefilter_xgb_seed", -1), ("prefilter_xgb_seed", 1.5),
    ("prefilter_xgb_eta", 0.0), ("prefilter_xgb_subsample", 1.1),
    ("prefilter_xgb_colsample_bytree", 0.0), ("prefilter_xgb_reg_lambda", -1.0),
    ("prefilter_xgb_gamma", float("nan")),
])
def test_invalid_dedicated_parameters_are_rejected(field, value):
    with pytest.raises(ValueError, match=field):
        replace(DEFAULT_CONFIG, **{field: value})


@pytest.mark.parametrize("mode", ["prefilter", "temporal_prefilter", "wf", "resumable_wf", "planned_wf"])
def test_jobs_train_prefilter_and_walk_forward_with_distinct_parameters(tmp_path, monkeypatch, mode):
    config = replace(
        DEFAULT_CONFIG, project_root=tmp_path, **CUSTOM,
        predictor_prefilter_enabled=True, predictor_prefilter_top_n=2,
        lag_depth=1, permutation_depth=1, combination_workers=1, xgb_nthread=1,
        xgb_max_depth=6, xgb_eta=0.6, xgb_rounds=3, xgb_min_child_weight=3.0,
        xgb_subsample=0.9, xgb_colsample_bytree=0.95, xgb_gamma=0.3,
        xgb_reg_alpha=0.4, xgb_reg_lambda=1.5, xgb_seed=8765,
        walk_forward_min_train_size=10, walk_forward_test_size=5,
        walk_forward_step_size=5, final_holdout_size=5,
        walk_forward_max_combinations_per_batch=100 if mode == "planned_wf" else None,
        predictor_prefilter_min_median_auc=0.0,
        predictor_prefilter_min_pct_above_random=0.0,
        predictor_prefilter_min_worst_auc=0.0, predictor_prefilter_max_auc_std=1.0,
        qualification_min_windows=1, qualification_min_median_auc=0.0,
        qualification_min_pct_windows_above_random=0.0,
        qualification_min_worst_window_auc=0.0, qualification_max_auc_std=1.0,
        qualification_min_positive_observations=0,
    )
    index = pd.bdate_range(end="2026-09-25", periods=40)
    prices = pd.DataFrame(index=index)
    for offset, symbol in enumerate(("AAA", "BBB", "CCC")):
        prices[f"{symbol}.Open"] = 100.0
        prices[f"{symbol}.Close"] = np.where((np.arange(40) + offset) % 2, 102.0, 98.0)
        prices[f"{symbol}.High"] = 103.0
        prices[f"{symbol}.Low"] = 97.0
    prepared = prepare_dataset(prices, ["AAA", "BBB", "CCC"])
    prepared.attrs["effective_end_date"] = index[-1].isoformat()
    prepared.attrs["symbols_used"] = 3
    monkeypatch.setattr(workflows, "_prepared_inputs", lambda *_a, **_k: (
        prepared.copy(), ["AAA", "BBB", "CCC"], ["AAA"],
        {symbol: "XNYS" for symbol in ("AAA", "BBB", "CCC")},
    ))
    trained = []

    def fit(_matrix, effective_config, *, parameters):
        trained.append({**parameters.as_dict(), "seed": effective_config.xgb_seed,
                        "nthread": effective_config.xgb_nthread})
        return object()

    monkeypatch.setattr(walk_forward, "fit_booster_matrix", fit)
    monkeypatch.setattr(walk_forward, "predict_probabilities_matrix", lambda _b, matrix:
                        np.where(np.arange(matrix.num_row()) % 2, 0.7, 0.3))
    job = JobType.PREDICTOR_PREFILTER if "prefilter" in mode else JobType.WALK_FORWARD
    spec = ExperimentSpec(
        job_type=job, config=config, symbols=("AAA", "BBB", "CCC"),
        target_symbols=("AAA",), context_symbols=("BBB", "CCC"),
        predictor_symbols=("AAA", "BBB", "CCC"),
        historical_data_cutoff="2026-09-25", evaluate_final_holdout=False,
        prefilter_method="temporal_stability" if mode == "temporal_prefilter" else "single_origin",
        stability_origin_count=2 if job is JobType.PREDICTOR_PREFILTER else 5,
    )
    repository = RunRepository(tmp_path / "runs")
    if job is JobType.WALK_FORWARD:
        import hashlib
        from rstock.application.domain import JobStatus
        parent_spec = replace(spec, job_type=JobType.PREDICTOR_PREFILTER)
        parent = repository.create(parent_spec)
        parent_summary = workflows._predictor_prefilter(parent_spec, repository.run_directory(parent) / "results", None, None)
        repository.write_json(parent, "summary.json", parent_summary)
        repository.transition(parent, JobStatus.RUNNING)
        repository.transition(parent, JobStatus.COMPLETED)
        path = repository.run_directory(parent) / "results/prefilter_contract.json"
        spec = replace(spec, source_prefilter_run=parent,
                       source_prefilter_contract_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                       source_prepared_dataset_sha256=parent_summary["traceability"]["prepared_dataset_sha256"])
    run_id = repository.create(spec)
    output = repository.run_directory(run_id) / (
        "_working" if mode in {"resumable_wf", "planned_wf"} else "results"
    )
    execute = workflows._predictor_prefilter if job is JobType.PREDICTOR_PREFILTER else workflows._walk_forward
    execute(repository.load_spec(run_id), output, None, None)
    prefilter_expected = prefilter_xgboost_snapshot(config)
    wf_expected = {**historical_xgboost_parameters(config).as_dict(),
                   "seed": config.xgb_seed, "nthread": config.xgb_nthread}
    assert trained and trained[0] == prefilter_expected
    assert set(tuple(values.items()) for values in trained) <= {
        tuple(prefilter_expected.items()), tuple(wf_expected.items()),
    }
    if job is JobType.WALK_FORWARD:
        first_wf = trained.index(wf_expected)
        assert first_wf > 0
        assert all(values == prefilter_expected for values in trained[:first_wf])
        assert all(values == wf_expected for values in trained[first_wf:])
    else:
        assert all(values == prefilter_expected for values in trained)
    import json
    artifact = json.loads((output / "predictor_prefilter.json").read_text())
    assert artifact["xgboost_parameters"] == prefilter_expected
    if job is JobType.WALK_FORWARD:
        result = json.loads((output / "run_configuration.json").read_text())
        assert result["predictor_prefilter"]["xgboost_parameters"] == prefilter_expected
    assert repository.load_spec(run_id).config == config


def test_settings_ui_renders_edits_and_saves_prefilter_fields_in_prefilter_section(monkeypatch):
    shown = {}
    borders = []
    headings = []

    class FakeStreamlit:
        def __enter__(self):
            return self

        def __exit__(self, *args):
            return False

        def container(self, **kwargs):
            borders.append(kwargs)
            return nullcontext()

        def columns(self, count):
            return [self] * count

        def subheader(self, label):
            headings.append(label)

        def number_input(self, label, *, key, value, **kwargs):
            field = key.removeprefix("settings-")
            shown[field] = (label, value)
            return CUSTOM.get(field, value)

        def selectbox(self, label, choices, **kwargs):
            return choices[kwargs["index"]]

        def caption(self, *args):
            pass

    monkeypatch.setattr(streamlit_app, "st", FakeStreamlit())
    values = streamlit_app._prefilter_xgboost_settings(DEFAULT_CONFIG)
    assert {field: values[field] for field in CUSTOM} == CUSTOM
    assert values["prefilter_xgb_round_selection_mode"] == "fixed"
    assert values["prefilter_xgb_early_stopping_max_rounds"] == 500
    assert set(PREFILTER_XGBOOST_FIELDS) <= set(shown)
    assert all(shown[field] == (field.removeprefix("prefilter_xgb_"), value)
               for field, value in DEFAULTS.items())
    assert borders == [{"border": True}]
    assert headings == ["XGBoost du préfiltre"]
    source = Path(streamlit_app.__file__).read_text(encoding="utf-8")
    settings = source.split("def _settings()", 1)[1].split("def _history_model_contexts", 1)[0]
    prefilter = settings.split('st.subheader("Pré-filtrage des prédicteurs")', 1)[1].split(
        'st.subheader("Walk-forward")', 1)[0]
    assert "prefilter_xgboost_values = _prefilter_xgboost_settings(current, disabled=prefilter_disabled)" in prefilter
    assert "**prefilter_xgboost_values" in settings
    assert "save_user_settings(" in settings
