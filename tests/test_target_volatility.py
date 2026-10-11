from dataclasses import replace
from types import SimpleNamespace
import json

import exchange_calendars as xcals
import numpy as np
import pandas as pd
import pytest

from rstock.config import DEFAULT_CONFIG, HISTORICAL_MISSING_CONFIG_DEFAULTS
from rstock.features import model_predictor_columns, prepare_prediction_row
from rstock.target_volatility import (historical_volatility, attach, column, contract,
                                     freeze_prices, load_prices, operational_features)
from rstock.application.domain import ExperimentSpec, JobType
from rstock.modeling import production_training_config, validate_production_round_contract


def sample(tmp_path, kind="target_only", sessions=3):
    dates = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2025-01-02", "2025-02-14")).tz_localize(None)
    prices = pd.DataFrame({"AAA.Adjusted": 100 * np.exp(np.cumsum(np.arange(len(dates)) % 5 / 100)),
                           "BBB.Adjusted": 100 * np.exp(np.cumsum(np.arange(len(dates)) % 2 / 100))}, index=dates)
    prepared = pd.DataFrame({"AAA.intraday_return": .01, "AAA.intraday_target": 1,
                            "AAA.intraday_down_target": 0}, index=dates[5:-1])
    for symbol in ("AAA", "BBB"):
        for lag in (1, 2, 3):
            prepared[f"{symbol}_intraday_J-{lag}"] = .02
    config = replace(DEFAULT_CONFIG, project_root=tmp_path, predictive_model_type=kind,
                     lag_depth=3, target_volatility_enabled=True, target_volatility_sessions=sessions)
    return dates, prices, prepared, config


def test_exact_formula_strict_previous_session_and_no_future_access(tmp_path):
    dates, prices, _, config = sample(tmp_path)
    actual = historical_volatility(prices["AAA.Adjusted"], dates, 3)
    expected = np.std(np.log(prices["AAA.Adjusted"].iloc[3:6].to_numpy() / prices["AAA.Adjusted"].iloc[2:5].to_numpy()), ddof=1) * np.sqrt(252)
    assert actual.loc[dates[6]] == pytest.approx(expected)
    changed = prices["AAA.Adjusted"].copy()
    changed.loc[dates[6]:] *= 100
    assert historical_volatility(changed, dates, 3).loc[dates[6]] == actual.loc[dates[6]]
    assert actual.iloc[:4].isna().all()


def test_missing_expected_session_is_not_bridged_and_holidays_are_not_gaps(tmp_path):
    dates, prices, _, _ = sample(tmp_path)
    values = prices["AAA.Adjusted"].drop(dates[8])
    actual = historical_volatility(values, dates, 3)
    assert actual.loc[dates[9:13]].isna().all()
    assert np.isfinite(actual.loc[dates[13]])
    assert pd.Timestamp("2025-01-20") not in dates
    assert np.isfinite(historical_volatility(prices["AAA.Adjusted"], dates, 3).loc[pd.Timestamp("2025-01-21")])


@pytest.mark.parametrize("kind,count", [("constant_probability", 0), ("target_only", 4),
                                      ("target_and_external", 7), ("external_only", 4)])
def test_four_information_sets_and_single_target_feature(tmp_path, kind, count):
    _, prices, prepared, config = sample(tmp_path, kind)
    enriched = attach(prepared, prices, ["AAA", "BBB"], config)
    names = model_predictor_columns(enriched, "AAA", ["BBB"], config)
    assert len(names) == count
    assert (column("AAA", 3) in names) == (kind != "constant_probability")
    assert column("BBB", 3) not in names
    disabled = replace(config, target_volatility_enabled=False)
    assert attach(prepared, prices, ["AAA"], disabled) is prepared
    expected = [] if kind == "constant_probability" else names[:-1]
    assert model_predictor_columns(prepared, "AAA", ["BBB"], disabled) == expected


def test_shared_training_and_future_row_values_and_missing_history(tmp_path):
    dates, prices, prepared, config = sample(tmp_path)
    enriched = attach(prepared, prices, ["AAA"], config)
    target = dates[15]
    row = prepare_prediction_row(enriched, as_of_date=dates[14], target_date=target)
    assert row[column("AAA", 3)].iloc[0] == enriched.loc[target, column("AAA", 3)]
    changed = prices.copy()
    changed.loc[target:, "AAA.Adjusted"] = 1
    future = prepare_prediction_row(attach(prepared, changed, ["AAA"], config), as_of_date=dates[14], target_date=target)
    assert future[column("AAA", 3)].iloc[0] == row[column("AAA", 3)].iloc[0]
    broken = enriched.copy()
    broken.attrs.pop("target_volatility_prices")
    with pytest.raises(ValueError, match="source unavailable"):
        prepare_prediction_row(broken, as_of_date=dates[14], target_date=target)


def test_frozen_source_roundtrip_tamper_and_historical_config(tmp_path):
    _, prices, _, config = sample(tmp_path)
    source = freeze_prices(tmp_path, prices, "XNYS")
    pd.testing.assert_frame_equal(load_prices(tmp_path, source), prices, check_freq=False)
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config, symbols=("AAA",), target_volatility_source=source)
    restored = ExperimentSpec.from_dict(spec.to_dict())
    assert restored.target_volatility_source == source
    assert restored.config.target_volatility_enabled
    old = spec.to_dict()
    old["rstock_config"].pop("target_volatility_enabled")
    old["rstock_config"].pop("target_volatility_sessions")
    assert not ExperimentSpec.from_dict(old).config.target_volatility_enabled
    assert HISTORICAL_MISSING_CONFIG_DEFAULTS["target_volatility_sessions"] == 21
    path = tmp_path / source["path"]
    saved = json.loads(path.read_text())
    saved["values"][0][0] += 1
    path.write_text(json.dumps(saved))
    with pytest.raises(ValueError, match="digest differs"):
        load_prices(tmp_path, source)


def test_operational_model_owns_contract_independent_of_global_defaults(tmp_path):
    _, prices, prepared, config = sample(tmp_path, "external_only")
    model = SimpleNamespace(source_configuration={"rstock_config": {"target_volatility_enabled": True,
        "target_volatility_sessions": 3}}, target="AAA", training_metadata={}, round_selection_policy=None,
        xgboost_seed=1234, xgboost_threads=2)
    effective = production_training_config(replace(config, target_volatility_enabled=False, target_volatility_sessions=99), model)
    assert effective.target_volatility_enabled and effective.target_volatility_sessions == 3
    result = operational_features(prepared, model, prices)
    assert column("AAA", 3) in result
    validate_production_round_contract(model, {"target_volatility": contract(effective)})
    with pytest.raises(ValueError, match="volatility contract mismatch"):
        validate_production_round_contract(model, {})
    model.source_configuration = {}
    assert not production_training_config(config, model).target_volatility_enabled
    assert "target_volatility" not in operational_features(result, model).attrs


@pytest.mark.parametrize("value", [True, 1, 1.5, None])
def test_invalid_sessions_rejected(value):
    with pytest.raises(ValueError):
        replace(DEFAULT_CONFIG, target_volatility_sessions=value)


def test_derivation_reuses_frozen_prices_with_weekend_cutoff(tmp_path, monkeypatch):
    import rstock.target_volatility as volatility
    _, prices, prepared, config = sample(tmp_path)
    source = freeze_prices(tmp_path, prices, "XNYS")
    def forbidden(*args):
        raise AssertionError("Compatible frozen data must be reused")
    monkeypatch.setattr(volatility, "freeze_cached_source", forbidden)
    assert volatility.derivation_source(prepared, ["AAA"], config, "XNYS", "2025-02-16", source) == source


def test_wf_calibration_threshold_and_holdout_share_ordered_matrices(tmp_path, monkeypatch):
    import rstock.walk_forward as wf
    import rstock.calibration as calibration
    import rstock.threshold_calibration as threshold
    import rstock.modeling as modeling
    _, prices, prepared, config = sample(tmp_path)
    config = replace(config, combination_workers=1, walk_forward_min_train_size=4,
                     walk_forward_test_size=3, walk_forward_step_size=3)
    for suffix in ("overnight_return", "close_to_close_return", "mfe", "mae"):
        prepared[f"AAA.{suffix}"] = .01
    prepared = attach(prepared, prices, ["AAA"], config)
    names = model_predictor_columns(prepared, "AAA", [], config)
    matrices = []

    class Matrix:
        def __init__(self, frame, feature_names=None, label=None):
            assert list(frame.columns) == names
            assert feature_names == names
            matrices.append(frame.copy())
            self.size = len(frame)

        def set_label(self, label):
            pass

    class Booster:
        def predict(self, matrix):
            return np.full(matrix.size, .25)

        def attr(self, name):
            return None

    fake = SimpleNamespace(DMatrix=Matrix, train=lambda *a, **k: Booster())
    for module in (wf, calibration, modeling):
        monkeypatch.setattr(module, "xgboost_module", lambda: fake)
    development, holdout = prepared.iloc[:-3], prepared.iloc[-3:]
    row = {"V0": "AAA"}
    parameters = modeling.historical_xgboost_parameters(config)
    result = wf._walk_forward_combination(row, (prepared, config, holdout.index.min(), {}, 4, 3, 3), None)
    assert result.window_records[0]["TargetVolatilityEnabled"]
    wf_training = matrices[1].index.copy()  # First matrix is the WF test, then Up train.
    matrices.clear()
    calibration._calibration_combination(row, (development, config, (parameters,), {"fixed": parameters}, 4, 3, 3), None)
    assert matrices[0].index.equals(wf_training)
    matrices.clear()
    threshold._threshold_development_combination(row, (development, config, 4, 3, 3, {"Up": parameters, "Down": parameters}), None)
    assert matrices[0].index.equals(wf_training)
    matrices.clear()
    calibration.evaluate_locked_holdout(development, holdout, pd.DataFrame([row]), config,
        {"Up": parameters, "Down": parameters}, {"Up": {"configuration": "fixed"}, "Down": {"configuration": "fixed"}})
    assert matrices[0].index.max() < matrices[1].index.min()
    assert all(list(m.columns) == names for m in matrices)


def test_derived_volatility_pins_input_and_survives_cache_changes(tmp_path):
    from test_walk_forward_derivation import _parent
    from rstock.application.walk_forward_experiments import build_derived_walk_forward_spec
    from rstock.application.derived_snapshot import load_source_prepared_snapshot
    from rstock.application.repository import RunRepository
    from rstock.checkpoints import CheckpointManager
    repository, parent, _, _ = _parent(tmp_path)
    import pickle
    original = pickle.loads((repository.run_directory(parent) / "checkpoints/artifacts/prepared_snapshot.pkl").read_bytes())["prepared"]
    dates = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2026-04-01", "2026-07-07")).tz_localize(None)
    cache = tmp_path / "data/market/AAA.parquet"
    cache.parent.mkdir(parents=True)
    pd.DataFrame({"Adjusted": np.arange(len(dates)) + 100.}, index=dates).to_parquet(cache)
    source_bytes = {p: p.read_bytes() for p in repository.run_directory(parent).rglob("*") if p.is_file()}
    derived = build_derived_walk_forward_spec(repository, parent, {"target_volatility_enabled": True})
    first, *_ = load_source_prepared_snapshot(repository, derived)
    pd.testing.assert_frame_equal(first[original.columns], original)
    assert column("AAA", 21) in first
    run_id = repository.create(derived)
    checkpoint = CheckpointManager(repository.run_directory(run_id), run_id=run_id,
        job_type=derived.job_type.value, configuration_fingerprint=derived.fingerprint, batch_sizes={})
    checkpoint.start_attempt(resumed=False)
    checkpoint.commit_snapshot(first, {"test": True})
    checkpoint.finish_attempt("interrupted")
    pd.DataFrame({"Adjusted": np.arange(len(dates)) ** 2 + 50.}, index=dates).to_parquet(cache)
    restored = RunRepository(repository.root).load_spec(run_id)
    second, *_ = load_source_prepared_snapshot(repository, restored)
    pd.testing.assert_frame_equal(first, second)
    resumed = CheckpointManager(repository.run_directory(run_id), run_id=run_id,
        job_type=restored.job_type.value, configuration_fingerprint=restored.fingerprint, batch_sizes={})
    resumed.start_attempt(resumed=True)
    saved, _ = resumed.load_snapshot()
    pd.testing.assert_frame_equal(saved, first)
    assert all(p.read_bytes() == b for p, b in source_bytes.items())


def test_acquisition_prefix_preserves_labels_lags_and_scientific_boundaries(tmp_path, monkeypatch):
    from rstock.application import services
    from rstock.market_cache import MarketDataResult
    from rstock.features import prepare_dataset
    dates, prices, _, config = sample(tmp_path)
    stock = prices[["AAA.Adjusted"]].copy()
    for suffix, factor in (("Open", .99), ("High", 1.02), ("Low", .98), ("Close", 1.)):
        stock[f"AAA.{suffix}"] = stock["AAA.Adjusted"] * factor
    requested = []

    def get_market_data(universe, history_days, **kwargs):
        requested.append(history_days)
        start = pd.Timestamp(kwargs["as_of"]) - pd.Timedelta(days=history_days)
        return MarketDataResult(stock.loc[start:].copy(), ["AAA"], [], [])

    monkeypatch.setattr(services, "market_data_service", lambda cfg: SimpleNamespace(get_market_data=get_market_data))
    config = replace(config, model_history_days=15)
    spec = ExperimentSpec(job_type=JobType.WALK_FORWARD, config=config, symbols=("AAA",), target_symbols=("AAA",))
    result, _ = services.MarketDataService().load(spec, as_of=dates[-1].date())
    assert requested[0] > 15
    expected_stock = stock.loc[dates[-1] - pd.Timedelta(days=15):].copy()
    expected = prepare_dataset(expected_stock, ["AAA"])
    enriched = prepare_dataset(result.prices, ["AAA"])
    pd.testing.assert_frame_equal(enriched[expected.columns], expected)
    assert enriched[column("AAA", 3)].notna().all()
    assert enriched.index.min() == expected.index.min()
    assert enriched["AAA_intraday_J-3"].iloc[:3].isna().all()
    assert enriched.attrs["target_volatility"]["source"]["sha256"]


def test_prefilter_ignores_final_target_volatility_and_preserves_geometry(tmp_path, monkeypatch):
    import rstock.walk_forward as wf
    _, prices, prepared, config = sample(tmp_path)
    prepared = attach(prepared, prices, ["AAA"], config)
    captured = []

    def evaluate(row, context, cancellation, **kwargs):
        captured.append(context)
        names = model_predictor_columns(context[0], "AAA", ["BBB"], context[1])
        assert names == [f"BBB_intraday_J-{lag}" for lag in (1, 2, 3)]
        return SimpleNamespace(window_records=[], prediction_records=[])

    monkeypatch.setattr(wf, "_walk_forward_combination", evaluate)
    monkeypatch.setattr(wf, "qualify_combinations", lambda *args: pd.DataFrame([{"EligibleRank": 1, "Eligible": True}]))
    context = (prepared, config, prepared.index[-3], {"AAA": "XNYS"}, 4, 3, 3)
    assert wf._prefilter_combination({"V0": "AAA", "V1": "BBB"}, context, None)["Eligible"]
    assert captured[0][0] is prepared
    assert captured[0][2:] == context[2:]
    assert not captured[0][1].target_volatility_enabled


@pytest.mark.parametrize("split", [False, True])
def test_e2e_volatility_changes_start_at_wf_and_freeze_the_input(tmp_path, split):
    from test_derived_workflow import _source
    from rstock.application.derived_experiments import build_derived_spec
    from rstock.application.end_to_end import build_pipeline_manifest, build_stage_spec
    repository, parent, _, _ = _source(tmp_path, split=split)
    dates = pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2026-07-01", "2026-09-25")).tz_localize(None)
    directory = tmp_path / "data/market"
    directory.mkdir(parents=True)
    for symbol in ("AAA", "BBB"):
        pd.DataFrame({"Adjusted": np.arange(len(dates)) + 100.}, index=dates).to_parquet(directory / f"{symbol}.parquet")
    derived = build_derived_spec(repository, parent, "walk_forward", {"target_volatility_enabled": True})
    assert derived.target_volatility_source["sha256"]
    run_id = repository.create(derived)
    manifest = build_pipeline_manifest(repository, run_id, derived)
    child = build_stage_spec(repository, run_id, derived, "walk_forward", manifest)
    assert child.config.target_volatility_enabled
    assert child.target_volatility_source == derived.target_volatility_source
    assert all(stage["mode"] != "inherited" for stage in manifest["stages"] if stage["stage_key"] == "walk_forward")
    with pytest.raises(ValueError):
        build_derived_spec(repository, parent, "threshold_calibration", {"target_volatility_enabled": True})
