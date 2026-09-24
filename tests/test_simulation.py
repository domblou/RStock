import json
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import (
    DailyPredictionService,
    HistoricalReplayMode,
)
from rstock.application.repository import utc_now
from rstock.application.simulation import (
    SIMULATION_MODE_FROZEN_AT_START,
    SimulationService,
    benchmark_cumulative_for_trades,
    summarize_simulation_trades,
)
from rstock.application.simulation_repository import SimulationRepository
from rstock.config import DEFAULT_CONFIG


class FakeRepository:
    def __init__(
        self,
        signals,
        predictions=None,
        active_ids=(),
        evaluated_predictions=None,
        models=(),
    ):
        self.signals = signals
        self.predictions = predictions if predictions is not None else pd.DataFrame()
        self.evaluated_predictions = (
            evaluated_predictions
            if evaluated_predictions is not None
            else pd.DataFrame()
        )
        self._active_ids = frozenset(active_ids)
        self._models = list(models)

    def read_active_model_table(self, name):
        if name == "signals":
            return self.signals.copy()
        assert name == "realized_results"
        return self.evaluated_predictions.copy()

    def read_table(self, name):
        tables = {
            "signals": self.signals,
            "predictions": self.predictions,
            "realized_results": self.evaluated_predictions,
        }
        return tables[name].copy()

    def active_models(self):
        return [model for model in self._models if model.is_active]

    def active_model_ids(self):
        return self._active_ids


def _signal(identifier, date, symbol, model="model_one"):
    return {
        "signal_id": identifier,
        "prediction_id": identifier,
        "prediction_date": date,
        "created_at": date,
        "model_id": model,
        "target": symbol,
        "category": "bullish_signal",
        "up_probability": 0.72,
        "up_threshold": 0.65,
    }


def _prices(date, opened, closed):
    return pd.DataFrame({"Open": [opened], "Close": [closed]}, index=[pd.Timestamp(date)])


def _evaluated(identifier, date, symbol, opened, closed, model="model_one"):
    return {
        "result_id": identifier,
        "prediction_id": identifier,
        "prediction_date": date,
        "model_id": model,
        "target": symbol,
        "open": opened,
        "close": closed,
    }


def _model(model_id="model_active", status=ProductionModelStatus.ACTIVE):
    return ProductionModel(
        model_id=model_id,
        target="AAA",
        predictors=("BBB",),
        lag_depth=1,
        target_definition="Up/Down intraday",
        up_target_threshold=0.01,
        down_target_threshold=0.01,
        xgboost_parameters={
            "max_depth": 1, "eta": 0.1, "num_boost_round": 1,
            "min_child_weight": 1.0, "subsample": 1.0,
            "colsample_bytree": 1.0, "gamma": 0.0,
            "reg_alpha": 0.0, "reg_lambda": 1.0,
        },
        up_threshold=0.6,
        down_threshold=0.4,
        qualification_rules={},
        source_walk_forward_run="wf",
        source_xgboost_calibration_run=None,
        source_threshold_calibration_run=None,
        development_metrics={},
        holdout_metrics={},
        created_at=utc_now(),
        status=status,
        artifact_version=1,
        training_metadata={"calendar": "XNYS"},
    )


def _history():
    dates = pd.bdate_range("2026-01-01", periods=8)
    return {
        "AAA": pd.DataFrame({
            "Open": [100.0] * 8,
            "High": [102.0] * 8,
            "Low": [99.0] * 8,
            "Close": [101.0, 99.0, 102.0, 98.0, 103.0, 97.0, 104.0, 96.0],
        }, index=dates),
        "BBB": pd.DataFrame({
            "Open": [200.0] * 8,
            "High": [203.0] * 8,
            "Low": [197.0] * 8,
            "Close": [202.0, 198.0, 204.0, 196.0, 206.0, 194.0, 208.0, 192.0],
        }, index=dates),
    }


def test_simulation_calculates_winning_losing_kpis_and_cumulative_result():
    signals = pd.DataFrame([
        _signal("win", "2026-01-05", "AAA"),
        _signal("loss", "2026-01-06", "BBB"),
    ])
    prices = {
        "AAA": _prices("2026-01-05", 100.0, 110.0),
        "BBB": _prices("2026-01-06", 100.0, 95.0),
    }

    evaluated = pd.DataFrame([
        _evaluated("win", "2026-01-05", "AAA", 100.0, 110.0),
        _evaluated("loss", "2026-01-06", "BBB", 100.0, 95.0),
    ])
    result = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated), prices.get
    ).run(
        "2026-01-01", "2026-01-31", 1_000.0
    )

    assert result.trades["Statut"].tolist() == ["Gagnant", "Perdant"]
    assert result.trades["Profit / perte"].tolist() == pytest.approx([100.0, -50.0])
    assert result.metrics.total_profit_loss == pytest.approx(50.0)
    assert result.metrics.winning_trade_rate == 0.5
    assert result.metrics.average_return == pytest.approx(0.025)
    assert result.metrics.best_trade == pytest.approx(0.10)
    assert result.metrics.worst_trade == pytest.approx(-0.05)
    assert result.cumulative_results["Résultat cumulé"].tolist() == pytest.approx(
        [100.0, 50.0]
    )


def test_multiple_signals_and_models_on_one_day_are_independent_trades():
    signals = pd.DataFrame([
        _signal("one", "2026-02-02", "AAA", "model_one"),
        _signal("two", "2026-02-02", "BBB", "model_two"),
    ])
    prices = {
        "AAA": _prices("2026-02-02", 100.0, 102.0),
        "BBB": _prices("2026-02-02", 200.0, 204.0),
    }

    evaluated = pd.DataFrame([
        _evaluated("one", "2026-02-02", "AAA", 100.0, 102.0, "model_one"),
        _evaluated("two", "2026-02-02", "BBB", 200.0, 204.0, "model_two"),
    ])
    result = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated), prices.get
    ).run(
        "2026-02-02", "2026-02-02", 10_000.0
    )

    assert result.metrics.signals_found == 2
    assert result.metrics.calculated_trades == 2
    assert result.trades["Montant investi"].tolist() == [10_000.0, 10_000.0]
    assert result.metrics.total_profit_loss == pytest.approx(400.0)
    assert result.cumulative_results["Résultat cumulé"].tolist() == pytest.approx([400.0])


def test_evaluated_no_signal_prediction_does_not_create_a_financial_trade():
    signals = pd.DataFrame([
        _signal("bullish", "2026-02-03", "AAA"),
        {**_signal("no-signal", "2026-02-03", "BBB"), "category": "no_signal"},
    ])
    evaluated = pd.DataFrame([
        _evaluated("bullish", "2026-02-03", "AAA", 100.0, 110.0),
        _evaluated("no-signal", "2026-02-03", "BBB", 100.0, 50.0),
    ])

    result = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated),
        lambda _symbol: pytest.fail("Evaluated prices must be used directly"),
    ).run("2026-02-03", "2026-02-03", 1_000.0)

    assert result.trades["Symbole"].tolist() == ["AAA"]
    assert result.metrics.signals_found == 1
    assert result.metrics.calculated_trades == 1
    assert result.metrics.total_profit_loss == pytest.approx(100.0)
    assert result.metrics.winning_trade_rate == 1.0
    assert result.metrics.average_return == pytest.approx(0.10)


@pytest.mark.parametrize(
    "current_status",
    (
        ProductionModelStatus.ACTIVE,
        ProductionModelStatus.INACTIVE,
        ProductionModelStatus.RETIRED,
    ),
)
def test_evaluated_production_trade_survives_current_model_status_and_up_target(
    tmp_path, current_status
):
    model_id = f"model_{current_status.value}"
    repository = ProductionRepository(tmp_path)
    repository.add(_model(model_id, current_status))
    repository.write_table(
        "signals",
        pd.DataFrame([_signal("real-trade", "2026-09-15", "AAA", model_id)]),
    )
    repository.write_table(
        "predictions",
        pd.DataFrame([{
            "prediction_id": "real-trade",
            "prediction_date": "2026-09-15",
            "as_of_date": "2026-09-14",
            "model_id": model_id,
            "target": "AAA",
            "status": "predicted",
        }]),
    )
    repository.write_table(
        "realized_results",
        pd.DataFrame([{
            **_evaluated(
                "real-trade", "2026-09-15", "AAA", 53.79, 54.20, model_id
            ),
            "up_target": 0,
            "down_target": 0,
        }]),
    )

    result = SimulationService(
        repository,
        lambda _symbol: pytest.fail("Persisted evaluated prices must be used"),
    ).run("2026-09-15", "2026-09-15", 10_000.0)

    expected_return = 54.20 / 53.79 - 1.0
    assert result.metrics.signals_found == 1
    assert result.metrics.calculated_trades == 1
    assert result.metrics.winning_trade_rate == 1.0
    assert result.metrics.total_profit_loss == pytest.approx(10_000.0 * expected_return)
    assert result.metrics.average_return == pytest.approx(expected_return)
    assert result.trades["Rendement"].tolist() == pytest.approx([expected_return])
    assert result.cumulative_results.iloc[:, 1].tolist() == pytest.approx(
        [10_000.0 * expected_return]
    )


def test_spy_benchmark_uses_the_same_daily_notional_as_the_trades():
    signals = pd.DataFrame([_signal("trade", "2026-04-01", "AAA")])
    evaluated = pd.DataFrame([_evaluated("trade", "2026-04-01", "AAA", 100.0, 110.0)])
    service = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated),
        lambda _: pytest.fail("Evaluated trade must not reload its target price"),
        benchmark_price_loader=lambda symbol: (
            _prices("2026-04-01", 500.0, 510.0) if symbol == "SPY" else None
        ),
    )

    result = service.run("2026-04-01", "2026-04-01", 1_000.0)
    benchmark = benchmark_cumulative_for_trades(result.trades, result.benchmark_results)

    assert result.benchmark_results["SPY Rendement"].tolist() == pytest.approx([0.02])
    assert benchmark["SPY résultat cumulé"].tolist() == pytest.approx([20.0])


def test_spy_benchmark_counts_every_bullish_signal_even_without_target_price():
    trades = pd.DataFrame(
        {
            "Date trade": ["2026-04-01", "2026-04-01"],
            "Montant investi": [1_000.0, 1_000.0],
            "Rendement": [0.10, np.nan],
        }
    )
    benchmark_returns = pd.DataFrame(
        {"Date": ["2026-04-01"], "SPY Rendement": [0.02]}
    )

    benchmark = benchmark_cumulative_for_trades(trades, benchmark_returns)

    assert benchmark["SPY résultat cumulé"].tolist() == pytest.approx([40.0])


def test_evaluated_trade_requires_both_persisted_signal_and_realized_result():
    signals = pd.DataFrame([
        _signal("signal-only", "2026-04-01", "AAA"),
    ])
    evaluated = pd.DataFrame([
        _evaluated("result-only", "2026-04-01", "BBB", 100.0, 105.0),
    ])

    result = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated),
        lambda _symbol: pytest.fail("Unmatched facts must not load market prices"),
    ).run("2026-04-01", "2026-04-01")

    assert result.trades.empty
    assert result.metrics.signals_found == 0
    assert result.metrics.calculated_trades == 0


def test_missing_open_or_close_excludes_trade_without_estimation():
    signals = pd.DataFrame([
        _signal("open-missing", "2026-03-02", "AAA"),
        _signal("close-missing", "2026-03-02", "BBB"),
        _signal("no-market-row", "2026-03-02", "CCC"),
    ])
    prices = {
        "AAA": _prices("2026-03-02", None, 102.0),
        "BBB": _prices("2026-03-02", 100.0, None),
        "CCC": pd.DataFrame(columns=["Open", "Close"]),
    }

    evaluated = pd.DataFrame([
        _evaluated("open-missing", "2026-03-02", "AAA", None, 102.0),
        _evaluated("close-missing", "2026-03-02", "BBB", 100.0, None),
        _evaluated("no-market-row", "2026-03-02", "CCC", None, None),
    ])
    result = SimulationService(
        FakeRepository(signals, evaluated_predictions=evaluated), prices.get
    ).run(
        "2026-03-01", "2026-03-31"
    )

    assert result.trades["Statut"].tolist() == [
        "Prix d'achat manquant",
        "Prix de vente manquant",
        "Données de marché incomplètes",
    ]
    assert result.metrics.calculated_trades == 0
    assert result.metrics.excluded_trades == 3
    assert result.metrics.missing_prices == 3
    assert result.metrics.price_coverage == 0.0
    assert result.trades["Profit / perte"].isna().all()


def test_empty_period_and_invalid_dates_are_handled_explicitly():
    service = SimulationService(
        FakeRepository(pd.DataFrame([_signal("one", "2026-01-05", "AAA")])),
        lambda _: None,
    )

    empty = service.run("2026-02-01", "2026-02-28")

    assert empty.trades.empty
    assert empty.metrics.signals_found == 0
    assert empty.metrics.total_profit_loss == 0.0
    with pytest.raises(ValueError, match="date de début"):
        service.run("2026-02-02", "2026-02-01")
    with pytest.raises(ValueError, match="Date de début invalide"):
        service.run("not-a-date", "2026-02-01")


def test_historical_replay_uses_only_active_models_and_never_target_day_data(
    monkeypatch, tmp_path
):
    repository = ProductionRepository(tmp_path)
    active = _model()
    repository.add(active)
    repository.add(_model("model_inactive", ProductionModelStatus.INACTIVE))
    repository.add(_model("model_retired", ProductionModelStatus.RETIRED))
    directory = repository.artifact_directory(active.model_id)
    directory.mkdir(parents=True)
    (directory / "production.metadata.json").write_text(json.dumps({
        "model_id": active.model_id,
        "artifact_version": active.artifact_version,
        "feature_version": active.feature_version,
        "predictor_columns": ["BBB_intraday_J-1"],
    }), encoding="utf-8")
    fits = []
    predictions_seen = []

    def fake_fit(train, names, outcome, config, parameters=None):
        fits.append((train.index.max(), outcome, tuple(names)))
        return outcome

    def fake_predict(booster, current, names):
        predictions_seen.append((current.index[0], float(current.iloc[0][names[0]])))
        return np.array([0.8 if booster.endswith("intraday_target") else 0.2])

    monkeypatch.setattr(
        "rstock.application.production_services.fit_booster", fake_fit
    )
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities", fake_predict
    )
    history = _history()
    start, end = pd.Timestamp("2026-01-06"), pd.Timestamp("2026-01-09")

    replay = DailyPredictionService(repository).replay(
        history.get,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        start_date=start,
        end_date=end,
    )
    successful = replay[replay["status"] == "predicted"]

    assert set(successful["model_id"]) == {active.model_id}
    assert pd.to_datetime(successful["prediction_date"]).between(start, end).all()
    assert fits
    assert len(fits) == 8  # 4 target sessions × Up/Down; legacy daily behavior.
    assert all(train_end < target_date for (train_end, _, _), (target_date, _) in zip(
        fits[::2], predictions_seen[::2]
    ))
    expected_previous_return = history["BBB"].loc["2026-01-05", "Close"] / 200.0 - 1
    first_current_value = predictions_seen[0][1]
    assert first_current_value == pytest.approx(expected_previous_return)


def test_frozen_historical_replay_fits_once_at_the_session_before_start(monkeypatch, tmp_path):
    repository = ProductionRepository(tmp_path)
    active = _model()
    repository.add(active)
    directory = repository.artifact_directory(active.model_id)
    directory.mkdir(parents=True)
    (directory / "production.metadata.json").write_text(json.dumps({
        "model_id": active.model_id,
        "artifact_version": active.artifact_version,
        "feature_version": active.feature_version,
        "predictor_columns": ["BBB_intraday_J-1"],
    }), encoding="utf-8")
    fits, predictions_seen = [], []

    def fake_fit(train, names, outcome, config, parameters=None):
        fits.append((train.index.min(), train.index.max(), outcome))
        return f"frozen-{outcome}-{len(fits)}"

    def fake_predict(booster, current, names):
        predictions_seen.append((current.index[0], booster, float(current.iloc[0][names[0]])))
        return np.array([0.8 if "intraday_target" in booster else 0.2])

    monkeypatch.setattr("rstock.application.production_services.fit_booster", fake_fit)
    monkeypatch.setattr(
        "rstock.application.production_services.predict_probabilities", fake_predict
    )
    replay = DailyPredictionService(repository).replay(
        _history().get,
        replace(DEFAULT_CONFIG, project_root=tmp_path),
        start_date="2026-01-06",
        end_date="2026-01-09",
        mode=HistoricalReplayMode.FROZEN_AT_START,
    )

    assert len(fits) == 2
    assert {fit[1] for fit in fits} == {pd.Timestamp("2026-01-05")}
    assert replay["status"].eq("predicted").all()
    assert all(
        pd.Timestamp(row["as_of_date"]) < pd.Timestamp(row["prediction_date"])
        for _, row in replay.iterrows()
    )
    assert {booster for _, booster, _ in predictions_seen} == {
        "frozen-AAA.intraday_target-1",
        "frozen-AAA.intraday_down_target-2",
    }
    assert len({value for _, _, value in predictions_seen}) > 1
    assert replay.attrs["historical_replay"] == {
        "mode": "frozen_at_start",
        "version": 1,
        "initial_training_cutoff": "2026-01-05",
        "initial_training_cutoff_by_model": {active.model_id: "2026-01-05"},
        "first_predicted_session": "2026-01-06",
        "last_predicted_session": "2026-01-09",
        "model_count": 1,
    }


def test_historical_simulation_persists_frozen_replay_traceability(monkeypatch, tmp_path):
    active = _model()
    repository = FakeRepository(pd.DataFrame(), models=(active,), active_ids=(active.model_id,))
    predictions = pd.DataFrame([{
        "prediction_id": "frozen", "prediction_date": "2026-01-06",
        "as_of_date": "2026-01-05", "model_id": active.model_id,
        "target": active.target, "predictors": json.dumps(active.predictors),
        "status": "predicted", "up_probability": 0.8, "down_probability": 0.2,
        "up_threshold": active.signal_threshold, "down_threshold": active.down_threshold,
    }])
    predictions.attrs["historical_replay"] = {
        "mode": "frozen_at_start", "version": 1,
        "initial_training_cutoff": "2026-01-05",
        "first_predicted_session": "2026-01-06",
        "last_predicted_session": "2026-01-06", "model_count": 1,
    }
    monkeypatch.setattr(DailyPredictionService, "replay", lambda *args, **kwargs: predictions)
    result = SimulationService(
        repository, lambda _: _prices("2026-01-06", 100.0, 105.0)
    ).run_historical(
        "2026-01-06", "2026-01-06", DEFAULT_CONFIG, 1_000.0,
        mode=SIMULATION_MODE_FROZEN_AT_START,
    )

    assert result.historical_replay["mode"] == "frozen_at_start"
    assert result.historical_replay["initial_training_cutoff"] == "2026-01-05"
    assert result.metrics.total_profit_loss == pytest.approx(50.0)
    persisted = SimulationRepository(tmp_path).save(
        result,
        parameters={
            "simulation_mode": result.historical_replay["mode"],
            "historical_replay": result.historical_replay,
        },
        models=list(result.model_snapshots),
    )
    assert persisted["parameters"]["simulation_mode"] == "frozen_at_start"
    assert persisted["parameters"]["historical_replay"]["initial_training_cutoff"] == "2026-01-05"
    _, reopened = SimulationRepository(tmp_path).load(persisted["simulation_id"])
    assert reopened.historical_replay == result.historical_replay


def test_historical_and_persisted_modes_share_identical_trade_financials(monkeypatch):
    prediction = pd.DataFrame([{
        "prediction_id": "same", "prediction_date": "2026-02-02",
        "as_of_date": "2026-01-30", "model_id": "model_one", "target": "AAA",
        "predictors": '["BBB"]', "status": "predicted",
        "up_probability": 0.72, "down_probability": 0.20,
        "up_threshold": 0.65, "down_threshold": 0.40,
    }])
    signal = pd.DataFrame([_signal("same", "2026-02-02", "AAA")])
    repository = FakeRepository(
        signal,
        prediction,
        active_ids=("model_one",),
        evaluated_predictions=pd.DataFrame([
            _evaluated("same", "2026-02-02", "AAA", 100.0, 105.0),
        ]),
    )
    service = SimulationService(
        repository, lambda _: _prices("2026-02-02", 100.0, 105.0)
    )
    monkeypatch.setattr(
        DailyPredictionService, "replay", lambda *args, **kwargs: prediction.copy()
    )

    persisted = service.run("2026-02-02", "2026-02-02", 1_000.0)
    historical = service.run_historical(
        "2026-02-02", "2026-02-02", DEFAULT_CONFIG, 1_000.0
    )

    assert historical.trades["Rendement"].tolist() == persisted.trades["Rendement"].tolist()
    assert historical.trades["Profit / perte"].tolist() == persisted.trades["Profit / perte"].tolist()
    assert historical.metrics == persisted.metrics


def test_historical_mode_without_active_models_returns_empty_result(tmp_path):
    repository = ProductionRepository(tmp_path)
    repository.add(_model("inactive", ProductionModelStatus.INACTIVE))

    result = SimulationService(repository, lambda _: None).run_historical(
        "2026-01-01",
        "2026-01-31",
        replace(DEFAULT_CONFIG, project_root=tmp_path),
    )

    assert result.trades.empty
    assert result.metrics.signals_found == 0


def test_historical_mode_freezes_current_active_models_and_persisted_result(
    monkeypatch, tmp_path
):
    active = _model("model_active", ProductionModelStatus.ACTIVE)
    inactive = _model("model_inactive", ProductionModelStatus.INACTIVE)
    repository = FakeRepository(
        pd.DataFrame(),
        models=(active, inactive),
        active_ids=(active.model_id,),
    )
    replay_calls = []

    def fake_replay(
        _service, _price_loader, _config, *, start_date, end_date, models, mode
    ):
        replay_calls.append({
            "model_ids": tuple(model.model_id for model in models),
            "start": pd.Timestamp(start_date),
            "end": pd.Timestamp(end_date),
            "mode": mode,
        })
        if not models:
            return pd.DataFrame()
        return pd.DataFrame([{
            "prediction_id": "historical-active",
            "prediction_date": "2025-09-01",
            "as_of_date": "2025-08-29",
            "model_id": models[0].model_id,
            "model_version": models[0].artifact_version,
            "target": models[0].target,
            "predictors": json.dumps(models[0].predictors),
            "status": "predicted",
            "up_probability": 0.72,
            "down_probability": 0.20,
            "up_threshold": 0.65,
            "down_threshold": 0.40,
        }])

    monkeypatch.setattr(DailyPredictionService, "replay", fake_replay)
    service = SimulationService(
        repository, lambda _: _prices("2025-09-01", 100.0, 105.0)
    )

    original = service.run_historical(
        "2025-09-01", "2026-09-01", DEFAULT_CONFIG, 1_000.0
    )

    assert replay_calls[0] == {
        "model_ids": (active.model_id,),
        "start": pd.Timestamp("2025-09-01"),
        "end": pd.Timestamp("2026-09-01"),
        "mode": HistoricalReplayMode.DAILY_RETRAIN,
    }
    assert [model["model_id"] for model in original.model_snapshots] == [
        active.model_id
    ]
    assert original.trades.iloc[:, 3].tolist() == [active.model_id]

    persisted = SimulationRepository(tmp_path).save(
        original,
        parameters={"simulation_mode": "Historique"},
        models=list(original.model_snapshots),
    )
    repository._models = [
        replace(active, status=ProductionModelStatus.INACTIVE),
        inactive,
    ]

    _, reopened = SimulationRepository(tmp_path).load(persisted["simulation_id"])
    assert [model["model_id"] for model in reopened.model_snapshots] == [
        active.model_id
    ]
    assert reopened.metrics == original.metrics
    assert reopened.trades["Profit / perte"].tolist() == pytest.approx(
        original.trades["Profit / perte"].tolist()
    )

    new_result = service.run_historical(
        "2025-09-01", "2026-09-01", DEFAULT_CONFIG, 1_000.0
    )
    assert replay_calls[1]["model_ids"] == ()
    assert new_result.trades.empty
    assert new_result.model_snapshots == ()


def test_summarize_simulation_trades_recalculates_metrics_and_charts_for_filtered_population():
    trades = pd.DataFrame([
        {
            "Date trade": "2026-01-02",
            "Symbole": "AAA",
            "Modèle source": "model_a",
            "Rendement": 0.10,
            "Profit / perte": 100.0,
        },
        {
            "Date trade": "2026-01-02",
            "Symbole": "BBB",
            "Modèle source": "model_b",
            "Rendement": -0.05,
            "Profit / perte": -50.0,
        },
        {
            "Date trade": "2026-01-03",
            "Symbole": "AAA",
            "Modèle source": "model_a",
            "Rendement": None,
            "Profit / perte": None,
        },
    ])

    result = summarize_simulation_trades(trades[trades["Symbole"] == "AAA"])

    assert result.metrics.signals_found == 2
    assert result.metrics.calculated_trades == 1
    assert result.metrics.excluded_trades == 1
    assert result.metrics.total_profit_loss == pytest.approx(100.0)
    assert result.metrics.winning_trade_rate == pytest.approx(1.0)
    assert result.metrics.price_coverage == pytest.approx(0.5)
    assert result.cumulative_results.to_dict("records") == [
        {"Date": pd.Timestamp("2026-01-02"), "Résultat cumulé": 100.0}
    ]
    assert result.result_distribution.to_dict("records") == [
        {"Résultat": "Trades gagnants", "Trades": 1},
        {"Résultat": "Trades perdants", "Trades": 0},
    ]
