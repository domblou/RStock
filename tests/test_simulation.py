import json
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_services import DailyPredictionService
from rstock.application.repository import utc_now
from rstock.application.simulation import SimulationService
from rstock.config import DEFAULT_CONFIG


class FakeRepository:
    def __init__(self, signals, predictions=None, active_ids=()):
        self.signals = signals
        self.predictions = predictions if predictions is not None else pd.DataFrame()
        self._active_ids = frozenset(active_ids)

    def read_active_model_table(self, name):
        assert name == "signals"
        return self.signals.copy()

    def read_table(self, name):
        assert name == "predictions"
        return self.predictions.copy()

    def active_models(self):
        return []

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

    result = SimulationService(FakeRepository(signals), prices.get).run(
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

    result = SimulationService(FakeRepository(signals), prices.get).run(
        "2026-02-02", "2026-02-02", 10_000.0
    )

    assert result.metrics.signals_found == 2
    assert result.metrics.calculated_trades == 2
    assert result.trades["Montant investi"].tolist() == [10_000.0, 10_000.0]
    assert result.metrics.total_profit_loss == pytest.approx(400.0)
    assert result.cumulative_results["Résultat cumulé"].tolist() == pytest.approx([400.0])


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

    result = SimulationService(FakeRepository(signals), prices.get).run(
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
    assert all(train_end < target_date for (train_end, _, _), (target_date, _) in zip(
        fits[::2], predictions_seen[::2]
    ))
    expected_previous_return = history["BBB"].loc["2026-01-05", "Close"] / 200.0 - 1
    first_current_value = predictions_seen[0][1]
    assert first_current_value == pytest.approx(expected_previous_return)


def test_historical_and_persisted_modes_share_identical_trade_financials(monkeypatch):
    prediction = pd.DataFrame([{
        "prediction_id": "same", "prediction_date": "2026-02-02",
        "as_of_date": "2026-01-30", "model_id": "model_one", "target": "AAA",
        "predictors": '["BBB"]', "status": "predicted",
        "up_probability": 0.72, "down_probability": 0.20,
        "up_threshold": 0.65, "down_threshold": 0.40,
    }])
    signal = pd.DataFrame([_signal("same", "2026-02-02", "AAA")])
    repository = FakeRepository(signal, prediction, active_ids=("model_one",))
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
