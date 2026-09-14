import pandas as pd
import pytest

from rstock.application.simulation import SimulationService


class FakeRepository:
    def __init__(self, signals, predictions=None):
        self.signals = signals
        self.predictions = predictions if predictions is not None else pd.DataFrame()

    def read_active_model_table(self, name):
        assert name == "signals"
        return self.signals.copy()

    def read_table(self, name):
        assert name == "predictions"
        return self.predictions.copy()


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
