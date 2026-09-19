from __future__ import annotations

import copy

import pandas as pd
import pytest

from rstock.application.real_trades import (
    RealTradeService,
    filter_performance,
    performance_kpis,
    performance_table,
)


def _prediction(identifier: str = "prediction-1", *, date: str = "2026-09-15") -> dict[str, object]:
    return {
        "prediction_id": identifier,
        "model_id": "model-retired-later",
        "model_version": "v7",
        "prediction_date": date,
        "target": "AAA",
        "category": "bullish_signal",
        "intraday_return": 0.01,
    }


def test_real_trade_create_update_prevents_duplicate_and_persists(tmp_path):
    source = _prediction()
    original = copy.deepcopy(source)
    service = RealTradeService(tmp_path)

    first = service.save_from_prediction(
        source, entry_price=100, exit_price=110, quantity=4, note="initiale"
    )
    updated = service.save_from_prediction(
        source, entry_price=100, exit_price=105, quantity=2, note="corrigée"
    )

    assert first.trade_id == updated.trade_id
    assert updated.created_at == first.created_at
    assert updated.real_return == pytest.approx(0.05)
    assert updated.gross_pnl == pytest.approx(10.0)
    assert len(service.trades()) == 1
    reloaded = RealTradeService(tmp_path).trades()
    assert len(reloaded) == 1
    assert reloaded[0].note == "corrigée"
    assert source == original  # the scientific source record is never mutated


def test_real_trade_delete_allows_clean_reregistration_without_touching_source(tmp_path):
    service = RealTradeService(tmp_path)
    source = _prediction()
    created = service.save_from_prediction(source, entry_price=10, exit_price=11, quantity=3)

    assert service.delete(created.trade_id)
    assert service.trades() == []
    replacement = service.save_from_prediction(source, entry_price=20, exit_price=18, quantity=5)

    assert replacement.trade_id != created.trade_id
    assert len(service.trades()) == 1
    assert source["prediction_id"] == "prediction-1"


def test_real_trade_performance_kpis_filters_and_theoretical_comparison(tmp_path):
    service = RealTradeService(tmp_path)
    service.save_from_prediction(_prediction("one", date="2026-09-10"), entry_price=100, exit_price=110, quantity=2)
    service.save_from_prediction(_prediction("two", date="2026-09-20"), entry_price=100, exit_price=90, quantity=3)
    realized = pd.DataFrame([
        {"prediction_id": "one", "intraday_return": 0.04},
        {"prediction_id": "two", "intraday_return": -0.02},
    ])

    table = performance_table(service.trades(), realized)
    metrics = performance_kpis(table)
    september_early = filter_performance(table, start="2026-09-01", end="2026-09-15")

    assert metrics["transaction_count"] == 2
    assert metrics["total_pnl"] == pytest.approx(-10.0)
    assert metrics["mean_return"] == pytest.approx(0.0)
    assert metrics["win_rate"] == pytest.approx(0.5)
    assert metrics["average_gain"] == pytest.approx(20.0)
    assert metrics["average_loss"] == pytest.approx(-30.0)
    assert metrics["profit_factor"] == pytest.approx(2 / 3)
    assert len(september_early) == 1
    assert september_early.iloc[0]["theoretical_return"] == pytest.approx(0.04)
    assert september_early.iloc[0]["real_vs_theoretical"] == pytest.approx(0.06)


def test_real_trade_requires_a_stable_bullish_prediction_identity(tmp_path):
    service = RealTradeService(tmp_path)
    with pytest.raises(ValueError, match="identité historique"):
        service.save_from_prediction({}, entry_price=1, exit_price=2, quantity=1)
    with pytest.raises(ValueError, match="signaux haussiers"):
        service.save_from_prediction(
            {**_prediction(), "category": "no_signal"}, entry_price=1, exit_price=2, quantity=1
        )
    with pytest.raises(ValueError, match="entier positif"):
        service.save_from_prediction(
            _prediction(), entry_price=1, exit_price=2, quantity=1.5
        )
