import pandas as pd

from rstock.application.simulation import SimulationMetrics, SimulationResult
from rstock.application.simulation_repository import SimulationRepository


def _result() -> SimulationResult:
    trades = pd.DataFrame([{"Date trade": "2026-01-02", "Profit / perte": 25.0}])
    metrics = SimulationMetrics(
        total_profit_loss=25.0,
        winning_trade_rate=1.0,
        calculated_trades=1,
        average_return=0.01,
        signals_found=1,
        excluded_trades=0,
        average_winning_return=0.01,
        average_losing_return=None,
        best_trade=0.01,
        worst_trade=0.01,
        missing_prices=0,
        price_coverage=1.0,
    )
    return SimulationResult(
        trades=trades,
        metrics=metrics,
        cumulative_results=pd.DataFrame([{"Date": "2026-01-02", "Résultat cumulé": 25.0}]),
        result_distribution=pd.DataFrame([{"Résultat": "Trades gagnants", "Trades": 1}]),
    )


def test_simulation_repository_round_trips_and_isolates_records(tmp_path):
    repository = SimulationRepository(tmp_path)
    metadata = repository.save(
        _result(),
        parameters={"start_date": "2026-01-01", "end_date": "2026-01-31", "amount_per_signal": 1000.0},
        models=[{"model_id": "model-1", "status": "active"}],
    )

    listed = repository.list_simulations()
    assert [item["simulation_id"] for item in listed] == [metadata["simulation_id"]]
    loaded_metadata, loaded = repository.load(metadata["simulation_id"])
    assert loaded_metadata["models"][0]["model_id"] == "model-1"
    assert loaded.model_snapshots == (
        {"model_id": "model-1", "status": "active"},
    )
    assert loaded.metrics == _result().metrics
    pd.testing.assert_frame_equal(loaded.trades, _result().trades)

    repository.delete(metadata["simulation_id"])
    assert repository.list_simulations() == []


def test_simulation_repository_uses_project_root_isolation(tmp_path):
    first = SimulationRepository(tmp_path / "dev")
    second = SimulationRepository(tmp_path / "prod")
    first.save(_result(), parameters={}, models=[])
    assert second.list_simulations() == []
