"""Fixed-notional simulation of persisted bullish production signals."""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date
from pathlib import Path
from typing import Callable

import numpy as np
import pandas as pd

from rstock.config import RStockConfig
from rstock.market_cache import market_data_service

from .production_repository import ProductionRepository
from .production_services import DailyPredictionService, ProductionSignalService


TRADE_COLUMNS = (
    "Date signal",
    "Date trade",
    "Symbole",
    "Modèle source",
    "P(Up)",
    "Seuil Up",
    "Prix achat",
    "Prix vente",
    "Rendement",
    "Montant investi",
    "Profit / perte",
    "Statut",
)
BENCHMARK_COLUMNS = (
    "Date",
    "SPY Rendement",
)


@dataclass(frozen=True, slots=True)
class SimulationMetrics:
    total_profit_loss: float
    winning_trade_rate: float
    calculated_trades: int
    average_return: float
    signals_found: int
    excluded_trades: int
    average_winning_return: float | None
    average_losing_return: float | None
    best_trade: float | None
    worst_trade: float | None
    missing_prices: int
    price_coverage: float


@dataclass(frozen=True, slots=True)
class SimulationResult:
    trades: pd.DataFrame
    metrics: SimulationMetrics
    cumulative_results: pd.DataFrame
    result_distribution: pd.DataFrame
    model_snapshots: tuple[dict[str, object], ...] = ()
    benchmark_results: pd.DataFrame = field(
        default_factory=lambda: pd.DataFrame(columns=BENCHMARK_COLUMNS)
    )


def summarize_simulation_trades(trades: pd.DataFrame) -> SimulationResult:
    """Recalculate display metrics and charts from a selected trade population."""

    return SimulationService._result(trades)


def benchmark_cumulative_for_trades(
    trades: pd.DataFrame, benchmark_results: pd.DataFrame
) -> pd.DataFrame:
    """Apply saved SPY returns to the daily notional of bullish signals."""

    if trades.empty or benchmark_results.empty:
        return pd.DataFrame(columns=["Date", "SPY résultat cumulé"])
    amounts = pd.to_numeric(trades.get("Montant investi"), errors="coerce")
    # SPY mirrors every bullish signal's intended exposure, even if the target
    # price is unavailable and that target trade cannot itself be scored.
    exposure = trades.loc[amounts.notna(), ["Date trade"]].copy()
    exposure["Montant investi"] = amounts.loc[exposure.index]
    if exposure.empty:
        return pd.DataFrame(columns=["Date", "SPY résultat cumulé"])
    exposure["Date"] = pd.to_datetime(exposure.pop("Date trade"), errors="coerce")
    exposure = exposure.dropna(subset=["Date"]).groupby("Date", as_index=False)["Montant investi"].sum()
    spy = benchmark_results.copy()
    spy["Date"] = pd.to_datetime(spy["Date"], errors="coerce")
    spy["SPY Rendement"] = pd.to_numeric(spy["SPY Rendement"], errors="coerce")
    merged = exposure.merge(spy, on="Date", how="left").dropna(subset=["SPY Rendement"])
    if merged.empty:
        return pd.DataFrame(columns=["Date", "SPY résultat cumulé"])
    merged["SPY résultat cumulé"] = (
        merged["Montant investi"] * merged["SPY Rendement"]
    ).cumsum()
    return merged[["Date", "SPY résultat cumulé"]]
def _date(value: date | str | pd.Timestamp, label: str) -> pd.Timestamp:
    try:
        parsed = pd.Timestamp(value)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{label} invalide") from error
    if pd.isna(parsed):
        raise ValueError(f"{label} invalide")
    if parsed.tzinfo is not None:
        parsed = parsed.tz_localize(None)
    return parsed.normalize()


def _number(value: object) -> float | None:
    parsed = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    return None if pd.isna(parsed) else float(parsed)


class SimulationService:
    """Simulate independent Open-to-Close trades from persisted active signals."""

    def __init__(
        self,
        repository: ProductionRepository,
        price_loader: Callable[[str], pd.DataFrame | None],
        benchmark_price_loader: Callable[[str], pd.DataFrame | None] | None = None,
    ) -> None:
        self.repository = repository
        self.price_loader = price_loader
        self.benchmark_price_loader = benchmark_price_loader

    @classmethod
    def local(cls, project_root: Path, config: RStockConfig) -> "SimulationService":
        repository = ProductionRepository(project_root)
        price_loader = market_data_service(config).store.read
        return cls(repository, price_loader, benchmark_price_loader=price_loader)

    def run(
        self,
        start_date: date | str | pd.Timestamp,
        end_date: date | str | pd.Timestamp,
        amount_per_signal: float = 10_000.0,
    ) -> SimulationResult:
        start = _date(start_date, "Date de début")
        end = _date(end_date, "Date de fin")
        self._validate_parameters(start, end, amount_per_signal)
        # Evaluated predictions are realized production history. Persisted facts
        # remain valid after their originating model becomes inactive or retired.
        signals = self.repository.read_table("signals")
        evaluated_predictions = self.repository.read_table("realized_results")
        predictions = self.repository.read_table("predictions")
        evaluated_signals = self._evaluated_signal_rows(
            evaluated_predictions, signals
        )
        return self._with_spy_benchmark(self._simulate_signals(
            evaluated_signals, predictions, start, end, amount_per_signal
        ))

    @staticmethod
    def _evaluated_signal_rows(
        evaluated_predictions: pd.DataFrame, signals: pd.DataFrame
    ) -> pd.DataFrame:
        """Keep evaluated predictions that have a persisted signal record.

        The financial mode must not turn an evaluated ``no_signal`` prediction
        into a trade.  The inner join also means a prediction is tradable only
        after both evaluation and signal detection have been persisted.
        """

        if (
            evaluated_predictions.empty
            or signals.empty
            or "prediction_id" not in evaluated_predictions
            or "prediction_id" not in signals
        ):
            return pd.DataFrame()
        signal_rows = signals.drop_duplicates("prediction_id", keep="last")
        return evaluated_predictions.merge(
            signal_rows,
            on="prediction_id",
            how="inner",
            suffixes=("_evaluated", ""),
            validate="many_to_one",
        )

    def run_historical(
        self,
        start_date: date | str | pd.Timestamp,
        end_date: date | str | pd.Timestamp,
        config: RStockConfig,
        amount_per_signal: float = 10_000.0,
    ) -> SimulationResult:
        """Replay current active models, then use the common financial engine."""

        start = _date(start_date, "Date de début")
        end = _date(end_date, "Date de fin")
        self._validate_parameters(start, end, amount_per_signal)
        # Historical mode replays one frozen population: the models active when
        # this simulation starts, over the whole requested period.
        active_models = tuple(self.repository.active_models())
        predictions = DailyPredictionService(self.repository).replay(
            self.price_loader,
            config,
            start_date=start,
            end_date=end,
            models=active_models,
        )
        signals = ProductionSignalService(self.repository).screen(
            predictions, persist=False, restrict_to_active_models=False
        )
        return self._with_spy_benchmark(self._simulate_signals(
            signals,
            predictions,
            start,
            end,
            amount_per_signal,
            model_snapshots=tuple(model.to_dict() for model in active_models),
        ))

    def _with_spy_benchmark(self, result: SimulationResult) -> SimulationResult:
        """Freeze SPY returns used by this simulation when available in the cache."""

        if self.benchmark_price_loader is None:
            return result
        prices = self.benchmark_price_loader("SPY")
        if prices is None or prices.empty:
            return result
        spy = prices.copy()
        spy.index = pd.to_datetime(spy.index, errors="coerce").normalize()
        spy["Open"] = pd.to_numeric(spy.get("Open"), errors="coerce")
        spy["Close"] = pd.to_numeric(spy.get("Close"), errors="coerce")
        valid = spy[spy["Open"].ne(0) & spy["Open"].notna() & spy["Close"].notna()]
        benchmark = pd.DataFrame({
            "Date": valid.index,
            "SPY Rendement": valid["Close"].to_numpy() / valid["Open"].to_numpy() - 1.0,
        })
        return SimulationResult(
            result.trades, result.metrics, result.cumulative_results,
            result.result_distribution, result.model_snapshots, benchmark,
        )

    @staticmethod
    def _validate_parameters(
        start: pd.Timestamp, end: pd.Timestamp, amount_per_signal: float
    ) -> None:
        if start > end:
            raise ValueError("La date de début doit précéder ou égaler la date de fin")
        if not np.isfinite(amount_per_signal) or amount_per_signal <= 0:
            raise ValueError("Le montant par signal doit être positif")

    def _simulate_signals(
        self,
        signals: pd.DataFrame,
        predictions: pd.DataFrame,
        start: pd.Timestamp,
        end: pd.Timestamp,
        amount_per_signal: float,
        *,
        model_snapshots: tuple[dict[str, object], ...] = (),
    ) -> SimulationResult:
        """Convert persisted or replayed signals through one financial engine."""

        if signals.empty or "prediction_date" not in signals:
            return self._result(
                pd.DataFrame(columns=TRADE_COLUMNS),
                model_snapshots=model_snapshots,
            )
        work = signals.copy()
        trade_dates = pd.to_datetime(work["prediction_date"], errors="coerce").dt.normalize()
        work = work[
            work.get("category", pd.Series(index=work.index, dtype=object)).eq(
                "bullish_signal"
            )
            & trade_dates.between(start, end, inclusive="both")
        ].copy()
        work["_trade_date"] = trade_dates.loc[work.index]
        prediction_lookup = (
            predictions.drop_duplicates("prediction_id", keep="last").set_index("prediction_id")
            if not predictions.empty and "prediction_id" in predictions
            else pd.DataFrame()
        )
        price_cache: dict[str, pd.DataFrame | None] = {}
        rows: list[dict[str, object]] = []
        for signal in work.to_dict("records"):
            symbol = str(signal.get("target", ""))
            trade_date = pd.Timestamp(signal["_trade_date"])
            prediction_id = str(signal.get("prediction_id", ""))
            prediction = (
                prediction_lookup.loc[prediction_id]
                if not prediction_lookup.empty and prediction_id in prediction_lookup.index
                else None
            )
            if isinstance(prediction, pd.DataFrame):
                prediction = prediction.iloc[-1]
            signal_date = signal.get("created_at")
            if prediction is not None:
                signal_date = prediction.get("as_of_date", signal_date)
            has_evaluated_prices = "open" in signal or "close" in signal
            opened = _number(signal.get("open")) if has_evaluated_prices else None
            closed = _number(signal.get("close")) if has_evaluated_prices else None
            if not has_evaluated_prices:
                if symbol not in price_cache:
                    price_cache[symbol] = self.price_loader(symbol)
                prices = price_cache[symbol]
                if prices is not None and not prices.empty:
                    normalized = prices.copy()
                    normalized.index = pd.to_datetime(normalized.index).normalize()
                    if trade_date in normalized.index:
                        price_row = normalized.loc[trade_date]
                        if isinstance(price_row, pd.DataFrame):
                            price_row = price_row.iloc[-1]
                        opened = _number(price_row.get("Open"))
                        closed = _number(price_row.get("Close"))
            if opened is None and closed is None:
                status = "Données de marché incomplètes"
            elif opened is None:
                status = "Prix d'achat manquant"
            elif closed is None:
                status = "Prix de vente manquant"
            elif opened == 0:
                status = "Données de marché incomplètes"
            else:
                trade_return = closed / opened - 1.0
                status = "Gagnant" if trade_return > 0 else "Perdant"
            valid = status in {"Gagnant", "Perdant"}
            trade_return = closed / opened - 1.0 if valid else None
            probability = signal.get("up_probability")
            threshold = signal.get("up_threshold")
            if prediction is not None:
                probability = signal.get("up_probability", prediction.get("up_probability"))
                threshold = signal.get("up_threshold", prediction.get("up_threshold"))
            rows.append({
                "Date signal": signal_date,
                "Date trade": trade_date.date().isoformat(),
                "Symbole": symbol,
                "Modèle source": signal.get("model_id"),
                "P(Up)": _number(probability),
                "Seuil Up": _number(threshold),
                "Prix achat": opened,
                "Prix vente": closed,
                "Rendement": trade_return,
                "Montant investi": float(amount_per_signal),
                "Profit / perte": (
                    None if trade_return is None else float(amount_per_signal * trade_return)
                ),
                "Statut": status,
            })
        return self._result(
            pd.DataFrame(rows, columns=TRADE_COLUMNS),
            model_snapshots=model_snapshots,
        )

    @staticmethod
    def _result(
        trades: pd.DataFrame,
        *,
        model_snapshots: tuple[dict[str, object], ...] = (),
    ) -> SimulationResult:
        returns = pd.to_numeric(trades.get("Rendement"), errors="coerce")
        valid = trades[returns.notna()].copy() if not trades.empty else trades.copy()
        valid_returns = pd.to_numeric(valid.get("Rendement"), errors="coerce")
        profit_loss = pd.to_numeric(valid.get("Profit / perte"), errors="coerce")
        wins = valid_returns[valid_returns > 0]
        losses = valid_returns[valid_returns <= 0]
        found = len(trades)
        calculated = len(valid)

        def mean_or_none(values: pd.Series) -> float | None:
            return None if values.empty else float(values.mean())

        metrics = SimulationMetrics(
            total_profit_loss=float(profit_loss.sum()) if calculated else 0.0,
            winning_trade_rate=float((valid_returns > 0).mean()) if calculated else 0.0,
            calculated_trades=calculated,
            average_return=float(valid_returns.mean()) if calculated else 0.0,
            signals_found=found,
            excluded_trades=found - calculated,
            average_winning_return=mean_or_none(wins),
            average_losing_return=mean_or_none(losses),
            best_trade=None if not calculated else float(valid_returns.max()),
            worst_trade=None if not calculated else float(valid_returns.min()),
            missing_prices=found - calculated,
            price_coverage=(calculated / found) if found else 0.0,
        )
        if calculated:
            ordered = valid.assign(
                _date=pd.to_datetime(valid["Date trade"], errors="coerce")
            ).sort_values(["_date", "Modèle source", "Symbole"], kind="stable")
            cumulative = (
                ordered.groupby("_date", sort=True)["Profit / perte"].sum().cumsum()
                .rename("Résultat cumulé").reset_index().rename(columns={"_date": "Date"})
            )
        else:
            cumulative = pd.DataFrame(columns=["Date", "Résultat cumulé"])
        distribution = pd.DataFrame({
            "Résultat": ["Trades gagnants", "Trades perdants"],
            "Trades": [len(wins), len(losses)],
        })
        return SimulationResult(
            trades,
            metrics,
            cumulative,
            distribution,
            model_snapshots,
        )
