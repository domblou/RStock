"""Fixed-notional simulation of persisted bullish production signals."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Callable

import numpy as np
import pandas as pd

from rstock.config import RStockConfig
from rstock.market_cache import market_data_service

from .production_repository import ProductionRepository


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
    ) -> None:
        self.repository = repository
        self.price_loader = price_loader

    @classmethod
    def local(cls, project_root: Path, config: RStockConfig) -> "SimulationService":
        repository = ProductionRepository(project_root)
        return cls(repository, market_data_service(config).store.read)

    def run(
        self,
        start_date: date | str | pd.Timestamp,
        end_date: date | str | pd.Timestamp,
        amount_per_signal: float = 10_000.0,
    ) -> SimulationResult:
        start = _date(start_date, "Date de début")
        end = _date(end_date, "Date de fin")
        if start > end:
            raise ValueError("La date de début doit précéder ou égaler la date de fin")
        if not np.isfinite(amount_per_signal) or amount_per_signal <= 0:
            raise ValueError("Le montant par signal doit être positif")

        signals = self.repository.read_active_model_table("signals")
        if signals.empty or "prediction_date" not in signals:
            return self._result(pd.DataFrame(columns=TRADE_COLUMNS))
        work = signals.copy()
        trade_dates = pd.to_datetime(work["prediction_date"], errors="coerce").dt.normalize()
        work = work[
            work.get("category", pd.Series(index=work.index, dtype=object)).eq(
                "bullish_signal"
            )
            & trade_dates.between(start, end, inclusive="both")
        ].copy()
        work["_trade_date"] = trade_dates.loc[work.index]
        predictions = self.repository.read_table("predictions")
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
            if symbol not in price_cache:
                price_cache[symbol] = self.price_loader(symbol)
            prices = price_cache[symbol]
            opened = closed = None
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
        return self._result(pd.DataFrame(rows, columns=TRADE_COLUMNS))

    @staticmethod
    def _result(trades: pd.DataFrame) -> SimulationResult:
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
        return SimulationResult(trades, metrics, cumulative, distribution)
