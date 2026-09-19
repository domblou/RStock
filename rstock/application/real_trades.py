"""User-entered real trades, kept separate from scientific prediction history."""

from __future__ import annotations

from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from typing import Any, Mapping
from uuid import uuid4

import pandas as pd

from .production_repository import ProductionRepository


REAL_TRADE_SCHEMA_VERSION = 1


def _timestamp() -> str:
    return datetime.now(timezone.utc).isoformat()


def _text(value: object) -> str:
    return str(value or "").strip()


def _positive(value: object, label: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{label} doit être un nombre positif.") from error
    if number <= 0:
        raise ValueError(f"{label} doit être supérieur à zéro.")
    return number


def _positive_integer(value: object, label: str) -> int:
    number = _positive(value, label)
    if not number.is_integer():
        raise ValueError(f"{label} doit être un entier positif.")
    return int(number)


@dataclass(frozen=True, slots=True)
class RealTrade:
    trade_id: str
    prediction_id: str
    model_id: str
    model_version: str | None
    signal_date: str
    target: str
    direction: str
    entry_price: float
    exit_price: float
    quantity: float
    note: str | None
    created_at: str
    updated_at: str
    schema_version: int = REAL_TRADE_SCHEMA_VERSION

    @property
    def real_return(self) -> float | None:
        # Production signals are currently Up. Preserve a direction field for a
        # later extension without inventing a short-side business convention.
        return self.exit_price / self.entry_price - 1.0 if self.direction == "Up" else None

    @property
    def gross_pnl(self) -> float | None:
        return (self.exit_price - self.entry_price) * self.quantity if self.direction == "Up" else None

    def to_dict(self) -> dict[str, object]:
        return asdict(self)

    @classmethod
    def from_dict(cls, values: Mapping[str, object]) -> "RealTrade":
        if int(values.get("schema_version", REAL_TRADE_SCHEMA_VERSION)) != REAL_TRADE_SCHEMA_VERSION:
            raise ValueError("Version de transaction réelle incompatible")
        return cls(
            trade_id=_text(values.get("trade_id")),
            prediction_id=_text(values.get("prediction_id")),
            model_id=_text(values.get("model_id")),
            model_version=(None if values.get("model_version") is None else _text(values.get("model_version"))),
            signal_date=_text(values.get("signal_date")),
            target=_text(values.get("target")),
            direction=_text(values.get("direction")),
            entry_price=_positive(values.get("entry_price"), "Prix d’achat"),
            exit_price=_positive(values.get("exit_price"), "Prix de vente"),
            quantity=_positive(values.get("quantity"), "Quantité"),
            note=None if values.get("note") in {None, ""} else _text(values.get("note")),
            created_at=_text(values.get("created_at")),
            updated_at=_text(values.get("updated_at")),
            schema_version=int(values.get("schema_version", REAL_TRADE_SCHEMA_VERSION)),
        )


class RealTradeService:
    """Idempotent CRUD service for user-entered Up trades."""

    def __init__(self, project_root) -> None:
        self.repository = ProductionRepository(project_root)

    def trades(self) -> list[RealTrade]:
        return [RealTrade.from_dict(values) for values in self.repository.read_real_trades()]

    def for_prediction(self, prediction_id: object) -> RealTrade | None:
        identifier = _text(prediction_id)
        return next((item for item in self.trades() if item.prediction_id == identifier), None)

    @staticmethod
    def _source(prediction: Mapping[str, object]) -> dict[str, str | None]:
        prediction_id = _text(prediction.get("prediction_id"))
        model_id = _text(prediction.get("model_id"))
        target = _text(prediction.get("target"))
        signal_date = _text(prediction.get("prediction_date"))
        if not prediction_id or not model_id or not target or not signal_date:
            raise ValueError("La prédiction sélectionnée ne possède pas une identité historique complète.")
        category = _text(prediction.get("category") or prediction.get("signal_status"))
        if category and category != "bullish_signal":
            raise ValueError("Seuls les signaux haussiers peuvent être enregistrés dans cette V1.")
        version = _text(prediction.get("model_version")) or None
        return {
            "prediction_id": prediction_id,
            "model_id": model_id,
            "model_version": version,
            "signal_date": str(pd.Timestamp(signal_date).normalize().date()),
            "target": target,
            "direction": "Up",
        }

    def save_from_prediction(
        self,
        prediction: Mapping[str, object],
        *,
        entry_price: object,
        exit_price: object,
        quantity: object,
        note: object = None,
    ) -> RealTrade:
        source = self._source(prediction)
        return self._upsert(
            source,
            entry_price=entry_price,
            exit_price=exit_price,
            quantity=quantity,
            note=note,
        )

    def update(
        self,
        trade_id: str,
        *,
        entry_price: object,
        exit_price: object,
        quantity: object,
        note: object = None,
    ) -> RealTrade:
        existing = next((item for item in self.trades() if item.trade_id == trade_id), None)
        if existing is None:
            raise KeyError("Transaction réelle introuvable")
        return self._upsert(
            {
                "prediction_id": existing.prediction_id,
                "model_id": existing.model_id,
                "model_version": existing.model_version,
                "signal_date": existing.signal_date,
                "target": existing.target,
                "direction": existing.direction,
            },
            entry_price=entry_price,
            exit_price=exit_price,
            quantity=quantity,
            note=note,
        )

    def _upsert(self, source: Mapping[str, str | None], **values: object) -> RealTrade:
        entry = _positive(values["entry_price"], "Prix d’achat")
        exit_price = _positive(values["exit_price"], "Prix de vente")
        quantity = _positive_integer(values["quantity"], "Quantité")
        cleaned_note = _text(values.get("note")) or None
        saved: RealTrade | None = None

        def apply(current: list[dict[str, Any]]) -> list[dict[str, Any]]:
            nonlocal saved
            existing = next(
                (item for item in current if _text(item.get("prediction_id")) == source["prediction_id"]),
                None,
            )
            now = _timestamp()
            trade = RealTrade(
                trade_id=_text(existing.get("trade_id")) if existing else uuid4().hex,
                prediction_id=str(source["prediction_id"]),
                model_id=str(source["model_id"]),
                model_version=source.get("model_version"),
                signal_date=str(source["signal_date"]),
                target=str(source["target"]),
                direction=str(source["direction"]),
                entry_price=entry,
                exit_price=exit_price,
                quantity=quantity,
                note=cleaned_note,
                created_at=_text(existing.get("created_at")) if existing else now,
                updated_at=now,
            )
            saved = trade
            retained = [item for item in current if _text(item.get("prediction_id")) != trade.prediction_id]
            return [*retained, trade.to_dict()]

        self.repository.update_real_trades(apply)
        assert saved is not None
        return saved

    def delete(self, trade_id: str) -> bool:
        deleted = False

        def apply(current: list[dict[str, Any]]) -> list[dict[str, Any]]:
            nonlocal deleted
            retained = [item for item in current if _text(item.get("trade_id")) != trade_id]
            deleted = len(retained) != len(current)
            return retained

        self.repository.update_real_trades(apply)
        return deleted


def performance_table(
    trades: list[RealTrade], realized: pd.DataFrame | None = None
) -> pd.DataFrame:
    theoretical: dict[str, float] = {}
    if realized is not None and not realized.empty and {"prediction_id", "intraday_return"} <= set(realized):
        source = realized.copy()
        source["_theoretical"] = pd.to_numeric(source["intraday_return"], errors="coerce")
        theoretical = dict(
            source.dropna(subset=["_theoretical"]).drop_duplicates("prediction_id", keep="last")
            .set_index("prediction_id")["_theoretical"].astype(float)
        )
    rows: list[dict[str, object]] = []
    for trade in trades:
        actual = trade.real_return
        expected = theoretical.get(trade.prediction_id)
        rows.append({
            "transaction_id": trade.trade_id,
            "prediction_id": trade.prediction_id,
            "date": trade.signal_date,
            "target": trade.target,
            "model_id": trade.model_id,
            "model_version": trade.model_version,
            "direction": trade.direction,
            "entry_price": trade.entry_price,
            "exit_price": trade.exit_price,
            "quantity": trade.quantity,
            "real_return": actual,
            "gross_pnl": trade.gross_pnl,
            "theoretical_return": expected,
            "real_vs_theoretical": None if actual is None or expected is None else actual - expected,
            "note": trade.note,
        })
    return pd.DataFrame(rows)


def filter_performance(
    table: pd.DataFrame,
    *,
    start: object | None = None,
    end: object | None = None,
    target: str | None = None,
    model_id: str | None = None,
) -> pd.DataFrame:
    result = table.copy()
    if result.empty:
        return result
    dates = pd.to_datetime(result["date"], errors="coerce").dt.normalize()
    if start is not None:
        result = result.loc[dates >= pd.Timestamp(start).normalize()]
        dates = dates.loc[result.index]
    if end is not None:
        result = result.loc[dates <= pd.Timestamp(end).normalize()]
    if target and target != "Toutes":
        result = result[result["target"].astype(str) == target]
    if model_id and model_id != "Tous":
        result = result[result["model_id"].astype(str) == model_id]
    return result.sort_values(["date", "transaction_id"], ascending=[False, False], kind="stable")


def performance_kpis(table: pd.DataFrame) -> dict[str, float | int | None]:
    pnl = pd.to_numeric(table.get("gross_pnl", pd.Series(dtype=float)), errors="coerce").dropna()
    returns = pd.to_numeric(table.get("real_return", pd.Series(dtype=float)), errors="coerce").dropna()
    gains = pnl[pnl > 0]
    losses = pnl[pnl < 0]
    return {
        "transaction_count": int(len(table)),
        "total_pnl": float(pnl.sum()) if not pnl.empty else None,
        "mean_return": float(returns.mean()) if not returns.empty else None,
        "median_return": float(returns.median()) if not returns.empty else None,
        "win_rate": float((pnl > 0).mean()) if not pnl.empty else None,
        "average_gain": float(gains.mean()) if not gains.empty else None,
        "average_loss": float(losses.mean()) if not losses.empty else None,
        "profit_factor": (
            float(gains.sum() / abs(losses.sum())) if not gains.empty and not losses.empty else None
        ),
    }
