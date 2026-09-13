"""Pure presentation helpers for operational results in Surveillance."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import Mapping

import pandas as pd


REALIZED_DISPLAY_COLUMNS = (
    "prediction_date",
    "target",
    "model_id",
    "model_version",
    "open",
    "high",
    "low",
    "close",
    "intraday_return",
    "MFE",
    "MAE",
    "up_target_hit",
    "down_target_hit",
    "realized_at",
    "prediction_id",
    "signal_id",
    "category",
    "predictors",
    "up_probability",
    "down_probability",
)

PREDICTION_MAIN_COLUMNS = (
    "Date", "Cible", "Predictors", "P(Up)", "P(Down)", "Signal",
)
SIGNAL_MAIN_COLUMNS = (
    "Date", "Cible", "Predictors", "Catégorie", "P(Up)", "P(Down)",
)
REALIZED_MAIN_COLUMNS = (
    "Date", "Cible", "Predictors", "Open", "High", "Low", "Close",
    "Rendement", "MFE", "MAE", "UpTarget", "DownTarget",
)

SIGNAL_LABELS = {
    "bullish_signal": "Signal haussier",
    "no_signal": "Sans signal",
    "error": "Erreur",
}


@dataclass(frozen=True, slots=True)
class OperationalTableView:
    table: pd.DataFrame
    technical: pd.DataFrame


@dataclass(frozen=True, slots=True)
class SignalResultsView:
    signals: OperationalTableView
    no_signal: OperationalTableView


@dataclass(frozen=True, slots=True)
class RealizedResultsView:
    table: pd.DataFrame
    pending: pd.DataFrame
    next_validation_date: str | None
    latest_market_date: str | None
    technical: pd.DataFrame = field(default_factory=pd.DataFrame)

    @property
    def pending_count(self) -> int:
        return len(self.pending)

    @property
    def next_pending_date(self) -> str | None:
        """Backward-compatible alias used by the first Surveillance V1."""

        return self.next_validation_date


def _merge_missing(
    destination: pd.DataFrame,
    source: pd.DataFrame,
    columns: tuple[str, ...],
) -> pd.DataFrame:
    if (
        destination.empty
        or source.empty
        or "prediction_id" not in destination
        or "prediction_id" not in source
    ):
        return destination
    missing = [name for name in columns if name not in destination and name in source]
    if not missing:
        return destination
    lookup = source[["prediction_id", *missing]].drop_duplicates(
        "prediction_id", keep="last"
    )
    return destination.merge(lookup, on="prediction_id", how="left", validate="many_to_one")


def _short_date(value: object) -> str:
    parsed = pd.to_datetime(value, errors="coerce")
    return "—" if pd.isna(parsed) else parsed.strftime("%Y-%m-%d")


def _short_datetime(value: object) -> str:
    parsed = pd.to_datetime(value, errors="coerce", utc=True)
    return "—" if pd.isna(parsed) else parsed.strftime("%Y-%m-%d %H:%M")


def _percentage(value: object) -> str:
    numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    return "—" if pd.isna(numeric) else f"{float(numeric):.2%}"


def _yes_no(value: object) -> str:
    if pd.isna(value):
        return "—"
    if isinstance(value, str):
        normalized = value.strip().lower()
        return "Oui" if normalized in {"true", "1", "yes", "oui"} else "Non"
    return "Oui" if bool(value) else "Non"


def _predictors(value: object) -> str:
    if isinstance(value, (list, tuple)):
        return " + ".join(str(item) for item in value)
    if isinstance(value, str):
        try:
            parsed = json.loads(value)
        except (json.JSONDecodeError, TypeError):
            return value
        if isinstance(parsed, list):
            return " + ".join(str(item) for item in parsed)
    return "—" if pd.isna(value) else str(value)


def _column(frame: pd.DataFrame, name: str) -> pd.Series:
    if name in frame:
        return frame[name]
    return pd.Series(pd.NA, index=frame.index, dtype=object)


def _operational_table(
    frame: pd.DataFrame,
    *,
    category_column: str,
    columns: tuple[str, ...],
) -> OperationalTableView:
    technical = frame.copy().reset_index(drop=True)
    if technical.empty:
        return OperationalTableView(pd.DataFrame(columns=columns), technical)
    display = pd.DataFrame({
        "Date": _column(technical, "prediction_date").map(_short_date),
        "Cible": _column(technical, "target"),
        "Predictors": _column(technical, "predictors").map(_predictors),
        "P(Up)": _column(technical, "up_probability").map(_percentage),
        "P(Down)": _column(technical, "down_probability").map(_percentage),
        category_column: _column(
            technical,
            "signal_status" if category_column == "Signal" else "category",
        ).map(lambda value: SIGNAL_LABELS.get(str(value), str(value))),
    })
    display = display.loc[:, columns]
    return OperationalTableView(display.reset_index(drop=True), technical)


def build_predictions_view(predictions: pd.DataFrame, *, limit: int = 50) -> OperationalTableView:
    """Build the concise prediction grid and its aligned technical records."""

    source = predictions.tail(limit).iloc[::-1].reset_index(drop=True)
    return _operational_table(
        source, category_column="Signal", columns=PREDICTION_MAIN_COLUMNS
    )


def build_signals_view(signals: pd.DataFrame, *, limit: int = 50) -> SignalResultsView:
    """Separate actionable signals from folded-away no-signal observations."""

    if signals.empty or "category" not in signals:
        empty = _operational_table(
            pd.DataFrame(), category_column="Catégorie", columns=SIGNAL_MAIN_COLUMNS
        )
        return SignalResultsView(empty, empty)
    recent = signals.tail(limit).iloc[::-1].reset_index(drop=True)
    actual = recent[recent["category"] == "bullish_signal"].reset_index(drop=True)
    no_signal = recent[recent["category"] == "no_signal"].reset_index(drop=True)
    return SignalResultsView(
        _operational_table(actual, category_column="Catégorie", columns=SIGNAL_MAIN_COLUMNS),
        _operational_table(no_signal, category_column="Catégorie", columns=SIGNAL_MAIN_COLUMNS),
    )


def realized_main_table(table: pd.DataFrame) -> pd.DataFrame:
    """Project the formatted realized history onto its user-facing columns."""

    aliases = {
        "prediction_date": "Date", "target": "Cible", "predictors": "Predictors",
        "open": "Open", "high": "High", "low": "Low", "close": "Close",
        "intraday_return": "Rendement", "up_target_hit": "UpTarget",
        "down_target_hit": "DownTarget",
    }
    projected = table.rename(columns=aliases)
    for name in REALIZED_MAIN_COLUMNS:
        if name not in projected:
            projected[name] = pd.NA
    projected["Predictors"] = projected["Predictors"].map(_predictors)
    return projected.loc[:, REALIZED_MAIN_COLUMNS].reset_index(drop=True)


def _pending_predictions(
    predictions: pd.DataFrame,
    realized: pd.DataFrame,
) -> pd.DataFrame:
    if predictions.empty:
        return pd.DataFrame()
    required_prediction = {"prediction_id", "prediction_date", "status"}
    if not required_prediction <= set(predictions):
        return pd.DataFrame()
    realized_ids = (
        set(realized["prediction_id"].astype(str))
        if not realized.empty and "prediction_id" in realized
        else set()
    )
    pending = predictions[
        ~predictions["prediction_id"].astype(str).isin(realized_ids)
        & (predictions["status"] == "predicted")
    ].drop_duplicates("prediction_id", keep="first").copy()
    if pending.empty:
        return pending
    pending["prediction_date"] = pending["prediction_date"].map(_short_date)
    return pending.sort_values(["prediction_date", "target"], kind="stable")


def build_realized_results_view(
    predictions: pd.DataFrame,
    signals: pd.DataFrame,
    realized: pd.DataFrame,
    freshness: Mapping[str, str | None] | None = None,
) -> RealizedResultsView:
    """Join existing histories and format them without duplicating persistence."""

    joined = realized.copy()
    joined = _merge_missing(
        joined,
        predictions,
        ("model_version", "predictors", "up_probability", "down_probability"),
    )
    joined = _merge_missing(joined, signals, ("signal_id", "category"))
    aliases = {
        "mfe": "MFE",
        "mae": "MAE",
        "up_target": "up_target_hit",
        "down_target": "down_target_hit",
        "recorded_at": "realized_at",
    }
    joined = joined.rename(
        columns={old: new for old, new in aliases.items() if new not in joined}
    )
    for name in REALIZED_DISPLAY_COLUMNS:
        if name not in joined:
            joined[name] = pd.NA
    technical = joined.sort_values(
        "prediction_date", ascending=False, kind="stable"
    ).reset_index(drop=True) if not joined.empty else joined.copy()
    if not joined.empty:
        joined["prediction_date"] = joined["prediction_date"].map(_short_date)
        joined["realized_at"] = joined["realized_at"].map(_short_datetime)
        for name in ("intraday_return", "MFE", "MAE", "up_probability", "down_probability"):
            joined[name] = joined[name].map(_percentage)
        for name in ("up_target_hit", "down_target_hit"):
            joined[name] = joined[name].map(_yes_no)
        for name in ("open", "high", "low", "close"):
            joined[name] = pd.to_numeric(joined[name], errors="coerce").round(4)
        joined = joined.sort_values("prediction_date", ascending=False, kind="stable")
    table = joined.loc[:, REALIZED_DISPLAY_COLUMNS].reset_index(drop=True)

    pending = _pending_predictions(predictions, realized)
    next_date = None if pending.empty else str(pending["prediction_date"].min())
    available_dates = [
        _short_date(value)
        for value in (freshness or {}).values()
        if value is not None and _short_date(value) != "—"
    ]
    latest_market = max(available_dates) if available_dates else None
    return RealizedResultsView(table, pending, next_date, latest_market, technical)


def validation_feedback(
    new_results: int,
    view: RealizedResultsView,
) -> tuple[str, str]:
    """Return a Streamlit message level and explicit validation outcome."""

    if new_results > 0:
        if new_results == 1:
            return "success", "1 nouveau résultat réalisé ajouté."
        return "success", f"{new_results} nouveaux résultats réalisés ajoutés."
    if view.pending_count:
        prediction_word = "prédiction est" if view.pending_count == 1 else "prédictions sont"
        message = (
            "Aucun nouveau résultat disponible.\n\n"
            f"{view.pending_count} {prediction_word} encore en attente"
        )
        if view.next_validation_date:
            message += f" pour la séance du {view.next_validation_date}"
        message += "."
        if view.latest_market_date:
            message += (
                "\n\nDernières données disponibles : "
                f"{view.latest_market_date}."
            )
        return "info", message
    return "info", "Aucune prédiction en attente de validation."
