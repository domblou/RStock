"""Pure, bounded presentation helpers for the Laboratory history page."""

from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Any, Callable, Mapping, Sequence

import pandas as pd


EXPERIMENT_JOB_TYPES = frozenset({
    "walk_forward", "xgboost_calibration", "threshold_calibration",
})
PRODUCTION_JOB_TYPES = frozenset({
    "production_training", "market_update", "daily_prediction",
    "daily_screening", "realized_validation", "operational_run",
})
JOB_LABELS = {
    "walk_forward": "Walk-forward",
    "xgboost_calibration": "Calibration XGBoost",
    "threshold_calibration": "Calibration des seuils",
    "production_training": "Entraînement production",
    "market_update": "Mise à jour marché",
    "daily_prediction": "Prédictions quotidiennes",
    "daily_screening": "Screening",
    "realized_validation": "Résultats réalisés",
    "operational_run": "Exécution complète",
}
PERIOD_DAYS = {"Aujourd’hui": 0, "7 jours": 7, "30 jours": 30}


@dataclass(frozen=True, slots=True)
class HistoryRow:
    run_id: str
    date_time: str
    job_type: str
    context: str
    status: str
    duration: str
    summary: str

    def display(self) -> dict[str, str]:
        return {
            "Date / heure": self.date_time,
            "Type": JOB_LABELS.get(self.job_type, self.job_type),
            "Contexte": self.context,
            "Statut": self.status,
            "Durée": self.duration,
            "Résumé": self.summary,
        }


def _timestamp(value: object) -> pd.Timestamp | None:
    parsed = pd.to_datetime(value, errors="coerce", utc=True)
    return None if pd.isna(parsed) else pd.Timestamp(parsed)


def short_datetime(value: object) -> str:
    parsed = _timestamp(value)
    return "—" if parsed is None else parsed.strftime("%Y-%m-%d %H:%M")


def short_duration(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    seconds = max(0, int(float(value)))
    return f"{seconds // 3600:02d}:{seconds % 3600 // 60:02d}:{seconds % 60:02d}"


def filter_runs(
    runs: Sequence[Mapping[str, object]],
    *,
    allowed_types: frozenset[str],
    job_type: str = "Tous",
    status: str = "Tous",
    period: str = "Tout",
    model_id: str | None = None,
    detail_loader: Callable[[str], Mapping[str, object]] | None = None,
    now: datetime | None = None,
) -> list[Mapping[str, object]]:
    """Filter status records without making run identifiers a UI concern."""

    filtered = [
        run for run in runs
        if str(run.get("job_type")) in allowed_types
        and (job_type == "Tous" or str(run.get("job_type")) == job_type)
        and (status == "Tous" or str(run.get("status")) == status)
    ]
    if period in PERIOD_DAYS:
        # Runs are persisted in UTC, while the History page describes calendar
        # periods to the person using the application.  Use the local calendar
        # day, then convert each persisted UTC timestamp to that same timezone
        # before comparing it.  This notably keeps late-evening local runs in
        # "Aujourd’hui" even when their UTC date is already tomorrow.
        reference = pd.Timestamp(now or datetime.now().astimezone())
        reference = (
            reference.tz_localize("UTC")
            if reference.tzinfo is None
            else reference.tz_convert("UTC")
        )
        start = reference.normalize() - pd.Timedelta(days=PERIOD_DAYS[period])
        filtered = [
            run for run in filtered
            if (created := _timestamp(run.get("created_at"))) is not None
            and created.tz_convert(reference.tz) >= start
        ]
    if model_id is not None:
        if detail_loader is None:
            raise ValueError("A detail loader is required for a model filter")
        filtered = [
            run for run in filtered
            if str(detail_loader(str(run["run_id"])).get("configuration", {}).get("model_id") or "")
            == model_id
        ]
    return filtered


def paginate_runs(
    runs: Sequence[Mapping[str, object]], *, page: int, page_size: int
) -> tuple[list[Mapping[str, object]], int]:
    if page_size not in {25, 50}:
        raise ValueError("page_size must be 25 or 50")
    total_pages = max(1, (len(runs) + page_size - 1) // page_size)
    current = min(max(0, page), total_pages - 1)
    start = current * page_size
    return list(runs[start:start + page_size]), total_pages


def _symbols(configuration: Mapping[str, object]) -> tuple[str, ...]:
    return tuple(str(item) for item in configuration.get("symbols", ()) if item)


def _model_context(model_id: object, models: Mapping[str, str]) -> str:
    return models.get(str(model_id), str(model_id) if model_id else "—")


def _summary_text(job_type: str, summary: Mapping[str, object]) -> str:
    if job_type == "market_update":
        return f"{len(summary.get('updated_symbols', ())) } symboles mis à jour"
    if job_type == "daily_prediction":
        return f"{int(summary.get('predictions', 0))} prédictions"
    if job_type == "daily_screening":
        categories = summary.get("categories", {})
        if isinstance(categories, Mapping):
            no_signal = int(categories.get("no_signal", 0))
            signals = int(categories.get("bullish_signal", 0))
            return f"{signals} signaux · {no_signal} sans signal"
    if job_type == "realized_validation":
        return f"{int(summary.get('realized_results', 0))} résultats réalisés"
    if job_type == "production_training":
        return "1 modèle entraîné" if summary.get("model_id") else "Entraînement terminé"
    if job_type == "operational_run":
        return (
            f"{int(summary.get('predictions', 0))} prédictions · "
            f"{int(summary.get('signals', 0))} signaux"
        )
    if job_type == "walk_forward":
        return f"{int(summary.get('eligible_combinations', 0))} combinaisons qualifiées"
    if job_type.startswith("xgboost_") or job_type == "threshold_calibration":
        return "Calibration terminée"
    return "—"


def _context_text(
    job_type: str,
    configuration: Mapping[str, object],
    summary: Mapping[str, object],
    models: Mapping[str, str],
) -> str:
    model_id = configuration.get("model_id") or summary.get("model_id")
    if job_type == "production_training":
        return _model_context(model_id, models)
    if job_type == "market_update":
        return f"{len(summary.get('requested_symbols', _symbols(configuration)))} symboles"
    if job_type in {"daily_prediction", "daily_screening"}:
        return f"{int(summary.get('predictions', 0))} modèles actifs"
    if job_type == "operational_run":
        return f"{int(summary.get('updated_symbols', 0))} symboles opérationnels"
    symbols = _symbols(configuration)
    if job_type == "walk_forward":
        return f"{len(symbols)} symboles · profondeur {configuration.get('rstock_config', {}).get('permutation_depth', '—')}"
    return f"{len(symbols)} symboles" if symbols else "—"


def history_row(
    status: Mapping[str, object],
    detail: Mapping[str, object],
    models: Mapping[str, str],
) -> HistoryRow:
    configuration = detail.get("configuration", {})
    summary = detail.get("summary", {})
    job_type = str(status["job_type"])
    return HistoryRow(
        run_id=str(status["run_id"]),
        date_time=short_datetime(status.get("created_at")),
        job_type=job_type,
        context=_context_text(job_type, configuration, summary, models),
        status=str(status["status"]),
        duration=short_duration(status.get("duration_seconds")),
        summary=_summary_text(job_type, summary),
    )


def qualified_combinations_table(
    qualification: pd.DataFrame, holdout: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """Return sortable qualified combinations with development and holdout data."""

    if qualification.empty or "Eligible" not in qualification:
        return pd.DataFrame()
    eligible = qualification[
        qualification["Eligible"].map(
            lambda value: value is True or str(value).strip().lower() in {"true", "1", "yes"}
        )
    ].copy()
    if eligible.empty:
        return pd.DataFrame()
    final = holdout if holdout is not None else pd.DataFrame()
    if not final.empty and "Set" in final:
        final_columns = [name for name in ("Set", "FinalUpROCAUC") if name in final]
        eligible = eligible.merge(final[final_columns], on="Set", how="left")
    result = pd.DataFrame({
        "Combinaison": eligible["Set"].astype(str),
        "Cible": eligible.get("Observation", pd.Series(index=eligible.index, dtype=str)),
        "Predictors": eligible.get("Predictors", pd.Series(index=eligible.index, dtype=str)).map(_predictors_text),
        "AUC dev médiane": eligible.get("ROCAUCMedian", pd.Series(index=eligible.index)),
        "AUC holdout": eligible.get("FinalUpROCAUC", pd.Series(index=eligible.index)),
        "Stabilité / qualification": eligible.get("IneligibilityReasons", pd.Series(index=eligible.index)).map(
            lambda value: "Qualifiée" if not value or str(value) in {"[]", "nan"} else str(value)
        ),
    })
    return result.sort_values(["AUC dev médiane", "Combinaison"], ascending=[False, True], kind="stable")


def _predictors_text(value: object) -> str:
    try:
        parsed = json.loads(str(value))
    except (json.JSONDecodeError, TypeError):
        return str(value)
    return " + ".join(str(item) for item in parsed)


def already_promoted(
    *, walk_forward_run: str, set_name: str, models: Sequence[object]
) -> object | None:
    """Find an existing candidate without changing registry promotion semantics."""

    try:
        target, predictors = set_name.split("<-", maxsplit=1)
        predictor_values = tuple(part.strip() for part in predictors.split("+"))
    except ValueError:
        return None
    for model in models:
        if (
            getattr(model, "source_walk_forward_run", None) == walk_forward_run
            and getattr(model, "target", None) == target.strip()
            and tuple(getattr(model, "predictors", ())) == predictor_values
        ):
            return model
    return None
