"""Pure, bounded display transformations for Production quality UI."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Mapping

import numpy as np
import pandas as pd

from .production_quality import LIVE_PREDICTION_ORIGINS, MONITORING_NOTIONAL
from .production_quality_repository import ProductionQualityRepository
from .production_repository import ProductionRepository


HEALTH_LABELS = {
    "not_evaluated": "Données insuffisantes",
    "data_insufficient": "Données insuffisantes",
}
WINDOWS = (20, 63, 126)
GRID_TEXT_COLUMNS = (
    "model_id", "Cible", "Prédicteurs", "Statut", "Univers", "Source", "Santé",
)
GRID_DATE_COLUMNS = ("Promotion", "Dernier signal")
GRID_PERCENT_COLUMNS = ("Rendement moyen", "Trades gagnants")
GRID_CURRENCY_COLUMNS = ("P&L cumulé", "Drawdown")
MASTER_COLUMNS = (
    "model_id", "model_version", "target", "predictors", "status",
    "universe_id", "universe_name", "source_end_to_end_run_id",
    "predictor_prefilter_top_n", "promotion_date",
    "signal_count_20", "signal_count_63", "signal_count_126",
    "mean_return_20", "mean_return_63", "mean_return_126",
    "win_rate_20", "win_rate_63", "win_rate_126",
    "pnl_since_promotion", "max_drawdown_dollars",
    "max_drawdown_return_points", "last_signal_date", "last_evaluated_date",
    "baseline_status", "health_status", "quality_updated_at", "trend_63",
)
DERIVED_MASTER_COLUMNS = (
    "health_label", "predictors_search", "target_search", "trend_63_values",
    "quality_available", "quality_version_matches", "registry_payload",
)
QUALITY_COLUMNS = tuple(
    name for name in MASTER_COLUMNS
    if name not in {"model_id", "model_version", "target", "predictors", "status"}
)


def health_label(value: object) -> str:
    key = "not_evaluated" if value is None or pd.isna(value) else str(value)
    return HEALTH_LABELS.get(key, key.replace("_", " ").capitalize())


def _decode_list(value: object) -> list[Any]:
    if isinstance(value, list):
        return value
    if value is None or (not isinstance(value, (list, dict)) and pd.isna(value)):
        return []
    try:
        decoded = json.loads(str(value))
    except (TypeError, ValueError):
        return []
    return decoded if isinstance(decoded, list) else []


def _empty_models_master() -> pd.DataFrame:
    return pd.DataFrame(columns=(*MASTER_COLUMNS, *DERIVED_MASTER_COLUMNS))


def _versions_match(registry: pd.Series, quality: pd.Series) -> pd.Series:
    return registry.eq(quality) | (registry.isna() & quality.isna())


def load_models_master(project_root: Path) -> pd.DataFrame:
    """Build the registry catalogue and enrich it from one quality snapshot."""

    registry_rows = ProductionRepository(project_root).model_summaries()
    quality_repository = ProductionQualityRepository(project_root)
    quality = quality_repository.load_master_snapshot()
    snapshot_available = not quality.empty
    registry_ids = {str(item["model_id"]) for item in registry_rows}
    quality_ids = (
        set(quality["model_id"].dropna().astype(str))
        if "model_id" in quality else set()
    )
    orphan_count = len(quality_ids - registry_ids)
    if not registry_rows:
        result = _empty_models_master()
        result.attrs.update(
            quality_snapshot_available=snapshot_available,
            orphan_quality_count=orphan_count,
        )
        return result

    registry = pd.DataFrame(registry_rows).rename(
        columns={"artifact_version": "model_version"}
    )
    registry["predictors"] = registry["predictors"].map(
        lambda value: json.dumps(_decode_list(value))
    )
    if quality.empty or "model_id" not in quality:
        result = registry.copy()
        for column in QUALITY_COLUMNS:
            result[column] = None
        result["quality_available"] = False
        result["quality_version_matches"] = False
    else:
        quality = quality.reindex(columns=MASTER_COLUMNS).copy()
        quality["model_id"] = quality["model_id"].astype(str)
        quality = quality.drop_duplicates("model_id", keep="last")
        quality = quality.rename(
            columns={
                name: f"quality_{name}"
                for name in MASTER_COLUMNS if name != "model_id"
            }
        )
        result = registry.merge(
            quality, on="model_id", how="left", sort=False, indicator="_quality_merge"
        )
        has_quality = result["_quality_merge"].eq("both")
        version_matches = has_quality & _versions_match(
            result["model_version"], result["quality_model_version"]
        )
        for column in QUALITY_COLUMNS:
            result[column] = result[f"quality_{column}"].where(version_matches)
        result["quality_available"] = has_quality
        result["quality_version_matches"] = version_matches
        result = result.drop(columns="_quality_merge")
        result = result.drop(
            columns=[
                name for name in result
                if name.startswith("quality_") and name not in {
                    "quality_available", "quality_version_matches", "quality_updated_at"
                }
            ]
        )

    result["health_label"] = "Non calculé"
    matched = result["quality_version_matches"].fillna(False).astype(bool)
    result.loc[matched, "health_label"] = result.loc[
        matched, "health_status"
    ].map(health_label)
    mismatch = result["quality_available"].fillna(False).astype(bool) & ~matched
    result.loc[mismatch, "health_label"] = "Non calculé — version différente"
    result["predictors_search"] = result["predictors"].fillna("").astype(str).str.casefold()
    result["target_search"] = result["target"].fillna("").astype(str).str.casefold()
    result["trend_63_values"] = result["trend_63"].map(_decode_list)
    result = result.reindex(columns=(*MASTER_COLUMNS, *DERIVED_MASTER_COLUMNS))
    result.attrs.update(
        quality_snapshot_available=snapshot_available,
        orphan_quality_count=orphan_count,
    )
    return result


def filter_quality_models(
    frame: pd.DataFrame, *, statuses: Iterable[str] = (), universes: Iterable[str] = (),
    sources: Iterable[str] = (), health: Iterable[str] = (), query: str = "",
) -> pd.DataFrame:
    result = frame
    for column, values in (
        ("status", statuses), ("universe_name", universes),
        ("source_end_to_end_run_id", sources), ("health_label", health),
    ):
        selected = {str(item) for item in values}
        if selected:
            result = result[result[column].astype(str).isin(selected)]
    needle = str(query).strip().casefold()
    if needle:
        result = result[
            result["target_search"].str.contains(needle, regex=False)
            | result["predictors_search"].str.contains(needle, regex=False)
        ]
    return result.copy()


def global_quality_kpis(frame: pd.DataFrame, *, window: int = 63) -> dict[str, Any]:
    if window not in WINDOWS:
        raise ValueError("Quality window must be 20, 63 or 126 sessions")
    signals = pd.to_numeric(frame.get(f"signal_count_{window}"), errors="coerce").fillna(0)
    means = pd.to_numeric(frame.get(f"mean_return_{window}"), errors="coerce")
    wins = pd.to_numeric(frame.get(f"win_rate_{window}"), errors="coerce")
    signal_total = float(signals.sum())
    def weighted(values: pd.Series) -> float | None:
        valid = values.notna() & signals.gt(0)
        denominator = float(signals[valid].sum())
        return None if denominator == 0 else float((values[valid] * signals[valid]).sum() / denominator)
    return {
        "active_models": int(frame.get("status", pd.Series(dtype=str)).astype(str).eq("active").sum()),
        "data_insufficient": int(frame.get("health_label", pd.Series(dtype=str)).isin({
            "Données insuffisantes", "Non calculé", "Non calculé — version différente",
        }).sum()),
        "mean_return": weighted(means),
        "pnl": float(pd.to_numeric(frame.get("pnl_since_promotion"), errors="coerce").fillna(0).sum()),
        "win_rate": weighted(wins),
        "signals": int(signal_total),
    }


def _display_integer_text(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    if isinstance(value, str) and value.strip() == "—":
        return "—"
    try:
        number = float(value)
    except (TypeError, ValueError):
        return str(value)
    return str(int(number)) if np.isfinite(number) and number.is_integer() else str(value)


def _display_text(value: object) -> str:
    return "—" if value is None or pd.isna(value) else str(value)


def _display_date(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    parsed = pd.to_datetime(value, errors="coerce", utc=True)
    return str(value) if pd.isna(parsed) else parsed.strftime("%Y-%m-%d")


def _display_percent(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    try:
        return f"{float(value) * 100:.2f}".replace(".", ",") + " %"
    except (TypeError, ValueError):
        return "—"


def _display_currency(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    try:
        return f"{float(value):,.2f}".replace(",", " ").replace(".", ",") + " $"
    except (TypeError, ValueError):
        return "—"


def normalize_models_grid(frame: pd.DataFrame) -> pd.DataFrame:
    """Return an Arrow-safe, compact display frame without changing persisted values."""

    result = frame.copy()
    for column in GRID_TEXT_COLUMNS:
        if column in result:
            result[column] = result[column].map(_display_text).astype("string")
    if "Top-N" in result:
        result["Top-N"] = result["Top-N"].map(_display_integer_text).astype("string")
    if "Signaux" in result:
        result["Signaux"] = result["Signaux"].map(_display_integer_text).astype("string")
    for column in GRID_DATE_COLUMNS:
        if column in result:
            result[column] = result[column].map(_display_date).astype("string")
    for column in GRID_PERCENT_COLUMNS:
        if column in result:
            result[column] = result[column].map(_display_percent).astype("string")
    for column in GRID_CURRENCY_COLUMNS:
        if column in result:
            result[column] = result[column].map(_display_currency).astype("string")
    if "Tendance 63" in result:
        result["Tendance 63"] = result["Tendance 63"].map(_decode_list)
    return result


def models_grid(frame: pd.DataFrame, *, window: int) -> pd.DataFrame:
    if window not in WINDOWS:
        raise ValueError("Quality window must be 20, 63 or 126 sessions")
    grid = pd.DataFrame({
        "model_id": frame["model_id"], "Cible": frame["target"],
        "Prédicteurs": frame["predictors"].map(lambda value: ", ".join(str(item) for item in _decode_list(value))),
        "Statut": frame["status"], "Univers": frame["universe_name"].fillna("—"),
        "Source": frame["source_end_to_end_run_id"].fillna("—"),
        "Top-N": frame["predictor_prefilter_top_n"].where(
            frame["predictor_prefilter_top_n"].notna(), "—"
        ), "Promotion": frame["promotion_date"],
        "Signaux": frame[f"signal_count_{window}"], "Rendement moyen": frame[f"mean_return_{window}"],
        "Trades gagnants": frame[f"win_rate_{window}"], "P&L cumulé": frame["pnl_since_promotion"],
        "Drawdown": frame["max_drawdown_dollars"], "Santé": frame["health_label"],
        "Dernier signal": frame["last_signal_date"], "Tendance 63": frame["trend_63_values"],
    })
    return normalize_models_grid(grid)


@dataclass(frozen=True)
class ModelQualityDetail:
    snapshot: Mapping[str, Any] | None
    lineage: Mapping[str, Any] | None
    baseline: Mapping[str, Any] | None
    series: pd.DataFrame
    observations: pd.DataFrame
    quality_state: str = "current"


_UNSPECIFIED_VERSION = object()


def load_model_quality_detail(
    project_root: Path, model_id: str, *, model_version: object = _UNSPECIFIED_VERSION
) -> ModelQualityDetail:
    repository = ProductionQualityRepository(project_root)
    snapshot = repository.load_model_snapshot(model_id)
    if model_version is not _UNSPECIFIED_VERSION:
        if snapshot is None:
            return ModelQualityDetail(
                None, None, None, pd.DataFrame(), pd.DataFrame(), "missing"
            )
        snapshot_version = (snapshot.get("identity") or {}).get(
            "model_version", snapshot.get("model_version")
        )
        if not bool(_versions_match(
            pd.Series([model_version]), pd.Series([snapshot_version])
        ).iloc[0]):
            return ModelQualityDetail(
                None, None, None, pd.DataFrame(), pd.DataFrame(), "version_mismatch"
            )
    return ModelQualityDetail(
        snapshot=snapshot,
        lineage=repository.load_lineage(model_id),
        baseline=repository.load_baseline(model_id),
        series=repository.load_model_series(model_id),
        observations=repository.load_observations(model_id),
    )


def evaluated_bullish_signals(observations: pd.DataFrame, *, limit: int = 100) -> pd.DataFrame:
    if observations.empty:
        return pd.DataFrame()
    mask = (
        observations["prediction_origin"].astype(str).isin(LIVE_PREDICTION_ORIGINS)
        & observations["evaluation_status"].astype(str).eq("evaluated")
        & observations["is_bullish_signal"].fillna(False).astype(bool)
    )
    values = observations.loc[mask].sort_values("session_date", ascending=False).head(limit).copy()
    returns = pd.to_numeric(values["intraday_return"], errors="coerce")
    values["pnl"] = returns * MONITORING_NOTIONAL
    values["verdict"] = np.select(
        [returns.gt(0), returns.lt(0)], ["Gagnant", "Perdant"], default="Nul"
    )
    return values


def excluded_observations(observations: pd.DataFrame, *, limit: int = 100) -> pd.DataFrame:
    if observations.empty:
        return pd.DataFrame()
    return observations[
        observations["evaluation_status"].astype(str).eq("excluded")
    ].sort_values("session_date", ascending=False).head(limit).copy()


def baseline_comparison_rows(
    snapshot: Mapping[str, Any] | None, baseline: Mapping[str, Any] | None
) -> pd.DataFrame:
    if not snapshot or not baseline or baseline.get("availability_status") != "available":
        return pd.DataFrame()
    current = snapshot.get("since_promotion", {})
    initial = baseline.get("metrics", {})
    deltas = snapshot.get("baseline_comparison", {})
    definitions = (
        ("Rendement moyen", "mean_intraday_return", "mean_intraday_return", "delta_mean_return"),
        ("Rendement médian", "median_intraday_return", "median_intraday_return", "delta_median_return"),
        ("Taux de trades > 0", "win_rate_strict_gt_0", "win_rate", "delta_win_rate"),
        ("Fréquence des signaux", "signal_rate", "signal_rate", None),
        ("MFE moyenne", "mean_mfe", "mean_mfe", "delta_mean_mfe"),
        ("MAE moyenne", "mean_mae", "mean_mae", "delta_mean_mae"),
    )
    return pd.DataFrame([
        {"Métrique": label, "À la promotion": initial.get(base), "Actuel": current.get(now),
         "Écart": (deltas.get(delta) if delta else (None if initial.get(base) is None or current.get(now) is None else float(current[now]) - float(initial[base])))}
        for label, base, now, delta in definitions
    ])


def baseline_comparison_display_table(table: pd.DataFrame) -> pd.DataFrame:
    """Format already-computed baseline metrics for the model detail UI."""

    if table.empty:
        return table
    result = table.copy()
    for column in ("À la promotion", "Actuel", "Écart"):
        result[column] = result[column].map(_display_percent).astype("string")
    return result


def evaluated_bullish_signals_display_table(signals: pd.DataFrame) -> pd.DataFrame:
    """Select and format the existing signal view without recalculating it."""

    if signals.empty:
        return pd.DataFrame()
    result = pd.DataFrame({
        "Date": signals["session_date"].map(_display_date),
        "Cible": signals["target"].map(_display_text),
        "Prob. Up": signals["up_probability"].map(_display_percent),
        "Seuil Up": signals["up_threshold"].map(_display_percent),
        "Prob. Down": signals["down_probability"].map(_display_percent),
        "Seuil Down": signals["down_threshold"].map(_display_percent),
        "Rendement": signals["intraday_return"].map(_display_percent),
        "P&L": signals["pnl"].map(_display_currency),
        "MFE": signals["mfe"].map(_display_percent),
        "MAE": signals["mae"].map(_display_percent),
        "Verdict": signals["verdict"].map(_display_text),
    })
    return result.astype("string")


def excluded_observations_display_table(observations: pd.DataFrame) -> pd.DataFrame:
    """Format compact exclusion rows from the already-loaded model partition."""

    if observations.empty:
        return pd.DataFrame()
    return pd.DataFrame({
        "Date": observations["session_date"].map(_display_date),
        "Raison": observations["exclusion_reason"].map(_display_text),
        "Champs invalides": "—",
    }).astype("string")


def performance_windows_display_table(
    windows: Mapping[int, Mapping[str, Any]]
) -> pd.DataFrame:
    """Format the existing 20/63/126 metrics for a compact detail table."""

    return pd.DataFrame([
        {
            "Fenêtre": f"{window} séances",
            "Rendement moyen": _display_percent(values.get("mean_intraday_return")),
            "Trades gagnants": _display_percent(values.get("win_rate")),
            "Signaux": _display_integer_text(values.get("signal_count")),
        }
        for window, values in windows.items()
    ]).astype("string")
