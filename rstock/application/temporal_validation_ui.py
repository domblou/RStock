"""Presentation-only tables for temporal-validation artifacts."""

from __future__ import annotations

import json
from collections.abc import Mapping
from numbers import Integral
from pathlib import Path

import pandas as pd

from .auto_promotion import _promotion_guidance


_ARROW_INT64_MIN = -(2**63)
_ARROW_INT64_MAX = 2**63 - 1


def _arrow_safe_value(value: object) -> object:
    """Keep presentation values representable by Arrow without losing digits."""

    if isinstance(value, bool) or value is None:
        return value
    if isinstance(value, Integral):
        integer = int(value)
        if integer < _ARROW_INT64_MIN or integer > _ARROW_INT64_MAX:
            return str(integer)
        return integer
    if isinstance(value, Mapping):
        return {str(key): _arrow_safe_value(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_arrow_safe_value(item) for item in value]
    return value


def temporal_validation_gate_table(gates: object) -> pd.DataFrame:
    """Return an Arrow-safe presentation table for persisted gate results."""

    if not isinstance(gates, Mapping):
        return pd.DataFrame()
    rows = [
        {
            "Gate": key,
            "Status": value.get("status"),
            "Reason": value.get("reason"),
            "Metrics": _arrow_safe_value(value.get("metrics")),
        }
        for key, value in gates.items()
        if isinstance(value, Mapping)
    ]
    return pd.DataFrame(rows)


def _predictors_text(value: object) -> str:
    if isinstance(value, (list, tuple)):
        return " + ".join(str(item) for item in value)
    return str(value) if value is not None else "—"


def validation_promotion_lookup(results: Path) -> dict[tuple[str, str], dict[str, object]]:
    """Read the existing validation promotion guidance once for display only."""

    selected_path = results / "selected_thresholds_by_set.json"
    if not selected_path.is_file():
        return {}
    try:
        selected = json.loads(selected_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {}
    if not isinstance(selected, dict):
        return {}
    try:
        snapshot = json.loads((results.parent / "config.json").read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        snapshot = {}
    rstock_config = snapshot.get("rstock_config", {}) if isinstance(snapshot, dict) else {}
    try:
        guidance = _promotion_guidance(
            results,
            selected,
            promotion_config=(rstock_config if isinstance(rstock_config, dict) else None),
        )
    except ValueError:
        return {}
    lookup: dict[tuple[str, str], dict[str, object]] = {}
    for _, row in guidance.iterrows():
        key = (str(row.get("Combinaison", "")), str(row.get("Direction", "Up")))
        lookup[key] = {
            "holdout_precision": row.get("Précision holdout"),
            "holdout_auc": row.get("AUC holdout"),
            "directional_return_mean": row.get("Rendement directionnel moyen"),
            "holdout_signal_count": row.get("Signaux holdout"),
            "opposite_move_frequency": row.get("Fréquence mouvement opposé"),
            "promotion_reason": row.get("Raison"),
            "promotion_status": row.get("Statut promotion"),
        }
    return lookup


def _lost_display_value(value: object, *, percent: bool = False, integer: bool = False) -> str:
    if value is None or pd.isna(value):
        return "—"
    try:
        numeric = float(value)
    except (TypeError, ValueError):
        return "—"
    if integer:
        return str(int(numeric))
    if percent:
        return f"{numeric:.2%}"
    return str(value)


def lost_candidate_display_table(table: pd.DataFrame) -> pd.DataFrame:
    """Format only the lost-candidate presentation values, including missing data."""

    display = table.copy()
    for column in (
        "Précision réf.", "Précision val.", "AUC réf.", "AUC val.",
        "Rendement réf.", "Rendement val.", "Mouv. opposé val.",
    ):
        if column in display:
            display[column] = display[column].map(
                lambda value: _lost_display_value(value, percent=True)
            )
    for column in ("Signaux réf.", "Signaux val."):
        if column in display:
            display[column] = display[column].map(
                lambda value: _lost_display_value(value, integer=True)
            )
    if "Critère(s) échoué(s)" in display:
        display["Critère(s) échoué(s)"] = display["Critère(s) échoué(s)"].map(
            lambda value: "—"
            if value is None or pd.isna(value)
            else str(value).replace(" ; ", " · ")
        )
    for column in ("Dernière étape atteinte", "Raison d’élimination"):
        if column in display:
            display[column] = display[column].map(
                lambda value: "—" if value is None or pd.isna(value) else str(value)
            )
    return display


def candidate_identity_tables(
    stability: object,
    validation_lookup: Mapping[tuple[str, str], Mapping[str, object]] | None = None,
    trace_lookup: Mapping[tuple[str, str], Mapping[str, object]] | None = None,
) -> dict[str, pd.DataFrame]:
    """Build the three read-only candidate identity tables for Streamlit."""

    common_columns = [
        "Cible", "Predictors", "Direction", "Précision réf.", "Précision val.",
        "AUC réf.", "AUC val.", "Rendement réf.", "Rendement val.",
        "Signaux réf.", "Signaux val.",
    ]
    lost_columns = [
        "Cible", "Predictors", "Direction", "Précision réf.", "Précision val.",
        "AUC réf.", "AUC val.", "Rendement réf.", "Rendement val.",
        "Signaux réf.", "Signaux val.", "Mouv. opposé val.",
        "Critère(s) échoué(s)",
        "Dernière étape atteinte", "Raison d’élimination",
    ]
    new_columns = [
        "Cible", "Predictors", "Direction", "Précision val.",
        "AUC val.", "Rendement val.", "Signaux val.",
    ]
    if not isinstance(stability, Mapping):
        return {
            "common": pd.DataFrame(columns=common_columns),
            "lost": pd.DataFrame(columns=lost_columns),
            "new": pd.DataFrame(columns=new_columns),
        }

    common_rows = []
    for item in stability.get("common_candidates", ()):
        if not isinstance(item, Mapping):
            continue
        reference = item.get("reference", {})
        validation = item.get("validation", {})
        reference = reference if isinstance(reference, Mapping) else {}
        validation = validation if isinstance(validation, Mapping) else {}
        common_rows.append({
            "Cible": item.get("target"),
            "Predictors": _predictors_text(item.get("predictors")),
            "Direction": item.get("direction"),
            "Précision réf.": reference.get("holdout_precision"),
            "Précision val.": validation.get("holdout_precision"),
            "AUC réf.": reference.get("holdout_auc"),
            "AUC val.": validation.get("holdout_auc"),
            "Rendement réf.": reference.get("directional_return_mean"),
            "Rendement val.": validation.get("directional_return_mean"),
            "Signaux réf.": reference.get("holdout_signal_count"),
            "Signaux val.": validation.get("holdout_signal_count"),
        })

    lost_rows = []
    lookup = validation_lookup or {}
    traces = trace_lookup or {}
    for item in stability.get("lost_candidates", ()):
        if not isinstance(item, Mapping):
            continue
        validation = lookup.get(
            (str(item.get("symbol_set_id", "")), str(item.get("direction", "Up"))),
            {},
        )
        validation = validation if isinstance(validation, Mapping) else {}
        trace = traces.get(
            (str(item.get("symbol_set_id", "")), str(item.get("direction", "Up"))),
            {},
        )
        trace = trace if isinstance(trace, Mapping) else {}
        lost_rows.append({
            "Cible": item.get("target"),
            "Predictors": _predictors_text(item.get("predictors")),
            "Direction": item.get("direction"),
            "Précision réf.": item.get("holdout_precision"),
            "Précision val.": validation.get("holdout_precision"),
            "AUC réf.": item.get("holdout_auc"),
            "AUC val.": validation.get("holdout_auc"),
            "Rendement réf.": item.get("directional_return_mean"),
            "Rendement val.": validation.get("directional_return_mean"),
            "Signaux réf.": item.get("holdout_signal_count"),
            "Signaux val.": validation.get("holdout_signal_count"),
            "Mouv. opposé val.": validation.get("opposite_move_frequency"),
            "Critère(s) échoué(s)": validation.get("promotion_reason"),
            "Dernière étape atteinte": trace.get("last_stage", "—"),
            "Raison d’élimination": trace.get("elimination_reason", "—"),
        })

    def side_rows(name: str, suffix: str) -> list[dict[str, object]]:
        rows = []
        for item in stability.get(name, ()):
            if not isinstance(item, Mapping):
                continue
            rows.append({
                "Cible": item.get("target"),
                "Predictors": _predictors_text(item.get("predictors")),
                "Direction": item.get("direction"),
                f"Précision {suffix}": item.get("holdout_precision"),
                f"AUC {suffix}": item.get("holdout_auc"),
                f"Rendement {suffix}": item.get("directional_return_mean"),
                f"Signaux {suffix}": item.get("holdout_signal_count"),
            })
        return rows

    return {
        "common": pd.DataFrame(common_rows, columns=common_columns),
        "lost": pd.DataFrame(lost_rows, columns=lost_columns),
        "new": pd.DataFrame(
            side_rows("new_candidates", "val."), columns=new_columns
        ),
    }


def forced_candidate_revalidation_table(
    stability: object,
    forced_lookup: Mapping[tuple[str, str], Mapping[str, object]],
    trace_lookup: Mapping[tuple[str, str], Mapping[str, object]],
    promoted_sets: set[str] | None = None,
) -> pd.DataFrame:
    """Build the compact reference-versus-forced-validation table."""

    if not isinstance(stability, Mapping):
        return pd.DataFrame()
    promoted = promoted_sets or set()
    rows: list[dict[str, object]] = []
    candidates = [
        item
        for name in ("common_candidates", "lost_candidates")
        for item in stability.get(name, ())
        if isinstance(item, Mapping)
    ]
    for item in candidates:
        reference = item.get("reference", item)
        reference = reference if isinstance(reference, Mapping) else {}
        key = (str(item.get("symbol_set_id", "")), str(item.get("direction", "Up")))
        forced = forced_lookup.get(key, {})
        trace = trace_lookup.get(key, {})
        rows.append({
            "Cible": item.get("target"), "Predictors": _predictors_text(item.get("predictors")),
            "Direction": item.get("direction"), "Statut": trace.get("status", "—"),
            "Dernière étape atteinte": trace.get("last_stage", "—"), "Raison": trace.get("reason", "—"),
            "AUC réf.": reference.get("holdout_auc"), "AUC revalidation": forced.get("holdout_auc"),
            "Précision réf.": reference.get("holdout_precision"), "Précision revalidation": forced.get("holdout_precision"),
            "Rendement réf.": reference.get("directional_return_mean"), "Rendement revalidation": forced.get("directional_return_mean"),
            "Signaux réf.": reference.get("holdout_signal_count"), "Signaux revalidation": forced.get("holdout_signal_count"),
            "Promotion": "Promu" if key[0] in promoted else "Non promu",
        })
    return pd.DataFrame(rows)
