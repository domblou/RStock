"""Presentation-only tables for temporal-validation artifacts."""

from __future__ import annotations

from collections.abc import Mapping
from numbers import Integral

import pandas as pd


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



def candidate_identity_tables(
    stability: object,
) -> dict[str, pd.DataFrame]:
    """Build the three read-only candidate identity tables for Streamlit."""

    common_columns = [
        "Cible",
        "Predictors",
        "Direction",
        "Précision réf.",
        "Précision val.",
        "AUC réf.",
        "AUC val.",
        "Rendement réf.",
        "Rendement val.",
        "Signaux réf.",
        "Signaux val.",
    ]
    lost_columns = [
        "Cible", "Predictors", "Direction", "Précision réf.",
        "AUC réf.", "Rendement réf.", "Signaux réf.",
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
        "lost": pd.DataFrame(
            side_rows("lost_candidates", "réf."), columns=lost_columns
        ),
        "new": pd.DataFrame(
            side_rows("new_candidates", "val."), columns=new_columns
        ),
    }
