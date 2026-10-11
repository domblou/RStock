"""Shared read-only grid; presentation only, with stable row identities.

The historical frontend location is kept to preserve packaging and component identity.
Pass already formatted display values; raw values remain available for sorting.
"""
from collections.abc import Mapping
from datetime import date, datetime
from numbers import Integral, Real
import math
from pathlib import Path

import streamlit.components.v1 as components


_component = components.declare_component(
    "rstock_history_grid", path=str(Path(__file__).with_name("history_grid_frontend")),
)


def _value(value):
    if value is None:
        return None
    if isinstance(value, bool):
        return value
    if isinstance(value, (date, datetime)):
        return value.isoformat()
    if isinstance(value, Real) and not math.isfinite(value):
        return None
    if isinstance(value, Integral) and not isinstance(value, bool):
        return int(value)
    if isinstance(value, Real):
        return float(value)
    if hasattr(value, "tolist"):
        return _value(value.tolist())
    if isinstance(value, (str, int, float, bool)):
        return value
    if isinstance(value, Mapping):
        return {str(key): _value(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_value(item) for item in value]
    return str(value)


def render_grid(records, *, columns, key, row_id, selected_ids=(),
                selection_mode="multi", column_options=None, page_size=None,
                max_height=520, empty_message="Aucune donnée à afficher.", component=None,
                all_columns=None, toolbar=True, cell_styles=None, preserve_interactions=False,
                selection_sync=None):
    """Render the shared grid and return IDs plus original input row positions.

    column_options: per-column label, width/min_width/max_width, align, help,
    secondary_key and display_key (preformatted values). No arbitrary HTML.
    page_size=None delegates pagination to the caller, as in History.
    Selection is limited to the supplied population, retained across internal pages.
    """
    if selection_mode not in {"none", "single", "multi"}:
        raise ValueError("selection_mode must be none, single or multi")
    if page_size is not None and (isinstance(page_size, bool) or not isinstance(page_size, int) or page_size < 1):
        raise ValueError("page_size must be a positive integer")
    if max_height < 78:
        raise ValueError("max_height must be at least 78")
    records = list(records)
    ids = [str(row[row_id]) for row in records]
    if len(set(ids)) != len(ids):
        raise ValueError("Grid row IDs must be unique")
    if isinstance(selected_ids, str):
        selected_ids = [selected_ids]
    emit = component or _component
    selected = emit(
        rows=[_value(dict(row)) for row in records], columns=list(columns),
        row_id=row_id, selected_ids=[str(item) for item in selected_ids],
        selection_mode=selection_mode, column_options=column_options or {},
        page_size=page_size, max_height=max_height, empty_message=empty_message,
        all_columns=list(all_columns) if all_columns is not None else list(columns),
        toolbar=toolbar, cell_styles=cell_styles or {}, preserve_interactions=preserve_interactions,
        default=[], key=key,
        **({"selection_sync": selection_sync} if selection_sync is not None else {}),
    )
    # Controlled History selection has already consumed the widget event before
    # this render. Never reapply its (possibly obsolete-page) return value.
    if selection_sync is not None:
        selected = list(selected_ids)
    selected = set(selected) if isinstance(selected, list) and all(isinstance(item, str) for item in selected) else set()
    positions = [index for index, identity in enumerate(ids) if identity in selected]
    if selection_mode == "none":
        positions = []
    elif selection_mode == "single":
        positions = positions[:1]
    return {"selection": {"rows": positions}, "selected_ids": [ids[index] for index in positions]}
