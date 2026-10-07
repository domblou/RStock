"""DataFrame/Styler adapter for the shared grid. No domain or scientific logic."""
from collections import Counter
from collections.abc import Mapping
import hashlib
import inspect
import json
import re
from types import SimpleNamespace

import pandas as pd
import streamlit as st
from pandas.io.formats.style import Styler
from streamlit.runtime.scriptrunner import get_script_run_ctx

from .grid import _value, render_grid


WIDTHS = {"small": 100, "medium": 200, "large": 400}
SUPPORTED_TYPES = {"text", "number", "progress", "line_chart", "bar_chart", "checkbox", "date", "datetime", "time", "list", "link"}
SUPPORTED_STYLES = {"color", "background-color", "font-weight", "font-style", "text-decoration", "text-align"}


def column_options(configuration, columns):
    """Carry every column help verbatim, including config for hidden columns."""
    result = {}
    for index, name in enumerate(columns):
        config = configuration.get(name, configuration.get(index, {}))
        if config is None:
            result[str(name)] = {"hidden": True}
            continue
        if isinstance(config, str):
            config = {"label": config}
        if not isinstance(config, Mapping):
            raise TypeError(f"Unsupported grid column configuration: {name}")
        typed = config.get("type_config") or {}
        kind = typed.get("type", "text")
        if kind not in SUPPORTED_TYPES:
            raise ValueError(f"Grid renderer unavailable for column {name}: {kind}")
        width = config.get("width")
        result[str(name)] = {
            "label": config.get("label") or str(name), "help": config.get("help") or "",
            "width": WIDTHS.get(width, width), "min_width": WIDTHS.get(width, 100),
            "max_width": WIDTHS.get(width, 300), "align": config.get("alignment") or ("right" if kind in {"number", "progress"} else "left"),
            "type": kind, **typed,
            "pinned": bool(config.get("pinned")),
        }
    return result


def _format(value, pattern):
    if value is None:
        return "—"
    if not pattern or pattern == "plain":
        return str(value)
    if pattern == "percent":
        return f"{float(value):.2%}"
    if pattern in {"dollar", "euro", "yen", "localized", "compact", "scientific", "engineering"}:
        # The application currently uses printf and percent. Unsupported formats
        # fail explicitly rather than silently degrading scientific presentation.
        raise ValueError(f"Grid number format requires a renderer: {pattern}")
    percent = re.fullmatch(r"%([+ -]?)(?:\.(\d+))?%", pattern)
    if percent:
        return format(float(value), f"{percent[1]}.{percent[2] or '0'}%")
    try:
        return pattern % value
    except (ValueError, TypeError) as exc:
        raise ValueError(f"Grid format unsupported: {pattern}") from exc


def prepare_dataframe(data, *, column_config=None, column_order=None, hide_index=True, row_ids=None):
    styler = data if isinstance(data, Styler) else None
    frame = styler.data.copy(deep=False) if styler is not None else (data.copy(deep=False) if isinstance(data, pd.DataFrame) else pd.DataFrame(data))
    if not frame.columns.is_unique:
        raise ValueError("Grid columns must be unique")
    original_columns = list(frame.columns)
    options = column_options(column_config or {}, original_columns)
    for name in original_columns:
        config = (column_config or {}).get(name, {})
        if isinstance(config, Mapping) and not config.get("type_config"):
            if pd.api.types.is_bool_dtype(frame[name].dtype):
                options[str(name)]["type"] = "checkbox"
            elif pd.api.types.is_numeric_dtype(frame[name].dtype):
                options[str(name)]["type"] = "number"
                options[str(name)]["align"] = "right"
    styles = {}
    if styler is not None:
        styler._compute()  # Same pandas adapter API used by Streamlit.
    records, counts = [], Counter()
    unique_model_ids = "model_id" in frame and frame["model_id"].notna().all() and frame["model_id"].is_unique
    if row_ids is not None and len(row_ids) != len(frame):
        raise ValueError("row_ids must match the DataFrame population")
    for index, values in enumerate(frame.itertuples(index=False, name=None)):
        record = {str(name): _value(value) for name, value in zip(original_columns, values)}
        for name, value in list(record.items()):
            # pandas NA/NaT are missing display values, not the literal text '<NA>'.
            if value in ("<NA>", "NaT"):
                record[name] = None
        if row_ids is not None:
            identity = str(row_ids[index])
        elif unique_model_ids:
            identity = str(record["model_id"])
        else:
            identity = hashlib.sha256(json.dumps(record, ensure_ascii=False, sort_keys=True).encode()).hexdigest()
        occurrence = counts[identity]; counts[identity] += 1
        record["__grid_id"] = identity if row_ids is not None else f"{identity}:{occurrence}"
        for column_index, name in enumerate(original_columns):
            name = str(name); value = record[name]; option = options[name]
            display_key = f"__display_{column_index}"
            option["display_key"] = display_key
            if option.get("format") and option["type"] in {"number", "progress"}:
                record[display_key] = _format(value, option["format"])
            elif styler is not None:
                record[display_key] = "—" if value is None else str(styler._display_funcs[(index, column_index)](values[column_index]))
            else:
                record[display_key] = "—" if value is None else (json.dumps(value, ensure_ascii=False) if isinstance(value, (list, dict)) else str(value))
            if styler is not None and styler.ctx.get((index, column_index)):
                cell_style = dict(styler.ctx[(index, column_index)])
                unsupported = set(cell_style) - SUPPORTED_STYLES
                if unsupported:
                    raise ValueError(f"Grid styles require a renderer: {sorted(unsupported)}")
                styles.setdefault(record["__grid_id"], {})[name] = cell_style
        if hide_index is False:
            record["__index"] = _value(frame.index[index])
        records.append(record)
    columns = [str(name) for name in (column_order or original_columns) if not options[str(name)].get("hidden")]
    all_columns = [str(name) for name in original_columns]
    if hide_index is False:
        options["__index"] = {"label": frame.index.name or "Index", "pinned": True}
        columns.insert(0, "__index"); all_columns.insert(0, "__index")
    return records, columns, options, styles, all_columns


def dataframe(data=None, *, width="stretch", height=None, hide_index=None,
              column_order=None, column_config=None, key=None, on_select="ignore",
              selection_mode="single-row", row_ids=None, page_size=25, component=None):
    """Compatibility adapter preserving existing DataFrame callers and events.

    Stable row IDs can be supplied from domain metadata; otherwise a row content
    fingerprint prevents old selection positions from selecting unrelated records.
    No positional keys are used for selection.
    """
    if width not in {"stretch", "content", None} and not isinstance(width, int):
        raise ValueError(f"Unsupported grid width: {width}")
    records, columns, options, styles, all_columns = prepare_dataframe(
        data, column_config=column_config, column_order=column_order,
        hide_index=hide_index, row_ids=row_ids,
    )
    if key is None:
        caller = inspect.currentframe().f_back
        location = st._main._get_delta_path_str() if get_script_run_ctx(suppress_warning=True) else "bare"
        key = f"rstock-grid-{caller.f_code.co_name}-{caller.f_lineno}-{location}"
        del caller
    mode = "none" if on_select == "ignore" else {"single-row": "single", "multi-row": "multi"}.get(selection_mode)
    if mode is None:
        raise ValueError("Only single-row and multi-row selection are currently supported")
    inherited_selection = st.session_state.get(key, []) if get_script_run_ctx(suppress_warning=True) else []
    inherited_selection = inherited_selection if isinstance(inherited_selection, (list, tuple)) else []
    result = render_grid(records, columns=columns, key=key, row_id="__grid_id",
                         selection_mode=mode, column_options=options, page_size=page_size,
                         max_height=height or 520, all_columns=all_columns,
                         cell_styles=styles, component=component,
                         selected_ids=inherited_selection)
    return SimpleNamespace(selection=SimpleNamespace(rows=result["selection"]["rows"]))
