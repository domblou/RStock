"""Presentation used exclusively by the main History grid."""
from collections.abc import Mapping
import json
from pathlib import Path

import pandas as pd
from .grid import _component, render_grid

from .history_ui import JOB_LABELS, _derived_changes_text, _history_universe_label, _prepared_market_last_date

COLUMNS = ("Run ID", "Date / heure", "Type", "Univers", "Cutoff", "Dérivé", "Statut", "Durée")


def _date(value):
    if not isinstance(value, str) or not value.strip():
        return None
    parsed = pd.to_datetime(value, errors="coerce")
    return None if pd.isna(parsed) else pd.Timestamp(parsed).date().isoformat()


def _grid_changes(row, config):
    changes = _derived_changes_text(row.job_type, config)
    structured = any(
        isinstance(config.get(field), Mapping) and "overrides" in config[field]
        for field in ("derivation", "prefilter_derivation", "walk_forward_derivation")
    )
    # Empty structured overrides are authoritative; never revive stale summary text.
    if not changes and not structured and "Modifications :" in row.summary:
        changes = row.summary.split("Modifications :", 1)[1].strip()
    return changes.replace("num_boost_round ", "rounds ") if changes else "—"


def _associated_lineage(run_id, job_type, config, metadata, related_details, runs_root):
    """Resolve labels from existing run records without changing their relationships."""
    associated = None
    derived = False
    for field, source_field in (
        ("derivation", "source_end_to_end_run_id"),
        ("prefilter_derivation", "source_run_id"),
        ("walk_forward_derivation", "source_run_id"),
    ):
        derivation = config.get(field)
        if isinstance(derivation, Mapping) and derivation.get(source_field):
            associated, derived = derivation[source_field], True
            break
    if not associated and job_type == "qualification_holdout_diagnostic":
        associated = config.get("source_walk_forward_run") or config.get("source_forced_candidate_validation_run")
    associated = associated or metadata.get("parent_run_id") or metadata.get("reference_run_id")
    if not associated and job_type == "walk_forward":
        associated = config.get("source_prefilter_run")
    for field in ("source_experiment_run", "source_walk_forward_run", "source_threshold_parameter_calibration_run",
                  "source_xgboost_calibration_run", "source_end_to_end_run", "source_forced_candidate_validation_run"):
        associated = associated or config.get(field)
    if not associated and metadata.get("root_run_id") != run_id:
        associated = metadata.get("root_run_id")
    if not isinstance(associated, str) or associated == run_id:
        return "Run racine"
    related = related_details.get(associated, {})
    related_config = related.get("configuration", {}) if isinstance(related, Mapping) else {}
    associated_type = related_config.get("job_type") if isinstance(related_config, Mapping) else None
    if not associated_type and Path(associated).name == associated and associated not in {".", ".."}:
        # A source may be outside the grid's type filter. Read only its small status/config record.
        for filename in ("status.json", "config.json"):
            try:
                values = json.loads((Path(runs_root) / associated / filename).read_text(encoding="utf-8"))
                associated_type = values.get("job_type") if isinstance(values, Mapping) else None
            except (OSError, ValueError, TypeError):
                continue
            if associated_type:
                break
    if not associated_type:
        return "Run racine"
    label = JOB_LABELS.get(str(associated_type), str(associated_type))
    return f"{'Dérivé de ' if derived else ''}{label} : {associated}"


def history_grid_row(row, detail, *, universe_labels, related_details, runs_root):
    config = detail.get("configuration", {})
    config = config if isinstance(config, Mapping) else {}
    cutoff = None
    if row.job_type == "walk_forward" and config.get("source_prefilter_run"):
        source = str(config["source_prefilter_run"])
        # Display only persisted information. Purged/missing legacy sources use the run snapshot.
        if Path(source).name == source and source not in {".", ".."}:
            try:
                contract = json.loads((Path(runs_root) / source / "results/prefilter_contract.json").read_text(encoding="utf-8"))
                cutoff = _date(contract.get("cutoff")) if isinstance(contract, Mapping) else None
            except (OSError, ValueError, TypeError):
                pass
    for field in ("resolved_market_session_cutoff", "historical_data_cutoff", "requested_historical_cutoff"):
        cutoff = cutoff or _date(config.get(field))
    summary = detail.get("summary", {})
    summary = summary if isinstance(summary, Mapping) else {}
    trace = summary.get("traceability", {})
    trace = trace if isinstance(trace, Mapping) else {}
    cutoff = cutoff or _date(summary.get("prepared_dataset_as_of")) or _date(trace.get("prepared_dataset_as_of"))
    cutoff = cutoff or _prepared_market_last_date(row.job_type, summary, related_details)
    metadata = detail.get("metadata", {})
    metadata = metadata if isinstance(metadata, Mapping) else {}
    universe = _history_universe_label(config, metadata, universe_labels, related_details)
    return {
        "Run ID": row.run_id, "Date / heure": row.date_time,
        "Type": row.display()["Type"], "Univers": universe or row.context,
        "Cutoff": cutoff or "—", "Dérivé": _grid_changes(row, config),
        "Statut": row.status, "Durée": row.duration,
        "_lineage": _associated_lineage(row.run_id, row.job_type, config, metadata, related_details, runs_root),
    }


def render_history_grid(records, *, key, selected_ids=()):
    """Return native-selection-shaped row indices, using stable IDs across sorting/pages."""
    result = render_grid(
        records, columns=COLUMNS, key=key, row_id="Run ID", selected_ids=selected_ids,
        column_options={
            "Run ID": {"secondary_key": "_lineage", "min_width": 250, "max_width": 300},
            "Univers": {"min_width": 170, "max_width": 250},
            "Dérivé": {"min_width": 140, "max_width": 230},
        }, component=_component,
    )
    return {"selection": result["selection"]}
