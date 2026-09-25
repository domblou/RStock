"""One-time, conservative repair for missing production-universe lineage names.

The universe identifier is already persisted with a promoted model.  A name is
backfilled only when the persisted universe registry record demonstrably
predates (or is contemporaneous with) the model promotion.  This avoids
presenting a subsequently renamed universe as its historical promotion name.
"""

from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Any

from .production_quality_repository import ProductionQualityRepository
from .production_repository import ProductionRepository
from .universes import UniverseService


def _blank(value: object) -> bool:
    return value is None or (isinstance(value, str) and not value.strip())


def _timestamp(value: object) -> datetime | None:
    if _blank(value):
        return None
    try:
        return datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError:
        return None


def _historical_name(
    lineage: dict[str, Any], universes: UniverseService,
) -> tuple[str | None, str | None, dict[str, str | None]]:
    """Return a name only when its persisted registry timestamp is reliable."""

    universe_id = lineage.get("primary_universe_id")
    promotion_at = _timestamp(lineage.get("promotion_date"))
    if _blank(universe_id):
        return None, "primary_universe_id_missing", {"universe_id": None, "universe_updated_at": None}
    if promotion_at is None:
        return None, "promotion_date_missing_or_invalid", {
            "universe_id": str(universe_id), "universe_updated_at": None,
        }
    try:
        record = universes.record(str(universe_id))
    except ValueError:
        return None, "universe_record_missing", {
            "universe_id": str(universe_id), "universe_updated_at": None,
        }
    updated_at = _timestamp(record.updated_at)
    details = {"universe_id": record.universe_id, "universe_updated_at": record.updated_at}
    if updated_at is None:
        return None, "universe_updated_at_missing_or_invalid", details
    if updated_at > promotion_at:
        return None, "universe_updated_after_promotion", details
    return record.name, None, details


def _refresh_master_snapshot_names(
    quality: ProductionQualityRepository,
    names_by_model: dict[str, str],
    *,
    apply: bool,
) -> dict[str, Any]:
    """Synchronize only the derived master-display universe-name column.

    ``models.parquet`` is a derived grid snapshot.  It stores the lineage field
    under the presentation-oriented ``universe_name`` column; no metric,
    health, baseline, series, or source observation is touched here.
    """

    report: dict[str, Any] = {
        "artifact_path": str(quality.snapshot_path()),
        "models_found": 0,
        "names_updated": [],
        "models_absent": [],
        "models_already_coherent": [],
        "discrepancies": [],
        "errors": [],
    }
    if not names_by_model:
        return report
    try:
        with quality.transaction():
            path = quality.snapshot_path()
            report["artifact_path"] = str(path)
            frame = quality.load_master_snapshot()
            report["models_found"] = len(frame)
            if frame.empty:
                report["models_absent"] = sorted(names_by_model)
                return report
            required = {"model_id", "universe_name"}
            missing = required - set(frame.columns)
            if missing:
                report["errors"].append(
                    {"error": f"master_snapshot_missing_columns:{','.join(sorted(missing))}"}
                )
                return report
            identifiers = frame["model_id"].astype(str)
            duplicate_ids = set(identifiers[identifiers.duplicated(keep=False)])
            changed = frame.copy()
            has_changes = False
            for model_id, expected_name in sorted(names_by_model.items()):
                matching = changed.index[identifiers.eq(model_id)].tolist()
                if not matching:
                    report["models_absent"].append(model_id)
                    continue
                if model_id in duplicate_ids:
                    report["errors"].append({
                        "model_id": model_id, "error": "duplicate_model_id_in_master_snapshot",
                    })
                    continue
                index = matching[0]
                current = changed.at[index, "universe_name"]
                if _blank(current):
                    report["names_updated"].append({
                        "model_id": model_id, "old_value": current,
                        "new_value": expected_name,
                    })
                    if apply:
                        changed.at[index, "universe_name"] = expected_name
                        has_changes = True
                elif str(current) == expected_name:
                    report["models_already_coherent"].append(model_id)
                else:
                    report["discrepancies"].append({
                        "model_id": model_id, "snapshot_value": current,
                        "lineage_value": expected_name,
                    })
            if apply and has_changes:
                quality._atomic_parquet(path, changed)
    except Exception as error:
        report["errors"].append({"error": str(error)})
    return report


def backfill_primary_universe_names(
    project_root: Path,
    *,
    apply: bool = True,
) -> dict[str, Any]:
    """Fill only missing, historically demonstrable universe names in lineage.

    The function intentionally does not rebuild quality snapshots, series,
    baselines, observations, predictions, or production models.
    """

    root = Path(project_root).resolve()
    production = ProductionRepository(root)
    quality = ProductionQualityRepository(root)
    universes = UniverseService(root=root)
    report: dict[str, Any] = {
        "operation": "backfill_primary_universe_name_at_promotion",
        "mode": "apply" if apply else "dry_run",
        "models_inspected": 0,
        "models_corrected": [],
        "models_already_populated": [],
        "models_unresolved": [],
        "errors": [],
    }

    for model in production.models():
        report["models_inspected"] += 1
        try:
            lineage = quality.load_lineage(model.model_id)
            if lineage is None:
                report["models_unresolved"].append({
                    "model_id": model.model_id, "reason": "lineage_missing",
                })
                continue
            old_value = lineage.get("primary_universe_name_at_promotion")
            if not _blank(old_value):
                report["models_already_populated"].append({
                    "model_id": model.model_id, "value": old_value,
                })
                continue
            name, reason, details = _historical_name(lineage, universes)
            if name is None:
                report["models_unresolved"].append({
                    "model_id": model.model_id, "old_value": old_value,
                    "reason": reason, **details,
                })
                continue
            change = {
                "model_id": model.model_id,
                "old_value": old_value,
                "new_value": name,
                "source": "persisted_universe_registry_preceding_promotion",
                "promotion_date": lineage.get("promotion_date"),
                **details,
            }
            if apply:
                corrected = dict(lineage)
                corrected["primary_universe_name_at_promotion"] = name
                quality.write_lineage(model.model_id, corrected, overwrite=True)
            report["models_corrected"].append(change)
        except Exception as error:  # keep independent model repairs independent
            report["errors"].append({"model_id": model.model_id, "error": str(error)})

    names_by_model = {
        model.model_id: str((quality.load_lineage(model.model_id) or {}).get(
            "primary_universe_name_at_promotion"
        ))
        for model in production.models()
        if not _blank((quality.load_lineage(model.model_id) or {}).get(
            "primary_universe_name_at_promotion"
        ))
    }
    report["master_snapshot"] = _refresh_master_snapshot_names(
        quality, names_by_model, apply=apply,
    )

    report["counts"] = {
        "corrected": len(report["models_corrected"]),
        "already_populated": len(report["models_already_populated"]),
        "unresolved": len(report["models_unresolved"]),
        "errors": len(report["errors"]),
    }
    return report
