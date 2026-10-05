"""Read-only comparison of persisted Predictor prefilter runs."""

from __future__ import annotations

import csv
import io
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Mapping, Sequence

from rstock.combinations import canonical_combination_id
from rstock.config import historical_prefilter_config_values


ND = "N/D"
_PASSED_STATUSES = frozenset({"retained", "removed_redundancy", "rejected_top_n"})
_TOP_N_STATUSES = frozenset({"retained", "removed_redundancy"})
_PREFILTER_SETTINGS = (
    "predictor_prefilter_min_worst_auc",
    "predictor_prefilter_min_pct_above_random",
    "predictor_prefilter_min_median_auc",
    "predictor_prefilter_max_auc_std",
    "predictor_prefilter_correlation_threshold",
    "predictor_prefilter_top_n",
)


def _json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return value if isinstance(value, dict) else {}


def _csv(path: Path) -> list[dict[str, str]] | None:
    try:
        with path.open(encoding="utf-8-sig", newline="") as stream:
            reader = csv.DictReader(stream)
            if not reader.fieldnames:
                return None
            return list(reader)
    except (OSError, csv.Error, UnicodeError):
        return None


def _mapping(value: object) -> Mapping[str, Any]:
    return value if isinstance(value, dict) else {}


def _number(value: object) -> float | None:
    try:
        result = float(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return None
    return result if result == result and abs(result) != float("inf") else None


def _identity(target: object, predictor: object) -> str | None:
    try:
        return canonical_combination_id(str(target or ""), "Up", [str(predictor or "")])
    except ValueError:
        return None


def _selected_from_manifest(manifest: Mapping[str, Any]) -> frozenset[str] | None:
    by_target = manifest.get("predictors_by_target")
    if not isinstance(by_target, dict):
        return None
    selected: set[str] = set()
    for target, predictors in by_target.items():
        if not isinstance(predictors, list):
            return None
        for predictor in predictors:
            identity = _identity(target, predictor)
            if identity is None:
                return None
            selected.add(identity)
    return frozenset(selected)


def _profile(config: Mapping[str, Any], manifest: Mapping[str, Any]) -> str | None:
    scientific = _mapping(config.get("rstock_config"))
    recorded = {
        key: value
        for source in (config, manifest, scientific)
        for key, value in source.items()
        if ("profile" in key.casefold() or "preset" in key.casefold())
        and value is not None
    }
    return json.dumps(recorded, ensure_ascii=False, sort_keys=True) if recorded else None


def _profile_parameters(config: Mapping[str, Any], manifest: Mapping[str, Any]) -> dict[str, Any]:
    scientific = _mapping(config.get("rstock_config"))
    settings = {
        key: value for key, value in scientific.items()
        if key.startswith("predictor_prefilter_") or key.startswith("temporal_consensus_")
        or "profile" in key.casefold() or "preset" in key.casefold()
        or key in {
            "lag_depth", "intraday_target_threshold", "intraday_down_threshold",
            "final_holdout_size", "walk_forward_window_mode",
            "walk_forward_min_train_size", "walk_forward_train_size",
            "walk_forward_test_size", "walk_forward_step_size",
        }
    }
    if scientific:
        settings.update(historical_prefilter_config_values(scientific))
    for key in ("prefilter_method", "stability_origin_count", "stability_step_sessions", "temporal_consensus_origins", "temporal_consensus_step_sessions", "temporal_consensus_min_occurrences"):
        value = config.get(key, manifest.get(key))
        if value is not None:
            settings[key] = value
    for key, value in _mapping(manifest.get("profile_parameters")).items():
        settings.setdefault(key, value)
    return settings


@dataclass(frozen=True, slots=True)
class PrefilterComparison:
    run_id: str
    requested_cutoff: str | None
    resolved_cutoff: str | None
    profile: str | None
    method: str | None
    universe: str | None
    settings: dict[str, Any]
    before: int | None
    passed: int | None
    after_top_n: int | None
    retained: int | None
    candidates: frozenset[str] | None
    origin_evaluations: int | None
    occurrence_distribution: dict[str, int] = field(default_factory=dict)
    origin_count: int | None = None

    @property
    def survival_rate(self) -> float | None:
        return self.passed / self.before if self.before and self.passed is not None else None

    def display_row(self) -> dict[str, Any]:
        return {
            **{f"Sélection {count}/{self.origin_count}": number for count, number in self.occurrence_distribution.items()},
            "Run ID": self.run_id,
            "Cutoff demandé": self.requested_cutoff or ND,
            "Cutoff résolu": self.resolved_cutoff or ND,
            "Profil / preset": self.profile or ND,
            "Méthode": self.method or ND,
            "Univers": self.universe or ND,
            "Worst AUC min": self.settings.get("predictor_prefilter_min_worst_auc", ND),
            "% fenêtres AUC > 0,50 min": self.settings.get("predictor_prefilter_min_pct_above_random", ND),
            "Médiane AUC min": self.settings.get("predictor_prefilter_min_median_auc", ND),
            "Std AUC max": self.settings.get("predictor_prefilter_max_auc_std", ND),
            "Corrélation max": self.settings.get("predictor_prefilter_correlation_threshold", ND),
            "Top N": self.settings.get("predictor_prefilter_top_n", ND),
            "Combinaisons avant filtre": self.before if self.before is not None else ND,
            "Admissibles": self.passed if self.passed is not None else ND,
            "Taux de survie": self.survival_rate if self.survival_rate is not None else ND,
            "Après Top N": self.after_top_n if self.after_top_n is not None else ND,
            "Retenus après corrélation": self.retained if self.retained is not None else ND,
            "Évaluations par origine": self.origin_evaluations if self.origin_evaluations is not None else ND,
        }


def load_prefilter_comparison(project_root: Path, run_id: str) -> PrefilterComparison:
    root = Path(project_root) / "runs" / run_id
    config = _json(root / "config.json")
    if config.get("job_type") not in (None, "predictor_prefilter"):
        raise ValueError("Prefilter comparison requires Predictor prefilter runs")
    summary = _json(root / "summary.json")
    manifest = _json(root / "results" / "predictor_prefilter.json")
    settings = _profile_parameters(config, manifest)
    method = settings.get("prefilter_method")
    if method is not None:
        method = str(method)
    scientific = _mapping(config.get("rstock_config"))
    for field in _PREFILTER_SETTINGS:
        if field in scientific:
            settings[field] = scientific[field]
    universe = config.get("primary_universe_id") or _mapping(config.get("universe_selection")).get("universe")
    requested = config.get("requested_historical_cutoff")
    resolved = (
        config.get("resolved_market_session_cutoff")
        or config.get("historical_data_cutoff")
        or summary.get("prepared_dataset_as_of")
        or manifest.get("prepared_dataset_as_of")
    )
    rows = _csv(root / "results" / "predictor_prefilter.csv")
    before = passed = after_top_n = None
    fallback_candidates: frozenset[str] | None = None
    if rows is not None and all("Observation" in row and "Predictor" in row for row in rows):
        identities = [_identity(row.get("Observation"), row.get("Predictor")) for row in rows]
        if all(identity is not None for identity in identities):
            before = len(set(identities))
            temporal = method == "temporal_stability" or any("EligibleFrequency" in row for row in rows)
            if temporal:
                frequencies = [_number(row.get("EligibleFrequency")) for row in rows]
                if all(value is not None for value in frequencies):
                    passed = sum(value > 0 for value in frequencies if value is not None)
            elif all("Eligible" in row for row in rows):
                parsed = [str(row["Eligible"]).casefold() for row in rows]
                if all(value in {"true", "false", "1", "0"} for value in parsed):
                    passed = sum(value in {"true", "1"} for value in parsed)
            if all("PrefilterStatus" in row for row in rows):
                after_top_n = sum(row["PrefilterStatus"] in _TOP_N_STATUSES for row in rows)
                fallback_candidates = frozenset(
                    identity for row, identity in zip(rows, identities, strict=True)
                    if row["PrefilterStatus"] == "retained" and identity is not None
                )
    candidates = _selected_from_manifest(manifest)
    if candidates is None:
        candidates = fallback_candidates
    origin_rows = _csv(root / "results" / "predictor_prefilter_origins.csv")
    return PrefilterComparison(
        run_id=run_id,
        requested_cutoff=None if requested is None else str(requested),
        resolved_cutoff=None if resolved is None else str(resolved),
        profile=_profile(config, manifest), method=method,
        universe=None if universe is None else str(universe),
        settings=settings, before=before, passed=passed,
        after_top_n=after_top_n,
        retained=None if candidates is None else len(candidates),
        candidates=candidates,
        occurrence_distribution=dict(manifest.get("occurrence_distribution", {})),
        origin_count=len(manifest["origin_cutoffs"]) if "origin_cutoffs" in manifest else None,
        origin_evaluations=None if origin_rows is None else len(origin_rows),
    )


@dataclass(frozen=True, slots=True)
class PrefilterOverlap:
    common: frozenset[str] | None
    own: dict[str, frozenset[str]] | None
    overlap_rate: float | None
    union: frozenset[str] | None


def compare_prefilter_candidates(items: Sequence[PrefilterComparison]) -> PrefilterOverlap:
    if not 2 <= len(items) <= 6 or len({item.run_id for item in items}) != len(items):
        raise ValueError("Compare two to six distinct Predictor prefilter runs")
    if any(item.candidates is None for item in items):
        return PrefilterOverlap(None, None, None, None)
    sets = [set(item.candidates or ()) for item in items]
    common = set.intersection(*sets)
    union = set.union(*sets)
    own = {
        item.run_id: frozenset(sets[index] - set.union(
            *(sets[:index] + sets[index + 1:])
        )) for index, item in enumerate(items)
    }
    return PrefilterOverlap(
        frozenset(common), own,
        len(common) / len(union) if union else None,
        frozenset(union),
    )


def prefilter_profile_differences(
    items: Sequence[PrefilterComparison],
) -> dict[str, dict[str, Any]]:
    """Expose every recorded parameter that differs, including missing values."""
    profiles = {
        item.run_id: {
            "Profil / preset": item.profile if item.profile is not None else ND,
            "Méthode": item.method if item.method is not None else ND,
            "Univers": item.universe if item.universe is not None else ND,
            **item.settings,
        } for item in items
    }
    fields = sorted(set().union(*(values.keys() for values in profiles.values())))
    return {
        field: {
            item.run_id: profiles[item.run_id].get(field, ND) for item in items
        }
        for field in fields
        if len({
            json.dumps(profiles[item.run_id].get(field, ND), sort_keys=True, default=str)
            for item in items
        }) > 1
    }


def prefilter_comparison_csv(items: Sequence[PrefilterComparison]) -> str:
    overlap = compare_prefilter_candidates(items)
    presence_columns = [f"present_in_{item.run_id}" for item in items]
    summary_columns = list(items[0].display_row())
    columns = [
        "record_type", *summary_columns, "profile_parameters_json",
        "count_definitions", "candidate_identity_definition",
        "candidate_id", "candidate_target", "candidate_direction",
        "candidate_predictor", "present_in_all", *presence_columns,
    ]
    output = io.StringIO()
    writer = csv.DictWriter(output, fieldnames=columns, lineterminator="\n")
    writer.writeheader()
    for item in items:
        writer.writerow({
            "record_type": "run_summary", **item.display_row(),
            "profile_parameters_json": json.dumps(
                item.settings, ensure_ascii=False, sort_keys=True,
            ),
            "count_definitions": (
                "before=distinct target/predictor pairs; "
                "passed=Eligible or EligibleFrequency>0 for temporal; "
                "after_top_n=before redundancy; retained=after redundancy; "
                "survival=passed/before"
            ),
            "candidate_identity_definition": "canonical_combination_id(target,Up,[predictor])",
        })
    known_union = set().union(*(item.candidates or () for item in items))
    for identity in sorted(known_union):
        values = json.loads(identity)
        presence = {
            column: (ND if item.candidates is None else int(identity in item.candidates))
            for column, item in zip(presence_columns, items, strict=True)
        }
        writer.writerow({
            "record_type": "candidate", "candidate_id": identity,
            "candidate_target": values[0], "candidate_direction": values[1],
            "candidate_predictor": values[2],
            "present_in_all": (
                ND if overlap.common is None else int(identity in overlap.common)
            ),
            **presence,
        })
    return output.getvalue()
