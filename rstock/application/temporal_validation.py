"""Persisted, policy-versioned comparison for temporal End-to-end validation."""

from __future__ import annotations

import hashlib
import json
import math
import random
from dataclasses import replace
from datetime import datetime, timezone
from pathlib import Path
from statistics import median, pstdev
from typing import Any, Callable

import pandas as pd

from .auto_promotion import _promotion_guidance
from .domain import JobType, RunPurpose, RunRole
from .repository import RunRepository


TEMPORAL_VALIDATION_SCHEMA_VERSION = 1
TEMPORAL_VALIDATION_POLICY_VERSION = 1
BOOTSTRAP_METHOD_VERSION = "moving_date_block_bootstrap_v1"
BOOTSTRAP_REPLICATIONS = 2_000
CANDIDATE_IDENTITY_STABILITY_POLICY_VERSION = "candidate_identity_stability_v1"
TEMPORAL_VALIDATION_CHECKPOINT = "orchestration/temporal_validation.json"
TEMPORAL_VALIDATION_RESULT = "results/temporal_validation_comparison.json"

_SOURCE_FILES = (
    "selected_thresholds_by_set.json",
    "threshold_metrics_by_set.csv",
    "holdout_metrics.csv",
    "holdout_predictions.csv",
)


def _sha256(path: Path) -> str | None:
    if not path.is_file():
        return None
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _read_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"Artefact temporel illisible: {path.name}") from error
    if not isinstance(value, dict):
        raise ValueError(f"Artefact temporel invalide: {path.name}")
    return value


def _read_csv(path: Path) -> pd.DataFrame:
    if not path.is_file():
        return pd.DataFrame()
    try:
        return pd.read_csv(path)
    except (OSError, pd.errors.ParserError, pd.errors.EmptyDataError) as error:
        raise ValueError(f"Artefact temporel illisible: {path.name}") from error


def _as_float(value: object) -> float | None:
    result = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    return None if pd.isna(result) else float(result)


def _summary(values: list[float]) -> dict[str, object]:
    if not values:
        return {"count": 0, "median": None, "q1": None, "q3": None, "minimum": None, "std": None}
    ordered = sorted(values)
    return {
        "count": len(ordered),
        "median": float(median(ordered)),
        "q1": float(pd.Series(ordered).quantile(0.25)),
        "q3": float(pd.Series(ordered).quantile(0.75)),
        "minimum": float(ordered[0]),
        "std": float(pstdev(ordered)) if len(ordered) > 1 else 0.0,
    }


def _selected_up_pairs(selected: dict[str, Any], metrics: pd.DataFrame) -> pd.DataFrame:
    required = {"Set", "Direction", "ROCAUC"}
    if not required.issubset(metrics.columns):
        return pd.DataFrame(columns=list(required))
    rows: list[pd.DataFrame] = []
    for set_name, directions in selected.items():
        if not isinstance(directions, dict):
            continue
        choice = directions.get("Up")
        if not isinstance(choice, dict) or choice.get("status") != "selected":
            continue
        matches = metrics[
            (metrics["Set"].astype(str) == str(set_name))
            & (metrics["Direction"].astype(str) == "Up")
        ].copy()
        threshold = _as_float(choice.get("threshold"))
        if threshold is not None and "Threshold" in matches:
            observed = pd.to_numeric(matches["Threshold"], errors="coerce")
            matches = matches[(observed - threshold).abs() <= 1e-9]
        rows.append(matches)
    if not rows:
        return metrics.iloc[0:0].copy()
    return pd.concat(rows, ignore_index=True)


def _candidate_sets(selected: dict[str, Any], results: Path) -> set[str]:
    guidance = _promotion_guidance(results, selected)
    if guidance.empty or "Statut promotion" not in guidance or "Combinaison" not in guidance:
        return set()
    return set(
        guidance.loc[
            guidance["Statut promotion"].astype(str) == "Candidat", "Combinaison"
        ].astype(str)
    )


def _candidate_symbols(
    symbol_set_id: str, target: object, predictors: object
) -> tuple[str, list[str]]:
    """Decode the existing persisted Set identity without changing its order."""

    try:
        symbols = json.loads(symbol_set_id)
    except (TypeError, json.JSONDecodeError):
        symbols = None
    if isinstance(symbols, list) and symbols:
        values = [str(value) for value in symbols]
        return values[0], values[1:]
    if "<-" in symbol_set_id:
        legacy_target, legacy_predictors = symbol_set_id.split("<-", 1)
        return legacy_target, [
            value.strip()
            for value in legacy_predictors.split("+")
            if value.strip()
        ]
    predictor_values = [
        value.strip()
        for value in str(predictors).replace(" + ", "+").split("+")
        if value.strip() and value.strip() != str(target)
    ]
    return str(target), predictor_values


def _optional_int(value: object) -> int | None:
    numeric = _as_float(value)
    if numeric is None or not numeric.is_integer():
        return None
    return int(numeric)



def _candidate_population(
    selected: dict[str, Any], results: Path
) -> dict[tuple[str, str], dict[str, object]]:
    """Build the final Up population already selected by promotion guidance."""

    guidance = _promotion_guidance(results, selected)
    if (
        guidance.empty
        or "Statut promotion" not in guidance
        or "Combinaison" not in guidance
    ):
        return {}
    candidates = guidance[
        guidance["Statut promotion"].astype(str) == "Candidat"
    ]
    population: dict[tuple[str, str], dict[str, object]] = {}
    for _, row in candidates.iterrows():
        set_id = str(row.get("Combinaison", ""))
        direction = str(row.get("Direction", "Up"))
        target, predictors = _candidate_symbols(
            set_id, row.get("Cible", ""), row.get("Predictors", "")
        )
        population[(set_id, direction)] = {
            "target": target,
            "predictors": predictors,
            "direction": direction,
            "symbol_set_id": set_id,
            "threshold": _as_float(row.get("Seuil calibré")),
            "holdout_signal_count": _optional_int(row.get("Signaux holdout")),
            "holdout_precision": _as_float(row.get("Précision holdout")),
            "holdout_auc": _as_float(row.get("AUC holdout")),
            "directional_return_mean": _as_float(
                row.get("Rendement directionnel moyen")
            ),
            "opposite_move_frequency": _as_float(
                row.get("Fréquence mouvement opposé")
            ),
        }
    return population



def candidate_identity_stability(
    reference_candidates: dict[tuple[str, str], dict[str, object]],
    validation_candidates: dict[tuple[str, str], dict[str, object]],
) -> dict[str, object]:
    """Describe candidate identity overlap without affecting any gate."""

    reference_keys = set(reference_candidates)
    validation_keys = set(validation_candidates)
    common_keys = reference_keys & validation_keys
    lost_keys = reference_keys - validation_keys
    new_keys = validation_keys - reference_keys

    def ordered(keys: set[tuple[str, str]]) -> list[tuple[str, str]]:
        return sorted(keys, key=lambda item: (item[0], item[1]))

    common = [
        {
            "target": reference_candidates[key]["target"],
            "predictors": reference_candidates[key]["predictors"],
            "direction": key[1],
            "symbol_set_id": key[0],
            "reference": {
                name: value
                for name, value in reference_candidates[key].items()
                if name not in {
                    "target", "predictors", "direction", "symbol_set_id"
                }
            },
            "validation": {
                name: value
                for name, value in validation_candidates[key].items()
                if name not in {
                    "target", "predictors", "direction", "symbol_set_id"
                }
            },
        }
        for key in ordered(common_keys)
    ]
    reference_count = len(reference_keys)
    validation_count = len(validation_keys)
    common_count = len(common_keys)
    union_count = len(reference_keys | validation_keys)
    return {
        "policy_version": CANDIDATE_IDENTITY_STABILITY_POLICY_VERSION,
        "reference_candidate_count": reference_count,
        "validation_candidate_count": validation_count,
        "common_candidate_count": common_count,
        "lost_candidate_count": len(lost_keys),
        "new_candidate_count": len(new_keys),
        "candidate_survival_rate": (
            common_count / reference_count if reference_count else None
        ),
        "validation_overlap_rate": (
            common_count / validation_count if validation_count else None
        ),
        "jaccard_index": common_count / union_count if union_count else None,
        "common_candidates": common,
        "lost_candidates": [reference_candidates[key] for key in ordered(lost_keys)],
        "new_candidates": [validation_candidates[key] for key in ordered(new_keys)],
    }


def _single_candidate_yield(
    selected: dict[str, Any], results: Path
) -> dict[str, object]:
    metrics = _read_csv(results / "holdout_metrics.csv")
    eligible = _selected_up_pairs(selected, metrics)
    if not eligible.empty:
        eligible = eligible.assign(
            Set=eligible["Set"].astype(str),
            Direction=eligible["Direction"].astype(str),
        ).drop_duplicates(["Set", "Direction"], keep="first")
    denominator = len(eligible)
    candidates = _candidate_sets(selected, results)
    numerator = len(candidates)
    if denominator == 0:
        return {
            "status": "inconclusive",
            "reason": "Aucune paire Up évaluée avec seuil sélectionné.",
            "metrics": {"candidate_count": numerator, "eligible_pair_count": 0, "ratio": None},
        }
    ratio = numerator / denominator
    return {
        "status": "passed" if ratio >= 0.0 else "failed",  # policy threshold applied by caller
        "reason": None,
        "metrics": {"candidate_count": numerator, "eligible_pair_count": denominator, "ratio": ratio},
    }


def _gate_yield(
    reference_selected: dict[str, Any],
    reference_results: Path,
    validation_selected: dict[str, Any],
    validation_results: Path,
) -> dict[str, object]:
    """Compare the final, evaluated Up population used by promotion.

    This deliberately uses selected holdout pairs rather than raw generated
    combinations, so the numerator and denominator remain comparable across
    periods with different available universes.
    """
    reference_gate = _single_candidate_yield(reference_selected, reference_results)
    validation_gate = _single_candidate_yield(validation_selected, validation_results)
    reference = dict(reference_gate["metrics"])
    validation = dict(validation_gate["metrics"])
    reference["candidate_yield"] = reference.pop("ratio")
    validation["candidate_yield"] = validation.pop("ratio")
    reference_yield = reference["candidate_yield"]
    validation_yield = validation["candidate_yield"]
    metrics = {"reference": reference, "validation": validation, "ratio": None}
    if reference_yield is None or validation_yield is None:
        return {
            "status": "inconclusive",
            "reason": "Insufficient evaluated Up population for candidate comparison.",
            "metrics": metrics,
        }
    if float(reference_yield) == 0:
        return {
            "status": "inconclusive",
            "reason": "Reference candidate yield is zero, so the ratio is not interpretable.",
            "metrics": metrics,
        }
    metrics["ratio"] = float(validation_yield) / float(reference_yield)
    return {"status": "passed", "reason": None, "metrics": metrics}


def _gate_auc(reference: Path, validation: Path) -> dict[str, object]:
    source = []
    for label, directory in (("reference", reference), ("validation", validation)):
        selected_path = directory / "selected_thresholds_by_set.json"
        if not selected_path.is_file():
            return {"status": "inconclusive", "reason": f"Sélections {label} absentes.", "metrics": {}}
        selected = _read_json(selected_path)
        pairs = _selected_up_pairs(selected, _read_csv(directory / "holdout_metrics.csv"))
        values = [item for item in (_as_float(value) for value in pairs.get("ROCAUC", [])) if item is not None]
        source.append((label, _summary(values)))
    metrics = dict(source)
    ref_median = metrics["reference"]["median"]
    validation_median = metrics["validation"]["median"]
    if ref_median is None or validation_median is None:
        return {"status": "inconclusive", "reason": "AUC holdout insuffisante.", "metrics": metrics}
    metrics["median_degradation"] = float(ref_median) - float(validation_median)
    return {"status": "passed", "reason": None, "metrics": metrics}


def _candidate_predictions(selected: dict[str, Any], results: Path) -> pd.DataFrame:
    candidates = _candidate_sets(selected, results)
    values = _read_csv(results / "holdout_predictions.csv")
    required = {"Set", "Direction", "Date", "Target", "IntradayReturn", "Prediction"}
    if not required.issubset(values.columns) or not candidates:
        return values.iloc[0:0].copy()
    return values[
        values["Set"].astype(str).isin(candidates)
        & (values["Direction"].astype(str) == "Up")
    ].copy()


def _directional_returns(frame: pd.DataFrame) -> pd.Series:
    """Use the existing holdout IntradayReturn convention for both sides."""
    returns = pd.to_numeric(frame["IntradayReturn"], errors="coerce")
    return returns.where(
        frame["Direction"].astype(str) == "Up",
        -returns.where(frame["Direction"].astype(str) == "Down"),
    )


def _precision_metrics(frame: pd.DataFrame) -> dict[str, float] | None:
    """Calculate each candidate pair first, then take their unweighted mean."""
    required = {"Set", "Direction", "_target", "_signal"}
    if not required.issubset(frame.columns):
        return None
    pair_metrics: list[tuple[float, float, float]] = []
    for _, pair in frame.groupby(["Set", "Direction"], sort=True):
        baseline = pair["_target"].mean()
        precision = pair.loc[pair["_signal"], "_target"].mean()
        if pd.isna(baseline) or pd.isna(precision):
            continue
        pair_metrics.append((float(precision), float(baseline), float(precision - baseline)))
    if not pair_metrics:
        return None
    precision, baseline, edge = zip(*pair_metrics, strict=True)
    return {
        "precision_signal": float(sum(precision) / len(precision)),
        "baseline_direction_rate": float(sum(baseline) / len(baseline)),
        "precision_edge": float(sum(edge) / len(edge)),
        "pair_count": len(pair_metrics),
    }


def _bootstrap_seed(
    reference_run_id: str,
    validation_run_id: str,
    reference_holdout_predictions_digest: str,
    validation_holdout_predictions_digest: str,
    method_version: str = BOOTSTRAP_METHOD_VERSION,
) -> int:
    """Return the prescribed seed from IDs, prediction digests, and method."""
    material = "\n".join(
        (
            reference_run_id,
            validation_run_id,
            reference_holdout_predictions_digest,
            validation_holdout_predictions_digest,
            method_version,
        )
    )
    return int(hashlib.sha256(material.encode("utf-8")).hexdigest()[:16], 16)


def _bootstrap(
    frame: pd.DataFrame,
    statistic: Callable[[pd.DataFrame], float | None],
    *,
    seed_material: str,
    confidence_level: float,
    seed: int | None = None,
) -> dict[str, object] | None:
    dates = sorted(frame["Date"].dropna().astype(str).unique())
    if len(dates) < 2:
        return None
    block_size = max(2, math.ceil(len(dates) ** (1 / 3)))
    seed = (
        int(hashlib.sha256(seed_material.encode()).hexdigest()[:16], 16)
        if seed is None
        else seed
    )
    grouped = {date: frame[frame["Date"].astype(str) == date] for date in dates}
    randomizer = random.Random(seed)
    samples: list[float] = []
    for _ in range(BOOTSTRAP_REPLICATIONS):
        sampled: list[str] = []
        while len(sampled) < len(dates):
            start = randomizer.randrange(len(dates))
            sampled.extend(dates[(start + offset) % len(dates)] for offset in range(block_size))
        value = statistic(pd.concat([grouped[date] for date in sampled[: len(dates)]], ignore_index=True))
        if value is not None and math.isfinite(value):
            samples.append(value)
    if not samples:
        return None
    return {
        "method_version": BOOTSTRAP_METHOD_VERSION,
        "replications": BOOTSTRAP_REPLICATIONS,
        "seed": seed,
        "date_count": len(dates),
        "block_size": block_size,
        "effective_replications": len(samples),
        "confidence_level": confidence_level,
        "lower_ci": float(pd.Series(samples).quantile((1 - confidence_level) / 2)),
        "upper_ci": float(pd.Series(samples).quantile(1 - (1 - confidence_level) / 2)),
    }


def _ci_gate_status(
    lower: float,
    upper: float,
    threshold: float,
    *,
    max_width: float | None = None,
) -> str:
    """Classify an interval without treating uncertainty as a failure."""
    if lower > threshold and (max_width is None or upper - lower <= max_width):
        return "passed"
    if upper < threshold:
        return "failed"
    return "inconclusive"


def _yield_gate_status(ratio: float | None, threshold: float) -> str:
    return "inconclusive" if ratio is None else (
        "passed" if ratio >= threshold else "failed"
    )


def _auc_gate_status(
    reference_median: float | None,
    validation_median: float | None,
    max_degradation: float,
) -> str:
    if reference_median is None or validation_median is None:
        return "inconclusive"
    return (
        "passed"
        if validation_median > 0.50
        and validation_median >= reference_median - max_degradation
        else "failed"
    )


def _final_status(gates: dict[str, dict[str, object]]) -> str:
    statuses = [str(gate["status"]) for gate in gates.values()]
    if "invalid" in statuses:
        return "invalid"
    if "failed" in statuses:
        return "failed"
    if "inconclusive" in statuses:
        return "inconclusive"
    return "passed"


def _scientific_snapshot(spec: Any) -> dict[str, Any]:
    """Remove intentional orchestration differences before comparison."""
    values = spec.to_dict()
    for key in ("temporal_validation_enabled", "auto_promote_candidates", "run_description"):
        values.pop(key, None)
    config = dict(values["rstock_config"])
    config.pop("walk_forward_end_offset_sessions", None)
    values["rstock_config"] = config
    return values


def _resolved_temporal_context(
    *, run_id: str, purpose: RunPurpose, end_offset: int, results: Path
) -> dict[str, object]:
    """Persist the effective holdout period already resolved by the pipeline."""
    predictions_path = results / "holdout_predictions.csv"
    effective_start: str | None = None
    effective_end: str | None = None
    if predictions_path.is_file():
        predictions = _read_csv(predictions_path)
        required = {"Set", "Direction", "Date", "Target", "IntradayReturn", "Prediction"}
        if not required.issubset(predictions.columns):
            raise ValueError("Validation holdout predictions are structurally invalid")
        dates = pd.to_datetime(predictions["Date"], errors="coerce").dropna()
        if predictions.shape[0] and dates.empty:
            raise ValueError("Validation holdout prediction dates are invalid")
        if not dates.empty:
            effective_start = dates.min().normalize().date().isoformat()
            effective_end = dates.max().normalize().date().isoformat()
    return {
        "run_id": run_id,
        "purpose": purpose.value,
        "end_offset_sessions": end_offset,
        "effective_period": {"start": effective_start, "end": effective_end},
        "effective_start": effective_start,
        "effective_end": effective_end,
    }


class TemporalValidationRunner:
    """Calculate once from persisted child outputs, then reuse the checkpoint."""

    def __init__(
        self,
        repository: RunRepository,
        *,
        root_run_id: str,
        validation_run_id: str,
        result_output: Path | None = None,
        recover_invalid_preflight: bool = False,
    ) -> None:
        self.repository = repository
        self.root_run_id = root_run_id
        self.validation_run_id = validation_run_id
        self._result_output = result_output
        self._recover_invalid_preflight = recover_invalid_preflight

    @property
    def checkpoint_path(self) -> Path:
        return self.repository.run_directory(self.root_run_id) / TEMPORAL_VALIDATION_CHECKPOINT

    @property
    def result_path(self) -> Path:
        if self._result_output is not None:
            return self._result_output
        return self.repository.run_directory(self.root_run_id) / TEMPORAL_VALIDATION_RESULT

    def _threshold_run_id(self, run_id: str) -> str:
        manifest = self.repository.read_json(run_id, "orchestration/pipeline.json")
        stages = manifest.get("stages")
        if not isinstance(stages, list):
            raise ValueError("Manifest End-to-end temporel invalide")
        matches = [item for item in stages if isinstance(item, dict) and item.get("stage_key") == "threshold_calibration"]
        if len(matches) != 1 or not matches[0].get("child_run_id"):
            raise ValueError("Étape threshold_calibration temporelle absente")
        return str(matches[0]["child_run_id"])

    def _preflight(self) -> tuple[dict[str, object], str | None]:
        try:
            reference_spec = self.repository.load_spec(self.root_run_id)
            validation_spec = self.repository.load_spec(self.validation_run_id)
            reference_metadata = self.repository.run_metadata(self.root_run_id)
            validation_metadata = self.repository.run_metadata(self.validation_run_id)
            if reference_spec.job_type is not JobType.END_TO_END or validation_spec.job_type is not JobType.END_TO_END:
                raise ValueError("Les deux runs doivent être End-to-end")
            if reference_metadata.run_purpose is not RunPurpose.REFERENCE:
                raise ValueError("Le run de référence n’a pas le purpose reference")
            if validation_metadata.run_purpose is not RunPurpose.TEMPORAL_VALIDATION:
                raise ValueError("Le run de validation n’a pas le purpose temporal_validation")
            if validation_metadata.reference_run_id != self.root_run_id:
                raise ValueError("La référence du child temporel est incompatible")
            if reference_spec.config.walk_forward_end_offset_sessions != 0:
                raise ValueError("L’offset de référence doit être 0")
            if validation_spec.config.walk_forward_end_offset_sessions != 63:
                raise ValueError("L’offset de validation doit être 63")
            if validation_spec.temporal_validation_enabled or validation_spec.auto_promote_candidates:
                raise ValueError("Le child temporel ne doit pas chaîner validation ou promotion")
            if _scientific_snapshot(reference_spec) != _scientific_snapshot(validation_spec):
                raise ValueError("The scientific configurations of both chains differ")
            reference_threshold = self._threshold_run_id(self.root_run_id)
            validation_threshold = self._threshold_run_id(self.validation_run_id)
            source_digests: dict[str, dict[str, str | None]] = {}
            for label, run_id in (("reference", reference_threshold), ("validation", validation_threshold)):
                results = self.repository.run_directory(run_id) / "results"
                source_digests[label] = {name: _sha256(results / name) for name in _SOURCE_FILES}
            required_sources = {
                "reference": _SOURCE_FILES[:3],
                "validation": _SOURCE_FILES[:3],
            }
            for label, names in required_sources.items():
                missing = [name for name in names if source_digests[label].get(name) is None]
                if missing:
                    raise ValueError(
                        f"Artefacts temporels requis absents ({label}) : {', '.join(missing)}"
                    )
            temporal_context = {
                "reference": _resolved_temporal_context(
                    run_id=self.root_run_id,
                    purpose=reference_metadata.run_purpose,
                    end_offset=reference_spec.config.walk_forward_end_offset_sessions,
                    results=self.repository.run_directory(reference_threshold) / "results",
                ),
                "validation": _resolved_temporal_context(
                    run_id=self.validation_run_id,
                    purpose=validation_metadata.run_purpose,
                    end_offset=validation_spec.config.walk_forward_end_offset_sessions,
                    results=self.repository.run_directory(validation_threshold) / "results",
                ),
            }
            parameters = {
                "temporal_min_candidate_yield_ratio": reference_spec.config.temporal_min_candidate_yield_ratio,
                "temporal_max_auc_degradation": reference_spec.config.temporal_max_auc_degradation,
                "temporal_min_precision_edge": reference_spec.config.temporal_min_precision_edge,
                "temporal_min_mean_directional_return": reference_spec.config.temporal_min_mean_directional_return,
                "temporal_confidence_level": reference_spec.config.temporal_confidence_level,
                "temporal_max_ci_width": reference_spec.config.temporal_max_ci_width,
            }
            identity: dict[str, object] = {
                "schema_version": TEMPORAL_VALIDATION_SCHEMA_VERSION,
                "policy_version": TEMPORAL_VALIDATION_POLICY_VERSION,
                "bootstrap_method_version": BOOTSTRAP_METHOD_VERSION,
                "reference_run_id": self.root_run_id,
                "validation_run_id": self.validation_run_id,
                "reference_threshold_run_id": reference_threshold,
                "validation_threshold_run_id": validation_threshold,
                "reference_fingerprint": self.repository.configuration_fingerprint(self.root_run_id),
                "validation_fingerprint": self.repository.configuration_fingerprint(self.validation_run_id),
                "source_artifact_digests": source_digests,
                "temporal_context": temporal_context,
                "parameters": parameters,
            }
            return identity, None
        except (OSError, ValueError, KeyError) as error:
            return {"reference_run_id": self.root_run_id, "validation_run_id": self.validation_run_id}, str(error)

    def _persist(self, state: dict[str, object]) -> dict[str, object]:
        self.checkpoint_path.parent.mkdir(parents=True, exist_ok=True)
        self.result_path.parent.mkdir(parents=True, exist_ok=True)
        self.repository.write_json(self.root_run_id, TEMPORAL_VALIDATION_CHECKPOINT, state)
        payload = json.dumps(state, indent=2, ensure_ascii=False, default=str) + "\n"
        self.repository._atomic_json_write(self.result_path, payload)
        return state

    def _completed_invalid(self, identity: dict[str, object], error: str) -> dict[str, object]:
        state = {**identity, "status": "completed", "final_status": "invalid", "error": error, "gates": {}, "executed_at": datetime.now(timezone.utc).isoformat()}
        return self._persist(state)

    def execute(self) -> dict[str, object]:
        if self.checkpoint_path.exists():
            legacy = self.repository.read_json(
                self.root_run_id, TEMPORAL_VALIDATION_CHECKPOINT
            )
            if (
                legacy.get("status") == "completed"
                and "temporal_context" not in legacy
                and not (
                    self._recover_invalid_preflight
                    and legacy.get("final_status") == "invalid"
                )
            ):
                return legacy
        identity, preflight_error = self._preflight()
        if preflight_error is not None:
            return self._completed_invalid(identity, preflight_error)
        canonical = json.dumps(identity, sort_keys=True, separators=(",", ":"))
        input_sha256 = hashlib.sha256(canonical.encode()).hexdigest()
        if self.checkpoint_path.exists():
            state = self.repository.read_json(self.root_run_id, TEMPORAL_VALIDATION_CHECKPOINT)
            if state.get("input_sha256") == input_sha256 and state.get("status") == "completed":
                if state.get("temporal_context") != identity["temporal_context"]:
                    raise ValueError("Temporal checkpoint context is incompatible")
                self._persist(state)
                return state
            if state.get("input_sha256") != input_sha256:
                self._persist({
                    **identity,
                    "input_sha256": input_sha256,
                    "supersedes_input_sha256": state.get("input_sha256"),
                    "status": "pending",
                    "final_status": None,
                    "error": None,
                    "gates": {},
                    "executed_at": None,
                })
        else:
            self._persist({**identity, "input_sha256": input_sha256, "status": "pending", "final_status": None, "error": None, "gates": {}, "executed_at": None})

        reference_results = self.repository.run_directory(str(identity["reference_threshold_run_id"])) / "results"
        validation_results = self.repository.run_directory(str(identity["validation_threshold_run_id"])) / "results"
        reference_selected = _read_json(reference_results / "selected_thresholds_by_set.json")
        validation_selected = _read_json(validation_results / "selected_thresholds_by_set.json")
        parameters = identity["parameters"]
        assert isinstance(parameters, dict)
        identity_stability = candidate_identity_stability(
            _candidate_population(reference_selected, reference_results),
            _candidate_population(validation_selected, validation_results),
        )

        yield_gate = _gate_yield(
            reference_selected,
            reference_results,
            validation_selected,
            validation_results,
        )
        ratio = yield_gate["metrics"].get("ratio") if isinstance(yield_gate.get("metrics"), dict) else None
        if ratio is not None:
            yield_gate["status"] = _yield_gate_status(
                float(ratio), float(parameters["temporal_min_candidate_yield_ratio"])
            )
            yield_gate["reason"] = None if yield_gate["status"] == "passed" else "Rendement de candidats inférieur au seuil."

        auc_gate = _gate_auc(reference_results, validation_results)
        if auc_gate["status"] == "passed":
            metrics = auc_gate["metrics"]
            assert isinstance(metrics, dict)
            validation_median = float(metrics["validation"]["median"])
            reference_median = float(metrics["reference"]["median"])
            max_degradation = float(parameters["temporal_max_auc_degradation"])
            auc_gate["status"] = _auc_gate_status(
                reference_median, validation_median, max_degradation
            )
            auc_gate["reason"] = None if auc_gate["status"] == "passed" else "Médiane AUC validation insuffisante ou dégradée."

        predictions = _candidate_predictions(validation_selected, validation_results)
        source_digests = identity["source_artifact_digests"]
        assert isinstance(source_digests, dict)
        reference_digests = source_digests["reference"]
        validation_digests = source_digests["validation"]
        assert isinstance(reference_digests, dict) and isinstance(validation_digests, dict)
        bootstrap_seed = _bootstrap_seed(
            self.root_run_id,
            self.validation_run_id,
            str(reference_digests.get("holdout_predictions.csv") or ""),
            str(validation_digests.get("holdout_predictions.csv") or ""),
        )
        precision_gate: dict[str, object] = {"status": "inconclusive", "reason": "Prédictions candidat Up insuffisantes.", "metrics": {}}
        return_gate: dict[str, object] = {"status": "inconclusive", "reason": "Prédictions candidat Up insuffisantes.", "metrics": {}}
        if not predictions.empty:
            numeric_target = pd.to_numeric(predictions["Target"], errors="coerce")
            signals = pd.to_numeric(predictions["Prediction"], errors="coerce") == 1
            precision_frame = predictions.assign(_target=numeric_target, _signal=signals)
            return_frame = predictions.assign(
                _return=_directional_returns(predictions), _signal=signals
            )
            def precision_stat(frame: pd.DataFrame) -> float | None:
                metrics = _precision_metrics(frame)
                return None if metrics is None else metrics["precision_edge"]
            def return_stat(frame: pd.DataFrame) -> float | None:
                value = frame.loc[frame["_signal"], "_return"].mean()
                return None if pd.isna(value) else float(value)
            confidence_level = float(parameters["temporal_confidence_level"])
            precision_bootstrap = _bootstrap(
                precision_frame,
                precision_stat,
                seed_material="moving_date_block_bootstrap_v1",
                confidence_level=confidence_level,
                seed=bootstrap_seed,
            )
            return_bootstrap = _bootstrap(
                return_frame,
                return_stat,
                seed_material="moving_date_block_bootstrap_v1",
                confidence_level=confidence_level,
                seed=bootstrap_seed,
            )
            if precision_bootstrap is not None:
                lower, upper = float(precision_bootstrap["lower_ci"]), float(precision_bootstrap["upper_ci"])
                width = upper - lower
                threshold = float(parameters["temporal_min_precision_edge"])
                status = _ci_gate_status(
                    lower,
                    upper,
                    threshold,
                    max_width=float(parameters["temporal_max_ci_width"]),
                )
                precision_metrics = _precision_metrics(precision_frame) or {}
                precision_gate = {"status": status, "reason": None if status == "passed" else "IC insuffisant.", "metrics": {**precision_metrics, "signal_count": int(signals.sum()), "ci_width": width, "bootstrap": precision_bootstrap}}
            if return_bootstrap is not None:
                lower, upper = float(return_bootstrap["lower_ci"]), float(return_bootstrap["upper_ci"])
                threshold = float(parameters["temporal_min_mean_directional_return"])
                status = _ci_gate_status(lower, upper, threshold)
                return_gate = {"status": status, "reason": None if status == "passed" else "IC non concluant.", "metrics": {"mean_directional_return": return_stat(return_frame), "signal_count": int(signals.sum()), "bootstrap": return_bootstrap}}

        gates = {"candidate_yield": yield_gate, "holdout_auc": auc_gate, "precision_edge": precision_gate, "directional_return": return_gate}
        final_status = _final_status(gates)
        # Reconcile the persistent checkpoint before the final atomic writes.
        current = self.repository.read_json(self.root_run_id, TEMPORAL_VALIDATION_CHECKPOINT)
        if current.get("input_sha256") != input_sha256:
            raise ValueError("Checkpoint temporel modifié pendant l’exécution")
        return self._persist({
            **identity,
            "input_sha256": input_sha256,
            **({"supersedes_input_sha256": current["supersedes_input_sha256"]}
               if current.get("supersedes_input_sha256") else {}),
            "status": "completed",
            "final_status": final_status,
            "error": None,
            "gates": gates,
            "candidate_identity_stability": identity_stability,
            "executed_at": datetime.now(timezone.utc).isoformat(),
        })


def recover_temporal_validation(
    repository: RunRepository, root_run_id: str
) -> dict[str, object]:
    """Recover one explicitly requested comparison after an invalid preflight."""

    spec = repository.load_spec(root_run_id)
    if spec.job_type is not JobType.END_TO_END or not spec.temporal_validation_enabled:
        raise ValueError("Le run cible doit etre un End-to-end temporel.")
    if repository.status(root_run_id).get("status") != "completed":
        raise ValueError("La chaine de reference doit etre completed.")

    checkpoint_path = (
        repository.run_directory(root_run_id) / TEMPORAL_VALIDATION_CHECKPOINT
    )
    if not checkpoint_path.is_file():
        raise ValueError("Le checkpoint temporel invalide est absent.")
    previous = repository.read_json(root_run_id, TEMPORAL_VALIDATION_CHECKPOINT)
    metadata = repository.run_metadata(root_run_id)
    if (
        metadata.run_role is RunRole.PIPELINE_PARENT
        and metadata.run_purpose is RunPurpose.REFERENCE
        and previous.get("status") == "completed"
        and "temporal_context" in previous
    ):
        return previous
    if (
        previous.get("status") != "completed"
        or previous.get("final_status") != "invalid"
        or "temporal_context" in previous
    ):
        raise ValueError(
            "La recuperation exige un ancien preflight terminal invalid incomplet."
        )
    if (
        metadata.run_role is not RunRole.STANDALONE
        or metadata.run_purpose is not RunPurpose.STANDARD
    ):
        raise ValueError(
            "Les metadonnees historiques ne correspondent pas au defaut cible."
        )

    manifest = repository.read_json(root_run_id, "orchestration/pipeline.json")
    temporal_stages = [
        item
        for item in manifest.get("stages", ())
        if isinstance(item, dict)
        and item.get("stage_key") == "temporal_validation_end_to_end"
    ]
    if len(temporal_stages) != 1 or not temporal_stages[0].get("child_run_id"):
        raise ValueError("Le child End-to-end temporel est absent du manifest.")
    validation_run_id = str(temporal_stages[0]["child_run_id"])
    if repository.status(validation_run_id).get("status") != "completed":
        raise ValueError("La chaine offset 63 doit etre completed.")
    validation_metadata = repository.run_metadata(validation_run_id)
    if (
        validation_metadata.run_purpose is not RunPurpose.TEMPORAL_VALIDATION
        or validation_metadata.reference_run_id != root_run_id
    ):
        raise ValueError("La relation du child temporel est incompatible.")

    validation_spec = repository.load_spec(validation_run_id)
    if spec.config.walk_forward_end_offset_sessions != 0:
        raise ValueError("Offset de reference attendu: 0.")
    if validation_spec.config.walk_forward_end_offset_sessions != 63:
        raise ValueError("Offset de validation attendu: 63.")
    if _scientific_snapshot(spec) != _scientific_snapshot(validation_spec):
        raise ValueError("The scientific configurations of both chains differ")

    def threshold_run_id(run_id: str) -> str:
        pipeline = repository.read_json(run_id, "orchestration/pipeline.json")
        matches = [
            item
            for item in pipeline.get("stages", ())
            if isinstance(item, dict)
            and item.get("stage_key") == "threshold_calibration"
        ]
        if len(matches) != 1 or not matches[0].get("child_run_id"):
            raise ValueError(
                f"Etape threshold_calibration absente pour {run_id}."
            )
        threshold_id = str(matches[0]["child_run_id"])
        if repository.status(threshold_id).get("status") != "completed":
            raise ValueError(
                f"La calibration des seuils {threshold_id} doit etre completed."
            )
        return threshold_id

    reference_threshold = threshold_run_id(root_run_id)
    validation_threshold = threshold_run_id(validation_run_id)
    source_digests: dict[str, dict[str, str]] = {}
    artifact_paths: dict[str, dict[str, str]] = {}
    for label, threshold_id in (
        ("reference", reference_threshold),
        ("validation", validation_threshold),
    ):
        results = repository.run_directory(threshold_id) / "results"
        source_digests[label] = {}
        artifact_paths[label] = {}
        for name in _SOURCE_FILES:
            path = results / name
            digest = _sha256(path)
            if digest is None:
                raise ValueError(
                    f"Artefact temporel requis absent ({label}) : {name}"
                )
            source_digests[label][name] = digest
            artifact_paths[label][name] = str(path)

    recovery_context = {
        "event": "temporal_validation_recovered_from_invalid_preflight",
        "root_run_id": root_run_id,
        "old_run_role": metadata.run_role.value,
        "old_run_purpose": metadata.run_purpose.value,
        "new_run_role": RunRole.PIPELINE_PARENT.value,
        "new_run_purpose": RunPurpose.REFERENCE.value,
        "reference_threshold_run_id": reference_threshold,
        "validation_run_id": validation_run_id,
        "validation_threshold_run_id": validation_threshold,
        "artifact_paths": artifact_paths,
        "source_artifact_digests": source_digests,
        "previous_checkpoint_final_status": previous.get("final_status"),
        "auto_promote_candidates": spec.auto_promote_candidates,
    }
    repository.append_log(
        root_run_id,
        "temporal_validation_recovered_from_invalid_preflight started "
        + json.dumps(recovery_context, sort_keys=True, ensure_ascii=False),
    )
    repository.write_json(
        root_run_id,
        "metadata.json",
        replace(
            metadata,
            run_role=RunRole.PIPELINE_PARENT,
            run_purpose=RunPurpose.REFERENCE,
        ).to_dict(),
    )
    repository.append_log(
        root_run_id,
        "temporal_validation_recovered_from_invalid_preflight "
        "invalidated_previous_checkpoint",
    )

    result = TemporalValidationRunner(
        repository,
        root_run_id=root_run_id,
        validation_run_id=validation_run_id,
        recover_invalid_preflight=True,
    ).execute()
    repository.append_log(
        root_run_id,
        "temporal_validation_recovered_from_invalid_preflight completed "
        + json.dumps(
            {
                "final_status": result.get("final_status"),
                "gates": {
                    name: gate.get("status")
                    for name, gate in dict(result.get("gates", {})).items()
                    if isinstance(gate, dict)
                },
            },
            sort_keys=True,
        ),
    )
    return result
