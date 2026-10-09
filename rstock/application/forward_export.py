"""Read-only, on-demand ZIP of persisted Forward evidence. No scientific calculations."""
from __future__ import annotations

import hashlib
import io
import json
from datetime import datetime, timezone
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile

import pandas as pd

from .forward_diagnostic import (
    ANALYSIS, BINS, MANIFEST, METRICS, PROTOCOL, _key, _select, _stage_ids, _stage_expected,
)
from .market_context import DIAGNOSTIC_MANIFEST, METRICS_FILE, ROBUSTNESS_FILE
from .market_context_runtime import load_context_diagnostic


CORE_FILES = {
    "forward_period_metrics.csv": "performances_forward.csv",
    "forward_observations.csv": "observations_forward.csv",
    "forward_exclusions.csv": "exclusions_forward.csv",
    "forward_daily_metrics.csv": "evolution_quotidienne.csv",
    "forward_population_metrics.csv": "population_forward.csv",
}
STRING_COLUMNS = {name: str for name in (
    "source_model_id", "canonical_combination_id", "model_key", "Set", "target",
    "forward_simulation_run_id", "source_end_to_end_run_id",
)}
JOIN_KEYS = ["source_model_id", "period_kind", "horizon"]


def _hash(raw: bytes) -> str:
    return hashlib.sha256(raw).hexdigest()


def _value(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, allow_nan=False) if isinstance(value, (dict, list, tuple)) else value


class _Evidence:
    def __init__(self, runs: Path):
        self.runs = runs.resolve()
        self.sources = {}
        self.unavailable = {}

    def read(self, path: Path, expected: str | None = None) -> bytes:
        resolved = path.resolve()
        if not resolved.is_relative_to(self.runs):
            raise ValueError("export_reference_outside_runs")
        raw = resolved.read_bytes()
        sha = _hash(raw)
        if expected is not None and sha != expected:
            raise ValueError(f"export_source_integrity_error: {resolved.relative_to(self.runs)}")
        key = resolved.relative_to(self.runs).as_posix()
        old = self.sources.get(key)
        if old and old["sha256"] != sha:
            raise ValueError("export_source_changed_during_generation")
        self.sources[key] = {"sha256": sha, "verification": "verified" if expected or old and old["verification"] == "verified" else "persisted_without_expected_digest"}
        return raw

    def document(self, path: Path, expected: str | None = None):
        value = json.loads(self.read(path, expected))
        if not isinstance(value, dict):
            raise ValueError("export_invalid_manifest")
        return value

    def frame(self, path: Path, expected: str | None = None):
        return pd.read_csv(io.BytesIO(self.read(path, expected)), dtype=STRING_COLUMNS)

    def unchanged(self):
        for name, source in self.sources.items():
            if _hash((self.runs / name).read_bytes()) != source["sha256"]:
                raise ValueError("export_source_changed_during_generation")


def _identity(model):
    return {
        "source_model_id": model.get("source_model_id"),
        "canonical_combination_id": model.get("canonical_combination_id") or (
            _key(model["set"], model["direction"]) if model.get("set") and model.get("direction") else None),
        "target": model.get("target"), "direction": model.get("direction"),
        "set": model.get("set"), "origin": model.get("origin", "unavailable"),
    }


def _annotate(frame, models, *, run_id, source_id):
    result = frame.copy()
    result["forward_run_id"] = run_id
    result["source_e2e_run_id"] = source_id
    if "source_model_id" in result:
        by_id = {str(m["source_model_id"]): _identity(m) for m in models if m.get("source_model_id") is not None}
        for column in ("canonical_combination_id", "target", "direction", "origin"):
            mapped = result.source_model_id.map({key: value[column] for key, value in by_id.items()})
            result[column] = result[column].combine_first(mapped) if column in result else mapped
    return result


def _t0_rows(models, snapshot_models, run_id, source_id, reference, cutoff):
    snapshots = {str(m["source_model_id"]): m for m in snapshot_models if m.get("source_model_id") is not None}
    rows = []
    for model in models:
        snapshot = snapshots.get(str(model.get("source_model_id")), {})
        row = {"forward_run_id": run_id, "source_e2e_run_id": source_id,
               "parent_e2e_run_id": reference.get("parent_e2e_run_id"),
               "cutoff_t0": cutoff, **_identity(model),
               "forward_available": model.get("forward_available"),
               "predictors": _value(snapshot.get("predictors")),
               "feature_names": _value(snapshot.get("feature_names")),
               "lag_depth": snapshot.get("lag_depth"),
               "xgboost_parameters": _value(snapshot.get("xgboost_parameters")),
               "train_start": snapshot.get("train_start"), "train_end": snapshot.get("train_end"),
               "training_observations": snapshot.get("training_observations"),
               "up_threshold": model.get("up_threshold"), "down_threshold": model.get("down_threshold"),
               "t0_reference_status": "persisted" if reference else "unavailable"}
        for group in ("wf", "holdout_qualification", "holdout_comparable", "qualification_decision", "parent_decision"):
            values = model.get(group) or {}
            row[f"{group}__status"] = values.get("availability", "persisted" if values else "unavailable")
            for name, value in values.items():
                if name not in {"bins", "probability_bins"}:
                    row[f"{group}__{name}"] = _value(value)
        rows.append(row)
    return pd.DataFrame(rows)


def _qualification_fallback(evidence, source_id, models):
    """Read existing qualification tables only; never construct comparable metrics."""
    if not source_id:
        return
    owner = evidence.runs / str(source_id)
    try:
        evidence.document(owner / "orchestration/pipeline.json")
        stages = _stage_ids(owner)
    except (OSError, ValueError):
        return
    for group, stage, filename in (
        ("wf", "walk_forward", "qualification.csv"),
        ("holdout_qualification", "holdout_evaluation", "holdout_metrics.csv"),
    ):
        stage = stage if stage in stages else "threshold_calibration" if group == "holdout_qualification" else stage
        if stage not in stages:
            continue
        try:
            path = evidence.runs / stages[stage] / "results" / filename
            table = evidence.frame(path, _stage_expected(owner, stage, f"results/{filename}"))
            for model in models:
                # Parent-only removed evidence must not be replaced with a
                # different derivative's qualification table.
                if model.get(group) or model.get("origin") == "removed" or not model.get("set"):
                    continue
                rows = _select(table, model["set"], model.get("direction") if group == "holdout_qualification" else None)
                if len(rows) == 1:
                    model[group] = {key: None if pd.isna(value) else value for key, value in rows.iloc[0].to_dict().items()}
        except (OSError, ValueError, KeyError) as exc:
            evidence.unavailable[group] = str(exc)


def _stored_config(saved):
    # ExperimentSpec serializes this under rstock_config. Keep the older export
    # alias readable, without instantiating current configuration defaults.
    return saved.get("rstock_config", saved.get("config", {}))


def _context_exports(evidence, output, run_id, source_id, models, frames, metadata, saved):
    queue = []
    coverage = {}

    def discover(path, stage, expected=None, configuration=None):
        if path.exists():
            queue.append((path, expected, stage))
            return
        enabled = _stored_config(configuration or {}).get("market_context_enabled")
        reason = ("disabled_in_run_configuration" if enabled is False else
                  "historical_context_not_enabled" if enabled is None else "diagnostic_not_persisted")
        coverage[stage] = {"status": "unavailable", "reason": reason,
                           "market_context_enabled": enabled, "source_run_id": path.parent.parent.name}

    discover(output / DIAGNOSTIC_MANIFEST, "forward", configuration=saved)
    # A missing/failed Forward diagnostic must not hide valid upstream evidence.
    # Resolve physical inherited stages, rather than assuming a derivative owns
    # new WF/Holdout outputs or scanning unrelated runs.
    if source_id:
        owner = evidence.runs / str(source_id)
        try:
            evidence.document(owner / "orchestration/pipeline.json")
            stages = _stage_ids(owner)
            for key, stage in (("walk_forward", "walk_forward"),
                               ("threshold_calibration", "development_calibrated"),
                               ("holdout_evaluation", "holdout")):
                if key not in stages:
                    coverage[stage] = {"status": "unavailable", "reason": "source_stage_not_recorded"}
                    continue
                directory = evidence.runs / stages[key]
                config_path = directory / "config.json"
                configuration = evidence.document(config_path) if config_path.exists() else {}
                discover(directory / "results" / DIAGNOSTIC_MANIFEST, stage,
                         _stage_expected(owner, key, "results/" + DIAGNOSTIC_MANIFEST), configuration)
        except (OSError, ValueError, KeyError) as exc:
            evidence.unavailable["context_stage_discovery"] = str(exc)
    seen = set()
    contexts, metrics, robustness = [], [], []
    while queue:
        path, expected, stage = queue.pop(0)
        if path.resolve() in seen:
            continue
        seen.add(path.resolve())
        label = path.parent.parent.name
        try:
            manifest = evidence.document(path, expected)
            if manifest.get("status") != "available":
                evidence.unavailable[f"context:{label}"] = manifest.get("reason", "unavailable")
                coverage[stage] = {"status": "unavailable", "reason": manifest.get("reason", "unavailable"), "source_run_id": label}
                continue
            if manifest.get("stage") != stage:
                raise ValueError("export_context_stage_mismatch")
            context_path = evidence.runs / manifest["context_manifest"]
            context_meta = evidence.document(context_path, manifest["context_manifest_sha256"])
            if (context_meta.get("protocol_id") != manifest["protocol_id"] or
                    context_meta.get("revision") != manifest["context_revision"]):
                raise ValueError("export_context_revision_mismatch")
            for name, sha in manifest["artifact_digests"].items():
                evidence.read(path.parent / name, sha)
            for name, sha in context_meta["artifact_digests"].items():
                evidence.read(context_path.parent / name, sha)
            load_context_diagnostic(path.parent, evidence.runs)  # validate immutable evidence
            # Read identifiers as strings (including numeric IDs with leading zeros).
            context = evidence.frame(context_path.parent / "market_context.csv")
            table = evidence.frame(path.parent / METRICS_FILE)
            summary = evidence.frame(path.parent / ROBUSTNESS_FILE)
            # Export physical stage and revision keys: bands from different
            # development references must never be silently pooled.
            keys = {"context_protocol_id": manifest["protocol_id"], "context_revision": manifest["context_revision"], "context_reference_start": context_meta["reference_start"], "context_reference_end_exclusive": context_meta["reference_end_exclusive"]}
            contexts.append(context.assign(**keys))
            canonical_to_id = {_identity(m)["canonical_combination_id"]: m.get("source_model_id") for m in models}
            canonical_to_origin = {_identity(m)["canonical_combination_id"]: m.get("origin", "unavailable") for m in models}
            for source, destination in ((table, metrics), (summary, robustness)):
                source = source.loc[source.model_key.isin(canonical_to_id)].copy()
                source["canonical_combination_id"] = source["model_key"]
                source["source_model_id"] = source.model_key.map(canonical_to_id)
                source["origin"] = source.model_key.map(canonical_to_origin)
                source["context_source_run_id"] = label
                destination.append(_annotate(source.assign(**keys), models, run_id=run_id, source_id=source_id))
            metadata.append({"source_run_id": label, "diagnostic": manifest, "context": context_meta})
            coverage[stage] = {"status": "available", "source_run_id": label,
                               "protocol_id": manifest["protocol_id"], "context_revision": manifest["context_revision"]}
            for key, reference in manifest.get("stage_references", {}).items():
                reference_stage = {"threshold_calibration": "development_calibrated", "holdout_evaluation": "holdout"}.get(key, key)
                reference_path = evidence.runs / reference["path"]
                try:
                    evidence.read(reference_path, reference["sha256"])
                except (OSError, ValueError) as exc:
                    reference_label = reference_path.parent.parent.name
                    evidence.unavailable[f"context:{reference_label}"] = str(exc)
                    coverage[reference_stage] = {"status": "unavailable", "reason": str(exc), "source_run_id": reference_label}
                    seen.add(reference_path.resolve())
                    continue
                # Prefer the recorded digest over an unverified discovery of
                # the same physical stage through a historical pipeline.
                queue.insert(0, (reference_path, reference["sha256"], reference_stage))
        except (OSError, ValueError, KeyError) as exc:
            evidence.unavailable[f"context:{label}"] = str(exc)
            coverage[stage] = {"status": "unavailable", "reason": str(exc), "source_run_id": label}
    for name, values in (("market_context.csv", contexts), ("context_metrics.csv", metrics), ("context_robustness.csv", robustness)):
        if values:
            frame = pd.concat(values, ignore_index=True)
            frames[name] = frame.drop_duplicates() if name == "market_context.csv" else frame
    if not contexts:
        evidence.unavailable["context"] = coverage["forward"].get("reason", "context_evidence_unavailable")
    return coverage


def _readme(frames, unavailable):
    descriptions = {
        "modeles_t0.csv": "Une ligne par candidat simulé ou retiré : identité, seuils et références T0 séparées.",
        "performances_forward.csv": "Métriques persistées, par scope/modèle/type de période/horizon, enrichies des diagnostics disponibles.",
        "observations_forward.csv": "Toutes les observations évaluées, y compris sans signal ; aucune matrice de features.",
        "exclusions_forward.csv": "Observations exclues, raisons et champs invalides.",
        "evolution_quotidienne.csv": "Séries quotidiennes persistées des modèles et du run.",
        "population_forward.csv": "Statistiques persistées de population ; les horizons désignent les intervalles définis par leurs bornes.",
        "calibration.csv": "Tranches de probabilités Holdout comparable et Forward ; populations et périodes distinctes.",
        "market_context.csv": "Contexte SPY par séance et révision ; une séance peut apparaître dans plusieurs révisions.",
        "context_metrics.csv": "Agrégats par étape WF/développement/Holdout/Forward, fenêtre, contexte et période.",
        "context_robustness.csv": "Synthèses de robustesse descriptives persistées, avec support et disponibilité.",
    }
    text = """# Analyse Forward — export de données persistées

Export de tous les candidats et de toutes les périodes disponibles, indépendant
des filtres et sélections de l'interface. Aucun recalcul scientifique ni téléchargement.
Les données absentes ne sont pas remplacées par zéro. Les colonnes techniques
conservent leurs noms, identifiants et valeurs d'origine. CSV UTF-8, séparateur
virgule, point décimal, dates ISO ; listes/dictionnaires encodés en JSON dans les cellules.

## Jointures et populations

- `forward_run_id` identifie cette simulation ; `source_e2e_run_id` sa source.
- `source_model_id` identifie le modèle figé ; `canonical_combination_id` son
  identité scientifique. La présence dans une autre simulation ne prouve pas
  l'égalité des boosters ou des seuils. Ne pas renuméroter les identifiants.
- Périodes : (`forward_run_id`, `scope`, `source_model_id`, `period_kind`, `horizon`).
  Observations : (`forward_run_id`, `source_model_id`, `session_date`).
- `scope=run` a une clé modèle vide : il s'agit d'un total, pas d'un candidat.
- `full_run` conserve la fin réelle, même personnalisée. Tous les horizons
  persistés, y compris +84/+126, sont exportés. Aucun intervalle final manquant
  n'est inventé. Cumul, intervalles et totalité se recouvrent : ne pas les sommer.
- Origines : normal, common, additional, removed, unavailable. Un candidat
  retiré figure à T0 ; il ne possède pas de Forward dans ce dérivé.

## Métriques et unités

- `wf__*` : qualification WF persistée ; l'AUC agrégée peut être une médiane
  de fenêtres. Aucune AUC globale n'est déduite de cette médiane.
- `holdout_qualification__*` : métriques utilisées pour la qualification.
- `holdout_comparable__*` et `t0_*` des performances : Holdout comparable avec
  la règle combinée Up >= seuil Up et Down < seuil Down. Ce n'est pas le WF.
- `stage=holdout_comparable` dans calibration.csv désigne uniquement cette référence.
- `precision`/`mean_return` : succès de la cible/rendement intraday moyen sur
  les signaux. AUC/Brier : toutes les observations évaluables, pas les seuls signaux.
- AUC, probabilités, précision et taux sont des proportions (0.33 = 33 %).
  Rendements et deltas de rendement sont décimaux (0.01 = 1 %).
- `pnl`, `drawdown`, `cumulative_pnl` : montants monétaires du protocole,
  drawdown positif depuis le sommet ; pas des pourcentages de capital.
  Le notionnel par signal est indiqué dans le résumé du manifest s'il est persisté.
  Aucun capital fixe, coût de transaction ou rendement de portefeuille n'est déduit.
- `delta_*` = Forward moins Holdout comparable, selon la période de la ligne.
  Brier croissant = calibration moins bonne ; AUC/précision/rendement croissants
  généralement favorables. Le Brier dépend aussi de la prévalence.
- `positives`/`negatives` : classes réalisées ; `signals` : décisions positives.
  Pas de précision sans signal, pas d'AUC avec une seule classe.
- Les champs de concentration et les moyennes pondérées/médianes conservent
  leur définition source ; ne pas confondre pondération par signal et par modèle.
- Contexte SPY ajusté connu à J−1 ; terciles descriptifs, pas des régimes
  économiques officiels. Joindre aussi `context_protocol_id`, `context_revision`,
  l'étape et `Window`. Vérifier les coupures et périodes de référence dans le manifest
  avant de comparer des bandes. Ne pas joindre directement des agrégats par bande
  aux observations, ce qui multiplierait les lignes.
- Dans les CSV de contexte, `stage=holdout` avec la règle
  `frozen_combined_up_down` désigne le Holdout comparable, pas la qualification.
  `walk_forward`, `development_calibrated` et `forward` restent distincts.
  `context_coverage` dans manifest.json décrit les étapes disponibles, désactivées
  ou absentes. SPY parmi les prédicteurs n'active pas ce diagnostic optionnel.
- Les statuts d'indisponibilité et indicateurs de petits échantillons doivent
  accompagner les métriques. Les observations, fenêtres et horizons se chevauchent.
- Drift de features : non évalué. Les probabilités et le contexte SPY ne
  remplacent pas les matrices de prédicteurs réellement utilisées.

## Consigne proposée à GPT/Codex

Vérifie la couverture, les clés et les conventions avant l'analyse. Compare
séparément WF, Holdout de qualification, Holdout comparable et Forward. Analyse
les trajectoires et deltas à tous les horizons, calibration, rendement,
concentration et cohortes de qualification. Examine le contexte disponible et
les effectifs. Distingue les indices de faux positifs/faux négatifs des preuves,
les associations des causes, et signale toute donnée insuffisante. N'invente ni
métrique absente, ni performance pour les candidats retirés, ni diagnostic de drift.

## Fichiers et colonnes
"""
    for name, frame in frames.items():
        text += f"\n### {name}\n{descriptions.get(name, '')}\n\nColonnes : " + ", ".join(f"`{c}`" for c in frame.columns) + ".\n"
    text += "\n## Données indisponibles\n" + (json.dumps(unavailable, ensure_ascii=False, indent=2) if unavailable else "Aucune absence signalée.") + "\n"
    return text


def build_forward_export(output: Path) -> bytes:
    """Build in memory on click, never persist ZIPs or materialize diagnostics."""
    output = Path(output).resolve()
    runs, run_id = output.parent.parent, output.parent.name
    evidence = _Evidence(runs)
    analysis = evidence.document(output / ANALYSIS) if (output / ANALYSIS).exists() else {}
    if analysis and analysis.get("forward_run_id") != run_id:
        raise ValueError("export_forward_run_mismatch")
    saved = evidence.document(output.parent / "config.json") if (output.parent / "config.json").exists() else {}
    source_id = analysis.get("source_e2e_run_id") or saved.get("source_end_to_end_run")
    if not source_id:
        evidence.unavailable["source_e2e_run_id"] = "source_not_recorded"
    frames = {}
    for source_name, name in CORE_FILES.items():
        try:
            frames[name] = evidence.frame(output / source_name, analysis.get("artifact_digests", {}).get(source_name))
        except FileNotFoundError:
            evidence.unavailable[name] = "source_missing_or_purged"
    if not frames:
        raise ValueError("export_forward_data_unavailable")
    reference, diagnostic, bins = {}, pd.DataFrame(), pd.DataFrame()
    try:
        if (output / MANIFEST).exists():
            manifest = evidence.document(output / MANIFEST, analysis.get("diagnostic", {}).get("sha256"))
            if manifest.get("protocol") != PROTOCOL or manifest.get("forward_run_id") != run_id or manifest.get("source_e2e_run_id") != source_id:
                raise ValueError("export_diagnostic_source_mismatch")
            ref = manifest["t0_reference"]
            reference = evidence.document(runs / ref["path"], ref["sha256"])
            if (reference.get("source_e2e_run_id") != source_id or
                analysis.get("source_snapshot_sha256") and reference.get("source_snapshot_sha256") != analysis["source_snapshot_sha256"]):
                raise ValueError("export_t0_source_mismatch")
            try:
                if (set(manifest["input_digests"]) != {"forward_observations.csv", "forward_period_metrics.csv", "forward_exclusions.csv"}
                    or set(manifest["artifact_digests"]) != {METRICS, BINS}):
                    raise ValueError("export_diagnostic_manifest_incomplete")
                for name, sha in {**manifest["input_digests"], **manifest["artifact_digests"]}.items():
                    evidence.read(output / name, sha)
                diagnostic = evidence.frame(output / METRICS, manifest["artifact_digests"][METRICS])
                bins = evidence.frame(output / BINS, manifest["artifact_digests"][BINS])
            except (OSError, ValueError, KeyError) as exc:
                evidence.unavailable["forward_diagnostic_metrics"] = str(exc)
        else:
            evidence.unavailable["t0_diagnostic"] = "diagnostic_not_persisted"
    except (OSError, ValueError, KeyError) as exc:
        reference, diagnostic, bins = {}, pd.DataFrame(), pd.DataFrame()
        evidence.unavailable["t0_diagnostic"] = str(exc)
    snapshot_models, snapshot = [], {}
    if source_id:
        try:
            snapshot = evidence.document(runs / str(source_id) / "results/forward_model_snapshot.json", analysis.get("source_snapshot_sha256"))
            if snapshot.get("source_end_to_end_run_id") != source_id:
                raise ValueError("export_snapshot_source_mismatch")
            snapshot_models = snapshot.get("models", [])
        except (OSError, ValueError) as exc:
            evidence.unavailable["model_snapshot"] = str(exc)
    models = [dict(model) for model in reference.get("models", [])]
    known = {str(m["source_model_id"]) for m in models if m.get("source_model_id") is not None}
    for model in snapshot_models:
        if str(model["source_model_id"]) not in known:
            models.append({**model, "forward_available": True})
            known.add(str(model["source_model_id"]))
    for frame in frames.values():
        if "source_model_id" in frame:
            for model_id in frame.source_model_id.dropna().unique():
                if model_id not in known:
                    models.append({"source_model_id": model_id, "forward_available": True})
                    known.add(model_id)
    model_ids = [str(m["source_model_id"]) for m in models if m.get("source_model_id") is not None]
    if len(model_ids) != len(set(model_ids)):
        raise ValueError("export_duplicate_model_identity")
    if not reference:
        _qualification_fallback(evidence, source_id, models)
    periods = frames.get("performances_forward.csv")
    if periods is not None and not diagnostic.empty:
        additions = [name for name in diagnostic if name not in periods or name in JOIN_KEYS]
        if periods.duplicated(["scope", *JOIN_KEYS]).any() or diagnostic.duplicated(JOIN_KEYS).any():
            raise ValueError("export_duplicate_period_key")
        # Never join population totals to model diagnostics or overwrite economics.
        diagnostics = diagnostic[additions].assign(scope="model")
        frames["performances_forward.csv"] = periods.merge(diagnostics, on=["scope", *JOIN_KEYS], how="left", validate="one_to_one")
    frames = {name: _annotate(frame, models, run_id=run_id, source_id=source_id) for name, frame in frames.items()}
    frames["modeles_t0.csv"] = _t0_rows(models, snapshot_models, run_id, source_id, reference,
        reference.get("cutoff_t0") or snapshot.get("resolved_market_session_cutoff") or analysis.get("cutoff_t0"))
    calibration = []
    if not bins.empty:
        calibration.append(_annotate(bins.assign(stage="forward"), models, run_id=run_id, source_id=source_id))
    for model in models:
        buckets = (model.get("holdout_comparable") or {}).get("probability_bins", [])
        if buckets:
            calibration.append(_annotate(pd.DataFrame(buckets).assign(source_model_id=model.get("source_model_id"),
                canonical_combination_id=_identity(model)["canonical_combination_id"],
                target=model.get("target"), direction=model.get("direction"), origin=model.get("origin", "unavailable"),
                stage="holdout_comparable", period_kind="holdout_reference", horizon=None), models, run_id=run_id, source_id=source_id))
    if calibration:
        frames["calibration.csv"] = pd.concat(calibration, ignore_index=True)
    else:
        evidence.unavailable["calibration.csv"] = "probability_bins_not_persisted"
    context_metadata = []
    context_coverage = _context_exports(evidence, output, run_id, source_id, models, frames, context_metadata, saved)
    summary = {}
    if (output.parent / "summary.json").exists():
        summary = evidence.document(output.parent / "summary.json")
    config = {}
    if (output.parent / "config.json").exists():
        config = {name: saved[name] for name in (
            "forward_policy", "forward_simulation_start_date", "forward_simulation_end_date", "calendar",
            "source_end_to_end_run", "source_forward_model_snapshot_sha256", "derivation",
        ) if name in saved}
        config["config"] = {name: value for name, value in _stored_config(saved).items() if name != "project_root"}
    manifest = {
        "export_schema_version": 1, "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "forward_run_id": run_id, "source_e2e_run_id": source_id,
        "parent_e2e_run_id": reference.get("parent_e2e_run_id"),
        "scope": "all_candidates_all_persisted_periods_independent_of_ui_filters",
        "feature_drift": "not_evaluated", "analysis": analysis,
        "t0_provenance": {name: value for name, value in reference.items() if name != "models"},
        "configuration": config, "summary": summary, "context_provenance": context_metadata,
        "context_coverage": context_coverage,
        "sources": evidence.sources, "unavailable": evidence.unavailable, "files": {},
    }
    payload = io.BytesIO()
    with ZipFile(payload, "w", compression=ZIP_DEFLATED) as archive:
        for name, frame in frames.items():
            raw = frame.to_csv(index=False).encode("utf-8")
            archive.writestr(name, raw)
            manifest["files"][name] = {"rows": len(frame), "columns": list(frame.columns), "sha256": _hash(raw)}
        readme = _readme(frames, evidence.unavailable).encode("utf-8")
        archive.writestr("README.md", readme)
        manifest["files"]["README.md"] = {"sha256": _hash(readme)}
        evidence.unchanged()
        archive.writestr("manifest.json", json.dumps(manifest, ensure_ascii=False, indent=2, allow_nan=False).encode("utf-8"))
    return payload.getvalue()


def render_forward_export(st, output: Path):
    """Callable data is executed by Streamlit only when downloading; no session cache."""
    output = Path(output)
    st.download_button(
        "Exporter l’analyse Forward (.zip)", data=lambda: build_forward_export(output),
        file_name=f"analyse_forward_{output.parent.name}.zip", mime="application/zip",
        key=f"forward-export-{output.parent.name}", on_click="ignore",
        disabled=not any((output / name).is_file() for name in CORE_FILES),
        help="Tous les candidats et toutes les périodes conservées, avec références T0 et contexte disponible. Indépendant des filtres et sélections.",
    )



def build_selection_export(run: Path) -> bytes:
    """Compact E2E diagnostic package using the existing evidence/ZIP mechanism.

    Preserve original bytes/relative paths; exclude raw predictions, models,
    checkpoint payloads and duplicate per-model UI partitions.
    """
    from .selection_diagnostic import MANIFEST as SELECTION, FILES, FORWARD_FILES
    run=Path(run).resolve();evidence=_Evidence(run.parent)
    meta=evidence.document(run/"results"/SELECTION)
    if meta.get("source_e2e_run_id")!=run.name: raise ValueError("export_selection_source_mismatch")
    included={}
    source_expectations={Path(name).as_posix():sha for name,sha in meta.get("sources",{}).items()}
    def add(path,expected=None):
        path=Path(path)
        if not path.exists():
            evidence.unavailable[str(path.relative_to(evidence.runs))]="missing_or_purged";return
        relative=path.resolve().relative_to(evidence.runs).as_posix()
        expected=expected or source_expectations.get(relative)
        key="runs/"+relative
        if key not in included: included[key]=evidence.read(path,expected)
    def identity(owner):
        for name in ("config.json","metadata.json","status.json","summary.json","storage.json","orchestration/pipeline.json"):
            if (owner/name).exists(): add(owner/name)
    def context(directory):
        path=directory/DIAGNOSTIC_MANIFEST
        if not path.exists(): return
        document=evidence.document(path);add(path)
        for name,sha in document.get("artifact_digests",{}).items(): add(directory/name,sha)
        if document.get("status")=="available":
            ref=evidence.runs/document["context_manifest"]
            context_meta=evidence.document(ref,document["context_manifest_sha256"])
            add(ref,document["context_manifest_sha256"])
            for name,sha in context_meta.get("artifact_digests",{}).items(): add(ref.parent/name,sha)
    identity(run);add(run/"results"/SELECTION)
    for name in FILES: add(run/"results"/name,meta.get("artifact_digests",{}).get(name))
    for name in ("forward_model_snapshot.json","forward_t0_reference.json","pipeline_summary.json"):
        if (run/"results"/name).exists(): add(run/"results"/name)
    for stage,rid in meta.get("stage_run_ids",{}).items():
        owner=evidence.runs/rid;identity(owner);context(owner/"results")
        # Scientific windows are compact compared with the prediction population.
        names=("windows.csv","qualification.csv","selection_results.csv","predictor_prefilter.csv","prefilter_qualification.csv") if stage=="walk_forward" else ("predictor_prefilter.csv","prefilter_qualification.csv","prefilter_contract.json","prefilter_stability.csv") if stage=="prefilter" else ("qualification.json",) if stage=="promotion_qualification" else ("holdout_metrics.csv",) if stage=="holdout_evaluation" else ("selected_thresholds_by_set.json",) if stage=="threshold_calibration" else ()
        for name in names:
            if (owner/"results"/name).exists(): add(owner/"results"/name)
    saved=_json_export(run/"config.json")
    parent=(saved.get("derivation") or {}).get("source_end_to_end_run_id")
    visited={run.name}
    while parent and parent not in visited:
        visited.add(parent);owner=evidence.runs/parent;identity(owner)
        saved=_json_export(owner/"config.json")
        parent=(saved.get("derivation") or {}).get("source_end_to_end_run_id")
    index_path=run/"results/selection_forward_index.json"
    if index_path.exists():
        index=evidence.document(index_path);add(index_path)
        for rid,ref in index.get("runs",{}).items():
            path=evidence.runs/ref["manifest"]
            forward=evidence.document(path,ref["sha256"])
            source_expectations.update({Path(name).as_posix():sha for name,sha in forward.get("sources",{}).items()})
            if forward.get("source_e2e_run_id")!=run.name: raise ValueError("export_forward_source_mismatch")
            add(path,ref["sha256"]);identity(path.parent.parent)
            for name,sha in forward.get("artifact_digests",{}).items(): add(path.parent/name,sha)
            for name in (ANALYSIS,MANIFEST,"forward_summary.json","forward_daily_metrics.csv","forward_period_metrics.csv","forward_population_metrics.csv",METRICS,BINS):
                if (path.parent/name).exists(): add(path.parent/name)
            context(path.parent)
    export_manifest={"schema_version":1,"kind":"compact_selection_diagnostic","source_e2e_run_id":run.name,
        "scope":"all_persisted_diagnostic_candidates_and_linked_forward_periods_independent_of_ui_filters",
        "missing":meta.get("missing",{}),"limitations":meta.get("limitations",[]),"unavailable":evidence.unavailable,
        "files":{key:{"size_bytes":len(raw),"sha256":_hash(raw)} for key,raw in included.items()},
        "sources":evidence.sources,"excluded":"raw predictions, binary snapshots, boosters, caches and duplicated UI partitions"}
    payload=io.BytesIO()
    with ZipFile(payload,"w",compression=ZIP_DEFLATED) as archive:
        for name,raw in included.items(): archive.writestr(name,raw)
        evidence.unchanged()
        archive.writestr("export_manifest.json",json.dumps(export_manifest,ensure_ascii=False,indent=2).encode("utf-8"))
    return payload.getvalue()


def _json_export(path):
    # Only used for ancestor discovery; all exported identities are separately
    # verified through _Evidence and rechecked before ZIP completion.
    try: return json.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError,ValueError): return {}
