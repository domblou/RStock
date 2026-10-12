"""Historical persistence of Up relations on archived, retrained WF predictions."""
from __future__ import annotations

from dataclasses import dataclass
from functools import lru_cache
import hashlib
import json
import pickle
from math import erfc, sqrt
from pathlib import Path

import numpy as np
import pandas as pd

from rstock.evaluation import probability_losses
from rstock.features import intraday_target_column, intraday_down_target_column
from rstock.progress import check_cancellation
from rstock.checkpoints import _atomic_json

ANALYSIS_ID = "rare_relations_persistence"
VERSION = "3"
RULES = {"direction": "Up", "minimum_sessions": 100, "minimum_class_count": 20,
         "minimum_double_lag_blocks": 10, "minimum_relative_gain": .01,
         "alpha": .05, "coverage_fraction": 1., "hac_lags": "ceil(n**(1/3)), twice",
         "multiplicity": "Holm", "evaluation_segments": 3,
         "minimum_positive_segments": 2, "minimum_gain_without_best_segment": 0.,
         "thresholds_empirically_validated": False}
REQUIRED_ARTIFACTS = (
    "results/windows.csv", "results/predictions.csv",
    "checkpoints/artifacts/prepared_snapshot.pkl",
    "checkpoints/artifacts/prepared_snapshot.json",
)


def validate_parameters(values: dict) -> dict:
    # Historical manifests remain readable as stored. They cannot silently
    # acquire the new session-based semantics on execution or restart.
    allowed = {"selection_mode", "selection_fraction", "selection_sessions", "resolved_split", "rules"}
    if set(values) - allowed:
        raise ValueError("Paramètres inconnus pour l'analyse de persistance")
    mode = values.get("selection_mode", "proportion")
    result = {"selection_mode": mode}
    if mode == "proportion":
        if "selection_sessions" in values:
            raise ValueError("Les deux modes de sélection sont mutuellement exclusifs")
        fraction = values.get("selection_fraction", .6)
        if isinstance(fraction, bool) or not isinstance(fraction, (int, float)) or not 0 < fraction < 1:
            raise ValueError("La proportion de sélection doit être strictement entre 0 et 100 %")
        result["selection_fraction"] = float(fraction)
    elif mode == "sessions":
        if "selection_fraction" in values:
            raise ValueError("Les deux modes de sélection sont mutuellement exclusifs")
        count = values.get("selection_sessions")
        if isinstance(count, bool) or not isinstance(count, int) or count < 1:
            raise ValueError("Le nombre de séances de sélection doit être un entier positif")
        result["selection_sessions"] = count
    else:
        raise ValueError("Mode de sélection inconnu")
    if "resolved_split" in values:
        result["resolved_split"] = values["resolved_split"]
    if "rules" in values and values["rules"] != RULES:
        raise ValueError("Les règles scientifiques figées ne correspondent plus à la définition")
    result["rules"] = dict(RULES)
    return result


def preflight(source, parameters: dict, cancellation_check=None) -> dict:
    parameters = validate_parameters(parameters)
    calendar = source_calendar(source.directory, cancellation_check)
    split = resolve_split(calendar, parameters)
    return {"ready": True, "reasons": [], "resolved_parameters": {**parameters, "resolved_split": split},
            "temporal_summary": split, "warnings": split["warnings"]}


def source_calendar(directory: Path, cancellation_check=None) -> dict:
    """Inspect archived rows in blocks, once per source file revision.

    The union is a shared date axis, not an assertion of complete coverage.
    No probability loss, selection or statistical test is calculated here.
    """
    directory = Path(directory).resolve()
    signature = tuple((name, (directory / name).stat().st_size,
                       (directory / name).stat().st_mtime_ns) for name in REQUIRED_ARTIFACTS)
    result = (_read_calendar.__wrapped__(str(directory), signature, cancellation_check)
              if cancellation_check else _read_calendar(str(directory), signature))
    after = tuple((name, (directory / name).stat().st_size,
                   (directory / name).stat().st_mtime_ns) for name in REQUIRED_ARTIFACTS)
    if after != signature:
        raise ValueError("Les artefacts WF ont changé pendant la lecture du calendrier")
    return result


@lru_cache(maxsize=2)
def _read_calendar(directory: str, signature: tuple, cancellation_check=None) -> dict:
    root = Path(directory)
    check_cancellation(cancellation_check)
    frame = load_snapshot(root / REQUIRED_ARTIFACTS[2], root / REQUIRED_ARTIFACTS[3])
    windows = pd.read_csv(root / REQUIRED_ARTIFACTS[0])
    required = {"Set", "Window", "Observation", "FeatureColumns", "TestStart", "TestEnd", "TestObservations"}
    if windows.empty or not required.issubset(windows) or windows.duplicated(["Set", "Window"]).any():
        raise ValueError("Fenêtres WF absentes, incomplètes ou dupliquées")
    if windows.groupby("Set").Observation.nunique().ne(1).any():
        raise ValueError("Cible du candidat incohérente")
    starts, ends = pd.to_datetime(windows.TestStart), pd.to_datetime(windows.TestEnd)
    if starts.isna().any() or ends.isna().any() or (starts > ends).any():
        raise ValueError("Bornes des fenêtres WF invalides")
    dates = frame.index[(frame.index >= starts.min()) & (frame.index <= ends.max())]
    candidates = windows.Set.drop_duplicates().tolist()
    positions = {candidate: i for i, candidate in enumerate(candidates)}
    present = np.zeros((len(candidates), len(dates)), dtype=bool)
    contracts = {(r.Set, int(r.Window)): r for r in windows.itertuples()}
    observed = {key: 0 for key in contracts}
    columns = ["Set", "Window", "Date", "Observation", "UpProbability", "IntradayTarget"]
    for chunk in pd.read_csv(root / REQUIRED_ARTIFACTS[1], usecols=columns, chunksize=65536):
        check_cancellation(cancellation_check)
        if chunk.duplicated(["Set", "Date"]).any():
            raise ValueError("Prédictions candidate/séance dupliquées")
        for key, rows in chunk.groupby(["Set", "Window"], sort=False):
            check_cancellation(cancellation_check)
            contract = contracts.get((key[0], int(key[1])))
            if contract is None or not rows.Observation.eq(contract.Observation).all():
                raise ValueError("Prédiction sans fenêtre ou cible correspondante")
            actual = pd.DatetimeIndex(pd.to_datetime(rows.Date))
            ix = dates.get_indexer(actual)
            if (ix < 0).any() or (actual < pd.Timestamp(contract.TestStart)).any() or (actual > pd.Timestamp(contract.TestEnd)).any():
                raise ValueError("Prédiction hors du calendrier de sa fenêtre WF")
            ci = positions[key[0]]
            if present[ci, ix].any():
                raise ValueError("Prédictions candidate/séance dupliquées entre blocs")
            features = json.loads(contract.FeatureColumns)
            if not isinstance(features, list) or not all(isinstance(name, str) for name in features):
                raise ValueError("Colonnes de fenêtre invalides")
            label = intraday_target_column(str(contract.Observation))
            names = list(dict.fromkeys([*features, label, intraday_down_target_column(str(contract.Observation))]))
            values = frame.loc[actual, names]
            probability = rows.UpProbability.to_numpy(dtype=float)
            if values.isna().any().any() or not np.isfinite(probability).all() or ((probability < 0) | (probability > 1)).any():
                raise ValueError("Prédictions non évaluables dans le snapshot WF")
            if not np.array_equal(rows.IntradayTarget.to_numpy(), values[label].to_numpy()) or not np.isin(rows.IntradayTarget, [0, 1]).all():
                raise ValueError("Labels archivés différents du snapshot WF")
            present[ci, ix] = True
            observed[(key[0], int(key[1]))] += len(rows)
    if any(observed[key] > contract.TestObservations for key, contract in contracts.items()):
        raise ValueError("Effectifs archivés supérieurs aux fenêtres WF")
    occupied = present.any(axis=0)
    dates = dates[occupied]
    if len(dates) < 2:
        raise ValueError("Au moins deux séances évaluables sont nécessaires")
    present = present[:, occupied]
    present.setflags(write=False)
    return {"dates": dates, "coverage": present, "candidates": candidates,
            "incomplete_windows": sum(observed[key] < row.TestObservations for key, row in contracts.items()),
            "source_artifacts": list(REQUIRED_ARTIFACTS)}


def resolve_split(calendar: dict, parameters: dict) -> dict:
    """One chronological boundary for all candidates; never consult losses."""
    parameters = validate_parameters(parameters)
    dates = calendar["dates"]
    total = len(dates)
    count = (int(np.floor(total * parameters["selection_fraction"]))
             if parameters["selection_mode"] == "proportion" else parameters["selection_sessions"])
    if not 1 <= count < total:
        raise ValueError("La sélection et l'évaluation doivent chacune contenir au moins une séance")
    coverage = calendar["coverage"]
    incomplete = int((~coverage.all(axis=1)).sum())
    warnings = []
    if incomplete:
        warnings.append(f"{incomplete} relations ont une couverture incomplète ; la coupure commune reste inchangée.")
    if calendar["incomplete_windows"]:
        warnings.append(f"{calendar['incomplete_windows']} fenêtres contiennent moins de prédictions que leur effectif test archivé.")
    iso = lambda value: value.date().isoformat()
    split = {"wf_start": iso(dates[0]), "wf_end": iso(dates[-1]), "total_sessions": total,
             "selection_sessions": count, "selection_end": iso(dates[count - 1]),
             "evaluation_sessions": total - count, "evaluation_start": iso(dates[count]),
             "evaluation_end": iso(dates[-1]), "calendar_sha256": hashlib.sha256(
                 "\n".join(iso(value) for value in dates).encode()).hexdigest(),
             "source_artifacts": calendar["source_artifacts"], "warnings": warnings,
             "coverage": [{"candidate": candidate, "selection_sessions": int(coverage[i, :count].sum()),
                           "evaluation_sessions": int(coverage[i, count:].sum())}
                          for i, candidate in enumerate(calendar["candidates"])]}
    if "resolved_split" in parameters and parameters["resolved_split"] != split:
        raise ValueError("Le calendrier ou le découpage figé ne correspond plus à la source WF")
    return split


@dataclass(frozen=True)
class WindowReference:
    target: str
    probability: float
    test_positions: np.ndarray
    offset: int


def load_snapshot(path: Path, metadata_path: Path) -> pd.DataFrame:
    raw = path.read_bytes()
    metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
    if hashlib.sha256(raw).hexdigest() != metadata["sha256"]:
        raise ValueError("Empreinte du snapshot préparé incohérente")
    frame = pickle.loads(raw)["prepared"]
    if (not isinstance(frame, pd.DataFrame) or not isinstance(frame.index, pd.DatetimeIndex)
            or frame.index.has_duplicates or frame.index.hasnans or not frame.index.is_monotonic_increasing):
        raise ValueError("Calendrier du snapshot invalide")
    return frame


def build_references(frame: pd.DataFrame, windows: pd.DataFrame, cancellation_check=None):
    """Use each stored candidate's exact features, two-label mask and train bounds."""
    required = {"Set", "Window", "Observation", "FeatureColumns", "TrainStart", "TrainEnd",
                "TestStart", "TestEnd", "TrainObservations", "TestObservations"}
    if not required.issubset(windows.columns) or windows.duplicated(["Set", "Window"]).any():
        raise ValueError("Contrats de fenêtres manquants ou dupliqués")
    refs, offset = {}, 0
    for candidate, rows in windows.groupby("Set", sort=False):
        check_cancellation(cancellation_check)
        targets = rows.Observation.unique()
        if len(targets) != 1:
            raise ValueError("Cible du candidat incohérente")
        target = str(targets[0])
        labels = [intraday_target_column(target), intraday_down_target_column(target)]
        occupied = np.zeros(len(frame), dtype=bool)
        for row in rows.itertuples():
            names = json.loads(row.FeatureColumns)
            if not isinstance(names, list) or not all(isinstance(name, str) for name in names):
                raise ValueError("Colonnes du modèle invalides")
            data = frame[list(dict.fromkeys([*names, *labels]))].dropna()
            train = data.loc[pd.Timestamp(row.TrainStart):pd.Timestamp(row.TrainEnd)]
            test = data.loc[pd.Timestamp(row.TestStart):pd.Timestamp(row.TestEnd)]
            if (len(train) != row.TrainObservations or len(test) != row.TestObservations
                    or train.empty or test.empty or train.index.max() >= test.index.min()
                    or train.index.min() != pd.Timestamp(row.TrainStart)
                    or train.index.max() != pd.Timestamp(row.TrainEnd)
                    or test.index.min() != pd.Timestamp(row.TestStart)
                    or test.index.max() != pd.Timestamp(row.TestEnd)):
                raise ValueError("Dates ou effectifs du train/test incohérents")
            if not np.isin(train[labels[0]].to_numpy(), [0, 1]).all():
                raise ValueError("Labels d'apprentissage non binaires")
            positions = frame.index.get_indexer(test.index)
            if occupied[positions].any():
                raise ValueError("Fenêtres test chevauchantes pour un même candidat")
            occupied[positions] = True
            key = (candidate, int(row.Window))
            refs[key] = WindowReference(target, float(train[labels[0]].mean()), positions, offset)
            offset += len(test)
    return refs, offset


def score_chunk(chunk: pd.DataFrame, frame: pd.DataFrame, references: dict,
                seen: np.ndarray, selection_end: str, evaluation_end: str) -> pd.DataFrame:
    """Validate exact source rows, return paired losses without choosing candidates."""
    required = {"Set", "Window", "Date", "Observation", "UpProbability", "IntradayTarget"}
    if not required.issubset(chunk.columns) or chunk.duplicated(["Set", "Date"]).any():
        raise ValueError("Prédictions manquantes ou dupliquées")
    outputs = []
    for key, group in chunk.groupby(["Set", "Window"], sort=False):
        ref = references.get((key[0], int(key[1])))
        if ref is None or not group.Observation.eq(ref.target).all():
            raise ValueError("Prédiction sans contrat de fenêtre/cible")
        dates = pd.DatetimeIndex(pd.to_datetime(group.Date))
        indices = frame.index.get_indexer(dates)
        locations = np.searchsorted(ref.test_positions, indices)
        if ((indices < 0).any() or (locations >= len(ref.test_positions)).any()
                or not np.array_equal(ref.test_positions[locations], indices)):
            raise ValueError("Prédiction hors du calendrier test archivé")
        slots = ref.offset + locations
        if seen[slots].any():
            raise ValueError("Prédiction candidate/date déjà rencontrée")
        actual = group.IntradayTarget.to_numpy()
        expected = frame.iloc[indices][intraday_target_column(ref.target)].to_numpy()
        if not np.array_equal(actual, expected):
            raise ValueError("Labels archivés différents du snapshot")
        ll, br = probability_losses(actual, group.UpProbability.to_numpy())
        base_ll, base_br = probability_losses(actual, np.full(len(group), ref.probability))
        seen[slots] = True
        part = pd.DataFrame({"Set": key[0], "Target": ref.target, "Date": dates,
            "Window": int(key[1]), "Probability": group.UpProbability.to_numpy(), "Outcome": actual,
            "ConstantProbability": ref.probability, "LogLoss": ll, "Brier": br,
            "ConstantLogLoss": base_ll, "ConstantBrier": base_br,
            "GainLogLoss": base_ll - ll, "GainBrier": base_br - br})
        part["Period"] = np.where(part.Date <= pd.Timestamp(selection_end), "selection",
                         np.where(part.Date <= pd.Timestamp(evaluation_end), "evaluation", "outside"))
        outputs.append(part)
    return pd.concat(outputs, ignore_index=True) if outputs else pd.DataFrame()


def coverage_table(daily: pd.DataFrame) -> pd.DataFrame:
    """Coverage remains explicit; neither a missing row nor an exclusion is a zero."""
    counts = daily.groupby(["Set", "Target", "Period"]).Date.nunique().unstack(fill_value=0)
    counts = counts.rename(columns={"selection": "SelectionDates", "evaluation": "EvaluationDates"})
    for column in ("SelectionDates", "EvaluationDates"):
        if column not in counts:
            counts[column] = 0
    return counts.reset_index()


def execute(context) -> dict:
    parameters = validate_parameters(context.parameters)
    calendar = source_calendar(context.source.directory, context.cancellation_check)
    split = resolve_split(calendar, parameters)
    frame = load_snapshot(context.source.prepared_snapshot_path, context.source.prepared_metadata_path)
    windows = pd.read_csv(context.source.windows_path)
    refs, size = build_references(frame, windows, context.cancellation_check)
    dates, candidates = calendar["dates"], calendar["candidates"]
    positions = {candidate: i for i, candidate in enumerate(candidates)}
    # Four compact numeric arrays avoid retaining repeated strings and daily DataFrames.
    losses = np.full((len(candidates), len(dates), 4), np.nan)
    seen = np.zeros(size, dtype=bool)
    for chunk in pd.read_csv(context.source.predictions_path, chunksize=65536):
        check_cancellation(context.cancellation_check)
        daily = score_chunk(chunk, frame, refs, seen, split["selection_end"], split["evaluation_end"])
        for candidate, rows in daily.groupby("Set", sort=False):
            indices = dates.get_indexer(rows.Date)
            if (indices < 0).any():
                raise ValueError("Prédiction hors du calendrier commun")
            losses[positions[candidate], indices] = rows[["LogLoss", "ConstantLogLoss", "Brier", "ConstantBrier"]]
    targets = windows.groupby("Set").Observation.first().to_dict()
    outcomes = {target: frame.loc[dates, intraday_target_column(target)].to_numpy()
                for target in set(targets.values())}
    boundary = split["selection_sessions"]
    checkpoint = context.checkpoint
    if checkpoint.phase_is_completed("rare_selection"):
        selection = checkpoint.load_artifact("rare_selection")
        if set(selection.Set) != set(candidates):
            raise ValueError("Population sélectionnée incompatible avec la source")
    else:
        selection = period_table(candidates, targets, losses[:, :boundary],
                                 {t: y[:boundary] for t, y in outcomes.items()}, context.cancellation_check)
        selection["AdjustedP"] = holm(selection.P.to_numpy())
        selection["Selected"] = (selection.Admissible & (selection.RelativeGain >= RULES["minimum_relative_gain"])
                                 & (selection.AdjustedP <= RULES["alpha"]))
        selection["SelectionReason"] = selection.apply(lambda row: row.ExclusionReason if not row.Admissible else
            "; ".join(reason for condition, reason in ((row.RelativeGain < RULES["minimum_relative_gain"], "gain relatif inférieur à 1 %"),
                (row.AdjustedP > RULES["alpha"], "Holm non rejeté")) if condition), axis=1)
        selection = selection.sort_values(["RelativeGain", "AdjustedP", "Set"], ascending=[False, True, True])
        selection["Rank"] = np.nan
        selection.loc[selection.Selected, "Rank"] = np.arange(1, int(selection.Selected.sum()) + 1)
        checkpoint.commit_artifact("rare_selection", selection)
        checkpoint.phase_completed("rare_selection")
    selected = selection.loc[selection.Selected, "Set"].tolist()
    evaluation = period_table(selected, targets, losses[[positions[c] for c in selected], boundary:],
                              {t: y[boundary:] for t, y in outcomes.items()}, context.cancellation_check)
    evaluation["AdjustedP"] = holm(evaluation.P.to_numpy())
    for column in ("Segment1Gain", "Segment2Gain", "Segment3Gain", "GainWithoutBestSegment"):
        evaluation[column] = np.nan
    evaluation["DecisionReason"] = ""
    thirds = np.array_split(np.arange(len(dates) - boundary), 3)
    for candidate in selected:
        i = evaluation.index[evaluation.Set.eq(candidate)][0]
        gains = losses[positions[candidate], boundary:, 1] - losses[positions[candidate], boundary:, 0]
        segment_means = [float(np.nanmean(gains[part])) if np.isfinite(gains[part]).any() else np.nan for part in thirds]
        for j, mean in enumerate(segment_means, 1):
            evaluation.loc[i, f"Segment{j}Gain"] = mean
        remaining = np.concatenate([part for j, part in enumerate(thirds) if j != int(np.nanargmax(segment_means))]) if np.isfinite(segment_means).any() else np.array([], dtype=int)
        evaluation.loc[i, "GainWithoutBestSegment"] = float(np.nanmean(gains[remaining])) if np.isfinite(gains[remaining]).any() else np.nan
        sustained = (evaluation.loc[i, "Admissible"] and evaluation.loc[i, "RelativeGain"] >= RULES["minimum_relative_gain"]
                     and evaluation.loc[i, "AdjustedP"] <= RULES["alpha"]
                     and sum(mean > 0 for mean in segment_means) >= RULES["minimum_positive_segments"]
                     and evaluation.loc[i, "GainWithoutBestSegment"] > RULES["minimum_gain_without_best_segment"])
        evaluation.loc[i, "Decision"] = ("persistance historique soutenue" if sustained else
            "persistance non démontrée" if evaluation.loc[i, "Admissible"] else "indéterminé")
        evaluation.loc[i, "DecisionReason"] = (evaluation.loc[i, "ExclusionReason"] if not evaluation.loc[i, "Admissible"] else
            "; ".join(reason for condition, reason in (
                (evaluation.loc[i, "RelativeGain"] < RULES["minimum_relative_gain"], "gain relatif inférieur à 1 %"),
                (evaluation.loc[i, "AdjustedP"] > RULES["alpha"], "Holm non rejeté"),
                (sum(mean > 0 for mean in segment_means) < RULES["minimum_positive_segments"], "moins de deux segments positifs"),
                (not evaluation.loc[i, "GainWithoutBestSegment"] > RULES["minimum_gain_without_best_segment"], "gain sans meilleur segment non positif")) if condition))
    evaluation["Selected"] = True
    coverage = selection[["Set", "Target", "Sessions", "Coverage"]].rename(columns={"Sessions": "SelectionDates", "Coverage": "SelectionCoverage"})
    coverage["EvaluationDates"] = [int(calendar["coverage"][positions[c], boundary:].sum()) for c in coverage.Set]
    coverage["EvaluationCoverage"] = coverage.EvaluationDates / split["evaluation_sessions"]
    population = selection.merge(evaluation[["Set", "Decision"]], on="Set", how="left")
    target_summary = selection.groupby("Target").agg(Population=("Set", "size"), Selected=("Selected", "sum")).reset_index()
    for name, mask in (("Evaluable", evaluation.Admissible), ("Indeterminate", ~evaluation.Admissible),
                       ("Sustained", evaluation.Decision.eq("persistance historique soutenue"))):
        target_summary[name] = target_summary.Target.map(evaluation.loc[mask].groupby("Target").size()).fillna(0).astype(int)
    count = len(selected)
    sustained_count = int(evaluation.Decision.eq("persistance historique soutenue").sum())
    stop = ("aucune relation admissible" if not selection.Admissible.any() else "aucune relation sélectionnée" if not count else
            "aucune évaluation concluante" if not evaluation.Admissible.any() else None)
    result = {"complete": True, "definition_version": VERSION, "rules": dict(RULES), "temporal_summary": split,
              "population_count": len(candidates), "selected_count": count,
              "evaluable_count": int(evaluation.Admissible.sum()), "indeterminate_count": int((~evaluation.Admissible).sum()),
              "sustained_count": sustained_count, "sustained_fraction": sustained_count / count if count else None,
              "stop_reason": stop, "interpretation": "Preuve historique conditionnelle sur relations WF réentraînées; aucune validation indépendante Forward, rentabilité ou MVP.",
              "result_files": [f"{name}.csv" for name in ("selection", "persistence", "coverage", "targets", "population")] + ["scientific_rules.json"]}
    # The common workflow publishes/freezes these outputs only after source revalidation.
    for name, table in (("selection", selection), ("persistence", evaluation), ("coverage", coverage),
                        ("targets", target_summary), ("population", population)):
        check_cancellation(context.cancellation_check)
        table.to_csv(context.output / f"{name}.csv", index=False)
    _atomic_json(context.output / "scientific_rules.json", {"definition_version": VERSION, "rules": RULES,
                 "split": split, "selected_relations": selected, "source_sha256": context.source.artifacts})
    return result


def holm(pvalues: np.ndarray) -> np.ndarray:
    """Adjusted p-values for the entire family, including ineligible p=1."""
    pvalues = np.asarray(pvalues, dtype=float)
    if not np.isfinite(pvalues).all() or ((pvalues < 0) | (pvalues > 1)).any():
        raise ValueError("P-values invalides")
    order = np.argsort(pvalues, kind="stable")
    adjusted = np.empty(len(pvalues))
    adjusted[order] = np.minimum(1., np.maximum.accumulate(pvalues[order] * np.arange(len(pvalues), 0, -1)))
    return adjusted


def hac_test(gains: np.ndarray) -> dict:
    n = len(gains)
    lag = int(np.ceil(np.cbrt(n)))
    result = {"HACLag": lag, "P": 1., "InferenceAvailable": False,
              "SEL": np.nan, "SE2L": np.nan, "PL": np.nan, "P2L": np.nan}
    if not n or not np.isfinite(gains).all() or 2 * lag >= n:
        return result
    centered = gains - gains.mean()
    gamma0 = float(centered @ centered / n)
    ps = []
    for q, suffix in ((lag, "L"), (2 * lag, "2L")):
        variance = gamma0 + 2 * sum((1 - k / (q + 1)) * float(centered[k:] @ centered[:-k] / n) for k in range(1, q + 1))
        if not np.isfinite(variance) or variance <= 0:
            return result
        se = sqrt(variance / n)
        p = .5 * erfc(float(gains.mean()) / se / sqrt(2))
        result[f"SE{suffix}"], result[f"P{suffix}"] = se, p
        ps.append(p)
    result.update(P=max(ps), InferenceAvailable=True)
    return result


def period_table(candidates, targets, losses, outcomes, cancellation_check=None):
    rows = []
    for candidate, values in zip(candidates, losses):
        check_cancellation(cancellation_check)
        present = np.isfinite(values).all(axis=1)
        n = int(present.sum())
        y = outcomes[targets[candidate]][present]
        up, down = int((y == 1).sum()), int((y == 0).sum())
        gains = values[present, 1] - values[present, 0]
        inference = hac_test(gains) if present.all() else hac_test(np.array([]))
        lag = int(np.ceil(np.cbrt(n)))
        support = n >= RULES["minimum_sessions"] and min(up, down) >= RULES["minimum_class_count"] and n // (2 * max(lag, 1)) >= RULES["minimum_double_lag_blocks"]
        baseline = float(values[present, 1].mean()) if n else np.nan
        mean_gain = float(gains.mean()) if n else np.nan
        reasons = [reason for condition, reason in ((not present.all(), "couverture incomplète"),
                   (not support, "support insuffisant"), (not inference["InferenceAvailable"], "inférence indisponible"),
                   (not baseline > 0, "référence non positive")) if condition]
        admissible = not reasons
        rows.append({"Set": candidate, "Target": targets[candidate], "Sessions": n, "ExpectedSessions": len(values),
                     "Coverage": n / len(values), "UpCount": up, "DownCount": down,
                     "SupportAvailable": bool(support), "Admissible": admissible, "ExclusionReason": "; ".join(reasons),
                     "MeanLogLoss": float(values[present, 0].mean()) if n else np.nan,
                     "MeanConstantLogLoss": baseline, "MeanGainLogLoss": mean_gain,
                     "RelativeGain": mean_gain / baseline if baseline > 0 else np.nan,
                     "MeanBrier": float(values[present, 2].mean()) if n else np.nan,
                     "MeanConstantBrier": float(values[present, 3].mean()) if n else np.nan,
                     "MeanGainBrier": float((values[present, 3] - values[present, 2]).mean()) if n else np.nan,
                     **inference, "P": inference["P"] if admissible else 1.})
    columns = list(rows[0]) if rows else ["Set", "Target", "Sessions", "ExpectedSessions", "Coverage", "UpCount", "DownCount",
        "SupportAvailable", "Admissible", "ExclusionReason", "MeanLogLoss", "MeanConstantLogLoss", "MeanGainLogLoss",
        "RelativeGain", "MeanBrier", "MeanConstantBrier", "MeanGainBrier", "HACLag", "P", "InferenceAvailable", "SEL", "SE2L", "PL", "P2L"]
    table = pd.DataFrame(rows, columns=columns)
    table["Admissible"] = table.Admissible.astype(bool)
    if "Decision" not in table:
        table["Decision"] = ""
    return table


def render_parameters(key: str) -> dict:
    import streamlit as st
    st.caption("Up · gain relatif log loss ≥ 1 % · Holm 5 % · couverture complète · "
               "support minimal et HAC. Seuils proposés, non validés empiriquement; conclusion historique conditionnelle.")
    mode = st.radio("Définir la sélection", ["proportion", "sessions"],
        format_func=lambda value: "Proportion de séances" if value == "proportion" else "Nombre de séances",
        key=f"{key}_mode", horizontal=True)
    if mode == "proportion":
        percentage = st.number_input("Séances de sélection (%)", min_value=1., max_value=99.,
            value=60., step=1., key=f"{key}_fraction")
        return {"selection_mode": mode, "selection_fraction": float(percentage) / 100}
    count = st.number_input("Nombre de séances de sélection", min_value=1, value=1,
        step=1, key=f"{key}_sessions")
    return {"selection_mode": mode, "selection_sessions": int(count)}


def render_results(tab_id: str, run_id: str, detail: dict) -> None:
    import streamlit as st
    from .grid_dataframe import dataframe
    location = detail.get("run_directory", detail.get("directory"))
    if not location:
        st.warning("Dossier de résultats indisponible.")
        return
    root = Path(location)
    filename = {"persistence": "persistence.csv", "coverage": "coverage.csv", "selection": "selection.csv",
                "targets": "targets.csv", "population": "population.csv"}[tab_id]
    path = root / "results" / filename
    if not path.is_file():
        st.info("Résultats non publiés ; aucun recalcul n'est déclenché.")
        return
    preview = path.with_name(path.name + ".preview.json")
    if not preview.is_file() or preview.stat().st_size > 2 * 1024 * 1024 + 4096:
        st.info("Vue bornée indisponible pour cet ancien résultat ; aucune lecture complète dans l'interface.")
        return
    payload = json.loads(preview.read_text(encoding="utf-8"))
    data = pd.DataFrame(payload["data"], columns=payload["columns"])
    st.caption("Aperçu limité à 500 lignes ; le filtre porte uniquement sur cet aperçu.")
    if "Target" in data and not data.empty:
        target = st.selectbox("Cible", ["Toutes", *sorted(data.Target.dropna().unique())], key=f"analysis_{run_id}_{tab_id}_target")
        if target != "Toutes":
            data = data.loc[data.Target.eq(target)]
    st.caption("Résultats historiques conditionnels ; les relations et séances peuvent être dépendantes.")
    dataframe(data, key=f"analysis_{run_id}_{tab_id}", hide_index=True)
    if tab_id == "targets" and not data.empty and "MeanGainLogLoss" in data:
        plotted = data.loc[data.Selected.eq(True)] if "Selected" in data else data
        if not plotted.empty:
            st.bar_chart(plotted.set_index("Target")[["MeanGainLogLoss"]])


def definition():
    from .scientific_analysis import AnalysisDefinition
    from .domain import JobType
    from .run_detail_tabs import RunTabDefinition
    tabs = tuple(RunTabDefinition(key, label, frozenset({JobType.SCIENTIFIC_ANALYSIS}),
        "scientific_analysis_results", filename) for key, label, filename in (
            ("persistence", "Persistance", "persistence.csv"),
            ("selection", "Sélection", "selection.csv"),
            ("coverage", "Couverture", "coverage.csv"),
            ("targets", "Cibles", "targets.csv"),
            ("population", "Population", "population.csv")))
    return AnalysisDefinition(ANALYSIS_ID, VERSION, "Persistance des relations rares",
        "Persistance des relations sur les prédictions WF réentraînées ; sélection puis évaluation sur un calendrier commun.",
        REQUIRED_ARTIFACTS, validate_parameters, preflight, execute,
        render_parameters, tabs, render_results,
        worker_prepare=lambda source, parameters, cancel: preflight(source, parameters, cancel))
