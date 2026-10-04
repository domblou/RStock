"""Temporal aggregation confined to standalone Predictor prefilter runs."""

from __future__ import annotations

from collections.abc import Sequence

import numpy as np
import pandas as pd

from rstock.calendars import offset_market_session
from rstock.config import RStockConfig
from rstock.features import require_complete_last_session
from rstock.predictor_prefilter import filter_correlated_predictors


ORIGIN_METRICS = (
    "PrefilterScore", "ROCAUCMedian", "PctWindowsAboveRandom",
    "ROCAUCWorst", "ROCAUCStd", "Eligible", "PrefilterScoreRank",
    "PrefilterRank", "PrefilterStatus",
)


def resolve_stability_origins(
    prepared: pd.DataFrame, *, cutoff: str, calendar: str,
    origin_count: int, step_sessions: int, symbols: Sequence[str],
) -> tuple[pd.Timestamp, ...]:
    """Resolve J, J-step, ... on the calendar and require each prepared session."""
    if not isinstance(origin_count, int) or isinstance(origin_count, bool) or origin_count < 1:
        raise ValueError("stability_origin_count must be positive")
    if not isinstance(step_sessions, int) or isinstance(step_sessions, bool) or step_sessions < 1:
        raise ValueError("stability_step_sessions must be positive")
    available = set(pd.DatetimeIndex(prepared.index).normalize())
    origins: list[pd.Timestamp] = []
    for index in range(origin_count):
        try:
            origin = offset_market_session(cutoff, calendar, index * step_sessions)
        except (ValueError, IndexError, KeyError) as error:
            raise ValueError("Not enough calendar sessions for temporal stability") from error
        origin = pd.Timestamp(origin).normalize()
        if origin not in available:
            raise ValueError(f"Prepared snapshot lacks temporal origin {origin.date()}")
        require_complete_last_session(prepared.loc[:origin], symbols)
        origins.append(origin)
    return tuple(origins)


def aggregate_temporal_prefilter(
    origin_tables: Sequence[pd.DataFrame], *, targets: Sequence[str],
    predictors: Sequence[str], config: RStockConfig,
    principal_development: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, tuple[str, ...]]]:
    """Rank per-origin results deterministically, then apply Top-N and redundancy."""
    if not origin_tables:
        raise ValueError("Temporal prefilter requires at least one origin")
    dates = [str(table["OriginCutoff"].iloc[0]) for table in origin_tables]
    if len(set(dates)) != len(dates):
        raise ValueError("Temporal origins must be unique")
    detail = pd.concat(origin_tables, ignore_index=True)
    if detail.duplicated(["OriginCutoff", "Observation", "Predictor"]).any():
        raise ValueError("Duplicate temporal prefilter identity")
    expected = pd.DataFrame([
        (date, target, predictor)
        for date in dates for target in targets for predictor in predictors
        if predictor != target
    ], columns=["OriginCutoff", "Observation", "Predictor"])
    detail = expected.merge(
        detail, on=["OriginCutoff", "Observation", "Predictor"], how="left",
        validate="one_to_one",
    )
    detail["Eligible"] = detail["Eligible"].fillna(False).astype(bool)
    detail["PrefilterStatus"] = detail["PrefilterStatus"].fillna("missing_origin_result")
    for column in ("PrefilterScoreRank", "PrefilterRank", "PrefilterScore",
                   "ROCAUCMedian", "PctWindowsAboveRandom", "ROCAUCWorst", "ROCAUCStd"):
        detail[column] = pd.to_numeric(detail[column], errors="coerce")
    candidate_counts = detail.groupby(["OriginCutoff", "Observation"])["Predictor"].transform("size")
    detail["RankForAggregation"] = detail["PrefilterScoreRank"].fillna(candidate_counts + 1)
    detail["InOriginTopN"] = (
        detail["Eligible"] & detail["PrefilterRank"].le(config.predictor_prefilter_top_n)
    ).fillna(False).astype(bool)
    aggregate = detail.groupby(["Observation", "Predictor"], as_index=False).agg(
        MedianRank=("RankForAggregation", "median"),
        RankStd=("RankForAggregation", lambda values: float(np.std(values, ddof=0))),
        MedianScore=("PrefilterScore", "median"),
        EligibleFrequency=("Eligible", "mean"),
        TopNFrequency=("InOriginTopN", "mean"),
    )
    aggregate["AggregateRank"] = 0
    aggregate["PrefilterStatus"] = "rejected_ineligible"
    aggregate["SelectedAfterCorrelation"] = False
    aggregate["RedundantWith"] = pd.NA
    aggregate["RedundancyCorrelation"] = np.nan
    retained_by_target: dict[str, tuple[str, ...]] = {}
    for target in targets:
        target_rows = aggregate.loc[aggregate["Observation"] == target].sort_values(
            ["EligibleFrequency", "MedianRank", "RankStd", "MedianScore", "Predictor"],
            ascending=[False, True, True, False, True], kind="stable",
            na_position="last",
        )
        aggregate.loc[target_rows.index, "AggregateRank"] = range(1, len(target_rows) + 1)
        eligible = target_rows.loc[target_rows["EligibleFrequency"] > 0]
        aggregate.loc[eligible.index, "PrefilterStatus"] = "rejected_top_n"
        top = eligible.head(config.predictor_prefilter_top_n)
        aggregate.loc[top.index, "PrefilterStatus"] = "retained"
        retained, redundant = filter_correlated_predictors(
            top["Predictor"].astype(str).tolist(), principal_development,
            lag_depth=config.lag_depth,
            threshold=config.predictor_prefilter_correlation_threshold,
        )
        for index, row in top.iterrows():
            predictor = str(row["Predictor"])
            if predictor in redundant:
                aggregate.at[index, "PrefilterStatus"] = "removed_redundancy"
                aggregate.at[index, "RedundantWith"] = redundant[predictor][0]
                aggregate.at[index, "RedundancyCorrelation"] = redundant[predictor][1]
        aggregate.loc[
            (aggregate["Observation"] == target)
            & aggregate["Predictor"].isin(retained), "SelectedAfterCorrelation",
        ] = True
        retained_by_target[target] = tuple(retained)
    return (
        aggregate.sort_values(["Observation", "AggregateRank"], kind="stable").reset_index(drop=True),
        detail.sort_values(["OriginCutoff", "Observation", "Predictor"], kind="stable").reset_index(drop=True),
        retained_by_target,
    )
