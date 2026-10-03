"""Descriptive Forward measurements on XNYS sessions, without qualification rules."""

from __future__ import annotations

from typing import Any, Mapping, Sequence

import numpy as np
import pandas as pd


CHECKPOINTS = (21, 42, 63, 84, 126)
NOTIONAL_PER_SIGNAL = 10_000.0

PERIOD_COLUMNS = (
    "scope", "source_model_id", "target", "period_kind", "horizon",
    "interval_start", "interval_end", "session_start", "session_end",
    "expected_observations", "evaluated_observations", "excluded_observations",
    "signals", "precision", "mean_return", "pnl", "drawdown",
)
POPULATION_COLUMNS = (
    "horizon", "interval_start", "interval_end", "session_start", "session_end",
    "models_t0", "models_with_observations", "models_with_signals",
    "signals", "median_model_precision", "median_model_return",
    "weighted_precision", "weighted_return", "precision_q25", "precision_q75",
    "return_q25", "return_q75",
)
DAILY_COLUMNS = (
    "scope", "source_model_id", "target", "horizon", "session_date",
    "signals", "cumulative_signals", "cumulative_precision", "cumulative_pnl",
)


def _bool_column(frame: pd.DataFrame, name: str) -> pd.Series:
    if name not in frame:
        return pd.Series(False, index=frame.index)
    return frame[name].map(
        lambda value: value is True or str(value).strip().lower() in {"true", "1"}
    )


def _period_metrics(
    observations: pd.DataFrame,
    exclusions: pd.DataFrame,
    dates: pd.DatetimeIndex,
    *,
    scope: str,
    model_id: str,
    target: str,
    horizon: int,
    interval_start: int,
    period_kind: str,
    model_count: int,
) -> dict[str, Any]:
    start, end = dates[0], dates[-1]
    observed = observations.loc[
        observations["session_date"].between(start, end)
    ]
    excluded = exclusions.loc[
        exclusions["session_date"].between(start, end)
    ]
    signals = observed.loc[_bool_column(observed, "signal")]
    signal_count = len(signals)
    returns = pd.to_numeric(signals.get("directional_return"), errors="coerce")
    if signal_count and (returns.isna().any() or not np.isfinite(returns.to_numpy()).all()):
        raise ValueError("forward_analysis_invalid_signal_return")
    daily_profit = (
        pd.DataFrame({
            "session_date": signals["session_date"],
            "profit": returns * NOTIONAL_PER_SIGNAL,
        }).groupby("session_date")["profit"].sum().reindex(dates, fill_value=0.0)
        if signal_count else pd.Series(0.0, index=dates)
    )
    equity = pd.concat([pd.Series([0.0]), daily_profit.cumsum()], ignore_index=True)
    drawdown = float((equity.cummax() - equity).max())
    expected = model_count * len(dates)
    if len(observed) + len(excluded) != expected:
        raise ValueError("forward_analysis_incomplete_coverage")
    return {
        "scope": scope, "source_model_id": model_id, "target": target,
        "period_kind": period_kind, "horizon": horizon,
        "interval_start": interval_start, "interval_end": horizon,
        "session_start": start.date().isoformat(), "session_end": end.date().isoformat(),
        "expected_observations": expected, "evaluated_observations": len(observed),
        "excluded_observations": len(excluded), "signals": signal_count,
        "precision": float(_bool_column(signals, "correct_direction").mean()) if signal_count else None,
        "mean_return": float(returns.mean()) if signal_count else None,
        "pnl": float(daily_profit.sum()), "drawdown": drawdown,
    }


def analyze_forward_periods(
    observations: pd.DataFrame,
    exclusions: pd.DataFrame,
    models: Sequence[Mapping[str, Any]],
    sessions: pd.DatetimeIndex,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Build period, population and daily series from one immutable observation set."""

    dates = pd.DatetimeIndex(sessions).normalize().tz_localize(None)
    if not dates.is_monotonic_increasing or dates.has_duplicates:
        raise ValueError("forward_analysis_invalid_sessions")
    observed = observations.copy()
    excluded = exclusions.copy()
    model_ids = [str(model["source_model_id"]) for model in models]
    if len(set(model_ids)) != len(model_ids):
        raise ValueError("forward_analysis_duplicate_model")
    for frame in (observed, excluded):
        if "source_model_id" not in frame or "session_date" not in frame:
            raise ValueError("forward_analysis_missing_identity")
        frame["source_model_id"] = frame["source_model_id"].astype(str)
        frame["session_date"] = pd.to_datetime(frame["session_date"]).dt.normalize()
        if not frame["source_model_id"].isin(model_ids).all() or not frame["session_date"].isin(dates).all():
            raise ValueError("forward_analysis_outside_population_or_period")
        if frame.duplicated(["source_model_id", "session_date"]).any():
            raise ValueError("forward_analysis_duplicate_observation")
    observed_keys = set(zip(observed["source_model_id"], observed["session_date"]))
    excluded_keys = set(zip(excluded["source_model_id"], excluded["session_date"]))
    if observed_keys & excluded_keys or len(observed_keys | excluded_keys) != len(model_ids) * len(dates):
        raise ValueError("forward_analysis_incomplete_coverage")

    periods: list[dict[str, Any]] = []
    population: list[dict[str, Any]] = []
    daily: list[dict[str, Any]] = []
    checkpoint_horizons = [h for h in CHECKPOINTS if h <= len(dates)]
    scopes = [("run", "", "", observed, excluded, len(models))]
    scopes.extend(
        ("model", str(model["source_model_id"]), str(model["target"]),
         observed.loc[observed["source_model_id"] == str(model["source_model_id"])],
         excluded.loc[excluded["source_model_id"] == str(model["source_model_id"])], 1)
        for model in models
    )
    for scope, model_id, target, model_observed, model_excluded, count in scopes:
        if len(dates):
            periods.append(_period_metrics(
                model_observed, model_excluded, dates,
                scope=scope, model_id=model_id, target=target,
                horizon=len(dates), interval_start=1,
                period_kind="full_run", model_count=count,
            ))
        previous = 0
        for horizon in checkpoint_horizons:
            periods.append(_period_metrics(
                model_observed, model_excluded, dates[:horizon],
                scope=scope, model_id=model_id, target=target, horizon=horizon,
                interval_start=1, period_kind="cumulative", model_count=count,
            ))
            periods.append(_period_metrics(
                model_observed, model_excluded, dates[previous:horizon],
                scope=scope, model_id=model_id, target=target, horizon=horizon,
                interval_start=previous + 1, period_kind="interval", model_count=count,
            ))
            previous = horizon
        # The daily series is persisted once and read directly for charts.
        signal_rows = model_observed.loc[_bool_column(model_observed, "signal")]
        profits = pd.DataFrame({
            "session_date": signal_rows["session_date"],
            "profit": pd.to_numeric(signal_rows.get("directional_return"), errors="coerce")
            * NOTIONAL_PER_SIGNAL,
            "correct": _bool_column(signal_rows, "correct_direction").astype(int),
        })
        by_day = profits.groupby("session_date")[["profit", "correct"]].sum().reindex(
            dates, fill_value=0
        ) if not profits.empty else pd.DataFrame(0, index=dates, columns=["profit", "correct"])
        counts = signal_rows.groupby("session_date").size().reindex(dates, fill_value=0)
        cumulative_count = counts.cumsum()
        cumulative_correct = by_day["correct"].cumsum()
        for horizon, session in enumerate(dates, 1):
            n = int(cumulative_count.loc[session])
            daily.append({
                "scope": scope, "source_model_id": model_id, "target": target,
                "horizon": horizon, "session_date": session.date().isoformat(),
                "signals": int(counts.loc[session]), "cumulative_signals": n,
                "cumulative_precision": float(cumulative_correct.loc[session] / n) if n else None,
                "cumulative_pnl": float(by_day["profit"].cumsum().loc[session]),
            })
    period_frame = pd.DataFrame(periods, columns=PERIOD_COLUMNS)
    for horizon in checkpoint_horizons:
        section = period_frame.loc[
            (period_frame["scope"] == "model")
            & (period_frame["period_kind"] == "interval")
            & (period_frame["horizon"] == horizon)
        ]
        run_row = period_frame.loc[
            (period_frame["scope"] == "run")
            & (period_frame["period_kind"] == "interval")
            & (period_frame["horizon"] == horizon)
        ].iloc[0]
        contributors = section.loc[section["signals"] > 0]
        def quantile(column: str, q: float) -> float | None:
            return float(contributors[column].quantile(q)) if not contributors.empty else None
        population.append({
            "horizon": horizon, "interval_start": run_row["interval_start"],
            "interval_end": horizon, "session_start": run_row["session_start"],
            "session_end": run_row["session_end"], "models_t0": len(models),
            "models_with_observations": int((section["evaluated_observations"] > 0).sum()),
            "models_with_signals": len(contributors), "signals": int(run_row["signals"]),
            "median_model_precision": quantile("precision", 0.5),
            "median_model_return": quantile("mean_return", 0.5),
            "weighted_precision": run_row["precision"],
            "weighted_return": run_row["mean_return"],
            "precision_q25": quantile("precision", 0.25),
            "precision_q75": quantile("precision", 0.75),
            "return_q25": quantile("mean_return", 0.25),
            "return_q75": quantile("mean_return", 0.75),
        })
    return (
        period_frame,
        pd.DataFrame(population, columns=POPULATION_COLUMNS),
        pd.DataFrame(daily, columns=DAILY_COLUMNS),
    )
