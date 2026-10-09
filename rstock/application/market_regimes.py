"""Causal SPY episode state machine. RStock definitions, never fitted to outcomes."""
from __future__ import annotations
import numpy as np
import pandas as pd

PROTOCOL = "rstock_spy_regimes_v1"
LABELS = ("Forte hausse", "Normal / neutre", "Repli", "Correction", "Bear market", "Reprise")
RULES = {
    "episode_entry_drawdown": 0.05, "correction_drawdown": 0.10,
    "bear_drawdown": 0.20, "recovery_from_trough": 0.05,
    "strong_trend_63": 0.10, "recovery_trend_sessions": 21,
    "peak_lookback_sessions": 252,
    "priority": ["unavailable", "Reprise (active episode)", "Bear market", "Correction", "Repli", "Forte hausse", "Normal / neutre"],
    "episode_exit": "drawdown from frozen peak < 5%; new trough cancels recovery",
    "warmup": "252 consecutive exchange sessions; after a gap restart warmup",
}


def severity(drawdown):
    if drawdown >= .20 - 1e-12: return "Bear market"
    if drawdown >= .10 - 1e-12: return "Correction"
    return "Repli"


def classify_regimes(values: pd.Series) -> pd.DataFrame:
    """State at each CLOSE; caller shifts to J-1. Missing prices reset knowledge.

    Origin severity is fixed at entry, maximum severity and frozen peak survive
    recovery. Exit is evaluated before recovery; exact 5% remains an episode.
    """
    peak = trough = None
    peak_date = trough_date = start = None
    maximum = 0.0
    episode = None
    origin = None
    age = run = 0
    recovering = False
    history = []
    records = []
    for day, price in values.items():
        row = {"regime": "Indisponible", "regime_availability": "unavailable_history_or_gap"}
        if not np.isfinite(price) or price <= 0:
            history = []; run = 0; episode = None; recovering = False
            records.append(row); continue
        history.append((day, float(price))); history = history[-252:]; run += 1
        if run < 252:
            records.append(row); continue
        trend21 = price / history[-22][1] - 1
        trend63 = price / history[-64][1] - 1
        closed = False
        if episode is None:
            peak_date, peak = max(history, key=lambda x: (x[1], x[0]))
            drawdown = 1 - price / peak
            if drawdown >= .05 - 1e-12:
                start = day; trough = float(price); trough_date = day
                maximum = drawdown; origin = severity(drawdown); age = 0
                episode = f"{pd.Timestamp(peak_date).date()}_{pd.Timestamp(day).date()}"
                recovering = False
        if episode is not None:
            age += 1
            drawdown = 1 - price / peak
            new_low = price < trough - 1e-12
            if new_low:
                trough = float(price); trough_date = day; recovering = False
            maximum = max(maximum, drawdown)
            rebound = price / trough - 1
            if drawdown < .05 - 1e-12:
                regime = "Forte hausse" if trend63 >= .10 - 1e-12 else "Normal / neutre"
                closed = True
            else:
                if not new_low and rebound >= .05 - 1e-12 and trend21 > 0:
                    recovering = True
                regime = "Reprise" if recovering else severity(drawdown)
            row.update(episode_id=episode, episode_start=str(pd.Timestamp(start).date()),
                reference_peak_date=str(pd.Timestamp(peak_date).date()), reference_peak=peak,
                trough_date=str(pd.Timestamp(trough_date).date()), trough=trough,
                episode_drawdown=drawdown, episode_max_drawdown=maximum,
                episode_duration_sessions=age, episode_origin=origin,
                episode_max_severity=severity(maximum), recovery=regime == "Reprise",
                rebound_from_trough=rebound, episode_closed=closed)
            if closed:
                episode = None; recovering = False
        else:
            drawdown = 1 - price / peak
            regime = "Forte hausse" if trend63 >= .10 - 1e-12 and drawdown < .05 else "Normal / neutre"
        row.update(regime=regime, regime_availability="available", trend_21=trend21)
        records.append(row)
    return pd.DataFrame(records, index=values.index)


def episode_table(context: pd.DataFrame) -> pd.DataFrame:
    columns = ["episode_id", "episode_start", "last_known_session", "reference_peak_date", "reference_peak", "trough_date", "trough", "episode_max_drawdown", "episode_max_severity", "episode_origin", "episode_duration_sessions", "episode_closed"]
    if "episode_id" not in context: return pd.DataFrame(columns=columns)
    active = context.loc[context.episode_id.notna()].copy()
    if active.empty: return pd.DataFrame(columns=columns)
    result = active.sort_values("session_date").groupby("episode_id", sort=False).tail(1)
    return result.rename(columns={"session_date": "last_known_session"}).reindex(columns=columns)
