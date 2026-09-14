"""Market-session resolution based on explicit exchange calendars."""

from __future__ import annotations

from collections.abc import Iterable

import exchange_calendars as xcals
import pandas as pd


US_EQUITIES_CALENDAR = "XNYS"


def normalise_dates(values: Iterable[object]) -> pd.DatetimeIndex:
    dates = pd.DatetimeIndex(pd.to_datetime(list(values), errors="raise"))
    if dates.tz is not None:
        dates = dates.tz_localize(None)
    if dates.hasnans:
        raise ValueError("Market dates cannot contain NaT")
    return dates.normalize().sort_values().unique()


def validate_calendar_name(calendar_name: str) -> None:
    if calendar_name not in xcals.get_calendar_names():
        raise ValueError(f"Unknown market calendar: {calendar_name}")


def offset_market_session(
    latest_available: object, calendar_name: str, offset_sessions: int
) -> pd.Timestamp:
    """Return the session ``offset_sessions`` before the latest available session."""

    if (
        not isinstance(offset_sessions, int)
        or isinstance(offset_sessions, bool)
        or offset_sessions < 0
    ):
        raise ValueError("offset_sessions must be an integer >= 0")
    validate_calendar_name(calendar_name)
    calendar = xcals.get_calendar(calendar_name)
    latest = pd.Timestamp(latest_available)
    if latest.tz is not None:
        latest = latest.tz_localize(None)
    latest = latest.normalize()
    current_session = calendar.date_to_session(latest, direction="previous")
    if offset_sessions == 0:
        session = current_session
    else:
        session = calendar.sessions_window(
            current_session, -(offset_sessions + 1)
        )[0]
    session = pd.Timestamp(session)
    return session.tz_localize(None) if session.tz is not None else session


def next_market_session(
    after: object,
    calendar_name: str,
    *,
    observed_dates: Iterable[object] = (),
) -> pd.Timestamp:
    """Return the next observed session, or consult the declared market calendar."""

    after_date = pd.Timestamp(after)
    if after_date.tz is not None:
        after_date = after_date.tz_localize(None)
    after_date = after_date.normalize()

    observed = normalise_dates(observed_dates)
    later_observed = observed[observed > after_date]
    if len(later_observed):
        return pd.Timestamp(later_observed[0])

    validate_calendar_name(calendar_name)
    calendar = xcals.get_calendar(calendar_name)
    sessions = calendar.sessions_in_range(
        after_date + pd.Timedelta(days=1), after_date + pd.Timedelta(days=15)
    )
    if not len(sessions):
        raise RuntimeError(
            f"No {calendar_name} session found in the 15 days after {after_date.date()}"
        )
    session = pd.Timestamp(sessions[0])
    return session.tz_localize(None) if session.tz is not None else session
