import pandas as pd

from rstock.calendars import next_market_session


def test_observed_market_date_has_priority_over_calendar():
    result = next_market_session(
        "2024-01-05", "XNYS", observed_dates=["2024-01-09", "2024-01-08"]
    )
    assert result == pd.Timestamp("2024-01-08")


def test_calendar_skips_weekend_and_market_holiday():
    # 2024-03-29 was Good Friday; XNYS reopened Monday, April 1.
    assert next_market_session("2024-03-28", "XNYS") == pd.Timestamp("2024-04-01")
