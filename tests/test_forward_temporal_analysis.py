"""Descriptive Forward checkpoints use exchange sessions and observed signals only."""

import pandas as pd
import pytest

from rstock.calendars import forward_market_sessions
from rstock.application.forward_temporal_analysis import analyze_forward_periods


def _population(horizon: int):
    sessions = forward_market_sessions("2026-01-02", "XNYS", horizon)
    models = [
        {"source_model_id": "a", "target": "AAA"},
        {"source_model_id": "b", "target": "BBB"},
    ]
    observations = []
    exclusions = []
    for age, session in enumerate(sessions, 1):
        day = session.date().isoformat()
        observations.append({
            "source_model_id": "a", "session_date": day,
            "signal": age in {1, 2, 22, 43, 64, 85},
            "correct_direction": age in {1, 22, 64, 85},
            "directional_return": {1: 0.02, 2: -0.03, 22: 0.01,
                                   43: -0.01, 64: 0.02, 85: 0.03}.get(age, 0.0),
        })
        if age == 22:
            exclusions.append({"source_model_id": "b", "session_date": day})
        else:
            observations.append({
                "source_model_id": "b", "session_date": day,
                "signal": False, "correct_direction": False,
                "directional_return": 0.0,
            })
    return (pd.DataFrame(observations),
            pd.DataFrame(exclusions, columns=["source_model_id", "session_date"]),
            models, sessions)


@pytest.mark.parametrize("horizon, expected", [(63, [21, 42, 63]),
                                               (126, [21, 42, 63, 84, 126]),
                                               (70, [21, 42, 63])])
def test_xnys_checkpoint_horizons_and_intervals(horizon, expected):
    observed, excluded, models, sessions = _population(horizon)
    periods, population, daily = analyze_forward_periods(
        observed, excluded, models, sessions
    )
    run = periods.loc[periods.scope == "run"]
    assert run.loc[run.period_kind == "cumulative", "horizon"].tolist() == expected
    assert run.loc[run.period_kind == "interval", "interval_start"].tolist() == (
        [1, 22, 43, 64, 85][:len(expected)]
    )
    assert population.horizon.tolist() == expected
    assert daily.loc[daily.scope == "run", "horizon"].max() == horizon
    assert "ROCAUC" not in periods.columns


def test_cumulative_interval_population_and_drawdown():
    observed, excluded, models, sessions = _population(63)
    periods, population, daily = analyze_forward_periods(
        observed, excluded, models, sessions
    )
    model = periods.loc[(periods.source_model_id == "a") & (periods.horizon == 42)]
    cumulative = model.loc[model.period_kind == "cumulative"].iloc[0]
    interval = model.loc[model.period_kind == "interval"].iloc[0]
    assert cumulative.signals == 3
    assert cumulative.precision == pytest.approx(2 / 3)
    assert cumulative.pnl == pytest.approx(0.0)
    assert cumulative.drawdown == pytest.approx(300.0)
    assert interval.signals == 1
    assert interval.precision == 1.0
    assert interval.pnl == pytest.approx(100.0)
    assert interval.drawdown == 0.0
    second = periods.loc[(periods.source_model_id == "b") &
                         (periods.horizon == 42) &
                         (periods.period_kind == "interval")].iloc[0]
    assert second.signals == 0
    assert pd.isna(second.precision)
    assert second.evaluated_observations == 20
    assert second.excluded_observations == 1
    pop = population.loc[population.horizon == 42].iloc[0]
    assert pop.models_t0 == 2
    assert pop.models_with_observations == 2
    assert pop.models_with_signals == 1
    assert pop.median_model_precision == 1.0
    assert daily.loc[(daily.scope == "run") & (daily.horizon == 42),
                     "cumulative_pnl"].iloc[0] == pytest.approx(0.0)


def test_missing_model_session_is_rejected_instead_of_invented():
    observed, excluded, models, sessions = _population(21)
    observed = observed.loc[~((observed.source_model_id == "b") &
                              (observed.session_date == sessions[-1].date().isoformat()))]
    with pytest.raises(ValueError, match="incomplete_coverage"):
        analyze_forward_periods(observed, excluded, models, sessions)


def test_population_median_keeps_models_equal_and_weighted_rate_counts_signals():
    observed, excluded, models, sessions = _population(21)
    mask = observed.source_model_id.eq("b") & observed.session_date.isin(
        [item.date().isoformat() for item in sessions[:10]]
    )
    observed.loc[mask, "signal"] = True
    observed.loc[mask, "correct_direction"] = False
    observed.loc[mask, "directional_return"] = -0.01
    _periods, population, _daily = analyze_forward_periods(
        observed, excluded, models, sessions
    )
    row = population.iloc[0]
    assert row.models_with_signals == 2
    assert row.signals == 12
    assert row.median_model_precision == pytest.approx(0.25)
    assert row.weighted_precision == pytest.approx(1 / 12)
    assert row.median_model_return == pytest.approx((-0.005 - 0.01) / 2)
