from dataclasses import replace
from types import SimpleNamespace

import exchange_calendars as xcals
import pandas as pd
import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.experiment_duplication import (
    experiment_spec_from_duplication,
    walk_forward_duplication_draft,
)
from rstock.application.workflows import (
    _persist_walk_forward_period,
    _prepared_experiment,
    _walk_forward,
)
from rstock.config import DEFAULT_CONFIG
from rstock.features import intraday_target_column, prepare_dataset


def _prices(index: pd.DatetimeIndex) -> pd.DataFrame:
    prices = pd.DataFrame(index=index)
    for position, symbol in enumerate(("AAA", "BBB"), start=1):
        base = pd.Series(range(len(index)), index=index, dtype=float) + 100 + position
        prices[f"{symbol}.Open"] = base
        prices[f"{symbol}.High"] = base + 2
        prices[f"{symbol}.Low"] = base - 1
        prices[f"{symbol}.Close"] = base + 1
    return prices


def _spec(tmp_path, offset: int) -> ExperimentSpec:
    return ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(
            DEFAULT_CONFIG,
            project_root=tmp_path,
            model_history_days=1095,
            permutation_depth=1,
            walk_forward_min_train_size=5,
            final_holdout_size=2,
            walk_forward_end_offset_sessions=offset,
        ),
        symbols=("AAA", "BBB"),
    )


def _install_market_loader(monkeypatch, sessions, calls):
    complete = _prices(sessions)

    def fake_load(self, spec, *, history_days=None, as_of=None, **kwargs):
        calls.append({"history_days": history_days, "as_of": as_of})
        end = sessions[-1] if as_of is None else pd.Timestamp(as_of)
        days = spec.config.model_history_days if history_days is None else history_days
        selected = complete.loc[
            (complete.index >= end - pd.Timedelta(days=days))
            & (complete.index <= end)
        ].copy()
        return SimpleNamespace(
            prices=selected,
            symbols=["AAA", "BBB"],
            failed_symbols=[],
        ), {"AAA": "XNYS", "BBB": "XNYS"}

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    return complete


def test_offset_zero_keeps_the_historical_preparation_path(monkeypatch, tmp_path):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2021-01-01", "2026-01-30")
    calls = []
    complete = _install_market_loader(monkeypatch, sessions, calls)
    spec = _spec(tmp_path, 0)

    prepared, _, _ = _prepared_experiment(spec, None, None)
    current = complete.loc[complete.index >= sessions[-1] - pd.Timedelta(days=1095)]
    expected = prepare_dataset(current, ["AAA", "BBB"])

    pd.testing.assert_frame_equal(prepared, expected)
    assert calls == [{"history_days": None, "as_of": None}]
    assert prepared.attrs["effective_end_date"] == sessions[-1].isoformat()


def test_historical_cutoff_is_applied_before_data_preparation(monkeypatch, tmp_path):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2021-01-01", "2026-01-30")
    calls = []
    _install_market_loader(monkeypatch, sessions, calls)
    cutoff = pd.Timestamp(sessions[-100])
    spec = replace(_spec(tmp_path, 0), historical_data_cutoff=cutoff.isoformat())

    prepared, _, _ = _prepared_experiment(spec, None, None)

    assert calls == [{"history_days": None, "as_of": cutoff.date()}]
    assert prepared.index.max() == cutoff
    assert not (prepared.index > cutoff).any()


def test_prefilter_and_walk_forward_receive_the_same_cutoff_data(monkeypatch, tmp_path):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2021-01-01", "2026-01-30")
    _install_market_loader(monkeypatch, sessions, [])
    cutoff = pd.Timestamp(sessions[-100])
    spec = replace(
        _spec(tmp_path, 0),
        historical_data_cutoff=cutoff.isoformat(),
        source_prepared_dataset_sha256="source-hash",
        config=replace(_spec(tmp_path, 0).config, predictor_prefilter_enabled=True),
    )
    received = []

    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_prefilter_walk_forward",
        lambda prepared, *args, **kwargs: (
            received.append(prepared)
            or SimpleNamespace(qualification=pd.DataFrame(), telemetry={})
        ),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.select_predictors",
        lambda *args, **kwargs: SimpleNamespace(
            predictors_by_target={"AAA": ["BBB"], "BBB": ["AAA"]},
            diagnostics={},
            metrics=pd.DataFrame(),
        ),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.generate_target_symbol_sets",
        lambda *args, **kwargs: pd.DataFrame({"V0": ["AAA"], "V1": ["BBB"]}),
    )
    monkeypatch.setattr(
        "rstock.application.workflows.evaluate_walk_forward",
        lambda prepared, *args, **kwargs: (
            received.append(prepared)
            or SimpleNamespace(
                aggregate_global=pd.DataFrame([{"Sets": 1}]),
                qualification=pd.DataFrame({"Eligible": [True]}),
                run_configuration={},
            )
        ),
    )
    persisted = {}

    def write_result(result, output):
        output.mkdir(parents=True, exist_ok=True)
        persisted.update(result.run_configuration)

    monkeypatch.setattr(
        "rstock.application.workflows.write_walk_forward_results", write_result
    )

    summary = _walk_forward(spec, tmp_path / "output", None, None)

    assert len(received) == 2
    assert received[0] is received[1]
    assert received[0].index.max() == cutoff
    assert summary["traceability"]["source_prepared_dataset_sha256"] == "source-hash"
    assert persisted["traceability"] == summary["traceability"]


@pytest.mark.parametrize("offset", [63, 126])
def test_offset_uses_the_common_market_session_and_preserves_history(
    monkeypatch, tmp_path, offset
):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2018-01-01", "2026-01-30")
    calls = []
    complete = _install_market_loader(monkeypatch, sessions, calls)
    spec = _spec(tmp_path, offset)

    prepared, _, _ = _prepared_experiment(spec, None, None)
    expected_end = pd.Timestamp(sessions[-(offset + 1)])
    expected_start = expected_end - pd.Timedelta(days=1095)

    assert pd.Timestamp(prepared.attrs["effective_end_date"]) == expected_end
    assert prepared.index.max() == expected_end
    assert not (prepared.index > expected_end).any()
    assert prepared.index.min() == complete.index[complete.index >= expected_start][0]
    assert calls[1] == {"history_days": 1095, "as_of": expected_end.date()}


def test_all_symbols_share_one_effective_end_even_with_a_missing_last_observation(
    monkeypatch, tmp_path
):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2021-01-01", "2026-01-30")
    calls = []
    complete = _install_market_loader(monkeypatch, sessions, calls)
    complete.loc[sessions[-64], [column for column in complete if column.startswith("BBB.")]] = pd.NA

    def fake_load(self, spec, *, history_days=None, as_of=None, **kwargs):
        end = sessions[-1] if as_of is None else pd.Timestamp(as_of)
        days = spec.config.model_history_days if history_days is None else history_days
        selected = complete.loc[
            (complete.index >= end - pd.Timedelta(days=days))
            & (complete.index <= end)
        ].copy()
        return SimpleNamespace(prices=selected, symbols=["AAA", "BBB"], failed_symbols=[]), {
            "AAA": "XNYS", "BBB": "XNYS"
        }

    monkeypatch.setattr("rstock.application.workflows.MarketDataService.load", fake_load)
    prepared, _, _ = _prepared_experiment(_spec(tmp_path, 63), None, None)
    effective_end = pd.Timestamp(sessions[-64])

    assert prepared.index.max() == effective_end
    assert pd.isna(prepared.loc[effective_end, intraday_target_column("BBB")])
    assert prepared.loc[effective_end, intraday_target_column("AAA")] == 0.0


def test_offset_too_large_has_a_clear_user_error(monkeypatch, tmp_path):
    sessions = xcals.get_calendar("XNYS").sessions_in_range("2024-01-01", "2026-01-30")
    _install_market_loader(monkeypatch, sessions, [])

    with pytest.raises(ValueError) as captured:
        _prepared_experiment(_spec(tmp_path, 100_000), None, None)

    message = str(captured.value)
    assert "offset demandé=100000" in message
    assert "date de fin effective=indéterminée" in message
    assert "observations disponibles=" in message
    assert "calendrier XNYS" in message


def test_legacy_snapshot_restores_historical_offset(tmp_path):
    source = _spec(tmp_path, 0).to_dict()
    source["rstock_config"].pop("walk_forward_end_offset_sessions")

    restored = ExperimentSpec.from_dict(source)

    assert restored.config.walk_forward_end_offset_sessions == 63


def test_duplication_preserves_walk_forward_end_offset(tmp_path):
    source = _spec(tmp_path, 126)
    draft = walk_forward_duplication_draft(
        "run_source", {"configuration": source.to_dict()}
    )

    duplicated = experiment_spec_from_duplication(
        draft, current_config=source.config, use_run_config=True
    )

    assert duplicated.config.walk_forward_end_offset_sessions == 126


def test_effective_period_is_added_to_the_persisted_run_configuration(tmp_path):
    prepared = pd.DataFrame(index=pd.DatetimeIndex(["2025-10-29"]))
    prepared.attrs["effective_end_date"] = "2025-10-29T00:00:00"
    run_configuration = {}

    period = _persist_walk_forward_period(
        run_configuration, prepared, _spec(tmp_path, 63).config
    )

    assert period == {
        "walk_forward_end_offset_sessions": 63,
        "effective_end_date": "2025-10-29T00:00:00",
    }
    assert run_configuration == period
