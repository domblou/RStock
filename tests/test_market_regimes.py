import json
from dataclasses import replace
import numpy as np
import pandas as pd
import pytest
from rstock.application.market_regimes import classify_regimes, episode_table
from rstock.application.market_context import ContextProtocol, build_context, adjusted_snapshot
from rstock.application.domain import _config_from_dict, _config_to_dict
from rstock.application.market_context_ui import contexts_comparable
from rstock.config import RStockConfig


def series(tail, prefix=300):
    return pd.Series([100.0]*prefix+tail,index=pd.bdate_range("2020-01-01",periods=prefix+len(tail)))


def test_exact_thresholds_and_frozen_peak_outside_252():
    values=series([95.,90.,80.]+[80.]*280)
    result=classify_regimes(values)
    assert result.regime.iloc[300:303].tolist()==["Repli","Correction","Bear market"]
    assert result.regime.iloc[-1]=="Bear market"
    assert result.reference_peak.iloc[-1]==100
    assert result.episode_max_drawdown.iloc[-1]==pytest.approx(.2)
    assert result.episode_duration_sessions.iloc[-1]==283
    assert result.episode_id.iloc[300]==result.episode_id.iloc[-1]


def test_recovery_new_low_exit_and_origin_severity():
    values=series([94.,89.,79.]+[79.]*23+[84.,78.,83.,96.])
    result=classify_regimes(values)
    assert result.regime.iloc[-4: ].tolist()==["Reprise","Bear market","Reprise","Normal / neutre"]
    row=result.iloc[-4]
    assert row.episode_origin=="Repli"
    assert row.episode_max_severity=="Bear market"
    assert bool(result.episode_closed.iloc[-1])
    assert result.trough.iloc[-3]==78
    table=episode_table(result.assign(session_date=result.index.strftime("%Y-%m-%d")))
    assert len(table)==1 and bool(table.episode_closed.iloc[0])


def test_short_pullback_correction_neutral_and_strong_rise():
    result=classify_regimes(series([94.,100.,115.,105.]))
    assert result.regime.iloc[-4:].tolist()==["Repli","Normal / neutre","Forte hausse","Repli"]
    assert result.episode_id.dropna().nunique()==2
    assert classify_regimes(series([89.])).regime.iloc[-1]=="Correction"


def test_missing_history_and_future_changes_never_relabel_past():
    values=series([94.,89.,79.]+[79.]*25+[84.,78.])
    original=classify_regimes(values)
    altered=values.copy();altered.iloc[-2:]=[120.,130.]
    pd.testing.assert_frame_equal(original.iloc[:-2],classify_regimes(altered).iloc[:-2])
    values.iloc[301]=np.nan
    after=classify_regimes(values)
    assert after.regime.iloc[301: ].eq("Indisponible").all()
    assert original.regime.iloc[:251].eq("Indisponible").all()


def test_historical_snapshot_explicit_priority_and_protocol_identity(tmp_path):
    config=RStockConfig(project_root=tmp_path,market_context_enabled=True)
    snapshot=_config_to_dict(config);snapshot.pop("market_context_regime_version")
    restored=_config_from_dict(snapshot)
    assert restored.market_context_regime_version is None
    assert restored.market_context_enabled is True
    assert _config_from_dict(_config_to_dict(restored))==restored
    assert _config_from_dict(_config_to_dict(config)).market_context_regime_version=="rstock_spy_regimes_v1"
    assert ContextProtocol().identifier=="194d4a6b786a86c1e2d0"
    assert ContextProtocol.from_config(config).identifier!=ContextProtocol().identifier


def test_comparability_requires_same_boundaries_and_reference():
    first={"protocol_id":"p","comparability":{"boundaries":{"trend":[.1,.2]},"reference_start":"2020"}}
    assert contexts_comparable(first,dict(first))
    second=json.loads(json.dumps(first));second["comparability"]["boundaries"]["trend"][0]=.05
    assert not contexts_comparable(first,second)
    assert not contexts_comparable(first,{"protocol_id":"p"})



@pytest.mark.parametrize("horizon",[84,126,97])
def test_context_uses_every_persisted_forward_period(tmp_path,horizon):
    from types import SimpleNamespace
    from test_forward_diagnostic import _fixture
    from test_market_context import prices
    from rstock.application.domain import JobType
    from rstock.application.forward_diagnostic import materialize_forward_diagnostic
    from rstock.application.market_context_runtime import materialize_context_diagnostic,load_context_diagnostic
    import exchange_calendars as xcals
    source,output=_fixture(tmp_path,derived=True,horizon=horizon)
    materialize_forward_diagnostic(output)
    wf=tmp_path/"runs/wf/results"
    pd.DataFrame([dict(TrainStart="2023-01-03",TestStart="2023-07-03")]).to_csv(wf/"windows.csv",index=False)
    dates=pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2022-01-03","2026-12-31")).tz_localize(None)
    raw=pd.DataFrame({"Adjusted":100*np.exp(.0002*np.arange(len(dates)))},index=dates)
    class Provider:
        source_name="synthetic"
        def fetch(self,symbol,start,end):return raw.loc[(raw.index>=pd.Timestamp(start))&(raw.index<pd.Timestamp(end))]
    spec=SimpleNamespace(config=RStockConfig(project_root=tmp_path,market_context_enabled=True),job_type=JobType.FORWARD_SIMULATION,source_end_to_end_run=source.name,source_experiment_run=None,source_walk_forward_run=None,source_threshold_calibration_run=None)
    materialize_context_diagnostic(spec,output,provider=Provider())
    meta,context,metrics,_=load_context_diagnostic(output,tmp_path/"runs")
    expected=pd.read_csv(output/"forward_period_metrics.csv")
    expected=set(expected.loc[expected.scope.eq("model"),["period_kind","horizon"]].itertuples(index=False,name=None))
    assert set(metrics[["period_kind","horizon"]].itertuples(index=False,name=None))==expected
    assert set(metrics.axis).issuperset({"trend","drawdown","volatility","regime","episode"})
    assert set(metrics.qualification_origin)=={"common","additional"}
    assert all(pd.to_datetime(context.context_as_of_date)<pd.to_datetime(context.session_date))


def test_regime_extension_preserves_episode_state_and_frozen_boundaries(tmp_path):
    import exchange_calendars as xcals
    from rstock.application.market_context import publish_context,validate_context
    dates=pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2020-01-02","2023-12-29")).tz_localize(None)
    values=np.full(len(dates),100.);values[400:]=80.;values[750:]=85.
    snapshot=pd.DataFrame({"adjusted_close":values},index=dates)
    protocol=ContextProtocol(regime_version="rstock_spy_regimes_v1")
    first=publish_context(tmp_path,snapshot.iloc[:700],dates[300:700],protocol,str(dates[260].date()),str(dates[390].date()))
    old=pd.read_csv(first.parent/"market_context.csv")
    next_path=publish_context(tmp_path,snapshot.iloc[670:800]*.97,dates[300:800],protocol,"ignored","ignored",parent_manifest=first)
    new=pd.read_csv(next_path.parent/"market_context.csv")
    pd.testing.assert_frame_equal(old,new.iloc[:len(old)])
    assert validate_context(first)["boundaries"]==validate_context(next_path)["boundaries"]
    assert new.reference_peak.iloc[-1]==100
    assert new.episode_max_severity.iloc[-1]=="Bear market"


def test_first_context_day_is_explicitly_unavailable_without_prior_close():
    import exchange_calendars as xcals
    dates=pd.DatetimeIndex(xcals.get_calendar("XNYS").sessions_in_range("2025-01-02","2025-02-28")).tz_localize(None)
    snapshot=pd.DataFrame({"adjusted_close":[100.]*len(dates)},index=dates)
    context,_=build_context(snapshot,dates,ContextProtocol(regime_version="rstock_spy_regimes_v1"),dates[0],dates[1])
    assert context.regime.eq("Indisponible").all()
    assert context.regime_availability.eq("unavailable_history_or_gap").all()
