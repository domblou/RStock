import io
import json
import time
from pathlib import Path
from types import SimpleNamespace
from zipfile import ZipFile
import pandas as pd
import pytest
from rstock.application import selection_diagnostic as sd
from rstock.application.selection_diagnostic_ui import load_table, load_detail, forward_tables
from rstock.application.forward_export import build_selection_export
from rstock.application.forward_diagnostic import digest, _json, _publish, materialize_forward_diagnostic
from rstock.application.market_context_runtime import prediction_groups


def fixture(tmp_path):
    from test_forward_diagnostic import _fixture
    source,output=_fixture(tmp_path,derived=True,horizon=126)
    wf=source.parent/"wf/results"
    q=pd.read_csv(wf/"qualification.csv")
    q["Eligible"]=[True,True,False];q["IneligibilityReasons"]=["[]","[]",'["worst_window_auc"]'];q["EligibleRank"]=[1,2,None]
    q["ROCAUCWorst"]=[.6,.5,.3];q["ROCAUCStd"]=[.01,.1,.2]
    q.to_csv(wf/"qualification.csv",index=False)
    rows=[]
    for i,row in q.iterrows():
        for w in range(1,4):rows.append({"Set":row.Set,"Window":w,"UpROCAUC":.7-i*.1-w*.01,"TestStart":f"2025-0{w}-01","TestEnd":f"2025-0{w}-28"})
    pd.DataFrame(rows).to_csv(wf/"windows.csv",index=False)
    pd.DataFrame({"PrefilterStatus":["retained","rejected_threshold","rejected_top_n"]}).to_csv(wf/"predictor_prefilter.csv",index=False)
    q.to_csv(wf/"selection_results.csv",index=False)
    materialize_forward_diagnostic(output)
    return source,output


def test_funnel_rejected_population_windows_and_derived_sources(tmp_path):
    source,output=fixture(tmp_path)
    config_path=source/"config.json"
    _publish(config_path,{**_json(config_path),"rstock_config":{"predictor_prefilter_top_n":12,"qualification_min_median_auc":.55,"threshold_calibration_min_signals_per_window":20}})
    before={p:digest(p) for p in source.parent.rglob("*") if p.is_file()}
    manifest=sd.materialize_selection(source)
    assert manifest["stage_run_ids"]["walk_forward"]=="wf"
    assert manifest["candidate_count"]==3
    assert manifest["criteria"]["predictor_prefilter_top_n"]==12
    assert manifest["criteria"]["threshold_calibration_min_signals_per_window"]==20
    candidates=load_table(source/"results",manifest,"selection_candidates.csv")
    assert candidates.wf_eligible.tolist()==[True,True,False]
    assert candidates.qualification_candidate.tolist()==[True,True,False]
    assert set(candidates.wf_window_count)=={3}
    distribution=load_table(source/"results",manifest,"selection_wf_windows.csv")
    assert set(distribution.selection_stage)=={"Walk-forward","Qualification promotion"}
    detail=load_detail(source/"results",manifest,candidates.model_key.iloc[0])
    assert len(detail["windows"])==3
    funnel=load_table(source/"results",manifest,"selection_funnel.csv")
    assert funnel.loc[funnel.stage.eq("Walk-forward"),"rejected"].iloc[0]==1
    assert "walk_forward/market_context" in manifest["missing"]
    for p,sha in before.items():assert digest(p)==sha


def test_forward_all_horizons_compact_export_and_no_raw_predictions(tmp_path,monkeypatch):
    source,output=fixture(tmp_path);sd.materialize_selection(source)
    sd.materialize_generalization(output)
    table,associations,unavailable=forward_tables(source)
    assert {21,42,63,84,126}.issubset(set(table.horizon))
    assert table.source_model_id.nunique()==2
    assert not table.origin.eq("removed").any()
    assert associations.status.eq("insufficient_support").all()
    # Export and UI must never touch raw scientific prediction CSVs.
    original=Path.read_bytes
    def guarded(path):
        assert path.name not in {"predictions.csv","holdout_predictions.csv","forward_observations.csv"}
        return original(path)
    monkeypatch.setattr(Path,"read_bytes",guarded)
    payload=build_selection_export(source)
    with ZipFile(io.BytesIO(payload)) as archive:
        assert "export_manifest.json" in archive.namelist()
        assert not any(n.endswith("predictions.csv") or n.endswith("forward_observations.csv") for n in archive.namelist())
        name=f"runs/{source.name}/results/selection_candidates.csv"
        assert archive.read(name)==(source/"results/selection_candidates.csv").read_bytes()


def test_interruption_retry_preserves_external_manifest_and_parent_index(tmp_path,monkeypatch):
    source,output=fixture(tmp_path)
    original=sd._publish
    def interrupt(path,value,**kwargs):
        if path.name==sd.MANIFEST:
            raise RuntimeError("interrupted")
        original(path,value,**kwargs)
    monkeypatch.setattr(sd,"_publish",interrupt)
    with pytest.raises(RuntimeError):sd.materialize_selection(source)
    assert not (source/"results"/sd.MANIFEST).exists()
    monkeypatch.setattr(sd,"_publish",original)
    _publish(source/"results"/sd.MANIFEST,{"external_field":"preserve"})
    assert sd.materialize_selection(source)["external_field"]=="preserve"
    index=source/"results/selection_forward_index.json"
    _publish(index,{"runs":{"another":{"manifest":"another/results/m.json","sha256":"old"}},"external":"keep"})
    sd.materialize_generalization(output)
    assert set(_json(index)["runs"])=={"another",output.parent.name}
    assert _json(index)["external"]=="keep"
    # An unchanged re-run returns the same commit and does not rewrite artifacts.
    stamp=(source/"results"/sd.MANIFEST).stat().st_mtime_ns
    sd.materialize_selection(source)
    assert (source/"results"/sd.MANIFEST).stat().st_mtime_ns==stamp


def test_streaming_pairs_across_chunk_boundaries_and_repeated_sets_rejected(tmp_path):
    p=tmp_path/"predictions.csv"
    pd.DataFrame({"Set":["a"]*5+["b"]*4,"Date":["2025-01-01"]*9}).to_csv(p,index=False)
    assert [len(g) for g in prediction_groups(p,chunksize=3)]==[5,4]
    pd.DataFrame({"Set":["a","b","a"]}).to_csv(p,index=False)
    with pytest.raises(ValueError,match="noncontiguous"):list(prediction_groups(p,chunksize=2))


def test_ui_data_performance_and_no_heavy_reads(tmp_path,monkeypatch):
    source,output=fixture(tmp_path);manifest=sd.materialize_selection(source)
    original=pd.read_csv
    reads=[]
    def guarded(path,*args,**kwargs):
        name=Path(path).name;reads.append(name)
        assert name.startswith("selection_")
        return original(path,*args,**kwargs)
    monkeypatch.setattr(pd,"read_csv",guarded)
    from rstock.application.selection_diagnostic_ui import _table
    _table.cache_clear()
    start=time.perf_counter()
    for _ in range(10):
        for name in (sd.FILES[0],sd.FILES[3],sd.FILES[4],sd.FILES[5]):load_table(source/"results",manifest,name)
    elapsed=time.perf_counter()-start
    assert len(reads)==4
    assert elapsed<2



def test_actual_streamlit_render_and_lazy_details(tmp_path):
    from streamlit.testing.v1 import AppTest
    source,output=fixture(tmp_path);sd.materialize_selection(source);sd.materialize_generalization(output)
    script="from pathlib import Path\nimport streamlit as st\nfrom rstock.application.selection_diagnostic_ui import render_selection_diagnostic\nrender_selection_diagnostic(st,Path("+repr(str(source))+"))"
    app=AppTest.from_string(script,default_timeout=15).run()
    assert not app.exception
    assert len(app.metric)==3
    app.toggle[0].set_value(True).run()
    assert not app.exception
    assert len(app.dataframe)>=8
    app.selectbox[-1].set_value(app.selectbox[-1].options[-1]).run()
    assert not app.exception



def test_selection_reads_persisted_regimes_for_all_candidates_and_forward(tmp_path):
    from test_market_context import setup_run
    from rstock.application.market_context_runtime import materialize_context_diagnostic
    from rstock.application.domain import JobType
    from dataclasses import replace
    source,output=fixture(tmp_path)
    spec,inputs,provider=setup_run(tmp_path/"context_inputs")
    spec.config=replace(spec.config,project_root=tmp_path)
    wf=tmp_path/"runs/wf/results"
    window_rows=pd.read_csv(wf/"windows.csv").assign(TrainStart="2023-01-03",TestStart="2023-07-03",TestEnd="2023-10-02")
    window_rows.to_csv(wf/"windows.csv",index=False)
    # Use the same scientific identities as qualification, without training.
    q=pd.read_csv(wf/"qualification.csv");base=pd.read_csv(inputs/"predictions.csv")
    pd.concat([base.assign(Set=value) for value in q.Set],ignore_index=True).to_csv(wf/"predictions.csv",index=False)
    spec.source_end_to_end_run=source.name
    materialize_context_diagnostic(spec,wf,provider=provider)
    manifest=sd.materialize_selection(source)
    assert manifest["comparability"]["context_store"]
    population=load_table(source/"results",manifest,"selection_context_population.csv")
    assert "regime" in set(population.axis)
    assert "Rejetés WF" in set(population.population)
    detail=load_detail(source/"results",manifest,pd.read_csv(source/"results/selection_candidates.csv").model_key.iloc[-1])
    assert detail["context"]
    assert "exposure_adjusted_signal_concentration" in detail["context"][0]


def test_missing_detail_partition_is_rebuilt_and_forward_commit_reconciles(tmp_path,monkeypatch):
    source,output=fixture(tmp_path)
    meta=sd.materialize_selection(source)
    path=source/"results"/next(iter(meta["detail_digests"]))
    path.unlink()
    repaired=sd.materialize_selection(source)
    assert digest(path)==repaired["detail_digests"][path.relative_to(source/"results").as_posix()]
    original=sd._publish
    def interrupt(path,value,**kwargs):
        if path.name=="selection_generalization_manifest.json":
            original(path,{"external":"preserve"})
            raise RuntimeError("interrupted_before_commit")
        original(path,value,**kwargs)
    monkeypatch.setattr(sd,"_publish",interrupt)
    with pytest.raises(RuntimeError,match="interrupted_before_commit"):
        sd.materialize_generalization(output)
    assert not (source/"results/selection_forward_index.json").exists()
    monkeypatch.setattr(sd,"_publish",original)
    sd.materialize_generalization(output)
    assert _json(output/"selection_generalization_manifest.json")["external"]=="preserve"
    assert output.parent.name in _json(source/"results/selection_forward_index.json")["runs"]


def test_forward_sources_changed_during_publish_are_not_committed(tmp_path,monkeypatch):
    source,output=fixture(tmp_path);sd.materialize_selection(source)
    original=sd._publish
    def mutate(path,value,**kwargs):
        original(path,value,**kwargs)
        if path.name==sd.FORWARD_FILES[-1]:
            with (source/"results/selection_candidates.csv").open("a",encoding="utf-8") as stream:stream.write("\n")
    monkeypatch.setattr(sd,"_publish",mutate)
    with pytest.raises(ValueError,match="sources_changed"):
        sd.materialize_generalization(output)
    assert not (output/"selection_generalization_manifest.json").exists()
    assert not (source/"results/selection_forward_index.json").exists()


def test_legacy_context_does_not_invent_exposure():
    table=pd.DataFrame({"auc":[.7],"axis":["trend"]})
    result=sd._context_compatibility_columns(table)
    assert result.market_exposure_share.isna().all()
    assert result.exposure_adjusted_signal_concentration.isna().all()
    assert result.auc.tolist()==[.7]


def test_streamlit_5000_candidates_performance(tmp_path):
    from streamlit.testing.v1 import AppTest
    from rstock.application.selection_diagnostic_ui import _table
    source,output=fixture(tmp_path);meta=sd.materialize_selection(source)
    path=source/"results/selection_candidates.csv"
    small=pd.read_csv(path)
    extra=pd.concat([small.iloc[[0]]]*4997,ignore_index=True)
    extra["model_key"]=[f"synthetic-{i}" for i in range(4997)]
    _publish(path,pd.concat([small,extra],ignore_index=True))
    meta["artifact_digests"][path.name]=digest(path);meta["candidate_count"]=5000
    _publish(source/"results"/sd.MANIFEST,meta)
    script="from pathlib import Path\nimport streamlit as st\nfrom rstock.application.selection_diagnostic_ui import render_selection_diagnostic\nrender_selection_diagnostic(st,Path("+repr(str(source))+"))"
    _table.cache_clear()
    started=time.perf_counter();app=AppTest.from_string(script,default_timeout=15).run();cold=time.perf_counter()-started
    assert not app.exception
    warm=[]
    for _ in range(5):
        started=time.perf_counter();app.run();warm.append(time.perf_counter()-started)
        assert not app.exception
    started=time.perf_counter();app.toggle[0].set_value(True).run();details=time.perf_counter()-started
    assert not app.exception
    started=time.perf_counter();app.selectbox[-1].set_value(app.selectbox[-1].options[1]).run();interaction=time.perf_counter()-started
    assert not app.exception
    median=sorted(warm)[2]
    print(f"UI_BENCH candidates=5000 cold_s={cold:.4f} warm_median_s={median:.4f} details_s={details:.4f} interaction_s={interaction:.4f} cache={_table.cache_info()}")
    assert median<2 and details<2 and interaction<2


def test_zip_rejects_changed_scientific_window_source(tmp_path):
    source,output=fixture(tmp_path);sd.materialize_selection(source)
    path=source.parent/"wf/results/windows.csv"
    with path.open("a",encoding="utf-8") as stream:stream.write("\n")
    with pytest.raises(ValueError):build_selection_export(source)


def test_e2e_posthook_uses_working_directory_until_publication(tmp_path):
    source,output=fixture(tmp_path)
    working=source/"_working"
    (source/"results").rename(working)
    spec=SimpleNamespace(job_type=SimpleNamespace(value="end_to_end"))
    meta=sd.optional_selection_diagnostic(spec,working)
    assert meta["status"]=="available"
    assert (working/sd.MANIFEST).is_file()
    assert not (source/"results").exists()
    assert (working/"forward_model_snapshot.json").is_file()
    working.rename(source/"results")
    published=load_table(source/"results",meta,"selection_candidates.csv")
    assert len(published)==3


def test_forward_links_remain_valid_after_working_directory_publication(tmp_path):
    source,output=fixture(tmp_path);sd.materialize_selection(source)
    working=output.parent/"_working"
    output.rename(working)
    sd.materialize_generalization(working)
    ref=_json(source/"results/selection_forward_index.json")["runs"][output.parent.name]
    assert "/_working/" not in ref["manifest"]
    assert "/results/" in ref["manifest"]
    meta=_json(working/"selection_generalization_manifest.json")
    assert not any("/_working/" in name for name in meta["sources"])
    working.rename(output)
    table,_,unavailable=forward_tables(source)
    assert not table.empty and not unavailable
    assert build_selection_export(source)
