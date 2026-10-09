from pathlib import Path
from types import SimpleNamespace
import pytest
from rstock.application.worker import _validate_publication_recovery


def test_partial_diagnostic_directory_cannot_be_marked_completed(tmp_path):
    (tmp_path/"results").mkdir()
    (tmp_path/"results/selection_funnel.csv").write_text("stage\nWF\n")
    (tmp_path/"_working").mkdir()
    (tmp_path/"_working/pipeline_summary.json").write_text("{}")
    summary={"result_files":["pipeline_summary.json"]}
    with pytest.raises(ValueError,match="working_not_reconciled"):
        _validate_publication_recovery(tmp_path,summary)
    (tmp_path/"_working/pipeline_summary.json").rename(tmp_path/"results/pipeline_summary.json")
    (tmp_path/"_working").rmdir()
    _validate_publication_recovery(tmp_path,summary)


def test_recovery_requires_published_files_and_forward_snapshot(tmp_path):
    (tmp_path/"results").mkdir()
    summary={"result_files":["pipeline_summary.json"],"forward_simulation":{"status":"pending"}}
    with pytest.raises(ValueError,match="result_missing"):_validate_publication_recovery(tmp_path,summary)
    (tmp_path/"results/pipeline_summary.json").write_text("{}")
    with pytest.raises(ValueError,match="forward_snapshot_missing"):_validate_publication_recovery(tmp_path,summary)
    (tmp_path/"results/forward_model_snapshot.json").write_text("{}")
    _validate_publication_recovery(tmp_path,summary)
    with pytest.raises(ValueError,match="result_missing"):
        _validate_publication_recovery(tmp_path,{"result_files":["../config.json"]})


def test_e2e_technical_tab_displays_parent_log_and_failure(tmp_path,monkeypatch):
    import rstock.application.streamlit_app as app
    messages=[];errors=[]
    st=SimpleNamespace(session_state=SimpleNamespace(lab_config=SimpleNamespace(project_root=tmp_path)),
        caption=lambda *a,**k:None,subheader=lambda *a,**k:None,
        code=messages.append,error=errors.append,json=lambda *a,**k:None,write=lambda *a,**k:None)
    monkeypatch.setattr(app,"st",st)
    app._render_pipeline_technical("parent",{"log_tail":["publishing","WinError 183"],"status":{"error":"collision"}})
    assert messages==["publishing\nWinError 183"]
    assert errors==["collision"]
