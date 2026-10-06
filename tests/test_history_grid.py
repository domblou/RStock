"""Main-grid presentation, persisted cutoffs and stable run selections."""
import json
from pathlib import Path
import subprocess
import sys
from dataclasses import replace

import pytest

from rstock.application import history_grid as grid
from rstock.application.history_ui import history_row, filter_runs, EXPERIMENT_JOB_TYPES
from rstock.application.history_analysis import selected_run_action
from test_history_ui import _run, _detail


def record(tmp_path, detail, identifier="wf", related=None, labels=None):
    row = history_row(_run(identifier, "walk_forward"), detail, {}, universe_labels=labels or {})
    return grid.history_grid_row(row, detail, universe_labels=labels or {},
                                 related_details=related or {}, runs_root=tmp_path)


def test_columns_lineage_universe_and_legacy_fallback(tmp_path):
    detail = _detail(symbols=("AAA", "BBB"))
    detail["configuration"].update(primary_universe_id="tech", source_prefilter_run="prefilter")
    result = record(tmp_path, detail, labels={"tech": "US Tech Growth (35)"},
                    related={"prefilter": {"configuration": {"job_type": "predictor_prefilter"}}})
    assert tuple(k for k in result if not k.startswith("_")) == grid.COLUMNS
    assert not {"Lignée", "Contexte", "Stockage", "Résumé"} & result.keys()
    assert result["Run ID"] == "wf"
    assert result["_lineage"] == "Préfiltre prédicteurs : prefilter"
    assert result["Univers"] == "US Tech Growth (35)"
    legacy = record(tmp_path, _detail(symbols=("AAA", "BBB")))
    assert legacy["Univers"] == "2 symboles"
    assert legacy["Cutoff"] == "—"


@pytest.mark.parametrize("config,metadata,source_type,expected", [
    ({}, {"parent_run_id": "source"}, "end_to_end", "End-to-end : source"),
    ({"source_prefilter_run": "source"}, {}, "predictor_prefilter", "Préfiltre prédicteurs : source"),
    ({"walk_forward_derivation": {"source_run_id": "source"}}, {}, "walk_forward", "Dérivé de Walk-forward : source"),
    ({"prefilter_derivation": {"source_run_id": "source"}}, {}, "predictor_prefilter", "Dérivé de Préfiltre prédicteurs : source"),
    ({}, {"root_run_id": "wf"}, None, "Run racine"),
    ({"source_prefilter_run": "missing"}, {}, None, "Run racine"),
])
def test_associated_run_type_under_id(tmp_path, config, metadata, source_type, expected):
    detail = _detail()
    detail["configuration"].update(config)
    detail["metadata"] = metadata
    related = {"source": {"configuration": {"job_type": source_type}}} if source_type else {}
    assert record(tmp_path, detail, related=related)["_lineage"] == expected


def test_associated_type_outside_current_history_filter(tmp_path):
    detail = _detail()
    detail["metadata"] = {"parent_run_id": "pipeline"}
    path = tmp_path / "pipeline/status.json"
    path.parent.mkdir()
    path.write_text(json.dumps({"job_type": "end_to_end"}))
    original = path.read_bytes()
    assert record(tmp_path, detail)["_lineage"] == "End-to-end : pipeline"
    assert path.read_bytes() == original


@pytest.mark.parametrize("job_type,key,overrides", [
    ("walk_forward", "walk_forward_derivation", {
        "xgb_eta": {"old_value": 0.5, "new_value": 0.2},
        "xgb_max_depth": {"old_value": 6, "new_value": 3},
        "xgb_reg_lambda": {"old_value": 1., "new_value": 5.},
        "xgb_rounds": {"old_value": 8, "new_value": 20},
        "xgb_seed": {"old_value": 1234, "new_value": 1234},
    }),
    ("predictor_prefilter", "prefilter_derivation", {
        "prefilter_xgb_eta": {"old_value": 0.5, "new_value": 0.2},
        "prefilter_xgb_num_boost_round": {"old_value": 8, "new_value": 20},
    }),
    ("end_to_end", "derivation", [
        {"field": "xgb_eta", "old_value": 0.5, "new_value": 0.2},
    ]),
])
def test_derived_column_only_contains_structured_changes(tmp_path, job_type, key, overrides):
    detail = _detail()
    detail["configuration"][key] = {"overrides": overrides}
    row = history_row(_run("derived", job_type), detail, {})
    row = replace(row, summary="Résumé complet à ignorer · Modifications : contenu périmé")
    result = grid.history_grid_row(row, detail, universe_labels={}, related_details={}, runs_root=tmp_path)
    assert "eta 0,5 → 0,2" in result["Dérivé"]
    assert "Résumé" not in result["Dérivé"] and "périmé" not in result["Dérivé"]
    assert "seed" not in result["Dérivé"]
    if job_type == "walk_forward":
        assert result["Dérivé"] == "eta 0,5 → 0,2 · max_depth 6 → 3 · reg_lambda 1 → 5 · rounds 8 → 20"
    if job_type == "predictor_prefilter":
        assert "rounds 8 → 20" in result["Dérivé"]


def test_derived_column_empty_and_legacy_summary_fallback(tmp_path):
    detail = _detail()
    assert record(tmp_path, detail)["Dérivé"] == "—"
    row = history_row(_run("legacy", "walk_forward"), detail, {})
    row = replace(row, summary="Univers · WF expansive · Modifications : eta 0,5 → 0,2")
    assert grid._grid_changes(row, detail["configuration"]) == "eta 0,5 → 0,2"
    detail["configuration"]["walk_forward_derivation"] = {"overrides": {}}
    assert grid._grid_changes(row, detail["configuration"]) == "—"


@pytest.mark.parametrize("configuration,expected", [
    ({"resolved_market_session_cutoff": "2026-07-06", "requested_historical_cutoff": "2026-07-07"}, "2026-07-06"),
    ({"historical_data_cutoff": "2026-07-06T00:00:00"}, "2026-07-06"),
    ({"requested_historical_cutoff": "2026-07-07"}, "2026-07-07"),
    ({}, "—"),
])
def test_cutoff_only_uses_existing_dates(tmp_path, configuration, expected):
    detail = _detail()
    detail["configuration"].update(configuration)
    assert record(tmp_path, detail)["Cutoff"] == expected


def test_wf_prefilter_contract_is_authoritative_and_missing_source_falls_back(tmp_path):
    detail = _detail()
    detail["configuration"].update(source_prefilter_run="prefilter", historical_data_cutoff="2026-07-07")
    path = tmp_path / "prefilter/results/prefilter_contract.json"
    path.parent.mkdir(parents=True)
    path.write_text(json.dumps({"cutoff": "2026-07-06T00:00:00"}))
    original = path.read_bytes()
    assert record(tmp_path, detail)["Cutoff"] == "2026-07-06"
    assert path.read_bytes() == original
    path.unlink()
    assert record(tmp_path, detail)["Cutoff"] == "2026-07-07"


def test_legacy_trace_date_and_inherited_universe(tmp_path):
    detail = _detail(summary={"traceability": {"prepared_market_last_date": "2026-07-06T00:00:00"}})
    detail["metadata"] = {"parent_run_id": "root"}
    parent = _detail()
    parent["configuration"]["primary_universe_id"] = "tech"
    result = record(tmp_path, detail, related={"root": parent}, labels={"tech": "US Tech Growth (35)"})
    assert result["Univers"] == "US Tech Growth (35)"
    assert result["Cutoff"] == "2026-07-06"


def test_selection_uses_ids_after_sorting_and_pagination(monkeypatch):
    calls = []
    def component(**kwargs):
        calls.append(kwargs)
        return ["second", "first", "off-page"]
    monkeypatch.setattr(grid, "_component", component)
    rows = [{"Run ID": "first"}, {"Run ID": "second"}, {"Run ID": "third"}]
    selected = grid.render_history_grid(rows, key="history-grid")
    assert selected == {"selection": {"rows": [0, 1]}}
    assert selected_run_action([rows[i]["Run ID"] for i in selected["selection"]["rows"]]) == "comparison"
    assert calls[0]["columns"] == list(grid.COLUMNS)
    assert grid.render_history_grid(rows[2:], key="history-grid")["selection"]["rows"] == []


def test_storage_filter_retained_independently_of_grid(tmp_path):
    runs = [_run("full", "walk_forward"), _run("purged", "walk_forward")]
    details = {identifier: {"storage": {"state": state}} for identifier, state in [("full", "full"), ("purged", "purged")]}
    assert [r["run_id"] for r in filter_runs(runs, allowed_types=EXPERIMENT_JOB_TYPES, storage="Complet", detail_loader=details.get)] == ["full"]
    assert [r["run_id"] for r in filter_runs(runs, allowed_types=EXPERIMENT_JOB_TYPES, storage="Résumé seulement", detail_loader=details.get)] == ["purged"]


def test_renderer_has_secondary_line_and_scoped_styles():
    html = (Path(grid.__file__).with_name("history_grid_frontend") / "index.html").read_text(encoding="utf-8")
    assert 'secondary.className = "lineage"' in html
    assert 'secondary.textContent = row[secondaryKey]' in html
    assert '.lineage { font-size: 11px; opacity: .65;' in html
    assert 'check.type = "checkbox"' in html
    assert 'value: [...selected]' in html
    assert 'text.textContent = value' in html  # Plain text, no run-provided HTML.


def test_real_browser_two_line_cells_checkboxes_and_sorting(tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    rows = [{column: "2026-07-06" for column in grid.COLUMNS} for _ in range(2)]
    rows[0].update({"Run ID": "wf-z", "_lineage": "Préfiltre source : pref-z", "Univers": "US Tech Growth (35)"})
    rows[1].update({"Run ID": "wf-a", "_lineage": "Run racine", "Univers": "2 symboles"})
    args = json.dumps(dict(rows=rows, columns=list(grid.COLUMNS), selected_ids=[],
                           column_options={"Run ID": {"secondary_key": "_lineage", "min_width": 250, "max_width": 300}}))
    harness = tmp_path / "grid.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><div id="result">WAIT</div>
<iframe style="width:1500px;height:220px;border:0" src="''' + frontend.as_uri() + '''"></iframe>
<script>
const frame = document.querySelector('iframe'); let stage = 0;
function assert(ok, message) { if (!ok) throw Error(message); }
window.addEventListener('message', event => {
 try {
  if (event.data.type === 'streamlit:componentReady') {
    frame.contentWindow.postMessage({type:'streamlit:render', args:''' + args + '''}, '*');
    setTimeout(() => {
     try {
      const doc = frame.contentDocument, second = doc.querySelector('.lineage');
      assert(second.textContent === 'Préfiltre source : pref-z', 'lineage');
      assert(frame.contentWindow.getComputedStyle(second).fontSize === '11px', 'font size');
      assert(frame.contentWindow.getComputedStyle(second).opacity === '0.65', 'muted');
      assert([...doc.querySelectorAll('th')].map(x=>x.textContent).join('|') === '|Run ID|Date / heure|Type|Univers|Cutoff|Dérivé|Statut|Durée', 'columns');
      doc.querySelector('tbody input').click();
     } catch(error) { document.querySelector('#result').textContent='FAIL '+error.message; }
    }, 100);
  }
  if (event.data.type === 'streamlit:setComponentValue') {
    const doc = frame.contentDocument;
    if (stage === 0) {
      assert(JSON.stringify(event.data.value) === '["wf-z"]', 'first selection'); stage=1;
      doc.querySelectorAll('th')[1].click();
      assert(doc.querySelector('.run-id').textContent === 'wf-a', 'sorted');
      doc.querySelector('tbody input').click();
    } else if(stage === 1) {
      assert(event.data.value.includes('wf-z') && event.data.value.includes('wf-a'), 'multi selection'); stage=2;
      doc.querySelector('thead input').click();
    } else {
      assert(event.data.value.length === 0, 'clear selection');
      document.querySelector('#result').textContent='PASS';
    }
  }
 } catch(error) { document.querySelector('#result').textContent='FAIL '+error.message; }
});
</script>''', encoding="utf-8")
    result = subprocess.run([
        str(browser), "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-gpu-sandbox",
        "--allow-file-access-from-files",
        f"--user-data-dir={tmp_path / 'profile'}", "--dump-dom", "--virtual-time-budget=3000",
        "--window-size=1600,800", f"--screenshot={tmp_path / 'grid.png'}", harness.as_uri(),
    ], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=40,
       creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]
