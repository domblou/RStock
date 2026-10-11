"""Shared grid contract, independent of any scientific model or screen."""
from datetime import date
import json
from pathlib import Path
import subprocess
import sys

import pytest

from rstock.application.grid import render_grid


def test_shared_grid_preserves_original_positions_and_display_values():
    calls = []
    def component(**kwargs):
        calls.append(kwargs)
        return ["b", "a", "outside"]
    result = render_grid(
        [{"id": "a", "score": .12, "display": "12 %", "date": date(2026, 7, 6)},
         {"id": "b", "score": float("nan")}],
        columns=["score", "date"], key="shared", row_id="id", page_size=1,
        column_options={"score": {"display_key": "display", "align": "right", "max_width": 120}},
        component=component,
    )
    assert result == {"selection": {"rows": [0, 1]}, "selected_ids": ["a", "b"]}
    assert calls[0]["rows"][0]["score"] == .12
    assert calls[0]["rows"][0]["date"] == "2026-07-06"
    assert calls[0]["rows"][1]["score"] is None
    assert calls[0]["page_size"] == 1
    assert calls[0]["column_options"]["score"]["display_key"] == "display"


@pytest.mark.parametrize("mode,positions", [("none", []), ("single", [0]), ("multi", [0, 1])])
def test_selection_modes(mode, positions):
    result = render_grid([{"id": "a"}, {"id": "b"}], columns=["id"], row_id="id",
                         key="test", selection_mode=mode, component=lambda **_: ["a", "b"])
    assert result["selection"]["rows"] == positions


def test_empty_grid_and_external_pagination():
    calls = []
    def component(**kwargs):
        calls.append(kwargs)
        return None
    assert render_grid([], columns=["id"], row_id="id", key="empty", component=component)["selected_ids"] == []
    assert calls[0]["page_size"] is None
    assert calls[0]["empty_message"] == "Aucune donnée à afficher."


@pytest.mark.parametrize("options", [{"page_size": 0}, {"page_size": True}, {"selection_mode": "bad"}, {"max_height": 50}])
def test_invalid_options_are_rejected(options):
    with pytest.raises(ValueError):
        render_grid([], columns=[], row_id="id", key="invalid", component=lambda **_: [], **options)


def test_duplicate_row_ids_are_rejected():
    with pytest.raises(ValueError, match="unique"):
        render_grid([{"id": "a"}, {"id": "a"}], columns=["id"], row_id="id", key="duplicate")


def test_shared_frontend_pagination_formats_selection_empty_and_mobile(tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    from rstock.application import grid
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    args = dict(rows=[{"id": "a", "score": 2, "formatted": "2 %"},
                      {"id": "b", "score": 10, "formatted": "10 %"}],
                columns=["score"], row_id="id", page_size=1, selection_mode="multi",
                column_options={"score": {"display_key": "formatted", "min_width": 500}})
    harness = tmp_path / "shared.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><div id="result">WAIT</div>
<iframe style="width:360px;height:240px" src="''' + frontend.as_uri() + '''"></iframe><script>
const frame = document.querySelector('iframe'), args = ''' + json.dumps(args) + ''';
let stage = 0;
function check(ok, message) { if (!ok) throw Error(message); }
function render() { frame.contentWindow.postMessage({type:'streamlit:render', args}, '*'); }
function later(fn) { setTimeout(() => { try { fn(frame.contentDocument); } catch(e) { document.querySelector('#result').textContent='FAIL '+e.message; } }, 100); }
window.addEventListener('message', event => {
 try {
  if (event.data.type === 'streamlit:componentReady') {
   render(); later(doc => {
    check(doc.querySelector('tbody').textContent === '2 %', 'format');
    check(doc.querySelector('#grid').scrollWidth > doc.querySelector('#grid').clientWidth, 'mobile scroll');
    doc.querySelector('tbody input').click();
   });
  }
  if (event.data.type === 'streamlit:setComponentValue') {
   const doc = frame.contentDocument;
   if (stage === 0) {
    check(JSON.stringify(event.data.value) === '["a"]', 'first page selection'); stage++;
    doc.querySelectorAll('#pager button')[1].click();
    check(doc.querySelector('#pager').textContent.includes('page 2 / 2'), 'pagination');
    check(doc.querySelector('tbody').textContent === '10 %', 'second page');
    doc.querySelector('tbody input').click();
   } else if(stage === 1) {
    check(event.data.value.length === 2, 'selection across pages'); stage++;
    args.selection_mode='single'; args.page_size=null; args.selected_ids=[]; render();
    later(doc => { check(!doc.querySelector('thead input'), 'single header'); doc.querySelectorAll('tbody input')[1].click(); });
   } else if(stage === 2) {
    check(JSON.stringify(event.data.value) === '["b"]', 'single selection'); stage++;
    doc.querySelector('tbody input').click();
   } else if(stage === 3) {
    check(JSON.stringify(event.data.value) === '["a"]', 'single replacement'); stage++;
    args.selection_mode='none'; render();
    later(doc => {
     check(!doc.querySelector('table input'), 'read only');
     doc.querySelector('th').click();
     check(doc.querySelector('tbody tr').textContent === '2 %', 'numeric sort');
     args.rows=[]; args.empty_message='Aucun résultat'; render();
     later(doc => { check(doc.querySelector('.empty').textContent === 'Aucun résultat', 'empty'); document.querySelector('#result').textContent='PASS'; });
    });
   }
  }
 } catch(e) { document.querySelector('#result').textContent='FAIL '+e.message; }
});</script>''', encoding="utf-8")
    result = subprocess.run([
        str(browser), "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-gpu-sandbox",
        "--allow-file-access-from-files", f"--user-data-dir={tmp_path / 'profile'}",
        "--dump-dom", "--virtual-time-budget=3000", harness.as_uri(),
    ], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=40,
       creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]
