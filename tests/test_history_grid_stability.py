"""Actual Edge checks for duplicate renders and preserved History interactions."""
import json
from pathlib import Path
import subprocess
import sys

import pytest

from rstock.application import grid


def test_history_grid_duplicate_renders_preserve_dom_and_live_updates_preserve_interactions(tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    args = dict(rows=[{"id": f"run-{number:02}", "status": "completed"} for number in range(30)],
                columns=["id", "status"], row_id="id", selection_mode="multi",
                column_options={"id": {"min_width": 700}}, max_height=300,
                selected_ids=[], preserve_interactions=True)
    harness = tmp_path / "stability.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><div id="result">WAIT</div>
<iframe style="width:400px;height:400px" src="''' + frontend.as_uri() + '''"></iframe><script>
const frame=document.querySelector('iframe'), args=''' + json.dumps(args) + ''';
const delay=()=>new Promise(resolve=>setTimeout(resolve,40));
function check(ok,message){if(!ok)throw Error(message);}
window.addEventListener('message',async event=>{
 if(event.data.type!=='streamlit:componentReady')return;
 try{
 const render=()=>frame.contentWindow.postMessage({type:'streamlit:render',args},'*');
 render();await delay();
 const doc=frame.contentDocument, win=frame.contentWindow;
 let search=doc.querySelector('input[type=search]');
 search.value='run';search.dispatchEvent(new win.Event('input'));
 doc.querySelectorAll('th')[1].click();doc.querySelectorAll('th')[1].click();
 doc.querySelector('tbody input').click();args.selected_ids=['run-29'];
 search.focus();search.setSelectionRange(1,2);
 const grid=doc.querySelector('#grid');grid.scrollLeft=120;grid.scrollTop=150;
 const scroll=[grid.scrollLeft,grid.scrollTop], original=doc.querySelector('tbody tr');
 let mutations=0;
 const observer=new win.MutationObserver(items=>{mutations+=items.filter(x=>x.type==='childList').length;});
 observer.observe(doc.querySelector('table'),{subtree:true,childList:true});
 for(let i=0;i<10;i++){render();await delay();}
 check(mutations===0,'duplicate renders reconstructed table');
 check(doc.querySelector('tbody tr')===original,'row identity');
 check(doc.activeElement===search&&search.selectionStart===1&&search.selectionEnd===2,'search focus/caret');
 check(search.value==='run','search query');
 check(grid.scrollLeft===scroll[0]&&grid.scrollTop===scroll[1],'duplicate scroll');
 check(doc.querySelector('tbody tr').dataset.identity==='run-29','descending sort');
 check(doc.querySelector('tbody input').checked,'selected row');
 const checkbox=doc.querySelector('tbody input');checkbox.focus({preventScroll:true});
 args.rows[29].status='failed';render();await delay();
 check(doc.querySelector('tbody tr').textContent.includes('failed'),'live status');
 check(doc.activeElement.dataset.focus==='check:run-29','checkbox focus after update');
 check(grid.scrollLeft===scroll[0]&&grid.scrollTop===scroll[1],'updated scroll');
 check(doc.querySelector('input[type=search]')===search&&search.value==='run','toolbar stable');
 args.rows.push({id:'run-30',status:'completed'});render();await delay();
 check(doc.querySelector('tbody tr').dataset.identity==='run-30','sort survives identity change');
 check(doc.activeElement.dataset.focus==='check:run-29','focus survives inserted row');
 check(doc.querySelector('tr[data-identity="run-29"] input').checked,'selection survives inserted row');
 check(grid.scrollLeft===scroll[0]&&grid.scrollTop===scroll[1],'inserted scroll');
 document.querySelector('#result').textContent='PASS';
 }catch(error){document.querySelector('#result').textContent='FAIL '+error.message;}
});</script>''', encoding="utf-8")
    result = subprocess.run([str(browser), "--headless=new", "--disable-gpu", "--no-sandbox",
        "--disable-gpu-sandbox", "--allow-file-access-from-files", f"--user-data-dir={tmp_path / 'profile'}",
        "--dump-dom", "--virtual-time-budget=3000", harness.as_uri()], capture_output=True,
        text=True, encoding="utf-8", errors="replace", timeout=40,
        creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]
