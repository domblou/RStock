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


def test_history_selection_revisions_reject_stale_renders_and_preserve_pending_clicks(tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    args = dict(rows=[{"id": "run-1"}, {"id": "run-2"}], columns=["id"], row_id="id",
                selection_mode="multi", selected_ids=[], preserve_interactions=True,
                selection_sync=dict(context=1, version=1, ack=None))
    harness = tmp_path / "selection-ordering.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><div id="result">WAIT</div>
<iframe style="width:800px;height:400px" src="''' + frontend.as_uri() + '''"></iframe><script>
const frame=document.querySelector('iframe'), initial=''' + json.dumps(args) + ''';
const messages=[];
const delay=()=>new Promise(resolve=>setTimeout(resolve,40));
function check(ok,message){if(!ok)throw Error(message);}
window.addEventListener('message',event=>{
 if(event.data.type==='streamlit:setComponentValue')messages.push(event.data.value);
});
window.addEventListener('message',async event=>{
 if(event.data.type!=='streamlit:componentReady')return;
 try{
 const doc=frame.contentDocument;
 const render=async args=>{frame.contentWindow.postMessage({type:'streamlit:render',args},'*');await delay();};
 const click=async id=>{doc.querySelector('tr[data-identity="'+id+'"] input').click();await delay();return messages.at(-1);};
 const checked=()=>[...doc.querySelectorAll('tbody tr[data-identity]')].filter(tr=>tr.querySelector('input').checked).map(tr=>tr.dataset.identity);
 const equal=(actual,expected,label)=>check(JSON.stringify([...actual].sort())===JSON.stringify([...expected].sort()),label+': '+JSON.stringify(actual));
 const reply=(version,ids,ack,extra={})=>({...initial,...extra,selected_ids:ids,selection_sync:{version,context:1,ack:ack&&{client_id:ack.client_id,revision:ack.revision}}});
 await render(initial);
 const first=await click('run-2');
 equal(first.selected_ids,['run-2'],'first click');
 check(first.revision===1&&first.context===1,'first revision/context');
 await render(reply(2,['run-2'],first));
 const second=await click('run-1');
 equal(second.selected_ids,['run-2','run-1'],'run 2 then run 1 message');
 // Reproduce the original one-click lag, even with a newer server render.
 await render(reply(3,['run-2'],first));
 equal(checked(),['run-1','run-2'],'old acknowledgement cannot undo run 1');
 await render(reply(4,['run-2','run-1'],second));
 equal(checked(),['run-1','run-2'],'latest acknowledgement');
 await render(reply(3,['run-2'],first));
 equal(checked(),['run-1','run-2'],'out-of-order render ignored after acknowledgement');
 await render(reply(5,['run-2','run-1'],second));
 equal(checked(),['run-1','run-2'],'refresh');
 // Several synchronous clicks before any server response, including deselection.
 doc.querySelector('tr[data-identity="run-1"] input').click();
 doc.querySelector('tr[data-identity="run-2"] input').click();
 doc.querySelector('tr[data-identity="run-1"] input').click();
 await delay();
 const rapid=messages.at(-1);
 equal(rapid.selected_ids,['run-1'],'rapid clicks keep latest selection');
 check(rapid.revision===second.revision+3,'rapid revisions');
 await render(reply(6,['run-2','run-1'],second));
 equal(checked(),['run-1'],'pending deselection survives stale response');
 await render(reply(7,['run-1'],rapid));
 const empty=await click('run-1');
 await render(reply(8,['run-1'],rapid));
 equal(checked(),[],'empty pending selection is retained');
 await render(reply(9,[],empty));
 check(!doc.querySelector('thead input').checked&&!doc.querySelector('thead input').indeterminate,'empty header');
 // Select all uses the same revision protocol.
 doc.querySelector('thead input').click();await delay();
 const all=messages.at(-1);
 equal(all.selected_ids,['run-1','run-2'],'select all');
 await render(reply(10,['run-1','run-2'],all));
 // External pagination has authoritative contexts, and includes off-page IDs.
 const page2={...reply(11,['run-1','run-2'],all),rows:[{id:'run-3'}],selection_sync:{version:11,context:2,ack:all}};
 await render(page2);
 equal(checked(),[],'off-page selection is not drawn on wrong rows');
 const third=await click('run-3');
 check(third.context===2,'new-page click context');
 equal(third.selected_ids,['run-3'],'events contain only current page');
 await render(reply(10,['run-1','run-2'],all));
 equal(checked(),['run-3'],'obsolete page render ignored');
 check(doc.querySelector('tbody tr').dataset.identity==='run-3','obsolete rows ignored');
 await render({...page2,selected_ids:['run-1','run-2','run-3'],selection_sync:{version:12,context:2,ack:third}});
 const returned={...reply(13,['run-1','run-2','run-3'],third),selection_sync:{version:13,context:3,ack:third}};
 await render(returned);
 equal(checked(),['run-1','run-2'],'return to first page restores selection');
 // A fresh server decision with no pending clicks remains authoritative.
 await render({...returned,selected_ids:[],selection_sync:{version:14,context:3,ack:third}});
 equal(checked(),[],'server selection can be cleared');
 document.querySelector('#result').textContent='PASS';
 }catch(error){document.querySelector('#result').textContent='FAIL '+error.message;}
});</script>''', encoding="utf-8")
    result = subprocess.run([str(browser), "--headless=new", "--disable-gpu", "--no-sandbox",
        "--disable-gpu-sandbox", "--allow-file-access-from-files", f"--user-data-dir={tmp_path / 'profile'}",
        "--dump-dom", "--virtual-time-budget=5000", harness.as_uri()], capture_output=True,
        text=True, encoding="utf-8", errors="replace", timeout=40,
        creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]
