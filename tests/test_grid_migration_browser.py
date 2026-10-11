"""Browser checks with the real presentation adapters/configs for each migration lot."""
import json
import os
from pathlib import Path
import subprocess
import sys

import pandas as pd
import pytest
import streamlit as st

from rstock.application import grid, streamlit_app as app
from rstock.application.grid_dataframe import prepare_dataframe


def lot_payload(lot):
    if lot == 1:
        frame = pd.DataFrame({"Nom": ["US Tech Growth", "Dividend Aristocrats"], "Nombre de symboles": [35, 69], "Type": ["Standard", "Standard"], "Benchmark": ["QQQ", "SPY"], "Source": ["Fichier", "Fichier"], "Dernière modification": ["2026-07-06", "2026-07-07"]})
        configs = {}
    elif lot == 2:
        frame = pd.DataFrame({"Batch": [1, 2], "Run ID": ["batch-a", "batch-b"], "Statut": ["running", "completed"], "Progression": [26.6, 100.], "Range start": [0, 100]})
        configs = app._grid_column_help_config(frame.columns, app._WF_BATCH_COLUMN_HELP, {"Progression": st.column_config.ProgressColumn(min_value=0., max_value=100., format="%.1f%%")})
    elif lot == 3:
        frame = pd.DataFrame({"Précision": [.75, .82], "Recall": [.5, .6], "F1": [.6, .7], "Seuil": [.55, .6]})
        configs = app._grid_column_help_config(frame.columns, app._THRESHOLD_SENSITIVITY_COLUMN_HELP, {"Précision": st.column_config.NumberColumn(format="percent"), "Seuil": st.column_config.NumberColumn(format="%.4f")})
    elif lot == 4:
        frame = pd.DataFrame({"Date": ["2026-07-06", "2026-07-07"], "Cible": ["AAPL", "MSFT"], "Modèle / combinaison": ["model-a", "model-b"], "Prix d’achat": [123.45, 67.89], "Prix de vente": [124.9, 67.], "Quantité": [10, 20], "P&L": [14.5, -17.8], "Note": ["Transaction réelle", "Deuxième transaction"]})
        configs = {}
    elif lot == 5:
        frame = pd.DataFrame({"Date": ["2026-07-06", "2026-07-07"], "Cible": ["AAPL", "MSFT"], "Prédicteurs": ["NVDA", "AMD"], "P(Up)": ["75 %", "80 %"], "Rendement moyen historique (63 séances)": ["+2,5 %", "-1,2 %"], "Trades gagnants": ["70 %", "45 %"]})
        configs = app._surveillance_column_config(frame.columns)
        frame = app._styled_surveillance_table(frame, ("Rendement moyen historique (63 séances)", "Trades gagnants"))
    elif lot == 6:
        frame = pd.DataFrame({"model_id": ["model-a", "model-b"], "Cible": ["AAPL", "MSFT"], "Prédicteurs": ["NVDA", "AMD"], "Statut": ["Actif", "Observation"], "Rendement moyen": ["+2,5 %", "-1,2 %"], "Trades gagnants": ["70 %", "45 %"], "P&L cumulé": ["+250 $", "-120 $"], "Drawdown": ["-1,0 %", "-2,0 %"], "Tendance 63": [[0, .1, -.1, .3], [.2, .1, None, -.2]]})
        configs = app._models_grid_column_config((-1., 1.))
        frame = app.style_directional_columns(frame, ("Rendement moyen", "P&L cumulé", "Drawdown")).map(app.winning_trades_display_style, subset=["Trades gagnants"])
    else:
        frame = pd.DataFrame({"P(Up)": [.75, .82], "Seuil Up": [.55, .6], "Prix achat": [123.45, 67.89], "Prix vente": [124.9, 67.], "Rendement": [.012, -.01], "Montant investi": [10000., 10000.], "Profit / perte": [120., -100.]})
        configs = {"P(Up)": st.column_config.NumberColumn(format="%.4f"), "Rendement": st.column_config.NumberColumn(format="percent"), "Prix achat": st.column_config.NumberColumn(format="%.2f $"), "Profit / perte": st.column_config.NumberColumn(format="%.2f $")}
    rows, columns, options, styles, all_columns = prepare_dataframe(frame, column_config=configs, hide_index=True)
    return dict(rows=rows, columns=columns, column_options=options, cell_styles=styles,
                all_columns=all_columns, row_id="__grid_id", selection_mode="single", page_size=1)


@pytest.mark.parametrize("lot", range(1, 8), ids=lambda value: f"lot{value}")
def test_lot_real_browser(lot, tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    payload = lot_payload(lot)
    harness = tmp_path / "lot.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><style>body{font:13px sans-serif}</style><div id="result">WAIT</div>
<iframe style="width:1220px;height:520px;border:0" src="''' + frontend.as_uri() + '''"></iframe><script>
const frame = document.querySelector('iframe'), args = ''' + json.dumps(payload) + ''';
const events=[];
function check(ok,message){if(!ok)throw Error(message);}
window.addEventListener('message', event => {
 if(event.data.type==='streamlit:setComponentValue')events.push(event.data.value);
 if(event.data.type!=='streamlit:componentReady')return;
 frame.contentWindow.postMessage({type:'streamlit:render',args},'*');
 setTimeout(()=>{try{
 const doc=frame.contentDocument, win=frame.contentWindow;
 const heads=[...doc.querySelectorAll('th')].slice(1);
 args.columns.forEach((c,i)=>check(heads[i].title===(args.column_options[c].help||''),'help '+c));
 const help=doc.querySelector('th .help');
 if(help){help.click();check(doc.querySelector('dialog pre').textContent===help.title,'help dialog');doc.querySelector('dialog').close();}
 check(win.csvText().includes(args.all_columns[0]),'CSV');
 check(win.copyMatrix().includes(args.column_options[args.columns[0]].label),'copy');
 for(const [id,cols] of Object.entries(args.cell_styles)){for(const [c,styles] of Object.entries(cols)){
  if(id!==args.rows[0].__grid_id)continue;
  const cell=doc.querySelectorAll('tbody tr')[0].querySelectorAll('td')[args.columns.indexOf(c)+1];
  for(const [property,value] of Object.entries(styles)){const expected=doc.createElement('span');expected.style.setProperty(property,value);check(cell.style.getPropertyValue(property)===expected.style.getPropertyValue(property),'conditional style '+property);}
 }}
 if(args.columns.some(c=>args.column_options[c].type==='line_chart'))check(doc.querySelector('svg path').getAttribute('d').includes('L'),'sparkline');
 if(args.columns.some(c=>args.column_options[c].type==='progress'))check(doc.querySelector('[role=progressbar]').getAttribute('aria-valuenow')==='26.6','progress');
 const search=doc.querySelector('input[type=search]');search.value='impossible-no-match';search.dispatchEvent(new Event('input'));check(doc.querySelector('.empty'),'search empty');
 search.value='';search.dispatchEvent(new Event('input'));
 doc.querySelectorAll('#pager button')[1].click();check(doc.querySelector('#pager').textContent.includes('page 2 / 2'),'page');
 doc.querySelector('tbody input').click();
 doc.querySelectorAll('#pager button')[0].click();
 setTimeout(()=>{try{
  check(events.at(-1)[0]===args.rows[1].__grid_id,'selected ID after pagination');
  doc.querySelector('tbody input').click();
  setTimeout(()=>{try{
   check(events.at(-1).length===1&&events.at(-1)[0]===args.rows[0].__grid_id,'single row replacement');
   doc.querySelector('tbody input').click();
   setTimeout(()=>{try{check(events.at(-1).length===0,'deselection');document.querySelector('#result').textContent='PASS';}catch(e){document.querySelector('#result').textContent='FAIL '+e.message;}},100);
  }catch(e){document.querySelector('#result').textContent='FAIL '+e.message;}},100);
 }catch(e){document.querySelector('#result').textContent='FAIL '+e.message;}},100);
 }catch(e){document.querySelector('#result').textContent='FAIL '+e.message;}},100);
});</script>''', encoding="utf-8")
    qa = Path(os.environ.get("RSTOCK_GRID_QA_DIR", str(tmp_path)))
    qa.mkdir(parents=True, exist_ok=True)
    result = subprocess.run([str(browser), "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-gpu-sandbox", "--allow-file-access-from-files",
                             f"--user-data-dir={tmp_path / 'profile'}", "--dump-dom", "--virtual-time-budget=3000", "--window-size=1270,680",
                             f"--screenshot={qa / f'lot{lot}.png'}", harness.as_uri()], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=40,
                            creationflags=subprocess.CREATE_NO_WINDOW if sys.platform=="win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]


def test_tools_export_copy_resize_columns_refresh_and_dark_mobile(tmp_path):
    browser = Path("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
    if not browser.exists():
        pytest.skip("Local Edge browser is unavailable")
    frontend = Path(grid.__file__).with_name("history_grid_frontend") / "index.html"
    payload = lot_payload(6)
    payload.update(page_size=None, selection_mode="multi")
    harness = tmp_path / "tools.html"
    harness.write_text('''<!doctype html><meta charset="utf-8"><div id="result">WAIT</div>
<iframe style="width:360px;height:520px;border:0" src="''' + frontend.as_uri() + '''"></iframe><script>
const frame=document.querySelector('iframe'), args=''' + json.dumps(payload) + ''';
const theme={backgroundColor:'#0e1117',textColor:'#fafafa',secondaryBackgroundColor:'#262730',primaryColor:'#ff4b4b'};
const delay=()=>new Promise(resolve=>setTimeout(resolve,100));
function check(ok,message){if(!ok)throw Error(message);}
window.addEventListener('message',async event=>{
 if(event.data.type!=='streamlit:componentReady')return;
 try {
 frame.contentWindow.postMessage({type:'streamlit:render',args,theme},'*');await delay();
 const win=frame.contentWindow,doc=frame.contentDocument;
 check(win.getComputedStyle(doc.body).backgroundColor==='rgb(14, 17, 23)','dark theme');
 check(doc.querySelector('#grid').scrollWidth>doc.querySelector('#grid').clientWidth,'mobile scroll');
 let blob=null,filename=null,copied=null;
 win.URL.createObjectURL=value=>{blob=value;return 'blob:test'};
 win.URL.revokeObjectURL=()=>{};
 win.HTMLAnchorElement.prototype.click=function(){filename=this.download;};
 [...doc.querySelectorAll('#toolbar button')].find(b=>b.textContent==='Exporter CSV').click();
 check(filename==='rstock.csv','export action');check((await blob.text()).includes('model-a'),'raw hidden ID exported');
 Object.defineProperty(win.navigator,'clipboard',{value:{writeText:async value=>{copied=value}},configurable:true});
 [...doc.querySelectorAll('#toolbar button')].find(b=>b.textContent==='Copier').click();await delay();
 check(copied&&copied.includes('AAPL')&&copied.includes('MSFT'),'clipboard table');
 const cells=doc.querySelectorAll('td[data-row]');
 cells[0].dispatchEvent(new win.PointerEvent('pointerdown',{button:0}));
 cells[1].dispatchEvent(new win.PointerEvent('pointerenter',{buttons:1}));
 check(doc.querySelectorAll('td.range').length===2,'range selection');
 check(win.copyMatrix()==='AAPL\\tNVDA','range copy');
 const header=doc.querySelectorAll('th')[1],handle=header.querySelector('.resize');
 handle.setPointerCapture=()=>{};
 handle.dispatchEvent(new win.PointerEvent('pointerdown',{button:0,clientX:100}));
 handle.dispatchEvent(new win.PointerEvent('pointermove',{clientX:160}));
 handle.dispatchEvent(new win.PointerEvent('pointerup',{clientX:160}));
 const resized=doc.querySelectorAll('th')[1].style.width;
 [...doc.querySelectorAll('#toolbar button')].find(b=>b.textContent==='Colonnes').click();
 const labels=[...doc.querySelectorAll('dialog label')];
 const status=labels.find(label=>label.textContent.trim()==='Statut');
 status.querySelector('input').click();
 doc.querySelector('dialog').close();
 check(![...doc.querySelectorAll('th .header-label')].some(h=>h.textContent==='Statut'),'hide column');
 frame.contentWindow.postMessage({type:'streamlit:render',args,theme},'*');await delay();
 check(doc.querySelectorAll('th')[1].style.width===resized,'resize survives refresh');
 check(![...doc.querySelectorAll('th .header-label')].some(h=>h.textContent==='Statut'),'hidden survives refresh');
 const help=doc.querySelector('th .help');help.click();
 check(doc.querySelector('dialog pre').textContent===help.title,'all help visible');doc.querySelector('dialog').close();
 doc.querySelector('tbody td[data-row]').dispatchEvent(new win.MouseEvent('dblclick',{bubbles:true}));
 check(doc.querySelector('dialog pre').textContent==='AAPL','cell detail');doc.querySelector('dialog').close();
 document.querySelector('#result').textContent='PASS';
 }catch(e){document.querySelector('#result').textContent='FAIL '+e.message;}
});</script>''', encoding="utf-8")
    qa = Path(os.environ.get("RSTOCK_GRID_QA_DIR", str(tmp_path)))
    qa.mkdir(parents=True, exist_ok=True)
    result = subprocess.run([str(browser), "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-gpu-sandbox", "--allow-file-access-from-files",
                             f"--user-data-dir={tmp_path / 'profile'}", "--dump-dom", "--virtual-time-budget=3000", "--window-size=800,680",
                             f"--screenshot={qa / 'dark-mobile.png'}", harness.as_uri()], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=40,
                            creationflags=subprocess.CREATE_NO_WINDOW if sys.platform=="win32" else 0)
    assert '<div id="result">PASS</div>' in result.stdout, result.stdout + result.stderr[-1000:]
