/* Shared presentation tools. Values are inserted as text, never as HTML. */
function displayValue(row, column) {
  const option = options[column] || {};
  return row[option.display_key || column] ?? "—";
}
function showDetail(title, value) {
  const dialog = document.getElementById("detail"); dialog.replaceChildren();
  const heading = document.createElement("h3"), content = document.createElement("pre"), close = document.createElement("button"), copy = document.createElement("button");
  heading.textContent = title; content.textContent = value; close.textContent = "Fermer"; copy.textContent = "Copier";
  close.onclick = () => dialog.close(); copy.onclick = () => copyText(value);
  dialog.append(heading, content, copy, close); dialog.showModal();
}
function applyCellStyle(cell, styles) {
  const allowed = new Set(["color", "background-color", "font-weight", "font-style", "text-decoration", "text-align"]);
  for (const [name, value] of Object.entries(styles)) if (allowed.has(name)) cell.style.setProperty(name, value);
}
function renderSpecialCell(cell, row, column, option) {
  const value = row[column];
  if (option.type === "progress" && typeof value === "number") {
    const track = document.createElement("div"), fill = document.createElement("div");
    const min = option.min_value ?? 0, max = option.max_value ?? 100;
    track.className = "progress-track"; fill.className = "progress-fill";
    fill.style.width = Math.max(0, Math.min(100, 100 * (value - min) / (max - min || 1))) + "%";
    if (option.color) fill.style.backgroundColor = option.color;
    track.role = "progressbar"; track.ariaValueMin = min; track.ariaValueMax = max; track.ariaValueNow = value;
    track.append(fill); cell.append(track);
  } else if (["line_chart", "area_chart", "bar_chart"].includes(option.type) && Array.isArray(value)) {
    cell.replaceChildren();
    const points = value.map(v => typeof v === "number" && Number.isFinite(v) ? v : null);
    const finite = points.filter(v => v !== null);
    if (!finite.length) { cell.textContent = "—"; return; }
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.classList.add("sparkline"); svg.setAttribute("viewBox", "0 0 160 28"); svg.role = "img";
    svg.setAttribute("aria-label", (option.label || column) + " : " + value.join(", "));
    const title = document.createElementNS(svg.namespaceURI, "title"); title.textContent = value.join(", "); svg.append(title);
    const min = option.y_min ?? Math.min(...finite), max = option.y_max ?? Math.max(...finite);
    const xy = (v, i) => [2 + i * 156 / Math.max(1, points.length - 1), 26 - (v - min) * 24 / (max - min || 1)];
    if (option.type === "bar_chart") {
      points.forEach((v, i) => { if (v === null) return; const [x,y] = xy(v,i), rect = document.createElementNS(svg.namespaceURI, "rect"); rect.setAttribute("x", x); rect.setAttribute("y", y); rect.setAttribute("width", Math.max(1, 150 / points.length)); rect.setAttribute("height", 26-y); rect.setAttribute("fill", option.color || "#2563eb"); svg.append(rect); });
    } else {
      let path = "", next = true;
      points.forEach((v, i) => { if (v === null) { next = true; return; } const [x,y] = xy(v,i); path += (next ? "M" : "L") + x + " " + y + " "; next = false; });
      const line = document.createElementNS(svg.namespaceURI, "path"); line.setAttribute("d", path);
      line.setAttribute("fill", "none"); line.setAttribute("stroke", option.color || "#2563eb"); line.setAttribute("stroke-width", "1.5"); svg.append(line);
    }
    cell.append(svg); cell.title = String(displayValue(row, column));
  } else if (option.type === "checkbox") {
    cell.replaceChildren(); const check = document.createElement("input"); check.type = "checkbox"; check.checked = value === true; check.disabled = true; check.ariaLabel = option.label || column; cell.append(check);
  } else if (option.type === "link" && typeof value === "string" && /^https?:\/\//.test(value)) {
    cell.replaceChildren(); const link = document.createElement("a"); link.href = value; link.textContent = option.display_text || displayValue(row,column); link.target = "_blank"; link.rel = "noopener noreferrer"; link.onclick = event => event.stopPropagation(); cell.append(link);
  }
}
function addResize(cell, column) {
  const handle = document.createElement("span"); handle.className = "resize"; handle.title = "Redimensionner la colonne";
  handle.onclick = event => event.stopPropagation();
  handle.onpointerdown = event => {
    event.stopPropagation(); event.preventDefault(); const start = event.clientX, width = cell.getBoundingClientRect().width;
    handle.setPointerCapture(event.pointerId);
    handle.onpointermove = move => {
      const next = Math.max(60, width + move.clientX - start);
      resizedColumns.set(column, next);
      options[column] = {...options[column], width: next, min_width: next, max_width: next};
      cell.style.width = next + "px"; cell.style.minWidth = next + "px"; cell.style.maxWidth = next + "px";
    };
    handle.onpointerup = () => { handle.onpointermove = null; range = null; draw(); };
  }; cell.append(handle);
}
function paintRange() {
  for (const cell of document.querySelectorAll("td[data-row]")) {
    const r = Number(cell.dataset.row), c = Number(cell.dataset.column);
    cell.classList.toggle("range", !!range && range.dragged && r >= Math.min(range.start[0],range.end[0]) && r <= Math.max(range.start[0],range.end[0]) && c >= Math.min(range.start[1],range.end[1]) && c <= Math.max(range.start[1],range.end[1]));
  }
}
function copyMatrix() {
  if (range) {
    const cols = activeColumns.slice(Math.min(range.start[1],range.end[1]), Math.max(range.start[1],range.end[1])+1);
    return pageView.slice(Math.min(range.start[0],range.end[0]), Math.max(range.start[0],range.end[0])+1).map(row => cols.map(c => displayValue(row,c)).join("\t")).join("\n");
  }
  const chosen = rows.filter(row => selected.has(identity(row)));
  const population = chosen.length ? chosen : rows;
  return [activeColumns.map(c => (options[c] || {}).label || c).join("\t"), ...population.map(row => activeColumns.map(c => displayValue(row,c)).join("\t"))].join("\n");
}
async function copyText(value) {
  try {
    if (navigator.clipboard && window.isSecureContext) await navigator.clipboard.writeText(value);
    else {
      const area = document.createElement("textarea"); area.value = value; document.body.append(area); area.select();
      const copied = document.execCommand("copy"); area.remove(); if (!copied) throw Error("clipboard");
    }
    document.getElementById("status").textContent = "Copié.";
  } catch (_) { showDetail("Texte à copier (Ctrl+C)", value); }
}
function csvText() {
  const escape = value => '"' + String(value ?? "").replaceAll('"','""') + '"';
  // Match native data export: raw values, including hidden source columns.
  return [allColumns.map(c => escape((options[c] || {}).label || c)).join(","), ...rows.map(row => allColumns.map(c => escape(Array.isArray(row[c]) || (row[c] && typeof row[c] === "object") ? JSON.stringify(row[c]) : row[c])).join(","))].join("\r\n");
}
function exportCSV() {
  const blob = new Blob(["\ufeff", csvText()], {type:"text/csv;charset=utf-8"}), url = URL.createObjectURL(blob), link = document.createElement("a");
  link.href = url; link.download = "rstock.csv"; link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
}
function columnMenu() {
  const dialog = document.getElementById("detail"); dialog.replaceChildren();
  const title = document.createElement("h3"); title.textContent = "Colonnes"; dialog.append(title);
  allColumns.forEach(column => {
    const label = document.createElement("label"), check = document.createElement("input"), move = document.createElement("button");
    check.type = "checkbox"; check.checked = columns.includes(column) && !hiddenColumns.has(column);
    check.onchange = () => { if (check.checked) { hiddenColumns.delete(column); if (!columns.includes(column)) columns.push(column); } else hiddenColumns.add(column); userColumnOrder=[...columns]; range=null; draw(); };
    label.append(check, document.createTextNode(" " + ((options[column] || {}).label || column))); label.title = (options[column] || {}).help || "";
    move.textContent = "←"; move.title = "Déplacer la colonne vers la gauche";
    move.onclick = () => { const index = columns.indexOf(column); if (index > 0) [columns[index-1],columns[index]] = [columns[index],columns[index-1]]; userColumnOrder=[...columns]; range=null; draw(); };
    const line = document.createElement("div"); line.append(label, move); dialog.append(line);
  });
  const close = document.createElement("button"); close.textContent="Fermer"; close.onclick=()=>dialog.close(); dialog.append(close); dialog.showModal();
}
function drawToolbar() {
  const bar = document.getElementById("toolbar"); bar.replaceChildren(); bar.hidden = !toolbarEnabled;
  if (!toolbarEnabled) return;
  const search = document.createElement("input"); search.type="search"; search.placeholder="Rechercher dans la grille"; search.ariaLabel=search.placeholder; search.value=query;
  search.oninput = () => { query=search.value; page=0; range=null; draw(); }; bar.append(search);
  for (const [name, action] of [["Exporter CSV",exportCSV], ["Copier",()=>copyText(copyMatrix())], ["Colonnes",columnMenu], ["Plein écran",async()=>{ try { if (document.fullscreenElement) await document.exitFullscreen(); else await document.documentElement.requestFullscreen(); } catch (_) { document.getElementById('status').textContent='Le navigateur ne permet pas le plein écran.'; } }]]) {
    const button=document.createElement("button"); button.textContent=name; button.onclick=action; bar.append(button);
  }
}
document.addEventListener("keydown", event => {
  if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "c" && !["INPUT","TEXTAREA"].includes(event.target.tagName) && !document.querySelector("dialog[open]")) { event.preventDefault(); copyText(copyMatrix()); }
});
document.addEventListener("fullscreenchange", () => draw());
