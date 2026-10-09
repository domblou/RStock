"""Lazy UI of compact persisted diagnostics; no scientific work during rendering."""
from functools import lru_cache
from pathlib import Path
from time import perf_counter
import json
import pandas as pd
from .forward_diagnostic import _json, digest
from .selection_diagnostic import MANIFEST, FILES, FORWARD_FILES, detail_name

HELP = {
    "AUC": "Capacité à classer les observations positives devant les négatives. 0,5 correspond au hasard; une seule classe ne permet pas le calcul.",
    "Brier": "Erreur quadratique moyenne des probabilités par rapport aux résultats observés. Plus bas est meilleur; dépend aussi de la fréquence de la cible.",
    "Dispersion": "Variabilité des AUC entre fenêtres; ce n’est pas une mesure d’incertitude fondée sur des essais indépendants.",
    "Concentration": "Part des signaux dans un contexte, rapportée à sa fréquence de marché. Un ratio élevé reste un indice exploratoire.",
    "Robustesse": "Support descriptif: au moins 30 observations et 10 par classe pour l’AUC, 10 signaux pour les résultats des signaux. Les épisodes ne sont pas des essais indépendants.",
}


@lru_cache(maxsize=24)
def _table(path, stamp, sha):
    path=Path(path)
    if digest(path)!=sha: raise ValueError("diagnostic_artifact_changed")
    try: return pd.read_csv(path)
    except pd.errors.EmptyDataError: return pd.DataFrame()


def load_table(output, manifest, name):
    path=Path(output)/name
    sha=manifest.get("artifact_digests",{}).get(name)
    if not sha or not path.exists(): return pd.DataFrame()
    return _table(str(path),path.stat().st_mtime_ns,sha)


def forward_tables(run, run_id=None):
    runs=Path(run).parent
    index=_json(Path(run)/"results/selection_forward_index.json")
    tables, associations, unavailable=[],[],[]
    for rid,ref in index.get("runs",{}).items():
        if run_id is not None and rid != run_id: continue
        path=(runs/ref["manifest"]).resolve()
        if not path.is_relative_to(runs.resolve()) or not path.exists() or digest(path)!=ref["sha256"]:
            unavailable.append(rid);continue
        meta=_json(path)
        if meta.get("source_e2e_run_id")!=Path(run).name:
            unavailable.append(rid);continue
        tables.append(load_table(path.parent,meta,FORWARD_FILES[0]))
        a=load_table(path.parent,meta,FORWARD_FILES[1])
        if not a.empty: associations.append(a.assign(forward_run_id=rid))
    return (pd.concat(tables,ignore_index=True) if tables else pd.DataFrame(),
            pd.concat(associations,ignore_index=True) if associations else pd.DataFrame(), unavailable)


def forward_context_tables(run, run_id=None):
    runs=Path(run).parent;index=_json(Path(run)/"results/selection_forward_index.json")
    tables,compositions=[],[]
    for rid,ref in index.get("runs",{}).items():
        if run_id is not None and rid != run_id: continue
        path=(runs/ref["manifest"]).resolve()
        if not path.is_relative_to(runs.resolve()) or not path.exists() or digest(path)!=ref["sha256"]:continue
        meta=_json(path)
        if meta.get("source_e2e_run_id")!=Path(run).name:continue
        for destination,name in ((tables,FORWARD_FILES[2]),(compositions,FORWARD_FILES[3])):
            frame=load_table(path.parent,meta,name)
            if not frame.empty:destination.append(frame)
    return (pd.concat(tables,ignore_index=True) if tables else pd.DataFrame(),pd.concat(compositions,ignore_index=True) if compositions else pd.DataFrame())


def load_detail(output, manifest, key):
    name="selection_details/"+detail_name(key);path=Path(output)/name
    if digest(path)!=manifest.get("detail_digests",{}).get(name): raise ValueError("diagnostic_detail_changed")
    return _json(path)


def render_selection_diagnostic(st, run: Path):
    started=perf_counter();run=Path(run);output=run/"results"
    meta=_json(output/MANIFEST)
    if not meta:
        attempt=_json(output/"selection_diagnostic_attempt.json")
        st.info("Diagnostic de sélection non disponible pour ce run."+(" "+attempt.get("reason","") if attempt else ""))
        return
    st.markdown("**Synthèse du diagnostic**")
    choices=list(_json(output/"selection_forward_index.json").get("runs",{}))
    choice=st.selectbox("Simulation Forward",choices,key=f"selection-forward-{run.name}") if choices else None
    try:
        funnel=load_table(output,meta,FILES[0]); stability=load_table(output,meta,FILES[3])
        context=load_table(output,meta,FILES[4]);composition=load_table(output,meta,FILES[5])
        forward, associations, unavailable=forward_tables(run,choice)
        forward_context,forward_composition=forward_context_tables(run,choice)
        # Do not pool cumulative, interval and full-run rows in a context chart.
        if not forward_context.empty:
            forward_context=forward_context.loc[forward_context.period_kind.eq("full_run")].copy()
            forward_context["stage"]="Forward · "+forward_context.forward_run_id.astype(str)
            context=pd.concat([context,forward_context],ignore_index=True)
        if not forward_composition.empty:
            forward_composition=forward_composition.loc[forward_composition.period_kind.eq("full_run")].copy()
            forward_composition["stage"]="Forward · "+forward_composition.forward_run_id.astype(str)
            composition=pd.concat([composition,forward_composition],ignore_index=True)
    except (OSError,ValueError,KeyError) as exc:
        st.warning(f"Diagnostic indisponible : {exc}");return
    cols=st.columns(3)
    cols[0].metric("Combinaisons WF",int(meta.get("candidate_count",0)))
    q=funnel.loc[funnel.stage.eq("Qualification promotion")] if not funnel.empty else pd.DataFrame()
    cols[1].metric("Éliminés à la qualification",int(q.rejected.iloc[0]) if not q.empty and pd.notna(q.rejected.iloc[0]) else "?")
    cols[2].metric("Modèles suivis en Forward",forward.source_model_id.nunique() if "source_model_id" in forward else 0)
    wf=funnel.loc[funnel.stage.eq("Walk-forward")] if not funnel.empty else pd.DataFrame()
    if not wf.empty and pd.notna(wf.rejected.iloc[0]):
        st.caption(f"Constat mesuré · {int(wf.rejected.iloc[0])} combinaisons éliminées au WF sur {int(wf.evaluated.iloc[0])} examinées.")
    comparison=meta.get("wf_holdout_summary",{})
    if comparison.get("availability")=="available":
        st.caption(f"Constat mesuré · Parmi {comparison['matched_candidates']} candidats appariés, {comparison['lower_holdout_count']} ont une AUC Holdout de qualification inférieure à leur AUC médiane WF. Définitions et périodes différentes : ce constat ne mesure pas un effet causal.")
    if not forward.empty and "delta_auc" in forward:
        full=forward.loc[forward.period_kind.eq("full_run")]
        measured=full.delta_auc.dropna()
        if len(measured): st.success(f"Constat mesuré · {int(measured.lt(0).sum())} comparaison(s) modèle/simulation sur {len(measured)} ont une AUC Forward inférieure au Holdout comparable.")
    st.info("Indice exploratoire · Les différences par fenêtre et contexte décrivent des associations, sans démontrer leur cause ni justifier une modification des critères.")
    st.caption("Conclusion non démontrée · Surajustement causal, supériorité d’un nouveau critère et performances Forward des candidats non simulés.")
    if meta.get("missing") or unavailable:
        st.warning(f"Couverture incomplète : {len(meta.get('missing',{}))} source(s) absente(s) ou désactivée(s), {len(unavailable)} référence(s) Forward indisponible(s).")
    st.download_button("Exporter le diagnostic scientifique (.zip)",data=lambda: _export(run),file_name=f"diagnostic_selection_{run.name}.zip",mime="application/zip",key=f"selection-export-{run.name}",on_click="ignore")
    st.markdown("**A. Entonnoir de sélection**")
    st.caption("Conversion au sein de chaque étape. Le préfiltre compte des couples cible/prédicteur; le WF compte des combinaisons. Le Holdout est une évaluation, sa décision appartient à la qualification.")
    if not funnel.empty: st.bar_chart(funnel.set_index("stage")[["evaluated","retained","rejected"]].rename(columns={"evaluated":"Examinés","retained":"Retenus","rejected":"Rejetés"}),stack=False)
    st.markdown("**B. Stabilité Walk-forward**", help=HELP["AUC"]+" "+HELP["Dispersion"])
    st.caption(HELP["AUC"]+" "+HELP["Dispersion"])
    if not stability.empty:
        if "selection_stage" in stability:
            level=st.selectbox("Décision à comparer",stability.selection_stage.drop_duplicates().tolist(),key=f"selection-stability-level-{run.name}")
            chart_rows=stability.loc[stability.selection_stage.eq(level)]
        else: chart_rows=stability
        chart=chart_rows.pivot(index="Window",columns="population",values="auc_median")
        st.line_chart(chart)
        st.caption("AUC médiane par fenêtre; quartiles, extrêmes, effectifs et dates dans les détails. Les fenêtres ne sont pas indépendantes.")
    else: st.info("Fenêtres WF absentes ou purgées.")
    st.markdown("**C. Généralisation WF → Holdout → Forward**")
    st.caption("WF initial, Holdout de qualification et Holdout comparable gardent leurs définitions propres. Les deltas Forward existants utilisent le Holdout comparable.")
    if forward.empty: st.info("Aucun diagnostic Forward lié disponible; aucune performance n’est attribuée aux candidats non simulés.")
    else:
        part=forward.loc[forward.forward_run_id.eq(choice)]
        labels=part[["period_kind","horizon"]].drop_duplicates()
        periods=[(str(r.period_kind),r.horizon) for r in labels.itertuples(index=False)]
        period=st.selectbox("Période persistée",periods,format_func=lambda x:f"{x[0]} · {x[1]} séances",key=f"selection-period-{run.name}")
        selected=part.loc[part.period_kind.eq(period[0]) & part.horizon.eq(period[1])]
        if {"ROCAUCMedian","auc"}.issubset(selected):
            st.scatter_chart(selected.rename(columns={"ROCAUCMedian":"AUC médiane WF","auc":"AUC Forward"}),x="AUC médiane WF",y="AUC Forward")
        st.caption("Comparaison des modèles effectivement simulés; biais de sélection résiduel. Les corrélations T0/Forward restent exploratoires, sans test de causalité.")
    st.markdown("**D. Influence des régimes SPY**", help=HELP["Brier"]+" "+HELP["Concentration"]+" "+HELP["Robustesse"])
    st.caption(HELP["Brier"]+" "+HELP["Concentration"]+" "+HELP["Robustesse"])
    if composition.empty: st.info("Contexte SPY absent : diagnostic désactivé, données manquantes ou étape héritée sans contexte.")
    else:
        axis=st.selectbox("Contexte",composition.axis.drop_duplicates().tolist(),format_func=lambda x:{"regime":"Régime économique","trend":"Tendance","drawdown":"Recul depuis le sommet","volatility":"Volatilité"}.get(x,x),key=f"selection-axis-{run.name}")
        c=composition.loc[composition.axis.eq(axis)]
        st.bar_chart(c.pivot_table(index="band",columns="stage",values="sessions",aggfunc="sum"))
        st.caption("Séances distinctes par étape. Les épisodes peuvent être séparés dans le temps sans être statistiquement indépendants. Les contextes sont connus à J−1.")
        if not context.empty:
            stage=st.selectbox("Étape à comparer par contexte",context.stage.drop_duplicates().tolist(),key=f"selection-context-stage-{run.name}")
            selected=context.loc[context.axis.eq(axis)&context.stage.eq(stage)]
            if "median_model_window_auc" in selected:
                st.bar_chart(selected.pivot_table(index="band",columns="population",values="median_model_window_auc",aggfunc="median"))
                st.caption("Médianes descriptives des AUC modèle/fenêtre; ce ne sont pas des AUC recalculées sur une population fusionnée. Vérifier les effectifs et indicateurs de support.")
    if st.toggle("Afficher les tableaux détaillés",value=False,key=f"selection-details-{run.name}"):
        for label,table in (("Populations et conversions",funnel),("Motifs de rejet",load_table(output,meta,FILES[1])),("Distributions WF",stability),("Régimes et exposition",context),("Composition historique",composition),("Généralisation",forward),("Associations exploratoires T0",associations)):
            st.write(label);st.dataframe(table,hide_index=True,width="stretch")
        st.write("Critères persistés et données manquantes");st.json({"criteria":meta.get("criteria"),"qualification_policy":meta.get("qualification_policy"),"missing":meta.get("missing"),"limitations":meta.get("limitations")})
        candidates=load_table(output,meta,FILES[2])
        if "model_key" in candidates and not candidates.empty:
            key=st.selectbox("Combinaison à examiner",candidates.model_key.tolist(),key=f"selection-model-{run.name}")
            detail=load_detail(output,meta,key)
            st.json(detail.get("candidate",{}))
            st.dataframe(pd.DataFrame(detail.get("windows",[])),hide_index=True,width="stretch")
            st.dataframe(pd.DataFrame(detail.get("context",[])),hide_index=True,width="stretch")
    st.caption(f"Agrégats persistés · préparation des données d’affichage : {perf_counter()-started:.3f} s")


def _export(run):
    from .forward_export import build_selection_export
    return build_selection_export(run)
