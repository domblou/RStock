"""Shared analysis controls and lazy persisted-result views. No scientific work."""
import json
import uuid
from dataclasses import replace

from .domain import JobType
from .run_detail_tabs import render_lazy_tabs, tabs_for_job
from .scientific_analysis import compatible_analyses, get_analysis


def render_analysis_launcher(st, service, source_run_id, *, key, navigate):
    """Only the catalogue selector belongs in the action menu."""
    with st.popover("Lancer une analyse"):
        try:
            definitions = compatible_analyses(service.run_service.repository, source_run_id)
        except (OSError, ValueError) as error:
            st.warning(str(error))
            return None
        if not definitions:
            st.info("Aucune analyse programmée compatible.")
            return None
        return st.selectbox("Analyse programmée", [x.id for x in definitions],
            index=None, placeholder="Choisir une analyse",
            format_func=lambda value: next(x.label for x in definitions if x.id == value), key=key+"-type")


def render_analysis_form(st, service, source_run_id, selected, *, key, navigate):
    """Inline configuration below the actions, outside their columns/popover."""
    request_key = key+"-request"
    st.session_state.setdefault(request_key, uuid.uuid4().hex)
    if selected:
        item = get_analysis(selected)
        with st.container(border=True):
            st.subheader(item.label)
            st.caption(item.description)
            parameters = item.render_parameters(key) if item.render_parameters else {}
            try:
                preflight = service.analysis_preflight(source_run_id, selected, parameters)
            except (OSError, ValueError, KeyError, TypeError) as error:
                st.warning(str(error))
                preflight = {"ready": False}
            temporal = preflight.get("temporal_summary")
            if temporal:
                st.write(f"WF disponible : {temporal['wf_start']} → {temporal['wf_end']} · "
                         f"{temporal['total_sessions']} séances évaluables")
                st.write(f"Sélection : {temporal['selection_sessions']} séances · "
                         f"dernière séance : {temporal['selection_end']}")
                st.write(f"Évaluation : {temporal['evaluation_sessions']} séances · "
                         f"{temporal['evaluation_start']} → {temporal['evaluation_end']}")
                st.caption("Calendrier commun issu des prédictions archivées. La proportion est arrondie "
                           "à l'entier inférieur ; toutes les séances restantes servent à l'évaluation.")
                st.caption("Persistance des relations à travers les modèles réentraînés du WF ; "
                           "ceci ne valide pas la survie de modèles figés.")
            for reason in (*preflight.get("warnings", []), *preflight.get("reasons", [])):
                st.warning(reason)
            confirmed = st.checkbox("Confirmer explicitement le lancement", key=key+"-confirm")
            if st.button("Lancer l’analyse sélectionnée", key=key+"-submit",
                         disabled=not (confirmed and preflight.get("ready"))):
                try:
                    result = service.start_analysis(source_run_id, selected,
                        preflight.get("parameters", parameters), request_id=st.session_state[request_key])
                except (OSError, ValueError, RuntimeError) as error:
                    st.error(str(error))
                else:
                    st.success(f"Analyse : {result.run_id}")
                    navigate("detail", [result.run_id])
    previous = service.analyses_for_source(source_run_id)
    if previous:
        with st.expander("Analyses précédentes"):
            choice = st.selectbox("Ouvrir une analyse", [str(x["run_id"]) for x in previous], key=key+"-previous")
            if st.button("Ouvrir l’analyse", key=key+"-open"):
                navigate("detail", [choice])
            if st.button("Préparer une exécution indépendante", key=key+"-new-execution"):
                st.session_state[request_key] = uuid.uuid4().hex
                st.rerun()


def render_analysis_detail(st, service, run_id, status, detail, *, render_resources, render_technical, navigate):
    manifest = detail.get("configuration", {}).get("scientific_analysis") or {}
    root = service.run_service.repository.run_directory(run_id)
    try:
        item = get_analysis(manifest.get("analysis_id", ""))
    except ValueError as error:
        st.error(str(error)); return
    def summary():
        st.subheader(item.label)
        st.json({"status": status, "progress": detail.get("progress"), "parameters": manifest.get("parameters"),
                 "source_run_id": manifest.get("source_run_id"), "definition_version": manifest.get("definition_version")})
        path = root / "results/analysis_result.json"
        if status.get("status") != "completed":
            st.info("Exécution non terminée : les artefacts disponibles sont partiels.")
        elif path.is_file():
            st.json(json.loads(path.read_text(encoding="utf-8")))
        if st.button("Ouvrir le run source", key=f"analysis-source-{run_id}"):
            navigate("detail", [manifest["owner_run_id"]])
    def technical():
        st.json(manifest)
        checkpoint = root / "checkpoints/manifest.json"
        if checkpoint.is_file(): st.json(json.loads(checkpoint.read_text(encoding="utf-8")))
        render_technical(run_id, detail)
    renderers = {"summary": summary, "resources": lambda:render_resources(run_id), "technical": technical}
    result_tabs = tuple(replace(tab, renderer_key=tab.key) for tab in item.result_tabs)
    tabs = (*tabs_for_job(JobType.SCIENTIFIC_ANALYSIS), *result_tabs)
    if item.render_results:
        result_detail = {**detail, "run_directory": str(root)}
        for tab in result_tabs:
            renderers[tab.renderer_key] = lambda tabkey=tab.key:item.render_results(tabkey, run_id, result_detail)
    render_lazy_tabs(st, tabs, renderers, key=f"run-detail-{run_id}")
