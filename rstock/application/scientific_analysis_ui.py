"""Shared analysis controls and lazy persisted-result views. No scientific work."""
import json
import uuid
from dataclasses import replace

from .domain import JobType
from .run_detail_tabs import render_lazy_tabs, tabs_for_job
from .scientific_analysis import compatible_analyses, get_analysis, validate_interpretation, interpretation_inputs_hash


def _read_summary_json(path):
    if not path.is_file() or path.stat().st_size > 2 * 1024 * 1024:
        return None
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    return value if isinstance(value, dict) else None


def render_scientific_interpretation(st, item, result, manifest, status, persisted=None):
    interpretation = validate_interpretation(item.interpret_results(result, manifest, status))
    if persisted is not None:
        try:
            if persisted.get("input_sha256") == interpretation_inputs_hash(result, manifest, status) and all(
                persisted.get(key) == interpretation[key] for key in ("version", "understanding", "findings", "limits", "verdict")):
                interpretation = validate_interpretation(persisted)
            else:
                interpretation = validate_interpretation(item.interpret_results(None, manifest, status))
        except (TypeError, ValueError):
            interpretation = validate_interpretation(item.interpret_results(None, manifest, status))
    st.subheader("Interprétation scientifique")
    for label, key in (("Comprendre les résultats", "understanding"),
                       ("Interprétation des résultats", "findings"),
                       ("Limites et verdict scientifique", "limits")):
        st.markdown(f"**{label}**")
        for paragraph in interpretation[key]:
            st.write(paragraph)
        if key == "limits":
            st.write("Verdict : " + interpretation["verdict"])


def render_analysis_launcher(st, service, source_run_id, *, key, navigate):
    """Keep the launch button and catalogue selector on the action row."""
    try:
        definitions = compatible_analyses(service.run_service.repository, source_run_id)
    except (OSError, ValueError) as error:
        st.warning(str(error))
        definitions = []
    st.button("Lancer une analyse", key=key+"-configure",
                        disabled=not st.session_state.get(key+"-type"))
    selected = st.selectbox("Sélectionner une analyse", [x.id for x in definitions],
        index=None, placeholder="Sélectionner une analyse", label_visibility="collapsed",
        width=315, disabled=not definitions,
        format_func=lambda value: next(x.label for x in definitions if x.id == value), key=key+"-type")
    return selected


def render_analysis_form(st, service, source_run_id, selected, *, key, navigate):
    """Inline configuration below the action row."""
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


def render_analysis_detail(st, service, run_id, status, detail, *, render_technical, navigate):
    manifest = detail.get("configuration", {}).get("scientific_analysis") or {}
    root = service.run_service.repository.run_directory(run_id)
    try:
        item = get_analysis(manifest.get("analysis_id", ""))
    except ValueError as error:
        st.error(str(error)); return
    def summary():
        st.subheader(item.label)
        prepared = _read_summary_json(root / "results/analysis_preparation.json")
        result = _read_summary_json(root / "results/analysis_result.json")
        persisted = _read_summary_json(root / "results/scientific_interpretation.json")
        effective_manifest = prepared if prepared is not None else manifest
        render_scientific_interpretation(st, item, result, effective_manifest, status, persisted)
        if status.get("status") != "completed":
            st.info("Exécution non terminée : les artefacts disponibles sont partiels.")
        with st.expander("Détails du résumé (JSON)", expanded=False):
            st.json({"status": status, "progress": detail.get("progress"), "parameters": manifest.get("parameters"),
                     "source_run_id": manifest.get("source_run_id"), "definition_version": manifest.get("definition_version")})
            if prepared is not None:
                st.json({"parameters_effectifs": prepared.get("parameters")})
            if status.get("status") == "completed" and result is not None:
                st.json(result)
        if st.button("Ouvrir le run source", key=f"analysis-source-{run_id}"):
            navigate("detail", [manifest["owner_run_id"]])
    def provenance():
        st.json(manifest)
        checkpoint = root / "checkpoints/manifest.json"
        if checkpoint.is_file(): st.json(json.loads(checkpoint.read_text(encoding="utf-8")))
    renderers = {"summary": summary, "technical": lambda: render_technical(run_id, detail, render_provenance=provenance)}
    result_tabs = tuple(replace(tab, renderer_key=tab.key) for tab in item.result_tabs)
    main_tabs = tabs_for_job(JobType.SCIENTIFIC_ANALYSIS)
    tabs = (*[tab for tab in main_tabs if tab.key != "technical"], *result_tabs,
            *[tab for tab in main_tabs if tab.key == "technical"])
    if item.render_results:
        result_detail = {**detail, "run_directory": str(root)}
        for tab in result_tabs:
            renderers[tab.renderer_key] = lambda tabkey=tab.key:item.render_results(tabkey, run_id, result_detail)
    render_lazy_tabs(st, tabs, renderers, key=f"run-detail-{run_id}")
