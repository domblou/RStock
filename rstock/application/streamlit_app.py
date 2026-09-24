"""Thin Streamlit interface for RStock Laboratory."""

from __future__ import annotations

import base64
import html
import json
from dataclasses import asdict, replace
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Mapping, Sequence

import altair as alt
import pandas as pd
import streamlit as st

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.calendars import forward_market_sessions, resolve_market_session_on_or_before
from rstock.application.production_domain import ProductionModel
from rstock.application.experiment_duplication import (
    DUPLICATION_JOB_TYPES,
    JOB_TYPE_BY_LABEL,
    JOB_TYPE_LABELS,
    duplication_combination_count,
    duplication_submission_values,
    experiment_spec_from_duplication,
    normalize_duplication_job_type,
    validate_duplication_job,
    walk_forward_duplication_draft,
)
from rstock.application.history_ui import (
    EXPERIMENT_JOB_TYPES,
    JOB_LABELS,
    PRODUCTION_JOB_TYPES,
    already_promoted,
    filter_runs,
    history_row,
    paginate_runs,
    qualified_combinations_table,
)
from rstock.application.experiment_launch import (
    launch_walk_forward_config,
    walk_forward_confirmation_text,
    walk_forward_launch_controls_visible,
)
from rstock.application.history_analysis import (
    RunAnalytics,
    altair_serializable_distribution,
    analyze_run,
    comparison_display_table,
    comparison_table,
    comparison_chart_frames,
    configuration_differences,
    filter_combinations,
    filter_threshold_calibration_results,
    load_model_selection_artifact,
    load_threshold_calibration_artifacts,
    load_threshold_holdout_predictions,
    load_walk_forward_artifacts,
    load_xgboost_calibration_artifacts,
    predictor_prefilter_summary,
    run_universe_summary,
    selected_run_action,
    threshold_calibration_table,
    threshold_calibration_choice_diagnostic_table,
    threshold_calibration_selection_summary,
    threshold_parameter_calibration_table,
    promotion_policy,
    threshold_promotion_guidance,
    threshold_sensitivity_summary,
    threshold_sensitivity_table,
    xgboost_calibration_selection_display_table,
    xgboost_calibration_selection_table,
)
from rstock.application.runner import running_duration
from rstock.application.end_to_end import historical_forced_validation_state
from rstock.application.qualification_holdout_diagnostic import diagnostic_state
from rstock.application.temporal_validation_ui import (
    candidate_identity_tables,
    forced_candidate_revalidation_table,
    lost_candidate_display_table,
    temporal_validation_gate_table,
    validation_promotion_lookup,
)
from rstock.application.temporal_validation_trace import (
    forced_candidate_trace_lookup,
    lost_candidate_trace_lookup,
)
from rstock.application.run_detail_tabs import (
    PIPELINE_CHILD_TABS,
    PIPELINE_STAGE_LABEL_COLUMN,
    batch_status_counts,
    pipeline_stage_by_key,
    pipeline_stage_rows,
    render_lazy_tabs,
    tabs_for_job,
    walk_forward_batch_rows,
)
from rstock.application.surveillance import (
    EvaluatedPredictionsView,
    OperationalTableView,
    SignalResultsView,
    build_predictions_view,
    build_evaluated_predictions_view,
    build_signals_view,
    evaluated_predictions_main_table,
    evaluation_feedback,
    filter_evaluated_predictions_view,
    filter_signal_results_view,
    prioritize_signals_view,
    signal_priority_model_lookup,
    prediction_feature_tables,
    source_observation_tables,
)
from rstock.application.surveillance_refresh import (
    OPERATIONAL_JOB_TYPES,
    surveillance_refresh_decision,
)
from rstock.application.services import (
    ExperimentService,
    MarketDataService,
    ModelService,
    PredictionService,
    SignalService,
)
from rstock.application.production_repository import ProductionRepository
from rstock.application.real_trades import (
    RealTradeService,
    filter_performance,
    performance_kpis,
    performance_table,
)
from rstock.application.model_ui import (
    DEFAULT_MODEL_STATUSES,
    filter_models,
    job_domain,
    job_domain_title,
    model_filter_options,
)
from rstock.application.simulation import (
    SimulationResult,
    SimulationService,
    benchmark_cumulative_for_trades,
    summarize_simulation_trades,
)
from rstock.application.simulation_repository import SimulationRepository
from rstock.application.universes import (
    CONTEXT_UNIVERSE_TYPE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    STANDARD_UNIVERSE_TYPE,
    TOP_N,
    UniverseSelection,
    UniverseService,
)
from rstock.application.universe_ui import universe_display_name, universe_ui_preview
from rstock.combination_planning import (
    build_combination_plan,
    build_combination_preview,
)
from rstock.config import (
    DEFAULT_CONFIG,
    UI_SETTINGS_DEFAULTS,
    load_user_settings,
    save_user_settings,
)


THUMBNAIL_PATH = Path(__file__).resolve().parents[2] / "assets" / "rstock-thumbnail.png"
st.set_page_config(page_title="RStock", page_icon=str(THUMBNAIL_PATH), layout="wide")

LOGO_PATH = Path(__file__).resolve().parents[1] / "assets" / "rstock_logo.png"
USER_GUIDE_PATH = Path(__file__).resolve().parents[1] / "docs" / "user_guide.md"
DUPLICATION_DRAFT_KEY = "experiment-duplication-draft"
DUPLICATION_CONFIG_CHOICE_KEY = "experiment-duplication-config-choice"
DUPLICATION_JOB_TYPE_KEY = "experiment-duplication-job-type"
DUPLICATION_WF_MODE_KEY = "experiment-duplication-wf-mode"
EXPERIMENT_WF_MODE_KEY = "experiment-launch-wf-mode"
EXPERIMENT_NAVIGATION_KEY = "requested-primary-page"
_PRIMARY_PAGES: list[st.Page] | None = None
WORKFLOW_PHASE_LABELS = {
    "data_preparation": "Préparation des données",
    "predictor_prefilter_generation": "Predictor prefilter",
    "predictor_prefilter_walk_forward": "Walk-forward du prefilter",
    "predictor_prefilter_selection": "Sélection des prédicteurs",
    "combination_generation": "Génération des combinaisons",
    "walk_forward": "Walk-forward",
    "aggregation": "Agrégation",
    "qualification": "Qualification",
    "final_holdout": "Holdout final",
    "metrics": "Métriques",
    "result_writing": "Écriture",
    "publishing": "Publication",
}

# Historical visual order retained by the shared registry:
# st.tabs(["Résumé", "Analyse", "Combinaisons", "Validation", "Technique"])


def _page_header(title: str) -> None:
    """Render a compact page title with the optional bundled RStock logo."""

    try:
        encoded_logo = base64.b64encode(LOGO_PATH.read_bytes()).decode("ascii")
    except OSError:
        st.title(title)
        return
    st.markdown(
        f"""
        <div style="display: flex; align-items: center; gap: 8px; margin: 0 0 0.35rem; line-height: 1;">
          <img src="data:image/png;base64,{encoded_logo}" alt="RStock"
               style="display: block; width: 130px; height: auto; flex: 0 0 auto;" />
          <h1 style="margin: 0; padding: 0; font-size: 2rem; line-height: 1.12; transform: translateY(4px);">
            {html.escape(title)}</h1>
        </div>
        """,
        unsafe_allow_html=True,
    )


def _compact_datetime(value: object) -> str:
    timestamp = pd.to_datetime(value, errors="coerce")
    return "—" if pd.isna(timestamp) else timestamp.strftime("%Y-%m-%d %H:%M")


def _state() -> None:
    if "lab_config" not in st.session_state:
        loaded_config, ui_settings, load_warning = load_user_settings(DEFAULT_CONFIG)
        st.session_state.lab_config = loaded_config
        for name, value in ui_settings.items():
            st.session_state.setdefault(name, value)
        if load_warning:
            st.session_state["_settings_load_warning"] = load_warning

    for name, value in UI_SETTINGS_DEFAULTS.items():
        st.session_state.setdefault(name, value)

    cached = MarketDataService().available_symbols(st.session_state.lab_config)
    st.session_state.setdefault("lab_symbols", cached or ["AAPL", "MSFT"])
    st.session_state.setdefault("lab_universe_selection", UniverseSelection())
    st.session_state.setdefault("lab_market_benchmark_symbol", None)
    st.session_state.setdefault("lab_context_universe_ids", [])
    st.session_state.setdefault("lab_context_sample_size", None)
    st.session_state.setdefault("lab_context_selection_method", None)
    st.session_state.setdefault("lab_context_seed", None)
    st.session_state.setdefault("lab_target_symbols", [])
    st.session_state.setdefault("lab_context_symbols", [])


def _start_walk_forward_duplication(run_id: str, detail: dict[str, object]) -> None:
    """Store a fresh editable draft, leaving the persisted run untouched."""

    duplication_draft = walk_forward_duplication_draft(
        run_id, detail, project_root=st.session_state.lab_config.project_root
    )
    st.session_state[DUPLICATION_DRAFT_KEY] = duplication_draft
    st.session_state[DUPLICATION_CONFIG_CHOICE_KEY] = "Paramètres du run"
    raw_config = duplication_draft.get("rstock_config", {})
    source_config = raw_config if isinstance(raw_config, Mapping) else {}
    st.session_state[DUPLICATION_WF_MODE_KEY] = str(
        source_config.get("walk_forward_window_mode", "expanding")
    )
    configuration = detail.get("configuration", {})
    source_job_type = normalize_duplication_job_type(
        configuration.get("job_type") if isinstance(configuration, dict) else None
    )
    st.session_state[DUPLICATION_JOB_TYPE_KEY] = JOB_TYPE_LABELS[
        source_job_type or JobType.WALK_FORWARD
    ]
    st.session_state[EXPERIMENT_NAVIGATION_KEY] = "Expériences"
    st.rerun()


def _render_locked_duplication_mode(service: ExperimentService) -> bool:
    """Render and submit an immutable historical experiment duplication."""

    draft = st.session_state.get(DUPLICATION_DRAFT_KEY)
    if not isinstance(draft, dict):
        return False
    selection = UniverseSelection.from_dict(draft.get("universe_selection"))
    choice = st.session_state.get(
        DUPLICATION_CONFIG_CHOICE_KEY, "Paramètres du run"
    )
    context_ids = ", ".join(str(item) for item in draft["context_universe_ids"])
    primary_mode = selection.source
    if selection.sample_size is not None:
        primary_mode += f" · {selection.selection_method} · N={selection.sample_size}"
        if selection.seed is not None:
            primary_mode += f" · seed={selection.seed}"
    context_mode = draft.get("context_selection_method") or "population figée"
    if draft.get("context_sample_size") is not None:
        context_mode += f" · N={draft['context_sample_size']}"
    if draft.get("context_seed") is not None:
        context_mode += f" · seed={draft['context_seed']}"
    stored_label = st.session_state.get(DUPLICATION_JOB_TYPE_KEY)
    raw_job_type = stored_label if stored_label is not None else draft.get("job_type")
    normalized_job_type = normalize_duplication_job_type(raw_job_type)
    job_type_fallback_message = None
    if normalized_job_type is None:
        normalized_job_type = JobType.WALK_FORWARD
        job_type_fallback_message = (
            f"Type de job historique inconnu ({raw_job_type!r}) : "
            "Walk-forward est utilisé comme valeur de secours."
        )
    selected_label = JOB_TYPE_LABELS[normalized_job_type]
    # Existing browser sessions may still contain the prior internal enum value.
    # Normalize it before the widget requires one of its display labels.
    if st.session_state.get(DUPLICATION_JOB_TYPE_KEY) != selected_label:
        st.session_state[DUPLICATION_JOB_TYPE_KEY] = selected_label
    selected_job_type = JOB_TYPE_BY_LABEL[selected_label]
    selected_config = duplication_submission_values(
        draft,
        current_config=st.session_state.lab_config,
        use_run_config=choice == "Paramètres du run",
        job_type=selected_job_type,
        current_combinations_per_target=(
            st.session_state.lab_combinations_per_target
        ),
    )["config"]
    selected_window_mode = str(
        st.session_state.get(
            DUPLICATION_WF_MODE_KEY, selected_config.walk_forward_window_mode
        )
    )
    selected_train_size = selected_config.walk_forward_train_size
    try:
        validate_duplication_job(draft, selected_job_type)
        duplication_error = None
        expected_combinations = duplication_combination_count(draft, selected_config)
    except (TypeError, ValueError) as error:
        duplication_error = str(error)
        expected_combinations = "—"
    summary = pd.DataFrame(
        [
            ("Run source", draft["source_run_id"]),
            ("Univers principal", draft["primary_universe_id"] or "—"),
            ("Mode de sélection", primary_mode),
            ("Cibles", len(draft["target_symbols"])),
            ("Univers de contexte", context_ids or "Aucun"),
            ("Mode/sélection du contexte", context_mode),
            ("Contexte", len(draft["context_symbols"])),
            ("Prédicteurs uniques", len(draft["predictor_symbols"])),
            ("Profondeur", selected_config.permutation_depth),
            (
                "Fenêtre Walk-forward",
                "Expansive"
                if selected_window_mode == "expanding"
                else f"Glissante {selected_train_size}",
            ),
            (
                "Combinaisons attendues",
                expected_combinations,
            ),
            ("Calendrier", draft["calendar"]),
            ("Holdout final", "Oui" if draft["evaluate_final_holdout"] else "Non"),
        ],
        columns=["Élément", "Valeur"],
    )
    with st.container(border=True):
        st.caption("Duplication en lecture seule — le run source ne sera pas modifié.")
        selected_label = st.selectbox(
            "Type de job",
            list(JOB_TYPE_BY_LABEL),
            key=DUPLICATION_JOB_TYPE_KEY,
        )
        selected_job_type = JOB_TYPE_BY_LABEL[selected_label]
        summary["Valeur"] = summary["Valeur"].astype(str)
        st.dataframe(summary, hide_index=True, width="stretch")
        if job_type_fallback_message:
            st.warning(job_type_fallback_message)
        if duplication_error:
            st.error(f"Duplication impossible : {duplication_error}")
        parameter_label = {
            JobType.WALK_FORWARD: "Paramètres du walk-forward",
            JobType.XGBOOST_CALIBRATION: "Paramètres de calibration XGBoost",
            JobType.THRESHOLD_PARAMETER_CALIBRATION: (
                "Paramètres du processus de calibration des seuils"
            ),
            JobType.THRESHOLD_CALIBRATION: "Paramètres de calibration des seuils",
        }[selected_job_type]
        choice = st.radio(
            parameter_label,
            ["Paramètres du run", "Paramètres actuels"],
            horizontal=True,
            key=DUPLICATION_CONFIG_CHOICE_KEY,
        )
        st.selectbox(
            "Mode de fenêtre WF",
            ["expanding", "rolling"],
            format_func=lambda value: (
                "Expansive" if value == "expanding" else "Glissante"
            ),
            key=DUPLICATION_WF_MODE_KEY,
        )
        xgboost_source = draft.get("source_xgboost_calibration_run")
        if selected_job_type is JobType.XGBOOST_CALIBRATION:
            st.caption(
                "Les paramètres XGBoost du walk-forward source restent la baseline. "
                "Ce choix modifie uniquement les paramètres propres au processus "
                "de calibration XGBoost."
            )
        elif selected_job_type in {
            JobType.THRESHOLD_PARAMETER_CALIBRATION,
            JobType.THRESHOLD_CALIBRATION,
        }:
            if xgboost_source or draft.get("frozen_xgboost_parameters"):
                st.caption(
                    "Les configurations XGBoost Up/Down sélectionnées par la "
                    "calibration source seront conservées. Ce choix modifie "
                    "uniquement les paramètres de calibration des seuils."
                )
            else:
                st.caption(
                    "Les paramètres XGBoost du walk-forward source seront conservés. "
                    "Ce choix modifie uniquement les paramètres de calibration des seuils."
                )
        elif xgboost_source:
            st.caption(
                f"Calibration XGBoost {xgboost_source} : "
                + ("conservée" if choice == "Paramètres du run" else "détachée")
            )
        actions = st.columns([2, 1, 5])
        if actions[0].button(
            "Soumettre la duplication",
            type="primary",
            width="stretch",
            disabled=duplication_error is not None,
        ):
            spec = experiment_spec_from_duplication(
                draft,
                current_config=st.session_state.lab_config,
                use_run_config=choice == "Paramètres du run",
                job_type=selected_job_type,
                current_combinations_per_target=(
                    st.session_state.lab_combinations_per_target
                ),
            )
            spec = replace(
                spec,
                config=replace(
                    spec.config,
                    walk_forward_window_mode=str(
                        st.session_state[DUPLICATION_WF_MODE_KEY]
                    ),
                ),
            )
            submitted = service.submit(spec)
            if submitted.created:
                st.session_state.pop(DUPLICATION_DRAFT_KEY, None)
                st.session_state.pop(DUPLICATION_CONFIG_CHOICE_KEY, None)
                st.session_state.pop(DUPLICATION_JOB_TYPE_KEY, None)
                st.session_state.pop(DUPLICATION_WF_MODE_KEY, None)
                st.session_state["duplication-submitted-run-id"] = submitted.run_id
                st.rerun()
            else:
                st.warning(f"Configuration déjà active : {submitted.run_id}")
        if actions[1].button("Annuler", width="stretch"):
            st.session_state.pop(DUPLICATION_DRAFT_KEY, None)
            st.session_state.pop(DUPLICATION_CONFIG_CHOICE_KEY, None)
            st.session_state.pop(DUPLICATION_JOB_TYPE_KEY, None)
            st.session_state.pop(DUPLICATION_WF_MODE_KEY, None)
            st.rerun()
    _live_job_panel(service, domain="experiment")
    return True


def _service() -> ExperimentService:
    return ExperimentService.local(
        st.session_state.lab_config.project_root,
        max_concurrent_heavy_jobs=st.session_state.max_concurrent_heavy_jobs,
    )


def _duration(value: float | None) -> str:
    if value is None:
        return "—"
    seconds = int(value)
    return f"{seconds // 3600:02d}:{seconds % 3600 // 60:02d}:{seconds % 60:02d}"


def _job_panel(
    service: ExperimentService,
    *,
    active_only: bool = True,
    domain: str | None = None,
) -> bool:
    runs = service.runs()
    if active_only:
        runs = [run for run in runs if run["status"] in {"pending", "running"}]
    if domain is not None:
        runs = [run for run in runs if job_domain(run.get("job_type")) == domain]
    if not runs:
        return False
    title = job_domain_title(domain) if domain is not None else None
    if title is not None:
        st.subheader(title)
    for status in runs:
        run_id = str(status["run_id"])
        detail = service.run(run_id)
        progress = detail["progress"]
        with st.container(border=True):
            columns = st.columns([2, 2, 1, 1])
            columns[0].markdown(f"**{status['job_type']}**")
            columns[1].code(run_id)
            columns[2].metric("Statut", status["status"])
            columns[3].metric("Durée", _duration(running_duration(status)))
            st.caption(
                "Étape: "
                f"{WORKFLOW_PHASE_LABELS.get(str(progress.get('stage')), progress.get('stage', '—'))} · "
                f"Sous-étape: {progress.get('substage') or '—'}"
            )
            workflow_percent = progress.get("workflow_percent")
            if workflow_percent is not None:
                st.progress(float(workflow_percent) / 100.0)
            if progress.get("stage_percent") is not None:
                completed = progress.get("completed_units")
                total = progress.get("total_units")
                eta = progress.get("eta_seconds")
                message = f"{completed} / {total} unités"
                if eta is not None:
                    message += f" · ETA estimée {_duration(float(eta))}"
                st.caption(message)
            if progress.get("stage_percent") is None:
                st.caption("Étape non mesurable : voir la durée totale du job.")
            if detail["log_tail"]:
                with st.expander("Derniers messages"):
                    st.code("\n".join(detail["log_tail"][-8:]))
            if status["status"] in {"pending", "running"}:
                if st.button("Annuler", key=f"cancel-{run_id}"):
                    service.cancel(run_id)
                    st.rerun()
    return True


def _live_job_panel(service: ExperimentService, *, domain: str | None = None) -> bool:
    return _job_panel(service, domain=domain)


if hasattr(st, "fragment"):
    _live_job_panel = st.fragment(run_every=2)(_live_job_panel)


def _universe_service() -> UniverseService:
    return UniverseService(root=st.session_state.lab_config.project_root)


def _market_benchmark_options(
    service: UniverseService, *, current: str | None = None
) -> list[str]:
    """Return explicit benchmark choices; context membership never selects one."""

    symbols = {
        symbol
        for record in service.records()
        if record.type == CONTEXT_UNIVERSE_TYPE
        for symbol in record.symbols
    }
    options = ["Aucun", *sorted(symbols)]
    if current is not None and current not in options:
        options.append(current)
    return options


def _combination_plan_preview(job_type: JobType) -> bool:
    """Render the raw plan shared with future WF and End-to-end execution."""

    st.session_state.pop("experiment-combination-preview", None)
    if job_type not in {JobType.WALK_FORWARD, JobType.END_TO_END}:
        return True
    try:
        plan = build_combination_plan(
            target_symbols=st.session_state.lab_target_symbols,
            predictor_symbols=st.session_state.lab_symbols,
            permutation_depth=st.session_state.lab_config.permutation_depth,
        )
        preview = build_combination_preview(
            plan,
            context_symbols=st.session_state.lab_context_symbols,
            max_combinations_per_batch=(
                st.session_state.lab_config.walk_forward_max_combinations_per_batch
            ),
            prefilter_enabled=(
                st.session_state.lab_config.predictor_prefilter_enabled
            ),
            prefilter_top_n=(
                st.session_state.lab_config.predictor_prefilter_top_n
            ),
        )
    except ValueError as error:
        st.error(f"Plan de combinaisons invalide : {error}")
        return False
    st.session_state["experiment-combination-preview"] = preview
    with st.container(border=True):
        st.subheader("Prévisualisation des combinaisons")
        first = st.columns(4)
        first[0].metric("Cibles", f"{preview.target_count:,}")
        first[1].metric("Contexte", f"{preview.context_count:,}")
        first[2].metric("Prédicteurs", f"{preview.predictor_count:,}")
        first[3].metric("Profondeur", preview.permutation_depth)
        second = st.columns(4)
        second[0].metric(
            "Combinaisons brutes exactes",
            f"{preview.raw_combination_count:,}",
        )
        second[1].metric(
            "Maximum après préfiltrage",
            f"{preview.max_combinations_after_prefilter:,}",
        )
        capacity = preview.max_combinations_per_batch
        second[2].metric(
            "Maximum par batch",
            "Mono-run historique" if capacity is None else f"{capacity:,}",
        )
        second[3].metric(
            "Batchs max après préfiltrage",
            preview.max_batches_after_prefilter,
        )
        st.caption(
            "Borne supérieure calculée à partir du Top N du préfiltre, de la "
            "profondeur des combinaisons et de la population de prédicteurs. Le "
            "nombre réel peut être inférieur après qualification et suppression "
            "des redondances."
        )
    return True


def _render_experiment_submission_confirmation(
    service: ExperimentService,
    spec: ExperimentSpec,
    label: str,
    preview: object | None,
) -> None:
    """Render the inline confirmation immediately above the submit action."""

    maximum = getattr(preview, "max_combinations_after_prefilter", None)
    maximum_text = "—" if maximum is None else f"{maximum:,}"
    temporal_text = ""
    if spec.temporal_validation_enabled:
        promotion_text = (
            "promotion automatique uniquement si la comparaison réussit"
            if spec.auto_promote_candidates
            else "sans promotion automatique"
        )
        temporal_text = (
            " · validation temporelle activée "
            "(référence offset 0, validation offset 63) · "
            f"{promotion_text}"
        )
    walk_forward_text = ""
    if walk_forward_launch_controls_visible(spec.job_type):
        walk_forward_text = f" · {walk_forward_confirmation_text(spec.config)}"
    st.success(
        f"Soumettre l’expérience {label} ? "
        f"{len(spec.target_symbols):,} cibles · "
        f"{len(spec.context_symbols):,} contexte · "
        f"offset {spec.config.walk_forward_end_offset_sessions} · "
        f"max {maximum_text} combinaisons après préfiltrage"
        f"{walk_forward_text}"
        f"{temporal_text}"
    )
    confirm, cancel, _ = st.columns([0.2, 0.2, 1])
    if confirm.button(
        "Confirmer",
        type="primary",
        key="confirm-experiment-submission",
    ):
        submitted = service.submit(spec)
        st.session_state.pop("pending-experiment-submission", None)
        st.session_state.pop(EXPERIMENT_WF_MODE_KEY, None)
        if submitted.created:
            st.success(f"Run créé : {submitted.run_id}")
        else:
            st.warning(f"Configuration déjà active : {submitted.run_id}")
        st.rerun()
    if cancel.button("Annuler", key="cancel-experiment-submission"):
        st.session_state.pop("pending-experiment-submission", None)
        st.rerun()

def _experiment_universe_selector() -> bool:
    """Resolve the exact experiment symbols before the job is submitted."""

    service = _universe_service()
    current = st.session_state.lab_universe_selection
    labels = {
        "Univers complet": SAVED_SOURCE,
        "Échantillon d’un univers": SAMPLE_SOURCE,
    }
    current_label = next(
        (label for label, source in labels.items() if source == current.source),
        "Univers complet",
    )
    primary_column, context_column = st.columns(2)
    with primary_column:
        with st.container(border=True):
            st.subheader("Univers principal")
            names = service.standard_universe_names()
            universe_id = st.selectbox(
                "Sélection de l’univers principal",
                names,
                index=names.index(current.universe) if current.universe in names else 0,
                format_func=lambda item: universe_display_name(item, service),
                key="experiment-saved-universe",
            )
            selected_label = st.radio(
                "Mode d’utilisation",
                list(labels),
                index=list(labels).index(current_label),
                horizontal=True,
                key="experiment-universe-mode",
            )
            source = labels[selected_label]
            size = None
            method = None
            seed = None
            if source == SAMPLE_SOURCE:
                available = len(service.universe_symbols(universe_id))
                size = int(st.number_input(
                    "Nombre de symboles",
                    min_value=1,
                    max_value=available,
                    value=min(available, current.sample_size or available),
                    key="experiment-sample-size",
                ))
                method_label = st.selectbox(
                    "Méthode",
                    ["Top N", "Échantillon reproductible"],
                    index=0 if current.selection_method == TOP_N else 1,
                    key="experiment-sample-method",
                )
                method = TOP_N if method_label == "Top N" else SEEDED_SAMPLE
                if method == SEEDED_SAMPLE:
                    seed = int(st.number_input(
                        "Seed", min_value=0, value=current.seed or 1234,
                        key="experiment-sample-seed",
                    ))
            selection = UniverseSelection(
                source=source,
                universe=universe_id,
                sample_size=size,
                selection_method=method,
                seed=seed,
            )
            try:
                preview = universe_ui_preview(selection, service)
            except ValueError as error:
                st.error(f"Univers invalide : {error}")
                return False
            st.caption(f"{len(preview.resolved_symbols)} cibles sélectionnées")
    context_names = tuple(
        record.universe_id
        for record in service.records()
        if record.type == CONTEXT_UNIVERSE_TYPE
    )
    context_labels = {
        "Univers complet": SAVED_SOURCE,
        "Échantillon d’un univers": SAMPLE_SOURCE,
    }
    context_sample_size = None
    context_method = None
    context_seed = None
    with context_column:
        with st.container(border=True):
            st.subheader("Univers de contexte")
            selected_contexts = st.multiselect(
                "Sélection de l’univers de contexte",
                context_names,
                default=[
                    item
                    for item in st.session_state.lab_context_universe_ids
                    if item in context_names
                ],
                format_func=lambda item: universe_display_name(item, service),
                key="experiment-context-universes",
            )
            full_context = service.resolve_experiment(preview.selection, selected_contexts)
            context_mode = st.radio(
                "Mode d’utilisation du contexte",
                list(context_labels),
                horizontal=True,
                key="experiment-context-mode",
            )
            if context_labels[context_mode] == SAMPLE_SOURCE and full_context.context_symbols:
                context_sample_size_key = "experiment-context-sample-size"
                saved_context_size = st.session_state.get(
                    context_sample_size_key, len(full_context.context_symbols)
                )
                if not 1 <= int(saved_context_size) <= len(full_context.context_symbols):
                    st.session_state[context_sample_size_key] = len(full_context.context_symbols)
                context_sample_size = int(st.number_input(
                    "Nombre de symboles de contexte",
                    min_value=1,
                    max_value=len(full_context.context_symbols),
                    value=len(full_context.context_symbols),
                    key=context_sample_size_key,
                ))
                context_method_label = st.selectbox(
                    "Méthode de sélection du contexte",
                    ["Top N", "Échantillon reproductible"],
                    index=(
                        0
                        if st.session_state.lab_context_selection_method == TOP_N
                        else 1
                    ),
                    key="experiment-context-sample-method",
                )
                context_method = TOP_N if context_method_label == "Top N" else SEEDED_SAMPLE
                if context_method == SEEDED_SAMPLE:
                    context_seed = int(st.number_input(
                        "Seed du contexte", min_value=0, value=1234,
                        key="experiment-context-sample-seed",
                    ))
            elif context_labels[context_mode] == SAMPLE_SOURCE:
                st.caption("Aucun symbole de contexte disponible.")
            st.caption(f"{len(full_context.context_symbols)} symboles de contexte disponibles")
    resolved = service.resolve_experiment(
        preview.selection,
        selected_contexts,
        context_sample_size=context_sample_size,
        context_selection_method=context_method,
        context_seed=context_seed,
    )
    target_symbols = resolved.target_symbols
    context_symbols = resolved.context_symbols
    predictor_symbols = resolved.predictor_symbols
    st.session_state.lab_universe_selection = preview.selection
    st.session_state.lab_context_universe_ids = list(selected_contexts)
    st.session_state.lab_context_sample_size = context_sample_size
    st.session_state.lab_context_selection_method = context_method
    st.session_state.lab_context_seed = context_seed
    st.session_state.lab_target_symbols = list(target_symbols)
    st.session_state.lab_context_symbols = list(context_symbols)
    st.session_state.lab_symbols = list(predictor_symbols)
    st.session_state.lab_market_benchmark_symbol = resolved.benchmark_symbol
    context_label = ", ".join(selected_contexts) if selected_contexts else "Aucun"
    st.caption(
        f"Univers principal : {universe_id} · {len(target_symbols)} cibles · "
        f"Contexte : {context_label} · {len(context_symbols)} symboles · "
        f"{len(predictor_symbols)} prédicteurs"
    )
    with st.expander("Voir les symboles sélectionnés"):
        st.write("Cibles")
        st.code(", ".join(target_symbols))
        st.write("Contexte")
        st.code(", ".join(context_symbols) or "Aucun")
    return True


def _experiments(service: ExperimentService) -> None:
    _page_header("Expériences")
    duplicated_run_id = st.session_state.pop("duplication-submitted-run-id", None)
    if duplicated_run_id is not None:
        st.success(f"Run créé : {duplicated_run_id}")
    # Paramètres à utiliser et duplication_submission_values sont gérés ici.
    if _render_locked_duplication_mode(service):
        return
    labels = {
        "Walk-forward": JobType.WALK_FORWARD,
        "Calibration XGBoost": JobType.XGBOOST_CALIBRATION,
        "Calibration des paramètres de seuils": (
            JobType.THRESHOLD_PARAMETER_CALIBRATION
        ),
        "Calibration des seuils": JobType.THRESHOLD_CALIBRATION,
        "End-to-end": JobType.END_TO_END,
    }
    choice = st.selectbox("Type de job", list(labels))
    selected_job_type = labels[choice]
    run_config = st.session_state.lab_config
    auto_promote_candidates = False
    temporal_validation_enabled = False
    requested_historical_cutoff = None
    resolved_historical_cutoff = None
    forward_simulation_enabled = False
    forward_simulation_mode = None
    forward_simulation_end_date = None
    if selected_job_type is JobType.END_TO_END:
        requested_historical_cutoff = st.date_input(
            "Cutoff historique", value=None,
            help="Dernière séance XNYS disponible pour la découverte scientifique.",
        )
        if requested_historical_cutoff is not None:
            resolved_historical_cutoff = resolve_market_session_on_or_before(
                requested_historical_cutoff, st.session_state.lab_calendar
            ).date().isoformat()
            st.caption(f"Séance XNYS résolue : {resolved_historical_cutoff}")
            forward_simulation_enabled = st.checkbox(
                "Lancer une Forward Simulation après succès", value=False
            )
            if forward_simulation_enabled:
                forward_simulation_mode = st.radio(
                    "Durée Forward", ["63_sessions", "126_sessions", "custom_end_date"],
                    format_func=lambda value: {
                        "63_sessions": "63 séances", "126_sessions": "126 séances",
                        "custom_end_date": "Date de fin personnalisée",
                    }[value], horizontal=True,
                )
                if forward_simulation_mode == "custom_end_date":
                    forward_simulation_end_date = st.date_input("Date de fin Forward")
        auto_promote_candidates = st.checkbox(
            "Promouvoir automatiquement les candidats admissibles",
            value=False,
            help=(
                "La promotion est une etape interne persistee du pipeline; "
                "elle n'entraine et n'active aucun modele."
            ),
        )
        if auto_promote_candidates and not st.session_state.lab_evaluate_holdout:
            st.error("La promotion automatique exige le holdout final.")
        temporal_validation_enabled = st.checkbox(
            "Exécuter une validation temporelle",
            value=False,
            help=(
                "Exécute automatiquement un second End-to-end sur une période "
                "décalée de 63 séances afin de permettre une validation temporelle "
                "de la méthodologie."
            ),
        )
        if temporal_validation_enabled and auto_promote_candidates:
            st.info(
                "Temporal comparison precedes promotion; four passed gates are required for automatic promotion.",
            )
        if (
            temporal_validation_enabled
            and st.session_state.lab_config.walk_forward_end_offset_sessions != 0
        ):
            st.error("La validation temporelle exige un End-to-end de référence avec offset 0.")
        if temporal_validation_enabled and resolved_historical_cutoff is not None:
            st.error("Le cutoff historique et la validation temporelle ne sont pas combinables.")
    if walk_forward_launch_controls_visible(selected_job_type):
        default_mode = st.session_state.lab_config.walk_forward_window_mode
        mode_label = st.selectbox(
            "Mode de fenêtre Walk-forward",
            ["Expansive", "Glissante"],
            index=0 if default_mode == "expanding" else 1,
            key=EXPERIMENT_WF_MODE_KEY,
        )
        window_mode = "expanding" if mode_label == "Expansive" else "rolling"
        if window_mode == "expanding":
            st.caption(
                "Train minimal utilisé : "
                f"{st.session_state.lab_config.walk_forward_min_train_size}."
            )
        else:
            st.caption(
                "Taille du train glissant utilisée : "
                f"{st.session_state.lab_config.walk_forward_train_size}."
            )
        run_config = launch_walk_forward_config(
            st.session_state.lab_config,
            window_mode,
        )
    valid_universe = _experiment_universe_selector()
    valid_plan = (
        _combination_plan_preview(selected_job_type) if valid_universe else False
    )
    submit_disabled = not (valid_universe and valid_plan) or (
        auto_promote_candidates and not st.session_state.lab_evaluate_holdout
    ) or (temporal_validation_enabled and (
        st.session_state.lab_config.walk_forward_end_offset_sessions != 0
    )) or (temporal_validation_enabled and resolved_historical_cutoff is not None)
    pending_spec = st.session_state.get("pending-experiment-submission")
    if pending_spec is not None:
        _render_experiment_submission_confirmation(
            service,
            pending_spec,
            choice,
            st.session_state.get("experiment-combination-preview"),
        )
    elif st.button("Soumettre l’expérience", type="primary", disabled=submit_disabled):
        spec = ExperimentSpec(
            job_type=selected_job_type,
            config=run_config,
            symbols=tuple(st.session_state.lab_symbols),
            calendar=st.session_state.lab_calendar,
            combinations_per_target=st.session_state.lab_combinations_per_target,
            evaluate_final_holdout=st.session_state.lab_evaluate_holdout,
            universe_selection=st.session_state.lab_universe_selection,
            primary_universe_id=st.session_state.lab_universe_selection.universe,
            market_benchmark_symbol=st.session_state.lab_market_benchmark_symbol,
            context_universe_ids=tuple(st.session_state.lab_context_universe_ids),
            context_sample_size=st.session_state.lab_context_sample_size,
            context_selection_method=st.session_state.lab_context_selection_method,
            context_seed=st.session_state.lab_context_seed,
            target_symbols=tuple(st.session_state.lab_target_symbols),
            context_symbols=tuple(st.session_state.lab_context_symbols),
            predictor_symbols=tuple(st.session_state.lab_symbols),
            auto_promote_candidates=auto_promote_candidates,
            temporal_validation_enabled=temporal_validation_enabled,
            historical_data_cutoff=resolved_historical_cutoff,
            requested_historical_cutoff=(
                None if requested_historical_cutoff is None
                else requested_historical_cutoff.isoformat()
            ),
            resolved_market_session_cutoff=resolved_historical_cutoff,
            forward_simulation_enabled=forward_simulation_enabled,
            forward_simulation_mode=forward_simulation_mode,
            forward_simulation_end_date=(
                None if forward_simulation_end_date is None
                else forward_simulation_end_date.isoformat()
            ),
            run_description=(
                f"profondeur {st.session_state.lab_config.permutation_depth}"
            ),
        )
        st.session_state["pending-experiment-submission"] = spec
        st.rerun()
    st.caption(
        "Les listes se gèrent dans Univers; la sélection résolue et la "
        "configuration sont figées au lancement."
    )
    _live_job_panel(service, domain="experiment")


def _settings() -> None:
    _page_header("Paramètres")
    load_warning = st.session_state.pop("_settings_load_warning", None)
    if load_warning:
        st.warning(load_warning)
    current = st.session_state.lab_config
    with st.expander("Valeurs RStock par défaut"):
        defaults = asdict(DEFAULT_CONFIG)
        defaults["project_root"] = str(DEFAULT_CONFIG.project_root)
        st.json(defaults)
    with st.container():
        st.subheader("Préparation des données et génération")
        calendar = st.text_input("Calendrier", value=st.session_state.lab_calendar)
        c1, c2, c3 = st.columns(3)
        history = c1.number_input("Historique (jours)", min_value=1, value=current.model_history_days)
        permutation = c2.number_input("Permutation depth", min_value=1, value=current.permutation_depth)
        max_sets = c3.number_input("Max generated sets", min_value=1, value=current.max_generated_sets)
        lag = c1.number_input("Lag depth", min_value=1, value=current.lag_depth)
        up_threshold = c2.number_input("Seuil intraday hausse", min_value=0.0, value=current.intraday_target_threshold, format="%.4f")
        down_threshold = c3.number_input("Seuil intraday baisse", min_value=0.0, value=current.intraday_down_threshold, format="%.4f")

        st.subheader("Pré-filtrage des prédicteurs")
        prefilter_enabled = st.checkbox(
            "Activer le pré-filtrage",
            value=current.predictor_prefilter_enabled,
            help="Évalue les prédicteurs seuls avant de générer les combinaisons.",
        )
        p1, p2, p3 = st.columns(3)
        prefilter_top_n = p1.number_input(
            "Top N prédicteurs", min_value=1,
            value=current.predictor_prefilter_top_n,
            help="Nombre maximal de prédicteurs admissibles conservés par cible.",
        )
        prefilter_median_auc = p2.number_input(
            "AUC médiane minimale", min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_median_auc,
            help="Performance médiane minimale sur les fenêtres de développement.",
        )
        prefilter_pct_random = p3.number_input(
            "Part minimale de fenêtres > 0,50", min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_pct_above_random,
            help="Proportion minimale de fenêtres meilleures que le hasard.",
        )
        prefilter_worst_auc = p1.number_input(
            "Worst AUC minimal", min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_worst_auc,
            help="AUC minimale tolérée parmi les fenêtres valides.",
        )
        prefilter_auc_std = p2.number_input(
            "Dispersion AUC maximale", min_value=0.0,
            value=current.predictor_prefilter_max_auc_std,
            help="Écart-type maximal des AUC entre fenêtres.",
        )
        prefilter_correlation = p3.number_input(
            "Seuil de corrélation", min_value=0.0,
            value=current.predictor_prefilter_correlation_threshold,
            help="Au-delà de ce seuil absolu, seul le prédicteur le mieux classé est gardé.",
        )

        st.subheader("Walk-forward")
        window_mode_label = st.selectbox(
            "Mode de fenêtre",
            ["Expansive", "Glissante"],
            index=0 if current.walk_forward_window_mode == "expanding" else 1,
        )
        window_mode = "expanding" if window_mode_label == "Expansive" else "rolling"
        w1, w2, w3, w4, w5, w6 = st.columns(6)
        min_train = w1.number_input(
            "Train minimal",
            min_value=1,
            value=current.walk_forward_min_train_size,
            disabled=window_mode == "rolling",
            help="Utilisé uniquement en mode Expansive.",
        )
        rolling_train = w2.number_input(
            "Taille du train glissant",
            min_value=1,
            value=current.walk_forward_train_size,
            disabled=window_mode == "expanding",
            help="Utilisée uniquement en mode Glissante.",
        )
        test_size = w3.number_input("Taille test", min_value=1, value=current.walk_forward_test_size)
        step = w4.number_input("Step", min_value=1, value=current.walk_forward_step_size)
        holdout = w5.number_input("Holdout final", min_value=1, value=current.final_holdout_size)
        end_offset = w6.number_input(
            "Décalage de fin (jours de marché)",
            min_value=0,
            value=current.walk_forward_end_offset_sessions,
            help=(
                "Décale artificiellement la date de fin du walk-forward de N séances "
                "de marché. Permet de rejouer la même méthodologie sur une période "
                "historique différente sans modifier la taille des fenêtres. "
                "0 = données les plus récentes."
            ),
        )
        b1, b2, b3, b4 = st.columns(4)
        prefilter_batch_size = b1.number_input(
            "Batch préfiltre",
            min_value=1,
            value=current.predictor_prefilter_batch_size,
            help="Nombre de combinaisons univariées calculées avant chaque checkpoint.",
        )
        walk_forward_batch_size = b2.number_input(
            "Batch walk-forward",
            min_value=1,
            value=current.walk_forward_batch_size,
            help="Nombre de combinaisons détaillées conservées simultanément en mémoire.",
        )
        final_holdout_batch_size = b3.number_input(
            "Batch holdout final",
            min_value=1,
            value=current.final_holdout_batch_size,
            help="Nombre de modèles admissibles évalués entre deux checkpoints holdout.",
        )
        max_combinations_per_batch = b4.number_input(
            "Taille maximale d’un batch de combinaisons",
            min_value=1,
            value=(
                current.walk_forward_max_combinations_per_batch
                if current.walk_forward_max_combinations_per_batch is not None
                else DEFAULT_CONFIG.walk_forward_max_combinations_per_batch
            ),
            help=(
                "Contrôle uniquement le découpage des combinaisons en batches; "
                "ne change ni les combinaisons générées ni les critères du modèle."
            ),
        )

        if False: """
        st.subheader("Pré-filtrage des prédicteurs")
        prefilter_enabled = st.checkbox(
            "Activer le pré-filtrage",
            value=current.predictor_prefilter_enabled,
            help="Évalue les prédicteurs seuls avant de générer les combinaisons.",
        )
        p1, p2, p3 = st.columns(3)
        prefilter_top_n = p1.number_input(
            "Top N prédicteurs",
            min_value=1,
            value=current.predictor_prefilter_top_n,
            help="Nombre maximal de prédicteurs admissibles conservés par cible.",
        )
        prefilter_median_auc = p2.number_input(
            "AUC médiane minimale",
            min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_median_auc,
            help="Performance médiane minimale sur les fenêtres de développement.",
        )
        prefilter_pct_random = p3.number_input(
            "Part minimale de fenêtres > 0,50",
            min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_pct_above_random,
            help="Proportion minimale de fenêtres meilleures que le hasard.",
        )
        prefilter_worst_auc = p1.number_input(
            "Worst AUC minimal",
            min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_min_worst_auc,
            help="AUC minimale tolérée parmi les fenêtres valides.",
        )
        prefilter_auc_std = p2.number_input(
            "Dispersion AUC maximale",
            min_value=0.0,
            value=current.predictor_prefilter_max_auc_std,
            help="Écart-type maximal des AUC entre fenêtres.",
        )
        prefilter_correlation = p3.number_input(
            "Seuil de corrélation",
            min_value=0.0, max_value=1.0,
            value=current.predictor_prefilter_correlation_threshold,
            help="Au-delà de ce seuil absolu, seul le prédicteur le mieux classé est gardé.",
        )

        """
        st.subheader("Qualification et exécution")
        q1, q2, q3 = st.columns(3)
        min_windows = q1.number_input("Fenêtres minimales", min_value=1, value=current.qualification_min_windows)
        median_auc = q2.number_input("ROC-AUC médian minimal", min_value=0.0, max_value=1.0, value=current.qualification_min_median_auc)
        pct_random = q3.number_input("Part fenêtres > hasard", min_value=0.0, max_value=1.0, value=current.qualification_min_pct_windows_above_random)
        worst_auc = q1.number_input("Pire ROC-AUC minimal", min_value=0.0, max_value=1.0, value=current.qualification_min_worst_window_auc)
        min_positive = q2.number_input("Observations positives minimales", min_value=0, value=current.qualification_min_positive_observations)
        max_auc_std = q3.number_input("Écart-type ROC-AUC maximal", min_value=0.0, value=current.qualification_max_auc_std)
        final_auc = q1.number_input("ROC-AUC confirmation finale", min_value=0.0, max_value=1.0, value=current.final_confirmation_min_auc)
        prediction_threshold = q2.number_input("Seuil de décision standard", min_value=0.0, max_value=1.0, value=current.prediction_threshold)

        st.subheader("Classement des modèles")
        st.caption("Pondérations du score final; les composantes absentes sont exclues puis les poids disponibles sont renormalisés.")
        s1, s2, s3 = st.columns(3)
        selection_predictive_weight = s1.number_input(
            "Poids qualité prédictive", min_value=0.0,
            value=current.model_selection_predictive_quality_weight,
            help="Poids de l’AUC médiane walk-forward.",
        )
        selection_stability_weight = s2.number_input(
            "Poids stabilité", min_value=0.0,
            value=current.model_selection_stability_weight,
            help="Poids du Worst AUC, de la dispersion et de la constance entre fenêtres.",
        )
        selection_holdout_weight = s3.number_input(
            "Poids holdout", min_value=0.0,
            value=current.model_selection_holdout_weight,
            help="Poids de l’AUC holdout et de l’écart développement-holdout.",
        )
        selection_signal_weight = s1.number_input(
            "Poids qualité signal", min_value=0.0,
            value=current.model_selection_signal_quality_weight,
            help="Poids de la qualité, de la stabilité et du volume des signaux calibrés.",
        )
        selection_sample_weight = s2.number_input(
            "Poids adéquation échantillon", min_value=0.0,
            value=current.model_selection_sample_adequacy_weight,
            help="Poids du nombre et de la validité des fenêtres et observations.",
        )
        workers = q1.number_input("Workers marché", min_value=1, value=current.market_cache_workers)
        combination_workers = q2.number_input(
            "Workers combinaisons", min_value=1, value=current.combination_workers
        )
        nthread = q2.number_input("Threads XGBoost", min_value=1, value=current.xgb_nthread)
        seed = q3.number_input("Seed", min_value=0, value=current.xgb_seed)
        combinations = q1.number_input("Combinaisons par cible (calibrations)", min_value=1, value=st.session_state.lab_combinations_per_target)
        max_jobs = q2.number_input("Jobs lourds concurrents", min_value=1, value=st.session_state.max_concurrent_heavy_jobs)
        evaluate_holdout = q3.checkbox("Évaluer le holdout final", value=st.session_state.lab_evaluate_holdout)

        st.subheader("XGBoost")
        x1, x2, x3 = st.columns(3)
        max_depth = x1.number_input("max_depth", min_value=1, value=current.xgb_max_depth)
        eta = x2.number_input("eta", min_value=0.0001, value=current.xgb_eta, format="%.4f")
        rounds = x3.number_input("num_boost_round", min_value=1, value=current.xgb_rounds)
        child = x1.number_input("min_child_weight", min_value=0.0, value=current.xgb_min_child_weight)
        subsample = x2.number_input("subsample", min_value=0.01, max_value=1.0, value=current.xgb_subsample)
        colsample = x3.number_input("colsample_bytree", min_value=0.01, max_value=1.0, value=current.xgb_colsample_bytree)
        gamma = x1.number_input("gamma", min_value=0.0, value=current.xgb_gamma)
        alpha = x2.number_input("reg_alpha", min_value=0.0, value=current.xgb_reg_alpha)
        reg_lambda = x3.number_input("reg_lambda", min_value=0.0, value=current.xgb_reg_lambda)

        st.subheader("Calibration des seuils")
        t1, t2, t3 = st.columns(3)
        min_signals = t1.number_input(
            "Signaux minimaux par fenêtre",
            min_value=1,
            value=current.threshold_calibration_min_signals_per_window,
        )
        min_window_fraction = t2.number_input(
            "Fraction minimale de fenêtres",
            min_value=0.01,
            max_value=1.0,
            value=current.threshold_calibration_min_window_fraction,
        )
        min_robust_signals = t3.number_input(
            "Signaux totaux minimum pour un seuil robuste",
            min_value=1,
            value=current.threshold_calibration_min_robust_signals,
            help=(
                "Nombre total de signaux requis pour préférer un seuil robuste. "
                "Ce seuil est distinct du minimum de signaux requis par fenêtre."
            ),
        )
        unlimited_threshold_parameter_models = st.checkbox(
            "Sans plafond de modèles directionnels",
            value=current.threshold_parameter_calibration_max_models is None,
            help=(
                "Désactive la limite du nombre de modèles directionnels évalués "
                "lors de la calibration des paramètres de seuils."
            ),
        )
        threshold_parameter_columns = st.columns(3)
        threshold_parameter_calibration_max_models = threshold_parameter_columns[
            0
        ].number_input(
            "Nombre maximal de modèles directionnels",
            min_value=2,
            step=2,
            value=(
                current.threshold_parameter_calibration_max_models
                if current.threshold_parameter_calibration_max_models is not None
                else DEFAULT_CONFIG.threshold_parameter_calibration_max_models
            ),
            disabled=unlimited_threshold_parameter_models,
            help=(
                "Nombre maximal de modèles directionnels évalués. Deux modèles sont "
                "générés par combinaison : Up et Down. Exemple : 1500 = 750 "
                "combinaisons."
            ),
        )
        quantiles = threshold_parameter_columns[1].text_input(
            "Quantiles de la grille",
            value=", ".join(str(value) for value in current.threshold_calibration_quantiles),
        )
        precision_tolerance = threshold_parameter_columns[2].number_input(
            "Tolérance de précision — sélection Up",
            min_value=0.0,
            max_value=1.0,
            value=current.threshold_calibration_precision_tolerance,
            step=0.005,
            format="%.3f",
            help=(
                "Les seuils dont la précision est à moins de cette valeur de la "
                "meilleure précision admissible sont considérés comme équivalents "
                "avant le départage économique. 0,01 = 1 point de pourcentage."
            ),
        )
        sensitivity_1, sensitivity_2, sensitivity_3 = st.columns(3)
        sensitivity_threshold_min = sensitivity_1.number_input(
            "Seuil min — analyse de sensibilité",
            min_value=0.0,
            max_value=1.0,
            value=float(st.session_state.sensitivity_threshold_min),
            step=0.025,
        )
        sensitivity_threshold_max = sensitivity_2.number_input(
            "Seuil max — analyse de sensibilité",
            min_value=float(sensitivity_threshold_min),
            max_value=1.0,
            value=max(
                float(st.session_state.sensitivity_threshold_max),
                float(sensitivity_threshold_min),
            ),
            step=0.025,
        )
        sensitivity_threshold_step = sensitivity_3.number_input(
            "Pas — analyse de sensibilité",
            min_value=0.0001,
            max_value=1.0,
            value=float(st.session_state.sensitivity_threshold_step),
            step=0.005,
            format="%.4f",
        )

        st.subheader("Validation temporelle")
        st.caption(
            "Ces valeurs sont figées dans le snapshot End-to-end de référence. "
            "La largeur maximale de l’IC ne s’applique qu’à l’écart de précision."
        )
        tv1, tv2, tv3 = st.columns(3)
        temporal_candidate_yield = tv1.number_input(
            "Ratio minimal de candidats", min_value=0.0,
            value=current.temporal_min_candidate_yield_ratio,
            help=(
                "Ratio minimal entre le nombre de candidats validés et le nombre "
                "de candidats du run de référence."
            ),
        )
        temporal_auc_degradation = tv2.number_input(
            "Dégradation maximale de l’AUC", min_value=0.0,
            value=current.temporal_max_auc_degradation,
            help="Réduction maximale autorisée de l’AUC médiane sur le holdout.",
        )
        temporal_precision_edge = tv3.number_input(
            "Écart de précision minimal", value=current.temporal_min_precision_edge,
            help="La précision moins le taux de direction naturelle doit dépasser ce seuil.",
        )
        temporal_directional_return = tv1.number_input(
            "Rendement directionnel minimal", value=current.temporal_min_mean_directional_return,
            help="L’intervalle de confiance du rendement directionnel doit dépasser ce seuil.",
        )
        temporal_confidence = tv2.number_input(
            "Niveau de confiance", min_value=0.01, max_value=0.99,
            value=current.temporal_confidence_level,
            help="Niveau de confiance du bootstrap déterministe par blocs de dates.",
        )
        temporal_ci_width = tv3.number_input(
            "Largeur maximale de l’IC pour l’écart de précision", min_value=0.0,
            value=current.temporal_max_ci_width,
            help="Ce paramètre ne s’applique pas au rendement directionnel.",
        )

        st.subheader("Promotion")
        st.caption(
            "La promotion automatique se choisit au lancement d’un run End-to-end. "
            "Elle exige le holdout final et, si la validation temporelle est activée, "
            "le passage de ses critères avant toute promotion."
        )
        promotion_top = st.columns(3)
        promotion_min_signals = promotion_top[0].number_input(
            "Signaux holdout minimum", min_value=1,
            value=current.promotion_min_holdout_signals,
            help="Nombre minimal de signaux observés sur le holdout.",
        )
        promotion_min_auc = promotion_top[1].number_input(
            "AUC holdout minimum", min_value=0.0, max_value=1.0,
            value=current.promotion_min_holdout_auc,
            help="AUC minimale mesurée sur le holdout.",
        )
        promotion_min_precision = promotion_top[2].number_input(
            "Précision holdout minimum", min_value=0.0, max_value=1.0,
            value=current.promotion_min_holdout_precision,
            help="Précision minimale mesurée sur le holdout.",
        )
        promotion_bottom = st.columns(2)
        promotion_min_return = promotion_bottom[0].number_input(
            "Rendement directionnel minimum",
            value=current.promotion_min_mean_directional_return,
            help="Le rendement doit être strictement supérieur à cette valeur.",
        )
        promotion_max_opposite = promotion_bottom[1].number_input(
            "Mouvements opposés maximum", min_value=0.0, max_value=1.0,
            value=current.promotion_max_opposite_movement_frequency,
            help="Fréquence maximale autorisée des mouvements opposés sur le holdout.",
        )
        if st.button("Enregistrer les paramètres", type="primary"):
            parsed_quantiles = tuple(
                float(item.strip()) for item in quantiles.split(",") if item.strip()
            )
            new_config = replace(
                current,
                model_history_days=int(history),
                permutation_depth=int(permutation),
                max_generated_sets=int(max_sets),
                lag_depth=int(lag),
                intraday_target_threshold=float(up_threshold),
                intraday_down_threshold=float(down_threshold),
                walk_forward_window_mode=window_mode,
                walk_forward_min_train_size=int(min_train),
                walk_forward_train_size=int(rolling_train),
                walk_forward_test_size=int(test_size),
                walk_forward_step_size=int(step),
                final_holdout_size=int(holdout),
                walk_forward_end_offset_sessions=int(end_offset),
                predictor_prefilter_batch_size=int(prefilter_batch_size),
                walk_forward_batch_size=int(walk_forward_batch_size),
                final_holdout_batch_size=int(final_holdout_batch_size),
                walk_forward_max_combinations_per_batch=int(
                    max_combinations_per_batch
                ),
                temporal_min_candidate_yield_ratio=float(temporal_candidate_yield),
                temporal_max_auc_degradation=float(temporal_auc_degradation),
                temporal_min_precision_edge=float(temporal_precision_edge),
                temporal_min_mean_directional_return=float(temporal_directional_return),
                temporal_confidence_level=float(temporal_confidence),
                temporal_max_ci_width=float(temporal_ci_width),
                promotion_min_holdout_signals=int(promotion_min_signals),
                promotion_min_holdout_auc=float(promotion_min_auc),
                promotion_min_holdout_precision=float(promotion_min_precision),
                promotion_min_mean_directional_return=float(promotion_min_return),
                promotion_max_opposite_movement_frequency=float(promotion_max_opposite),
                predictor_prefilter_enabled=bool(prefilter_enabled),
                predictor_prefilter_top_n=int(prefilter_top_n),
                predictor_prefilter_min_median_auc=float(prefilter_median_auc),
                predictor_prefilter_min_pct_above_random=float(prefilter_pct_random),
                predictor_prefilter_min_worst_auc=float(prefilter_worst_auc),
                predictor_prefilter_max_auc_std=float(prefilter_auc_std),
                predictor_prefilter_correlation_threshold=float(
                    prefilter_correlation
                ),
                xgb_max_depth=int(max_depth),
                xgb_eta=float(eta),
                xgb_rounds=int(rounds),
                xgb_min_child_weight=float(child),
                xgb_subsample=float(subsample),
                xgb_colsample_bytree=float(colsample),
                xgb_gamma=float(gamma),
                xgb_reg_alpha=float(alpha),
                xgb_reg_lambda=float(reg_lambda),
                qualification_min_windows=int(min_windows),
                qualification_min_median_auc=float(median_auc),
                qualification_min_pct_windows_above_random=float(pct_random),
                qualification_min_worst_window_auc=float(worst_auc),
                qualification_min_positive_observations=int(min_positive),
                qualification_max_auc_std=float(max_auc_std),
                final_confirmation_min_auc=float(final_auc),
                prediction_threshold=float(prediction_threshold),
                model_selection_predictive_quality_weight=float(selection_predictive_weight),
                model_selection_stability_weight=float(selection_stability_weight),
                model_selection_holdout_weight=float(selection_holdout_weight),
                model_selection_signal_quality_weight=float(selection_signal_weight),
                model_selection_sample_adequacy_weight=float(selection_sample_weight),
                market_cache_workers=int(workers),
                combination_workers=int(combination_workers),
                xgb_nthread=int(nthread),
                xgb_seed=int(seed),
                threshold_calibration_min_signals_per_window=int(min_signals),
                threshold_calibration_min_robust_signals=int(min_robust_signals),
                threshold_calibration_min_window_fraction=float(min_window_fraction),
                threshold_calibration_precision_tolerance=float(precision_tolerance),
                threshold_calibration_quantiles=parsed_quantiles,
                threshold_parameter_calibration_max_models=(
                    None
                    if unlimited_threshold_parameter_models
                    else int(threshold_parameter_calibration_max_models)
                ),
            )
            st.session_state.lab_config = new_config
            st.session_state.lab_calendar = calendar
            st.session_state.lab_combinations_per_target = int(combinations)
            st.session_state.max_concurrent_heavy_jobs = int(max_jobs)
            st.session_state.lab_evaluate_holdout = evaluate_holdout
            st.session_state.sensitivity_threshold_min = float(sensitivity_threshold_min)
            st.session_state.sensitivity_threshold_max = float(sensitivity_threshold_max)
            st.session_state.sensitivity_threshold_step = float(sensitivity_threshold_step)

            ui_settings = {
                "lab_calendar": st.session_state.lab_calendar,
                "lab_combinations_per_target": st.session_state.lab_combinations_per_target,
                "lab_evaluate_holdout": st.session_state.lab_evaluate_holdout,
                "max_concurrent_heavy_jobs": st.session_state.max_concurrent_heavy_jobs,
                "sensitivity_threshold_min": st.session_state.sensitivity_threshold_min,
                "sensitivity_threshold_max": st.session_state.sensitivity_threshold_max,
                "sensitivity_threshold_step": st.session_state.sensitivity_threshold_step,
            }
            try:
                save_user_settings(
                    new_config, ui_settings, default_config=DEFAULT_CONFIG
                )
            except (OSError, TypeError, ValueError) as error:
                st.error(
                    "Paramètres appliqués à la session, mais la persistance a échoué : "
                    f"{error}"
                )
            else:
                st.success("Paramètres enregistrés.")


def _history_model_contexts(project_root) -> dict[str, str]:
    return {
        model.model_id: f"{model.target} ← {' + '.join(model.predictors)}"
        for model in ModelService(project_root).models()
    }


def _history_filters(
    runs: list[dict[str, object]],
    *,
    allowed_types: frozenset[str],
    models: dict[str, str],
    details_by_run_id: Mapping[str, Mapping[str, object]],
    key_prefix: str,
) -> list[dict[str, object]]:
    columns = st.columns(5)
    job_options = ["Tous", *sorted(allowed_types, key=lambda item: JOB_LABELS[item])]
    selected_type = columns[0].selectbox(
        "Type de run",
        job_options,
        format_func=lambda item: "Tous" if item == "Tous" else JOB_LABELS[item],
        key=f"{key_prefix}-type",
    )
    statuses = ["Tous", *sorted({str(run["status"]) for run in runs})]
    selected_status = columns[1].selectbox("Statut", statuses, key=f"{key_prefix}-status")
    period = columns[2].selectbox(
        "Période", ["Aujourd’hui", "7 jours", "30 jours", "Tout"], index=3, key=f"{key_prefix}-period"
    )
    model_options = [None, *sorted(models)]
    selected_model = columns[3].selectbox(
        "Modèle",
        model_options,
        format_func=lambda item: "Tous" if item is None else models[item],
        key=f"{key_prefix}-model",
    )
    selected_storage = columns[4].selectbox(
        "Stockage",
        ["Complet", "Résumé seulement", "Tous"],
        key=f"{key_prefix}-storage",
    )
    return list(filter_runs(
        runs,
        allowed_types=allowed_types,
        job_type=selected_type,
        status=selected_status,
        period=period,
        storage=selected_storage,
        model_id=selected_model,
        detail_loader=lambda run_id: details_by_run_id[run_id],
    ))


def _render_walk_forward_promotion(
    run_id: str,
    *,
    project_root,
) -> None:
    results = project_root / "runs" / run_id / "results"
    qualification_path = results / "qualification.csv"
    if not qualification_path.exists():
        st.info("Les combinaisons qualifiées ne sont pas disponibles pour ce run.")
        return
    qualification = pd.read_csv(qualification_path)
    holdout_path = results / "final_holdout.csv"
    holdout = pd.read_csv(holdout_path) if holdout_path.exists() else pd.DataFrame()
    selection_path = results / "selection_results.csv"
    scores = pd.read_csv(selection_path) if selection_path.exists() else pd.DataFrame()
    combinations = qualified_combinations_table(qualification, holdout, scores)
    st.subheader("Combinaisons qualifiées")
    if combinations.empty:
        st.info("Aucune combinaison ne satisfait les critères de qualification.")
        return
    selection = st.dataframe(
        combinations,
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="single-row",
        key=f"qualified-combinations-{run_id}",
    )
    selected_rows = _selected_rows(selection, len(combinations))
    selected_key = f"selected-qualified-combination-{run_id}"
    if selected_rows:
        st.session_state[selected_key] = combinations.iloc[selected_rows[0]]["Combinaison"]
    set_name = st.session_state.get(selected_key)
    if set_name not in set(combinations["Combinaison"]):
        st.caption("Sélectionnez une combinaison dans le tableau pour la promouvoir.")
        return
    selected = combinations[combinations["Combinaison"] == set_name].iloc[0]
    st.caption(f"Sélection : {selected['Cible']} ← {selected['Predictors']}")
    model_service = ModelService(project_root)
    existing = already_promoted(
        walk_forward_run=run_id,
        set_name=str(set_name),
        models=model_service.models(),
    )
    if existing is not None:
        st.info(f"Déjà promue — statut : {existing.status.value}.")
        return
    with st.expander("Sources de calibration optionnelles"):
        xgb_run = st.text_input("Run calibration XGBoost", key=f"xgb-source-{run_id}") or None
        threshold_run = st.text_input("Run calibration seuils", key=f"threshold-source-{run_id}") or None
    if st.button("Promouvoir comme candidat production", type="primary", key=f"promote-{run_id}"):
        try:
            model, created = model_service.promote(
                run_id,
                str(set_name),
                xgboost_calibration_run=xgb_run,
                threshold_calibration_run=threshold_run,
            )
        except (FileNotFoundError, KeyError, ValueError) as error:
            st.error(f"Promotion impossible : {error}")
        else:
            if created:
                st.success(f"Candidat production créé : {model.target} ← {' + '.join(model.predictors)}")
            else:
                st.info(f"Combinaison déjà promue — statut : {model.status.value}.")


def _render_threshold_calibration_promotion(
    run_id: str,
    *,
    project_root: Path,
    configuration: dict[str, object],
) -> None:
    """Inspect frozen per-set thresholds and promote through the existing service."""

    metrics, holdout, selected_by_set = load_threshold_calibration_artifacts(
        project_root, run_id
    )
    results = threshold_calibration_table(metrics, holdout, selected_by_set)
    st.subheader("Résultats de calibration des seuils")
    if results.empty:
        st.info("Aucun résultat par combinaison n'est disponible pour ce run.")
        return
    rstock_config = configuration.get("rstock_config", {})
    rstock_config = rstock_config if isinstance(rstock_config, dict) else {}
    minimum_robust_signals = int(
        rstock_config.get(
            "threshold_calibration_min_robust_signals",
            DEFAULT_CONFIG.threshold_calibration_min_robust_signals,
        )
    )
    holdout_predictions = load_threshold_holdout_predictions(project_root, run_id)
    policy = promotion_policy(rstock_config)
    results = threshold_promotion_guidance(
        results, selected_by_set, promotion_config=rstock_config
    )
    filter_defaults = {
        f"threshold-direction-{run_id}": "Up",
        f"threshold-min-signals-{run_id}": policy["promotion_min_holdout_signals"],
        f"threshold-min-precision-{run_id}": policy["promotion_min_holdout_precision"],
        f"threshold-min-auc-{run_id}": policy["promotion_min_holdout_auc"],
        f"threshold-max-opposite-{run_id}": policy["promotion_max_opposite_movement_frequency"],
        f"threshold-min-directional-return-{run_id}": policy[
            "promotion_min_mean_directional_return"
        ],
        f"threshold-promotion-status-{run_id}": "Tous",
        f"threshold-sort-{run_id}": "Précision holdout",
    }
    for key, value in filter_defaults.items():
        st.session_state.setdefault(key, value)
    filters = st.columns(4)
    direction = filters[0].selectbox(
        "Direction", ["Up", "Down", "Toutes"],
        key=f"threshold-direction-{run_id}",
    )
    min_signals = filters[1].number_input(
        "Signaux holdout minimum", min_value=0, step=1,
        key=f"threshold-min-signals-{run_id}",
    )
    sort_options = [
        "Précision holdout", "AUC holdout", "Rendement directionnel moyen"
    ]
    if "Score" in results:
        sort_options.append("Score")
    sort_key = f"threshold-sort-{run_id}"
    if st.session_state[sort_key] not in sort_options:
        st.session_state[sort_key] = sort_options[0]
    sort_by = filters[2].selectbox(
        "Trier par",
        sort_options,
        key=f"threshold-sort-{run_id}",
    )
    promotion_status = filters[3].selectbox(
        "Statut promotion",
        ["Tous", "Candidat", "Non candidat"],
        key=f"threshold-promotion-status-{run_id}",
    )
    quality_filters = st.columns(4)
    min_precision_value = quality_filters[0].number_input(
        "Précision holdout minimale", min_value=0.0, max_value=1.0,
        step=0.01, key=f"threshold-min-precision-{run_id}",
    )
    min_auc_value = quality_filters[1].number_input(
        "AUC holdout minimale", min_value=0.0, max_value=1.0,
        step=0.01, key=f"threshold-min-auc-{run_id}",
    )
    max_opposite_value = quality_filters[2].number_input(
        "Fréquence maximale de mouvement opposé", min_value=0.0, max_value=1.0,
        step=0.01, key=f"threshold-max-opposite-{run_id}",
    )
    min_return_value = quality_filters[3].number_input(
        "Rendement directionnel moyen minimal (filtre ; 0 désactive)", step=0.001,
        format="%.3f", key=f"threshold-min-directional-return-{run_id}",
        help="Filtre d'affichage indépendant : 0,00 le désactive.",
    )
    filtered = filter_threshold_calibration_results(
        results,
        direction=direction,
        min_signals=int(min_signals),
        min_precision=(None if min_precision_value <= 0 else float(min_precision_value)),
        min_holdout_auc=(None if min_auc_value <= 0 else float(min_auc_value)),
        max_opposite_move_frequency=(
            None if max_opposite_value >= 1 else float(max_opposite_value)
        ),
        min_directional_return=(
            None if min_return_value == 0 else float(min_return_value)
        ),
        promotion_status=promotion_status,
        sort_by=sort_by,
    )
    selection = st.dataframe(
        filtered, hide_index=True, width="stretch", on_select="rerun",
        selection_mode="single-row", key=f"threshold-results-{run_id}",
    )
    st.subheader("Synthèse de sensibilité des seuils")
    sensitivity_summary = threshold_sensitivity_summary(
        filtered,
        holdout_predictions,
        up_target_threshold=float(rstock_config.get("intraday_target_threshold", 0.01)),
        down_target_threshold=float(rstock_config.get("intraday_down_threshold", 0.01)),
        minimum_robust_signals=minimum_robust_signals,
        sensitivity_threshold_min=float(st.session_state.sensitivity_threshold_min),
        sensitivity_threshold_max=float(st.session_state.sensitivity_threshold_max),
        sensitivity_threshold_step=float(st.session_state.sensitivity_threshold_step),
    )
    selection_summary = threshold_calibration_selection_summary(filtered, metrics)
    if not sensitivity_summary.empty:
        sensitivity_summary = sensitivity_summary.merge(
            selection_summary,
            on=["Combinaison", "Direction"],
            how="left",
        )
    if sensitivity_summary.empty:
        st.caption("Aucune sensibilité holdout disponible pour les combinaisons visibles.")
    else:
        st.dataframe(
            sensitivity_summary,
            hide_index=True,
            width="stretch",
            column_config={
                "Seuil calibré": st.column_config.NumberColumn(format="%.4f"),
                "Meilleur seuil robuste": st.column_config.NumberColumn(format="%.4f"),
                "Delta seuil": st.column_config.NumberColumn(format="%+.4f"),
                "Précision au seuil calibré": st.column_config.NumberColumn(format="percent"),
                "Précision au meilleur seuil robuste": st.column_config.NumberColumn(format="percent"),
                "Delta précision": st.column_config.NumberColumn(format="%+.2%"),
                "Rendement directionnel moyen au seuil calibré": st.column_config.NumberColumn(format="percent"),
                "Rendement directionnel moyen au meilleur seuil robuste": st.column_config.NumberColumn(format="percent"),
                "Delta rendement": st.column_config.NumberColumn(format="%+.2%"),
                "Fréquence mouvement opposé au seuil calibré": st.column_config.NumberColumn(format="percent"),
                "Fréquence mouvement opposé au meilleur seuil robuste": st.column_config.NumberColumn(format="percent"),
            },
        )
    selected_rows = _selected_rows(selection, len(filtered))
    selected_key = f"selected-threshold-result-{run_id}"
    if selected_rows:
        st.session_state[selected_key] = filtered.iloc[selected_rows[0]].to_dict()
    elif selected_key in st.session_state:
        st.session_state.pop(selected_key, None)
    chosen = st.session_state.get(selected_key)
    if not isinstance(chosen, dict):
        st.caption("Sélectionnez une combinaison pour la promouvoir.")
        return
    st.caption(
        "Aide à la décision de promotion : "
        f"{chosen.get('Raison', 'Métriques insuffisantes.')}"
    )
    set_name = str(chosen.get("Combinaison", ""))
    selected_direction = str(chosen.get("Direction", ""))
    _render_threshold_sensitivity_analysis(
        project_root,
        run_id,
        set_name=set_name,
        direction=selected_direction,
        calibrated_threshold=chosen.get("Seuil calibré"),
        configuration=configuration,
    )
    st.subheader("Diagnostic du choix du seuil — calibration")
    choice_diagnostics = threshold_calibration_choice_diagnostic_table(
        metrics, set_name=set_name, direction=selected_direction
    )
    if choice_diagnostics.empty:
        st.caption("Diagnostics de calibration indisponibles pour cette combinaison.")
    else:
        st.dataframe(
            choice_diagnostics,
            hide_index=True,
            width="stretch",
            column_config={
                "Seuil": st.column_config.NumberColumn(format="%.4f"),
                "Fraction de fenêtres admissibles": st.column_config.NumberColumn(format="percent"),
                "Précision calibration": st.column_config.NumberColumn(format="percent"),
                "Stabilité précision": st.column_config.NumberColumn(format="percent"),
                "Rendement directionnel moyen": st.column_config.NumberColumn(format="percent"),
                "Stabilité rendement": st.column_config.NumberColumn(format="percent"),
                "Fréquence mouvement opposé": st.column_config.NumberColumn(format="percent"),
                "F1": st.column_config.NumberColumn(format="%.3f"),
            },
        )
    frozen = selected_by_set.get(set_name, {})
    directional_selection = frozen.get(selected_direction, {})
    if isinstance(directional_selection, dict):
        calibration_metrics = directional_selection.get("calibration_metrics", {})
        calibration_metrics = (
            calibration_metrics if isinstance(calibration_metrics, dict) else {}
        )
        if directional_selection.get("status") == "selected":
            st.caption(
                "Sélection calibration · seuil : "
                f"{directional_selection.get('threshold', '—')} · précision : "
                f"{_format_metric(calibration_metrics.get('precision'), percent=True)} · "
                f"{directional_selection.get('total_signals', '—')} signaux · rendement : "
                f"{_format_metric(calibration_metrics.get('directional_return_mean'), percent=True)} · "
                "mouvement opposé : "
                f"{_format_metric(calibration_metrics.get('opposite_move_frequency'), percent=True)} · "
                f"raison : {directional_selection.get('selection_reason', '—')}"
            )
    if not all(
        isinstance(frozen.get(item), dict)
        and frozen[item].get("status") == "selected"
        and frozen[item].get("threshold") is not None
        for item in ("Up", "Down")
    ):
        st.info("Cette combinaison ne possède pas de seuils gelés admissibles pour Up et Down.")
        return
    source_run = configuration.get("source_walk_forward_run")
    model_service = ModelService(project_root)
    if not source_run:
        source_run = model_service.resolve_walk_forward_source(run_id, set_name)
    if not source_run:
        st.info(
            "Aucun run walk-forward antérieur avec la même population n'a "
            "qualifié cette combinaison. Cette calibration historique ne peut "
            "pas être promue sans contourner les règles de qualification."
        )
        return
    st.caption(
        f"Sélection : {chosen.get('Cible', '—')} ← {chosen.get('Predictors', '—')} "
        f"· seuil {selected_direction} : {chosen.get('Seuil calibré', '—')}"
    )
    candidate = chosen.get("Statut promotion") == "Candidat"
    if not candidate:
        st.warning(
            "Ce modèle ne satisfait pas la politique de promotion : "
            f"{chosen.get('Raison', 'raison indisponible')}"
        )
        confirmed = st.checkbox(
            "Je confirme une promotion manuelle malgré ces critères.",
            key=f"confirm-override-threshold-{run_id}-{set_name}-{selected_direction}",
        )
    else:
        confirmed = True
    label = "Promouvoir le modèle" if candidate else "Promouvoir malgré les critères"
    if st.button(
        label,
        type="primary",
        disabled=not confirmed,
        key=f"promote-threshold-{run_id}-{set_name}-{selected_direction}",
    ):
        try:
            model, created = model_service.promote(
                str(source_run), set_name,
                threshold_calibration_run=run_id,
                selected_threshold_direction=selected_direction,
            )
        except (FileNotFoundError, KeyError, ValueError) as error:
            st.error(f"Promotion impossible : {error}")
        else:
            if created:
                st.success(f"Candidat production créé : {model.target} ← {' + '.join(model.predictors)}")
            else:
                st.info(f"Combinaison déjà promue — statut : {model.status.value}.")


def _render_threshold_sensitivity_analysis(
    project_root: Path,
    run_id: str,
    *,
    set_name: str,
    direction: str,
    calibrated_threshold: object,
    configuration: dict[str, object],
) -> None:
    """Render a read-only holdout threshold projection for one selected set."""

    st.subheader("Analyse de sensibilité au seuil")
    st.caption("Analyse de sensibilité au seuil — Holdout")
    rstock_config = configuration.get("rstock_config", {})
    rstock_config = rstock_config if isinstance(rstock_config, dict) else {}
    minimum_robust_signals = int(
        rstock_config.get(
            "threshold_calibration_min_robust_signals",
            DEFAULT_CONFIG.threshold_calibration_min_robust_signals,
        )
    )
    sensitivity = threshold_sensitivity_table(
        load_threshold_holdout_predictions(project_root, run_id),
        set_name=set_name,
        direction=direction,
        calibrated_threshold=calibrated_threshold,
        up_target_threshold=float(rstock_config.get("intraday_target_threshold", 0.01)),
        down_target_threshold=float(rstock_config.get("intraday_down_threshold", 0.01)),
        minimum_robust_signals=minimum_robust_signals,
        sensitivity_threshold_min=float(
            st.session_state.sensitivity_threshold_min
        ),
        sensitivity_threshold_max=float(
            st.session_state.sensitivity_threshold_max
        ),
        sensitivity_threshold_step=float(
            st.session_state.sensitivity_threshold_step
        ),
    )
    if sensitivity.empty:
        st.info("Analyse de sensibilité indisponible pour ce run historique.")
        return
    st.caption(
        "✓ identifie le seuil calibré actuel et le meilleur seuil par précision "
        f"avec au moins {minimum_robust_signals} signaux. Cette analyse ne modifie pas le run."
    )
    st.dataframe(
        sensitivity,
        hide_index=True,
        width="stretch",
        column_config={
            "Seuil": st.column_config.NumberColumn(format="%.4f"),
            "Précision": st.column_config.NumberColumn(format="percent"),
            "Recall": st.column_config.NumberColumn(format="percent"),
            "F1": st.column_config.NumberColumn(format="%.3f"),
            "Rendement directionnel moyen": st.column_config.NumberColumn(format="percent"),
            "Rendement médian": st.column_config.NumberColumn(format="percent"),
            "Fréquence mouvement opposé": st.column_config.NumberColumn(format="percent"),
            "MFE moyen": st.column_config.NumberColumn(format="percent"),
            "MAE moyen": st.column_config.NumberColumn(format="percent"),
        },
    )


def _render_resume_controls(
    run_id: str,
    status: dict[str, object],
    detail: dict[str, object],
) -> None:
    resumable_types = {
        JobType.WALK_FORWARD.value,
        JobType.THRESHOLD_PARAMETER_CALIBRATION.value,
        JobType.END_TO_END.value,
    }
    if status.get("job_type") not in resumable_types or status.get("status") not in {
        "failed", "cancelled", "interrupted"
    }:
        return
    manifest = detail.get("checkpoint")
    error = detail.get("checkpoint_error")
    checkpointed_resume = status.get("job_type") == JobType.WALK_FORWARD.value
    if checkpointed_resume and isinstance(manifest, dict):
        completed_phases = list(manifest.get("phases_completed", []))
        current_phase = str(manifest.get("current_phase") or "—")
        batches = manifest.get("batches", {})
        batch_info = batches.get(current_phase, {}) if isinstance(batches, dict) else {}
        if not batch_info and isinstance(batches, dict):
            batch_info = batches.get("walk_forward", {})
        completed_batches = len(batch_info.get("completed", [])) if isinstance(batch_info, dict) else 0
        total_batches = batch_info.get("total") if isinstance(batch_info, dict) else None
        last = manifest.get("last_checkpoint", {})
        last_time = last.get("at", "—") if isinstance(last, dict) else "—"
        st.caption(
            "Checkpoint · dernière phase complétée : "
            f"{WORKFLOW_PHASE_LABELS.get(completed_phases[-1], completed_phases[-1]) if completed_phases else 'aucune'} · "
            f"arrêt : {WORKFLOW_PHASE_LABELS.get(current_phase, current_phase)} · "
            f"batchs : {completed_batches}/{total_batches if total_batches is not None else '—'} · "
            f"dernier checkpoint : {last_time}"
        )
    if checkpointed_resume and error:
        st.warning(str(error))
    actions = st.columns(2)
    if actions[0].button(
        "Reprendre le run",
        key=f"resume-run-{run_id}",
        disabled=checkpointed_resume and bool(error),
        width="stretch",
    ):
        try:
            _service().resume(run_id)
        except (OSError, ValueError, RuntimeError) as resume_error:
            st.error(f"Reprise impossible : {resume_error}")
        else:
            st.success("Reprise soumise au worker.")
            st.rerun()
    if actions[1].button(
        "Relancer depuis le début",
        key=f"restart-run-{run_id}",
        width="stretch",
    ):
        try:
            restarted = _service().restart(run_id)
        except (OSError, ValueError, RuntimeError) as restart_error:
            st.error(f"Relance impossible : {restart_error}")
        else:
            st.success(f"Nouveau run créé : {restarted.run_id}")
            st.rerun()


def _render_history_detail(
    run_id: str,
    *,
    status: dict[str, object],
    detail: dict[str, object],
    context: str,
    summary_text: str,
) -> None:
    st.divider()
    st.subheader(summary_text if summary_text != "—" else "Détail du run")
    st.caption(f"ID technique : {run_id}")
    _render_resume_controls(run_id, status, detail)
    _render_job_detail_tabs(_service(), run_id, status=status, detail=detail)
    return
def _history_navigation(mode: str, run_ids: list[str]) -> None:
    st.session_state["history-navigation"] = {"mode": mode, "run_ids": run_ids}
    st.rerun()


def _clear_history_navigation() -> None:
    st.session_state.pop("history-navigation", None)
    st.rerun()


def _load_run_analytics(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> RunAnalytics:
    qualification, holdout = load_walk_forward_artifacts(
        st.session_state.lab_config.project_root, run_id
    )
    selection_results = load_model_selection_artifact(
        st.session_state.lab_config.project_root, run_id
    )
    return analyze_run(status, detail, qualification, holdout, selection_results)


def _format_metric(value: object, *, percent: bool = False) -> str:
    if value is None or pd.isna(value):
        return "—"
    return f"{float(value):.2%}" if percent else f"{float(value):.2f}"


def _render_xgboost_calibration_selection(run_id: str) -> None:
    development, holdout, selected = load_xgboost_calibration_artifacts(
        st.session_state.lab_config.project_root, run_id
    )
    table = xgboost_calibration_selection_table(selected, development, holdout)
    if not selected:
        st.caption("Les artefacts de sélection XGBoost ne sont pas disponibles pour ce run historique.")
        return
    st.subheader("Sélection et validation XGBoost")
    st.caption("Stabilité développement = écart-type ROC-AUC entre les fenêtres.")
    st.dataframe(
        xgboost_calibration_selection_display_table(table),
        hide_index=True,
        width="stretch",
    )
    with st.expander("Paramètres XGBoost complets"):
        st.json({
            direction: payload.get("parameters", {})
            for direction, payload in selected.items()
            if isinstance(payload, dict)
        })


def _render_threshold_parameter_calibration_selection(run_id: str) -> None:
    results = st.session_state.lab_config.project_root / "runs" / run_id / "results"
    selected_path = results / "selected_threshold_calibration_configuration.json"
    metrics_path = results / "development_metrics_by_configuration.csv"
    tested_path = results / "tested_threshold_parameter_configurations.csv"
    if not selected_path.exists() or not metrics_path.exists():
        st.caption(
            "Les artefacts de calibration des paramètres de seuils ne sont pas "
            "disponibles pour ce run historique."
        )
        return
    selected = json.loads(selected_path.read_text(encoding="utf-8"))
    metrics = pd.read_csv(metrics_path)
    tested = pd.read_csv(tested_path) if tested_path.exists() else pd.DataFrame()
    table = threshold_parameter_calibration_table(metrics, tested)
    st.subheader("Calibration des paramètres de seuils")
    columns = st.columns(4)
    columns[0].metric("Configuration gagnante", selected.get("configuration", "—"))
    columns[1].metric("Candidats testés", selected.get("candidates_tested", "—"))
    columns[2].metric("Candidats admissibles", selected.get("eligible_candidates", "—"))
    columns[3].metric(
        "Écart avec #2",
        _format_metric(selected.get("runner_up_primary_criterion_gap"), percent=True),
    )
    st.caption(
        f"Parent direct : {selected.get('parent_run') or '—'} · "
        "Source XGBoost : "
        f"{selected.get('source_xgboost_calibration_run') or selected.get('xgboost_parameter_source') or '—'} · "
        f"Digest : {selected.get('configuration_sha256', '—')}"
    )
    st.dataframe(
        table,
        hide_index=True,
        width="stretch",
        column_config={
            "% modèles admissibles (critère principal)": (
                st.column_config.NumberColumn(format="percent")
            ),
            "Précision médiane": st.column_config.NumberColumn(format="%.4f"),
            "F1 médian": st.column_config.NumberColumn(format="%.4f"),
            "Fraction fenêtres admissibles": st.column_config.NumberColumn(format="percent"),
            "Stabilité précision": st.column_config.NumberColumn(format="%.4f"),
            "Rendement directionnel moyen": st.column_config.NumberColumn(format="percent"),
            "Stabilité rendement": st.column_config.NumberColumn(format="%.4f"),
            "Mouvement opposé": st.column_config.NumberColumn(format="percent"),
        },
    )
    with st.expander("Paramètres gagnants complets"):
        st.json(selected.get("parameters", {}))


def _promote_combination_action(run_id: str, combination: pd.Series) -> None:
    st.caption(f"Sélection : {combination['Cible']} ← {combination['Predictors']}")
    model_service = ModelService(st.session_state.lab_config.project_root)
    existing = already_promoted(
        walk_forward_run=run_id,
        set_name=str(combination["Combinaison"]),
        models=model_service.models(),
    )
    if existing is not None:
        st.info(f"Déjà promue — statut : {existing.status.value}.")
        return
    if st.button(
        "Promouvoir comme candidat production",
        type="primary",
        key=f"promote-analysis-{run_id}-{combination['Combinaison']}",
    ):
        try:
            model, created = model_service.promote(run_id, str(combination["Combinaison"]))
        except (FileNotFoundError, KeyError, ValueError) as error:
            st.error(f"Promotion impossible : {error}")
        else:
            message = (
                f"Candidat production créé : {model.target} ← {' + '.join(model.predictors)}"
                if created else f"Combinaison déjà promue — statut : {model.status.value}."
            )
            (st.success if created else st.info)(message)


def _selected_combination(
    run_id: str, table: pd.DataFrame,
) -> pd.Series | None:
    if table.empty:
        return None
    detail_only = [
        "Score qualité prédictive", "Score stabilité", "Score holdout",
        "Score qualité signal", "Score adéquation échantillon",
    ]
    event = st.dataframe(
        table.drop(columns=["Eligible", "Holdout confirmé", *detail_only], errors="ignore"),
        hide_index=True, width="stretch",
        on_select="rerun", selection_mode="single-row", key=f"analysis-combinations-{run_id}",
    )
    selected_rows = _selected_rows(event, len(table))
    key = f"analysis-selected-combination-{run_id}"
    if selected_rows:
        st.session_state[key] = str(table.iloc[selected_rows[0]]["Combinaison"])
    selected = st.session_state.get(key)
    matching = table[table["Combinaison"] == selected]
    return None if matching.empty else matching.iloc[0]


def _render_combination_filters(analytics: RunAnalytics) -> pd.DataFrame:
    combinations = analytics.combinations
    targets = ["Toutes", *sorted(combinations["Cible"].dropna().unique())]
    columns = st.columns(4)
    target = columns[0].selectbox("Cible", targets, key=f"analysis-target-{analytics.run_id}")
    depth = columns[1].selectbox(
        "Profondeur", ["Toutes", str(analytics.depth)], key=f"analysis-depth-{analytics.run_id}"
    )
    min_dev = columns[2].number_input(
        "AUC dev min", min_value=0.0, max_value=1.0, value=0.0,
        key=f"analysis-dev-{analytics.run_id}",
    )
    min_holdout = columns[3].number_input(
        "AUC holdout min", min_value=0.0, max_value=1.0, value=0.0,
        key=f"analysis-holdout-{analytics.run_id}",
    )
    columns = st.columns(4)
    min_worst = columns[0].number_input(
        "Worst AUC min", min_value=0.0, max_value=1.0, value=0.0,
        key=f"analysis-worst-{analytics.run_id}",
    )
    max_dispersion = columns[1].number_input(
        "Dispersion max", min_value=0.0, max_value=1.0, value=1.0,
        key=f"analysis-dispersion-{analytics.run_id}",
    )
    min_positive = columns[2].number_input(
        "Observations positives min", min_value=0, value=0,
        key=f"analysis-positive-{analytics.run_id}",
    )
    confirmed = columns[3].checkbox(
        "Holdout confirmé seulement", key=f"analysis-confirmed-{analytics.run_id}"
    )
    return filter_combinations(
        combinations, target=target, depth=depth, min_dev_auc=float(min_dev),
        min_holdout_auc=float(min_holdout), min_worst_auc=float(min_worst),
        max_dispersion=float(max_dispersion), min_positive_observations=int(min_positive),
        confirmed_only=confirmed,
    )


def _render_run_configuration(detail: dict[str, object]) -> None:
    st.json(detail["configuration"])


def _render_run_files(detail: dict[str, object]) -> None:
    st.write(detail["files"] or "Aucun resultat publie")


def _render_run_logs(detail: dict[str, object]) -> None:
    st.code("\n".join(detail["log_tail"]) or "Aucun message")


def _render_run_technical_tabs(run_id: str, detail: dict[str, object]) -> None:
    st.caption(f"ID technique : {run_id}")
    technical_tabs = tabs_for_job(JobType.XGBOOST_CALIBRATION)[1:]
    render_lazy_tabs(
        st,
        technical_tabs,
        {
            "configuration": lambda: _render_run_configuration(detail),
            "files": lambda: _render_run_files(detail),
            "logs": lambda: _render_run_logs(detail),
        },
        key=f"run-technical-{run_id}",
    )


def _render_standard_results(
    run_id: str, job_type: JobType, status: dict[str, object], detail: dict[str, object]
) -> None:
    if job_type is JobType.FORWARD_SIMULATION:
        source = detail.get("configuration", {}).get("source_end_to_end_run")
        if source:
            st.caption(f"End-to-End source : {source}")
            if st.button("Ouvrir l’End-to-End source", key=f"forward-source-{run_id}"):
                st.session_state["selected-run-id"] = str(source)
                st.rerun()
    if job_type is JobType.XGBOOST_CALIBRATION:
        _render_xgboost_calibration_selection(run_id)
    elif job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION:
        _render_threshold_parameter_calibration_selection(run_id)
    elif (
        job_type is JobType.THRESHOLD_CALIBRATION
        and status.get("status") == "completed"
    ):
        _render_threshold_calibration_promotion(
            run_id,
            project_root=st.session_state.lab_config.project_root,
            configuration=detail["configuration"],
        )
    st.json(detail["summary"])


def _render_standard_job_tabs(
    run_id: str, job_type: JobType, status: dict[str, object], detail: dict[str, object]
) -> None:
    render_lazy_tabs(
        st,
        tabs_for_job(job_type),
        {
            "results": lambda: _render_standard_results(
                run_id, job_type, status, detail
            ),
            "configuration": lambda: _render_run_configuration(detail),
            "files": lambda: _render_run_files(detail),
            "logs": lambda: _render_run_logs(detail),
        },
        key=f"run-detail-{run_id}",
    )


def _walk_forward_analytics(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> RunAnalytics:
    return _load_run_analytics(run_id, status, detail)


def _render_walk_forward_metrics(analytics: RunAnalytics) -> None:
    metrics = st.columns(6)
    metrics[0].metric(
        "Combinaisons testees",
        analytics.tested_count if analytics.tested_count is not None else "-",
    )
    metrics[1].metric("Qualifiees", analytics.qualified_count)
    metrics[2].metric("Confirmees holdout", analytics.confirmed_count)
    metrics[3].metric("AUC dev mediane", _format_metric(analytics.dev_auc_median))
    metrics[4].metric("AUC holdout mediane", _format_metric(analytics.holdout_auc_median))
    windows = (
        pd.to_numeric(
            analytics.combinations.get("Fen\u00eatres valides"), errors="coerce"
        ).median()
        if not analytics.combinations.empty
        and "Fen\u00eatres valides" in analytics.combinations
        else None
    )
    metrics[5].metric("Fenetres (mediane)", _format_metric(windows))


def _render_walk_forward_summary(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> None:
    configuration = detail.get("configuration", {})
    raw_config = (
        configuration.get("rstock_config", {})
        if isinstance(configuration, Mapping)
        else {}
    )
    config = raw_config if isinstance(raw_config, Mapping) else {}
    mode = str(config.get("walk_forward_window_mode", "expanding"))
    train = (
        f"train fixe {int(config.get('walk_forward_train_size', 252))}"
        if mode == "rolling"
        else f"train min {int(config.get('walk_forward_min_train_size', 252))}"
    )
    st.caption(
        f"WF {'glissante' if mode == 'rolling' else 'expansive'} · {train} · "
        f"test {int(config.get('walk_forward_test_size', 63))} · "
        f"step {int(config.get('walk_forward_step_size', 63))} · "
        f"holdout {int(config.get('final_holdout_size', 63))} · "
        f"end offset {int(config.get('walk_forward_end_offset_sessions', 63))}"
    )
    analytics = _walk_forward_analytics(run_id, status, detail)
    _render_walk_forward_metrics(analytics)
    prefilter_table = predictor_prefilter_summary(detail.get("summary", {}))
    if not prefilter_table.empty:
        st.subheader("Pré-filtrage des prédicteurs")
        st.dataframe(prefilter_table, hide_index=True, width="stretch")
    rates = st.columns(2)
    rates[0].metric(
        "Taux de qualification",
        _format_metric(analytics.qualification_rate, percent=True),
    )
    rates[1].metric(
        "Taux de confirmation holdout",
        _format_metric(analytics.confirmation_rate, percent=True),
    )
    if analytics.combinations.empty:
        st.info("Les artefacts analytiques ne sont pas disponibles pour ce run historique.")
        return
    chart = analytics.combinations[["AUC dev", "AUC holdout"]].dropna(how="all")
    if not chart.empty:
        st.bar_chart(chart)
    scatter = analytics.combinations[["AUC dev", "AUC holdout"]].dropna()
    if not scatter.empty:
        st.caption("AUC developpement vs holdout - une ligne par combinaison")
        st.scatter_chart(scatter, x="AUC dev", y="AUC holdout")


def _render_walk_forward_analysis(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> None:
    analytics = _walk_forward_analytics(run_id, status, detail)
    if analytics.combinations.empty:
        st.caption("Aucune metrique de stabilite publiee pour ce run.")
    else:
        st.bar_chart(
            analytics.combinations[["Worst AUC", "Dispersion", "Delta dev\u2192holdout"]]
        )


def _render_walk_forward_combinations(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> None:
    analytics = _walk_forward_analytics(run_id, status, detail)
    st.caption(f"{analytics.qualified_count} combinaisons qualifiees")
    filtered = _render_combination_filters(analytics)
    st.caption(f"{len(filtered)} correspondent aux filtres")
    selected = _selected_combination(run_id, filtered)
    if selected is None:
        return
    with st.container(border=True):
        st.subheader(f"{selected['Cible']} <- {selected['Predictors']}")
        details_columns = st.columns(4)
        for column, name in zip(
            details_columns,
            ("AUC dev", "AUC holdout", "Delta dev\u2192holdout", "Worst AUC"),
            strict=True,
        ):
            column.metric(name, _format_metric(selected[name]))
        diagnostic = pd.DataFrame(
            [
                {"Indicateur": "Dispersion", "Valeur": selected.get("Dispersion")},
                {
                    "Indicateur": "Fenetres valides",
                    "Valeur": selected.get("Fen\u00eatres valides"),
                },
                {
                    "Indicateur": "Seuil calibré",
                    "Valeur": selected.get("Seuil calibr\u00e9", "-"),
                },
                {
                    "Indicateur": "Qualité signal",
                    "Valeur": selected.get("Score qualit\u00e9 signal", "-"),
                },
                {"Indicateur": "Score final", "Valeur": selected.get("Score", "-")},
                {"Indicateur": "Rang", "Valeur": selected.get("Rang", "-")},
            ]
        )
        st.dataframe(diagnostic, hide_index=True, width="stretch")
        subscores = [
            ("Qualité prédictive", "Score qualité prédictive"),
            ("Stabilité", "Score stabilité"),
            ("Holdout", "Score holdout"),
            ("Qualité signal", "Score qualité signal"),
            ("Adéquation échantillon", "Score adéquation échantillon"),
        ]
        if any(name in selected.index for _, name in subscores):
            st.markdown("**Sous-scores**")
            st.dataframe(
                pd.DataFrame(
                    [
                        {"Composante": label, "Score / 100": selected.get(name, "-")}
                        for label, name in subscores
                    ]
                ),
                hide_index=True,
                width="stretch",
            )
        _promote_combination_action(run_id, selected)


def _render_walk_forward_validation(
    run_id: str, status: dict[str, object], detail: dict[str, object]
) -> None:
    analytics = _walk_forward_analytics(run_id, status, detail)
    selected_name = st.session_state.get(f"analysis-selected-combination-{run_id}")
    selected = analytics.combinations[
        analytics.combinations["Combinaison"] == selected_name
    ]
    if selected.empty:
        st.caption("Selectionnez une combinaison dans l'onglet Combinaisons.")
        return
    row = selected.iloc[0]
    validation = pd.DataFrame(
        [
            {
                "Phase": "Developpement",
                "Fenetres": row["Fen\u00eatres valides"],
                "AUC mediane": row["AUC dev"],
                "AUC min": row["Worst AUC"],
                "Dispersion": row["Dispersion"],
                "Verdict": "Qualifiee developpement" if row["Eligible"] else "Non qualifiee",
            },
            {
                "Phase": "Holdout final",
                "Fenetres": "-",
                "AUC mediane": row["AUC holdout"],
                "AUC min": "-",
                "Dispersion": "-",
                "Verdict": "Holdout confirme" if row["Holdout confirm\u00e9"] else "Echec de confirmation holdout",
            },
        ]
    )
    st.dataframe(validation, hide_index=True, width="stretch")


def _render_walk_forward_batches(
    service: ExperimentService, run_id: str, detail: dict[str, object]
) -> None:
    manifest = detail.get("walk_forward_batch_manifest")
    batches = detail.get("walk_forward_batches")
    if not isinstance(manifest, dict):
        st.caption("Aucun manifest de batchs pour ce run monolithique.")
        return
    rows = walk_forward_batch_rows(batches)
    counts = batch_status_counts(batches)
    raw = int(manifest.get("raw_combination_count", 0))
    effective = int(manifest.get("prefiltered_combination_count", 0))
    planned = int(manifest.get("planned_batch_count", manifest.get("batch_count", 0)))
    maximum = manifest.get("max_combinations_per_batch", "-")
    progress_weight = sum(
        int(row.get("Combinaisons") or 0)
        * (
            100.0
            if row.get("Statut") == "completed"
            else float(row.get("Progression") or 0.0)
        )
        for row in rows
    )
    total_weight = sum(int(row.get("Combinaisons") or 0) for row in rows)
    progress = progress_weight / total_weight if total_weight else 0.0
    first = st.columns(4)
    first[0].metric("Combinaisons brutes", f"{raw:,}")
    first[1].metric("Après préfiltre", f"{effective:,}")
    first[2].metric("Batchs planifiés", planned)
    first[3].metric("Maximum par batch", maximum)
    second = st.columns(5)
    for column, status_name in zip(
        second[:4], ("completed", "running", "failed", "pending"), strict=True
    ):
        column.metric(status_name.capitalize(), counts[status_name])
    second[4].metric("Progression globale", f"{progress:.1f}%")
    if raw:
        st.caption(f"Réduction du préfiltre : {(raw - effective) / raw:.1%}")
    failed = [row for row in rows if row.get("Statut") == "failed"]
    if failed:
        st.error(
            "Batchs en échec : "
            + ", ".join(str(row.get("Batch")) for row in failed)
        )
    st.caption(
        "La reprise se fait sur le parent Walk-forward; les batchs completed "
        "ne seront pas recalculés."
    )
    table = pd.DataFrame(rows)
    event = st.dataframe(
        table,
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="single-row",
        key=f"wf-batches-{run_id}",
        column_config={
            "Progression": st.column_config.ProgressColumn(
                min_value=0.0, max_value=100.0, format="%.1f%%"
            )
        },
    )
    selected_rows = _selected_rows(event, len(rows))
    if not selected_rows:
        st.caption("Sélectionnez un batch pour inspecter son statut et ses logs.")
        return
    selected = rows[selected_rows[0]]
    child_run_id = str(selected["Run ID"])
    if selected.get("Statut") == "reserved":
        st.caption(
            f"Run technique réservé : {child_run_id}; répertoire non matérialisé."
        )
        return
    child = service.run(child_run_id)
    with st.container(border=True):
        st.subheader(f"Batch {selected['Batch']} - {selected['Statut']}")
        st.caption(f"ID technique : {child_run_id}")
        _render_run_technical_tabs(child_run_id, child)


def _render_walk_forward_tabs(
    service: ExperimentService,
    run_id: str,
    status: dict[str, object],
    detail: dict[str, object],
) -> None:
    has_batches = isinstance(detail.get("walk_forward_batch_manifest"), dict)
    render_lazy_tabs(
        st,
        tabs_for_job(JobType.WALK_FORWARD, has_walk_forward_batches=has_batches),
        {
            "summary": lambda: _render_walk_forward_summary(run_id, status, detail),
            "analysis": lambda: _render_walk_forward_analysis(run_id, status, detail),
            "combinations": lambda: _render_walk_forward_combinations(
                run_id, status, detail
            ),
            "validation": lambda: _render_walk_forward_validation(
                run_id, status, detail
            ),
            "walk_forward_batches": lambda: _render_walk_forward_batches(
                service, run_id, detail
            ),
            "technical": lambda: _render_run_technical_tabs(run_id, detail),
        },
        key=f"run-detail-{run_id}",
    )


def _render_pipeline_summary(
    run_id: str, detail: dict[str, object], service: ExperimentService | None = None
) -> None:
    summary = detail.get("summary", {})
    protocol = summary.get("walk_forward_protocol") if isinstance(summary, Mapping) else None
    if isinstance(protocol, str) and protocol:
        st.caption(protocol)
    snapshot = summary.get("forward_model_snapshot") if isinstance(summary, Mapping) else None
    if isinstance(snapshot, Mapping):
        st.caption(
            "Cutoff historique demandé : "
            f"{detail.get('configuration', {}).get('requested_historical_cutoff') or '—'} · "
            "séance résolue : "
            f"{snapshot.get('resolved_market_session_cutoff') or '—'}"
        )
        forward = summary.get("forward_simulation")
        if isinstance(forward, Mapping):
            st.caption(
                "Forward Simulation : "
                f"{forward.get('status', 'pending')} · run : {forward.get('child_run_id', '—')}"
            )
        if service is not None and snapshot.get("resolved_market_session_cutoff"):
            cutoff = pd.Timestamp(snapshot["resolved_market_session_cutoff"]).date()
            mode = st.radio(
                "Nouvelle Forward Simulation", ["63 séances", "126 séances", "Date de fin"],
                horizontal=True, key=f"forward-mode-{run_id}",
            )
            if mode == "Date de fin":
                requested_end = st.date_input(
                    "Date de fin Forward", value=cutoff + timedelta(days=90),
                    key=f"forward-end-{run_id}",
                )
                end = resolve_market_session_on_or_before(
                    requested_end, st.session_state.lab_calendar
                ).date()
            else:
                count = 63 if mode == "63 séances" else 126
                end = forward_market_sessions(
                    cutoff, st.session_state.lab_calendar, count
                )[-1].date()
            start = forward_market_sessions(cutoff, st.session_state.lab_calendar, 1)[0].date()
            if end <= cutoff:
                st.error("La fin Forward doit être postérieure au cutoff historique.")
            elif st.button("Lancer cette Forward Simulation", key=f"forward-start-{run_id}"):
                launched = service.start_forward_simulation(
                    run_id, start_date=start.isoformat(), end_date=end.isoformat()
                )
                st.success(f"Forward Simulation créée : {launched.run_id}")
    rows = pipeline_stage_rows(detail.get("pipeline_stages"))
    if not rows:
        st.info("Le manifest du pipeline n'est pas encore disponible.")
        return
    failed = [row for row in rows if row["Statut"] == "failed"]
    running = [row for row in rows if row["Statut"] == "running"]
    if failed:
        st.error(
            f"Étape en échec : {failed[0][PIPELINE_STAGE_LABEL_COLUMN]}"
        )
    elif running:
        st.info(
            f"Étape active : {running[0][PIPELINE_STAGE_LABEL_COLUMN]}"
        )
    elif all(row["Statut"] in {"completed", "disabled"} for row in rows):
        st.success("Pipeline terminé.")
    st.dataframe(
        pd.DataFrame(rows),
        hide_index=True,
        width="stretch",
        column_config={
            "Progression": st.column_config.ProgressColumn(
                min_value=0.0, max_value=100.0, format="%.1f%%"
            )
        },
    )

def _render_pipeline_child(
    service: ExperimentService,
    parent_run_id: str,
    detail: dict[str, object],
    renderer_key: str,
) -> None:
    stage_key = PIPELINE_CHILD_TABS[renderer_key]
    stage = pipeline_stage_by_key(detail.get("pipeline_stages"), stage_key)
    if stage is None:
        st.info("Cette étape n'est pas encore réservée dans le manifest.")
        return
    child_run_id = stage.get("child_run_id")
    if not child_run_id or stage.get("status") == "reserved":
        st.caption(
            f"Étape {stage.get('status', 'pending')} - aucun artefact chargé."
        )
        return
    child_detail = service.run(str(child_run_id))
    child_status = child_detail["status"]
    st.caption(
        f"Run enfant : {child_run_id} - statut : {child_status.get('status', '-')}"
    )
    _render_job_detail_tabs(
        service,
        str(child_run_id),
        status=child_status,
        detail=child_detail,
    )


def _render_pipeline_promotion(detail: dict[str, object]) -> None:
    stage = pipeline_stage_by_key(detail.get("pipeline_stages"), "promotion")
    if stage is None:
        st.info("L'étape Promotion n'est pas encore disponible.")
        return
    if stage.get("status") in {"not_requested", "disabled"}:
        st.info("Promotion désactivée (auto_promote_candidates=False).")
        return
    promotion = stage.get("promotion")
    if not isinstance(promotion, dict):
        st.caption(f"Promotion {stage.get('status', 'pending')}.")
        return
    diagnostics = promotion.get("diagnostics", [])
    candidates = promotion.get("candidates", [])
    candidate_count = int(promotion.get("candidate_count", 0))
    examined = len(diagnostics) if isinstance(diagnostics, list) else 0
    columns = st.columns(7)
    values = (
        ("Examinées", examined),
        ("Candidat", candidate_count),
        ("Non candidat", max(0, examined - candidate_count)),
        ("Créés", int(promotion.get("created_count", 0))),
        ("Réutilisés", int(promotion.get("reused_count", 0))),
        (
            "Échecs",
            sum(
                isinstance(item, dict) and item.get("status") == "failed"
                for item in candidates
            ),
        ),
        ("Statut", promotion.get("status", "pending")),
    )
    for column, (label, value) in zip(columns, values, strict=True):
        column.metric(label, value)
    completed = int(promotion.get("completed_count", 0))
    progress = 100.0 if candidate_count == 0 and promotion.get("status") == "completed" else (
        100.0 * completed / candidate_count if candidate_count else 0.0
    )
    st.progress(progress / 100.0, text=f"Progression : {progress:.1f}%")
    diagnostic_table = pd.DataFrame(diagnostics)
    candidates_table = pd.DataFrame(candidates)
    if diagnostic_table.empty:
        st.caption("Aucun diagnostic de promotion disponible.")
        return
    if not candidates_table.empty:
        candidates_table = candidates_table.rename(
            columns={
                "set_name": "Combinaison",
                "status": "Statut promotion technique",
                "model_id": "Model ID",
                "created": "Créé",
                "error": "Erreur promotion",
            }
        )
        diagnostic_table = diagnostic_table.merge(
            candidates_table[
                [
                    "Combinaison",
                    "Statut promotion technique",
                    "Model ID",
                    "Créé",
                    "Erreur promotion",
                ]
            ],
            on="Combinaison",
            how="left",
        )
    st.dataframe(diagnostic_table, hide_index=True, width="stretch")


def _render_candidate_identity_stability(
    comparison: dict[str, object],
    validation_lookup: Mapping[tuple[str, str], Mapping[str, object]] | None = None,
    trace_lookup: Mapping[tuple[str, str], Mapping[str, object]] | None = None,
) -> None:
    st.subheader("Stabilité des candidats")
    st.caption(
        "Compare l\u2019identité des candidats entre la période de référence et la "
        "période décalée. Cette analyse est descriptive et ne modifie pas la "
        "décision de validation temporelle."
    )
    stability = comparison.get("candidate_identity_stability")
    if not isinstance(stability, dict):
        st.info("Analyse de stabilité des candidats indisponible pour ce run.")
        return

    counts = st.columns(5)
    for column, label, key in zip(
        counts,
        ("Référence", "Validation", "Communs", "Perdus", "Nouveaux"),
        (
            "reference_candidate_count",
            "validation_candidate_count",
            "common_candidate_count",
            "lost_candidate_count",
            "new_candidate_count",
        ),
        strict=True,
    ):
        column.metric(label, stability.get(key, 0))

    rates = st.columns(3)
    rates[0].metric(
        "Taux de survie",
        _format_metric(stability.get("candidate_survival_rate"), percent=True),
    )
    rates[1].metric(
        "Overlap validation",
        _format_metric(stability.get("validation_overlap_rate"), percent=True),
    )
    rates[2].metric(
        "Jaccard",
        _format_metric(stability.get("jaccard_index"), percent=True),
    )

    survival = stability.get("candidate_survival_rate")
    if survival == 0:
        st.info(
            "Le pipeline conserve son rendement global de candidats, mais aucun "
            "candidat individuel n\u2019est commun aux deux périodes."
        )
    elif survival is not None:
        st.info(
            f"{_format_metric(survival, percent=True)} des candidats de référence "
            "restent candidats dans la période décalée."
        )

    tables = candidate_identity_tables(stability, validation_lookup, trace_lookup)
    common_tab, lost_tab, new_tab = st.tabs(["Communs", "Perdus", "Nouveaux"])
    percent_columns = {
        name: st.column_config.NumberColumn(format="percent")
        for name in (
            "Précision réf.",
            "Précision val.",
            "AUC réf.",
            "AUC val.",
            "Rendement réf.",
            "Rendement val.",
            "Précision",
            "AUC",
            "Rendement",
        )
    }
    with common_tab:
        if tables["common"].empty:
            st.info("Aucun candidat commun entre les deux périodes.")
        else:
            st.dataframe(
                tables["common"], hide_index=True, width="stretch",
                column_config=percent_columns,
            )
    with lost_tab:
        if tables["lost"].empty:
            st.info("Aucun candidat perdu dans la période décalée.")
        else:
            st.dataframe(
                lost_candidate_display_table(tables["lost"]),
                hide_index=True,
                width="stretch",
                column_config={
                    "Critère(s) échoué(s)": st.column_config.TextColumn(
                        width="large"
                    )
                },
            )
    with new_tab:
        if tables["new"].empty:
            st.info("Aucun nouveau candidat dans la période décalée.")
        else:
            st.dataframe(
                tables["new"], hide_index=True, width="stretch",
                column_config=percent_columns,
            )


def _render_temporal_validation(
    service: ExperimentService, parent_run_id: str, detail: dict[str, object]
) -> None:
    stage = pipeline_stage_by_key(
        detail.get("pipeline_stages"), "temporal_validation_end_to_end"
    )
    if stage is None:
        st.info("Temporal validation was not enabled for this End-to-end.")
        return
    child_run_id = str(stage["child_run_id"])
    st.caption(
        f"Reference: {parent_run_id}, offset 0 | validation: {child_run_id}, offset 63"
    )
    root = st.session_state.lab_config.project_root / "runs"
    comparison = _read_light_json(
        root / parent_run_id / "results" / "temporal_validation_comparison.json"
    )
    if comparison is None:
        st.info("Quality comparison is pending until both chains complete.")
    else:
        st.subheader(f"Decision: {comparison.get('final_status', 'unknown')}")
        gates = comparison.get("gates", {})
        if isinstance(gates, dict):
            st.dataframe(
                temporal_validation_gate_table(gates),
                hide_index=True,
                width="stretch",
            )
        st.caption(f"Compared at: {comparison.get('executed_at', 'n/a')}")
        st.caption(
            "candidate_yield mesure la capacité du pipeline à continuer de produire "
            "des candidats; la stabilité d\u2019identité mesure combien des mêmes "
            "candidats persistent entre les périodes."
        )
        validation_lookup = {}
        validation_threshold_run_id = comparison.get("validation_threshold_run_id")
        if (
            isinstance(comparison.get("candidate_identity_stability"), dict)
            and isinstance(validation_threshold_run_id, str)
        ):
            validation_lookup = validation_promotion_lookup(
                root / validation_threshold_run_id / "results"
            )
        trace_lookup = (
            lost_candidate_trace_lookup(
                comparison["candidate_identity_stability"], root / child_run_id
            )
            if isinstance(comparison.get("candidate_identity_stability"), dict)
            else {}
        )
        _render_candidate_identity_stability(
            comparison, validation_lookup, trace_lookup
        )
        with st.expander("Comparison parameters and provenance"):
            st.json({
                "parameters": comparison.get("parameters", {}),
                "source_artifact_digests": comparison.get("source_artifact_digests", {}),
            })
        pipeline_summary = _read_light_json(
            root / parent_run_id / "results" / "pipeline_summary.json"
        ) or {}
        promotion = pipeline_summary.get("promotion", {})
        if isinstance(promotion, dict):
            status = "executed" if promotion.get("executed") else promotion.get("reason", "not requested")
            st.caption(f"Automatic promotion: {status}")
        forced_stage = pipeline_stage_by_key(
            detail.get("pipeline_stages"), "forced_candidate_validation_end_to_end"
        )
        st.subheader("Revalidation des candidats de référence")
        st.caption(
            "Réévalue les candidats trouvés dans le run de référence sur la période "
            "décalée, indépendamment du préfiltre."
        )
        if forced_stage is None:
            try:
                backfill = historical_forced_validation_state(
                    service.run_service.repository, parent_run_id
                )
            except (OSError, ValueError) as error:
                st.error(f"Revalidation historique indisponible : {error}")
                backfill = {"exists": False}
            if not backfill.get("exists"):
                st.caption(
                    "Réentraîne uniquement les candidats de référence sur l'offset 63, "
                    "avec leurs hyperparamètres et seuils figés."
                )
                if st.button(
                    "Lancer la revalidation des candidats de référence",
                    key=f"start-historical-forced-{parent_run_id}",
                ):
                    try:
                        service.start_historical_forced_validation(parent_run_id)
                    except ValueError as error:
                        st.error(f"Revalidation historique impossible : {error}")
                    else:
                        st.rerun()
            else:
                forced_id = str(backfill["child_run_id"])
                forced_stage = {"child_run_id": forced_id}
                status = str(backfill.get("status") or "unknown")
                st.caption(
                    f"Run : {forced_id} · statut : {status} · "
                    f"créé : {backfill.get('created_at') or 'n/a'}"
                )
                controls = st.columns(2)
                if controls[0].button(
                    "Open revalidation", key=f"open-historical-forced-{parent_run_id}"
                ):
                    st.session_state["selected-run-id"] = forced_id
                    st.rerun()
                if status in {"failed", "cancelled", "interrupted"} and controls[1].button(
                    "Resume", key=f"resume-historical-forced-{parent_run_id}"
                ):
                    try:
                        service.resume(forced_id)
                    except ValueError as error:
                        st.error(f"Reprise impossible : {error}")
                    else:
                        st.rerun()
        if forced_stage is not None:
            forced_id = str(forced_stage.get("child_run_id"))
            st.caption(f"Run : {forced_id}")
            forced_manifest = _read_light_json(
                root / forced_id / "orchestration" / "pipeline.json"
            ) or {}
            forced_threshold_id = next(
                (
                    str(item.get("child_run_id"))
                    for item in forced_manifest.get("stages", [])
                    if isinstance(item, dict)
                    and item.get("stage_key")
                    in {"fixed_candidate_evaluation", "threshold_calibration"}
                ),
                None,
            )
            forced_lookup = (
                validation_promotion_lookup(root / forced_threshold_id / "results")
                if forced_threshold_id else {}
            )
            stability = comparison.get("candidate_identity_stability", {})
            forced_trace = forced_candidate_trace_lookup(stability, root / forced_id)
            checkpoint = _read_light_json(
                root / parent_run_id / "orchestration" / "promotion.json"
            ) or {}
            promoted = {
                str(item.get("set_name"))
                for item in checkpoint.get("candidates", [])
                if isinstance(item, dict) and item.get("status") == "completed"
            }
            table = forced_candidate_revalidation_table(
                stability, forced_lookup, forced_trace, promoted
            )
            total = len(table)
            confirmed = int((table.get("Statut") == "Candidat confirmé").sum()) if total else 0
            non_evaluable = int((table.get("Statut") == "Non évaluable").sum()) if total else 0
            metrics = st.columns(6)
            for column, (label, value) in zip(metrics, (
                ("Candidats référence", total), ("Réévalués", total - non_evaluable),
                ("Confirmés", confirmed), ("Non confirmés", total - confirmed - non_evaluable),
                ("Non évaluables", non_evaluable), ("Promus", len(promoted)),
            )):
                column.metric(label, value)
            if table.empty:
                st.info("Aucun candidat de référence à réévaluer.")
            else:
                st.dataframe(table, hide_index=True, width="stretch")
            try:
                diagnostic = diagnostic_state(
                    service.run_service.repository, forced_id
                )
            except (OSError, ValueError) as error:
                st.caption(f"Diagnostic holdout indisponible : {error}")
                diagnostic = {"exists": False, "candidate_count": 0}
            if diagnostic.get("exists"):
                diagnostic_id = str(diagnostic["run_id"])
                st.caption(
                    f"Diagnostic holdout : {diagnostic_id} · "
                    f"statut : {diagnostic.get('status')} · "
                    f"candidats : {diagnostic.get('candidate_count', 0)} · "
                    f"favorables : {diagnostic.get('favorable_count', 0)}"
                )
                actions = st.columns(2)
                if actions[0].button(
                    "Ouvrir", key=f"open-wf-reject-diagnostic-{forced_id}"
                ):
                    st.session_state["selected-run-id"] = diagnostic_id
                    st.rerun()
                if diagnostic.get("status") in {"failed", "cancelled", "interrupted"}:
                    if actions[1].button(
                        "Reprendre", key=f"resume-wf-reject-diagnostic-{forced_id}"
                    ):
                        service.resume(diagnostic_id)
                        st.rerun()
            elif int(diagnostic.get("candidate_count", 0)) > 0:
                forced_status = service.run_service.repository.status(forced_id)
                if forced_status.get("status") == "completed" and st.button(
                    "Lancer le diagnostic holdout des rejets WF",
                    key=f"start-wf-reject-diagnostic-{forced_id}",
                ):
                    service.start_qualification_holdout_diagnostic(forced_id)
                    st.rerun()
    st.dataframe(
        pd.DataFrame(pipeline_stage_rows([stage])), hide_index=True, width="stretch"
    )
    if stage.get("status") == "reserved":
        return
    child_detail = service.run(child_run_id)
    child_metadata = child_detail.get("metadata", {})
    st.json(
        {
            "reference_run_id": child_metadata.get("reference_run_id"),
            "run_purpose": child_metadata.get("run_purpose"),
            "offset": child_detail["configuration"]["rstock_config"].get(
                "walk_forward_end_offset_sessions"
            ),
        }
    )
    _render_pipeline_summary(child_run_id, child_detail)
    if st.button("Open validation End-to-end", key=f"open-temporal-{parent_run_id}"):
        st.session_state["selected-run-id"] = child_run_id
        st.rerun()


def _read_light_json(path: Path) -> dict[str, object] | None:
    if not path.is_file():
        return None
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    return value if isinstance(value, dict) else None


def _render_pipeline_technical(run_id: str, detail: dict[str, object]) -> None:
    root = st.session_state.lab_config.project_root / "runs"
    run_root = root / run_id
    st.caption(f"Parent run_id : {run_id}")
    st.subheader("Métadonnées et provenance")
    st.json(detail.get("metadata", {}))
    pipeline = _read_light_json(run_root / "orchestration" / "pipeline.json")
    if pipeline is not None:
        with st.expander("Pipeline manifest"):
            st.json(pipeline)
    stages = detail.get("pipeline_stages")
    if isinstance(stages, list):
        for stage in stages:
            if not isinstance(stage, dict) or not stage.get("child_run_id"):
                continue
            child_id = str(stage["child_run_id"])
            with st.expander(f"{stage.get('stage_key')} - {child_id}"):
                st.json(
                    {
                        "child_run_id": child_id,
                        "expected_fingerprint": stage.get("expected_fingerprint"),
                        "artifact_digests": stage.get("artifact_digests"),
                    }
                )
                light_paths = (
                    root / child_id / "results" / "sampling_manifest.json",
                    root / child_id / "orchestration" / "walk_forward_batches.json",
                    root / child_id / "results" / "run_configuration.json",
                )
                for path in light_paths:
                    values = _read_light_json(path)
                    if values is not None:
                        st.caption(path.name)
                        st.json(values)
    st.subheader("Artifacts du parent")
    st.write(detail.get("files") or "Aucun résultat publié")


def _render_end_to_end_tabs(
    service: ExperimentService, run_id: str, detail: dict[str, object]
) -> None:
    child_renderers = {
        renderer_key: (
            lambda renderer_key=renderer_key: _render_pipeline_child(
                service, run_id, detail, renderer_key
            )
        )
        for renderer_key in PIPELINE_CHILD_TABS
    }
    render_lazy_tabs(
        st,
        tabs_for_job(JobType.END_TO_END),
        {
            "summary": lambda: _render_pipeline_summary(run_id, detail, service),
            **child_renderers,
            "promotion": lambda: _render_pipeline_promotion(detail),
            "temporal_validation": lambda: _render_temporal_validation(
                service, run_id, detail
            ),
            "technical": lambda: _render_pipeline_technical(run_id, detail),
        },
        key=f"run-detail-{run_id}",
    )


def _render_forced_candidate_validation_tabs(
    service: ExperimentService, run_id: str, detail: dict[str, object]
) -> None:
    child_renderers = {
        renderer_key: (
            lambda renderer_key=renderer_key: _render_pipeline_child(
                service, run_id, detail, renderer_key
            )
        )
        for renderer_key in (
            "child_walk_forward",
            "child_fixed_candidate_evaluation",
        )
    }
    render_lazy_tabs(
        st,
        tabs_for_job(JobType.FORCED_CANDIDATE_VALIDATION),
        {
            "summary": lambda: _render_pipeline_summary(run_id, detail),
            **child_renderers,
            "technical": lambda: _render_pipeline_technical(run_id, detail),
        },
        key=f"run-detail-{run_id}",
    )


def _render_qualification_holdout_diagnostic(
    run_id: str, detail: dict[str, object]
) -> None:
    """Render one autonomous diagnostic page from artifacts loaded once."""

    root = st.session_state.lab_config.project_root / "runs" / run_id / "results"
    path = root / "diagnostic_results.csv"
    if not path.is_file():
        st.info("Les résultats diagnostiques ne sont pas encore disponibles.")
        return
    results = pd.read_csv(path)
    summary = detail.get("summary", {})
    directional = pd.to_numeric(
        results.get("Rendement directionnel", pd.Series(dtype=float)), errors="coerce"
    )
    metrics = st.columns(6)
    values = (
        ("Rejets analysés", int(summary.get("candidate_count", len(results)))),
        ("Holdouts calculés", int(summary.get("holdout_count", 0))),
        ("Non évaluables", int(summary.get("non_evaluable_count", 0))),
        ("AUC ≥ 0,50", int((pd.to_numeric(results.get("AUC holdout diagnostic"), errors="coerce") >= 0.50).sum())),
        ("Critères satisfaits", int(summary.get("criteria_satisfied_count", 0))),
        ("Rendement médian", "—" if directional.dropna().empty else f"{directional.median():.2%}"),
    )
    for column, (label, value) in zip(metrics, values):
        column.metric(label, value)

    def render_results() -> None:
        st.dataframe(results.drop(columns=["AUC WF par fenetre"], errors="ignore"), hide_index=True, width="stretch")

    def render_sensitivity() -> None:
        work = results.copy()
        worst = pd.to_numeric(work["Worst AUC WF"], errors="coerce")
        work["Bande"] = pd.cut(
            worst,
            [-float("inf"), 0.40, 0.42, 0.43, 0.44, 0.45],
            labels=["< 0.40", "0.40–0.42", "0.42–0.43", "0.43–0.44", "0.44–0.45"],
            right=False,
        )
        work["Favorable"] = work["Statut diagnostique"].eq("Holdout favorable")
        table = work.groupby("Bande", observed=False).agg(
            Candidats=("Set", "size"),
            **{
                "AUC holdout médiane": ("AUC holdout diagnostic", "median"),
                "Précision médiane": ("Precision", "median"),
                "Rendement médian": ("Rendement directionnel", "median"),
                "% holdouts favorables": ("Favorable", "mean"),
            },
        ).reset_index()
        st.dataframe(table, hide_index=True, width="stretch")

    def render_candidate() -> None:
        if results.empty:
            st.info("Aucun candidat diagnostiqué.")
            return
        choice = st.selectbox("Candidat", results["Set"].astype(str).tolist(), key=f"diagnostic-candidate-{run_id}")
        row = results[results["Set"].astype(str) == choice].iloc[0]
        st.json({key: value for key, value in row.to_dict().items() if key != "AUC WF par fenetre"})
        try:
            st.caption("AUC Walk-forward par fenêtre")
            st.write(json.loads(str(row.get("AUC WF par fenetre", "[]"))))
        except json.JSONDecodeError:
            st.write("—")
        direction = str(row.get("Direction", ""))
        st.caption("Hyperparamètres XGBoost figés")
        st.json(detail["configuration"].get("frozen_xgboost_parameters", {}).get(direction, {}))

    render_lazy_tabs(
        st,
        tabs_for_job(JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC),
        {
            "diagnostic_results": render_results,
            "worst_auc_sensitivity": render_sensitivity,
            "diagnostic_candidate": render_candidate,
        },
        key=f"run-detail-{run_id}",
    )


def _render_job_detail_tabs(
    service: ExperimentService,
    run_id: str,
    *,
    status: dict[str, object],
    detail: dict[str, object],
) -> None:
    if detail.get("storage", {}).get("state") == "purged":
        st.info(
            "Résumé seulement : les données lourdes de ce run ont été purgées. "
            "Les détails intermédiaires supprimés ne sont plus disponibles."
        )
    try:
        job_type = JobType(str(status["job_type"]))
    except ValueError:
        job_type = JobType.XGBOOST_CALIBRATION
    if job_type is JobType.END_TO_END:
        _render_end_to_end_tabs(service, run_id, detail)
    elif job_type is JobType.FORCED_CANDIDATE_VALIDATION:
        _render_forced_candidate_validation_tabs(service, run_id, detail)
    elif job_type is JobType.QUALIFICATION_HOLDOUT_DIAGNOSTIC:
        _render_qualification_holdout_diagnostic(run_id, detail)
    elif job_type is JobType.WALK_FORWARD:
        _render_walk_forward_tabs(service, run_id, status, detail)
    else:
        _render_standard_job_tabs(run_id, job_type, status, detail)


def _render_run_detail_view(
    service: ExperimentService, run_id: str,
) -> None:
    detail = service.run(run_id)
    status = detail["status"]
    _page_header("Historique")
    st.caption("Historique > Détail du run")
    if st.button("<- Retour a Historique", key="history-back-detail"):
        _clear_history_navigation()
    history = history_row(status, detail, {})
    if status["job_type"] == JobType.WALK_FORWARD.value:
        configuration = detail.get("configuration", {})
        universe_summary = run_universe_summary(configuration)
        targets = configuration.get("target_symbols") or configuration.get("symbols") or []
        rstock_config = configuration.get("rstock_config", {})
        depth = rstock_config.get("permutation_depth", "-") if isinstance(rstock_config, dict) else "-"
        st.subheader(f"Walk-forward — {len(targets)} symboles — profondeur {depth}")
        context_universes = ", ".join(universe_summary["context_universe_ids"]) or "Aucun"
        st.caption(
            f"{universe_summary['primary_universe_id']} · "
            f"{universe_summary['target_count']} cibles · {context_universes} · "
            f"{universe_summary['predictor_count']} prédicteurs · "
            f"{history.date_time} · {history.duration} · {status.get('status', '-')} · "
            f"ID technique : {run_id}"
        )
    elif status["job_type"] == JobType.END_TO_END.value:
        st.subheader("Pipeline End-to-end")
        st.caption(
            f"{history.date_time} - {history.duration} - {status.get('status', '-')} - "
            f"ID technique : {run_id}"
        )
    elif status["job_type"] == JobType.FORCED_CANDIDATE_VALIDATION.value:
        st.subheader("Revalidation forcée des candidats")
        st.caption(
            f"{history.date_time} - {history.duration} - {status.get('status', '-')} - "
            f"ID technique : {run_id}"
        )
    else:
        st.subheader(history.summary if history.summary != "-" else "Detail du run")
        st.caption(f"ID technique : {run_id}")
    _render_resume_controls(run_id, status, detail)
    _render_job_detail_tabs(service, run_id, status=status, detail=detail)
    return
def _render_run_comparison_view(service: ExperimentService, run_ids: list[str]) -> None:
    details = [service.run(run_id) for run_id in run_ids]
    analytics = [_load_run_analytics(run_id, item["status"], item) for run_id, item in zip(run_ids, details, strict=True)]
    labels = {
        item.run_id: history_row(detail["status"], detail, {}).date_time
        for item, detail in zip(analytics, details, strict=True)
    }
    if len(set(labels.values())) != len(labels):
        labels = {key: f"{value} · {key[-4:]}" for key, value in labels.items()}
    _page_header("Historique")
    st.caption("Historique > Comparaison de runs")
    if st.button("← Retour à Historique", key="history-back-comparison"):
        _clear_history_navigation()
    st.subheader("Comparaison de runs — Walk-forward")
    cards = st.columns(len(analytics))
    for index, (column, analysis, detail) in enumerate(
        zip(cards, analytics, details, strict=True), start=1
    ):
        history = history_row(detail["status"], detail, {})
        column.caption(
            f"Run {chr(64 + index)} · {history.date_time} · "
            f"{len(analysis.symbols)} symboles · profondeur {analysis.depth} · "
            f"{analysis.status}"
        )
    summary = comparison_table(analytics, labels)
    tabs = st.tabs(["Synthèse", "Métriques", "Combinaisons", "Validation", "Technique"])
    with tabs[0]:
        best_holdout = max(
            (item for item in analytics if item.holdout_auc_median is not None),
            key=lambda item: item.holdout_auc_median, default=None,
        )
        fastest = min(
            (item for item in analytics if item.duration_seconds is not None),
            key=lambda item: item.duration_seconds, default=None,
        )
        stable_delta = min(
            (item for item in analytics if item.delta_median is not None),
            key=lambda item: abs(item.delta_median), default=None,
        )
        kpis = st.columns(4)
        kpis[0].metric("Meilleur AUC holdout médian", _format_metric(None if best_holdout is None else best_holdout.holdout_auc_median))
        kpis[1].metric("Combinaisons qualifiées max", max(item.qualified_count for item in analytics))
        kpis[2].metric("Run le plus rapide", "—" if fastest is None else history_row(next(detail["status"] for detail in details if detail["status"]["run_id"] == fastest.run_id), next(detail for detail in details if detail["status"]["run_id"] == fastest.run_id), {}).duration)
        kpis[3].metric("Delta dev→holdout le plus stable", _format_metric(None if stable_delta is None else stable_delta.delta_median))
        st.dataframe(comparison_display_table(summary), hide_index=True, width="stretch")
    with tabs[1]:
        quality, durations = comparison_chart_frames(analytics, labels)
        st.subheader("Qualité prédictive")
        quality_long = quality.melt(
            id_vars=["Run", "Date / heure"],
            value_vars=["AUC dev médiane", "AUC holdout médiane"],
            var_name="Métrique", value_name="AUC",
        ).dropna(subset=["AUC"])
        if quality_long.empty:
            st.caption("Aucune AUC publiée pour les runs sélectionnés.")
        else:
            quality_chart = alt.Chart(quality_long).mark_bar().encode(
                x=alt.X("Run:N", title=None),
                xOffset="Métrique:N",
                y=alt.Y("AUC:Q", title="AUC", scale=alt.Scale(domain=[0, 1])),
                color=alt.Color("Métrique:N", title=None),
                tooltip=["Run:N", "Métrique:N", alt.Tooltip("AUC:Q", format=".3f"), "Date / heure:N"],
            )
            st.altair_chart(quality_chart, width="stretch")
        st.dataframe(
            quality[["Run", "Delta dev→holdout", "Date / heure"]],
            hide_index=True, width="stretch",
            column_config={"Delta dev→holdout": st.column_config.NumberColumn(format="%.3f")},
        )
        st.subheader("Durée d’exécution")
        duration_chart_data = durations.dropna(subset=["Durée (s)"])
        if duration_chart_data.empty:
            st.caption("Aucune durée publiée pour les runs sélectionnés.")
        else:
            duration_chart = alt.Chart(duration_chart_data).mark_bar().encode(
                y=alt.Y("Run:N", title=None, sort="-x"),
                x=alt.X("Durée (s):Q", title="Durée (secondes)"),
                tooltip=["Run:N", "Durée:N", "Date / heure:N"],
            )
            st.altair_chart(duration_chart, width="stretch")
    with tabs[2]:
        for analysis in analytics:
            st.subheader(labels[analysis.run_id])
            top = analysis.combinations[analysis.combinations["Eligible"]].head(10)
            if top.empty:
                st.caption("Aucune combinaison qualifiée disponible.")
            else:
                st.dataframe(top.drop(columns=["Eligible", "Holdout confirmé"]), hide_index=True, width="stretch")
    with tabs[3]:
        validation = summary[summary["Indicateur"].isin(["% qualifiées", "% confirmées", "Delta dev→holdout", "Combinaisons qualifiées", "Confirmées holdout"])]
        st.dataframe(validation, hide_index=True, width="stretch")
    with tabs[4]:
        differences = configuration_differences(
            [detail["configuration"] for detail in details],
            [labels[analysis.run_id] for analysis in analytics],
        )
        if differences.empty:
            st.caption("Aucun paramètre expérimental suivi ne diffère entre ces runs.")
        else:
            st.subheader("Paramètres différents")
            st.dataframe(differences, hide_index=True, width="stretch")
        for detail, analysis in zip(details, analytics, strict=True):
            with st.expander(labels[analysis.run_id]):
                st.caption(f"ID technique : {analysis.run_id}")
                st.json(detail["configuration"])


def _history_purge_preview(
    service: ExperimentService, run_id: str
) -> int | None:
    """Validate purge eligibility only after the user explicitly requests it."""

    eligibility = service.purge_eligibility(run_id)
    if not eligibility.eligible:
        st.info(eligibility.reason or "Purge impossible pour ce run.")
        return None
    try:
        plan = service.purge_preview(run_id)
    except (OSError, ValueError, RuntimeError) as error:
        st.error(f"Purge impossible : {error}")
        return None
    return int(plan.reclaimable_bytes)


def _history_runs_panel(
    service: ExperimentService,
    *,
    allowed_types: frozenset[str],
    key_prefix: str,
) -> None:
    history_runs = service.history_runs(job_types=allowed_types)
    runs = [record.status for record in history_runs]
    details_by_run_id = {
        str(record.status["run_id"]): record.detail() for record in history_runs
    }
    models = _history_model_contexts(st.session_state.lab_config.project_root)
    filtered = _history_filters(
        runs,
        allowed_types=allowed_types,
        models=models,
        details_by_run_id=details_by_run_id,
        key_prefix=key_prefix,
    )
    if not filtered:
        st.session_state[f"{key_prefix}-selected-runs"] = []
        st.info("Aucun run ne correspond aux filtres.")
        return
    page_controls = st.columns([1, 1, 4])
    page_size = page_controls[0].selectbox("Runs par page", [25, 50], key=f"{key_prefix}-page-size")
    total_pages = max(1, (len(filtered) + int(page_size) - 1) // int(page_size))
    page_key = f"{key_prefix}-page"
    if int(st.session_state.get(page_key, 1)) > total_pages:
        st.session_state[page_key] = total_pages
    page = page_controls[1].number_input(
        "Page", min_value=1, max_value=total_pages, value=1, step=1, key=page_key
    )
    visible, total_pages = paginate_runs(filtered, page=int(page) - 1, page_size=int(page_size))
    page_controls[2].caption(f"{len(filtered)} runs · page {int(page)} / {total_pages}")
    rows = [
        history_row(run, details_by_run_id[str(run["run_id"])], models)
        for run in visible
    ]
    selection = st.dataframe(
        pd.DataFrame([row.display() for row in rows]),
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="multi-row",
        key=f"{key_prefix}-grid",
    )
    selected_rows = _selected_rows(selection, len(rows))
    selected_key = f"{key_prefix}-selected-runs"
    st.session_state[selected_key] = [rows[index].run_id for index in selected_rows]
    selected = st.session_state.get(selected_key, [])
    if isinstance(selected, str):
        selected = [selected]
    filtered_ids = {str(run["run_id"]) for run in filtered}
    selected = [run_id for run_id in selected if run_id in filtered_ids]
    st.session_state[selected_key] = selected
    if not selected:
        st.caption("Sélectionnez un run pour l’ouvrir, ou de 2 à 4 runs pour les comparer.")
        return
    action = selected_run_action(selected)
    if action == "detail":
        selected_run_id = selected[0]
        selected_run = next(
            run for run in filtered if str(run["run_id"]) == selected_run_id
        )
        actions = st.columns([1.2, 2.4, 2.6, 4])
        if actions[0].button(
            "Ouvrir le run",
            type="primary",
            key=f"open-history-{key_prefix}",
        ):
            _history_navigation("detail", selected)
        if str(selected_run["job_type"]) in {
            job_type.value for job_type in DUPLICATION_JOB_TYPES
        } and actions[1].button(
            "Dupliquer l’expérience",
            type="primary",
            key=f"duplicate-history-{key_prefix}",
        ):
            _start_walk_forward_duplication(selected_run_id, service.run(selected_run_id))
        selected_storage = details_by_run_id[selected_run_id].get("storage", {})
        can_request_purge = selected_storage.get("state") != "purged"
        if can_request_purge and actions[2].button(
            "Purger les données lourdes",
            key=f"purge-history-{key_prefix}",
        ):
            reclaimable_bytes = _history_purge_preview(service, selected_run_id)
            if reclaimable_bytes is not None:
                st.session_state["pending-run-purge"] = {
                    "run_id": selected_run_id,
                    "reclaimable_bytes": reclaimable_bytes,
                }
                st.rerun()
        pending_purge = st.session_state.get("pending-run-purge")
        if (
            isinstance(pending_purge, dict)
            and pending_purge.get("run_id") == selected_run_id
        ):
            _render_run_purge_confirmation(
                service,
                selected_run_id,
                int(pending_purge.get("reclaimable_bytes", 0)),
            )
        return
    if action == "comparison":
        selected_types = {
            str(next(run for run in filtered if str(run["run_id"]) == run_id)["job_type"])
            for run_id in selected
        }
        if selected_types != {JobType.WALK_FORWARD.value}:
            st.caption("La comparaison analytique est disponible pour des runs walk-forward uniquement.")
        elif st.button("Comparer les runs", type="primary", key=f"compare-history-{key_prefix}"):
            _history_navigation("comparison", selected)
        return
    st.warning("Sélectionnez au maximum 4 runs pour une comparaison.")


def _format_storage_size(size_bytes: int) -> str:
    value = float(max(0, size_bytes))
    units = ("o", "Ko", "Mo", "Go", "To")
    for unit in units:
        if value < 1024.0 or unit == units[-1]:
            return f"{value:.0f} {unit}" if unit in {"o", "Ko"} else f"{value:.1f} {unit}"
        value /= 1024.0
    return f"{value:.1f} To"


def _render_run_purge_confirmation(
    service: ExperimentService, run_id: str, reclaimable_bytes: int
) -> None:
    with st.container(border=True):
        st.warning(
            f"Cette opération libérera environ "
            f"{_format_storage_size(reclaimable_bytes)}.\n\n"
            "L’expérience restera dans l’Historique avec sa configuration, "
            "sa provenance et ses résultats synthétiques.\n\n"
            "Les détails intermédiaires supprimés ne seront plus disponibles."
        )
        confirm, cancel, _ = st.columns([1.8, 1, 6])
        if confirm.button(
            "Purger les données lourdes",
            type="primary",
            key=f"confirm-run-purge-{run_id}",
        ):
            try:
                storage = service.purge(run_id)
            except (OSError, ValueError, RuntimeError) as error:
                st.error(f"Purge impossible : {error}")
            else:
                st.session_state.pop("pending-run-purge", None)
                st.success(
                    "Purge terminée : "
                    f"{_format_storage_size(int(storage.get('reclaimed_bytes', 0)))} "
                    "libérés."
                )
                st.rerun()
        if cancel.button("Annuler", key=f"cancel-run-purge-{run_id}"):
            st.session_state.pop("pending-run-purge", None)
            st.rerun()


def _experiments_page() -> None:
    _experiments(_service())


def _settings_page() -> None:
    _settings()


def _create_universe_panel(service: UniverseService) -> None:
    if st.button("+ Créer un univers", key="show-create-universe"):
        st.session_state.show_create_universe = not st.session_state.get(
            "show_create_universe", False
        )
    if not st.session_state.get("show_create_universe", False):
        return
    with st.container(border=True):
        st.subheader("Créer un univers")
        mode = st.radio(
            "Mode de création", ["Création manuelle", "Import CSV"],
            horizontal=True, key="create-universe-mode",
        )
        name = st.text_input("Nom de l’univers", key="create-universe-name")
        type_label = st.radio(
            "Type d’univers", ["Standard", "Contexte"], horizontal=True,
            key="create-universe-type",
        )
        universe_type = (
            STANDARD_UNIVERSE_TYPE if type_label == "Standard" else CONTEXT_UNIVERSE_TYPE
        )
        benchmark_symbol = None
        if universe_type == STANDARD_UNIVERSE_TYPE:
            benchmark_choice = st.selectbox(
                "Benchmark de marché", _market_benchmark_options(service),
                key="create-universe-benchmark",
            )
            benchmark_symbol = None if benchmark_choice == "Aucun" else benchmark_choice
        else:
            st.caption("Le benchmark est une propriété des univers principaux.")
        if mode == "Création manuelle":
            symbols = st.text_area(
                "Symboles",
                placeholder="AAPL, MSFT, NVDA\nou un symbole par ligne",
                key="create-universe-symbols",
            )
            if st.button("Enregistrer", type="primary", key="save-manual-universe"):
                try:
                    created = service.create(
                        name, symbols, universe_type=universe_type,
                        benchmark_symbol=benchmark_symbol,
                    )
                except ValueError as error:
                    st.error(str(error))
                else:
                    st.session_state.selected_universe_id = created.universe_id
                    st.session_state.show_create_universe = False
                    st.success(f"Univers créé : {created.name}")
                    st.rerun()
        else:
            uploaded = st.file_uploader("Importer un CSV", type=["csv"], key="universe-csv")
            selected_column = "symbol"
            if uploaded is not None:
                try:
                    imported = pd.read_csv(uploaded)
                except Exception as error:
                    st.error(f"CSV illisible : {error}")
                    imported = None
                if imported is not None and len(imported.columns):
                    columns = [str(column) for column in imported.columns]
                    default = next(
                        (index for index, column in enumerate(columns) if column.casefold() == "symbol"),
                        0,
                    )
                    selected_column = st.selectbox(
                        "Colonne des symboles", columns, index=default,
                        key="universe-csv-column",
                    )
                    st.caption(f"{len(imported)} lignes détectées. Aucune donnée marché ne sera récupérée.")
            if st.button(
                "Enregistrer", type="primary", key="save-csv-universe",
                disabled=uploaded is None,
            ):
                try:
                    created = service.create_from_csv(
                        name, uploaded.getvalue(), column=selected_column,
                        universe_type=universe_type,
                        benchmark_symbol=benchmark_symbol,
                    )
                except ValueError as error:
                    st.error(str(error))
                else:
                    st.session_state.selected_universe_id = created.universe_id
                    st.session_state.show_create_universe = False
                    st.success(f"Univers importé : {created.name}")
                    st.rerun()


def _universe_selection_preview(service: UniverseService, universe_id: str) -> None:
    record = service.record(universe_id)
    st.subheader("Aperçu de sélection")
    if record.type == CONTEXT_UNIVERSE_TYPE:
        st.caption("Univers de contexte · utilisé au complet · ne peut pas fournir de cibles")
        st.write(", ".join(record.symbols[:8]) + (", …" if len(record.symbols) > 8 else ""))
        return
    mode = st.radio(
        "Mode", ["Univers complet", "Top N", "Échantillon reproductible"],
        horizontal=True, key=f"universe-preview-mode-{universe_id}",
    )
    if mode == "Univers complet":
        selection = UniverseSelection(source=SAVED_SOURCE, universe=universe_id)
    else:
        size = int(st.number_input(
            "Nombre de symboles", min_value=1, max_value=len(record.symbols),
            value=min(50, len(record.symbols)), key=f"universe-preview-size-{universe_id}",
        ))
        seed = None
        method = TOP_N
        if mode == "Échantillon reproductible":
            method = SEEDED_SAMPLE
            seed = int(st.number_input(
                "Seed", min_value=0, value=1234,
                key=f"universe-preview-seed-{universe_id}",
            ))
        selection = UniverseSelection(
            source=SAMPLE_SOURCE, universe=universe_id, sample_size=size,
            selection_method=method, seed=seed,
        )
    resolved = service.resolve(selection)
    st.caption(
        f"Univers : {record.name} · Mode : {mode} · "
        f"{len(resolved.symbols)} symboles résolus"
    )
    st.write(", ".join(resolved.symbols[:8]) + (", …" if len(resolved.symbols) > 8 else ""))
    with st.expander("Voir tous les symboles résolus"):
        st.code(", ".join(resolved.symbols))


def _universe_detail(service: UniverseService, universe_id: str) -> None:
    record = service.record(universe_id)
    st.subheader(record.name)
    type_label = "Standard" if record.type == STANDARD_UNIVERSE_TYPE else "Contexte"
    st.caption(
        f"{len(record.symbols)} symboles · Type : {type_label} · Source : {record.source}"
    )
    st.write(", ".join(record.symbols[:8]) + (", …" if len(record.symbols) > 8 else ""))
    benchmark_label = record.benchmark_symbol or "Aucun"
    st.caption(f"Benchmark de marché : {benchmark_label}")
    with st.expander("Voir tous les symboles"):
        st.code(", ".join(record.symbols))

    if record.system:
        st.info("Univers système protégé : vous pouvez le dupliquer, mais pas le modifier ni le supprimer.")
    else:
        with st.expander("Modifier l’univers"):
            name = st.text_input(
                "Nom", value=record.name, key=f"edit-universe-name-{universe_id}"
            )
            symbols = st.text_area(
                "Liste complète des symboles",
                value="\n".join(record.symbols),
                key=f"edit-universe-symbols-{universe_id}",
            )
            edited_type_label = st.radio(
                "Type d’univers",
                ["Standard", "Contexte"],
                index=0 if record.type == STANDARD_UNIVERSE_TYPE else 1,
                horizontal=True,
                key=f"edit-universe-type-{universe_id}",
            )
            edited_type = (
                STANDARD_UNIVERSE_TYPE
                if edited_type_label == "Standard"
                else CONTEXT_UNIVERSE_TYPE
            )
            benchmark_symbol = None
            if edited_type == STANDARD_UNIVERSE_TYPE:
                benchmark_options = _market_benchmark_options(
                    service, current=record.benchmark_symbol
                )
                benchmark_choice = st.selectbox(
                    "Benchmark de marché",
                    benchmark_options,
                    index=benchmark_options.index(record.benchmark_symbol or "Aucun"),
                    key=f"edit-universe-benchmark-{universe_id}",
                )
                benchmark_symbol = (
                    None if benchmark_choice == "Aucun" else benchmark_choice
                )
            else:
                st.caption("Le benchmark est une propriété des univers principaux.")
            st.caption("Vous pouvez ajouter, retirer ou remplacer les symboles avant d’enregistrer.")
            if st.button("Enregistrer les modifications", key=f"update-universe-{universe_id}"):
                try:
                    service.update(
                        universe_id,
                        name=name,
                        symbols=symbols,
                        universe_type=edited_type,
                        benchmark_symbol=benchmark_symbol,
                    )
                except ValueError as error:
                    st.error(str(error))
                else:
                    st.success("Univers mis à jour. Les anciens runs conservent leur liste figée.")
                    st.rerun()

    action_columns = st.columns(2)
    if action_columns[0].button("Dupliquer", key=f"duplicate-universe-{universe_id}"):
        copied = service.duplicate(universe_id)
        st.session_state.selected_universe_id = copied.universe_id
        st.success(f"Copie créée : {copied.name}")
        st.rerun()
    if not record.system:
        current = st.session_state.lab_universe_selection
        if current.universe == universe_id:
            st.warning("Cet univers est actuellement sélectionné dans le brouillon d’expérience.")
        confirmation_key = "pending-delete-universe"
        if action_columns[1].button("Supprimer", key=f"delete-universe-{universe_id}"):
            st.session_state[confirmation_key] = universe_id
            st.rerun()
        if st.session_state.get(confirmation_key) == universe_id:
            st.warning(f"Confirmer la suppression de l’univers « {record.name} » ?")
            confirmation_columns = st.columns(2)
            if confirmation_columns[0].button(
                "Annuler", key=f"cancel-delete-universe-{universe_id}"
            ):
                st.session_state.pop(confirmation_key, None)
                st.rerun()
            if confirmation_columns[1].button(
                "Confirmer la suppression",
                type="primary",
                key=f"confirm-delete-universe-{universe_id}",
            ):
                was_current = current.universe == universe_id
                service.delete(universe_id)
                if was_current:
                    fallback = service.record(service.standard_universe_names()[0])
                    st.session_state.lab_universe_selection = UniverseSelection(
                        source=SAVED_SOURCE, universe=fallback.universe_id
                    )
                    st.session_state.lab_symbols = list(fallback.symbols)
                st.session_state.pop(confirmation_key, None)
                st.session_state.pop("selected_universe_id", None)
                st.session_state.lab_context_universe_ids = [
                    item
                    for item in st.session_state.lab_context_universe_ids
                    if item != universe_id
                ]
                st.success("Univers supprimé. Aucun run historique n’a été modifié.")
                st.rerun()
    _universe_selection_preview(service, universe_id)


def _universes_page() -> None:
    _page_header("Univers")
    st.caption("Gérez les listes de symboles utilisées par vos expériences.")
    service = _universe_service()
    st.subheader("Univers sauvegardés")
    records = service.records()
    table = pd.DataFrame([
        {
            "Nom": record.name,
            "Nombre de symboles": len(record.symbols),
            "Type": "Standard" if record.type == STANDARD_UNIVERSE_TYPE else "Contexte",
            "Benchmark": record.benchmark_symbol or "—",
            "Source": record.source,
            "Dernière modification": (
                "—" if record.updated_at is None
                else pd.to_datetime(record.updated_at).strftime("%Y-%m-%d")
            ),
        }
        for record in records
    ])
    event = st.dataframe(
        table, hide_index=True, width="stretch",
        on_select="rerun", selection_mode="single-row", key="saved-universes-grid",
    )
    selected = _selected_rows(event, len(records))
    if selected:
        st.session_state.selected_universe_id = records[selected[0]].universe_id
    else:
        st.session_state.pop("selected_universe_id", None)
    _create_universe_panel(service)
    selected_id = st.session_state.get("selected_universe_id")
    available = {record.universe_id for record in records}
    if selected_id in available:
        st.divider()
        _universe_detail(service, str(selected_id))
    else:
        st.caption("Sélectionnez un univers dans la grille pour afficher ses détails.")


def _submit_operational_job(
    job_type: JobType, *, model_id: str | None = None
) -> None:
    project_root = st.session_state.lab_config.project_root
    models = ModelService(project_root)
    if model_id:
        symbols = models.repository.get(model_id).symbols
    else:
        symbols = models.operational_universe().symbols
    if len(symbols) < 2:
        st.error("Au moins deux symboles opérationnels sont nécessaires.")
        return
    spec = ExperimentSpec(
        job_type=job_type,
        config=st.session_state.lab_config,
        symbols=symbols,
        calendar=st.session_state.lab_calendar,
        model_id=model_id,
    )
    submission = _service().submit(spec)
    message = "Job créé" if submission.created else "Job identique déjà actif"
    st.success(f"{message} : {submission.run_id}")


def _selected_rows(event: object, row_count: int | None = None) -> list[int]:
    """Return current selection positions, dropping stale grid positions."""

    selected = list(getattr(getattr(event, "selection", None), "rows", []))
    if row_count is None:
        return selected
    return [
        index
        for index in selected
        if isinstance(index, int) and not isinstance(index, bool)
        and 0 <= index < row_count
    ]


def _invalidate_surveillance_selection_state() -> None:
    """Drop row selections whose model population changed lifecycle status."""

    for key in (
        "surveillance-predictions",
        "surveillance-signals",
        "surveillance-no-signals",
        "surveillance-evaluated-predictions",
    ):
        st.session_state.pop(key, None)


def _technical_record(view: OperationalTableView, selected: list[int]) -> dict[str, object] | None:
    if not selected or not 0 <= selected[0] < len(view.technical):
        return None
    return json.loads(view.technical.iloc[selected[0]].to_json(date_format="iso"))


def _render_prediction_audit_details(
    record: dict[str, object],
    *,
    technical_title: str = "Détails techniques",
    technical_payload: object | None = None,
) -> None:
    lagged_features, other_features = prediction_feature_tables(record)
    st.markdown("**Entrées du modèle au moment de la prédiction**")
    if lagged_features.empty and other_features.empty:
        st.caption("Non disponible pour cette prédiction historique.")
    if not lagged_features.empty:
        st.dataframe(lagged_features, hide_index=True, width="stretch")
    if not other_features.empty:
        if not lagged_features.empty:
            st.caption("Autres features")
        st.dataframe(other_features, hide_index=True, width="content")
    observations, other_observations = source_observation_tables(record)
    st.markdown("**Observations sources**")
    if observations.empty and other_observations.empty:
        st.caption("Non disponible pour cette prédiction historique.")
    if not observations.empty:
        st.dataframe(observations, hide_index=True, width="stretch")
    if not other_observations.empty:
        if not observations.empty:
            st.caption("Autres observations")
        st.dataframe(other_observations, hide_index=True, width="stretch")
    with st.expander(technical_title, expanded=False):
        st.json(record if technical_payload is None else technical_payload)


def _surveillance_styles() -> None:
    """Install page-scoped polish without changing the rest of the application."""

    st.markdown(
        """
        <div class="rstock-surveillance-scope"></div>
        <style>
          div[data-testid="stMainBlockContainer"]:has(.rstock-surveillance-scope) {
            max-width: 1520px;
            padding-top: 2rem;
            padding-bottom: 2rem;
          }
          div[data-testid="stMainBlockContainer"]:has(.rstock-surveillance-scope)
          div[data-testid="stMetric"] {
            padding: 0.1rem 0;
          }
          div[data-testid="stMainBlockContainer"]:has(.rstock-surveillance-scope)
          div[data-testid="stButton"] button {
            min-height: 2.45rem;
            border-radius: 0.55rem;
          }
          div[data-testid="stMainBlockContainer"]:has(.rstock-surveillance-scope)
          div[data-testid="stButton"] button[kind="primary"] {
            background: linear-gradient(135deg, #2563eb 0%, #0ea5e9 100%);
            border-color: #2563eb;
            color: #ffffff;
          }
          .rstock-equal-height-marker { display: none; }
          div[data-testid="stHorizontalBlock"]:has(.rstock-signals-card-marker):has(.rstock-daily-update-card-marker) {
            align-items: stretch;
          }
          div[data-testid="stHorizontalBlock"]:has(.rstock-signals-card-marker):has(.rstock-daily-update-card-marker)
          > div[data-testid="stColumn"] {
            align-self: stretch;
            display: flex;
            flex-direction: column;
          }
          div[data-testid="stHorizontalBlock"]:has(.rstock-signals-card-marker):has(.rstock-daily-update-card-marker)
          > div[data-testid="stColumn"]
          > div[data-testid="stVerticalBlock"] {
            flex: 1 1 auto;
            height: 100%;
          }
          div[data-testid="stLayoutWrapper"]:has(.rstock-signals-card-marker),
          div[data-testid="stLayoutWrapper"]:has(.rstock-daily-update-card-marker) {
            flex: 1 1 auto;
            height: 100%;
          }
          div[data-testid="stLayoutWrapper"]:has(.rstock-signals-card-marker)
          > div[data-testid="stVerticalBlock"],
          div[data-testid="stLayoutWrapper"]:has(.rstock-daily-update-card-marker)
          > div[data-testid="stVerticalBlock"] {
            height: 100%;
          }
          .rstock-surveillance-subtitle {
            color: #64748b;
            font-size: 0.98rem;
            margin: -0.15rem 0 0.75rem;
          }
          .rstock-header-right-spacer { height: 0.8rem; }
          .rstock-status-pill, .rstock-error-pill, .rstock-new-pill {
            display: inline-block;
            border-radius: 999px;
            font-size: 0.73rem;
            font-weight: 700;
            padding: 0.18rem 0.55rem;
            margin-top: 0.4rem;
          }
          .rstock-status-pill { background: #dcfce7; color: #15803d; margin-right: 0.35rem; }
          .rstock-error-pill { background: #fee2e2; color: #b91c1c; margin-left: 0.35rem; }
          .rstock-priority-card {
            border: 1px solid #e2e6ec;
            border-radius: 8px;
            padding: 10px 12px;
            margin-bottom: 8px;
            background: #ffffff;
          }
          .rstock-priority-card.rstock-priority-first {
            border-color: #8ab4f8;
            background: #fafcff;
          }
          .rstock-priority-header {
            display: flex;
            align-items: center;
            gap: 8px;
            white-space: nowrap;
          }
          .rstock-priority-rank {
            min-width: 28px;
            color: #4b5563;
            font-weight: 700;
          }
          .rstock-priority-first .rstock-priority-rank {
            color: #2563eb;
          }
          .rstock-priority-symbol {
            overflow: hidden;
            color: #111827;
            font-size: 16px;
            font-weight: 700;
            text-overflow: ellipsis;
          }
          .rstock-priority-score {
            margin-left: auto;
            padding: 2px 7px;
            border-radius: 10px;
            background: #e7f6ea;
            color: #28783b;
            font-size: 12px;
            font-weight: 600;
          }
          .rstock-priority-probability {
            margin-left: 6px;
            color: #374151;
            font-size: 13px;
            font-weight: 600;
          }
          .rstock-priority-edge {
            margin-left: 36px;
            margin-top: 2px;
            color: #16803a;
            font-size: 12px;
            font-weight: 600;
          }
          .rstock-priority-metrics {
            margin-left: 36px;
            margin-top: 3px;
            color: #667085;
            font-size: 11px;
            line-height: 1.25;
          }
          .rstock-priority-see-all {
            margin-top: 4px;
            padding: 6px 8px;
            border-radius: 7px;
            background: #eff6ff;
            text-align: center;
            font-size: 12px;
            font-weight: 600;
          }
          .rstock-priority-see-all a {
            color: #2563eb;
            text-decoration: none;
          }
        </style>
        """,
        unsafe_allow_html=True,
    )


def _freshness_state(freshness: dict[str, str | None]) -> tuple[str, str]:
    if freshness and all(value for value in freshness.values()):
        return "À jour", "rstock-status-pill"
    return "À vérifier", "rstock-error-pill"


def _render_surveillance_header(
    *,
    last_market: object,
    freshness: dict[str, str | None],
    error_count: int,
) -> None:
    left, right = st.columns([2.7, 1], gap="large")
    with left:
        _page_header("Surveillance")
        st.markdown(
            '<p class="rstock-surveillance-subtitle">'
            "Détectez les opportunités, suivez les signaux et passez à l’action."
            "</p>",
            unsafe_allow_html=True,
        )
    state_label, state_class = _freshness_state(freshness)
    error_class = "rstock-status-pill" if error_count == 0 else "rstock-error-pill"
    with right:
        st.markdown('<div class="rstock-header-right-spacer"></div>', unsafe_allow_html=True)
        st.caption("Dernière mise à jour")
        st.markdown(f"**{_compact_datetime(last_market)}**")
        st.markdown(
            f'<span class="{state_class}">{html.escape(state_label)}</span>'
            f'<span class="{error_class}">'
            f"{error_count} erreur{'s' if error_count != 1 else ''}</span>",
            unsafe_allow_html=True,
        )


def _render_surveillance_kpis(
    *,
    today_signals: int,
    pending_predictions: int,
    active_models: int,
    monitored_symbols: int,
    last_market: object,
    freshness: dict[str, str | None],
) -> None:
    state_label, _ = _freshness_state(freshness)
    columns = st.columns(4, gap="small")
    cards = (
        (
            "Signaux haussiers aujourd’hui",
            today_signals,
            "À surveiller maintenant",
        ),
        (
            "Prédictions en attente",
            pending_predictions,
            "Validation à venir",
        ),
        (
            "Modèles actifs",
            active_models,
            f"{monitored_symbols} symboles surveillés",
        ),
        (
            "Dernière mise à jour",
            _compact_datetime(last_market),
            state_label,
        ),
    )
    for column, (label, value, caption) in zip(columns, cards, strict=True):
        with column:
            with st.container(border=True):
                st.metric(label, value)
                st.caption(caption)


def _styled_signal_table(table: pd.DataFrame) -> pd.io.formats.style.Styler:
    return (
        table.style
        .map(lambda _: "font-weight: 750; color: #0f172a", subset=["Cible"])
        .map(lambda _: "font-weight: 700; color: #059669", subset=["P(Up)"])
        .map(lambda _: "font-weight: 700; color: #dc2626", subset=["P(Down)"])
        .map(lambda _: "color: #475569", subset=["Date"])
    )


def _render_signals_card(
    view: SignalResultsView,
    *,
    stretch: bool = False,
) -> tuple[SignalResultsView, dict[str, object] | None]:
    selected_signal = None
    with st.container(border=True, height="stretch" if stretch else "content"):
        if stretch:
            st.markdown(
                '<span class="rstock-equal-height-marker rstock-signals-card-marker"></span>',
                unsafe_allow_html=True,
            )
        st.subheader("Signaux haussiers à traiter")
        st.caption("Opportunités détectées par les modèles actifs.")
        displayed = filter_signal_results_view(view, "Aujourd’hui et demain")
        if displayed.signals.table.empty:
            empty_message = "Aucun signal haussier aujourd’hui ou demain."
            st.info(empty_message)
        else:
            event = st.dataframe(
                _styled_signal_table(displayed.signals.table),
                hide_index=True,
                width="stretch",
                height=max(78, min(420, 36 * len(displayed.signals.table) + 42)),
                on_select="rerun",
                selection_mode="single-row",
                key="surveillance-signals",
                column_config={
                    "Date": st.column_config.TextColumn(width="small"),
                    "Cible": st.column_config.TextColumn(width="small"),
                    "Predictors": st.column_config.TextColumn(width="medium"),
                    "P(Up)": st.column_config.TextColumn(width="small"),
                    "P(Down)": st.column_config.TextColumn(width="small"),
                    "Catégorie": st.column_config.TextColumn(width="medium"),
                },
            )
            selected_signal = _technical_record(
                displayed.signals, _selected_rows(event, len(displayed.signals.technical))
            )

    return displayed, selected_signal


def _render_signals_followup(
    displayed: SignalResultsView,
    selected_signal: dict[str, object] | None,
    models: ModelService,
    *,
    active_models: Sequence[ProductionModel] | None = None,
) -> None:
    no_signal_selection: list[int] = []
    with st.expander(
        f"Autres prédictions sans signal ({len(displayed.no_signal.table)})",
        expanded=False,
    ):
        if displayed.no_signal.table.empty:
            st.caption("Aucune prédiction sans signal.")
        else:
            event = st.dataframe(
                displayed.no_signal.table,
                hide_index=True,
                width="stretch",
                height=max(78, min(420, 36 * len(displayed.no_signal.table) + 42)),
                on_select="rerun",
                selection_mode="single-row",
                key="surveillance-no-signals",
            )
            no_signal_selection = _selected_rows(
                event, len(displayed.no_signal.technical)
            )

    selected_no_signal = _technical_record(
        displayed.no_signal, no_signal_selection
    )
    selected = selected_signal or selected_no_signal
    if selected is not None:
        st.markdown("**Détail du signal**")
        available_models = (
            models.active_models() if active_models is None else active_models
        )
        source_model = next(
            (
                model
                for model in available_models
                if model.model_id == selected.get("model_id")
            ),
            None,
        )
        _render_prediction_audit_details(
            selected,
            technical_title="Pourquoi ce signal ?",
            technical_payload={
                "signal": selected,
                "modèle_source": None if source_model is None else source_model.to_dict(),
            },
        )


def _render_signals_section(
    view: SignalResultsView,
    models: ModelService,
    *,
    stretch: bool = False,
) -> None:
    displayed, selected_signal = _render_signals_card(view, stretch=stretch)
    _render_signals_followup(displayed, selected_signal, models)


def _priority_card_html(
    rank: int,
    row: pd.Series,
) -> str:
    def percentage(value: object, *, digits: int = 0, signed: bool = False) -> str:
        numeric = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
        if pd.isna(numeric):
            return "—"
        sign = "+" if signed else ""
        return f"{float(numeric) * 100:{sign}.{digits}f}".replace(".", ",") + " %"

    edge = percentage(row.get("signal_edge"), digits=1, signed=True)
    edge_text = "Marge vs seuil —" if edge == "—" else f"{edge[:-2]} pts vs seuil"
    target = html.escape(str(row.get("target", "—")))
    score = int(row.get("priority_score", 0))
    probability = percentage(row.get("up_probability"))
    precision = percentage(row.get("holdout_precision"))
    directional_return = percentage(
        row.get("holdout_directional_return"), digits=1, signed=True
    )
    opposite = percentage(row.get("opposite_move_frequency"))
    first_class = " rstock-priority-first" if rank == 1 else ""
    return f"""
    <div class="rstock-priority-card{first_class}">
      <div class="rstock-priority-header">
        <span class="rstock-priority-rank">#{rank}</span>
        <span class="rstock-priority-symbol">{target}</span>
        <span class="rstock-priority-score">Score {score}</span>
        <span class="rstock-priority-probability">P(Up) {probability}</span>
      </div>
      <div class="rstock-priority-edge">{edge_text}</div>
      <div class="rstock-priority-metrics">Précision {precision} · Rend. {directional_return} · Opposé {opposite}</div>
    </div>
    """


def _render_priorities_panel(
    signals: OperationalTableView,
    model_metrics_by_id: Mapping[str, Mapping[str, object]],
) -> None:
    priorities = prioritize_signals_view(
        signals,
        model_metrics_by_id=model_metrics_by_id,
        limit=3,
    )
    with st.container(border=True):
        st.subheader(
            "Priorités du jour",
            help=(
                "Le classement combine la force du signal par rapport à son seuil "
                "calibré et les performances hors échantillon du modèle. Il sert "
                "à prioriser les signaux et ne modifie pas leur statut scientifique."
            ),
        )
        st.caption("Signaux classés par pertinence opérationnelle.")
        if priorities.table.empty:
            st.info("Aucune priorité pour le moment.")
            return
        for index, row in priorities.technical.iterrows():
            st.markdown(
                _priority_card_html(index + 1, row),
                unsafe_allow_html=True,
            )
        st.markdown(
            '<div class="rstock-priority-see-all">'
            '<a href="#signaux-haussiers-a-traiter">↗ Voir tous les signaux</a>'
            "</div>",
            unsafe_allow_html=True,
        )


def _render_operational_info(
    *,
    freshness: dict[str, str | None],
    last_prediction: object,
    errors: list[dict[str, object]],
) -> None:
    state_label, state_class = _freshness_state(freshness)
    with st.container(border=True):
        st.subheader("État opérationnel")
        st.markdown(
            f'<span class="{state_class}">{html.escape(state_label)}</span>',
            unsafe_allow_html=True,
        )
        st.caption(
            f"{len(freshness)} symboles surveillés · "
            f"Dernière prédiction : {_compact_datetime(last_prediction)}"
        )
        with st.expander("Informations techniques", expanded=False):
            if freshness:
                st.caption(
                    "Fraîcheur des données : "
                    + ", ".join(
                        f"{symbol}={last_date or 'manquant'}"
                        for symbol, last_date in freshness.items()
                    )
                )
            else:
                st.caption("Aucune donnée de fraîcheur disponible.")
            if errors:
                st.markdown("**Erreurs opérationnelles récentes**")
                st.json(errors[:10])
            else:
                st.caption("Aucune erreur opérationnelle récente.")


_DAILY_UPDATE_STAGES = (
    ("market_update", "Mise à jour du marché"),
    ("daily_prediction", "Prédictions quotidiennes"),
    ("screening", "Détection des signaux"),
    ("realized_validation", "Évaluation des prédictions"),
)


def _render_daily_update_card(
    runs: list[dict[str, object]], *, active_model_count: int, stretch: bool = False
) -> None:
    """Submit and follow the existing full operational workflow in one place."""

    current = next(
        (
            run for run in runs
            if run.get("job_type") == JobType.OPERATIONAL_RUN.value
            and run.get("status") in {"pending", "running", "failed"}
        ),
        None,
    )
    latest = next(
        (run for run in runs if run.get("job_type") == JobType.OPERATIONAL_RUN.value),
        None,
    )
    with st.container(border=True, height="stretch" if stretch else "content"):
        if stretch:
            st.markdown(
                '<span class="rstock-equal-height-marker rstock-daily-update-card-marker"></span>',
                unsafe_allow_html=True,
            )
        st.subheader("Mise à jour quotidienne")
        if current is not None:
            detail = _service().run(str(current["run_id"]))
            progress = detail["progress"]
            stage = str(progress.get("stage") or "market_update")
            stage_index, stage_label = next(
                (
                    (index, label)
                    for index, (name, label) in enumerate(_DAILY_UPDATE_STAGES, start=1)
                    if name == stage
                ),
                (1, "Mise à jour du marché"),
            )
            st.caption(f"Étape {stage_index}/4 — {stage_label}")
            workflow_percent = progress.get("workflow_percent")
            if workflow_percent is not None:
                st.progress(float(workflow_percent) / 100.0)
            if current.get("status") == "failed":
                st.error(str(current.get("error") or "La mise à jour a échoué."))
            elif current.get("status") == "running":
                st.info("Mise à jour quotidienne en cours…")
            else:
                st.info("Mise à jour quotidienne en attente…")
        elif latest is not None and latest.get("status") == "completed":
            st.success("Mise à jour quotidienne terminée.")
        if st.button(
            "Mettre à jour RStock",
            type="primary",
            width="stretch",
            disabled=active_model_count == 0 or (
                current is not None and current.get("status") in {"pending", "running"}
            ),
            key="daily-operational-update",
        ):
            _submit_operational_job(JobType.OPERATIONAL_RUN)
            st.rerun()
        st.caption("Lance la mise à jour quotidienne de bout en bout.")


def _load_evaluated_predictions_view(
    predictions: pd.DataFrame,
    signals: pd.DataFrame,
    *,
    project_root: Path,
) -> EvaluatedPredictionsView:
    signal_service = SignalService(project_root)
    realized = signal_service.active_realized_results()
    model_service = ModelService(project_root)
    history_targets = (
        set(predictions["target"].dropna().astype(str))
        if not predictions.empty and "target" in predictions
        else set()
    )
    freshness_symbols = tuple(
        sorted(set(model_service.operational_universe().symbols) | history_targets)
    )
    freshness = MarketDataService().freshness(
        freshness_symbols,
        st.session_state.lab_config,
    )
    return build_evaluated_predictions_view(
        predictions, signals, realized, freshness
    )


def _render_real_trade_from_prediction(
    record: dict[str, object], *, project_root: Path
) -> None:
    """Render the explicit, inline real-trade action for one evaluated signal."""

    category = str(record.get("category") or record.get("signal_status") or "")
    if category != "bullish_signal":
        st.caption("Une transaction réelle peut être enregistrée uniquement pour un signal haussier.")
        return
    prediction_id = str(record.get("prediction_id") or "")
    if not prediction_id:
        st.warning("Cette prédiction ne possède pas d’identifiant stable exploitable.")
        return
    trades = RealTradeService(project_root)
    existing = trades.for_prediction(prediction_id)
    action_label = (
        "Modifier la transaction réelle" if existing is not None
        else "Enregistrer une transaction réelle"
    )
    state_key = "real-trade-prediction-editor"
    if st.button(action_label, key=f"real-trade-action-{prediction_id}"):
        st.session_state[state_key] = prediction_id
    if st.session_state.get(state_key) != prediction_id:
        return
    st.markdown("#### Transaction réelle")
    st.caption(
        f"{record.get('prediction_date', '—')} · {record.get('target', '—')} · "
        f"modèle {record.get('model_id', '—')} · signal Up"
    )
    with st.form(f"real-trade-form-{prediction_id}"):
        columns = st.columns(3)
        entry = columns[0].number_input(
            "Prix d’achat", min_value=0.01,
            value=float(existing.entry_price) if existing else 1.0,
            step=0.01,
            format="%.2f",
        )
        exit_price = columns[1].number_input(
            "Prix de vente", min_value=0.01,
            value=float(existing.exit_price) if existing else 1.0,
            step=0.01,
            format="%.2f",
        )
        quantity = columns[2].number_input(
            "Quantité", min_value=1,
            value=int(existing.quantity) if existing else 1,
            step=1,
        )
        note = st.text_area("Note facultative", value=existing.note or "" if existing else "")
        saved = st.form_submit_button("Enregistrer la transaction", type="primary")
        if saved:
            try:
                trades.save_from_prediction(
                    record,
                    entry_price=entry,
                    exit_price=exit_price,
                    quantity=quantity,
                    note=note,
                )
            except ValueError as error:
                st.error(str(error))
            else:
                st.session_state.pop(state_key, None)
                st.success("Transaction réelle enregistrée.")
                st.rerun()


def _evaluated_predictions_panel(
    view: EvaluatedPredictionsView,
    runs: list[dict[str, object]],
    *,
    project_root: Path,
) -> None:
    """Render evaluated predictions, pending predictions and evaluation feedback."""

    validation_jobs = [
        run
        for run in runs
        if run["job_type"]
        in {JobType.REALIZED_VALIDATION.value, JobType.OPERATIONAL_RUN.value}
    ]
    selected_record = None
    with st.expander(
        f"Prédictions évaluées récemment ({len(view.table)})",
        expanded=False,
    ):
        prediction_word = "prédiction" if view.pending_count == 1 else "prédictions"
        st.caption(
            f"{view.pending_count} {prediction_word} en attente"
            f" · Prochaine validation : {view.next_validation_date or '—'}"
            f" · Données jusqu’au : {view.latest_market_date or '—'}"
        )
        if validation_jobs:
            latest = validation_jobs[0]
            if latest["status"] in {"pending", "running"}:
                st.info("Validation des résultats en cours…")
            elif latest["status"] == "failed":
                st.error(
                    "La dernière validation a échoué : "
                    f"{latest.get('error') or 'erreur inconnue'}"
                )
            elif latest["status"] == "completed":
                summary = _service().run(str(latest["run_id"]))["summary"]
                new_results = int(summary.get("realized_results", 0))
                if new_results:
                    level, message = evaluation_feedback(new_results, view)
                    getattr(st, level)(message)
        status_filter = st.selectbox(
            "Afficher",
            ["Toutes", "Signaux seulement", "Sans signal"],
            index=1,
            key="surveillance-evaluated-predictions-filter",
        )
        displayed_view = filter_evaluated_predictions_view(view, status_filter)
        main_table = evaluated_predictions_main_table(displayed_view.table)
        event = st.dataframe(
            main_table, hide_index=True, width="stretch",
            on_select="rerun", selection_mode="single-row", key="surveillance-evaluated-predictions",
        )
        selected = _selected_rows(event, len(displayed_view.technical))
        if selected:
            selected_record = json.loads(
                displayed_view.technical.iloc[selected[0]].to_json(date_format="iso")
            )
        if selected_record is not None:
            _render_real_trade_from_prediction(selected_record, project_root=project_root)
        if not displayed_view.pending.empty:
            st.markdown("**Prédictions en attente**")
            st.dataframe(
                build_predictions_view(
                    displayed_view.pending, limit=len(displayed_view.pending)
                ).table,
                hide_index=True,
                width="stretch",
            )
    if selected_record is not None:
        _render_prediction_audit_details(selected_record)


def _render_surveillance_page(*, polling: bool) -> None:
    _surveillance_styles()
    project_root = st.session_state.lab_config.project_root
    models = ModelService(project_root)
    active_models = models.active_models()
    priority_model_metrics = signal_priority_model_lookup(active_models)
    universe = models.operational_universe(active_models)
    predictions = PredictionService(project_root).active_history()
    signal_service = SignalService(project_root)
    signals = signal_service.active_history()
    freshness = MarketDataService().freshness(universe.symbols, st.session_state.lab_config)
    runs = _service().runs()
    signal_view = build_signals_view(signals, predictions)
    today_signal_view = filter_signal_results_view(signal_view, "Aujourd’hui et demain")
    evaluated_view = _load_evaluated_predictions_view(
        predictions,
        signals,
        project_root=project_root,
    )
    last_market = next(
        (run.get("finished_at") or run.get("created_at") for run in runs if run["job_type"] in {JobType.MARKET_UPDATE.value, JobType.OPERATIONAL_RUN.value} and run["status"] == "completed"),
        None,
    )
    last_prediction = (
        predictions["created_at"].max()
        if not predictions.empty and "created_at" in predictions
        else None
    )
    errors = [
        run
        for run in runs
        if run["status"] == "failed" and run["job_type"] in OPERATIONAL_JOB_TYPES
    ]
    _render_surveillance_header(
        last_market=last_market,
        freshness=freshness,
        error_count=len(errors),
    )
    _render_surveillance_kpis(
        today_signals=len(today_signal_view.signals.table),
        pending_predictions=evaluated_view.pending_count,
        active_models=len(universe.model_ids),
        monitored_symbols=len(universe.symbols),
        last_market=last_market,
        freshness=freshness,
    )
    top_main, top_sidebar = st.columns([2.25, 1], gap="large")
    with top_main:
        displayed_signals, selected_signal = _render_signals_card(
            signal_view,
            stretch=True,
        )
    with top_sidebar:
        _render_daily_update_card(
            runs, active_model_count=len(universe.model_ids), stretch=True
        )
    main, sidebar = st.columns([2.25, 1], gap="large")
    with main:
        _render_signals_followup(
            displayed_signals,
            selected_signal,
            models,
            active_models=active_models,
        )
        _evaluated_predictions_panel(evaluated_view, runs, project_root=project_root)
        with st.expander("Univers opérationnel"):
            st.write(", ".join(universe.symbols) or "Aucun symbole")
            if universe.used_by:
                st.dataframe(
                    [{"Symbole": symbol, "Modèles": ", ".join(ids)} for symbol, ids in universe.used_by.items()],
                    hide_index=True, width="stretch",
                )
    with sidebar:
        priority_view = filter_signal_results_view(signal_view, "Aujourd’hui et demain")
        _render_priorities_panel(priority_view.signals, priority_model_metrics)
        _render_operational_info(
            freshness=freshness,
            last_prediction=last_prediction,
            errors=errors,
        )
    _live_job_panel(_service(), domain="production")
    if surveillance_refresh_decision(runs, polling=polling).final_rerun:
        st.rerun(scope="app")


if hasattr(st, "fragment"):
    _polling_surveillance_page = st.fragment(run_every=2)(_render_surveillance_page)


def _surveillance_page() -> None:
    runs = _service().runs()
    decision = surveillance_refresh_decision(runs, polling=False)
    if decision.poll and hasattr(st, "fragment"):
        _polling_surveillance_page(polling=True)
    else:
        _render_surveillance_page(polling=False)


def _render_real_trade_editor(trades: RealTradeService, trade) -> None:
    with st.form(f"returns-edit-{trade.trade_id}"):
        columns = st.columns(3)
        entry = columns[0].number_input("Prix d’achat", min_value=0.01, value=float(trade.entry_price), step=0.01, format="%.2f")
        exit_price = columns[1].number_input("Prix de vente", min_value=0.01, value=float(trade.exit_price), step=0.01, format="%.2f")
        quantity = columns[2].number_input("Quantité", min_value=1, value=int(trade.quantity), step=1)
        note = st.text_area("Note facultative", value=trade.note or "")
        if st.form_submit_button("Enregistrer les modifications", type="primary"):
            trades.update(
                trade.trade_id,
                entry_price=entry,
                exit_price=exit_price,
                quantity=quantity,
                note=note,
            )
            st.session_state.pop("returns-edit-trade", None)
            st.success("Transaction réelle mise à jour.")
            st.rerun()


def _returns_page() -> None:
    _page_header("Rendement")
    st.caption("Analyse des transactions réelles saisies manuellement. Cette page ne modifie pas les modèles.")
    project_root = st.session_state.lab_config.project_root
    trades = RealTradeService(project_root)
    realized = SignalService(project_root).realized_results()
    table = performance_table(trades.trades(), realized)
    if table.empty:
        st.info("Aucune transaction réelle enregistrée.")
        return
    dates = pd.to_datetime(table["date"], errors="coerce").dropna()
    filters = st.columns(4)
    start = filters[0].date_input("Date de début", value=dates.min().date())
    end = filters[1].date_input("Date de fin", value=dates.max().date())
    targets = ["Toutes", *sorted(table["target"].dropna().astype(str).unique())]
    target = filters[2].selectbox("Cible", targets)
    models = ["Tous", *sorted(table["model_id"].dropna().astype(str).unique())]
    model_id = filters[3].selectbox("Modèle / combinaison", models)
    filtered = filter_performance(table, start=start, end=end, target=target, model_id=model_id)
    metrics = performance_kpis(filtered)
    labels = (
        ("Transactions", metrics["transaction_count"], None),
        ("P&L total", metrics["total_pnl"], ".2f"),
        ("Rendement moyen", metrics["mean_return"], ".2%"),
        ("Rendement médian", metrics["median_return"], ".2%"),
        ("Taux gagnant", metrics["win_rate"], ".2%"),
        ("Gain moyen", metrics["average_gain"], ".2f"),
        ("Perte moyenne", metrics["average_loss"], ".2f"),
        ("Profit factor", metrics["profit_factor"], ".2f"),
    )
    for row in (labels[:4], labels[4:]):
        for column, (label, value, fmt) in zip(st.columns(4), row, strict=True):
            rendered = "Indisponible" if value is None else (
                str(value) if fmt is None else format(float(value), fmt)
            )
            column.metric(label, rendered)
    display = filtered.rename(columns={
        "date": "Date", "target": "Cible", "model_id": "Modèle / combinaison",
        "entry_price": "Prix d’achat", "exit_price": "Prix de vente", "quantity": "Quantité",
        "real_return": "Rendement réel", "gross_pnl": "P&L",
        "theoretical_return": "Rendement théorique", "real_vs_theoretical": "Écart réel vs théorique",
        "note": "Note",
    })
    visible = [
        "Date", "Cible", "Modèle / combinaison", "Prix d’achat", "Prix de vente", "Quantité",
        "Rendement réel", "P&L", "Rendement théorique", "Écart réel vs théorique", "Note",
    ]
    event = st.dataframe(display.loc[:, visible], hide_index=True, width="stretch", on_select="rerun", selection_mode="single-row", key="real-trades-grid")
    selected = _selected_rows(event, len(filtered))
    if not selected:
        return
    selected_trade_id = str(filtered.iloc[selected[0]]["transaction_id"])
    selected_trade = next(item for item in trades.trades() if item.trade_id == selected_trade_id)
    actions = st.columns(2)
    if actions[0].button("Modifier", key=f"edit-real-trade-{selected_trade_id}"):
        st.session_state["returns-edit-trade"] = selected_trade_id
    if actions[1].button("Supprimer / Annuler", key=f"delete-real-trade-{selected_trade_id}"):
        st.session_state["returns-delete-trade"] = selected_trade_id
    if st.session_state.get("returns-edit-trade") == selected_trade_id:
        _render_real_trade_editor(trades, selected_trade)
    if st.session_state.get("returns-delete-trade") == selected_trade_id:
        st.warning("Confirmer la suppression de cette transaction réelle ? La prédiction source sera conservée.")
        confirmation = st.columns(2)
        if confirmation[0].button("Confirmer la suppression", type="primary", key=f"confirm-delete-real-trade-{selected_trade_id}"):
            trades.delete(selected_trade_id)
            st.session_state.pop("returns-delete-trade", None)
            st.success("Transaction réelle supprimée.")
            st.rerun()
        if confirmation[1].button("Annuler", key=f"cancel-delete-real-trade-{selected_trade_id}"):
            st.session_state.pop("returns-delete-trade", None)
            st.rerun()


def _models_page() -> None:
    _page_header("Modèles")
    service = ModelService(st.session_state.lab_config.project_root)
    models = service.models()
    if not models:
        st.info("Aucun candidat production.")
        return
    status_options = model_filter_options(models)
    target_options = sorted({str(model.target) for model in models})
    filter_columns = st.columns([1.4, 1.2, 1.8])
    selected_statuses = filter_columns[0].multiselect(
        "Statut",
        status_options,
        default=[status for status in status_options if status in DEFAULT_MODEL_STATUSES],
        key="models-status-filter",
    )
    selected_targets = filter_columns[1].multiselect(
        "Cible",
        target_options,
        key="models-target-filter",
    )
    predictor_query = filter_columns[2].text_input(
        "Pr\u00e9dicteurs",
        placeholder="Rechercher un pr\u00e9dicteur",
        key="models-predictor-filter",
    )
    visible_models = filter_models(
        models,
        statuses=selected_statuses,
        targets=selected_targets,
        predictor_query=predictor_query,
    )
    st.caption(f"{len(visible_models)} mod\u00e8les affich\u00e9s sur {len(models)}")
    selected_key = "selected-model-id"
    if not visible_models:
        st.info("Aucun mod\u00e8le ne correspond aux filtres.")
        _live_job_panel(_service(), domain="model")
        return
    table = pd.DataFrame([
        {
            "Cible": model.target,
            "Predictors": ", ".join(model.predictors),
            "Statut": model.status.value,
            "Créé": model.created_at,
            "Walk-forward": model.source_walk_forward_run,
            "Version": model.artifact_version,
            "AUC dev médiane": model.development_metrics.get("ROCAUCMedian"),
            "AUC holdout": model.holdout_metrics.get("FinalUpROCAUC"),
            "Taux de réussite": _format_metric(
                model.calibration_metrics.get("success_rate"), percent=True
            ),
        }
        for model in visible_models
    ])
    event = st.dataframe(
        table,
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="single-row",
        key="models-grid",
    )
    selected_rows = _selected_rows(event, len(visible_models))
    if selected_rows:
        st.session_state[selected_key] = visible_models[selected_rows[0]].model_id
    else:
        st.session_state.pop(selected_key, None)
    available_ids = {model.model_id for model in visible_models}
    selected_id = st.session_state.get(selected_key)
    if selected_id not in available_ids:
        st.session_state.pop(selected_key, None)
        st.caption("Sélectionnez un modèle dans la grille pour afficher les actions.")
        _live_job_panel(_service(), domain="model")
        return
    selected = next(model for model in visible_models if model.model_id == selected_id)
    st.markdown(f"**{selected.target} ← {' + '.join(selected.predictors)}**")
    st.caption(selected.model_id)
    if selected.calibrated_signal_threshold is not None:
        metrics = selected.calibration_metrics
        holdout = selected.holdout_signal_metrics
        st.markdown("**Calibration du signal haussier**")
        calibration_columns = st.columns(4)
        calibration_columns[0].metric("Seuil calibré", f"{selected.calibrated_signal_threshold:.3f}")
        calibration_columns[1].metric("Signaux", metrics.get("total_signals", "—"))
        calibration_columns[2].metric(
            "Taux de réussite",
            _format_metric(metrics.get("success_rate"), percent=True),
        )
        calibration_columns[3].metric(
            "Rendement moyen",
            _format_metric(metrics.get("mean_return"), percent=True),
        )
        st.caption(
            "MFE : " + _format_metric(metrics.get("mfe_mean"), percent=True)
            + " · MAE : " + _format_metric(metrics.get("mae_mean"), percent=True)
            + " · Stabilité : " + _format_metric(metrics.get("return_stability"), percent=True)
            + " · Holdout : " + _format_metric(
                holdout.get("IntradayReturnMean"), percent=True
            )
        )
    else:
        st.caption("Seuil de signal : seuil global de décision (aucune calibration associée).")
    controls = st.columns(5)
    if controls[0].button(
        "Entraîner", disabled=selected.status.value in {"active", "retired"}
    ):
        _submit_operational_job(JobType.PRODUCTION_TRAINING, model_id=selected_id)
    if controls[1].button("Activer", disabled=selected.status.value not in {"trained", "inactive"}):
        service.activate(selected_id)
        _invalidate_surveillance_selection_state()
        st.rerun()
    if controls[2].button("Désactiver", disabled=selected.status.value != "active"):
        service.deactivate(selected_id)
        _invalidate_surveillance_selection_state()
        st.rerun()
    if controls[3].button("Retirer", disabled=selected.status.value == "active"):
        service.retire(selected_id)
        _invalidate_surveillance_selection_state()
        st.rerun()
    with st.expander("Voir détails"):
        st.json(selected.to_dict())
    _live_job_panel(_service(), domain="model")


def _history_page() -> None:
    navigation = st.session_state.get("history-navigation")
    if isinstance(navigation, dict):
        run_ids = [str(run_id) for run_id in navigation.get("run_ids", [])]
        mode = navigation.get("mode")
        service = _service()
        available = {str(run["run_id"]) for run in service.runs()}
        if mode == "detail" and len(run_ids) == 1 and run_ids[0] in available:
            _render_run_detail_view(service, run_ids[0])
            return
        if mode == "comparison" and 2 <= len(run_ids) <= 4 and set(run_ids) <= available:
            _render_run_comparison_view(service, run_ids)
            return
        st.session_state.pop("history-navigation", None)
    _page_header("Historique")
    tabs = st.tabs(
        ["Backtest / walk-forward", "Holdout", "Production réelle"],
        key="history-tabs",
        on_change="rerun",
    )
    if tabs[0].open:
        _history_runs_panel(
            _service(), allowed_types=EXPERIMENT_JOB_TYPES, key_prefix="experimental-history"
        )
    if tabs[1].open:
        st.caption("Les métriques holdout restent attachées aux runs expérimentaux et aux modèles promus.")
        models = ModelService(st.session_state.lab_config.project_root).models()
        st.dataframe(
            [{"model_id": model.model_id, **model.holdout_metrics} for model in models],
            hide_index=True, width="stretch",
        )
    if tabs[2].open:
        _history_runs_panel(
            _service(), allowed_types=PRODUCTION_JOB_TYPES, key_prefix="production-history"
        )
        st.divider()
        signal_service = SignalService(st.session_state.lab_config.project_root)
        predictions = PredictionService(
            st.session_state.lab_config.project_root
        ).history()
        signals = signal_service.history()
        results = signal_service.realized_results()
        st.caption("Résultats opérationnels réels — jamais fusionnés avec le walk-forward ou le holdout.")
        production_columns = st.columns(3)
        production_columns[0].metric("Prédictions", len(predictions))
        production_columns[1].metric(
            "Signaux réels",
            int((signals.get("category") == "bullish_signal").sum())
            if not signals.empty
            else 0,
        )
        production_columns[2].metric("Résultats arrivés à échéance", len(results))
        if results.empty:
            st.info("Échantillon de production insuffisant ou aucun résultat réalisé.")
        else:
            summary = results.groupby("model_id").agg(
                prédictions_réalisées=("result_id", "count"),
                taux_succes=("up_target", "mean"),
                retour_moyen=("intraday_return", "mean"), mfe_moyenne=("mfe", "mean"),
                mae_moyenne=("mae", "mean"), fortes_baisses=("down_target", "mean"),
            ).reset_index()
            st.dataframe(summary, hide_index=True, width="stretch")
            if (summary["prédictions_réalisées"] < 30).any():
                st.warning(
                    "Au moins un modèle compte moins de 30 prédictions réalisées; "
                    "ces statistiques restent descriptives."
                )
            distribution = pd.cut(
                results["intraday_return"], bins=10, duplicates="drop"
            ).value_counts(sort=False)
            st.bar_chart(
                altair_serializable_distribution(distribution).rename(
                    "Nombre de prédictions"
                )
            )
            st.dataframe(results.tail(100), hide_index=True, width="stretch")
        with st.expander("Historique des prédictions de production"):
            st.dataframe(predictions.tail(200), hide_index=True, width="stretch")
        with st.expander("Historique des signaux de production"):
            st.dataframe(signals.tail(200), hide_index=True, width="stretch")


def _simulation_currency(value: float) -> str:
    return f"{value:+,.2f} $".replace(",", " ")


def _simulation_percent(value: float | None) -> str:
    return "—" if value is None else f"{value:.2%}"


def _render_simulation_results(result: SimulationResult) -> None:
    selected_symbols = st.session_state.get(
        "simulation-trades-symbol-filter", ["Tous"]
    )
    if isinstance(selected_symbols, str):
        # Preserve filters saved by the former single-select control.
        selected_symbols = [selected_symbols]
        st.session_state["simulation-trades-symbol-filter"] = selected_symbols
    selected_symbols = [str(symbol) for symbol in selected_symbols]
    selected_model = st.session_state.get("simulation-trades-model-filter", "Tous")
    filtered_trades = result.trades
    active_symbols = [symbol for symbol in selected_symbols if symbol != "Tous"]
    if active_symbols:
        filtered_trades = filtered_trades[
            filtered_trades["Symbole"].astype(str).isin(active_symbols)
        ]
    if selected_model != "Tous":
        filtered_trades = filtered_trades[
            filtered_trades["Modèle source"].astype(str) == selected_model
        ]
    filtered_result = summarize_simulation_trades(filtered_trades)
    metrics = filtered_result.metrics
    calculated_mask = pd.to_numeric(filtered_trades["Rendement"], errors="coerce").notna()
    total_invested = pd.to_numeric(
        filtered_trades.loc[calculated_mask, "Montant investi"], errors="coerce"
    ).sum()
    global_return = (
        None
        if total_invested == 0
        else metrics.total_profit_loss / float(total_invested)
    )
    kpis = st.columns(6)
    kpis[0].metric("Profit / perte total(e)", _simulation_currency(metrics.total_profit_loss))
    kpis[1].metric("Taux de trades gagnants", _simulation_percent(metrics.winning_trade_rate))
    kpis[2].metric("Trades calculés", metrics.calculated_trades)
    kpis[3].metric("Rendement moyen / trade", _simulation_percent(metrics.average_return))
    kpis[4].metric("Signaux trouvés", metrics.signals_found)
    kpis[5].metric("Trades exclus", metrics.excluded_trades)

    charts = st.columns([3, 1])
    with charts[0]:
        st.subheader("Évolution du résultat cumulé")
        if filtered_result.cumulative_results.empty:
            st.caption("Aucun trade calculé sur la période.")
        else:
            strategy_chart = (
                alt.Chart(filtered_result.cumulative_results)
                .mark_line(point=True, color="#1677ff")
                .encode(
                    x=alt.X("Date:T", title="Date des trades"),
                    y=alt.Y("Résultat cumulé:Q", title="Profit / perte cumulé ($)"),
                    tooltip=[
                        alt.Tooltip("Date:T", title="Date"),
                        alt.Tooltip("Résultat cumulé:Q", title="Résultat", format=",.2f"),
                    ],
                )
            )
            benchmark = benchmark_cumulative_for_trades(
                filtered_trades, result.benchmark_results
            )
            cumulative_chart = strategy_chart
            if not benchmark.empty:
                cumulative_chart = strategy_chart + (
                    alt.Chart(benchmark).mark_line(color="#7a7f87", strokeDash=[4, 3])
                    .encode(
                        x="Date:T",
                        y="SPY résultat cumulé:Q",
                        tooltip=[
                            alt.Tooltip("Date:T", title="Date"),
                            alt.Tooltip("SPY résultat cumulé:Q", title="SPY", format=",.2f"),
                        ],
                    )
                )
            st.altair_chart(cumulative_chart, width="stretch")
            if benchmark.empty:
                st.caption("Référence SPY indisponible pour cette simulation.")
            else:
                st.caption(
                    "Comparaison notionnelle : SPY reçoit le même montant par signal "
                    "haussier; les fractions de titres sont admises dans les deux cas."
                )
    with charts[1]:
        st.subheader("Répartition des résultats")
        distribution_chart = (
            alt.Chart(filtered_result.result_distribution)
            .mark_bar()
            .encode(
                x=alt.X("Résultat:N", title=None),
                y=alt.Y("Trades:Q", title="Nombre de trades"),
                color=alt.Color(
                    "Résultat:N",
                    scale=alt.Scale(domain=["Trades gagnants", "Trades perdants"], range=["#21a366", "#ef5b57"]),
                    legend=None,
                ),
                tooltip=["Résultat", "Trades"],
            )
        )
        st.altair_chart(distribution_chart, width="stretch")

    synthesis = st.columns(5)
    synthesis[0].metric("Gain moyen", _simulation_percent(metrics.average_winning_return))
    synthesis[1].metric("Perte moyenne", _simulation_percent(metrics.average_losing_return))
    synthesis[2].metric("Meilleur trade", _simulation_percent(metrics.best_trade))
    synthesis[3].metric("Pire trade", _simulation_percent(metrics.worst_trade))
    synthesis[4].metric("Rendement global", _simulation_percent(global_return))

    st.subheader("Détail des trades")
    filter_columns = st.columns(2)
    symbols = ["Tous", *sorted(result.trades["Symbole"].dropna().astype(str).unique())]
    models = ["Tous", *sorted(result.trades["Modèle source"].dropna().astype(str).unique())]
    filter_columns[0].multiselect(
        "Symbole",
        symbols,
        default=["Tous"],
        key="simulation-trades-symbol-filter",
    )
    filter_columns[1].selectbox(
        "Modèle source",
        models,
        key="simulation-trades-model-filter",
    )
    displayed_trades = filtered_trades.sort_values(
        "Date signal", ascending=False, kind="stable"
    )
    st.dataframe(
        displayed_trades,
        hide_index=True,
        width="stretch",
        column_config={
            "P(Up)": st.column_config.NumberColumn(format="%.4f"),
            "Seuil Up": st.column_config.NumberColumn(format="%.4f"),
            "Prix achat": st.column_config.NumberColumn(format="%.2f $"),
            "Prix vente": st.column_config.NumberColumn(format="%.2f $"),
            "Rendement": st.column_config.NumberColumn(format="percent"),
            "Montant investi": st.column_config.NumberColumn(format="%.2f $"),
            "Profit / perte": st.column_config.NumberColumn(format="%.2f $"),
        },
    )

    st.subheader("Qualité des données")
    quality_kpis = st.columns(3)
    quality_kpis[0].metric("Prix manquants", metrics.missing_prices)
    quality_kpis[1].metric("Signaux ignorés", metrics.excluded_trades)
    quality_kpis[2].metric("Couverture des prix", _simulation_percent(metrics.price_coverage))
    if metrics.signals_found == 0:
        st.info("Aucun signal haussier actif dans la période sélectionnée.")
    elif metrics.price_coverage >= 0.9:
        st.success("Données globalement conformes pour la simulation.")
    else:
        st.warning("Couverture partielle : certains trades ont été exclus.")

def _simulation_model_snapshots(
    project_root: Path,
    result: SimulationResult,
    simulation_mode: str,
) -> list[dict[str, object]]:
    if simulation_mode == "Historique":
        return [dict(model) for model in result.model_snapshots]
    try:
        models = ProductionRepository(project_root).models()
    except (OSError, ValueError, json.JSONDecodeError):
        models = []
    used_ids = set(result.trades.get("Modèle source", pd.Series(dtype=object)).dropna().astype(str))
    selected = [model for model in models if model.model_id in used_ids]
    return [model.to_dict() for model in selected]


def _simulation_parameters(start_date, end_date, amount, exit_mode, simulation_mode) -> dict[str, object]:
    return {
        "start_date": pd.Timestamp(start_date).date().isoformat(),
        "end_date": pd.Timestamp(end_date).date().isoformat(),
        "amount_per_signal": float(amount),
        "exit_mode": str(exit_mode),
        "simulation_mode": str(simulation_mode),
    }


def _parse_simulation_date(value: object) -> date | None:
    """Convert persisted simulation dates to the type expected by date_input."""

    if isinstance(value, datetime):
        return value.date()
    if isinstance(value, date):
        return value
    if isinstance(value, str):
        try:
            return date.fromisoformat(value[:10])
        except ValueError:
            return None
    return None


def _restore_simulation_parameters(parameters: object) -> None:
    if not isinstance(parameters, dict):
        return
    start_date = _parse_simulation_date(parameters.get("start_date"))
    end_date = _parse_simulation_date(parameters.get("end_date"))
    if start_date is not None:
        st.session_state["simulation-start-date"] = start_date
    if end_date is not None:
        st.session_state["simulation-end-date"] = end_date
    amount = parameters.get("amount_per_signal")
    try:
        if amount is not None:
            st.session_state["simulation-amount"] = float(amount)
    except (TypeError, ValueError):
        pass
    exit_mode = parameters.get("exit_mode")
    if exit_mode in {"Clôture du jour"}:
        st.session_state["simulation-exit-mode"] = exit_mode
    simulation_mode = parameters.get("simulation_mode")
    if simulation_mode in {"Historique", "Prédictions évaluées"}:
        st.session_state["simulation-mode"] = simulation_mode


def _simulation_sidebar(project_root: Path) -> None:
    repository = SimulationRepository(project_root)
    records = repository.list_simulations()
    st.markdown(
        '<div style="font-size:1.4rem; line-height:1.2; font-weight:600; '
        'margin-top:50px; margin-bottom:10px;">'
        "Simulations précédentes"
        "</div>",
        unsafe_allow_html=True,
    )
    selected_record = st.session_state.get("simulation-record")
    raw_selected_id = (
        selected_record.get("simulation_id")
        if isinstance(selected_record, dict)
        else None
    )
    selected_id = str(raw_selected_id) if raw_selected_id else None
    query = st.text_input(
        "Rechercher…",
        key="simulation-search",
        placeholder="Recherche",
        label_visibility="collapsed",
    )
    visible = [record for record in records if not query or str(record.get("created_at", "")).lower().find(query.lower()) >= 0]
    st.markdown(
        """
        <style>
        div[class*="st-key-simulation-card-"] {
            border-radius: 12px;
            margin-bottom: 8px;
            position: relative;
        }
        div[class*="st-key-simulation-card-selected-"] {
            background: #eef6ff;
            border-color: #d9eaff;
        }
        div[class*="st-key-open-simulation-"] {
            position: absolute;
            inset: 0;
            z-index: 1;
        }
        div[class*="st-key-open-simulation-"] button {
            height: 100%;
            opacity: 0;
        }
        div[class*="st-key-delete-simulation-"] {
            position: relative;
            z-index: 2;
        }
        div[class*="st-key-delete-simulation-"] [data-testid="stMarkdownContainer"] {
            display: none;
        }
        </style>
        """,
        unsafe_allow_html=True,
    )
    for record in visible:
        simulation_id = str(record.get("simulation_id", ""))
        created = str(record.get("created_at", "")).replace("T", " ")[:16]
        params = record.get("parameters", {})
        period = f"{params.get('start_date', '—')} → {params.get('end_date', '—')}"
        pnl = float(record.get("metrics", {}).get("total_profit_loss", 0.0))
        selected = (selected_id == simulation_id)
        card_key = (
            f"simulation-card-selected-{simulation_id}"
            if selected
            else f"simulation-card-{simulation_id}"
        )
        status = {"completed": "Terminée"}.get(
            str(record.get("status", "completed")), str(record.get("status", "—"))
        )
        color = "#2f9e44" if pnl >= 0 else "#e03131"
        amount = f"{float(params.get('amount_per_signal', 0)):,.0f}".replace(",", " ")
        pnl_text = f"P/L {pnl:+,.0f} $".replace(",", " ")
        with st.container(border=True, key=card_key):
            header = st.columns([6, 1], vertical_alignment="center")
            header[0].markdown(f"**{created}**")
            delete_requested = header[1].button(
                "Supprimer",
                icon=":material/delete:",
                key=f"delete-simulation-{simulation_id}",
                help="Supprimer",
            )
            st.caption(period)
            st.caption(f"{amount} $ | {params.get('simulation_mode', '—')}")
            footer = st.columns([1, 1], vertical_alignment="center")
            footer[0].markdown(
                '<span style="background:#d8f3dc; color:#2b8a3e; border-radius:12px; '
                'padding:3px 10px; font-size:0.85rem; font-weight:600;">'
                f"{html.escape(status)}</span>",
                unsafe_allow_html=True,
            )
            open_requested = st.button(
                "Ouvrir la simulation",
                key=f"open-simulation-{simulation_id}",
                width="stretch",
            )
            footer[1].markdown(
                f'<div style="text-align:right; color:{color}; font-weight:600;">'
                f"{pnl_text}</div>",
                unsafe_allow_html=True,
            )
        if delete_requested:
            repository.delete(simulation_id)
            if selected:
                st.session_state.pop("simulation-record", None)
                st.session_state.pop("simulation-result", None)
            st.rerun()
        if open_requested:
            try:
                metadata, result = repository.load(simulation_id)
                st.session_state["simulation-result"] = result
                st.session_state["simulation-record"] = metadata
                _restore_simulation_parameters(metadata.get("parameters"))
                st.rerun()
            except (OSError, ValueError, json.JSONDecodeError) as error:
                st.error(f"Simulation illisible : {error}")
    if not records:
        st.caption("Aucune simulation enregistrée")


def _render_simulation_main(project_root: Path) -> None:
    st.caption("Évaluez les signaux haussiers actifs avec les prix réels Open/Close.")
    project_root = st.session_state.lab_config.project_root
    signals = SignalService(project_root).active_history()
    available_dates = pd.to_datetime(
        signals.get("prediction_date", pd.Series(dtype=object)), errors="coerce"
    ).dropna()
    default_end = (
        available_dates.max().date() if not available_dates.empty else pd.Timestamp.today().date()
    )
    default_start = default_end - timedelta(days=365)
    if st.session_state.get("simulation-mode") == "Résultats réalisés":
        st.session_state["simulation-mode"] = "Prédictions évaluées"
    defaults = {
        "simulation-start-date": default_start,
        "simulation-end-date": default_end,
        "simulation-amount": 10_000.0,
        "simulation-exit-mode": "Clôture du jour",
        "simulation-mode": "Prédictions évaluées",
    }
    for key, value in defaults.items():
        st.session_state.setdefault(key, value)

    with st.container(border=True):
        controls = st.columns([1, 1, 1, 1, 1.4, 1])
        start_date = controls[0].date_input("Date de début", key="simulation-start-date")
        end_date = controls[1].date_input("Date de fin", key="simulation-end-date")
        amount = controls[2].number_input(
            "Montant par signal", min_value=0.01, step=1_000.0,
            key="simulation-amount",
        )
        controls[3].selectbox(
            "Mode de sortie", ["Clôture du jour"], disabled=True,
            key="simulation-exit-mode",
        )
        simulation_mode = controls[4].selectbox(
            "Mode de simulation",
            ["Historique", "Prédictions évaluées"],
            key="simulation-mode",
        )
        # Reserve the label's height so the action lines up with the inputs.
        controls[5].markdown('<div style="height: 2rem;"></div>', unsafe_allow_html=True)
        launch = controls[5].button(
            "Lancer la simulation", type="primary", width="stretch"
        )
        st.caption(
            "Les trades sans prix Open ou Close sont conservés dans le détail, "
            "mais exclus des calculs financiers. Chaque signal Up représente une "
            "transaction indépendante. Tous les modèles actifs sont utilisés."
        )
    if launch:
        try:
            service = SimulationService.local(
                project_root, st.session_state.lab_config
            )
            if simulation_mode == "Historique":
                result = service.run_historical(
                    start_date,
                    end_date,
                    st.session_state.lab_config,
                    float(amount),
                )
            else:
                result = service.run(start_date, end_date, float(amount))
            st.session_state["simulation-result"] = result
            try:
                metadata = SimulationRepository(project_root).save(
                    result,
                    parameters=_simulation_parameters(
                        start_date, end_date, amount, st.session_state["simulation-exit-mode"], simulation_mode
                    ),
                    models=_simulation_model_snapshots(
                        project_root, result, simulation_mode
                    ),
                )
                st.session_state["simulation-record"] = metadata
                st.rerun()
            except OSError as error:
                st.error(f"Simulation calculée mais non persistée : {error}")
        except ValueError as error:
            st.error(str(error))
            st.session_state.pop("simulation-result", None)
    result = st.session_state.get("simulation-result")
    if isinstance(result, SimulationResult):
        _render_simulation_results(result)
    else:
        st.info("Configurez la période puis lancez la simulation.")


def _simulation_page() -> None:
    _page_header("Simulation")
    project_root = st.session_state.lab_config.project_root
    layout = st.columns([1, 4], gap="medium")
    with layout[0]:
        _simulation_sidebar(project_root)
    with layout[1]:
        _render_simulation_main(project_root)


def _documentation_sections(markdown: str) -> list[tuple[str, str]]:
    """Split the user guide into its top-level Markdown sections."""

    sections: list[tuple[str, str]] = []
    title: str | None = None
    body: list[str] = []
    for line in markdown.splitlines():
        if line.startswith("# "):
            if title is not None:
                sections.append((title, "\n".join(body).strip()))
            title = line[2:].strip()
            body = []
        elif title is not None:
            body.append(line)
    if title is not None:
        sections.append((title, "\n".join(body).strip()))
    return sections[1:]


def _documentation_page() -> None:
    _page_header("Documentation")
    st.caption("Guide fonctionnel du processus RStock, en consultation seulement.")
    try:
        guide = USER_GUIDE_PATH.read_text(encoding="utf-8")
    except OSError as error:
        st.error(f"Documentation indisponible : {error}")
        return
    sections = _documentation_sections(guide)
    if not sections:
        st.warning("La documentation ne contient aucune section à afficher.")
        return
    st.markdown("**Sommaire** · " + " · ".join(title for title, _ in sections))
    for index, (title, content) in enumerate(sections):
        with st.expander(title, expanded=index == 0):
            image_name = "rstock_process_user_guide.png"
            image_markdown = f"![Processus général RStock](../assets/{image_name})"

            if image_markdown in content:
                before, after = content.split(image_markdown, 1)

                st.markdown(before)

                image_path = USER_GUIDE_PATH.parent.parent / "assets" / image_name
                if image_path.is_file():
                    st.image(
                        str(image_path),
                        caption="Processus général RStock",
                        width=1080,
                    )

                st.markdown(after)
            else:
                st.markdown(content)


def _primary_pages() -> list[st.Page]:
    """Flat V1 navigation; this factory can later return grouped page mappings."""

    global _PRIMARY_PAGES
    if _PRIMARY_PAGES is not None:
        return _PRIMARY_PAGES
    _PRIMARY_PAGES = [
        st.Page(_surveillance_page, title="Surveillance", icon=":material/monitoring:", default=True),
        st.Page(_returns_page, title="Rendement", icon=":material/attach_money:"),
        st.Page(_models_page, title="Modèles", icon=":material/model_training:"),
        st.Page(_history_page, title="Historique", icon=":material/history:"),
        st.Page(_experiments_page, title="Expériences", icon=":material/science:"),
        st.Page(_universes_page, title="Univers", icon=":material/list_alt:"),
        st.Page(_settings_page, title="Paramètres", icon=":material/settings:"),
        st.Page(_simulation_page, title="Simulation", icon=":material/monitoring:"),
        st.Page(_documentation_page, title="Documentation", icon=":material/menu_book:"),
    ]
    return _PRIMARY_PAGES


_state()
selected_page = st.navigation(_primary_pages(), position="top")
requested_page = st.session_state.pop(EXPERIMENT_NAVIGATION_KEY, None)
if requested_page is not None:
    target_page = next(
        (page for page in _primary_pages() if page.title == requested_page), None
    )
    if target_page is not None:
        st.switch_page(target_page)
selected_page.run()
