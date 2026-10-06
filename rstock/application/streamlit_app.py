"""Thin Streamlit interface for RStock Laboratory."""

from __future__ import annotations

import base64
import hashlib
import html
import json
import logging
import math
import time
from dataclasses import asdict, replace
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any, Mapping, Sequence

import altair as alt
import pandas as pd
import streamlit as st

LOGGER = logging.getLogger(__name__)

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.batch_purge import (
    BatchPurgeOutcome,
    BatchPurgeReview,
    execute_batch_purge,
    preview_batch_purge,
)
from rstock.application.batch_delete import (
    BatchDeleteOutcome,
    BatchDeleteReview,
    execute_batch_delete,
    preview_batch_delete,
)
from rstock.application.run_delete import DeletePlan, DeletionCleanupPending
from rstock.application.promotion_qualification_ui import (
    DEFAULT_PROMOTION_SORT, sort_promotion_decisions, upstream_diagnostic,
)
from rstock.application.temporal_validation import read_time_candidate_identity_stability
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
from rstock.application.run_comparison import (
    comparison_types,
    load_end_to_end_comparison,
)
from rstock.application.prefilter_comparison import (
    ND, compare_prefilter_candidates, load_prefilter_comparison,
    prefilter_comparison_csv, prefilter_profile_differences,
)
from rstock.application.runner import running_duration
from rstock.application.end_to_end import historical_forced_validation_state
from rstock.application.derivation import (
    FORK_STAGE_KEYS, SPLIT_FORK_STAGE_KEYS, PREFILTER_SCIENTIFIC_STAGE_KEYS, PREFILTER_STAGE_PARAMETER_FIELDS, STAGE_PARAMETER_FIELDS,
    SPLIT_STAGE_PARAMETER_FIELDS, stage_modes,
)
from rstock.application.derived_experiments import source_parameter_value
from rstock.application.prefilter_experiments import (
    PREFILTER_DERIVATION_FIELDS, PREFILTER_XGBOOST_FIELDS,
)
from rstock.application.walk_forward_experiments import (
    WF_GEOMETRY_FIELDS, WF_XGBOOST_FIELDS, WF_QUALIFICATION_FIELDS,
    WF_SELECTION_FIELDS,
)
from rstock.application.end_to_end import load_pipeline_manifest
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
    latest_session_results_view,
    next_session_signals_view,
    surveillance_display_session,
    surveillance_model_history_table,
    prioritize_signals_view,
    signal_priority_model_lookup,
    prediction_feature_tables,
    session_crosses_weekend,
    source_observation_tables,
    surveillance_kpi_values,
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
from rstock.application.production_quality_ui import (
    baseline_comparison_display_table,
    baseline_comparison_rows,
    baseline_unavailability_reason,
    directional_display_style,
    evaluated_bullish_signals_display_table,
    evaluated_bullish_signals,
    excluded_observations_display_table,
    excluded_observations as quality_excluded_observations,
    filter_quality_models,
    global_quality_kpis,
    health_label,
    initial_qualification_metrics,
    load_model_quality_detail,
    load_models_master,
    models_grid,
    model_phase_comparison_table,
    performance_windows_display_table,
    sort_quality_models,
    style_directional_columns,
    winning_trades_display_style,
)
from rstock.application.real_trades import (
    RealTradeService,
    filter_performance,
    performance_kpis,
    performance_table,
)
from rstock.application.model_ui import (
    DEFAULT_MODEL_STATUSES,
    MODEL_STATUS_ORDER,
    filter_models,
    job_domain,
    job_domain_title,
    model_filter_options,
    model_status_label,
)
from rstock.application.simulation import (
    SIMULATION_MODE_DAILY_RETRAIN,
    SIMULATION_MODE_EVALUATED_PREDICTIONS,
    SIMULATION_MODE_FROZEN_AT_START,
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
    CombinationPlan,
    build_combination_plan,
    build_combination_preview,
)
from rstock.application.repository import RunRepository
from rstock.config import (
    DEFAULT_CONFIG,
    RStockConfig,
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
    "production_quality": "Qualité des modèles Production",
    "production_quality_rebuild": "Rebuild qualité Production",
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


def _configure_top_navigation_spacing() -> None:
    """Use the same compact top offset on every page with native top navigation."""
    st.markdown(
        """
        <style>
        div[data-testid="stMainBlockContainer"] {
            padding-top: 2rem !important;
        }
        </style>
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
        if selected_job_type is JobType.WALK_FORWARD and draft.get("source_prefilter_run"):
            from rstock.application.prefilter_contract import CONTRACT
            contract = service.run_service.repository.read_json(draft["source_prefilter_run"], CONTRACT)
            st.session_state["duplication-prefilter-cutoff"] = str(contract["cutoff"])
            st.text_input("Cutoff hérité du Préfiltre", disabled=True,
                          key="duplication-prefilter-cutoff")
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
    service = ExperimentService.local(
        st.session_state.lab_config.project_root,
        max_concurrent_heavy_jobs=st.session_state.max_concurrent_heavy_jobs,
    )
    project_key = str(st.session_state.lab_config.project_root)
    recovery_key = "_forward-dispatch-recovery-scan"
    previous = st.session_state.get(recovery_key)
    now = time.monotonic()
    if (
        not isinstance(previous, tuple)
        or len(previous) != 2
        or previous[0] != project_key
        or now < float(previous[1])
        or now - float(previous[1]) >= 15.0
    ):
        # Persist the throttle before scanning so nested UI rendering cannot
        # initiate a second scan in the same Streamlit rerun.
        st.session_state[recovery_key] = (project_key, now)
        try:
            service.recover_pending_forward_dispatches()
        except (OSError, TimeoutError) as error:
            LOGGER.warning("Automatic Forward dispatch recovery scan failed: %s", error)
    return service


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
            origin_details = progress.get("details", {})
            temporal_progress = origin_details.get("progress_scope") == "temporal_prefilter"
            if temporal_progress:
                st.caption(f"Origine : {origin_details['origin_number']} / {origin_details['origin_count']} · "
                           f"Cutoff origine : {origin_details['origin_cutoff']}")
                workflow_percent = progress.get("stage_percent")
            if workflow_percent is not None:
                st.progress(float(workflow_percent) / 100.0)
            if progress.get("stage_percent") is not None:
                completed = progress.get("completed_units")
                total = progress.get("total_units")
                eta = progress.get("eta_seconds")
                message = f"{completed} / {total} unités" + (" globales" if temporal_progress else "")
                if temporal_progress:
                    message += (f" · Origine {origin_details['origin_number']}/{origin_details['origin_count']} : "
                                f"{origin_details['origin_completed_units']} / {origin_details['origin_total_units']}")
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


def _combination_plan_preview(job_type: JobType, *, config=None, selection_mode=None, source_prefilter_run=None) -> bool:
    """Render the raw plan shared with future WF and End-to-end execution."""

    st.session_state.pop("experiment-combination-preview", None)
    if job_type not in {JobType.WALK_FORWARD, JobType.END_TO_END}:
        return True
    config = config or st.session_state.lab_config
    selection_mode = selection_mode or config.prefilter_selection_mode
    bounds_config = config
    effective = None
    targets = st.session_state.lab_target_symbols
    predictors = st.session_state.lab_symbols
    contexts = st.session_state.lab_context_symbols
    if source_prefilter_run:
        repository = RunRepository(config.project_root / "runs")
        source_spec = repository.load_spec(source_prefilter_run)
        bounds_config, selection_mode = source_spec.config, source_spec.prefilter_method
        targets, predictors, contexts = source_spec.target_symbols, source_spec.predictor_symbols, source_spec.context_symbols
        from rstock.application.prefilter_contract import CONTRACT
        contract = json.loads((repository.run_directory(source_prefilter_run) / CONTRACT).read_text(encoding="utf-8"))
        effective = CombinationPlan.from_target_predictors(dict(contract["ordered_predictors_by_target"]), config.permutation_depth)
    try:
        plan = build_combination_plan(
            target_symbols=targets,
            predictor_symbols=predictors,
            permutation_depth=config.permutation_depth,
        )
        preview = build_combination_preview(
            plan,
            context_symbols=contexts,
            effective_plan=effective, prefilter_selection_mode=selection_mode,
            temporal_consensus_origins=bounds_config.temporal_consensus_origins,
            temporal_consensus_min_occurrences=bounds_config.temporal_consensus_min_occurrences,
            max_combinations_per_batch=(
                config.walk_forward_max_combinations_per_batch
            ),
            prefilter_enabled=(
                config.predictor_prefilter_enabled
            ),
            prefilter_top_n=(
                bounds_config.predictor_prefilter_top_n
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
        if selection_mode == "temporal_consensus":
            st.caption("Borne consensus : Top N × origines / occurrences minimales. Aucun Top N global n'est appliqué.")
        if effective is not None:
            st.metric("Combinaisons figées du Préfiltre", f"{effective.count():,}")
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
    if spec.job_type is JobType.PREDICTOR_PREFILTER:
        method_text = (
            "Origine unique"
            if spec.prefilter_method == "single_origin" else
            f"Consensus temporel ({spec.config.temporal_consensus_min_occurrences}/{spec.config.temporal_consensus_origins})"
            if spec.prefilter_method == "temporal_consensus" else
            f"Stabilité temporelle ({spec.stability_origin_count} origines, "
            f"pas {spec.stability_step_sessions} séances)"
        )
        st.success(
            f"Soumettre le préfiltre prédicteurs ? {len(spec.target_symbols):,} cibles · "
            f"{len(spec.predictor_symbols):,} prédicteurs · "
            f"cutoff {spec.historical_data_cutoff} · "
            f"Top {spec.config.predictor_prefilter_top_n} · {method_text}. "
            "Le job s'arrête après le classement et la sélection."
        )
    else:
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


def _launch_consensus_config(current: RStockConfig) -> tuple[RStockConfig, bool]:
    """Edit the existing consensus fields without changing shared Settings."""
    with st.container(border=True):
        st.subheader("Paramètres du consensus temporel")
        columns = st.columns(3)
        fields = (
            ("temporal_consensus_origins", "Nombre d'origines"),
            ("temporal_consensus_step_sessions", "Espacement entre origines (séances XNYS)"),
            ("temporal_consensus_min_occurrences", "Occurrences minimales"),
        )
        values = {
            field: int(column.number_input(
                label, min_value=1, value=getattr(current, field), step=1,
                key=f"launch-{field}",
            ))
            for column, (field, label) in zip(columns, fields, strict=True)
        }
        if values["temporal_consensus_min_occurrences"] > values["temporal_consensus_origins"]:
            st.error("Les occurrences minimales ne peuvent pas dépasser le nombre d'origines.")
            return current, False
        try:
            return replace(current, **values), True
        except ValueError as error:
            st.error(f"Paramètres du consensus invalides : {error}")
            return current, False


def _inherit_prefilter_universe(source: ExperimentSpec) -> None:
    """Use the source snapshot, even when saved universe definitions have changed."""
    values = {
        "lab_universe_selection": source.universe_selection,
        "lab_market_benchmark_symbol": source.market_benchmark_symbol,
        "lab_context_universe_ids": list(source.context_universe_ids),
        "lab_context_sample_size": source.context_sample_size,
        "lab_context_selection_method": source.context_selection_method,
        "lab_context_seed": source.context_seed,
        "lab_target_symbols": list(source.target_symbols),
        "lab_context_symbols": list(source.context_symbols),
        "lab_symbols": list(source.predictor_symbols),
        "lab_calendar": source.calendar,
    }
    for key, value in values.items():
        st.session_state[key] = value
    with st.container(border=True):
        st.subheader("Univers hérités du Préfiltre (lecture seule)")
        st.caption(f"Univers principal : {source.primary_universe_id} · "
                   f"{len(source.target_symbols)} cibles · "
                   f"{len(source.context_symbols)} symboles de contexte · "
                   f"{len(source.predictor_symbols)} prédicteurs")
        st.caption("Pour changer d'univers, créez un nouveau Préfiltre puis sélectionnez son run source.")
        with st.expander("Voir les symboles hérités"):
            st.write("Cibles")
            st.code(", ".join(source.target_symbols))
            st.write("Contexte")
            st.code(", ".join(source.context_symbols) or "Aucun")


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
        "Préfiltre prédicteurs": JobType.PREDICTOR_PREFILTER,
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
    prefilter_method = run_config.prefilter_selection_mode if selected_job_type in {JobType.PREDICTOR_PREFILTER, JobType.END_TO_END} else "single_origin"
    stability_origin_count = 5
    stability_step_sessions = 1
    valid_consensus = True
    if selected_job_type in {JobType.PREDICTOR_PREFILTER, JobType.END_TO_END}:
        st.caption("Évaluation univariée et sélection uniquement; aucun Walk-forward complet."
                   if selected_job_type is JobType.PREDICTOR_PREFILTER else
                   "Le Préfiltre, lorsqu'il est activé, est une étape autonome avant le Walk-forward.")
        prefilter_method = st.radio(
            "Méthode de préfiltre", ("single_origin", "temporal_stability", "temporal_consensus"),
            format_func=lambda value: {
                "single_origin": "Origine unique",
                "temporal_stability": "Stabilité temporelle",
                "temporal_consensus": "Consensus temporel",
            }[value], index=("single_origin", "temporal_stability", "temporal_consensus").index(prefilter_method), horizontal=True, key="launch-prefilter-method",
        )
        if prefilter_method == "temporal_consensus":
            run_config, valid_consensus = _launch_consensus_config(run_config)
        elif prefilter_method == "temporal_stability":
            first, second = st.columns(2)
            stability_origin_count = int(first.number_input(
                "Nombre d'origines", min_value=1, value=5, step=1,
                key="launch-prefilter-origin-count",
            ))
            stability_step_sessions = int(second.number_input(
                "Pas en séances", min_value=1, value=1, step=1,
                key="launch-prefilter-step-sessions",
            ))
        with st.container(border=True):
            st.caption(
                "Paramètres scientifiques hérités des Settings : "
                f"Top {run_config.predictor_prefilter_top_n}, "
                f"AUC médiane ≥ {run_config.predictor_prefilter_min_median_auc:.3f}, "
                f"fenêtres > 0,50 ≥ {run_config.predictor_prefilter_min_pct_above_random:.3f}, "
                f"pire AUC ≥ {run_config.predictor_prefilter_min_worst_auc:.3f}, "
                f"std AUC ≤ {run_config.predictor_prefilter_max_auc_std:.3f}, "
                f"corrélation < {run_config.predictor_prefilter_correlation_threshold:.3f}."
            )
        run_config = replace(
            run_config, prefilter_selection_mode=prefilter_method,
            predictor_prefilter_enabled=True if selected_job_type is JobType.PREDICTOR_PREFILTER else run_config.predictor_prefilter_enabled,
            walk_forward_end_offset_sessions=0 if selected_job_type is JobType.PREDICTOR_PREFILTER else run_config.walk_forward_end_offset_sessions,
        )
    auto_promote_candidates = False
    temporal_validation_enabled = False
    requested_historical_cutoff = None
    resolved_historical_cutoff = None
    forward_simulation_enabled = False
    forward_simulation_mode = None
    forward_simulation_end_date = None
    prefilter_reference = None
    if selected_job_type is JobType.WALK_FORWARD and run_config.predictor_prefilter_enabled:
        repository = service.run_service.repository
        sources = []
        for candidate in repository.list_run_ids():
            if (repository.status(candidate).get("status") == "completed"
                    and repository.load_spec(candidate).job_type is JobType.PREDICTOR_PREFILTER
                    and repository.storage(candidate)["state"] == "full"
                    and (repository.run_directory(candidate) / "results/prefilter_contract.json").is_file()):
                sources.append(candidate)
        if sources:
            prefilter_reference = st.selectbox("Préfiltre source (résultat figé)", sources,
                                              key="launch-wf-prefilter-source")
            st.caption("Le Walk-forward consomme les candidats et les données figées de ce Préfiltre.")
            from rstock.application.prefilter_contract import CONTRACT
            contract = repository.read_json(prefilter_reference, CONTRACT)
            resolved_historical_cutoff = str(contract["cutoff"])
            st.session_state["launch-wf-inherited-cutoff"] = resolved_historical_cutoff
            st.text_input("Cutoff hérité du Préfiltre", disabled=True,
                          key="launch-wf-inherited-cutoff")
            pending = st.session_state.get("pending-experiment-submission")
            if pending is not None and pending.source_prefilter_run != prefilter_reference:
                st.session_state.pop("pending-experiment-submission", None)
        else:
            st.info("Lancez d'abord un job Préfiltre, ou utilisez End-to-End pour enchaîner les étapes.")
    if selected_job_type in {JobType.END_TO_END, JobType.PREDICTOR_PREFILTER} or (
        selected_job_type is JobType.WALK_FORWARD and not run_config.predictor_prefilter_enabled
    ):
        requested_historical_cutoff = st.date_input(
            "Cutoff historique", value=None,
            help="Dernière séance XNYS disponible pour la découverte scientifique.",
        )
        if requested_historical_cutoff is not None:
            resolved_historical_cutoff = resolve_market_session_on_or_before(
                requested_historical_cutoff, st.session_state.lab_calendar
            ).date().isoformat()
            st.caption(f"Séance XNYS résolue : {resolved_historical_cutoff}")
        if prefilter_method == "temporal_consensus" and valid_consensus:
            st.caption(
                f"{run_config.temporal_consensus_origins} origines, espacées de "
                f"{run_config.temporal_consensus_step_sessions} séances XNYS. "
                f"Règle : retenu si présent dans au moins "
                f"{run_config.temporal_consensus_min_occurrences} origines sur "
                f"{run_config.temporal_consensus_origins}."
            )
            if resolved_historical_cutoff is not None:
                from rstock.application.prefilter_consensus import resolve_consensus_origins
                try:
                    origins = resolve_consensus_origins(
                        resolved_historical_cutoff, st.session_state.lab_calendar,
                        run_config.temporal_consensus_origins,
                        run_config.temporal_consensus_step_sessions,
                    )
                    st.caption("Cutoffs prévus : " + ", ".join(
                        origin.date().isoformat() for origin in origins
                    ))
                except ValueError as error:
                    st.error(f"Origines du consensus invalides : {error}")
                    valid_consensus = False
            else:
                st.caption("Les cutoffs seront résolus à partir de la dernière séance disponible au lancement.")
    if selected_job_type is JobType.END_TO_END:
        if requested_historical_cutoff is not None:
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
            run_config,
            window_mode,
        )
    inherited_universe = None
    if selected_job_type is JobType.WALK_FORWARD and run_config.predictor_prefilter_enabled:
        valid_universe = prefilter_reference is not None
        if prefilter_reference is not None:
            inherited_universe = repository.load_spec(prefilter_reference)
            _inherit_prefilter_universe(inherited_universe)
            pending = st.session_state.get("pending-experiment-submission")
            if pending is not None and any(
                getattr(pending, field) != getattr(inherited_universe, field)
                for field in (
                    "target_symbols", "context_symbols", "predictor_symbols", "calendar",
                    "universe_selection", "primary_universe_id", "market_benchmark_symbol",
                    "context_universe_ids", "context_sample_size", "context_selection_method", "context_seed",
                )
            ):
                st.session_state.pop("pending-experiment-submission", None)
    else:
        valid_universe = _experiment_universe_selector()
    valid_plan = (
        _combination_plan_preview(selected_job_type, config=run_config, selection_mode=prefilter_method,
                                  source_prefilter_run=prefilter_reference) if valid_universe and valid_consensus else False
    )
    submit_disabled = not valid_consensus or (selected_job_type is JobType.WALK_FORWARD and run_config.predictor_prefilter_enabled and prefilter_reference is None) or not (valid_universe and valid_plan) or (
        auto_promote_candidates and not st.session_state.lab_evaluate_holdout
    ) or (selected_job_type is JobType.PREDICTOR_PREFILTER
          and resolved_historical_cutoff is None
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
            prefilter_method=prefilter_method,
            stability_origin_count=stability_origin_count,
            stability_step_sessions=stability_step_sessions,
            symbols=tuple(st.session_state.lab_symbols),
            calendar=st.session_state.lab_calendar,
            combinations_per_target=st.session_state.lab_combinations_per_target,
            evaluate_final_holdout=(
                selected_job_type is not JobType.PREDICTOR_PREFILTER
                and st.session_state.lab_evaluate_holdout
            ),
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
                "Préfiltre prédicteurs" if selected_job_type is JobType.PREDICTOR_PREFILTER
                else f"profondeur {st.session_state.lab_config.permutation_depth}"
            ),
        )
        if prefilter_reference is not None:
            from rstock.application.prefilter_contract import CONTRACT
            repository = service.run_service.repository
            path = repository.run_directory(prefilter_reference) / CONTRACT
            contract = json.loads(path.read_text(encoding="utf-8"))
            import hashlib
            spec = replace(spec, source_prefilter_run=prefilter_reference,
                           primary_universe_id=inherited_universe.primary_universe_id,
                           source_prefilter_contract_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                           source_prepared_dataset_sha256=contract["prepared_dataset_sha256"],
                           historical_data_cutoff=contract["cutoff"],
                           requested_historical_cutoff=None,
                           resolved_market_session_cutoff=contract["cutoff"],
                           prepared_snapshot_required=True, prepared_dataset_digest_required=True)
        st.session_state["pending-experiment-submission"] = spec
        st.rerun()
    st.caption(
        "Les listes se gèrent dans Univers; la sélection résolue et la "
        "configuration sont figées au lancement."
    )
    _live_job_panel(service, domain="experiment")


def _prefilter_temporal_settings(current):
    with st.container(border=True):
        st.subheader("Sélection temporelle")
        modes = ("single_origin", "temporal_stability", "temporal_consensus")
        mode = st.selectbox("Mode de sélection", modes,
                            index=modes.index(current.prefilter_selection_mode),
                            key="settings-prefilter-selection-mode")
        columns = st.columns(3)
        origins = int(columns[0].number_input("Nombre d'origines", min_value=1,
            value=current.temporal_consensus_origins, disabled=mode != "temporal_consensus",
            key="settings-consensus-origins"))
        step = int(columns[1].number_input("Espacement en séances", min_value=1,
            value=current.temporal_consensus_step_sessions, disabled=mode != "temporal_consensus",
            key="settings-consensus-step"))
        minimum = int(columns[2].number_input("Occurrences minimales", min_value=1,
            value=current.temporal_consensus_min_occurrences, disabled=mode != "temporal_consensus",
            key="settings-consensus-minimum"))
        if mode == "temporal_stability":
            st.caption("Les origines de stabilité se règlent au lancement ; les champs ci-dessus sont propres au consensus.")
        st.caption("Le consensus temporel retient les candidats sélectionnés de façon répétée sur plusieurs cutoffs historiques point-in-time.")
    return dict(prefilter_selection_mode=mode, temporal_consensus_origins=origins,
                temporal_consensus_step_sessions=step, temporal_consensus_min_occurrences=minimum)


def _prefilter_xgboost_settings(config: RStockConfig) -> dict[str, int | float]:
    """Edit dedicated training values within the predictor-prefilter section."""
    values: dict[str, int | float] = {}
    with st.container(border=True):
        st.subheader("XGBoost du préfiltre")
        columns = st.columns(3)
        for index, field in enumerate(PREFILTER_XGBOOST_FIELDS):
            label = field.removeprefix("prefilter_xgb_")
            original = getattr(config, field)
            bounds: dict[str, object] = {}
            if field in {
                "prefilter_xgb_max_depth", "prefilter_xgb_num_boost_round", "prefilter_xgb_seed",
            }:
                bounds["min_value"] = 0 if field == "prefilter_xgb_seed" else 1
                bounds["step"] = 1
                value = int(columns[index % 3].number_input(
                    label, value=int(original), key=f"settings-{field}", **bounds,
                ))
            else:
                bounds["min_value"] = (
                    0.0001 if field == "prefilter_xgb_eta" else
                    0.01 if field in {"prefilter_xgb_subsample", "prefilter_xgb_colsample_bytree"}
                    else 0.0
                )
                if field in {"prefilter_xgb_subsample", "prefilter_xgb_colsample_bytree"}:
                    bounds["max_value"] = 1.0
                value = float(columns[index % 3].number_input(
                    label, value=float(original), format="%.4f",
                    key=f"settings-{field}", **bounds,
                ))
            values[field] = value
    return values


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

        prefilter_temporal_values = _prefilter_temporal_settings(current)
        prefilter_xgboost_values = _prefilter_xgboost_settings(current)

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
        st.subheader("Qualification")
        q1, q2, q3 = st.columns(3)
        min_windows = q1.number_input(
            "Fenêtres minimales", min_value=1, value=current.qualification_min_windows,
            help="Nombre minimum de fenêtres Walk-forward valides requises pour qu’un modèle puisse être qualifié.",
        )
        median_auc = q2.number_input(
            "ROC-AUC médian minimal", min_value=0.0, max_value=1.0,
            value=current.qualification_min_median_auc,
            help=(
                "Valeur minimale de la médiane des ROC-AUC calculés sur les fenêtres Walk-forward. "
                "Un seuil supérieur à 0,50 exige une performance globale meilleure que le hasard."
            ),
        )
        pct_random = q3.number_input(
            "Part fenêtres > hasard", min_value=0.0, max_value=1.0,
            value=current.qualification_min_pct_windows_above_random,
            help=(
                "Proportion minimale de fenêtres Walk-forward dont le ROC-AUC est supérieur à 0,50. "
                "Par exemple, 0,67 signifie qu’environ deux fenêtres sur trois doivent battre le hasard."
            ),
        )
        worst_auc = q1.number_input(
            "Pire ROC-AUC minimal", min_value=0.0, max_value=1.0,
            value=current.qualification_min_worst_window_auc,
            help=(
                "Valeur minimale autorisée pour le plus faible ROC-AUC observé parmi les fenêtres Walk-forward. "
                "Ce critère limite les modèles qui s’effondrent sur une période particulière."
            ),
        )
        min_positive = q2.number_input(
            "Observations positives minimales", min_value=0,
            value=current.qualification_min_positive_observations,
            help=(
                "Nombre minimum d’observations appartenant réellement à la classe positive requis pour évaluer "
                "une fenêtre de façon fiable. Il s’agit d’exemples réels de la cible positive utilisés pour "
                "calculer les métriques, et non du nombre de signaux générés par le modèle."
            ),
        )
        max_auc_std = q3.number_input(
            "Écart-type ROC-AUC maximal", min_value=0.0, value=current.qualification_max_auc_std,
            help=(
                "Écart-type maximal autorisé des ROC-AUC entre les fenêtres Walk-forward. "
                "Une valeur plus faible exige une performance plus stable dans le temps."
            ),
        )
        final_auc = q1.number_input(
            "ROC-AUC confirmation finale", min_value=0.0, max_value=1.0,
            value=current.final_confirmation_min_auc,
            help=(
                "ROC-AUC minimal requis lors de la confirmation finale du modèle. "
                "Ce contrôle sert de barrière supplémentaire avant de poursuivre le pipeline."
            ),
        )
        prediction_threshold = q2.number_input(
            "Seuil de décision standard", min_value=0.0, max_value=1.0,
            value=current.prediction_threshold,
            help=(
                "Seuil de probabilité utilisé comme référence standard pour convertir une probabilité en "
                "décision binaire lorsqu’aucun seuil calibré spécifique n’est appliqué."
            ),
        )
        evaluate_holdout = q3.checkbox(
            "Évaluer le holdout final", value=st.session_state.lab_evaluate_holdout,
            help=(
                "Active l’évaluation finale sur le jeu holdout, conservé hors des étapes de sélection "
                "précédentes afin de mesurer la performance hors échantillon."
            ),
        )

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
                "Nombre minimal de signaux générés au total, toutes fenêtres de calibration "
                "confondues, pour qu’un seuil admissible soit considéré comme suffisamment robuste. "
                "Ce critère ne remplace pas la règle « Signaux minimaux par fenêtre × Fraction "
                "minimale de fenêtres » : un seuil doit d’abord respecter la couverture temporelle "
                "requise. `RobustSample` sert ensuite à privilégier les seuils disposant d’un "
                "échantillon global suffisant parmi ceux déjà admissibles. "
                "Exemple : avec 7 fenêtres, 5 signaux minimum par fenêtre et 60 % de fenêtres "
                "requises, il faut au moins 5 fenêtres conformes, donc au moins 25 signaux "
                "répartis dans le temps. Un seuil avec 25+ signaux au total mais mal répartis "
                "peut quand même être rejeté."
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

        st.subheader("Échantillonnage et reproductibilité")
        sampling_1, sampling_2 = st.columns(2)
        combinations = sampling_1.number_input(
            "Combinaisons par cible (calibrations)", min_value=1,
            value=st.session_state.lab_combinations_per_target,
        )
        seed = sampling_2.number_input("Seed", min_value=0, value=current.xgb_seed)

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

        st.subheader("Exécution")
        execution_1, execution_2, execution_3 = st.columns(3)
        workers = execution_1.number_input(
            "Workers marché", min_value=1, value=current.market_cache_workers
        )
        combination_workers = execution_2.number_input(
            "Workers combinaisons", min_value=1, value=current.combination_workers
        )
        nthread = execution_3.number_input(
            "Threads XGBoost", min_value=1, value=current.xgb_nthread
        )
        max_jobs = execution_1.number_input(
            "Jobs lourds concurrents", min_value=1,
            value=st.session_state.max_concurrent_heavy_jobs,
        )
        if st.button("Enregistrer les paramètres", type="primary"):
            parsed_quantiles = tuple(
                float(item.strip()) for item in quantiles.split(",") if item.strip()
            )
            new_config = replace(
                current,
                **prefilter_xgboost_values,
                **prefilter_temporal_values,
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
    configuration_path = results / "run_configuration.json"
    if configuration_path.is_file():
        run_configuration = json.loads(configuration_path.read_text(encoding="utf-8"))
        if run_configuration.get("final_holdout_evaluated") is False:
            st.caption(
                "Holdout final WF non calculé : score et rang composites indisponibles. "
                "Tri par AUC médiane des fenêtres Walk-forward."
            )
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
        column_config=_grid_column_help_config(combinations.columns, _WF_COMBINATION_COLUMN_HELP),
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
        f"threshold-sort-{run_id}": DEFAULT_PROMOTION_SORT,
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
        DEFAULT_PROMOTION_SORT, "Précision holdout", "AUC holdout",
        "Rendement directionnel moyen",
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
    chosen = _render_qualification_decision_grid(
        filtered, key=f"threshold-results-{run_id}", project_root=project_root,
        threshold_run_id=run_id,
        walk_forward_run_id=configuration.get("source_walk_forward_run"),
        pre_sorted=True,
        column_config=_grid_column_help_config(filtered.columns, _THRESHOLD_RESULT_COLUMN_HELP),
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
            column_config=_grid_column_help_config(sensitivity_summary.columns, _THRESHOLD_SUMMARY_COLUMN_HELP, {
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
            }),
        )
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
            column_config=_grid_column_help_config(choice_diagnostics.columns, _THRESHOLD_CHOICE_COLUMN_HELP, {
                "Seuil": st.column_config.NumberColumn(format="%.4f"),
                "Fraction de fenêtres admissibles": st.column_config.NumberColumn(format="percent"),
                "Précision calibration": st.column_config.NumberColumn(format="percent"),
                "Stabilité précision": st.column_config.NumberColumn(format="percent"),
                "Rendement directionnel moyen": st.column_config.NumberColumn(format="percent"),
                "Stabilité rendement": st.column_config.NumberColumn(format="percent"),
                "Fréquence mouvement opposé": st.column_config.NumberColumn(format="percent"),
                "F1": st.column_config.NumberColumn(format="%.3f"),
            }),
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
        column_config=_grid_column_help_config(sensitivity.columns, {
            **_THRESHOLD_SENSITIVITY_COLUMN_HELP,
            sensitivity.columns[-1]: "Marque le seuil de meilleure précision parmi ceux qui respectent le minimum de signaux affiché. Cible : un choix robuste, pas un maximum sur un faible échantillon.",
        }, {
            "Seuil": st.column_config.NumberColumn(format="%.4f"),
            "Précision": st.column_config.NumberColumn(format="percent"),
            "Recall": st.column_config.NumberColumn(format="percent"),
            "F1": st.column_config.NumberColumn(format="%.3f"),
            "Rendement directionnel moyen": st.column_config.NumberColumn(format="percent"),
            "Rendement médian": st.column_config.NumberColumn(format="percent"),
            "Fréquence mouvement opposé": st.column_config.NumberColumn(format="percent"),
            "MFE moyen": st.column_config.NumberColumn(format="percent"),
            "MAE moyen": st.column_config.NumberColumn(format="percent"),
        }),
    )


def _render_resume_controls(
    run_id: str,
    status: dict[str, object],
    detail: dict[str, object],
) -> None:
    if status.get("job_type") == JobType.FORWARD_SIMULATION.value:
        diagnosis = _service().forward_recovery_diagnosis(run_id)
        st.caption(diagnosis.message)
        if diagnosis.recoverable and st.button(
            "Reprendre cette simulation", key=f"resume-forward-{run_id}", width="stretch"
        ):
            try:
                _service().resume_forward_simulation(run_id)
            except (OSError, ValueError, RuntimeError) as resume_error:
                st.error(f"Reprise impossible : {resume_error}")
            else:
                st.success(
                    "Même run repris avec sa configuration et son snapshot historiques."
                )
                st.rerun()
        return
    resumable_types = {
        JobType.WALK_FORWARD.value,
        JobType.PREDICTOR_PREFILTER.value,
        JobType.THRESHOLD_PARAMETER_CALIBRATION.value,
        JobType.END_TO_END.value,
        JobType.OPERATIONAL_RUN.value,
    }
    if status.get("job_type") not in resumable_types or status.get("status") not in {
        "failed", "cancelled", "interrupted"
    }:
        return
    manifest = detail.get("checkpoint")
    error = detail.get("checkpoint_error")
    checkpointed_resume = status.get("job_type") in {
        JobType.WALK_FORWARD.value, JobType.PREDICTOR_PREFILTER.value,
    }
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
    is_operational_run = status.get("job_type") == JobType.OPERATIONAL_RUN.value
    actions = st.columns(2)
    if actions[0].button(
        "Relancer le run" if is_operational_run else "Reprendre le run",
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
    if not is_operational_run and actions[1].button(
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


def _render_upstream_qualification_diagnostic(
    decision: Mapping[str, object], *, project_root: Path,
    threshold_run_id: str | None, walk_forward_run_id: str | None,
) -> None:
    evidence = upstream_diagnostic(
        project_root, decision,
        threshold_run_id=threshold_run_id,
        walk_forward_run_id=walk_forward_run_id,
    )
    counts = evidence["signal_counts_by_window"]
    count_text = "—" if counts is None else json.dumps(counts, ensure_ascii=False)
    with st.container(border=True):
        st.markdown("**Qualité amont — Walk-forward / calibration**")
        fields = (
            ("AUC médiane WF", _format_metric(evidence["wf_median_auc"]), None),
            ("Pire AUC WF", _format_metric(evidence["wf_worst_auc"]),
             "Plus faible AUC observée parmi les fenêtres Walk-forward."),
            ("Dispersion AUC", _format_metric(evidence["wf_auc_std"]),
             "Écart-type des AUC entre les fenêtres Walk-forward; plus faible indique une meilleure stabilité."),
            ("Signaux par fenêtre", count_text, None),
            ("Total signaux calibration", _model_detail_integer(evidence["calibration_total_signals"]), None),
            ("Précision calibration", _format_metric(evidence["calibration_precision"], percent=True),
             "Précision du seuil sélectionné sur les fenêtres de calibration."),
            ("Rendement directionnel moyen calibration",
             _format_metric(evidence["calibration_directional_return_mean"], percent=True),
             "Rendement moyen des signaux de calibration dans la direction prédite."),
        )
        for offset in (0, 4):
            columns = st.columns((1, 1, 1, 2) if offset == 0 else 3, gap="small")
            for column, (label, value, help_text) in zip(columns, fields[offset:offset + 4]):
                column.metric(label, value, help=help_text)


def _render_qualification_decision_grid(
    rows: pd.DataFrame, *, key: str, project_root: Path,
    qualification: Mapping[str, object] | None = None,
    threshold_run_id: str | None = None,
    walk_forward_run_id: str | None = None,
    pre_sorted: bool = False,
    column_config: Mapping[str, object] | None = None,
) -> dict[str, object] | None:
    """Shared decision grid for standalone, pipeline and legacy promotion views."""

    table = rows if pre_sorted else sort_promotion_decisions(rows)
    selection = st.dataframe(
        table, hide_index=True, width="stretch", on_select="rerun",
        selection_mode="single-row", key=key, column_config=column_config,
    )
    selected_rows = _selected_rows(selection, len(table))
    if not selected_rows:
        return None
    chosen = table.iloc[selected_rows[0]].to_dict()
    source = qualification or {}
    _render_upstream_qualification_diagnostic(
        chosen, project_root=project_root,
        threshold_run_id=(threshold_run_id or source.get("source_threshold_calibration_run")),
        walk_forward_run_id=(walk_forward_run_id or source.get("source_walk_forward_run")),
    )
    return chosen


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
        column_config=_grid_column_help_config(table.columns, _XGBOOST_SELECTION_COLUMN_HELP),
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
        column_config=_grid_column_help_config(table.columns, _THRESHOLD_PARAMETER_COLUMN_HELP, {
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
        }),
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
        column_config=_grid_column_help_config(
            table.drop(columns=["Eligible", "Holdout confirmé", *detail_only], errors="ignore").columns,
            _WF_COMBINATION_COLUMN_HELP,
        ),
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
            "resources": lambda: _render_run_resources(run_id),
            "configuration": lambda: _render_run_configuration(detail),
            "files": lambda: _render_run_files(detail),
            "logs": lambda: _render_run_logs(detail),
        },
        key=f"run-technical-{run_id}",
    )


def _render_run_resources(run_id: str) -> None:
    """Display observed resource counters from the versioned run artifact."""
    root = st.session_state.lab_config.project_root / "runs" / run_id / "telemetry"
    document = _read_light_json(root / "resource_summary.json")
    if not isinstance(document, dict) or document.get("schema_version") != 1:
        st.info("Télémétrie indisponible pour ce run")
        return
    attempts = document.get("attempts")
    if not isinstance(attempts, list) or not attempts:
        st.info("Télémétrie indisponible pour ce run")
        return
    attempt_number = len(attempts)
    if len(attempts) > 1:
        attempt_number = st.selectbox(
            "Tentative", range(1, len(attempts) + 1), index=len(attempts) - 1,
            key=f"resources-attempt-{run_id}",
        )
    attempt = attempts[attempt_number - 1]
    if not isinstance(attempt, dict):
        st.info("Télémétrie indisponible pour ce run")
        return

    def cpu_text(cores: object) -> str:
        logical = attempt.get("logical_processors")
        if not isinstance(cores, (int, float)) or not isinstance(logical, int) or logical < 1:
            return "—"
        return f"{cores:.1f} / {logical} cœurs — {100 * cores / logical:.0f} %"

    def memory_text(value: object) -> str:
        return "—" if not isinstance(value, (int, float)) else f"{value / 2**30:.2f} Gio"

    waits = attempt.get("wait_seconds") or {}
    wait_total = sum(float(value) for value in waits.values()) if isinstance(waits, dict) else 0.0
    st.caption(
        f"Tentative {attempt_number} / {len(attempts)} · {attempt.get('status', '—')} · "
        "CPU et mémoire mesurés sur le processus du run et ses enfants observés. "
        "La durée inclut l’attente initiale des verrous. "
        "Le RSS additionné peut inclure des pages partagées."
    )
    summary = st.columns(7)
    summary[0].metric("Durée", _duration(attempt.get("elapsed_seconds")))
    summary[1].metric("CPU moyen", cpu_text(attempt.get("cpu_mean_cores")))
    summary[2].metric("CPU max échantillonné", cpu_text(attempt.get("cpu_max_sampled_cores")))
    summary[3].metric("Pic RSS simultané", memory_text(attempt.get("rss_peak_sampled_bytes")))
    summary[4].metric("Processus enfants max", attempt.get("max_child_processes_observed")
                      if attempt.get("max_child_processes_observed") is not None else "—")
    summary[5].metric("Workers CPU actifs max",
                      attempt.get("max_cpu_active_children_observed")
                      if attempt.get("max_cpu_active_children_observed") is not None else "—")
    summary[6].metric("Attente verrous", f"{wait_total:.1f} s")
    configuration = attempt.get("configuration") or {}
    if isinstance(configuration, dict):
        st.caption(
            "Configuré : " + " · ".join(
                f"{name}={configuration[name]}" for name in (
                    "combination_workers", "market_cache_workers", "xgb_nthread",
                    "walk_forward_batch_size", "walk_forward_max_combinations_per_batch",
                ) if name in configuration
            )
        )
    st.caption(f"Couverture CPU mesurée : {attempt.get('cpu_covered_seconds', 0):.1f} s. "
               f"Enfants sortis entre relevés : {attempt.get('exited_children_between_samples', 0)}. "
               "Un tiret signifie que la mesure n’est pas disponible.")
    checkpoint = attempt.get("checkpoint")
    if isinstance(checkpoint, dict):
        st.caption(
            f"Checkpoints : {checkpoint.get('attempt_count', '—')} essai(s), "
            f"{checkpoint.get('resume_count', '—')} reprise(s)."
        )
    if attempt.get("error"):
        st.caption(f"Erreur : {attempt['error']}")

    execution_seconds = max(0.0, float(attempt.get("elapsed_seconds") or 0) - wait_total)
    phase_rows = []
    for phase in attempt.get("phase_rows", []):
        if not isinstance(phase, dict):
            continue
        duration = phase.get("duration_seconds")
        count = phase.get("completed_items")
        throughput = (
            f"{count / duration:.2f} {phase.get('item_kind')}/s"
            if isinstance(count, int) and isinstance(duration, (int, float)) and duration > 0
            else "—"
        )
        phase_rows.append({
            "Phase": phase.get("name"),
            "Durée (s)": duration,
            "% du run": None if duration is None or execution_seconds <= 0 else 100 * duration / execution_seconds,
            "CPU moyen": cpu_text(phase.get("cpu_mean_cores")),
            "CPU max": cpu_text(phase.get("cpu_max_sampled_cores")),
            "Pic RSS": memory_text(phase.get("rss_peak_sampled_bytes")),
            "Processus enfants": phase.get("max_child_processes_observed"),
            "Workers CPU actifs": phase.get("max_cpu_active_children_observed"),
            "Débit": throughput,
            "Attente (s)": None,
        })
    selected_phase = None
    if phase_rows:
        phase_table = st.dataframe(
            pd.DataFrame(phase_rows), hide_index=True, width="stretch",
            key=f"resources-phases-{run_id}", on_select="rerun",
            selection_mode="single-row",
            column_config={
                "Phase": st.column_config.TextColumn(help="Phase du workflow persistée par le run."),
                "Durée (s)": st.column_config.NumberColumn(help="Temps monotone réel entre début et fin de la phase."),
                "% du run": st.column_config.NumberColumn(help="Durée de la phase divisée par la durée d’exécution hors attente initiale."),
                "CPU moyen": st.column_config.TextColumn(help="Temps CPU mesuré par seconde couverte, en cœurs et en pourcentage des processeurs logiques."),
                "CPU max": st.column_config.TextColumn(help="Maximum parmi les intervalles CPU échantillonnés; ce n’est pas un pic continu."),
                "Pic RSS": st.column_config.TextColumn(help="Maximum échantillonné de la somme simultanée des RSS parent et enfants observés."),
                "Processus enfants": st.column_config.NumberColumn(help="Maximum de processus enfants présents; ce n’est pas un nombre de tâches occupées."),
                "Workers CPU actifs": st.column_config.NumberColumn(help="Maximum échantillonné d’enfants dont le compteur CPU a progressé. Un worker en attente I/O n’est pas compté."),
                "Débit": st.column_config.TextColumn(help="Items réellement terminés divisés par la durée de la phase; l’unité est indiquée."),
                "Attente (s)": st.column_config.NumberColumn(help="Attente mesurée dans cette phase, si disponible; le heavy slot initial figure dans le résumé."),
            },
        )
        selected_rows = getattr(getattr(phase_table, "selection", None), "rows", [])
        if selected_rows:
            selected_phase = phase_rows[selected_rows[0]]["Phase"]
            selected_data = attempt["phase_rows"][selected_rows[0]]
            st.caption(f"Phase sélectionnée : {selected_phase}")
            if selected_data.get("details"):
                with st.expander("Paramètres et compteurs de la phase"):
                    st.json(selected_data["details"])
    else:
        st.caption("Aucune phase terminée mesurée pour cette tentative.")

    aggregation_phase = next(
        (phase for phase in reversed(attempt.get("phase_rows", []))
         if isinstance(phase, dict) and phase.get("name") == "aggregation"
         and isinstance(phase.get("details"), dict)
         and isinstance(phase["details"].get("subphases"), list)),
        None,
    )
    if aggregation_phase is not None:
        details = aggregation_phase["details"]

        def volume_text(value: object) -> str:
            if not isinstance(value, (int, float)):
                return "—"
            return (f"{value / 2**30:.2f} Gio" if value >= 2**30
                    else f"{value / 2**20:.1f} Mio")

        labels = {
            "load_batch": "Chargement des lots",
            "local_classification_windows": "Classification et fenêtres locales",
            "aggregate_risk": "Risque local",
            "sqlite_insertion": "Insertion SQLite",
            "progress_reporting": "Suivi des lots",
            "sql_global_metrics": "Métriques SQL globales",
            "assembly_finalization": "Assemblage et finalisation",
        }
        subphase_rows = []
        for row in details["subphases"]:
            if not isinstance(row, dict) or row.get("name") not in labels:
                continue
            duration = row.get("duration_seconds")
            cpu_seconds = row.get("cpu_seconds_parent")
            cpu_mean = (
                cpu_seconds / duration
                if isinstance(cpu_seconds, (int, float))
                and isinstance(duration, (int, float)) and duration > 0 else None
            )
            count, unit = next(
                ((row[key], label) for key, label in (
                    ("batches", "lots"), ("combinations", "combinaisons"),
                    ("prediction_rows", "lignes"),
                ) if isinstance(row.get(key), int)),
                (None, None),
            )
            throughput = (
                f"{count / duration:.1f} {unit}/s"
                if isinstance(count, int) and unit
                and isinstance(duration, (int, float)) and duration > 0 else "—"
            )
            subphase_rows.append({
                "Sous-phase": labels[row["name"]],
                "Durée (s)": duration,
                "CPU moyen parent": "—" if cpu_mean is None else f"{cpu_mean:.2f} cœur",
                "CPU moyen total échantillonné": (
                    "—" if not isinstance(row.get("cpu_mean_sampled_cores"), (int, float))
                    else f"{row['cpu_mean_sampled_cores']:.2f} cœurs"
                ),
                "CPU max échantillonné": (
                    "—" if not isinstance(row.get("cpu_max_sampled_cores"), (int, float))
                    else f"{row['cpu_max_sampled_cores']:.2f} cœurs"
                ),
                "RSS parent observé": memory_text(row.get("rss_peak_observed_parent_bytes")),
                "RSS total max échantillonné": memory_text(row.get("rss_peak_sampled_bytes")),
                "Traités": f"{count} {unit}" if unit else "—",
                "Débit": throughput,
                "Volume lu estimé": volume_text(row.get("estimated_read_bytes")),
                "Taille du fichier produit": volume_text(
                    row.get("sqlite_database_bytes") or row.get("aggregation_checkpoint_bytes")
                ),
            })
        if subphase_rows:
            st.subheader("Détail de la phase aggregation")
            st.caption(
                f"{details.get('batches', '—')} lots · "
                f"{details.get('combinations_processed', '—')} combinaisons · "
                f"{details.get('window_evaluations', '—')} évaluations de fenêtres · "
                f"{details.get('prediction_rows', '—')} lignes de prédictions. "
                "CPU et RSS des sous-phases : processus parent seulement. "
                "Pour Risque local, CPU moyen/max et RSS total échantillonnés incluent les workers et le parent pendant leur activité. "
                "Risque local chevauche le chargement, les métriques locales et l'insertion SQLite; les durées des sous-phases ne s'additionnent donc pas. "
                "RSS relevé à la fin des opérations; les pics intermédiaires peuvent échapper à la mesure. "
                "Lecture estimée : deux passages du payload par lot (vérification puis chargement). "
                "La taille des fichiers ne mesure pas les octets physiques écrits sur disque. "
                "L'assemblage inclut l'écriture du checkpoint, exclue du compteur historique aggregation_seconds."
            )
            st.dataframe(pd.DataFrame(subphase_rows), hide_index=True, width="stretch")

    samples = []
    path = root / "samples.jsonl"
    if path.is_file():
        for line in path.read_text(encoding="utf-8").splitlines():
            try:
                sample = json.loads(line)
            except json.JSONDecodeError:
                continue  # A crash may leave one final, incomplete JSONL line.
            if sample.get("attempt_id") == attempt.get("attempt_id"):
                samples.append(sample)
    if samples:
        timeline = pd.DataFrame(samples)
        if len(timeline) > 600:
            timeline = timeline.iloc[::max(1, len(timeline) // 600)]
        timeline = timeline.set_index("elapsed_seconds")
        if timeline["cpu_cores"].notna().any():
            st.subheader("CPU dans le temps")
            st.line_chart(timeline[["cpu_cores"]], x_label="Secondes", y_label="Cœurs occupés")
        if timeline["total_rss_bytes"].notna().any():
            st.subheader("Mémoire dans le temps")
            st.line_chart(timeline[["total_rss_bytes"]] / 2**30,
                          x_label="Secondes", y_label="RSS parent + enfants (Gio)")

    batch_path = root / "batches.jsonl"
    if batch_path.is_file():
        batches = []
        for line in batch_path.read_text(encoding="utf-8").splitlines():
            try:
                batch = json.loads(line)
            except json.JSONDecodeError:
                continue
            if batch.get("attempt_id") == attempt.get("attempt_id"):
                batches.append(batch)
        if selected_phase is not None:
            batches = [batch for batch in batches if batch.get("phase") == selected_phase]
        if batches:
            with st.expander(f"Détail des batchs ({len(batches)})"):
                batch_frame = pd.DataFrame(batches)
                durations = pd.to_numeric(batch_frame["duration_seconds"], errors="coerce").dropna()
                if not durations.empty:
                    st.caption(
                        f"Durée par batch : médiane {durations.median():.2f} s · "
                        f"P95 {durations.quantile(0.95):.2f} s"
                    )
                st.dataframe(batch_frame[[
                    "phase", "batch_id", "completed_items", "duration_seconds",
                    "calculation_seconds", "rows",
                ]], hide_index=True, width="stretch")


def _render_standard_results(
    run_id: str, job_type: JobType, status: dict[str, object], detail: dict[str, object]
) -> None:
    if job_type is JobType.PREDICTOR_PREFILTER:
        result_dir = st.session_state.lab_config.project_root / "runs" / run_id / "results"
        result_path = result_dir / "predictor_prefilter.csv"
        if result_path.is_file():
            st.subheader("Classement et sélection des prédicteurs")
            ranking = pd.read_csv(result_path)
            selected_target = "Toutes"
            if "AggregateRank" in ranking.columns:
                targets = sorted(ranking["Observation"].dropna().astype(str).unique())
                selected_target = st.selectbox(
                    "Cible", ["Toutes", *targets],
                    key=f"prefilter-results-target-{run_id}",
                )
                if selected_target != "Toutes":
                    ranking = ranking.loc[ranking["Observation"] == selected_target]
            st.dataframe(ranking, hide_index=True, width="stretch")
            origins_path = result_dir / "predictor_prefilter_origins.csv"
            if origins_path.is_file():
                with st.expander("Métriques par origine"):
                    origin_rows = pd.read_csv(origins_path)
                    if selected_target != "Toutes":
                        origin_rows = origin_rows.loc[
                            origin_rows["Observation"] == selected_target
                        ]
                    st.dataframe(origin_rows, hide_index=True, width="stretch")
                    st.download_button(
                        "Exporter les métriques par origine (CSV)",
                        data=origins_path.read_bytes(),
                        file_name=f"{run_id}_predictor_prefilter_origins.csv",
                        mime="text/csv", key=f"prefilter-origins-download-{run_id}",
                    )
        else:
            st.info("Classement indisponible tant que le préfiltre n'est pas terminé.")
        manifest = _read_light_json(result_dir / "predictor_prefilter.json")
        if manifest is not None:
            if manifest.get("prefilter_method") == "temporal_consensus":
                st.metric("Candidats consensus retenus", manifest["retained_predictors"])
                st.caption(f"Origines : {', '.join(manifest['origin_cutoffs'])} · minimum {manifest['min_occurrences']} occurrences")
                st.dataframe(pd.DataFrame([{"Occurrences": key, "Candidats": count}
                                          for key, count in manifest["occurrence_distribution"].items()]),
                             hide_index=True, width="stretch")
                consensus_path = result_dir / "temporal_consensus_candidates.csv"
                if consensus_path.is_file():
                    st.download_button("Exporter le consensus (CSV)", consensus_path.read_bytes(),
                                       file_name=f"{run_id}_temporal_consensus_candidates.csv", mime="text/csv")
            with st.expander("Provenance et diagnostic"):
                st.json(manifest)
        return
    if job_type is JobType.FORWARD_SIMULATION:
        source = detail.get("configuration", {}).get("source_end_to_end_run")
        if source:
            st.caption(f"End-to-End source : {source}")
            if st.button("Ouvrir l’End-to-End source", key=f"forward-source-{run_id}"):
                st.session_state["selected-run-id"] = str(source)
                st.rerun()
        summary = detail.get("summary", {})
        if summary.get("result") == "skipped_no_models":
            st.info("Aucun modèle admissible — Forward Simulation non exécutée.")
            st.metric("Modèles à T0", 0, help=_FORWARD_COLUMN_HELP["Modèles à T0"])
            st.json(summary)
            return
        has_quality_counters = "evaluated_observations" in summary
        if not has_quality_counters:
            st.json(detail["summary"])
            return
        evaluated = int(summary.get("evaluated_observations", 0) or 0)
        skipped = int(summary.get("skipped_observations", 0) or 0)
        quality = summary.get("evaluability_rate")
        kpis = st.columns(4)
        kpis[0].metric("Observations évaluées", evaluated)
        kpis[1].metric("Observations exclues", skipped)
        kpis[2].metric(
            "Taux d’observations évaluables",
            "—" if quality is None else f"{quality:.1%}",
        )
        kpis[3].metric(
            "Anomalies de données uniques",
            int(summary.get("unique_data_quality_issues", 0) or 0),
        )
        if skipped:
            st.info(
                "Certaines observations n’ont pas pu être évaluées en raison de "
                "données de marché incomplètes. Elles sont exclues des statistiques."
            )
            exclusions_path = (
                st.session_state.lab_config.project_root / "runs" / run_id / "results"
                / "forward_exclusions.csv"
            )
            if exclusions_path.is_file():
                exclusions = pd.read_csv(exclusions_path)
                columns = [
                    "target", "source_model_id", "Set", "direction", "session_date",
                    "as_of_date", "exclusion_reason", "invalid_fields",
                ]
                st.caption(
                    "Observations exclues — affichage limité aux 500 premières lignes."
                    if len(exclusions) > 500 else "Observations exclues"
                )
                st.dataframe(exclusions.loc[:, columns].head(500), hide_index=True, width="stretch")
        _render_forward_temporal_results(run_id, summary)
        return
    if job_type is JobType.XGBOOST_CALIBRATION:
        _render_xgboost_calibration_selection(run_id)
    elif job_type is JobType.THRESHOLD_PARAMETER_CALIBRATION:
        _render_threshold_parameter_calibration_selection(run_id)
    elif job_type is JobType.HOLDOUT_EVALUATION and status.get("status") == "completed":
        result_dir = st.session_state.lab_config.project_root / "runs" / run_id / "results"
        configuration = _read_light_json(result_dir / "run_configuration.json")
        for filename, label in (("holdout_metrics.csv", "Métriques holdout"),
                                ("holdout_predictions.csv", "Prédictions et observations holdout")):
            path = result_dir / filename
            if path.is_file():
                st.subheader(label)
                st.dataframe(pd.read_csv(path), hide_index=True, width="stretch")
        if configuration is not None:
            with st.expander("Protocole et provenance holdout"):
                st.json(configuration)
    elif job_type is JobType.PROMOTION_QUALIFICATION and status.get("status") == "completed":
        result_dir = st.session_state.lab_config.project_root / "runs" / run_id / "results"
        qualification = _read_light_json(result_dir / "qualification.json")
        if qualification is not None:
            st.subheader("Décision de qualification")
            _render_qualification_decision_grid(
                pd.DataFrame(qualification.get("decisions", [])),
                key=f"qualification-decisions-{run_id}",
                project_root=st.session_state.lab_config.project_root,
                qualification=qualification,
            )
            with st.expander("Protocole et provenance de qualification"):
                st.json({key: value for key, value in qualification.items() if key != "decisions"})
    elif (
        job_type is JobType.THRESHOLD_CALIBRATION
        and status.get("status") == "completed"
        and (st.session_state.lab_config.project_root / "runs" / run_id
             / "results" / "holdout_metrics.csv").is_file()
    ):
        _render_threshold_calibration_promotion(
            run_id,
            project_root=st.session_state.lab_config.project_root,
            configuration=detail["configuration"],
        )
    if job_type is JobType.THRESHOLD_CALIBRATION and status.get("status") == "completed":
        result_dir = st.session_state.lab_config.project_root / "runs" / run_id / "results"
        metrics_path = result_dir / "threshold_metrics_by_set.csv"
        if metrics_path.is_file() and not (result_dir / "holdout_metrics.csv").is_file():
            st.subheader("Seuils figés et diagnostics de calibration")
            metrics = pd.read_csv(metrics_path)
            if "Set" in metrics:
                metrics = metrics[["Set", *[column for column in metrics if column != "Set"]]]
            st.dataframe(metrics, hide_index=True, width="stretch")
            selected = _read_light_json(result_dir / "selected_thresholds_by_set.json")
            if selected is not None:
                st.json(selected)
    st.json(detail["summary"])


_FORWARD_COLUMN_HELP = {
    "Modèle": "Identifiant du modèle figé dans le snapshot de l’End-to-End source.",
    "Target": "Symbole cible prédit par ce modèle.",
    "Horizon": "Nombre de séances XNYS écoulées depuis le cutoff T0.",
    "Période": "Depuis T0 : de la première séance Forward au checkpoint. Intervalle : uniquement les séances indiquées.",
    "Séances": "Première et dernière séances XNYS incluses dans cette mesure.",
    "Évaluables": "Observations modèle × séance avec prédiction et résultat exploitables, avec ou sans signal.",
    "Exclues": "Observations modèle × séance non évaluables pour données manquantes ou invalides ; elles ne contribuent pas aux performances.",
    "Signaux": "Nombre de signaux évaluables produits sur la période indiquée ; zéro est distinct de données exclues.",
    "Signaux cumulés": "Nombre de signaux évaluables depuis T0 jusqu’au checkpoint.",
    "Signaux intervalle": "Nombre de signaux évaluables uniquement dans l’intervalle indiqué.",
    "Précision": "Part des signaux évaluables corrects sur la période indiquée, en %. Sans signal : indisponible.",
    "Précision cumulative": "Part des signaux évaluables corrects depuis T0 jusqu’au checkpoint, en %. Sans signal : indisponible.",
    "Précision intervalle": "Part des signaux évaluables corrects uniquement dans l’intervalle, en %. Sans signal : indisponible.",
    "Rendement moyen": "Moyenne des rendements directionnels par signal évaluable sur la période, en %. Sans signal : indisponible.",
    "Rendement moyen intervalle": "Moyenne des rendements directionnels par signal évaluable dans cet intervalle, en %. Sans signal : indisponible.",
    "P&L": "Somme des rendements directionnels des signaux × 10 000 $ par signal sur la période ; valeur théorique, sans frais.",
    "P&L cumulatif": "Somme depuis T0 des rendements directionnels des signaux × 10 000 $ par signal ; valeur théorique, sans frais.",
    "Drawdown": "Plus forte baisse du P&L agrégé par séance depuis un sommet antérieur, en $, sur la période indiquée.",
    "Modèles à T0": "Nombre de modèles présents dans le snapshot figé de l’E2E source ; ce dénominateur reste constant.",
    "Modèles observables": "Modèles avec au moins une observation évaluable dans cet intervalle.",
    "Modèles contributeurs": "Modèles avec au moins un signal évaluable dans cet intervalle ; eux seuls entrent dans les statistiques par modèle.",
    "Précision médiane": "Médiane des précisions par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "Rendement médian": "Médiane des rendements moyens par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "Précision pondérée": "Part correcte de tous les signaux évaluables de la population dans l’intervalle, en %. Les modèles actifs pèsent davantage.",
    "Rendement pondéré": "Rendement directionnel moyen de tous les signaux évaluables de la population dans l’intervalle, en %.",
    "Précision Q25": "Premier quartile des précisions par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "Précision Q75": "Troisième quartile des précisions par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "Rendement Q25": "Premier quartile des rendements moyens par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "Rendement Q75": "Troisième quartile des rendements moyens par modèle contributeur dans cet intervalle, en %. Chaque modèle compte une fois.",
    "P&L cumulatif modèle": "P&L théorique de ce modèle depuis T0 : somme des rendements de ses signaux × 10 000 $, en dollars.",
}


def _forward_column_config(columns: pd.Index) -> dict[str, Any]:
    percentages = {
        "Précision", "Précision cumulative", "Précision intervalle",
        "Rendement moyen", "Rendement moyen intervalle", "Précision médiane",
        "Rendement médian", "Précision pondérée", "Rendement pondéré",
        "Précision Q25", "Précision Q75", "Rendement Q25", "Rendement Q75",
    }
    currency = {"P&L", "P&L cumulatif", "P&L cumulatif modèle", "Drawdown"}
    formats = {
        name: st.column_config.NumberColumn(format="percent")
        for name in columns if name in percentages
    }
    formats.update({
        name: st.column_config.NumberColumn(format="%.2f $")
        for name in columns if name in currency
    })
    return _grid_column_help_config(columns, _FORWARD_COLUMN_HELP, formats)


def _forward_metric_table(rows: pd.DataFrame, *, label: str) -> pd.DataFrame:
    table = rows.copy()
    table["Horizon"] = table["horizon"].map(lambda value: f"+{int(value)}")
    table["Séances"] = table["session_start"].astype(str) + " → " + table["session_end"].astype(str)
    table = table.rename(columns={
        "evaluated_observations": "Évaluables", "excluded_observations": "Exclues",
        "signals": "Signaux", "precision": "Précision", "mean_return": "Rendement moyen",
        "pnl": "P&L", "drawdown": "Drawdown",
    })
    st.subheader(label)
    return table[["Horizon", "Séances", "Évaluables", "Exclues", "Signaux",
                  "Précision", "Rendement moyen", "P&L", "Drawdown"]]


def _forward_line_chart(
    frame: pd.DataFrame, *, x: str, y: str, title: str,
    percent: bool = False, checkpoints: Sequence[int] = (),
) -> None:
    chart = alt.Chart(frame).mark_line(point=True).encode(
        x=alt.X(x, title="Séances depuis T0"),
        y=alt.Y(y, title=title, axis=alt.Axis(format="%" if percent else ",.0f")),
        tooltip=[x, alt.Tooltip(y, format=".1%" if percent else ",.2f")],
    )
    if checkpoints:
        marks = alt.Chart(pd.DataFrame({x: list(checkpoints)})).mark_rule(
            color="#aaaaaa", strokeDash=[3, 3]
        ).encode(x=x)
        chart = chart + marks
    st.altair_chart(chart, width="stretch")


def _forward_precision_chart(rows: pd.DataFrame, checkpoints: Sequence[int]) -> None:
    values = rows.loc[rows["precision"].notna(),
                      ["horizon", "period_kind", "precision", "signals"]].copy()
    if values.empty:
        return
    values["Lecture"] = values["period_kind"].map({
        "cumulative": "Depuis T0", "interval": "Intervalle",
    })
    chart = alt.Chart(values).mark_line(point=True).encode(
        x=alt.X("horizon:Q", title="Séances depuis T0"),
        y=alt.Y("precision:Q", title="Précision", axis=alt.Axis(format="%")),
        color=alt.Color("Lecture:N"),
        tooltip=["horizon:Q", "Lecture:N", alt.Tooltip("precision:Q", format=".1%"), "signals:Q"],
    )
    marks = alt.Chart(pd.DataFrame({"horizon": list(checkpoints)})).mark_rule(
        color="#aaaaaa", strokeDash=[3, 3]
    ).encode(x="horizon:Q")
    st.altair_chart(chart + marks, width="stretch")


def _render_forward_temporal_results(run_id: str, summary: Mapping[str, object]) -> None:
    root = st.session_state.lab_config.project_root / "runs" / run_id / "results"
    manifest = _read_light_json(root / "forward_analysis_manifest.json")
    if manifest is None:
        st.info("Analyse temporelle détaillée indisponible pour ce run.")
        return
    required = (
        "forward_period_metrics.csv", "forward_population_metrics.csv",
        "forward_daily_metrics.csv",
    )
    if manifest.get("forward_run_id") != run_id or any(
        not (root / name).is_file()
        or hashlib.sha256((root / name).read_bytes()).hexdigest()
        != manifest.get("artifact_digests", {}).get(name)
        for name in required
    ):
        st.error("Les artefacts d’analyse Forward sont absents ou ont changé.")
        return
    periods = pd.read_csv(root / required[0]).fillna({"source_model_id": ""})
    population = pd.read_csv(root / required[1])
    daily = pd.read_csv(root / required[2]).fillna({"source_model_id": ""})
    view = st.radio(
        "Analyse Forward", ("Synthèse", "Modèles", "Évolution population"),
        horizontal=True, key=f"forward-view-{run_id}",
    )
    checkpoints = tuple(manifest.get("checkpoints", ()))
    if view == "Synthèse":
        st.metric("Modèles à T0", int(manifest.get("model_count_t0", 0)),
                  help=_FORWARD_COLUMN_HELP["Modèles à T0"])
        st.metric("Signaux évaluables", int(summary.get("total_signals", 0) or 0),
                  help=_FORWARD_COLUMN_HELP["Signaux"])
        full_run = periods.loc[(periods["scope"] == "run") &
                               (periods["period_kind"] == "full_run")]
        if not full_run.empty:
            final_table = _forward_metric_table(full_run, label="Résultat à la date réelle de fin")
            st.dataframe(final_table, hide_index=True, width="stretch",
                         column_config=_forward_column_config(final_table.columns))
        run_daily = daily.loc[daily["scope"] == "run"]
        if not run_daily.empty:
            _forward_line_chart(run_daily, x="horizon", y="cumulative_pnl",
                                title="P&L cumulatif ($)", checkpoints=checkpoints)
        _forward_precision_chart(periods.loc[periods["scope"] == "run"], checkpoints)
        intervals = periods.loc[(periods["scope"] == "run") & (periods["period_kind"] == "interval")]
        if not intervals.empty:
            _forward_line_chart(intervals.dropna(subset=["mean_return"]), x="horizon",
                                y="mean_return", title="Rendement moyen par intervalle",
                                percent=True, checkpoints=checkpoints)
        kind = st.radio("Lecture des checkpoints", ("Depuis T0", "Par intervalle"),
                        horizontal=True, key=f"forward-period-{run_id}")
        selected = periods.loc[(periods["scope"] == "run") &
                               (periods["period_kind"] == ("cumulative" if kind == "Depuis T0" else "interval"))]
        if selected.empty:
            st.info("Aucun checkpoint standard entièrement atteint ; le résultat global reste disponible dans le résumé du run.")
        else:
            table = _forward_metric_table(selected, label="Métriques aux checkpoints")
            st.dataframe(table, hide_index=True, width="stretch",
                         column_config=_forward_column_config(table.columns))
    elif view == "Modèles":
        latest = periods.loc[(periods["scope"] == "model") &
                             (periods["period_kind"] == "interval")]
        full_run = periods.loc[(periods["scope"] == "model") &
                               (periods["period_kind"] == "full_run")]
        if full_run.empty:
            st.info("Aucun modèle dans le snapshot source.")
            return
        latest = latest.sort_values("horizon").groupby("source_model_id", as_index=False).tail(1)
        if not latest.empty:
            last_horizon = int(latest["horizon"].max())
            last_start = int(latest.loc[latest["horizon"] == last_horizon, "interval_start"].iloc[0])
            st.caption(f"Colonnes « intervalle » : séances +{last_start} à +{last_horizon}. Les totaux couvrent toute la Forward.")
        pnl = dict(zip(full_run["source_model_id"], full_run["pnl"]))
        signal_totals = dict(zip(full_run["source_model_id"], full_run["signals"]))
        last_precision = dict(zip(latest["source_model_id"], latest["precision"]))
        last_return = dict(zip(latest["source_model_id"], latest["mean_return"]))
        grid = pd.DataFrame({
            "Modèle": full_run["source_model_id"], "Target": full_run["target"],
            "Signaux cumulés": full_run["source_model_id"].map(signal_totals),
            "Précision intervalle": full_run["source_model_id"].map(last_precision),
            "Rendement moyen intervalle": full_run["source_model_id"].map(last_return),
            "P&L cumulatif modèle": full_run["source_model_id"].map(pnl),
        }).reset_index(drop=True)
        event = st.dataframe(
            grid, hide_index=True, width="stretch", on_select="rerun",
            selection_mode="single-row", key=f"forward-model-select-{run_id}",
            column_config=_forward_column_config(grid.columns),
        )
        selected_rows = event.selection.rows if event is not None else []
        if not selected_rows:
            st.caption("Sélectionnez un modèle pour voir son évolution.")
            return
        model_id = str(grid.iloc[selected_rows[0]]["Modèle"])
        model_periods = periods.loc[(periods["scope"] == "model") &
                                    (periods["source_model_id"] == model_id)]
        model_daily = daily.loc[(daily["scope"] == "model") &
                                (daily["source_model_id"] == model_id)]
        st.subheader(f"Modèle {model_id}")
        source_file = (st.session_state.lab_config.project_root / "runs" /
                       str(manifest["source_e2e_run_id"]) / "results" /
                       "forward_model_snapshot.json")
        source_identity = ""
        if source_file.is_file() and hashlib.sha256(source_file.read_bytes()).hexdigest() == manifest.get("source_snapshot_sha256"):
            source_snapshot = _read_light_json(source_file) or {}
            matching = [item for item in source_snapshot.get("models", [])
                        if str(item.get("source_model_id")) == model_id]
            if len(matching) == 1:
                model = matching[0]
                source_identity = f" · Combinaison : {model.get('set')} · Direction : {model.get('direction')}"
        policy = "Figé" if manifest.get("forward_policy") == "FROZEN" else str(manifest.get("forward_policy"))
        st.caption(f"Target : {grid.iloc[selected_rows[0]]['Target']} · Mode : {policy} · Horizon maximal : +{manifest['horizon_max']} séances · E2E source : {manifest['source_e2e_run_id']}{source_identity}")
        _forward_line_chart(model_daily, x="horizon", y="cumulative_pnl",
                            title="P&L cumulatif ($)", checkpoints=checkpoints)
        _forward_line_chart(model_periods.loc[model_periods["period_kind"] == "interval"].dropna(subset=["precision"]),
                            x="horizon", y="precision", title="Précision par intervalle",
                            percent=True, checkpoints=checkpoints)
        kind = st.radio("Lecture du modèle", ("Par intervalle", "Depuis T0"),
                        horizontal=True, key=f"forward-model-period-{run_id}")
        chosen = model_periods.loc[model_periods["period_kind"] ==
                                   ("interval" if kind == "Par intervalle" else "cumulative")]
        if chosen.empty:
            st.info("Aucun checkpoint standard entièrement atteint ; le résultat global du modèle reste disponible dans la grille.")
        else:
            table = _forward_metric_table(chosen, label="Métriques par horizon")
            st.dataframe(table, hide_index=True, width="stretch",
                         column_config=_forward_column_config(table.columns))
    else:
        if population.empty:
            st.info("Aucun intervalle standard entièrement atteint.")
            return
        table = population.rename(columns={
            "models_t0": "Modèles à T0", "models_with_observations": "Modèles observables",
            "models_with_signals": "Modèles contributeurs", "signals": "Signaux intervalle",
            "median_model_precision": "Précision médiane",
            "median_model_return": "Rendement médian",
            "weighted_precision": "Précision pondérée",
            "weighted_return": "Rendement pondéré",
            "precision_q25": "Précision Q25", "precision_q75": "Précision Q75",
            "return_q25": "Rendement Q25", "return_q75": "Rendement Q75",
        })
        table["Horizon"] = table["horizon"].map(lambda value: f"+{int(value)}")
        table["Séances"] = table["session_start"].astype(str) + " → " + table["session_end"].astype(str)
        columns = ["Horizon", "Séances", "Modèles à T0", "Modèles observables",
                   "Modèles contributeurs", "Signaux intervalle", "Précision médiane",
                   "Précision Q25", "Précision Q75", "Rendement médian",
                   "Rendement Q25", "Rendement Q75",
                   "Précision pondérée", "Rendement pondéré"]
        st.dataframe(table[columns], hide_index=True, width="stretch",
                     column_config=_forward_column_config(pd.Index(columns)))
        _forward_line_chart(population.dropna(subset=["median_model_precision"]),
                            x="horizon", y="median_model_precision",
                            title="Précision médiane des modèles par intervalle",
                            percent=True, checkpoints=checkpoints)
        _forward_line_chart(population.dropna(subset=["median_model_return"]),
                            x="horizon", y="median_model_return",
                            title="Rendement médian des modèles par intervalle",
                            percent=True, checkpoints=checkpoints)
        st.caption("Les médianes comptent chaque modèle contributeur une fois ; les valeurs pondérées comptent chaque signal. Les modèles sans signal restent dans la population T0.")


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
            "resources": lambda: _render_run_resources(run_id),
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
    metrics[2].metric(
        "Confirmees holdout",
        analytics.confirmed_count if analytics.confirmed_count is not None else "—",
    )
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
        st.dataframe(
            prefilter_table, hide_index=True, width="stretch",
            column_config=_grid_column_help_config(prefilter_table.columns, _PREFILTER_COLUMN_HELP),
        )
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
    st.dataframe(
        validation, hide_index=True, width="stretch",
        column_config=_grid_column_help_config(validation.columns, _WF_VALIDATION_COLUMN_HELP),
    )


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
        column_config=_grid_column_help_config(table.columns, _WF_BATCH_COLUMN_HELP, {
            "Progression": st.column_config.ProgressColumn(
                min_value=0.0, max_value=100.0, format="%.1f%%"
            )
        }),
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
            "resources": lambda: _render_run_resources(run_id),
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


def _render_derived_creation(
    run_id: str, detail: dict[str, object], service: ExperimentService,
) -> None:
    configuration = detail.get("configuration", {})
    if not isinstance(configuration, Mapping):
        return
    derivation = configuration.get("derivation")
    if isinstance(derivation, Mapping):
        st.caption(f"Dérivé de {derivation.get('source_end_to_end_run_id', '—')}")
        if derivation.get("source_temporal_validation_enabled") is True:
            st.warning(
                "La validation temporelle du run source n’a été ni héritée ni "
                "rejouée : ce dérivé n’est pas temporellement revalidé."
            )
        return
    status = detail.get("status", {})
    if not isinstance(status, Mapping) or status.get("status") != "completed":
        return
    if configuration.get("forced_symbol_sets") is not None:
        return
    if configuration.get("temporal_validation_enabled"):
        st.warning(
            "La validation temporelle de ce run ne sera ni héritée ni rejouée "
            "dans le dérivé. Le nouveau run ne sera pas temporellement revalidé."
        )
    key = f"derive-open-{run_id}"
    if st.button("Créer une expérience dérivée", key=f"derive-button-{run_id}"):
        st.session_state[key] = True
    if not st.session_state.get(key):
        return
    repository = service.run_service.repository
    source_spec = repository.load_spec(run_id)
    source_manifest = load_pipeline_manifest(repository, run_id)
    if source_manifest is None or source_manifest.get("schema_version") not in {1, 3, 5}:
        st.error("Le manifest source ne permet pas cette dérivation.")
        return
    labels = {
        "walk_forward": "Walk-forward / préfiltre",
        "xgboost_calibration": "Calibration XGBoost",
        "threshold_parameter_calibration": "Calibration paramètres de seuil",
        "threshold_calibration": "Calibration des seuils",
    }
    labels.update({"holdout_evaluation": "Évaluation holdout",
                   "promotion_qualification": "Qualification promotion"})
    split = source_manifest["schema_version"] in {3, 5}
    separated = source_manifest["schema_version"] == 5
    labels["prefilter"] = "Préfiltre"
    if separated:
        labels["walk_forward"] = "Walk-forward"
    fork_keys = PREFILTER_SCIENTIFIC_STAGE_KEYS if separated else SPLIT_FORK_STAGE_KEYS if split else FORK_STAGE_KEYS
    parameter_fields = PREFILTER_STAGE_PARAMETER_FIELDS if separated else SPLIT_STAGE_PARAMETER_FIELDS if split else STAGE_PARAMETER_FIELDS
    fork = st.selectbox(
        "Point de dérivation", fork_keys,
        format_func=lambda value: labels[value], key=f"derive-fork-{run_id}",
    )
    if fork == "walk_forward":
        st.caption(
            "Snapshot préparé et cutoff hérités et figés depuis le child "
            "Walk-forward parent. Aucun téléchargement ni nouvelle préparation marché. "
            f"Session source : {source_manifest.get('prepared_dataset_as_of') or 'vérifiée dans le snapshot'}."
        )
    forward_enabled = st.checkbox(
        "Lancer une Forward Simulation pour cette expérience dérivée",
        value=False, key=f"derive-forward-{run_id}",
    )
    modes = stage_modes(fork, forward_enabled=forward_enabled,
                        schema_version=3 if separated else 2 if split else 1)
    st.caption("Amont hérité : " + ", ".join(
        labels.get(stage, "Walk-forward")
        for stage in ("walk_forward", *fork_keys)
        if modes[stage] == "inherited"
    ))
    st.caption("À recalculer : " + ", ".join(
        labels[stage] for stage in fork_keys
        if modes[stage] == "recomputed"
    ))
    if forward_enabled:
        st.caption("À recalculer : Forward.")
        st.caption("Non exécutée : promotion automatique.")
    else:
        st.caption("Non exécutées : Forward, promotion automatique.")
    fields = [
        field for stage in fork_keys
        if modes[stage] == "recomputed"
        for field in sorted(parameter_fields.get(stage, ()))
    ]
    changes: dict[str, object] = {}
    with st.container(border=True):
        if forward_enabled:
            original_mode = source_parameter_value(
                repository, source_spec, source_manifest, "forward_simulation_mode"
            )
            forward_modes = ("63_sessions", "126_sessions", "custom_end_date")
            selected_mode = st.selectbox(
                "Fenêtre Forward", forward_modes,
                index=(forward_modes.index(original_mode)
                       if original_mode in forward_modes else 0),
                key=f"derive-forward-mode-{run_id}-{fork}",
            )
            if selected_mode != original_mode:
                changes["forward_simulation_mode"] = selected_mode
            if selected_mode == "custom_end_date":
                original_end = source_parameter_value(
                    repository, source_spec, source_manifest,
                    "forward_simulation_end_date",
                )
                default_end = (
                    pd.Timestamp(original_end).date() if original_end
                    else pd.Timestamp(source_manifest["prepared_dataset_as_of"]).date()
                    + timedelta(days=90)
                )
                selected_end = st.date_input(
                    "Date de fin Forward", value=default_end,
                    key=f"derive-forward-end-{run_id}-{fork}",
                ).isoformat()
                if selected_end != original_end:
                    changes["forward_simulation_end_date"] = selected_end
        for field in fields:
            try:
                original = source_parameter_value(
                    repository, source_spec, source_manifest, field
                )
            except (OSError, ValueError, KeyError) as error:
                st.error(f"Valeur source indisponible pour {field} : {error}")
                return
            if field == "prefilter_method":
                modes = ("single_origin", "temporal_stability", "temporal_consensus")
                value = st.selectbox("Mode de sélection", modes, index=modes.index(original),
                                     key=f"derive-value-{run_id}-{fork}-{field}")
            elif isinstance(original, bool):
                value = st.checkbox(field, value=original, key=f"derive-value-{run_id}-{fork}-{field}")
            elif isinstance(original, int):
                value = st.number_input(field, value=original, step=1, key=f"derive-value-{run_id}-{fork}-{field}")
            elif isinstance(original, float):
                value = st.number_input(field, value=original, format="%.6f", key=f"derive-value-{run_id}-{fork}-{field}")
            else:
                raw = st.text_input(field, value=json.dumps(original), key=f"derive-value-{run_id}-{fork}-{field}")
                try:
                    value = json.loads(raw)
                except json.JSONDecodeError:
                    value = raw
            if value != original:
                changes[field] = value
        st.write("Paramètres modifiés :")
        st.write(", ".join(
            f"{field} : {source_parameter_value(repository, source_spec, source_manifest, field)} → {value}"
            for field, value in changes.items()
        ) or "Aucun")
        submitted = st.button(
            "Lancer l’expérience dérivée", disabled=not changes, key=f"derive-submit-{run_id}-{fork}"
        )
    if submitted:
        try:
            result = service.create_derived(
                run_id, fork, changes, forward_enabled=forward_enabled
            )
        except (OSError, ValueError, RuntimeError, TypeError) as error:
            st.error(f"Dérivation impossible : {error}")
        else:
            st.session_state[key] = False
            st.success(f"End-to-End dérivé créé : {result.run_id}")


def _render_prefilter_derived_creation(
    run_id: str, detail: dict[str, object], service: ExperimentService,
) -> None:
    configuration = detail.get("configuration", {})
    if not isinstance(configuration, Mapping):
        return
    provenance = configuration.get("prefilter_derivation")
    if isinstance(provenance, Mapping):
        st.caption(
            "Dérivé du préfiltre " + str(provenance.get("source_run_id", "—"))
            + " · snapshot SHA-256 "
            + str(provenance.get("prepared_snapshot_sha256", "—"))
        )
    status = detail.get("status", {})
    if not isinstance(status, Mapping) or status.get("status") != "completed":
        return
    key = f"prefilter-derive-open-{run_id}"
    if st.button("Créer une expérience dérivée", key=f"prefilter-derive-button-{run_id}"):
        st.session_state[key] = True
    if not st.session_state.get(key):
        return
    source = service.run_service.repository.load_spec(run_id)
    summary = detail.get("summary", {})
    cutoff = summary.get("prepared_dataset_as_of", "—") if isinstance(summary, Mapping) else "—"
    digest = (
        summary.get("traceability", {}).get("prepared_dataset_sha256", "—")
        if isinstance(summary, Mapping) else "—"
    )
    st.caption(
        f"Snapshot préparé, cutoff {cutoff} et digest {digest} hérités et figés "
        "depuis ce run. Les origines consensus existantes sont réutilisées ; "
        "les origines supplémentaires nécessitent leur propre snapshot historique."
    )
    labels = {
        "predictor_prefilter_top_n": "Nombre de prédicteurs retenus (Top-N)",
        "predictor_prefilter_min_median_auc": "Médiane AUC minimale",
        "predictor_prefilter_min_pct_above_random": "Proportion minimale de fenêtres AUC > 0,50",
        "predictor_prefilter_min_worst_auc": "Pire AUC minimale",
        "predictor_prefilter_max_auc_std": "Écart-type AUC maximal",
        "predictor_prefilter_correlation_threshold": "Seuil de corrélation / redondance",
        "prefilter_xgb_max_depth": "max_depth",
        "prefilter_xgb_eta": "eta",
        "prefilter_xgb_num_boost_round": "num_boost_round",
        "prefilter_xgb_min_child_weight": "min_child_weight",
        "prefilter_xgb_gamma": "gamma",
        "prefilter_xgb_subsample": "subsample",
        "prefilter_xgb_colsample_bytree": "colsample_bytree",
        "prefilter_xgb_reg_alpha": "reg_alpha",
        "prefilter_xgb_reg_lambda": "reg_lambda",
        "prefilter_xgb_seed": "seed",
    }
    changes: dict[str, object] = {}
    with st.container(border=True):
        method_values = ("single_origin", "temporal_stability", "temporal_consensus")
        selected_method = st.selectbox(
            "Méthode de préfiltre", method_values,
            index=method_values.index(source.prefilter_method),
            format_func=lambda value: {
                "single_origin": "Origine unique",
                "temporal_stability": "Stabilité temporelle",
                "temporal_consensus": "Consensus temporel",
            }[value], key=f"prefilter-derive-{run_id}-method",
        )
        if selected_method != source.prefilter_method:
            changes["prefilter_method"] = selected_method
        if selected_method == "temporal_stability":
            origin_count = int(st.number_input(
                "Nombre d'origines", min_value=1,
                value=source.stability_origin_count, step=1,
                key=f"prefilter-derive-{run_id}-origin-count",
            ))
            step_sessions = int(st.number_input(
                "Pas en séances", min_value=1,
                value=source.stability_step_sessions, step=1,
                key=f"prefilter-derive-{run_id}-step-sessions",
            ))
            if origin_count != source.stability_origin_count:
                changes["stability_origin_count"] = origin_count
            if step_sessions != source.stability_step_sessions:
                changes["stability_step_sessions"] = step_sessions
        if selected_method == "temporal_consensus":
            for field, label in (("temporal_consensus_origins", "Nombre d'origines"),
                                 ("temporal_consensus_step_sessions", "Espacement en séances"),
                                 ("temporal_consensus_min_occurrences", "Occurrences minimales")):
                original = getattr(source.config, field)
                value = int(st.number_input(label, min_value=1, value=original, step=1,
                                            key=f"prefilter-derive-{run_id}-{field}"))
                if value != original:
                    changes[field] = value
        for field in sorted(PREFILTER_DERIVATION_FIELDS - set(PREFILTER_XGBOOST_FIELDS) - {"temporal_consensus_origins", "temporal_consensus_step_sessions", "temporal_consensus_min_occurrences"}):
            original = getattr(source.config, field)
            if field == "predictor_prefilter_top_n":
                value = int(st.number_input(
                    labels[field], min_value=1, value=int(original), step=1,
                    key=f"prefilter-derive-{run_id}-{field}",
                ))
            else:
                value = float(st.number_input(
                    labels[field], min_value=0.0, max_value=1.0,
                    value=float(original), format="%.4f",
                    key=f"prefilter-derive-{run_id}-{field}",
                ))
            if value != original:
                changes[field] = value
        st.subheader("XGBoost du préfiltre")
        xgb_columns = st.columns(3)
        for index, field in enumerate(PREFILTER_XGBOOST_FIELDS):
            original = getattr(source.config, field)
            if field in {"prefilter_xgb_max_depth", "prefilter_xgb_num_boost_round", "prefilter_xgb_seed"}:
                value = int(xgb_columns[index % 3].number_input(
                    labels[field], min_value=0 if field == "prefilter_xgb_seed" else 1,
                    value=int(original), step=1,
                    key=f"prefilter-derive-{run_id}-{field}",
                ))
            else:
                bounds = {"min_value": 0.0}
                if field in {"prefilter_xgb_subsample", "prefilter_xgb_colsample_bytree"}:
                    bounds["max_value"] = 1.0
                value = float(xgb_columns[index % 3].number_input(
                    labels[field], value=float(original), format="%.4f",
                    key=f"prefilter-derive-{run_id}-{field}", **bounds,
                ))
            if value != original:
                changes[field] = value
        submitted = st.button(
            "Lancer le préfiltre dérivé", disabled=not changes,
            key=f"prefilter-derive-submit-{run_id}",
        )
    if submitted:
        try:
            result = service.create_derived(run_id, "predictor_prefilter", changes)
        except (OSError, ValueError, RuntimeError, TypeError) as error:
            st.error(f"Dérivation impossible : {error}")
        else:
            st.session_state[key] = False
            st.success(f"Préfiltre dérivé créé : {result.run_id}")


def _render_walk_forward_derived_creation(
    run_id: str, detail: dict[str, object], service: ExperimentService,
) -> None:
    status = detail.get("status", {})
    storage = detail.get("storage", {})
    if (not isinstance(status, Mapping) or status.get("status") != "completed"
            or isinstance(storage, Mapping) and storage.get("state") == "purged"):
        return
    key = f"wf-derive-open-{run_id}"
    if st.button("Créer une expérience dérivée", key=f"wf-derive-button-{run_id}"):
        st.session_state[key] = True
    if not st.session_state.get(key):
        return
    source = service.run_service.repository.load_spec(run_id)
    summary = detail.get("summary", {})
    trace = summary.get("traceability", {}) if isinstance(summary, Mapping) else {}
    as_of = trace.get("prepared_market_last_date", "N/D") if isinstance(trace, Mapping) else "N/D"
    st.caption(
        f"Hérité et figé : snapshot préparé, cutoff demandé "
        f"{source.requested_historical_cutoff or 'N/D'}, séance {as_of}, "
        "candidats/combinaisons d'entrée, préfiltre et offset "
        f"{source.config.walk_forward_end_offset_sessions}. "
        "Recalculé : Walk-forward, qualification, holdout interne s'il est activé, "
        "et classement. Aucune promotion ni Forward Simulation automatique."
    )
    labels = {
        "walk_forward_window_mode": "Mode de fenêtre",
        "walk_forward_min_train_size": "Train minimal (expansive)",
        "walk_forward_train_size": "Train fixe (glissante)",
        "walk_forward_test_size": "Taille du test",
        "walk_forward_step_size": "Pas des fenêtres",
        "final_holdout_size": "Taille du holdout final",
        "xgb_rounds": "num_boost_round",
        "qualification_min_median_auc": "Médiane AUC minimale",
        "qualification_min_pct_windows_above_random": "Proportion minimale de fenêtres AUC > 0,50",
        "qualification_min_worst_window_auc": "Worst AUC minimal",
        "qualification_min_windows": "Nombre minimal de fenêtres",
        "qualification_min_positive_observations": "Nombre minimal de positifs",
        "qualification_max_auc_std": "Std AUC maximal",
        "final_confirmation_min_auc": "AUC minimale de confirmation holdout",
        "prediction_threshold": "Seuil de prédiction binaire",
    }
    changes: dict[str, object] = {}

    def field_input(field: str, widget: object) -> None:
        original = getattr(source.config, field)
        label = labels.get(field, field.removeprefix("xgb_").removeprefix("model_selection_"))
        widget_key = f"wf-derive-{run_id}-{field}"
        if field == "walk_forward_window_mode":
            modes = ("expanding", "rolling")
            value = widget.selectbox(label, modes, index=modes.index(original), key=widget_key)
        elif field in {
            "walk_forward_min_train_size", "walk_forward_train_size",
            "walk_forward_test_size", "walk_forward_step_size",
            "final_holdout_size", "xgb_max_depth", "xgb_rounds",
            "qualification_min_windows", "qualification_min_positive_observations",
            "xgb_seed",
        }:
            minimum = 0 if field in {
                "qualification_min_positive_observations", "xgb_seed",
            } else 1
            value = int(widget.number_input(
                label, min_value=minimum, value=int(original), step=1,
                key=widget_key,
            ))
        else:
            bounds = {"min_value": 0.0}
            if field in {
                "xgb_subsample", "xgb_colsample_bytree",
                "qualification_min_median_auc",
                "qualification_min_pct_windows_above_random",
                "qualification_min_worst_window_auc",
                "final_confirmation_min_auc", "prediction_threshold",
            }:
                bounds["max_value"] = 1.0
            value = float(widget.number_input(
                label, value=float(original), format="%.4f", key=widget_key,
                **bounds,
            ))
        if value != original:
            changes[field] = value

    with st.container(border=True):
        st.subheader("Géométrie Walk-forward")
        for field in WF_GEOMETRY_FIELDS:
            field_input(field, st)
        st.subheader("XGBoost")
        columns = st.columns(3)
        for index, field in enumerate(WF_XGBOOST_FIELDS):
            field_input(field, columns[index % 3])
        st.subheader("Qualification et classement")
        for field in (*WF_QUALIFICATION_FIELDS, *WF_SELECTION_FIELDS):
            field_input(field, st)
        holdout = st.checkbox(
            "Évaluer le holdout final", value=source.evaluate_final_holdout,
            key=f"wf-derive-{run_id}-evaluate_final_holdout",
        )
        if holdout != source.evaluate_final_holdout:
            changes["evaluate_final_holdout"] = holdout
        submitted = st.button(
            "Lancer le Walk-forward dérivé", disabled=not changes,
            key=f"wf-derive-submit-{run_id}",
        )
    if submitted:
        try:
            result = service.create_derived(run_id, "walk_forward", changes)
        except (OSError, ValueError, RuntimeError, TypeError) as error:
            st.error(f"Dérivation impossible : {error}")
        else:
            st.session_state[key] = False
            st.success(f"Walk-forward dérivé créé : {result.run_id}")


def _render_pipeline_summary(
    run_id: str, detail: dict[str, object], service: ExperimentService | None = None
) -> None:
    if service is not None:
        _render_derived_creation(run_id, detail, service)
    summary = detail.get("summary", {})
    protocol = summary.get("walk_forward_protocol") if isinstance(summary, Mapping) else None
    if isinstance(protocol, str) and protocol:
        st.caption(protocol)
    pipeline_summary = (
        _read_light_json(
            service.run_service.repository.run_directory(run_id)
            / "results" / "pipeline_summary.json"
        )
        if service is not None else None
    )
    snapshot_source = pipeline_summary if pipeline_summary is not None else summary
    snapshot = (
        snapshot_source.get("forward_model_snapshot")
        if isinstance(snapshot_source, Mapping) else None
    )
    if isinstance(snapshot, Mapping):
        st.caption(
            "Cutoff historique demandé : "
            f"{detail.get('configuration', {}).get('requested_historical_cutoff') or '—'} · "
            "séance résolue : "
            f"{snapshot.get('resolved_market_session_cutoff') or '—'}"
        )
        forward = summary.get("forward_simulation")
        if isinstance(forward, Mapping):
            forward_status = forward.get("status", "pending")
            forward_result = None
            child_id = forward.get("child_run_id")
            if child_id and service is not None:
                repository = service.run_service.repository
                forward_status = repository.status(str(child_id)).get("status", forward_status)
                forward_result = repository.summary(str(child_id)).get("result")
            st.caption(
                "Forward Simulation : "
                f"{forward_status} · run : {child_id or '—'}"
                + (f" · résultat : {forward_result}" if forward_result else "")
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
    if stage.get("mode") == "inherited":
        st.caption(f"Héritée du run source : {stage.get('source_run_id')}")
    effective_run_id = stage.get("source_run_id") if stage.get("mode") == "inherited" else child_run_id
    if not effective_run_id or (stage.get("status") == "reserved" and stage.get("mode") != "inherited"):
        st.caption(
            f"Étape {stage.get('status', 'pending')} - aucun artefact chargé."
        )
        return
    child_detail = service.run(str(effective_run_id))
    child_status = child_detail["status"]
    st.caption(
        f"Run : {effective_run_id} - statut : {child_status.get('status', '-')} · "
        f"début : {child_status.get('started_at') or '—'} · "
        f"fin : {child_status.get('finished_at') or '—'}"
    )
    _render_job_detail_tabs(
        service,
        str(effective_run_id),
        status=child_status,
        detail=child_detail,
    )
    _render_pipeline_child_technical(
        parent_run_id, stage_key, stage, child_detail, str(effective_run_id)
    )


def _render_pipeline_child_technical(
    parent_run_id: str, stage_key: str, stage: dict[str, object],
    child_detail: dict[str, object], effective_run_id: str,
) -> None:
    """Keep provenance and standalone navigation after the stage's results."""

    with st.expander("Provenance technique et navigation"):
        st.json({"provenance": child_detail.get("metadata"),
                 "artifact_digests": stage.get("artifact_digests")})
        if stage.get("mode") == "inherited" and st.button(
            "Ouvrir le run source", key=f"open-inherited-{parent_run_id}-{stage_key}"
        ):
            _history_navigation("detail", [str(stage["source_run_id"])])
        if st.button("Ouvrir le détail autonome", key=f"open-child-{parent_run_id}-{stage_key}"):
            _history_navigation("detail", [effective_run_id])


def _render_pipeline_promotion(detail: dict[str, object]) -> None:
    stage = pipeline_stage_by_key(detail.get("pipeline_stages"), "promotion")
    if stage is None:
        st.info("L'étape Promotion n'est pas encore disponible.")
        return
    trigger = stage.get("promotion_trigger")
    qualification_source: dict[str, object] = {}
    if isinstance(trigger, dict):
        qualification_id = trigger.get("promotion_qualification_run_id")
        if qualification_id:
            qualification_source = _read_light_json(
                st.session_state.lab_config.project_root / "runs" / str(qualification_id)
                / "results" / "qualification.json"
            ) or {}
            st.caption(
                f"Qualification retenue : {qualification_id} "
                f"({trigger.get('source_selection_reason')})"
            )
        if stage.get("status") == "blocked":
            reason = {
                "temporal_validation_not_passed": "validation temporelle non validée",
                "no_reference_candidates": "aucun candidat du run de référence",
            }.get(str(trigger.get("reason")), str(trigger.get("reason")))
            st.info(f"Promotion non déclenchée : {reason}.")
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
    _render_qualification_decision_grid(
        diagnostic_table,
        key=f"promotion-decisions-{stage.get('child_run_id') or detail.get('status', {}).get('run_id', 'root')}",
        project_root=st.session_state.lab_config.project_root,
        qualification=qualification_source,
        threshold_run_id=promotion.get("source_threshold_calibration_run"),
        walk_forward_run_id=promotion.get("source_walk_forward_run"),
    )


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
    if any(
        isinstance(item, Mapping) and item.get("canonical_combination_id") is None
        for group in ("common_candidates", "lost_candidates", "new_candidates")
        for item in stability.get(group, ())
    ):
        st.caption("Identité canonique historique indisponible pour certains candidats incomplets.")

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
        derivation = detail.get("configuration", {}).get("derivation")
        if isinstance(derivation, Mapping) and derivation.get("source_temporal_validation_enabled") is True:
            st.info(
                "La validation temporelle du run source n’a été ni héritée ni "
                "rejouée pour ce dérivé."
            )
        else:
            st.info("La validation temporelle n’a pas été activée pour cet End-to-end.")
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
        stability = comparison.get("candidate_identity_stability")
        if isinstance(stability, dict):
            comparison = dict(comparison)
            comparison["candidate_identity_stability"] = read_time_candidate_identity_stability(
                stability
            )
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
        if comparison is not None:
            with st.expander("Provenance technique et navigation"):
                st.json({
                    "parameters": comparison.get("parameters", {}),
                    "source_artifact_digests": comparison.get("source_artifact_digests", {}),
                })
        return
    child_detail = service.run(child_run_id)
    child_metadata = child_detail.get("metadata", {})
    _render_pipeline_summary(child_run_id, child_detail)
    with st.expander("Provenance technique et navigation"):
        if comparison is not None:
            st.json({
                "parameters": comparison.get("parameters", {}),
                "source_artifact_digests": comparison.get("source_artifact_digests", {}),
            })
        st.json(
            {
                "reference_run_id": child_metadata.get("reference_run_id"),
                "run_purpose": child_metadata.get("run_purpose"),
                "offset": child_detail["configuration"]["rstock_config"].get(
                    "walk_forward_end_offset_sessions"
                ),
            }
        )
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
        tuple(tab for tab in tabs_for_job(JobType.END_TO_END)
              if tab.renderer_key not in {
                  "child_holdout_evaluation", "child_promotion_qualification"
              } or pipeline_stage_by_key(
                  detail.get("pipeline_stages"), PIPELINE_CHILD_TABS[tab.renderer_key]
              ) is not None),
        {
            "summary": lambda: _render_pipeline_summary(run_id, detail, service),
            "resources": lambda: _render_run_resources(run_id),
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
            "child_promotion_qualification",
        )
    }
    render_lazy_tabs(
        st,
        tuple(tab for tab in tabs_for_job(JobType.FORCED_CANDIDATE_VALIDATION)
              if tab.renderer_key != "child_promotion_qualification"
              or pipeline_stage_by_key(detail.get("pipeline_stages"),
                                       "promotion_qualification") is not None),
        {
            "summary": lambda: _render_pipeline_summary(run_id, detail),
            "resources": lambda: _render_run_resources(run_id),
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
        _render_walk_forward_derived_creation(run_id, detail, service)
        _render_walk_forward_tabs(service, run_id, status, detail)
    elif job_type is JobType.PREDICTOR_PREFILTER:
        _render_prefilter_derived_creation(run_id, detail, service)
        _render_standard_job_tabs(run_id, job_type, status, detail)
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
        if status["job_type"] == JobType.FORWARD_SIMULATION.value:
            policy = detail.get("configuration", {}).get("forward_policy") or "FROZEN"
            st.caption("Mode : Figé" if policy == "FROZEN" else f"Mode : {policy}")
    _render_resume_controls(run_id, status, detail)
    _render_job_detail_tabs(service, run_id, status=status, detail=detail)
    return
def _render_end_to_end_comparison(run_ids: list[str]) -> None:
    analyses = [
        load_end_to_end_comparison(st.session_state.lab_config.project_root, run_id)
        for run_id in run_ids
    ]

    def count(value: object) -> str:
        return "—" if value is None else str(value)

    def number(value: object, digits: int = 3) -> str:
        return "—" if value is None else f"{float(value):.{digits}f}"

    def percent(value: float | None, digits: int = 1) -> str:
        return "—" if value is None else f"{value:.{digits}%}"

    def passage(current: int | None, previous: int | None) -> str:
        if current is None:
            return "—"
        return f"{current} ({percent(current / previous)})" if previous else str(current)

    st.subheader("Comparabilité")
    profiles = [item.scientific_profile or {} for item in analyses]
    protocol_groups = ("Univers", "Profondeur", "WF scientifique", "Qualification",
                       "Holdout", "Calibration et seuils", "Version scientifique")
    differing = [
        group for group in protocol_groups
        if any(profile.get(group) != profiles[0].get(group) for profile in profiles[1:])
    ]
    mixed_wf_holdout = len({item.wf_final_holdout_evaluated for item in analyses}) > 1
    if differing:
        st.warning("Protocoles scientifiques différents : " + ", ".join(differing) + ".")
    elif not all(item.comparability_complete for item in analyses):
        st.warning("Comparabilité scientifique partielle : une configuration historique ou le contrat holdout WF est indisponible.")
    else:
        st.success("Protocoles scientifiques comparables; les cutoffs PIT diffèrent selon les runs.")
    if mixed_wf_holdout:
        st.warning("Holdout final WF : contrats mixtes. « Confirmées holdout WF » et « Taux confirmées » ne sont pas comparables entre tous les runs.")
    comparison_rows = []
    for item, profile in zip(analyses, profiles):
        universe = profile.get("Univers") or {}
        version = profile.get("Version scientifique") or {}
        comparison_rows.append({
            "Run": item.run_id,
            "Cutoff": count(item.cutoff),
            "Univers": count(universe.get("primary_universe_id") if isinstance(universe, dict) else universe),
            "Profondeur": count(profile.get("Profondeur")),
            "WF / qualification / holdout / seuils": "Identiques" if not differing else
                ", ".join(group for group in differing if group not in ("Univers", "Profondeur", "Version scientifique")) or "Identiques",
            "Final holdout WF": ("Activé" if item.wf_final_holdout_evaluated else "Désactivé"
                                  if item.wf_final_holdout_evaluated is False else "—"),
            "Version": str(version.get("pipeline_version") or "—") if isinstance(version, dict) else "—",
            "Commit scientifique": item.scientific_commit[:10] if item.scientific_commit else "—",
            "Digest dataset": item.dataset_digest[:12] if item.dataset_digest else "—",
        })
    st.dataframe(pd.DataFrame(comparison_rows), hide_index=True, width="stretch")
    with st.expander("Paramètres scientifiques comparés"):
        details = []
        for group in protocol_groups:
            details.append({"Groupe": group, **{
                item.run_id: str(profile.get(group, "—"))
                for item, profile in zip(analyses, profiles)
            }})
        st.dataframe(pd.DataFrame(details), hide_index=True, width="stretch")
        st.caption("Les workers, tailles de lots et autres réglages d’exécution sont exclus de la comparabilité scientifique.")

    st.subheader("Funnel scientifique")
    funnel = [
        ("Brutes planifiées", lambda item: count(item.raw)),
        ("Évaluées WF", lambda item: passage(item.evaluated, item.raw)),
        ("Qualifiées WF", lambda item: passage(item.qualified, item.evaluated)),
        ("Entrées calibration / seuils", lambda item: count(item.calibration_entries)),
        ("Up évaluables holdout", lambda item: passage(item.up_evaluable, item.calibration_entries)),
        ("Passent qualification / promotion", lambda item: passage(item.qualification_passed, item.up_evaluable)),
        ("Passent validation temporelle (individuel)", lambda item:
            passage(item.temporal_passed, item.temporal_entering) if item.temporal_entering is not None else
            "Non calculé" if item.temporal_requested else "Non requise"),
        ("Candidats finaux", lambda item: count(item.final_candidates)),
    ]
    st.dataframe(pd.DataFrame([
        {"Étape": label, **{item.run_id: render(item) for item in analyses}}
        for label, render in funnel
    ]), hide_index=True, width="stretch")
    st.caption("Taux affichés uniquement quand le dénominateur représente la population de l’étape précédente. La validation temporelle globale peut bloquer le déclenchement opérationnel sans remplacer la qualification individuelle forcée.")
    st.dataframe(pd.DataFrame([{
        "Run": item.run_id,
        "Validation globale": item.temporal_status or ("Non calculé" if item.temporal_requested else "Non requise"),
        "Confirmées holdout WF": "Non calculé" if item.wf_final_holdout_evaluated is False else count(item.confirmed),
        "Taux confirmées": "Non calculé" if item.wf_final_holdout_evaluated is False else percent(item.confirmation_rate),
    } for item in analyses]), hide_index=True, width="stretch")

    st.subheader("Analyse des rejets de qualification")
    st.caption("Chaque combinaison rejetée compte une fois dans le total et dans 1 / 2 / 3+ critères. « Seul » signifie qu’aucun autre critère n’a échoué; ses bandes de proximité sont cumulatives et utilisent la politique du run. « Impliqué » inclut aussi les rejets multi-critères. Pour le rendement, pb signifie point de base (0,01 point de pourcentage). Une distance indisponible ne vaut pas zéro. La qualification inclut les combinaisons sans seuil ou métrique holdout, donc son total peut dépasser les Up évaluables.")
    core_reasons = (
        ("Précision", "Précision seule", "Précision impliquée"),
        ("AUC", "AUC seule", "AUC impliquée"),
        ("Signaux", "Signaux seuls", "Signaux impliqués"),
        ("Rendement", "Rendement seul", "Rendement impliqué"),
        ("Mouvement opposé", "Mouvement opposé seul", "Mouvement opposé impliqué"),
    )
    other_labels = {
        "Seuil": ("Seuil seul", "Seuil impliqué"),
        "Direction": ("Direction seule", "Direction impliquée"),
        "Walk-forward forcé": ("Walk-forward forcé seul", "Walk-forward forcé impliqué"),
        "Évaluation forcée": ("Évaluation forcée seule", "Évaluation forcée impliquée"),
        "Autre": ("Autre seul", "Autre impliqué"),
    }
    extra_reasons = sorted({
        category for item in analyses if item.rejection_analysis
        for category in item.rejection_analysis.involved
        if category not in {name for name, _, _ in core_reasons}
    })
    def single_with_distance(item: EndToEndComparison, category: str) -> str:
        analysis = item.rejection_analysis
        if analysis is None:
            return "—"
        total = analysis.single.get(category, 0)
        bands = analysis.proximity.get(category)
        if bands is None:
            return f"{total} (distance indisponible)" if total and category in {
                "Précision", "AUC", "Signaux", "Rendement", "Mouvement opposé"
            } else str(total)
        details = [f"{label}: {value}" for label, value in zip(bands.labels, bands.counts, strict=True)]
        if bands.unavailable:
            details.append(f"distance indisponible: {bands.unavailable}")
        return f"{total} ({' · '.join(details)})"

    rejection_rows = [
        ("Combinaisons rejetées", lambda item: count(item.rejection_analysis.rejected)
         if item.rejection_analysis else "—"),
        ("1 seul critère", lambda item: count(item.rejection_analysis.one_criterion)
         if item.rejection_analysis else "—"),
        ("2 critères", lambda item: count(item.rejection_analysis.two_criteria)
         if item.rejection_analysis else "—"),
        ("3 critères ou plus", lambda item: count(item.rejection_analysis.three_or_more_criteria)
         if item.rejection_analysis else "—"),
    ]
    reason_labels = (
        *core_reasons,
        *((name, *other_labels.get(name, (f"{name} seul", f"{name} impliqué")))
          for name in extra_reasons),
    )
    for category, alone_label, _ in reason_labels:
        rejection_rows.append((alone_label, lambda item, name=category: single_with_distance(item, name)))
    for category, _, involved_label in reason_labels:
        rejection_rows.append((involved_label, lambda item, name=category: count(
            item.rejection_analysis.involved.get(name, 0)) if item.rejection_analysis else "—"))
    st.dataframe(pd.DataFrame([{
        "Mesure": "Qualification source",
        **{item.run_id: item.rejection_qualification_source or "—" for item in analyses},
    }, *[
        {"Mesure": label, **{
            item.run_id: render(item)
            for item in analyses
        }} for label, render in rejection_rows
    ]]), hide_index=True, width="stretch")

    st.subheader("Holdout E2E — population évaluable")
    st.caption("Toutes les combinaisons Up évaluées avant la qualification. Rendements : moyenne et médiane des rendements directionnels moyens par modèle; n indique le nombre de modèles renseignés.")
    st.dataframe(pd.DataFrame([{
        "Run": item.run_id,
        "Up évaluables": count((item.holdout_population or {}).get("count")),
        "AUC médiane": number((item.holdout_population or {}).get("auc_median")),
        "Précision médiane": percent((item.holdout_population or {}).get("precision_median")),
        "Rendement directionnel moyen": (
            f"{percent(item.holdout_population['return_mean'], 2)} (n={item.holdout_population['return_models']})"
            if item.holdout_population and item.holdout_population.get("return_mean") is not None else "—"),
        "Rendement directionnel médian": percent((item.holdout_population or {}).get("return_median"), 2),
        "Signaux médians / modèle": number((item.holdout_population or {}).get("signals_median"), 1),
    } for item in analyses]), hide_index=True, width="stretch")

    st.subheader("Candidats finaux")
    st.caption("Qualité des candidats individuellement qualifiés; après revalidation forcée lorsqu’elle existe.")
    st.dataframe(pd.DataFrame([{
        "Run": item.run_id,
        "Candidats": count(item.final_candidates),
        "Candidate yield / WF évaluées": percent(
            item.final_candidates / item.evaluated if item.final_candidates is not None and item.evaluated else None, 3),
        "AUC médiane": number((item.candidate_population or {}).get("auc_median")),
        "Précision médiane": percent((item.candidate_population or {}).get("precision_median")),
        "Rendement médian": percent((item.candidate_population or {}).get("return_median"), 2),
        "Signaux candidats": count((item.candidate_population or {}).get("signals_total")),
    } for item in analyses]), hide_index=True, width="stretch")

    chosen = st.selectbox("Ouvrir le détail d’un run", run_ids, key="comparison-open-run")
    if st.button("Ouvrir le run sélectionné", key="comparison-open-detail"):
        _history_navigation("detail", [chosen])


def _render_prefilter_comparison(run_ids: list[str]) -> None:
    root = st.session_state.lab_config.project_root
    items = [load_prefilter_comparison(root, run_id) for run_id in run_ids]
    st.caption(
        "Combinaisons = paires cible/prédicteur distinctes. En stabilité temporelle, "
        "admissible signifie admissible sur au moins une origine; "
        "la sélection finale suit le classement agrégé, le Top N et la corrélation. "
        "En consensus, les occurrences internes comptent les sélections finales par origine ; "
        "le Jaccard compare les populations finales entre runs."
    )
    st.subheader("Paramètres et volumes du préfiltre")
    summary_grid = pd.DataFrame([item.display_row() for item in items])
    summary_grid["_worst_sort"] = pd.to_numeric(
        summary_grid["Worst AUC min"], errors="coerce",
    )
    summary_grid = summary_grid.sort_values(
        ["Cutoff résolu", "_worst_sort", "Run ID"], na_position="last",
        kind="stable",
    ).drop(columns="_worst_sort")
    st.dataframe(
        summary_grid,
        hide_index=True, width="stretch",
    )
    profiles = {
        item.run_id: {
            "Profil / preset": item.profile if item.profile is not None else ND,
            "Méthode": item.method if item.method is not None else ND,
            "Univers": item.universe if item.universe is not None else ND,
            **item.settings,
        } for item in items
    }
    fields = sorted(set().union(*(values.keys() for values in profiles.values())))
    differences = prefilter_profile_differences(items)
    if differences:
        st.warning("Profils ou paramètres différents : " + ", ".join(differences) + ".")
        st.dataframe(pd.DataFrame([
            {"Paramètre": field, **values}
            for field, values in differences.items()
        ]), hide_index=True, width="stretch")
    else:
        st.caption("Aucune différence de profil ou de paramètres enregistrés.")
    with st.expander("Tous les paramètres de préfiltre enregistrés"):
        st.dataframe(pd.DataFrame([
            {"Paramètre": field, **{
                run_id: profiles[run_id].get(field, ND) for run_id in run_ids
            }} for field in fields
        ]), hide_index=True, width="stretch")

    overlap = compare_prefilter_candidates(items)
    st.subheader("Population retenue")
    metrics = st.columns(2)
    metrics[0].metric(
        "Candidats présents dans tous les runs",
        ND if overlap.common is None else len(overlap.common),
    )
    metrics[1].metric(
        "Recouvrement intersection / union",
        ND if overlap.overlap_rate is None else f"{overlap.overlap_rate:.1%}",
    )
    st.dataframe(pd.DataFrame([{
        "Run ID": item.run_id,
        "Candidats retenus": item.retained if item.retained is not None else ND,
        "Propres à ce run": (
            len(overlap.own[item.run_id]) if overlap.own is not None else ND
        ),
    } for item in items]), hide_index=True, width="stretch")
    if overlap.union is not None:
        candidate_rows = []
        for identity in sorted(overlap.union):
            target, direction, predictor = json.loads(identity)
            candidate_rows.append({
                "Candidat canonique": identity,
                "Cible": target, "Prédicteur": predictor,
                "Dans tous les runs": identity in (overlap.common or ()),
                **{
                    run_id: identity in (item.candidates or ())
                    for run_id, item in zip(run_ids, items, strict=True)
                },
            })
        st.dataframe(pd.DataFrame(candidate_rows), hide_index=True, width="stretch")
    else:
        st.info("Recouvrement indisponible : la sélection finale manque pour au moins un run.")
    st.download_button(
        "Exporter la comparaison Préfiltre (CSV)",
        data=prefilter_comparison_csv(items),
        file_name="comparaison_prefiltre.csv", mime="text/csv",
        key="prefilter-comparison-export",
    )
    chosen = st.selectbox("Ouvrir le détail d’un run", run_ids, key="comparison-open-run")
    if st.button("Ouvrir le run sélectionné", key="comparison-open-detail"):
        _history_navigation("detail", [chosen])


def _render_run_comparison_view(service: ExperimentService, run_ids: list[str]) -> None:
    types = [str(service.run_service.repository.status(run_id)["job_type"]) for run_id in run_ids]
    mode = comparison_types(types)
    if mode is None:
        _page_header("Historique")
        st.error("Sélectionnez de 2 à 6 runs du même type : Walk-forward, End-to-End ou Préfiltre.")
        return
    if mode == JobType.END_TO_END.value:
        _page_header("Historique")
        st.caption("Historique > Comparaison de runs")
        if st.button("← Retour à Historique", key="history-back-comparison"):
            _clear_history_navigation()
        st.subheader("Comparaison de runs — End-to-End")
        _render_end_to_end_comparison(run_ids)
        return
    if mode == JobType.PREDICTOR_PREFILTER.value:
        _page_header("Historique")
        st.caption("Historique > Comparaison de runs")
        if st.button("← Retour à Historique", key="history-back-comparison"):
            _clear_history_navigation()
        st.subheader("Comparaison de runs — Préfiltre")
        _render_prefilter_comparison(run_ids)
        return
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
    batch_result_key = f"{key_prefix}-batch-purge-result"
    batch_result = st.session_state.pop(batch_result_key, None)
    if isinstance(batch_result, BatchPurgeOutcome):
        _render_batch_purge_outcome(batch_result)
    delete_result = st.session_state.pop(f"{key_prefix}-batch-delete-result", None)
    if isinstance(delete_result, BatchDeleteOutcome):
        _render_batch_delete_outcome(delete_result)
    history_runs = service.history_runs(job_types=allowed_types)
    runs = [record.status for record in history_runs]
    details_by_run_id = {
        str(record.status["run_id"]): record.detail() for record in history_runs
    }
    models = _history_model_contexts(st.session_state.lab_config.project_root)
    universe_labels = {
        record.universe_id: record.name for record in _universe_service().records()
    }
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
        history_row(
            run, details_by_run_id[str(run["run_id"])], models,
            universe_labels=universe_labels, related_details=details_by_run_id,
        )
        for run in visible
    ]
    from rstock.application.history_grid import history_grid_row, render_history_grid
    grid_rows = [history_grid_row(
        row, details_by_run_id[row.run_id], universe_labels=universe_labels,
        related_details=details_by_run_id, runs_root=st.session_state.lab_config.project_root / "runs",
    ) for row in rows]
    selection = render_history_grid(
        grid_rows, key=f"{key_prefix}-grid",
        selected_ids=st.session_state.get(f"{key_prefix}-selected-runs", []),
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
    batch_pending_key = f"{key_prefix}-pending-batch-purge"
    pending_batch = st.session_state.get(batch_pending_key)
    if isinstance(pending_batch, BatchPurgeReview):
        _render_batch_purge_confirmation(service, pending_batch, key_prefix=key_prefix)
        return
    pending_delete = st.session_state.get(f"{key_prefix}-pending-batch-delete")
    if isinstance(pending_delete, BatchDeleteReview):
        _render_batch_delete_confirmation(service, pending_delete, key_prefix=key_prefix)
        return
    if not selected:
        st.caption("Sélectionnez des runs pour les ouvrir, comparer, purger ou supprimer définitivement ceux terminés, en échec ou annulés.")
        return
    if len(selected) > 1 and st.button(
        "Purger les données lourdes des runs sélectionnés",
        key=f"purge-selected-{key_prefix}",
    ):
        st.session_state[batch_pending_key] = preview_batch_purge(service, selected)
        st.rerun()
    if len(selected) > 1 and st.button(
        "Supprimer définitivement les runs sélectionnés",
        key=f"delete-selected-{key_prefix}",
    ):
        st.session_state[f"{key_prefix}-pending-batch-delete"] = preview_batch_delete(service, selected)
        st.rerun()
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
        if str(selected_run.get("status")) in {"completed", "failed", "cancelled"} and actions[3].button(
            "Supprimer définitivement", key=f"delete-history-{key_prefix}",
        ):
            try:
                st.session_state["pending-run-delete"] = service.delete_preview(selected_run_id)
            except (OSError, ValueError, RuntimeError) as error:
                st.error(f"Suppression impossible : {error}")
            else:
                st.rerun()
        pending_run_delete = st.session_state.get("pending-run-delete")
        if isinstance(pending_run_delete, DeletePlan) and pending_run_delete.run_id == selected_run_id:
            _render_run_delete_confirmation(service, pending_run_delete)
        return
    if action == "comparison":
        selected_types = {
            str(next(run for run in filtered if str(run["run_id"]) == run_id)["job_type"])
            for run_id in selected
        }
        if selected_types not in (
            {JobType.WALK_FORWARD.value}, {JobType.END_TO_END.value},
            {JobType.PREDICTOR_PREFILTER.value},
        ):
            st.caption("Sélectionnez uniquement des runs du même type : Walk-forward, End-to-End ou Préfiltre.")
        elif st.button("Comparer les runs", type="primary", key=f"compare-history-{key_prefix}"):
            _history_navigation("comparison", selected)
        return
    st.warning("Sélectionnez au maximum 6 runs pour une comparaison.")


def _format_storage_size(size_bytes: int) -> str:
    value = float(max(0, size_bytes))
    units = ("o", "Ko", "Mo", "Go", "To")
    for unit in units:
        if value < 1024.0 or unit == units[-1]:
            return f"{value:.0f} {unit}" if unit in {"o", "Ko"} else f"{value:.1f} {unit}"
        value /= 1024.0
    return f"{value:.1f} To"


def _render_batch_purge_outcome(outcome: BatchPurgeOutcome) -> None:
    message = (
        "Purge groupée terminée : "
        f"{len(outcome.succeeded)} succès, {len(outcome.skipped)} ignorés, "
        f"{len(outcome.errors)} erreurs. "
        f"{_format_storage_size(outcome.reclaimed_bytes)} libérés."
    )
    if outcome.errors and not outcome.succeeded:
        st.error(message)
    elif outcome.errors or outcome.skipped:
        st.warning(message)
    else:
        st.success(message)
    if outcome.skipped:
        st.info("Runs ignorés : " + "; ".join(f"{run_id} : {reason}" for run_id, reason in outcome.skipped))
    if outcome.errors:
        st.error("Erreurs : " + "; ".join(f"{run_id} : {reason}" for run_id, reason in outcome.errors))


def _render_batch_purge_confirmation(
    service: ExperimentService, review: BatchPurgeReview, *, key_prefix: str
) -> None:
    pending_key = f"{key_prefix}-pending-batch-purge"
    with st.container(border=True):
        st.warning(
            f"{len(review.requested_run_ids)} runs sélectionnés, "
            f"dont {len(review.eligible_run_ids)} admissibles à la purge "
            f"({len(review.affected_run_ids)} runs concernés, enfants liés inclus). "
            f"Environ {_format_storage_size(review.reclaimable_bytes)} de données lourdes "
            "seront libérées. Les runs et leurs métadonnées légères resteront dans l’Historique."
        )
        st.caption("Par type : " + ", ".join(
            f"{JOB_LABELS.get(job_type, job_type)} : {count}"
            for job_type, count in review.affected_by_type
        ))
        if review.skipped:
            st.info("Déjà purgés ou non admissibles : " + "; ".join(
                f"{run_id} : {reason}" for run_id, reason in review.skipped
            ))
        if review.errors:
            st.error("Préparation impossible : " + "; ".join(
                f"{run_id} : {reason}" for run_id, reason in review.errors
            ))
        confirm, cancel, _ = st.columns([2.5, 1, 5])
        if confirm.button(
            "Confirmer la purge groupée",
            type="primary",
            disabled=not review.eligible_run_ids,
            key=f"confirm-batch-purge-{key_prefix}",
        ):
            outcome = execute_batch_purge(service, review)
            st.session_state[f"{key_prefix}-batch-purge-result"] = outcome
            st.session_state.pop(pending_key, None)
            st.rerun()
        if cancel.button("Annuler", key=f"cancel-batch-purge-{key_prefix}"):
            st.session_state.pop(pending_key, None)
            st.rerun()


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


def _render_batch_delete_outcome(outcome: BatchDeleteOutcome) -> None:
    message = (
        "Suppression groupée terminée : "
        f"{len(outcome.succeeded)} succès, {len(outcome.skipped)} ignorés, "
        f"{len(outcome.errors)} erreurs; {len(outcome.deleted_run_ids)} runs supprimés."
    )
    if outcome.errors and not outcome.succeeded:
        st.error(message)
    elif outcome.errors or outcome.skipped:
        st.warning(message)
    else:
        st.success(message)
    if outcome.skipped:
        st.info("Runs ignorés : " + "; ".join(
            f"{run_id} : {reason}" for run_id, reason in outcome.skipped
        ))
    if outcome.errors:
        st.error("Erreurs : " + "; ".join(
            f"{run_id} : {reason}" for run_id, reason in outcome.errors
        ))


def _render_batch_delete_confirmation(
    service: ExperimentService, review: BatchDeleteReview, *, key_prefix: str
) -> None:
    pending_key = f"{key_prefix}-pending-batch-delete"
    with st.container(border=True):
        st.warning(
            "Suppression définitive et irréversible : "
            f"{len(review.requested_run_ids)} runs sélectionnés, "
            f"{len(review.affected_run_ids)} runs à supprimer, enfants propriétaires inclus. "
            f"({_format_storage_size(review.size_bytes)}). "
            "Leurs dossiers, logs, manifests, checkpoints, résultats et métadonnées disparaîtront."
        )
        st.caption("Par type : " + ", ".join(
            f"{JOB_LABELS.get(kind, kind)} : {count}"
            for kind, count in review.affected_by_type
        ))
        if review.affected_run_ids:
            st.caption("Périmètre confirmé : " + ", ".join(review.affected_run_ids))
        if review.skipped:
            st.info("Runs ignorés : " + "; ".join(
                f"{run_id} : {reason}" for run_id, reason in review.skipped
            ))
        if review.errors:
            st.error("Erreurs de préparation : " + "; ".join(
                f"{run_id} : {reason}" for run_id, reason in review.errors
            ))
        confirm, cancel, _ = st.columns([2.5, 1, 5])
        if confirm.button(
            "Confirmer la suppression définitive groupée", type="primary",
            disabled=not review.plans, key=f"confirm-batch-delete-{key_prefix}",
        ):
            st.session_state[f"{key_prefix}-batch-delete-result"] = execute_batch_delete(service, review)
            st.session_state.pop(pending_key, None)
            st.rerun()
        if cancel.button("Annuler", key=f"cancel-batch-delete-{key_prefix}"):
            st.session_state.pop(pending_key, None)
            st.rerun()


def _render_run_delete_confirmation(service: ExperimentService, plan: DeletePlan) -> None:
    with st.container(border=True):
        st.warning(
            "Suppression définitive et irréversible : "
            f"{len(plan.run_ids)} runs, enfants propriétaires inclus "
            f"({_format_storage_size(plan.size_bytes)}). "
            "Tous leurs fichiers et métadonnées disparaîtront de l’Historique."
        )
        st.caption("Par type : " + ", ".join(
            f"{JOB_LABELS.get(kind, kind)} : {count}" for kind, count in plan.by_type
        ))
        st.caption("Périmètre confirmé : " + ", ".join(plan.run_ids))
        confirm, cancel, _ = st.columns([2.5, 1, 5])
        if confirm.button(
            "Confirmer la suppression définitive", type="primary",
            key=f"confirm-run-delete-{plan.run_id}",
        ):
            try:
                service.delete_run(plan.run_id, expected_run_ids=plan.run_ids,
                                   expected_fingerprint=plan.fingerprint)
            except DeletionCleanupPending as error:
                st.session_state.pop("pending-run-delete", None)
                st.warning(str(error))
            except (OSError, ValueError, RuntimeError) as error:
                st.error(f"Suppression impossible : {error}")
            else:
                st.session_state.pop("pending-run-delete", None)
                st.rerun()
        if cancel.button("Annuler", key=f"cancel-run-delete-{plan.run_id}"):
            st.session_state.pop("pending-run-delete", None)
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
        symbols = models.tracked_universe().symbols
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

    selection = event.get("selection") if isinstance(event, dict) else getattr(event, "selection", None)
    selected = list(selection.get("rows", []) if isinstance(selection, dict) else getattr(selection, "rows", []))
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
    freshness: dict[str, str | None],
    error_count: int,
) -> None:
    left, right = st.columns([4, 1], gap="large")
    with left:
        _page_header("Surveillance")
        st.markdown(
            '<p class="rstock-surveillance-subtitle">'
            "Vue opérationnelle pour la prochaine séance et le suivi de la "
            "dernière séance."
            "</p>",
            unsafe_allow_html=True,
        )
    state_label, state_class = _freshness_state(freshness)
    error_class = "rstock-status-pill" if error_count == 0 else "rstock-error-pill"
    with right:
        st.markdown('<div class="rstock-header-right-spacer"></div>', unsafe_allow_html=True)
        st.markdown(
            f'<span class="{state_class}">{html.escape(state_label)}</span>'
            f'<span class="{error_class}">'
            f"{error_count} erreur{'s' if error_count != 1 else ''}</span>",
            unsafe_allow_html=True,
        )


def _render_surveillance_kpis(
    *,
    next_session: pd.Timestamp,
    reference_date: pd.Timestamp,
    crosses_weekend: bool,
    next_signals: OperationalTableView,
    latest_results: OperationalTableView,
    active_models: int,
    last_update: object,
) -> None:
    values = surveillance_kpi_values(next_signals, latest_results)
    _render_models_kpi_density_style()
    with st.container(key="models-kpis"):
        columns = st.columns(6, gap="small")
        _render_models_kpi_card(
            columns[0],
            "Séance d’aujourd’hui" if next_session == reference_date else "Prochaine séance",
            f"{next_session:%Y-%m-%d}",
            "calendar_month",
            caption=(
                "Séance en cours" if next_session == reference_date
                else "Week-end détecté" if crosses_weekend
                else "Séance ouvrable suivante"
            ),
        )
        _render_models_kpi_card(
            columns[1],
            "Signaux haussiers",
            values["next_signal_count"],
            "trending_up",
            caption=(
                f"P(Up) moyen {_models_percent(values['mean_up_probability'])}"
            ),
            directional=False,
        )
        _render_models_kpi_card(
            columns[2],
            "Rendement moyen signaux",
            _models_percent(values["mean_expected_return"]),
            "monitoring",
            caption=(
                "Séance d’aujourd’hui" if next_session == reference_date
                else "Prochaine séance"
            ),
            directional=True,
        )
        _render_models_kpi_card(
            columns[3],
            "Dernière séance",
            f"{values['winning_signal_count']} gagnants",
            "event_available",
            caption=f"Signaux évalués : {values['evaluated_signal_count']}",
        )
        _render_models_kpi_card(
            columns[4],
            "P&L dernière séance",
            _model_detail_currency(values["latest_session_pnl"]),
            "payments",
            caption="Dernière séance évaluée",
            directional=True,
        )
        _render_models_kpi_card(
            columns[5],
            "Modèles actifs",
            active_models,
            "model_training",
            caption=f"MAJ : {_compact_datetime(last_update)}",
        )


def _styled_surveillance_table(
    table: pd.DataFrame, directional_columns: tuple[str, ...]
) -> pd.io.formats.style.Styler:
    styled = style_directional_columns(table, directional_columns)
    if "Cible" in table:
        styled = styled.map(
            lambda _: "font-weight: 650; color: #0f172a", subset=["Cible"]
        )
    if "Catégorie" in table:
        styled = styled.map(
            lambda _: "font-weight: 600; color: #475569", subset=["Catégorie"]
        )
    return styled


_SURVEILLANCE_COLUMN_HELP = {
    "Date": "Séance boursière visée par le signal. Aucune cible de performance.",
    "Cible": "Titre dont le mouvement est prédit par le modèle. Aucune cible de performance.",
    "Prédicteurs": "Titres utilisés par le modèle pour prédire la cible. Plus de prédicteurs n'est pas nécessairement meilleur.",
    "P(Up)": "Probabilité de hausse estimée par le modèle. La force du signal se juge par rapport au seuil sélectionné du modèle, pas à 50 % en absolu.",
    "Catégorie": "Classification du signal selon le seuil de décision du modèle; elle aide à interpréter le signal.",
    "Rendement moyen historique (63 séances)": "Moyenne des rendements intrajournaliers des signaux historiques du modèle sur la fenêtre de 63 séances. Ce n’est pas le rendement réalisé du signal affiché.",
    "Rendement de la séance (Open→Close)": "Rendement réalisé de la cible pendant la séance évaluée : Close / Open − 1.",
    "Trades gagnants": "Proportion des signaux évalués avec un rendement positif. Cible : > 50 %; intéressant ≥ 55 %; solide ≥ 60 % avec un échantillon suffisant.",
    "Dernier signal": "Date du dernier signal déclenché; elle situe la récence de l'évaluation. Aucune cible de performance.",
    "P&L séance (10 000 $)": "P&L théorique de cette ligne pour la séance : rendement Open→Close × 10 000 $. Ce montant n’est pas cumulé.",
}


def _text_column_with_help(name: str, *, width: str, descriptions: dict[str, str]) -> Any:
    return st.column_config.TextColumn(width=width, help=descriptions[name])


def _grid_column_help_config(
    columns: pd.Index, descriptions: dict[str, str],
    existing: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Attach header help to the displayed columns, retaining existing formats."""
    existing = existing or {}
    config = {}
    for name in columns:
        if name in existing:
            column = dict(existing[name])
            column["help"] = descriptions[name]
            config[name] = column
        else:
            config[name] = st.column_config.Column(help=descriptions[name])
    return config


_PREFILTER_COLUMN_HELP = {
    "Cible": "Titre que les prédicteurs cherchent à prévoir. Aucune cible idéale.",
    "Candidats initiaux": "Prédicteurs disponibles avant le préfiltre; mesure la taille initiale de la recherche. Aucune cible idéale.",
    "Rejet AUC médiane": "Prédicteurs éliminés pour une AUC médiane sous le seuil du préfiltre. Aucune cible absolue; explique les éliminations.",
    "Rejet fenêtres > 0,50": "Prédicteurs rejetés faute d'une proportion suffisante de fenêtres avec AUC > 0,50. Un nombre élevé signale une constance temporelle limitée; aucune cible absolue.",
    "Rejet Worst AUC": "Prédicteurs rejetés car leur pire fenêtre ne respecte pas l'AUC minimale. Aucune cible absolue.",
    "Rejet dispersion": "Prédicteurs rejetés pour une variabilité d'AUC excessive entre fenêtres. Aucune cible absolue.",
    "Après qualification": "Prédicteurs encore admissibles après les critères de qualité du préfiltre. Aucune cible universelle.",
    "Après Top N": "Prédicteurs conservés après la limite Top-N. Cible : le Top-N configuré si assez de candidats sont qualifiés.",
    "Après redondance": "Prédicteurs restant après retrait des relations trop redondantes; préserve la diversité. Aucune cible universelle.",
    "Retenus": "Prédicteurs finaux utilisés pour construire les combinaisons. Aucune cible absolue.",
    "Combinaisons": "Combinaisons à évaluer après préfiltre, comparées au nombre avant préfiltre; mesure le volume de recherche. Aucune cible idéale.",
}

_WF_COMBINATION_COLUMN_HELP = {
    "Combinaison": "Identifiant de la combinaison cible et prédicteurs. Aucune cible idéale.",
    "Cible": "Titre dont le mouvement est prédit. Aucune cible idéale.",
    "Predictors": "Titres prédicteurs de la cible. Plus de prédicteurs n'implique pas nécessairement un meilleur modèle.",
    "Depth": "Nombre de prédicteurs dans la combinaison; une profondeur supérieure augmente la complexité. Aucune cible idéale.",
    "AUC dev": "AUC en développement Walk-forward; mesure la discrimination. Cible : > 0,50; ≥ 0,55 peut être intéressant.",
    "AUC dev médiane": "AUC médiane en développement Walk-forward; mesure la discrimination typique. Cible : > 0,50; ≥ 0,55 peut être intéressant.",
    "AUC holdout": "AUC sur le holdout indépendant. Cible : > 0,50, idéalement proche du développement; ≥ 0,55 peut être intéressant.",
    "Delta dev→holdout": "AUC holdout moins AUC développement; mesure le changement hors sélection. Cible : proche de 0; une valeur très négative indique une dégradation.",
    "Worst AUC": "Plus faible AUC des fenêtres de développement. Cible : > 0,50 si possible; éviter une valeur nettement inférieure.",
    "Dispersion": "Variabilité de l'AUC entre fenêtres Walk-forward. Cible : faible, proche de 0.",
    "Fenêtres valides": "Fenêtres Walk-forward évaluables. Cible : autant que possible parmi les fenêtres prévues.",
    "Observations positives": "Observations positives disponibles; situe la fiabilité des métriques. Cible : assez nombreuses pour éviter un faible échantillon.",
    "Statut": "Résultat de qualification. Cible : Holdout confirmé après les étapes de validation correspondantes.",
    "Stabilité / qualification": "Qualification de développement ou motif de non-admissibilité enregistré pour la combinaison. Cible : Qualifiée.",
    "Score": "Score interne de classement relatif des combinaisons dans le run. Aucun seuil absolu.",
    "Rang": "Position dans le classement du run. Cible : 1 selon la règle de classement appliquée.",
    "Seuil calibré": "Seuil de signal associé à la combinaison, lorsqu'il est disponible. Aucune cible universelle.",
}

_WF_VALIDATION_COLUMN_HELP = {
    "Phase": "Phase de validation, développement ou holdout final. Aucune cible idéale.",
    "Fenetres": "Fenêtres utilisées dans cette phase; davantage de fenêtres valides donne plus de contexte temporel.",
    "AUC mediane": "Médiane des AUC entre fenêtres; mesure la discrimination typique. Cible : > 0,50; ≥ 0,55 peut être intéressant.",
    "AUC min": "Plus faible AUC de la phase; mesure la pire fenêtre. Cible : idéalement > 0,50, sans dégradation extrême.",
    "Dispersion": "Variabilité des AUC entre fenêtres; mesure la stabilité temporelle. Cible : faible, proche de 0.",
    "Verdict": "Conclusion de RStock pour la phase. Cible : qualification ou confirmation selon la phase.",
}

_WF_BATCH_COLUMN_HELP = {
    "Batch": "Identifiant du batch Walk-forward. Aucune cible idéale.",
    "Range start": "Index de début de sa plage de combinaisons; permet la traçabilité. Aucune cible idéale.",
    "Range stop": "Index de fin de sa plage de combinaisons. Aucune cible idéale.",
    "Combinaisons": "Combinaisons affectées au batch. Cible : respecter le maximum configuré, sauf pour le dernier batch.",
    "Run ID": "Identifiant technique du run du batch; sert à la traçabilité. Aucune cible idéale.",
    "Statut": "État d'exécution du batch. Cible : completed.",
    "Progression": "Part du travail terminée pour ce batch. Cible : 100 %.",
    "Durée (s)": "Durée totale du batch en secondes. Aucune cible absolue; plus faible est préférable à charge comparable.",
    "Début": "Date et heure de démarrage du batch. Aucune cible idéale.",
    "Fin": "Date et heure de fin du batch. Aucune cible idéale.",
    "Erreur": "Message d'erreur du batch, s'il y en a un. Cible : aucune erreur.",
}

_XGBOOST_SELECTION_COLUMN_HELP = {
    "Direction": "Direction prédite, Up ou Down. Aucune cible idéale.",
    "Config gagnante": "Configuration XGBoost retenue pour cette direction. Aucune cible absolue.",
    "Selection score": "Score interne de sélection de la configuration; sert au classement relatif. Aucun seuil absolu.",
    "Écart avec le 2e meilleur candidat": "Écart de score avec la meilleure autre configuration; un faible écart indique un choix serré. Aucune cible universelle.",
    "ROC-AUC développement": "Discrimination en développement. Cible : > 0,50; ≥ 0,55 peut être intéressant.",
    "PR-AUC développement": "Aire sous la courbe précision-rappel en développement; utile si les classes sont déséquilibrées. Cible : au-dessus de la prévalence.",
    "Stabilité développement": "Dispersion de la ROC-AUC entre fenêtres de développement. Cible : faible, proche de 0.",
    "ROC-AUC holdout": "Discrimination sur le holdout. Cible : > 0,50 et proche du développement; ≥ 0,55 peut être intéressant.",
    "PR-AUC holdout": "Aire sous la courbe précision-rappel sur le holdout. Cible : au-dessus de la prévalence du holdout.",
    "Précision holdout": "Part des prédictions positives correctes sur le holdout. Cible : supérieure à la prévalence; les règles finales peuvent être plus exigeantes.",
    "Rappel holdout": "Part des observations positives détectées sur le holdout. À interpréter avec la précision et le volume; aucune cible universelle.",
    "F1 holdout": "Moyenne harmonique de précision et rappel sur le holdout. Plus élevé est préférable, sans seuil universel.",
    "Taux de prédictions positives": "Part des observations prédites positives; mesure la fréquence potentielle des signaux. Aucune cible universelle.",
    "Prévalence": "Part réelle de cas positifs; taux de base pour interpréter précision et PR-AUC. Aucune cible idéale.",
    "TP": "Vrais positifs : prédictions positives réalisées. À interpréter avec FP, rappel et taille de l'échantillon.",
    "FP": "Faux positifs : prédictions positives non réalisées. Cible : aussi peu que possible à couverture comparable.",
    "Sets": "Nombre de combinaisons (sets) reportées dans les métriques holdout de calibration. Aucune cible absolue.",
    "Observations": "Observations utilisées pour l'évaluation; un échantillon plus grand rend les métriques plus interprétables.",
    "Paramètres clés": "Résumé des principaux hyperparamètres XGBoost retenus. Aucune cible idéale.",
}

_THRESHOLD_PARAMETER_COLUMN_HELP = {
    "Configuration": "Identifiant de la configuration de calibration. Aucune cible idéale.",
    "Sélectionnée": "Indique la configuration retenue. Cible : cochée pour la configuration gagnante.",
    "% modèles admissibles (critère principal)": "Part des modèles évalués respectant les critères. Cible : élevée sans sacrifier leur qualité.",
    "Rang": "Position dans le classement des configurations. Cible : 1.",
    "Paramètres clés": "Résumé des paramètres de calibration de la configuration. Aucune cible idéale.",
    "Modèles évalués": "Modèles soumis à la configuration; situe la taille de l'échantillon. Aucune cible absolue.",
    "Modèles admissibles": "Modèles franchissant les critères; plus nombreux peut être utile sans relâcher excessivement les critères.",
    "Signaux": "Signaux produits par les modèles évalués; un échantillon plus grand rend les métriques plus fiables.",
    "Précision médiane": "Médiane de la précision des modèles évalués. Cible : au-dessus du taux de base; ≥ 55 % peut être intéressant avec assez de signaux.",
    "F1 médian": "Médiane du F1 des modèles évalués. Plus élevé est préférable, sans seuil universel.",
    "Fraction fenêtres admissibles": "Part des fenêtres respectant les critères de robustesse. Cible : élevée, proche de 100 %.",
    "Stabilité précision": "Variabilité de la précision entre fenêtres. Cible : faible, proche de 0.",
    "Rendement directionnel moyen": "Rendement Open→Close moyen dans la direction du signal. Cible : > 0 % et stable.",
    "Stabilité rendement": "Variabilité du rendement entre fenêtres. Cible : faible, proche de 0.",
    "Mouvement opposé": "Fréquence de mouvement opposé au signal. Cible : faible.",
    "Raison sélection/rejet": "Motif du choix ou du rejet de la configuration. Aucune cible idéale.",
}

_THRESHOLD_RESULT_COLUMN_HELP = {
    "Combinaison": "Combinaison cible et prédicteurs évaluée. Aucune cible idéale.",
    "Cible": "Titre prédit. Aucune cible idéale.",
    "Predictors": "Prédicteurs du modèle. Aucune cible idéale.",
    "Direction": "Direction du signal évalué. Aucune cible idéale.",
    "Statut promotion": "Admissibilité à la promotion. Cible : Candidat lorsque tous les critères passent.",
    "Raison": "Motif du statut de promotion. Aucune cible idéale.",
    "Seuil calibré": "Probabilité minimale retenue pour déclencher un signal. Aucune valeur universelle.",
    "Signaux holdout": "Signaux produits sur le holdout. Cible : assez nombreux pour une estimation fiable et au moins le minimum configuré.",
    "Précision holdout": "Part des prédictions positives correctes sur le holdout. Cible : au-dessus du taux de base; ≥ 55 % peut être intéressant avec assez de signaux.",
    "Success rate holdout": "Fréquence des mouvements favorables sur le holdout selon la métrique enregistrée par RStock. Plus élevée est préférable; distincte de la précision si les définitions diffèrent.",
    "Recall": "Part des observations positives détectées. À interpréter avec la précision; aucune cible universelle.",
    "F1": "Équilibre entre précision et rappel. Plus élevé est préférable, sans seuil universel.",
    "AUC holdout": "Discrimination sur le holdout. Cible : > 0,50; ≥ 0,55 peut être intéressant.",
    "Rendement directionnel moyen": "Rendement Open→Close moyen dans la direction prédite. Cible : > 0 %.",
    "Rendement médian": "Rendement directionnel médian, moins sensible aux extrêmes. Cible : > 0 %.",
    "MFE moyen": "Mouvement favorable maximal moyen en séance. Cible : positif et élevé par rapport au rendement capturé.",
    "MAE moyen": "Mouvement défavorable maximal moyen en séance. Cible : proche de 0 %.",
    "Fréquence mouvement opposé": "Part des observations avec un mouvement significatif contre le signal. Cible : faible.",
    "Score": "Score interne de classement relatif, si disponible. Aucun seuil absolu.",
}

_THRESHOLD_SUMMARY_COLUMN_HELP = {
    "Cible": "Titre prédit. Aucune cible idéale.",
    "Combinaison": "Combinaison évaluée. Aucune cible idéale.",
    "Direction": "Direction évaluée. Aucune cible idéale.",
    "Seuil calibré": "Seuil retenu par la calibration actuelle. Aucune cible universelle.",
    "Meilleur seuil robuste": "Seuil alternatif offrant la meilleure précision avec l'échantillon robuste requis. Aucune cible universelle.",
    "Delta seuil": "Seuil robuste moins seuil calibré; un faible écart indique moins de sensibilité au choix du seuil.",
    "Signaux au seuil calibré": "Signaux au seuil actuel; un échantillon suffisant rend l'évaluation plus fiable.",
    "Signaux au meilleur seuil robuste": "Signaux au seuil robuste; cible : respecter le minimum d'échantillon requis.",
    "Précision au seuil calibré": "Précision au seuil actuel. Cible : élevée et au-dessus du taux de base.",
    "Précision au meilleur seuil robuste": "Précision au seuil robuste. Cible : au moins celle du seuil calibré, à échantillon suffisant.",
    "Delta précision": "Précision du seuil robuste moins celle du seuil calibré; positif indique une amélioration.",
    "Rendement directionnel moyen au seuil calibré": "Rendement directionnel moyen au seuil actuel. Cible : > 0 %.",
    "Rendement directionnel moyen au meilleur seuil robuste": "Rendement directionnel moyen au seuil robuste. Cible : > 0 %, idéalement au moins celui du seuil actuel.",
    "Delta rendement": "Rendement moyen du seuil robuste moins celui du seuil calibré; positif indique une amélioration.",
    "Fréquence mouvement opposé au seuil calibré": "Fréquence de mouvement contraire au seuil actuel. Cible : faible.",
    "Fréquence mouvement opposé au meilleur seuil robuste": "Fréquence de mouvement contraire au seuil robuste. Cible : faible, idéalement non supérieure à l'actuelle.",
    "Diagnostic": "Conclusion de la sensibilité : indique si un autre seuil paraît préférable ou si l'actuel reste robuste.",
    "Raison sélection calibration": "Règle ayant déterminé le seuil calibré. Aucune cible idéale.",
    "Rang du seuil calibré": "Rang du seuil actuel parmi les candidats admissibles. Cible : proche de 1.",
    "Nombre de candidats admissibles": "Seuils respectant les règles d'admissibilité; indique si plusieurs choix robustes existent. Aucune cible absolue.",
}

_THRESHOLD_SENSITIVITY_COLUMN_HELP = {
    "Seuil": "Probabilité minimale évaluée pour déclencher un signal. Aucune cible universelle.",
    "Nombre de signaux": "Signaux générés à ce seuil. Cible : assez nombreux pour une estimation fiable.",
    "Précision": "Part des signaux corrects. Cible : au-dessus du taux de base; ≥ 55 % peut être intéressant avec assez de signaux.",
    "Recall": "Part des observations positives captées. À interpréter avec la précision; aucune cible universelle.",
    "F1": "Équilibre entre précision et rappel. Plus élevé est préférable, sans seuil universel.",
    "Rendement directionnel moyen": "Rendement moyen dans la direction du signal. Cible : > 0 %.",
    "Rendement médian": "Rendement médian des signaux, moins sensible aux extrêmes. Cible : > 0 %.",
    "Fréquence mouvement opposé": "Part des mouvements contraires au signal. Cible : faible.",
    "MFE moyen": "Mouvement favorable maximal moyen. Cible : positif.",
    "MAE moyen": "Mouvement défavorable maximal moyen. Cible : proche de 0 %.",
    "Seuil calibré actuel": "Marque le seuil actuellement retenu par la calibration. Aucune cible supplémentaire.",
}

_THRESHOLD_CHOICE_COLUMN_HELP = {
    "Seuil": "Seuil candidat évalué. Aucune cible universelle.",
    "Sélectionné": "Indique le seuil retenu. Cible : un seul seuil cohérent avec les règles de calibration.",
    "Admissible": "Indique si le seuil respecte les critères minimaux. Cible : vrai pour un seuil sélectionnable.",
    "Nombre total de signaux": "Signaux disponibles dans la calibration à ce seuil. Cible : respecter les exigences d'échantillon.",
    "Fraction de fenêtres admissibles": "Part des fenêtres respectant les critères. Cible : élevée, proche de 100 %.",
    "Précision calibration": "Précision agrégée selon la logique du run. Cible : élevée et au-dessus du taux de base.",
    "Stabilité précision": "Variabilité de la précision entre fenêtres. Cible : faible.",
    "Rendement directionnel moyen": "Rendement moyen dans la direction prédite. Cible : > 0 %.",
    "Stabilité rendement": "Variabilité du rendement entre fenêtres. Cible : faible.",
    "Fréquence mouvement opposé": "Part des mouvements contraires au signal. Cible : faible.",
    "F1": "Équilibre entre précision et rappel. Plus élevé est préférable, sans seuil universel.",
    "Dans tolérance précision": "Indique si la précision est dans la tolérance de sélection autorisée. Cible : vrai pour un seuil admissible à ce choix.",
    "Raison de rejet / sélection": "Motif du choix ou de l'élimination du seuil. Aucune cible idéale.",
}

def _surveillance_column_config(columns: pd.Index) -> dict[str, object]:
    return {
        name: _text_column_with_help(
            name, width="small", descriptions=_SURVEILLANCE_COLUMN_HELP
        )
        for name in columns
    }


def _render_next_session_signals(
    view: OperationalTableView, target_session: pd.Timestamp, today: pd.Timestamp,
) -> None:
    with st.container(border=True):
        session_label = (
            "séance d’aujourd’hui" if target_session == today else "prochaine séance"
        )
        st.subheader(f"Signaux haussiers — {session_label}")
        st.caption(f"Occasions à considérer pour la {session_label}.")
        controls = st.columns([1, 1.5, 3], gap="small")
        order = controls[0].selectbox(
            "Tri",
            ("P(Up) décroissant", "Cible A–Z", "Rendement décroissant"),
            key="surveillance-next-sort",
        )
        filter_label = controls[1].selectbox(
            "Filtre",
            (f"Tous les signaux ({len(view.table)})", "Rendement positif"),
            key="surveillance-next-filter",
        )
        table = view.table.copy()
        technical = view.technical.copy()
        if filter_label == "Rendement positif" and not technical.empty:
            keep = pd.to_numeric(
                technical["quality_mean_return"], errors="coerce"
            ).gt(0).to_numpy()
            table = table.iloc[keep].reset_index(drop=True)
            technical = technical.iloc[keep].reset_index(drop=True)
        if not technical.empty:
            if order == "P(Up) décroissant":
                sort_values = pd.to_numeric(technical["up_probability"], errors="coerce")
                positions = sort_values.sort_values(ascending=False, kind="stable").index
            elif order == "Rendement décroissant":
                sort_values = pd.to_numeric(
                    technical["quality_mean_return"], errors="coerce"
                )
                positions = sort_values.sort_values(ascending=False, kind="stable").index
            else:
                positions = table["Cible"].astype(str).str.upper().sort_values(
                    kind="stable"
                ).index
            table = table.iloc[positions].reset_index(drop=True)
        if table.empty:
            st.info(f"Aucun signal haussier pour la {session_label}.")
        else:
            st.dataframe(
                _styled_surveillance_table(
                    table, ("P(Up)", "Rendement moyen historique (63 séances)", "Trades gagnants")
                ),
                hide_index=True,
                width="stretch",
                column_config=_surveillance_column_config(table.columns),
            )


def _render_latest_session_results(view: OperationalTableView) -> None:
    with st.container(border=True):
        st.subheader("Dernière séance")
        st.caption("Résultats des signaux de la dernière séance évaluée.")
        if view.table.empty:
            st.info("Aucun signal évalué disponible.")
        else:
            st.dataframe(
                _styled_surveillance_table(
                    view.table, ("P(Up)", "Rendement de la séance (Open→Close)", "P&L séance (10 000 $)")
                ),
                hide_index=True,
                width="stretch",
                column_config=_surveillance_column_config(view.table.columns),
            )


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
    ("production_quality", "Qualité des modèles Production"),
)


def _render_daily_update_card(
    runs: list[dict[str, object]], *, tracked_model_count: int
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
    detail: dict[str, object] = {}
    quality: dict[str, object] = {}
    status_level = "info"
    status_message = "Aucune mise à jour quotidienne exécutée."
    stage_caption = ""
    workflow_percent = None
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
        stage_caption = f"Étape {stage_index}/5 — {stage_label}"
        workflow_percent = progress.get("workflow_percent")
        quality = detail.get("summary", {}).get("production_quality", {})
        if current.get("status") == "failed":
            status_level = "error"
            status_message = str(current.get("error") or "La mise à jour a échoué.")
        elif current.get("status") == "running":
            status_message = "Mise à jour quotidienne en cours…"
        else:
            status_message = "Mise à jour quotidienne en attente…"
    elif latest is not None and latest.get("status") == "completed":
        detail = _service().run(str(latest["run_id"]))
        quality = detail.get("summary", {}).get("production_quality", {})
        status_level = "success"
        status_message = "Mise à jour quotidienne terminée."
    if not isinstance(quality, dict):
        quality = {}
    processed = len(quality.get("models_processed", []))
    errors = quality.get("errors", [])
    error_count = len(errors) if isinstance(errors, list) else int(errors or 0)
    elapsed = float(quality.get("elapsed_seconds", 0.0) or 0.0)
    summary = f"{processed} modèles traités · {error_count} erreur · {elapsed:.1f} s"
    with st.container(border=True):
        columns = st.columns(
            [1.1, 3.4, 1.25, 1.35],
            gap="small",
            vertical_alignment="center",
        )
        columns[0].markdown("**Mise à jour quotidienne**")
        with columns[1]:
            getattr(st, status_level)(status_message)
            if stage_caption:
                st.caption(stage_caption)
            if workflow_percent is not None:
                st.progress(float(workflow_percent) / 100.0)
        retry_failed = current is not None and current.get("status") == "failed"
        with columns[2]:
            if st.button(
                "Relancer la mise à jour" if retry_failed else "Mettre à jour RStock",
                type="primary",
                width="stretch",
                disabled=tracked_model_count == 0 or (
                    current is not None
                    and current.get("status") in {"pending", "running"}
                ),
                key="daily-operational-update",
            ):
                if retry_failed:
                    _service().resume(str(current["run_id"]))
                else:
                    _submit_operational_job(JobType.OPERATIONAL_RUN)
                st.rerun()
        columns[3].caption(summary)


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


def _render_watching_surveillance_section(
    *, project_root: Path, models: ModelService,
    signal_service: SignalService, quality: pd.DataFrame,
    reference_time: pd.Timestamp, calendar_name: str,
) -> None:
    """Read only observation events; never expose a production trade action."""

    watching_models = [
        model for model in models.tracked_models() if model.status.value == "watching"
    ]
    with st.container(border=True):
        st.subheader("En observation")
        st.caption("Signaux suivis et évalués, non utilisés en production.")
        if not watching_models:
            st.info("Aucun modèle en observation.")
            return
        predictions = PredictionService(project_root).watching_history()
        signals = signal_service.watching_history()
        realized = signal_service.watching_realized_results()
        watching_quality = quality[quality["status"].astype(str).eq("watching")]
        watching_view = build_signals_view(
            signals, predictions, limit=max(50, len(signals))
        )
        freshness = MarketDataService().freshness(
            models.tracked_universe().symbols, st.session_state.lab_config
        )
        evaluated = build_evaluated_predictions_view(
            predictions, signals, realized, freshness
        )
        latest = latest_session_results_view(evaluated, watching_quality)
        target_session = surveillance_display_session(
            watching_view, predictions, evaluated, reference_time, calendar_name
        )
        upcoming = next_session_signals_view(
            watching_view, watching_quality, target_session
        )
        reference_date = reference_time.tz_localize(None).normalize()
        session_label = (
            "séance d’aujourd’hui" if target_session == reference_date
            else "prochaine séance"
        )
        st.markdown(f"**Signaux haussiers — {session_label}**")
        if upcoming.table.empty:
            st.caption(f"Aucun signal en observation pour la {session_label}.")
        else:
            st.dataframe(
                _styled_surveillance_table(
                    upcoming.table, ("P(Up)", "Rendement moyen historique (63 séances)", "Trades gagnants")
                ),
                hide_index=True, width="stretch",
                column_config=_surveillance_column_config(upcoming.table.columns),
            )
        st.markdown("**Dernière séance**")
        st.caption("Résultats des signaux de la dernière séance évaluée.")
        if latest.table.empty:
            st.caption("Aucun signal en observation évalué récemment.")
        else:
            st.dataframe(
                _styled_surveillance_table(
                    latest.table, ("P(Up)", "Rendement de la séance (Open→Close)", "P&L séance (10 000 $)")
                ),
                hide_index=True, width="stretch",
                column_config=_surveillance_column_config(latest.table.columns),
            )


def _render_surveillance_model_history(project_root: Path) -> None:
    """Expose prior ACTIVE and WATCHING events for today's tracked models."""

    repository = ProductionRepository(project_root)
    history = surveillance_model_history_table(
        repository.read_tracked_model_table("predictions"),
        repository.read_tracked_model_table("signals"),
        repository.read_tracked_model_table("realized_results"),
    )
    with st.expander(f"Historique complet des modèles suivis ({len(history)})"):
        st.caption(
            "La période de chaque événement correspond au statut du modèle lors de sa prédiction. "
            "Le P&L historique inclut les périodes actives et en observation."
        )
        if history.empty:
            st.info("Aucun événement historique pour les modèles suivis.")
        else:
            st.dataframe(history, hide_index=True, width="stretch")


def _render_surveillance_page(*, polling: bool) -> None:
    _surveillance_styles()
    project_root = st.session_state.lab_config.project_root
    models = ModelService(project_root)
    active_models = models.active_models()
    universe = models.operational_universe(active_models)
    predictions = PredictionService(project_root).active_history()
    signal_service = SignalService(project_root)
    signals = signal_service.active_history()
    freshness = MarketDataService().freshness(universe.symbols, st.session_state.lab_config)
    runs = _service().runs()
    quality = load_models_master(project_root)
    signal_view = build_signals_view(signals, predictions, limit=max(50, len(signals)))
    evaluated_view = _load_evaluated_predictions_view(
        predictions,
        signals,
        project_root=project_root,
    )
    reference_time = pd.Timestamp.now(tz="America/Toronto")
    reference_date = reference_time.tz_localize(None).normalize()
    calendar_name = getattr(st.session_state, "lab_calendar", "XNYS")
    latest_results = latest_session_results_view(evaluated_view, quality)
    next_session = surveillance_display_session(
        signal_view, predictions, evaluated_view, reference_time, calendar_name,
    )
    next_signals = next_session_signals_view(signal_view, quality, next_session)
    last_market = next(
        (run.get("finished_at") or run.get("created_at") for run in runs if run["job_type"] in {JobType.MARKET_UPDATE.value, JobType.OPERATIONAL_RUN.value} and run["status"] == "completed"),
        None,
    )
    quality_updated = pd.to_datetime(
        quality.get("quality_updated_at", pd.Series(dtype=object)),
        errors="coerce",
        utc=True,
    ).max()
    last_update = last_market if pd.isna(quality_updated) else quality_updated
    latest_operational = next(
        (run for run in runs if run["job_type"] in OPERATIONAL_JOB_TYPES),
        None,
    )
    error_count = int(
        latest_operational is not None and latest_operational.get("status") == "failed"
    )
    _render_surveillance_header(
        freshness=freshness,
        error_count=error_count,
    )
    _render_surveillance_kpis(
        next_session=next_session,
        reference_date=reference_date,
        crosses_weekend=session_crosses_weekend(reference_date, next_session),
        next_signals=next_signals,
        latest_results=latest_results,
        active_models=len(universe.model_ids),
        last_update=last_update,
    )
    _render_daily_update_card(
        runs, tracked_model_count=len(models.tracked_models())
    )
    st.subheader("Production")
    _render_next_session_signals(next_signals, next_session, reference_date)
    _render_latest_session_results(latest_results)
    _render_watching_surveillance_section(
        project_root=project_root, models=models, signal_service=signal_service,
        quality=quality, reference_time=reference_time,
        calendar_name=calendar_name,
    )
    _render_surveillance_model_history(project_root)
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


def _legacy_models_page() -> None:
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


def _quality_percent(value: object) -> str:
    return "—" if value is None or pd.isna(value) else f"{float(value):.2%}"


def _quality_currency(value: object) -> str:
    return "—" if value is None or pd.isna(value) else f"{float(value):+,.2f} $".replace(",", " ")


def _models_percent(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    return f"{float(value) * 100:.2f}".replace(".", ",") + " %"


def _models_currency(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    return f"{float(value):,.2f}".replace(",", " ").replace(".", ",") + " $"


def _render_models_kpi_density_style() -> None:
    """Keep Models-page and model-detail KPI strips legible at normal zoom."""
    st.markdown(
        """
        <style>
        .st-key-models-kpis [data-testid="stMetricLabel"],
        .st-key-models-kpis [data-testid="stMetricLabel"] p,
        .st-key-model-detail-kpis [data-testid="stMetricLabel"],
        .st-key-model-detail-kpis [data-testid="stMetricLabel"] p {
            font-size: 0.78rem !important;
            line-height: 1.15 !important;
        }
        .st-key-models-kpis [data-testid="stMetricLabel"] > div,
        .st-key-model-detail-kpis [data-testid="stMetricLabel"] > div {
            overflow: visible !important;
            white-space: normal !important;
        }
        .st-key-models-kpis [data-testid="stMetricValue"],
        .st-key-models-kpis [data-testid="stMetricValue"] > div,
        .st-key-models-kpis [data-testid="stMetricValue"] p,
        .st-key-model-detail-kpis [data-testid="stMetricValue"],
        .st-key-model-detail-kpis [data-testid="stMetricValue"] > div,
        .st-key-model-detail-kpis [data-testid="stMetricValue"] p {
            font-size: 1.65rem !important;
            line-height: 1.15 !important;
            white-space: nowrap !important;
        }
        [class*="st-key-model-detail-kpi-card-"][class*="-positive"] [data-testid="stMetricValue"],
        [class*="st-key-model-detail-kpi-card-"][class*="-positive"] [data-testid="stMetricValue"] *,
        [class*="st-key-models-kpi-card-"][class*="-positive"] [data-testid="stMetricValue"],
        [class*="st-key-models-kpi-card-"][class*="-positive"] [data-testid="stMetricValue"] * {
            color: #198754 !important;
            font-weight: 600 !important;
        }
        [class*="st-key-model-detail-kpi-card-"][class*="-negative"] [data-testid="stMetricValue"],
        [class*="st-key-model-detail-kpi-card-"][class*="-negative"] [data-testid="stMetricValue"] *,
        [class*="st-key-models-kpi-card-"][class*="-negative"] [data-testid="stMetricValue"],
        [class*="st-key-models-kpi-card-"][class*="-negative"] [data-testid="stMetricValue"] * {
            color: #dc3545 !important;
            font-weight: 600 !important;
        }
        </style>
        """,
        unsafe_allow_html=True,
    )


def _render_models_kpi_card(
    column: Any,
    label: str,
    value: object,
    icon: str,
    *,
    caption: str | None = None,
    directional: bool | None = None,
    help: str | None = None,
) -> None:
    uses_directional_color = (
        icon in {"trending_up", "payments", "target"}
        if directional is None
        else directional
    )
    tone = (
        _directional_tone(value)
        if uses_directional_color
        else "neutral"
    )
    with column:
        with st.container(border=True, key=f"models-kpi-card-{icon}-{tone}"):
            st.metric(f":material/{icon}: {label}", value, help=help)
            if caption:
                st.caption(caption)


def _directional_tone(value: object) -> str:
    """Classify a signed display value for presentation-only KPI color."""

    color = directional_display_style(value)
    if "#198754" in color:
        return "positive"
    if "#dc3545" in color:
        return "negative"
    return "neutral"


def _render_model_detail_kpi_card(
    column: Any, label: str, value: str, raw_value: object, icon: str
) -> None:
    """Render a detail-only KPI with color derived from its signed value."""

    tone = _directional_tone(raw_value)
    with column:
        with st.container(border=True, key=f"model-detail-kpi-card-{icon}-{tone}"):
            st.metric(f":material/{icon}: {label}", value)


def _render_model_lineage_cards(
    items: tuple[tuple[str, str, str], ...],
) -> None:
    """Render compact lineage metadata without the former technical text line."""

    st.markdown(
        """
        <style>
        [class*="st-key-model-detail-lineage-card-"] {
            background-color: #f3f5f7 !important;
            border-radius: 0.5rem;
        }
        [class*="st-key-model-detail-lineage-card-"] [data-testid="stVerticalBlockBorderWrapper"] {
            background-color: #f3f5f7 !important;
        }
        [class*="st-key-model-detail-lineage-card-"] [data-testid="stCaptionContainer"] {
            font-size: 0.72rem;
            line-height: 1.1;
        }
        </style>
        """,
        unsafe_allow_html=True,
    )
    columns = st.columns(len(items), gap="small")
    for index, (column, (label, value, icon)) in enumerate(zip(columns, items)):
        with column:
            with st.container(border=True, key=f"model-detail-lineage-card-{index}"):
                st.caption(f":material/{icon}: {label}")
                st.markdown(f"**{value}**")


_MODELS_GRID_COLUMN_HELP = {
    "Cible": "Titre boursier dont le mouvement est prédit par le modèle. Aucune cible idéale.",
    "Prédicteurs": "Titres utilisés comme variables prédictives pour prévoir le mouvement de la cible. Plus de prédicteurs n’implique pas nécessairement un meilleur modèle.",
    "Statut": "État actuel du modèle dans son cycle de vie. Cible : actif pour un modèle actuellement utilisé en production.",
    "Univers": "Univers de titres dans lequel le modèle a été découvert; il situe le contexte de recherche. Aucune cible idéale.",
    "Source": "Expérience End-to-End à l’origine du modèle; elle assure sa traçabilité. Aucune cible idéale.",
    "Top-N": "Nombre de prédicteurs présélectionnés avant la recherche des combinaisons; il décrit la largeur de l’espace de recherche. Aucune cible idéale : plus élevé n’est pas nécessairement meilleur.",
    "Promotion": "Date de promotion du modèle en production; elle indique son ancienneté réelle en exploitation. Aucune cible idéale.",
    "Signaux": "Nombre de signaux évalués pendant la fenêtre d’analyse sélectionnée. Un échantillon plus grand rend les métriques plus interprétables; éviter de conclure avec quelques signaux.",
    "Rendement moyen": "Rendement directionnel moyen Open→Close des signaux évalués. Cible : > 0 %, idéalement positif et stable sur suffisamment de signaux.",
    "Trades gagnants": "Pourcentage des signaux évalués avec un rendement positif. Cible : > 50 % positif; ≥ 55 % intéressant; ≥ 60 % solide si l’échantillon est suffisant.",
    "P&L cumulé": "Somme des gains et pertes des signaux évalués selon le capital de simulation. Cible : > 0 $ avec une progression durable.",
    "Drawdown": "Plus forte baisse du P&L cumulé depuis un sommet pendant la période analysée. Cible : le plus près possible de 0 $, à interpréter relativement au P&L et au capital engagé.",
    "Santé": "Évaluation synthétique de la qualité récente selon les règles de surveillance. Cible : état sain/conforme. « Données insuffisantes » indique trop peu d’observations pour conclure.",
    "Dernier signal": "Date du dernier signal déclenché par le modèle; elle indique sa récence d’activité. Aucune cible idéale.",
    "Tendance 63": "Évolution visuelle du P&L cumulé pendant la fenêtre d’analyse sélectionnée. Elle montre si la performance récente progresse, stagne ou se détériore. Cible : tendance globalement ascendante; prudence avec peu de signaux.",
}


def _models_trend_y_bounds(table: pd.DataFrame) -> tuple[float, float] | None:
    finite = []
    for trend in table["Tendance 63"]:
        if not isinstance(trend, (list, tuple)):
            continue
        for value in trend:
            try:
                number = float(value)
            except (TypeError, ValueError):
                continue
            if math.isfinite(number):
                finite.append(number)
    return (min(finite), max(finite)) if finite else None


_MODEL_DETAIL_WINDOWS_HELP = {
    "Fenêtre": "Nombre de séances utilisées pour les métriques récentes; permet de comparer le court et le moyen terme. Aucune cible idéale.",
    "Rendement moyen": "Rendement directionnel moyen Open→Close des signaux sur cette fenêtre. Cible : > 0 %, stable sur suffisamment de signaux.",
    "Trades gagnants": "Part des signaux à rendement positif. Cible : > 50 % positif; ≥ 55 % intéressant; ≥ 60 % solide avec assez de signaux.",
    "Signaux": "Nombre de signaux évalués sur la fenêtre; plus il est grand, plus les métriques sont interprétables. Éviter de conclure sur quelques signaux.",
}

_MODEL_DETAIL_SIGNALS_HELP = {
    "Date": "Date de la séance évaluée. Aucune cible idéale.",
    "Prob. Up": "Probabilité de hausse estimée; à interpréter selon le seuil de décision du modèle, pas par rapport à 50 % en absolu.",
    "Prob. Down": "Probabilité de baisse estimée; à interpréter selon la logique et le seuil de décision du modèle.",
    "Rendement": "Rendement Open→Close observé pour la séance. Cible pour un signal haussier : > 0 %.",
    "P&L": "Gain ou perte simulé du signal selon le capital utilisé par RStock. Cible : > 0 $.",
    "MFE": "Meilleur mouvement favorable après l’ouverture; mesure le potentiel disponible. Cible : positif, idéalement supérieur au rendement capturé.",
    "MAE": "Pire mouvement défavorable pendant la séance; mesure le risque intraday. Cible : près de 0 %, moins négatif étant préférable.",
    "Verdict": "Résultat final du signal selon les règles de classification de RStock. Cible : gagnant.",
}

_MODEL_DETAIL_BASELINE_HELP = {
    "Métrique": "Indicateur comparé entre la promotion et la performance récente. Aucune cible idéale commune à toutes les métriques.",
    "À la promotion": "Valeur de référence lors de la promotion; sert de baseline hors sélection. Aucune cible absolue.",
    "Actuel": "Valeur observée depuis la promotion. Cible selon la métrique : proche ou supérieure à la baseline lorsque plus élevé est meilleur.",
    "Écart": "Différence entre la valeur actuelle et la promotion; révèle une amélioration ou une dégradation. Cible : ≥ 0 lorsque plus élevé est meilleur.",
}


def _model_detail_column_config(
    descriptions: dict[str, str], *, medium_columns: tuple[str, ...] = (),
) -> dict[str, Any]:
    return {
        name: _text_column_with_help(
            name, width="medium" if name in medium_columns else "small",
            descriptions=descriptions,
        )
        for name in descriptions
    }


def _models_grid_column_config(
    trend_y_bounds: tuple[float, float] | None = None,
) -> dict[str, Any]:
    widths = {
        "Cible": "small", "Prédicteurs": "medium", "Statut": "small",
        "Univers": "medium", "Source": "medium", "Top-N": "small",
        "Promotion": "small", "Signaux": "small", "Rendement moyen": "small",
        "Trades gagnants": "small", "P&L cumulé": "small", "Drawdown": "small",
        "Santé": "medium", "Dernier signal": "small",
    }
    return {
        "model_id": None,
        **{
            name: _text_column_with_help(
                name, width=width, descriptions=_MODELS_GRID_COLUMN_HELP
            )
            for name, width in widths.items()
        },
        "Tendance 63": st.column_config.LineChartColumn(
            "Tendance 63", width="medium", help=_MODELS_GRID_COLUMN_HELP["Tendance 63"],
            color="#2563eb",
            **({"y_min": trend_y_bounds[0], "y_max": trend_y_bounds[1]}
               if trend_y_bounds is not None else {}),
        ),
    }


def _model_detail_date(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    parsed = pd.to_datetime(value, errors="coerce", utc=True)
    return str(value) if pd.isna(parsed) else parsed.strftime("%Y-%m-%d")


def _model_detail_short_id(value: object, *, limit: int = 16) -> str:
    text = "—" if value is None or pd.isna(value) else str(value)
    return text if len(text) <= limit else f"{text[:limit]}…"


def _model_detail_auc(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    try:
        return f"{float(value):.4f}".rstrip("0").rstrip(".").replace(".", ",")
    except (TypeError, ValueError):
        return "—"


def _model_detail_integer(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    try:
        return str(int(float(value)))
    except (TypeError, ValueError):
        return "—"


def _model_detail_currency(value: object) -> str:
    if value is None or pd.isna(value):
        return "—"
    return f"{float(value):+,.2f}".replace(",", " ").replace(".", ",") + " $"


def _render_qualification_metric_group(
    values: dict[str, object], fields: tuple[tuple[str, str, str, str | None], ...],
) -> None:
    formatters = {
        "auc": _model_detail_auc,
        "count": _model_detail_integer,
        "percent": _models_percent,
    }
    for offset in range(0, len(fields), 4):
        columns = st.columns(4, gap="small")
        for column, (label, key, format_name, help_text) in zip(columns, fields[offset:offset + 4]):
            with column:
                st.metric(label, formatters[format_name](values.get(key)), help=help_text)


def _render_initial_qualification(model: Any) -> None:
    groups = initial_qualification_metrics(model)
    wf = groups["walk_forward"]
    st.markdown("##### Walk-forward")
    _render_qualification_metric_group(wf, (
        ("AUC WF médiane", "median_auc", "auc", "Médiane des AUC des fenêtres Walk-forward valides."),
        ("Pire AUC WF", "worst_auc", "auc", None),
        ("Écart-type AUC WF", "auc_std", "auc", None),
        ("Fenêtres évaluées", "windows_evaluated", "count", None),
        ("Fenêtres AUC valides", "auc_windows", "count", None),
        ("Fenêtres AUC > 0,50", "windows_above_random", "percent", None),
        ("Observations positives", "positive_observations", "count", "Exemples réellement positifs utilisés pour qualifier le modèle, et non nombre de signaux."),
    ))
    st.caption(f"Run WF source : {wf['source_run_id'] or '—'}")

    st.markdown("##### Holdout")
    final = groups["holdout_final_wf"]
    st.caption("Holdout final WF")
    _render_qualification_metric_group(final, (
        ("AUC holdout final WF", "auc", "auc", "AUC du holdout final évalué par le Walk-forward, sans seuil de signal figé."),
        ("Précision finale WF", "precision", "percent", None),
        ("Recall final WF", "recall", "percent", None),
        ("F1 final WF", "f1", "auc", None),
    ))
    st.caption(f"Run WF source : {final['source_run_id'] or '—'}")

    signals = groups["holdout_signals"]
    st.caption("Signaux holdout avec seuil figé")
    _render_qualification_metric_group(signals, (
        ("Seuil calibré Up", "threshold", "percent", None),
        ("Signaux holdout", "signal_count", "count", None),
        ("Précision signaux", "precision", "percent", None),
        ("Recall signaux", "recall", "percent", None),
        ("F1 signaux", "f1", "auc", None),
        ("AUC signaux holdout", "auc", "auc", "AUC des probabilités du lot holdout associé aux seuils figés; distincte de l'AUC finale WF."),
        ("Rendement directionnel moyen", "directional_return_mean", "percent", None),
        ("Rendement médian", "intraday_return_median", "percent", "Médiane Open→Close des signaux Up; pour Up, elle est aussi directionnelle."),
        ("Mouvement opposé", "opposite_move_frequency", "percent", None),
        ("MFE moyenne", "mfe_mean", "percent", "Excursion favorable maximale moyenne durant la séance."),
        ("MAE moyenne", "mae_mean", "percent", "Excursion défavorable maximale moyenne durant la séance."),
    ))
    source_label = (
        "Run évaluation holdout" if signals["source_job_type"] == "holdout_evaluation"
        else "Run calibration des seuils (historique)"
    )
    st.caption(f"{source_label} : {signals['source_run_id'] or '—'}")


def _render_model_quality_detail(model_id: str) -> None:
    project_root = st.session_state.lab_config.project_root
    try:
        model = ProductionRepository(project_root).get(model_id)
    except KeyError:
        st.session_state.pop("models-navigation", None)
        st.warning("Ce modèle n’existe plus dans le registre Production.")
        return
    detail = load_model_quality_detail(
        project_root, model_id, model_version=model.artifact_version
    )
    snapshot = dict(detail.snapshot or {})
    lineage = dict(detail.lineage or snapshot.get("identity") or {})
    st.caption("Modèles > Détail du modèle")
    target, predictors = model.target, model.predictors
    source_configuration = model.source_configuration or {}
    source_end_to_end = (
        source_configuration.get("source_end_to_end_run")
        or source_configuration.get("source_experiment_run")
    )
    quality_status = (
        "Non calculé — version différente"
        if detail.quality_state == "version_mismatch"
        else "Non calculé"
        if detail.quality_state == "missing"
        else health_label(snapshot.get("health_status"))
    )
    header_back, header_content = st.columns((0.8, 4.2), gap="small")
    with header_back:
        if st.button("← Retour à la liste", key="models-detail-back"):
            st.session_state.pop("models-navigation", None)
            st.rerun()
    with header_content:
        title_column, status_badge, quality_badge = st.columns((3.0, 0.65, 1.35), gap="small")
        title_column.subheader(f"{target} ← {', '.join(str(item) for item in predictors)}")
        status_badge.badge(
            model.status.display_label,
            color="green" if model.status.value == "active" else "gray",
        )
        quality_badge.badge(quality_status, color="gray")
        st.caption(
            "Dernière mise à jour qualité : "
            f"{_model_detail_date(snapshot.get('generated_at'))}"
            " | Dernière observation évaluée : "
            f"{_model_detail_date(snapshot.get('last_evaluated_date'))}"
        )
        if model.status.value == "watching":
            st.caption("En observation — non utilisé en production")
    if detail.quality_state == "missing":
        st.caption("Qualité non calculée pour ce modèle.")
    elif detail.quality_state == "version_mismatch":
        st.caption("Qualité non calculée pour la version Production courante.")

    prefilter_enabled = lineage.get("predictor_prefilter_enabled")
    prefilter_top_n = lineage.get("predictor_prefilter_top_n")
    prefilter_label = (
        f"Top {_model_detail_integer(prefilter_top_n)}"
        if prefilter_enabled and prefilter_top_n is not None and not pd.isna(prefilter_top_n)
        else "Désactivé" if prefilter_enabled is False else "Non disponible"
    )
    source_wf = lineage.get("source_walk_forward_run_id") or model.source_walk_forward_run
    source_e2e = lineage.get("source_end_to_end_run_id") or source_end_to_end
    lineage_items = (
        ("Univers", lineage.get("primary_universe_name_at_promotion") or "Non disponible", "public"),
        ("Préfiltre / Top-N", prefilter_label, "filter_alt"),
        ("Cutoff", _model_detail_date(lineage.get("cutoff_date")), "event"),
        ("Date de promotion", _model_detail_date(lineage.get("promotion_date") or model.created_at), "calendar_month"),
        ("Validation temporelle", lineage.get("temporal_validation_status") or "Non applicable", "science"),
        ("Version", f"v{model.artifact_version}" if model.artifact_version is not None else "Non disponible", "inventory_2"),
        ("Run WF", _model_detail_short_id(source_wf), "history"),
    )
    _render_model_lineage_cards(lineage_items)
    st.caption(
        "Début de l’observation : "
        f"{_model_detail_date(model.watching_started_at)}"
        " · Activation : "
        f"{_model_detail_date(model.activated_at)}"
        " · Dernière désactivation : "
        f"{_model_detail_date(model.deactivated_at)}"
    )
    windows = {
        width: snapshot.get(f"window_{width}", {}) for width in (20, 63, 126)
    }
    since, comparison = snapshot.get("since_promotion", {}), snapshot.get("baseline_comparison", {})
    _render_models_kpi_density_style()
    with st.container(key="model-detail-kpis"):
        kpis = st.columns(4, gap="small")
        _render_model_detail_kpi_card(
            kpis[0], "Rendement moyen 63 séances",
            _models_percent(windows[63].get("mean_intraday_return")),
            windows[63].get("mean_intraday_return"), "trending_up",
        )
        _render_model_detail_kpi_card(
            kpis[1], "Trades gagnants", _models_percent(since.get("win_rate")),
            since.get("win_rate"), "target",
        )
        _render_model_detail_kpi_card(
            kpis[2], "P&L cumulé", _model_detail_currency(since.get("pnl")),
            since.get("pnl"), "payments",
        )
        _render_model_detail_kpi_card(
            kpis[3], "Drawdown max",
            _model_detail_currency(since.get("max_drawdown_dollars")),
            since.get("max_drawdown_dollars"), "trending_down",
        )
    st.dataframe(
        style_directional_columns(
            performance_windows_display_table(windows),
            ("Rendement moyen", "Trades gagnants"),
        ),
        hide_index=True, width="stretch",
        height=145,
        column_config=_model_detail_column_config(_MODEL_DETAIL_WINDOWS_HELP),
    )
    st.markdown("#### Historique par période")
    if detail.observations.empty:
        st.caption("Aucune observation évaluée pour comparer les périodes.")
    else:
        st.dataframe(
            style_directional_columns(
                model_phase_comparison_table(detail.observations, detail.series),
                ("Rendement moyen", "Trades gagnants", "P&L cumulé", "Drawdown"),
            ),
            hide_index=True, width="stretch",
        )
        if detail.observations["model_status_at_prediction"].eq("legacy_unknown").any():
            st.caption(
                "Les événements antérieurs sans statut figé sont affichés dans "
                "une période distincte; ils ne sont pas reclassés selon le statut courant."
            )

    charts = st.columns(2, gap="small")
    series = detail.series.copy()
    with charts[0]:
        with st.container(border=True):
            st.markdown("#### P&L cumulé")
            if series.empty:
                st.info("Aucune série de suivi disponible.")
            else:
                pnl_columns = ["session_date", "cumulative_pnl"]
                if "baseline_expected_cumulative_pnl" in series:
                    pnl_columns.append("baseline_expected_cumulative_pnl")
                else:
                    st.caption("Courbe baseline indisponible")
                pnl = series[pnl_columns].rename(columns={
                    "cumulative_pnl": "Historique complet",
                    "baseline_expected_cumulative_pnl": "Baseline attendue",
                }).melt("session_date", var_name="Série", value_name="P&L ($)")
                st.altair_chart(
                    alt.Chart(pnl).mark_line().encode(
                        x=alt.X("session_date:T", title=None, axis=alt.Axis(format="%Y-%m-%d")),
                        y=alt.Y("P&L ($):Q", title="P&L ($)"), color="Série:N",
                    ).properties(height=240),
                    width="stretch",
                )
    with charts[1]:
        with st.container(border=True):
            st.markdown("#### Rendement moyen roulant")
            if series.empty:
                st.info("Aucune série roulante disponible.")
            else:
                rolling = series[[
                    "session_date", "rolling_mean_return_20", "rolling_mean_return_63",
                ]].rename(columns={
                    "rolling_mean_return_20": "20 séances",
                    "rolling_mean_return_63": "63 séances",
                }).melt("session_date", var_name="Fenêtre", value_name="Rendement")
                st.altair_chart(
                    alt.Chart(rolling).mark_line().encode(
                        x=alt.X("session_date:T", title=None, axis=alt.Axis(format="%Y-%m-%d")),
                        y=alt.Y("Rendement:Q", title="Rendement", axis=alt.Axis(format=".2%")),
                        color="Fenêtre:N",
                    ).properties(height=240),
                    width="stretch",
                )

    baseline_tab, signals_tab, technical_tab = st.tabs(["Baseline", "Signaux", "Technique"])
    with baseline_tab:
        st.caption("Baseline de promotion vs réel")
        comparison_table = baseline_comparison_display_table(
            baseline_comparison_rows(snapshot, detail.baseline)
        )
        if comparison_table.empty:
            reason = (
                detail.quality_state
                if detail.quality_state != "current"
                else baseline_unavailability_reason(snapshot, detail.baseline)
            )
            st.info(f"Baseline indisponible : {reason}")
        else:
            st.dataframe(
                style_directional_columns(
                    comparison_table, ("À la promotion", "Actuel", "Écart")
                ),
                hide_index=True, width="stretch",
                column_config=_model_detail_column_config(
                    _MODEL_DETAIL_BASELINE_HELP, medium_columns=("Métrique",)
                ),
            )
        with st.expander("Qualification initiale"):
            _render_initial_qualification(model)
    with signals_tab:
        st.caption("Derniers signaux évalués")
        signals = evaluated_bullish_signals(detail.observations)
        if signals.empty:
            st.info("Aucun signal haussier live évalué.")
        else:
            signal_table = evaluated_bullish_signals_display_table(signals).head(10)
            signal_table["Période"] = signals.loc[signal_table.index, "model_status_at_prediction"].map(
                lambda value: (
                    "En observation" if value == "watching" else
                    "Production active" if value == "active" else
                    "Historique antérieur"
                )
            )
            st.dataframe(
                style_directional_columns(
                    signal_table[["Date", "Période", "Prob. Up", "Prob. Down", "Rendement", "P&L", "MFE", "MAE", "Verdict"]],
                    ("Rendement", "P&L", "MFE", "MAE"),
                ),
                hide_index=True, width="stretch",
                column_config=_model_detail_column_config(_MODEL_DETAIL_SIGNALS_HELP),
            )
        excluded = quality_excluded_observations(detail.observations)
        if not excluded.empty:
            with st.expander(f"Observations exclues ({len(excluded)})"):
                st.dataframe(
                    excluded_observations_display_table(excluded), hide_index=True,
                    width="stretch",
                )
    with technical_tab:
        st.json({
            "model_id": model.model_id,
            "artifact_version": model.artifact_version,
            "source_end_to_end_run_id": source_e2e,
            "source_walk_forward_run_id": source_wf,
            "up_threshold": model.up_threshold,
            "down_threshold": model.down_threshold,
            "cutoff_date": lineage.get("cutoff_date"),
            "source_configuration": model.source_configuration,
            "training_metadata": model.training_metadata,
            "watching_started_at": model.watching_started_at,
            "activated_at": model.activated_at,
            "deactivated_at": model.deactivated_at,
            "status_history": model.status_history,
            "temporal_validation_reason": lineage.get("temporal_validation_reason"),
        })
    if detail.quality_state == "current":
        st.info("Données insuffisantes pour établir un statut de santé.")
        st.caption("Les seuils Stable / À surveiller / Dégradé ne sont pas encore définis.")
    else:
        st.info("Qualité non calculée pour la version Production courante.")
        st.caption("Les métriques sont affichées à titre de suivi.")
    with st.expander("Détails techniques"):
        st.json({
            "model_id": model.model_id,
            "source_end_to_end_run_id": source_e2e,
            "source_walk_forward_run_id": source_wf,
            "artifact_version": model.artifact_version,
            "temporal_validation_reason": lineage.get("temporal_validation_reason"),
        })


def _models_page() -> None:
    navigation = st.session_state.get("models-navigation")
    if isinstance(navigation, dict) and navigation.get("mode") == "detail":
        model_id = str(navigation.get("model_id") or "")
        if model_id:
            _page_header("Modèles")
            _render_model_quality_detail(model_id)
            return
        st.session_state.pop("models-navigation", None)
    _page_header("Modèles")
    project_root = st.session_state.lab_config.project_root
    master = load_models_master(project_root)
    if not master.attrs.get("quality_snapshot_available", False):
        st.warning("Qualité non calculée — un rebuild qualité est requis.")
    orphan_count = int(master.attrs.get("orphan_quality_count", 0))
    if orphan_count:
        st.caption(
            f"{orphan_count} entrée(s) qualité orpheline(s) ignorée(s) : "
            "le registre Production reste la source d’autorité."
        )
    if master.empty:
        st.info("Aucun modèle dans le registre Production.")
        _live_job_panel(_service(), domain="model")
        return
    window = st.selectbox("Fenêtre d’analyse", (20, 63, 126), index=1, format_func=lambda value: f"{value} séances", key="models-quality-window")
    values = global_quality_kpis(master, window=window)
    _render_models_kpi_density_style()
    with st.container(key="models-kpis"):
        kpis = st.columns(6, gap="small")
        _render_models_kpi_card(kpis[0], "Modèles actifs", int(values["active_models"]), "model_training")
        _render_models_kpi_card(kpis[1], "Données insuffisantes", int(values["data_insufficient"]), "info")
        _render_models_kpi_card(kpis[2], f"Rendement moyen {window} séances", _models_percent(values["mean_return"]), "trending_up")
        _render_models_kpi_card(kpis[3], "P&L cumulé", _models_currency(values["pnl"]), "payments", help="P&L théorique basé sur un notionnel de 10 000 $ par signal et par modèle.")
        _render_models_kpi_card(kpis[4], "Trades gagnants", _models_percent(values["win_rate"]), "target")
        _render_models_kpi_card(kpis[5], f"Signaux {window} séances", int(values["signals"]), "notifications")

    filters = st.columns((1, 1, 1.35, 1, 1.35), gap="small")
    status_options = [
        *MODEL_STATUS_ORDER,
        *sorted(set(master["status"].dropna().astype(str)) - set(MODEL_STATUS_ORDER)),
    ]
    statuses = filters[0].multiselect(
        "Statut", status_options, key="models-status-filter",
        format_func=model_status_label, placeholder="Tous les statuts",
    )
    universe_options = sorted(master["universe_name"].dropna().astype(str).unique())
    universes = filters[1].multiselect(
        "Univers", universe_options, key="models-universe-filter",
        placeholder="Aucune valeur disponible", disabled=not universe_options,
    )
    sources = filters[2].multiselect("Source End-to-End", sorted(master["source_end_to_end_run_id"].dropna().astype(str).unique()), key="models-source-filter")
    health = filters[3].multiselect("Santé", sorted(master["health_label"].dropna().astype(str).unique()), key="models-health-filter")
    query = filters[4].text_input("Recherche", placeholder="Cible ou prédicteur", key="models-predictor-filter")
    visible = filter_quality_models(master, statuses=statuses, universes=universes, sources=sources, health=health, query=query)
    visible = sort_quality_models(visible, window=window)
    updated = pd.to_datetime(master["quality_updated_at"], errors="coerce", utc=True).max()
    st.caption("Qualité non calculée" if pd.isna(updated) else f"Dernière mise à jour qualité modèles : {updated.tz_convert('America/Toronto').strftime('%Y-%m-%d %H:%M')}")
    pagination = st.columns((0.8, 0.45, 2.75), gap="small")
    page_size = pagination[0].selectbox("Modèles par page", (50, 100), key="models-page-size")
    page_count = max(1, (len(visible) + page_size - 1) // page_size)
    if int(st.session_state.get("models-page", 1)) > page_count:
        st.session_state["models-page"] = 1
    page = pagination[1].number_input("Page", min_value=1, max_value=page_count, value=1, step=1, key="models-page")
    pagination[2].caption(f"Page {int(page)} / {page_count}")
    displayed = visible.iloc[(int(page) - 1) * page_size:int(page) * page_size]
    st.caption(f"{len(visible)} modèles affichés sur {len(master)} · page {int(page)} / {page_count}")
    table = models_grid(displayed, window=window)
    event = st.dataframe(
        style_directional_columns(
            table,
            ("Rendement moyen", "P&L cumulé", "Drawdown"),
        ).map(winning_trades_display_style, subset=["Trades gagnants"]),
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="single-row",
        key="models-grid",
        column_config=_models_grid_column_config(_models_trend_y_bounds(table)),
    )
    selected_rows = _selected_rows(event, len(displayed))
    selected_key = "selected-model-id"
    if selected_rows:
        st.session_state[selected_key] = str(table.iloc[selected_rows[0]]["model_id"])
    selected_id = st.session_state.get(selected_key)
    if selected_id not in set(displayed["model_id"].astype(str)):
        st.session_state.pop(selected_key, None)
        st.caption("Sélectionnez un modèle dans la grille pour afficher les actions.")
        _live_job_panel(_service(), domain="model")
        return
    st.markdown("**1 modèle sélectionné**")
    controls = st.columns((1.8, 0.8, 1.3, 0.75, 1.2, 0.75, 1.0), gap="small")
    if controls[0].button("Ouvrir le détail du modèle", type="primary"):
        st.session_state["models-navigation"] = {"mode": "detail", "model_id": selected_id}
        st.rerun()
    service = ModelService(project_root)
    selected_row = displayed.loc[
        displayed["model_id"].astype(str).eq(str(selected_id))
    ].iloc[0]
    selected = service.repository.model_from_summary(
        {"registry_payload": selected_row["registry_payload"]}
    )
    if controls[1].button("Entraîner", disabled=selected.status.value in {"active", "watching", "retired"}):
        _submit_operational_job(JobType.PRODUCTION_TRAINING, model_id=selected_id)
    watch_label = (
        "Passer en observation" if selected.status.value == "active"
        else "Démarrer le suivi"
    )
    if controls[2].button(watch_label, disabled=selected.status.value not in {"trained", "active"}):
        service.watch(selected_id); _invalidate_surveillance_selection_state(); st.rerun()
    if controls[3].button("Activer", disabled=selected.status.value not in {"trained", "watching", "inactive"}):
        service.activate(selected_id); _invalidate_surveillance_selection_state(); st.rerun()
    stop_label = "Arrêter le suivi" if selected.status.value == "watching" else "Désactiver"
    if controls[4].button(stop_label, disabled=selected.status.value not in {"active", "watching"}):
        service.deactivate(selected_id); _invalidate_surveillance_selection_state(); st.rerun()
    if controls[5].button("Retirer", disabled=selected.status.value in {"active", "watching"}):
        service.retire(selected_id); _invalidate_surveillance_selection_state(); st.rerun()
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
        if mode == "comparison" and 2 <= len(run_ids) <= 6 and set(run_ids) <= available:
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
    replay = result.historical_replay
    if replay:
        mode = _simulation_mode_label(replay.get("mode", ""))
        cutoff = replay.get("initial_training_cutoff")
        detail = f"Mode : {mode}"
        if cutoff:
            detail += f" · cutoff d'entraînement : {cutoff}"
        st.caption(detail)
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

SIMULATION_MODE_LABELS = {
    SIMULATION_MODE_EVALUATED_PREDICTIONS: "Prédictions évaluées",
    SIMULATION_MODE_DAILY_RETRAIN: "Historique — réentraînement quotidien",
    SIMULATION_MODE_FROZEN_AT_START: "Historique — modèles figés",
}
SIMULATION_MODE_HELP = {
    SIMULATION_MODE_DAILY_RETRAIN: (
        "Réentraîne les boosters avec les données disponibles avant chaque séance simulée."
    ),
    SIMULATION_MODE_FROZEN_AT_START: (
        "Entraîne les boosters une seule fois au début de la simulation, puis les conserve pendant toute la période."
    ),
}
LEGACY_SIMULATION_MODES = {
    "Historique": SIMULATION_MODE_DAILY_RETRAIN,
    "Prédictions évaluées": SIMULATION_MODE_EVALUATED_PREDICTIONS,
    "Résultats réalisés": SIMULATION_MODE_EVALUATED_PREDICTIONS,
}


def _simulation_mode_label(value: object) -> str:
    code = LEGACY_SIMULATION_MODES.get(str(value), str(value))
    return SIMULATION_MODE_LABELS.get(code, str(value))


def _simulation_model_snapshots(
    project_root: Path,
    result: SimulationResult,
    simulation_mode: str,
) -> list[dict[str, object]]:
    if simulation_mode in {
        SIMULATION_MODE_DAILY_RETRAIN,
        SIMULATION_MODE_FROZEN_AT_START,
        "Historique",
    }:
        return [dict(model) for model in result.model_snapshots]
    try:
        models = ProductionRepository(project_root).models()
    except (OSError, ValueError, json.JSONDecodeError):
        models = []
    used_ids = set(result.trades.get("Modèle source", pd.Series(dtype=object)).dropna().astype(str))
    selected = [model for model in models if model.model_id in used_ids]
    return [model.to_dict() for model in selected]


def _simulation_parameters(
    start_date, end_date, amount, exit_mode, simulation_mode,
    historical_replay: dict[str, object] | None = None,
) -> dict[str, object]:
    parameters: dict[str, object] = {
        "start_date": pd.Timestamp(start_date).date().isoformat(),
        "end_date": pd.Timestamp(end_date).date().isoformat(),
        "amount_per_signal": float(amount),
        "exit_mode": str(exit_mode),
        "simulation_mode": str(simulation_mode),
    }
    if historical_replay:
        parameters["historical_replay"] = dict(historical_replay)
    return parameters


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
    simulation_mode = LEGACY_SIMULATION_MODES.get(
        str(parameters.get("simulation_mode", "")),
        str(parameters.get("simulation_mode", "")),
    )
    if simulation_mode in SIMULATION_MODE_LABELS:
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
            st.caption(
                f"{amount} $ | {_simulation_mode_label(params.get('simulation_mode', '—'))}"
            )
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
    stored_mode = st.session_state.get("simulation-mode")
    if stored_mode in LEGACY_SIMULATION_MODES:
        st.session_state["simulation-mode"] = LEGACY_SIMULATION_MODES[stored_mode]
    defaults = {
        "simulation-start-date": default_start,
        "simulation-end-date": default_end,
        "simulation-amount": 10_000.0,
        "simulation-exit-mode": "Clôture du jour",
        "simulation-mode": SIMULATION_MODE_EVALUATED_PREDICTIONS,
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
            list(SIMULATION_MODE_LABELS),
            format_func=_simulation_mode_label,
            key="simulation-mode",
            help=(
                "Choisissez entre les prédictions déjà réalisées et deux replays historiques PIT."
            ),
        )
        if simulation_mode in SIMULATION_MODE_HELP:
            controls[4].caption(SIMULATION_MODE_HELP[simulation_mode])
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
            if simulation_mode in {
                SIMULATION_MODE_DAILY_RETRAIN,
                SIMULATION_MODE_FROZEN_AT_START,
            }:
                result = service.run_historical(
                    start_date,
                    end_date,
                    st.session_state.lab_config,
                    float(amount),
                    mode=simulation_mode,
                )
            else:
                result = service.run(start_date, end_date, float(amount))
            st.session_state["simulation-result"] = result
            try:
                metadata = SimulationRepository(project_root).save(
                    result,
                    parameters=_simulation_parameters(
                        start_date,
                        end_date,
                        amount,
                        st.session_state["simulation-exit-mode"],
                        simulation_mode,
                        result.historical_replay,
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


_configure_top_navigation_spacing()
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
