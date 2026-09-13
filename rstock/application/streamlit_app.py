"""Thin Streamlit interface for RStock Laboratory."""

from __future__ import annotations

import base64
import html
import json
from dataclasses import asdict, replace
from pathlib import Path

import altair as alt
import pandas as pd
import streamlit as st

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
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
from rstock.application.history_analysis import (
    RunAnalytics,
    analyze_run,
    comparison_table,
    comparison_chart_frames,
    configuration_differences,
    filter_combinations,
    load_walk_forward_artifacts,
    predictor_prefilter_summary,
    run_universe_summary,
    selected_run_action,
)
from rstock.application.runner import running_duration
from rstock.application.surveillance import (
    OperationalTableView,
    build_predictions_view,
    build_realized_results_view,
    build_signals_view,
    prediction_feature_tables,
    realized_main_table,
    source_observation_tables,
    validation_feedback,
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
from rstock.config import DEFAULT_CONFIG


st.set_page_config(page_title="RStock Laboratory", page_icon="🧪", layout="wide")

LOGO_PATH = Path(__file__).resolve().parents[1] / "assets" / "rstock_logo.png"


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
               style="display: block; width: 120px; height: auto; flex: 0 0 auto;" />
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
    st.session_state.setdefault("lab_config", DEFAULT_CONFIG)
    cached = MarketDataService().available_symbols(DEFAULT_CONFIG)
    st.session_state.setdefault("lab_symbols", cached or ["AAPL", "MSFT"])
    st.session_state.setdefault("lab_universe_selection", UniverseSelection())
    st.session_state.setdefault("lab_context_universe_ids", [])
    st.session_state.setdefault("lab_target_symbols", [])
    st.session_state.setdefault("lab_context_symbols", [])
    st.session_state.setdefault("lab_calendar", "XNYS")
    st.session_state.setdefault("lab_combinations_per_target", 3)
    st.session_state.setdefault("lab_evaluate_holdout", True)
    st.session_state.setdefault("max_concurrent_heavy_jobs", 1)


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


def _job_panel(service: ExperimentService, *, active_only: bool = True) -> None:
    runs = service.runs()
    if active_only:
        runs = [run for run in runs if run["status"] in {"pending", "running"}]
    if not runs:
        st.info("Aucun job actif.")
        return
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
                f"Étape: {progress.get('stage', '—')} · "
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


def _live_job_panel(service: ExperimentService) -> None:
    _job_panel(service)


if hasattr(st, "fragment"):
    _live_job_panel = st.fragment(run_every=2)(_live_job_panel)


def _universe_service() -> UniverseService:
    return UniverseService(root=st.session_state.lab_config.project_root)


def _experiment_universe_selector() -> bool:
    """Resolve the exact experiment symbols before the job is submitted."""

    service = _universe_service()
    current = st.session_state.lab_universe_selection
    st.subheader("Univers de l’expérience")
    labels = {
        "Univers complet": SAVED_SOURCE,
        "Échantillon d’un univers": SAMPLE_SOURCE,
    }
    current_label = next(
        (label for label, source in labels.items() if source == current.source),
        "Univers complet",
    )
    selected_label = st.radio(
        "Mode d’utilisation",
        list(labels),
        index=list(labels).index(current_label),
        horizontal=True,
        key="experiment-universe-mode",
    )
    source = labels[selected_label]
    universe_id = None
    size = None
    method = None
    seed = None
    names = service.standard_universe_names()
    universe_id = st.selectbox(
        "Univers principal",
        names,
        index=names.index(current.universe) if current.universe in names else 0,
        format_func=lambda item: universe_display_name(item, service),
        key="experiment-saved-universe",
    )
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
            index=0 if current.selection_method != SEEDED_SAMPLE else 1,
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
    context_names = tuple(
        record.universe_id
        for record in service.records()
        if record.type == CONTEXT_UNIVERSE_TYPE
    )
    selected_contexts = st.multiselect(
        "Univers de contexte",
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
    context_labels = {
        "Univers complet": SAVED_SOURCE,
        "Échantillon d’un univers": SAMPLE_SOURCE,
    }
    context_mode = st.radio(
        "Mode d’utilisation du contexte",
        list(context_labels),
        horizontal=True,
        key="experiment-context-mode",
    )
    context_sample_size = None
    context_method = None
    context_seed = None
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
    st.session_state.lab_target_symbols = list(target_symbols)
    st.session_state.lab_context_symbols = list(context_symbols)
    st.session_state.lab_symbols = list(predictor_symbols)
    st.caption(f"Univers principal : {universe_id}")
    st.caption(f"Cibles résolues : {len(target_symbols)}")
    st.caption(
        "Univers de contexte : "
        + (", ".join(selected_contexts) if selected_contexts else "Aucun")
    )
    st.caption(f"Symboles contexte : {len(context_symbols)}")
    st.caption(f"Prédicteurs disponibles : {len(predictor_symbols)} symboles uniques")
    with st.expander("Voir les symboles sélectionnés"):
        st.write("Cibles")
        st.code(", ".join(target_symbols))
        st.write("Contexte")
        st.code(", ".join(context_symbols) or "Aucun")
    st.caption("La création et la modification des listes se font dans la page Univers.")
    return True


def _experiments(service: ExperimentService) -> None:
    _page_header("Expériences")
    labels = {
        "Walk-forward": JobType.WALK_FORWARD,
        "Calibration XGBoost": JobType.XGBOOST_CALIBRATION,
        "Calibration des seuils": JobType.THRESHOLD_CALIBRATION,
    }
    choice = st.selectbox("Type de job", list(labels))
    valid_universe = _experiment_universe_selector()
    st.caption("La liste résolue et la configuration seront figées avant le lancement.")
    if st.button("Soumettre l’expérience", type="primary", disabled=not valid_universe):
        spec = ExperimentSpec(
            job_type=labels[choice],
            config=st.session_state.lab_config,
            symbols=tuple(st.session_state.lab_symbols),
            calendar=st.session_state.lab_calendar,
            combinations_per_target=st.session_state.lab_combinations_per_target,
            evaluate_final_holdout=st.session_state.lab_evaluate_holdout,
            universe_selection=st.session_state.lab_universe_selection,
            primary_universe_id=st.session_state.lab_universe_selection.universe,
            context_universe_ids=tuple(st.session_state.lab_context_universe_ids),
            target_symbols=tuple(st.session_state.lab_target_symbols),
            context_symbols=tuple(st.session_state.lab_context_symbols),
            predictor_symbols=tuple(st.session_state.lab_symbols),
        )
        submitted = service.submit(spec)
        if submitted.created:
            st.success(f"Run créé: {submitted.run_id}")
        else:
            st.warning(f"Configuration déjà active: {submitted.run_id}")
    st.subheader("Jobs actifs")
    _live_job_panel(service)


def _settings() -> None:
    _page_header("Paramètres")
    current = st.session_state.lab_config
    with st.expander("Valeurs RStock par défaut"):
        defaults = asdict(DEFAULT_CONFIG)
        defaults["project_root"] = str(DEFAULT_CONFIG.project_root)
        st.json(defaults)
    with st.container():
        calendar = st.text_input("Calendrier", value=st.session_state.lab_calendar)
        c1, c2, c3 = st.columns(3)
        history = c1.number_input("Historique (jours)", min_value=1, value=current.model_history_days)
        permutation = c2.number_input("Permutation depth", min_value=1, value=current.permutation_depth)
        max_sets = c3.number_input("Max generated sets", min_value=1, value=current.max_generated_sets)
        lag = c1.number_input("Lag depth", min_value=1, value=current.lag_depth)
        up_threshold = c2.number_input("Seuil intraday hausse", min_value=0.0, value=current.intraday_target_threshold, format="%.4f")
        down_threshold = c3.number_input("Seuil intraday baisse", min_value=0.0, value=current.intraday_down_threshold, format="%.4f")

        st.subheader("Walk-forward")
        w1, w2, w3, w4 = st.columns(4)
        min_train = w1.number_input("Train minimal", min_value=1, value=current.walk_forward_min_train_size)
        test_size = w2.number_input("Taille test", min_value=1, value=current.walk_forward_test_size)
        step = w3.number_input("Step", min_value=1, value=current.walk_forward_step_size)
        holdout = w4.number_input("Holdout final", min_value=1, value=current.final_holdout_size)

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
        workers = q1.number_input("Workers marché", min_value=1, value=current.market_cache_workers)
        combination_workers = q2.number_input(
            "Workers combinaisons", min_value=1, value=current.combination_workers
        )
        nthread = q2.number_input("Threads XGBoost", min_value=1, value=current.xgb_nthread)
        seed = q3.number_input("Seed", min_value=0, value=current.xgb_seed)
        combinations = q1.number_input("Combinaisons par cible (calibrations)", min_value=1, value=st.session_state.lab_combinations_per_target)
        max_jobs = q2.number_input("Jobs lourds concurrents", min_value=1, value=st.session_state.max_concurrent_heavy_jobs)
        evaluate_holdout = q3.checkbox("Évaluer le holdout final", value=st.session_state.lab_evaluate_holdout)

        st.subheader("Calibration des seuils")
        t1, t2 = st.columns(2)
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
        quantiles = st.text_input(
            "Quantiles de la grille",
            value=", ".join(str(value) for value in current.threshold_calibration_quantiles),
        )

        if st.button("Enregistrer les paramètres", type="primary"):
            parsed_quantiles = tuple(
                float(item.strip()) for item in quantiles.split(",") if item.strip()
            )
            st.session_state.lab_config = replace(
                current,
                model_history_days=int(history),
                permutation_depth=int(permutation),
                max_generated_sets=int(max_sets),
                lag_depth=int(lag),
                intraday_target_threshold=float(up_threshold),
                intraday_down_threshold=float(down_threshold),
                walk_forward_min_train_size=int(min_train),
                walk_forward_test_size=int(test_size),
                walk_forward_step_size=int(step),
                final_holdout_size=int(holdout),
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
                market_cache_workers=int(workers),
                combination_workers=int(combination_workers),
                xgb_nthread=int(nthread),
                xgb_seed=int(seed),
                threshold_calibration_min_signals_per_window=int(min_signals),
                threshold_calibration_min_window_fraction=float(min_window_fraction),
                threshold_calibration_quantiles=parsed_quantiles,
            )
            st.session_state.lab_calendar = calendar
            st.session_state.lab_combinations_per_target = int(combinations)
            st.session_state.max_concurrent_heavy_jobs = int(max_jobs)
            st.session_state.lab_evaluate_holdout = evaluate_holdout
            st.success("Paramètres enregistrés pour les prochaines soumissions.")


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
    service: ExperimentService,
    key_prefix: str,
) -> list[dict[str, object]]:
    columns = st.columns(4)
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
        "Période", ["Aujourd’hui", "7 jours", "30 jours", "Tout"], key=f"{key_prefix}-period"
    )
    model_options = [None, *sorted(models)]
    selected_model = columns[3].selectbox(
        "Modèle",
        model_options,
        format_func=lambda item: "Tous" if item is None else models[item],
        key=f"{key_prefix}-model",
    )
    return list(filter_runs(
        runs,
        allowed_types=allowed_types,
        job_type=selected_type,
        status=selected_status,
        period=period,
        model_id=selected_model,
        detail_loader=service.run if selected_model is not None else None,
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
    combinations = qualified_combinations_table(qualification, holdout)
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
    selected_rows = getattr(getattr(selection, "selection", None), "rows", [])
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


def _render_history_detail(
    run_id: str,
    *,
    status: dict[str, object],
    detail: dict[str, object],
    context: str,
    summary_text: str,
) -> None:
    st.divider()
    st.subheader("Détail du run")
    columns = st.columns(5)
    columns[0].metric("Type", JOB_LABELS.get(str(status["job_type"]), str(status["job_type"])))
    columns[1].metric("Date", history_row(status, detail, {}).date_time)
    columns[2].metric("Statut", str(status["status"]))
    columns[3].metric("Durée", history_row(status, detail, {}).duration)
    columns[4].metric("Contexte", context)
    st.markdown("**Résumé du run**")
    st.write(summary_text)
    st.caption(f"ID technique : {run_id}")
    if (
        status["job_type"] == JobType.WALK_FORWARD.value
        and status["status"] == "completed"
    ):
        _render_walk_forward_promotion(
            run_id, project_root=st.session_state.lab_config.project_root
        )
    tabs = st.tabs(["Résultats", "Configuration", "Fichiers", "Logs"])
    tabs[0].json(detail["summary"])
    tabs[1].json(detail["configuration"])
    tabs[2].write(detail["files"] or "Aucun résultat publié")
    tabs[3].code("\n".join(detail["log_tail"]) or "Aucun message")


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
    return analyze_run(status, detail, qualification, holdout)


def _format_metric(value: object, *, percent: bool = False) -> str:
    if value is None or pd.isna(value):
        return "—"
    return f"{float(value):.2%}" if percent else f"{float(value):.2f}"


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
    event = st.dataframe(
        table.drop(columns=["Eligible", "Holdout confirmé"], errors="ignore"),
        hide_index=True, width="stretch",
        on_select="rerun", selection_mode="single-row", key=f"analysis-combinations-{run_id}",
    )
    selected_rows = _selected_rows(event)
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


def _render_run_detail_view(
    service: ExperimentService, run_id: str,
) -> None:
    detail = service.run(run_id)
    status = detail["status"]
    if status["job_type"] != JobType.WALK_FORWARD.value:
        st.caption("Historique > Détail du run")
        if st.button("← Retour à Historique"):
            _clear_history_navigation()
        _render_history_detail(
            run_id, status=status, detail=detail,
            context=history_row(status, detail, {}).context,
            summary_text=history_row(status, detail, {}).summary,
        )
        return
    analytics = _load_run_analytics(run_id, status, detail)
    universe_summary = run_universe_summary(detail["configuration"])
    st.caption("Historique > Détail du run")
    if st.button("← Retour à Historique", key="history-back-detail"):
        _clear_history_navigation()
    st.title(f"Walk-forward — {len(analytics.symbols)} symboles — profondeur {analytics.depth}")
    st.caption(f"Univers principal : {universe_summary['primary_universe_id']}")
    st.caption(f"Cibles : {universe_summary['target_count']}")
    st.caption(
        "Univers de contexte : "
        + (", ".join(universe_summary["context_universe_ids"]) or "Aucun")
    )
    st.caption(f"Prédicteurs disponibles : {universe_summary['predictor_count']}")
    st.caption(
        f"{history_row(status, detail, {}).date_time} · {history_row(status, detail, {}).duration} · "
        f"Statut : {analytics.status}"
    )
    metrics = st.columns(6)
    metrics[0].metric("Combinaisons testées", analytics.tested_count if analytics.tested_count is not None else "—")
    metrics[1].metric("Qualifiées", analytics.qualified_count)
    metrics[2].metric("Confirmées holdout", analytics.confirmed_count)
    metrics[3].metric("AUC dev médiane", _format_metric(analytics.dev_auc_median))
    metrics[4].metric("AUC holdout médiane", _format_metric(analytics.holdout_auc_median))
    metrics[5].metric("Fenêtres (médiane)", _format_metric(
        pd.to_numeric(analytics.combinations.get("Fenêtres valides"), errors="coerce").median()
        if not analytics.combinations.empty else None
    ))
    tabs = st.tabs(["Résumé", "Analyse", "Combinaisons", "Validation", "Technique"])
    with tabs[0]:
        prefilter_table = predictor_prefilter_summary(detail.get("summary", {}))
        if not prefilter_table.empty:
            st.subheader("Pré-filtrage des prédicteurs")
            st.dataframe(prefilter_table, hide_index=True, width="stretch")
        rate_columns = st.columns(2)
        rate_columns[0].metric("Taux de qualification", _format_metric(analytics.qualification_rate, percent=True))
        rate_columns[1].metric("Taux de confirmation holdout", _format_metric(analytics.confirmation_rate, percent=True))
        if analytics.combinations.empty:
            st.info("Les artefacts analytiques ne sont pas disponibles pour ce run historique.")
        else:
            chart = analytics.combinations[["AUC dev", "AUC holdout"]].dropna(how="all")
            if not chart.empty:
                st.bar_chart(chart)
            scatter = analytics.combinations[["AUC dev", "AUC holdout"]].dropna()
            if not scatter.empty:
                st.caption("AUC développement vs holdout — une ligne par combinaison")
                st.scatter_chart(scatter, x="AUC dev", y="AUC holdout")
    with tabs[1]:
        if analytics.combinations.empty:
            st.caption("Aucune métrique de stabilité publiée pour ce run.")
        else:
            st.bar_chart(analytics.combinations[["Worst AUC", "Dispersion", "Delta dev→holdout"]])
    with tabs[2]:
        st.caption(f"{analytics.qualified_count} combinaisons qualifiées")
        filtered = _render_combination_filters(analytics)
        st.caption(f"{len(filtered)} correspondent aux filtres")
        selected = _selected_combination(run_id, filtered)
        if selected is not None:
            with st.container(border=True):
                st.subheader(f"{selected['Cible']} ← {selected['Predictors']}")
                details_columns = st.columns(4)
                for column, name in zip(
                    details_columns,
                    ("AUC dev", "AUC holdout", "Delta dev→holdout", "Worst AUC"),
                    strict=True,
                ):
                    column.metric(name, _format_metric(selected[name]))
                _promote_combination_action(run_id, selected)
    with tabs[3]:
        selected_name = st.session_state.get(f"analysis-selected-combination-{run_id}")
        selected = analytics.combinations[analytics.combinations["Combinaison"] == selected_name]
        if selected.empty:
            st.caption("Sélectionnez une combinaison dans l’onglet Combinaisons.")
        else:
            row = selected.iloc[0]
            validation = pd.DataFrame([
                {"Phase": "Développement", "Fenêtres": row["Fenêtres valides"], "AUC médiane": row["AUC dev"], "AUC min": row["Worst AUC"], "Dispersion": row["Dispersion"], "Verdict": "Qualifiée développement" if row["Eligible"] else "Non qualifiée"},
                {"Phase": "Holdout final", "Fenêtres": "—", "AUC médiane": row["AUC holdout"], "AUC min": "—", "Dispersion": "—", "Verdict": "Holdout confirmé" if row["Holdout confirmé"] else "Échec de confirmation holdout"},
            ])
            st.dataframe(validation, hide_index=True, width="stretch")
    with tabs[4]:
        st.caption(f"ID technique : {run_id}")
        technical = st.tabs(["Configuration", "Fichiers", "Logs"])
        technical[0].json(detail["configuration"])
        technical[1].write(detail["files"] or "Aucun résultat publié")
        technical[2].code("\n".join(detail["log_tail"]) or "Aucun message")


def _render_run_comparison_view(service: ExperimentService, run_ids: list[str]) -> None:
    details = [service.run(run_id) for run_id in run_ids]
    analytics = [_load_run_analytics(run_id, item["status"], item) for run_id, item in zip(run_ids, details, strict=True)]
    labels = {
        item.run_id: history_row(detail["status"], detail, {}).date_time
        for item, detail in zip(analytics, details, strict=True)
    }
    if len(set(labels.values())) != len(labels):
        labels = {key: f"{value} · {key[-4:]}" for key, value in labels.items()}
    st.caption("Historique > Comparaison de runs")
    if st.button("← Retour à Historique", key="history-back-comparison"):
        _clear_history_navigation()
    st.title("Comparaison de runs — Walk-forward")
    cards = st.columns(len(analytics))
    for column, analysis in zip(cards, analytics, strict=True):
        column.markdown(
            f"**{labels[analysis.run_id]}**\n\n{len(analysis.symbols)} symboles · profondeur {analysis.depth}\n\n{analysis.status}"
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
        st.dataframe(summary, hide_index=True, width="stretch")
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
                st.json(detail["configuration"])


def _history_runs_panel(
    service: ExperimentService,
    *,
    allowed_types: frozenset[str],
    key_prefix: str,
) -> None:
    runs = service.runs()
    models = _history_model_contexts(st.session_state.lab_config.project_root)
    filtered = _history_filters(
        runs, allowed_types=allowed_types, models=models, service=service, key_prefix=key_prefix
    )
    if not filtered:
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
    rows = [history_row(run, service.run(str(run["run_id"])), models) for run in visible]
    selection = st.dataframe(
        pd.DataFrame([row.display() for row in rows]),
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="multi-row",
        key=f"{key_prefix}-grid",
    )
    selected_rows = getattr(getattr(selection, "selection", None), "rows", [])
    selected_key = f"{key_prefix}-selected-runs"
    if selected_rows:
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
        if st.button("Ouvrir le run", type="primary", key=f"open-history-{key_prefix}"):
            _history_navigation("detail", selected)
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
        if mode == "Création manuelle":
            symbols = st.text_area(
                "Symboles",
                placeholder="AAPL, MSFT, NVDA\nou un symbole par ligne",
                key="create-universe-symbols",
            )
            if st.button("Enregistrer", type="primary", key="save-manual-universe"):
                try:
                    created = service.create(name, symbols, universe_type=universe_type)
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
            st.caption("Vous pouvez ajouter, retirer ou remplacer les symboles avant d’enregistrer.")
            if st.button("Enregistrer les modifications", key=f"update-universe-{universe_id}"):
                try:
                    service.update(
                        universe_id,
                        name=name,
                        symbols=symbols,
                        universe_type=edited_type,
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
    selected = _selected_rows(event)
    if selected:
        st.session_state.selected_universe_id = records[selected[0]].universe_id
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


def _selected_rows(event: object) -> list[int]:
    return list(getattr(getattr(event, "selection", None), "rows", []))


def _technical_record(view: OperationalTableView, selected: list[int]) -> dict[str, object] | None:
    if not selected or selected[0] >= len(view.technical):
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


def _render_predictions_tab(predictions: pd.DataFrame) -> None:
    view = build_predictions_view(predictions)
    if view.table.empty:
        st.info("Aucune prédiction disponible.")
        return
    event = st.dataframe(
        view.table, hide_index=True, width="stretch",
        on_select="rerun", selection_mode="single-row", key="surveillance-predictions",
    )
    technical = _technical_record(view, _selected_rows(event))
    if technical is not None:
        _render_prediction_audit_details(technical)


def _render_signals_tab(
    signals: pd.DataFrame,
    predictions: pd.DataFrame,
    models: ModelService,
) -> None:
    view = build_signals_view(signals, predictions)
    selected_signal = None
    if view.signals.table.empty:
        st.info("Aucun signal haussier aujourd’hui.")
    else:
        event = st.dataframe(
            view.signals.table, hide_index=True, width="stretch",
            on_select="rerun", selection_mode="single-row", key="surveillance-signals",
        )
        selected_signal = _technical_record(view.signals, _selected_rows(event))

    no_signal_selection: list[int] = []
    with st.expander(f"Voir les prédictions sans signal ({len(view.no_signal.table)})"):
        if view.no_signal.table.empty:
            st.caption("Aucune prédiction sans signal.")
        else:
            event = st.dataframe(
                view.no_signal.table, hide_index=True, width="stretch",
                on_select="rerun", selection_mode="single-row", key="surveillance-no-signals",
            )
            no_signal_selection = _selected_rows(event)

    selected_no_signal = _technical_record(view.no_signal, no_signal_selection)
    selected = selected_signal or selected_no_signal
    if selected is not None:
        st.markdown("**Détail du signal**")
        source_model = next(
            (model for model in models.models() if model.model_id == selected.get("model_id")),
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


def _realized_results_panel(
    predictions: pd.DataFrame,
    signals: pd.DataFrame,
) -> None:
    """Render realized results, pending predictions and validation feedback."""

    project_root = st.session_state.lab_config.project_root
    signal_service = SignalService(project_root)
    realized = signal_service.realized_results()
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
    view = build_realized_results_view(predictions, signals, realized, freshness)

    prediction_word = "prédiction" if view.pending_count == 1 else "prédictions"
    st.caption(
        f"{view.pending_count} {prediction_word} en attente"
        f" · Prochaine validation : {view.next_validation_date or '—'}"
        f" · Données jusqu’au : {view.latest_market_date or '—'}"
    )

    validation_jobs = [
        run
        for run in _service().runs()
        if run["job_type"]
        in {JobType.REALIZED_VALIDATION.value, JobType.OPERATIONAL_RUN.value}
    ]
    if validation_jobs:
        latest = validation_jobs[0]
        if latest["status"] in {"pending", "running"}:
            st.info("Validation des résultats en cours…")
        elif latest["status"] == "failed":
            st.error(f"La dernière validation a échoué : {latest.get('error') or 'erreur inconnue'}")
        elif latest["status"] == "completed":
            summary = _service().run(str(latest["run_id"]))["summary"]
            new_results = int(summary.get("realized_results", 0))
            if new_results:
                level, message = validation_feedback(new_results, view)
                getattr(st, level)(message)

    main_table = realized_main_table(view.table)
    event = st.dataframe(
        main_table, hide_index=True, width="stretch",
        on_select="rerun", selection_mode="single-row", key="surveillance-realized",
    )
    selected = _selected_rows(event)
    if selected and selected[0] < len(view.technical):
        record = json.loads(view.technical.iloc[selected[0]].to_json(date_format="iso"))
        _render_prediction_audit_details(record)
    if not view.pending.empty:
        with st.expander("Voir les prédictions en attente"):
            st.dataframe(
                build_predictions_view(view.pending, limit=len(view.pending)).table,
                hide_index=True,
                width="stretch",
            )


def _render_surveillance_page(*, polling: bool) -> None:
    _page_header("Surveillance")
    project_root = st.session_state.lab_config.project_root
    models = ModelService(project_root)
    universe = models.operational_universe()
    predictions = PredictionService(project_root).history()
    signal_service = SignalService(project_root)
    signals = signal_service.history()
    freshness = MarketDataService().freshness(universe.symbols, st.session_state.lab_config)
    runs = _service().runs()
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
    columns = st.columns([1, 1.15, 1.2, 1.9, 0.7])
    columns[0].metric("Modèles actifs", len(universe.model_ids))
    columns[1].metric("Symboles surveillés", len(universe.symbols))
    columns[2].metric(
        "Signaux haussiers",
        int((signals.get("category") == "bullish_signal").sum()) if not signals.empty else 0,
    )
    columns[3].metric("Dernière mise à jour", _compact_datetime(last_market))
    columns[4].metric("Erreurs", len(errors))
    st.caption(f"Dernière prédiction : {_compact_datetime(last_prediction)}")
    if freshness:
        st.caption(
            "Fraîcheur des données : "
            + ", ".join(f"{symbol}={date or 'manquant'}" for symbol, date in freshness.items())
        )
    with st.expander("Univers opérationnel"):
        st.write(", ".join(universe.symbols) or "Aucun symbole")
        if universe.used_by:
            st.dataframe(
                [{"Symbole": symbol, "Modèles": ", ".join(ids)} for symbol, ids in universe.used_by.items()],
                hide_index=True, width="stretch",
            )
    action_columns = st.columns(5)
    actions = [
        ("Mettre à jour le marché", JobType.MARKET_UPDATE),
        ("Prédictions quotidiennes", JobType.DAILY_PREDICTION),
        ("Détecter les signaux", JobType.DAILY_SCREENING),
        ("Résultats réalisés", JobType.REALIZED_VALIDATION),
        ("Exécution complète", JobType.OPERATIONAL_RUN),
    ]
    for column, (label, job_type) in zip(action_columns, actions, strict=True):
        if column.button(label, disabled=len(universe.model_ids) == 0):
            _submit_operational_job(job_type)
            # The first rerun installs the conditional polling fragment; later
            # reruns are driven by that fragment only while work is active.
            st.rerun()
    st.divider()
    content_tabs = st.tabs(["Prédictions", "Signaux", "Résultats réalisés"])
    with content_tabs[0]:
        _render_predictions_tab(predictions)
    with content_tabs[1]:
        _render_signals_tab(signals, predictions, models)
    with content_tabs[2]:
        _realized_results_panel(predictions, signals)
    if errors:
        with st.expander("Erreurs opérationnelles récentes"):
            st.json(errors[:10])
    st.subheader("Jobs actifs")
    _job_panel(_service())
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


def _models_page() -> None:
    _page_header("Modèles")
    service = ModelService(st.session_state.lab_config.project_root)
    models = service.models()
    if not models:
        st.info("Aucun candidat production. Promouvez une combinaison qualifiée depuis Historique.")
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
        }
        for model in models
    ])
    event = st.dataframe(
        table,
        hide_index=True,
        width="stretch",
        on_select="rerun",
        selection_mode="single-row",
        key="models-grid",
    )
    selected_rows = _selected_rows(event)
    selected_key = "selected-model-id"
    if selected_rows and selected_rows[0] < len(models):
        st.session_state[selected_key] = models[selected_rows[0]].model_id
    available_ids = {model.model_id for model in models}
    selected_id = st.session_state.get(selected_key)
    if selected_id not in available_ids:
        st.session_state.pop(selected_key, None)
        st.caption("Sélectionnez un modèle dans la grille pour afficher les actions.")
        st.subheader("Jobs actifs")
        _live_job_panel(_service())
        return
    selected = next(model for model in models if model.model_id == selected_id)
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
        st.rerun()
    if controls[2].button("Désactiver", disabled=selected.status.value != "active"):
        service.deactivate(selected_id)
        st.rerun()
    if controls[3].button("Retirer", disabled=selected.status.value == "active"):
        service.retire(selected_id)
        st.rerun()
    with st.expander("Voir détails"):
        st.json(selected.to_dict())
    st.subheader("Jobs actifs")
    _live_job_panel(_service())


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
    tabs = st.tabs(["Backtest / walk-forward", "Holdout", "Production réelle"])
    with tabs[0]:
        _history_runs_panel(
            _service(), allowed_types=EXPERIMENT_JOB_TYPES, key_prefix="experimental-history"
        )
    with tabs[1]:
        st.caption("Les métriques holdout restent attachées aux runs expérimentaux et aux modèles promus.")
        models = ModelService(st.session_state.lab_config.project_root).models()
        st.dataframe(
            [{"model_id": model.model_id, **model.holdout_metrics} for model in models],
            hide_index=True, width="stretch",
        )
    with tabs[2]:
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
            st.bar_chart(distribution.rename("Nombre de prédictions"))
            st.dataframe(results.tail(100), hide_index=True, width="stretch")
        with st.expander("Historique des prédictions de production"):
            st.dataframe(predictions.tail(200), hide_index=True, width="stretch")
        with st.expander("Historique des signaux de production"):
            st.dataframe(signals.tail(200), hide_index=True, width="stretch")


def _primary_pages() -> list[st.Page]:
    """Flat V1 navigation; this factory can later return grouped page mappings."""

    return [
        st.Page(_surveillance_page, title="Surveillance", icon=":material/monitoring:", default=True),
        st.Page(_experiments_page, title="Expériences", icon=":material/science:"),
        st.Page(_models_page, title="Modèles", icon=":material/model_training:"),
        st.Page(_history_page, title="Historique", icon=":material/history:"),
        st.Page(_universes_page, title="Univers", icon=":material/list_alt:"),
        st.Page(_settings_page, title="Paramètres", icon=":material/settings:"),
    ]


_state()
selected_page = st.navigation(_primary_pages(), position="top")
selected_page.run()
