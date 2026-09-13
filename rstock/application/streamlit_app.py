"""Thin Streamlit interface for RStock Laboratory."""

from __future__ import annotations

import json
from dataclasses import asdict, replace

import pandas as pd
import streamlit as st

from rstock.application.domain import ExperimentSpec, JobStatus, JobType
from rstock.application.runner import running_duration
from rstock.application.services import (
    ExperimentService,
    MarketDataService,
    ModelService,
    PredictionService,
    SignalService,
)
from rstock.application.universes import (
    MANUAL_SOURCE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    TOP_N,
    UniverseSelection,
    UniverseService,
)
from rstock.application.universe_ui import universe_display_name, universe_ui_preview
from rstock.config import DEFAULT_CONFIG


st.set_page_config(page_title="RStock Laboratory", page_icon="🧪", layout="wide")


def _hidden_legacy_text_area(*args, **kwargs) -> str:
    """Temporary non-rendering bridge for the retired standalone symbol field."""

    return ""


def _state() -> None:
    st.session_state.setdefault("lab_config", DEFAULT_CONFIG)
    cached = MarketDataService().available_symbols(DEFAULT_CONFIG)
    st.session_state.setdefault("lab_symbols", cached or ["AAPL", "MSFT"])
    st.session_state.setdefault("lab_universe_selection", UniverseSelection())
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


def _dashboard(service: ExperimentService) -> None:
    st.title("RStock Laboratory")
    st.caption("Pilotage local des expériences RStock")
    runs = service.runs()
    counts = {status: sum(run["status"] == status for run in runs) for status in JobStatus._value2member_map_}
    columns = st.columns(5)
    for column, status in zip(columns, JobStatus._value2member_map_, strict=True):
        column.metric(status, counts[status])
    st.subheader("Jobs actifs")
    _live_job_panel(service)


def _experiments(service: ExperimentService) -> None:
    st.title("Expériences")
    labels = {
        "Walk-forward": JobType.WALK_FORWARD,
        "Calibration XGBoost": JobType.XGBOOST_CALIBRATION,
        "Calibration des seuils": JobType.THRESHOLD_CALIBRATION,
    }
    choice = st.selectbox("Type de job", list(labels))
    selection = st.session_state.lab_universe_selection
    st.caption(
        f"Univers : {selection.universe or 'liste manuelle'} · "
        f"{len(st.session_state.lab_symbols)} symboles résolus"
    )
    with st.expander("Voir les symboles sélectionnés"):
        st.code(", ".join(st.session_state.lab_symbols))
    st.caption("La configuration enregistrée dans Paramètres sera figée avant le lancement.")
    if st.button("Soumettre l’expérience", type="primary"):
        spec = ExperimentSpec(
            job_type=labels[choice],
            config=st.session_state.lab_config,
            symbols=tuple(st.session_state.lab_symbols),
            calendar=st.session_state.lab_calendar,
            combinations_per_target=st.session_state.lab_combinations_per_target,
            evaluate_final_holdout=st.session_state.lab_evaluate_holdout,
            universe_selection=st.session_state.lab_universe_selection,
        )
        submitted = service.submit(spec)
        if submitted.created:
            st.success(f"Run créé: {submitted.run_id}")
        else:
            st.warning(f"Configuration déjà active: {submitted.run_id}")
    st.subheader("Jobs actifs")
    _live_job_panel(service)


def _settings() -> None:
    st.title("Paramètres")
    current = st.session_state.lab_config
    with st.expander("Valeurs RStock par défaut"):
        defaults = asdict(DEFAULT_CONFIG)
        defaults["project_root"] = str(DEFAULT_CONFIG.project_root)
        st.json(defaults)
    with st.container():
        # Retained only to avoid altering the surrounding configuration layout.
        symbols = _hidden_legacy_text_area(
            "Symboles (séparés par virgule)", value=", ".join(st.session_state.lab_symbols)
        )
        st.subheader("Univers des titres")
        universe_service = UniverseService()
        current_selection = st.session_state.lab_universe_selection
        source_labels = {
            "Liste manuelle": MANUAL_SOURCE,
            "Univers sauvegardé": SAVED_SOURCE,
            "Échantillon d’un univers": SAMPLE_SOURCE,
        }
        source_labels = {
            "Liste personnalis\u00e9e": MANUAL_SOURCE,
            "Univers complet": SAVED_SOURCE,
            "\u00c9chantillon d'un univers": SAMPLE_SOURCE,
        }
        current_label = next(
            (label for label, value in source_labels.items() if value == current_selection.source),
            "Liste personnalis\u00e9e",
        )
        source_label = st.radio(
            "Mode de sélection", list(source_labels), index=list(source_labels).index(current_label), horizontal=True
        )
        source = source_labels[source_label]
        universe_name = None
        sample_size = None
        selection_method = None
        sample_seed = None
        if source == MANUAL_SOURCE:
            symbols = st.text_area(
                "Symboles (séparés par virgule)", value=", ".join(st.session_state.lab_symbols)
            )
        else:
            universe_names = universe_service.universe_names()
            universe_name = st.selectbox(
                "Univers",
                universe_names,
                index=(
                    universe_names.index(current_selection.universe)
                    if current_selection.universe in universe_names
                    else 0
                ),
                format_func=lambda name: universe_display_name(name, universe_service),
            )
            if source == SAMPLE_SOURCE:
                available = len(universe_service.universe_symbols(universe_name))
                sample_size = st.number_input(
                    "Nombre de symboles", min_value=1, max_value=available, value=min(available, current_selection.sample_size or available)
                )
                method_label = st.selectbox(
                    "Méthode", ["Top N", "Échantillon reproductible"],
                    index=0 if current_selection.selection_method != SEEDED_SAMPLE else 1,
                )
                selection_method = TOP_N if method_label == "Top N" else SEEDED_SAMPLE
                if selection_method == SEEDED_SAMPLE:
                    sample_seed = st.number_input("Seed", min_value=0, value=current_selection.seed or 1234)
            symbols = ", ".join(universe_service.universe_symbols(universe_name))
        manual_symbols = tuple(
            item.strip().upper() for item in symbols.split(",") if item.strip()
        )
        preview_selection = UniverseSelection(
            source=source,
            universe=universe_name,
            sample_size=None if sample_size is None else int(sample_size),
            selection_method=selection_method,
            seed=None if sample_seed is None else int(sample_seed),
        )
        try:
            preview = universe_ui_preview(
                preview_selection, universe_service, manual_symbols=manual_symbols
            )
        except ValueError as error:
            preview = None
            st.error(f"Univers invalide : {error}")
        if preview is not None:
            st.caption(f"{len(preview.resolved_symbols)} symboles sélectionnés")
            with st.expander("Voir les symboles sélectionnés"):
                st.code(", ".join(preview.resolved_symbols))
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
            manual_symbols = tuple(
                item.strip().upper() for item in symbols.split(",") if item.strip()
            )
            universe_selection = UniverseSelection(
                source=source,
                universe=universe_name,
                sample_size=None if sample_size is None else int(sample_size),
                selection_method=selection_method,
                seed=None if sample_seed is None else int(sample_seed),
            )
            try:
                resolved_universe = universe_service.resolve(
                    universe_selection, manual_symbols=manual_symbols
                )
            except ValueError as error:
                st.error(f"Univers invalide : {error}")
                return
            parsed_symbols = resolved_universe.symbols
            # Submission consumes the already displayed preview, not a second
            # independently constructed symbol list.
            if preview is None:
                return
            universe_selection = preview.selection
            parsed_symbols = preview.resolved_symbols
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
            st.session_state.lab_symbols = list(parsed_symbols)
            st.session_state.lab_universe_selection = universe_selection
            st.session_state.lab_calendar = calendar
            st.session_state.lab_combinations_per_target = int(combinations)
            st.session_state.max_concurrent_heavy_jobs = int(max_jobs)
            st.session_state.lab_evaluate_holdout = evaluate_holdout
            st.success("Paramètres enregistrés pour les prochaines soumissions.")


    resolved_selection = st.session_state.lab_universe_selection
    mode_names = {
        MANUAL_SOURCE: "Liste manuelle",
        SAVED_SOURCE: "Univers sauvegardé",
        SAMPLE_SOURCE: "Échantillon reproductible" if resolved_selection.selection_method == SEEDED_SAMPLE else "Top N",
    }
    st.subheader("Aperçu de l’univers résolu")
    st.caption(
        f"Univers : {resolved_selection.universe or 'liste manuelle'} · "
        f"Mode : {mode_names[resolved_selection.source]} · "
        f"Taille : {len(st.session_state.lab_symbols)}"
        + (f" · Seed : {resolved_selection.seed}" if resolved_selection.seed is not None else "")
    )
    with st.expander("Voir les symboles sélectionnés"):
        st.code(", ".join(st.session_state.lab_symbols))


def _history(service: ExperimentService) -> None:
    st.title("Historique")
    runs = service.runs()
    if not runs:
        st.info("Aucun run enregistré.")
        return
    table = []
    for run in runs:
        summary = service.run(str(run["run_id"]))["summary"]
        preview = json.dumps(summary, ensure_ascii=False, default=str)
        table.append(
            {
                "run_id": run["run_id"],
                "type": run["job_type"],
                "créé": run["created_at"],
                "durée_s": run.get("duration_seconds"),
                "statut": run["status"],
                "principales métriques": preview[:240],
            }
        )
    st.dataframe(table, use_container_width=True, hide_index=True)
    selected = st.selectbox("Ouvrir un run", [run["run_id"] for run in runs])
    detail = service.run(selected)
    tabs = st.tabs(["Résumé", "Configuration", "Fichiers", "Logs"])
    tabs[0].json(detail["summary"])
    tabs[1].json(detail["configuration"])
    tabs[2].write(detail["files"] or "Aucun résultat publié")
    tabs[3].code("\n".join(detail["log_tail"]) or "Aucun message")


    if detail["status"]["job_type"] == JobType.WALK_FORWARD.value and detail["status"]["status"] == "completed":
        project_root = st.session_state.lab_config.project_root
        qualification_path = project_root / "runs" / str(selected) / "results" / "qualification.csv"
        if qualification_path.exists():
            qualified = pd.read_csv(qualification_path)
            eligible = qualified["Eligible"].map(
                lambda value: value is True
                or str(value).strip().lower() in {"true", "1", "yes"}
            )
            qualified = qualified[eligible]
            if not qualified.empty:
                st.subheader("Promotion vers la production")
                set_name = st.selectbox("Combinaison qualifiée", qualified["Set"].astype(str).tolist())
                xgb_run = st.text_input("Run calibration XGBoost (optionnel)") or None
                threshold_run = st.text_input("Run calibration seuils (optionnel)") or None
                if st.button("Promouvoir comme candidat production"):
                    model, created = ModelService(project_root).promote(
                        str(selected), set_name,
                        xgboost_calibration_run=xgb_run,
                        threshold_calibration_run=threshold_run,
                    )
                    if created:
                        st.success(f"Candidat créé : {model.model_id}")
                    else:
                        st.warning(f"Candidat identique déjà présent : {model.model_id}")


def _dashboard_page() -> None:
    _dashboard(_service())


def _experiments_page() -> None:
    _experiments(_service())


def _settings_page() -> None:
    _settings()


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


def _surveillance_page() -> None:
    st.title("Surveillance")
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
    operational_types = {
        JobType.PRODUCTION_TRAINING.value,
        JobType.MARKET_UPDATE.value,
        JobType.DAILY_PREDICTION.value,
        JobType.DAILY_SCREENING.value,
        JobType.REALIZED_VALIDATION.value,
        JobType.OPERATIONAL_RUN.value,
    }
    errors = [
        run
        for run in runs
        if run["status"] == "failed" and run["job_type"] in operational_types
    ]
    columns = st.columns(6)
    columns[0].metric("Modèles actifs", len(universe.model_ids))
    columns[1].metric("Symboles surveillés", len(universe.symbols))
    columns[2].metric(
        "Signaux haussiers",
        int((signals.get("category") == "bullish_signal").sum()) if not signals.empty else 0,
    )
    columns[3].metric("Dernière mise à jour", last_market or "—")
    columns[4].metric("Dernière prédiction", last_prediction or "—")
    columns[5].metric("Erreurs", len(errors))
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
                hide_index=True, use_container_width=True,
            )
    action_columns = st.columns(5)
    actions = [
        ("Mettre à jour le marché", JobType.MARKET_UPDATE),
        ("Prédictions quotidiennes", JobType.DAILY_PREDICTION),
        ("Screening", JobType.DAILY_SCREENING),
        ("Résultats réalisés", JobType.REALIZED_VALIDATION),
        ("Exécution complète", JobType.OPERATIONAL_RUN),
    ]
    for column, (label, job_type) in zip(action_columns, actions, strict=True):
        if column.button(label, disabled=len(universe.model_ids) == 0):
            _submit_operational_job(job_type)
    st.subheader("Dernières prédictions")
    st.dataframe(predictions.tail(50), hide_index=True, use_container_width=True)
    st.subheader("Signaux du jour / sans signal / erreurs")
    st.dataframe(signals.tail(50), hide_index=True, use_container_width=True)
    if not signals.empty:
        signal_id = st.selectbox("Détail du signal", signals["signal_id"].astype(str).tolist())
        signal = signals[signals["signal_id"].astype(str) == signal_id].iloc[-1].to_dict()
        source_model = next(
            (model for model in models.models() if model.model_id == signal["model_id"]),
            None,
        )
        with st.expander("Pourquoi ce signal ?", expanded=False):
            st.json({"signal": signal, "modèle_source": None if source_model is None else source_model.to_dict()})
    if errors:
        with st.expander("Erreurs opérationnelles récentes"):
            st.json(errors[:10])
    st.subheader("Jobs actifs")
    _live_job_panel(_service())


def _models_page() -> None:
    st.title("Modèles")
    service = ModelService(st.session_state.lab_config.project_root)
    models = service.models()
    if not models:
        st.info("Aucun candidat production. Promouvez une combinaison qualifiée depuis Historique.")
        return
    st.dataframe(
        [
            {
                "model_id": model.model_id, "cible": model.target,
                "predictors": ", ".join(model.predictors), "statut": model.status.value,
                "créé": model.created_at, "walk_forward": model.source_walk_forward_run,
                "version": model.artifact_version,
                "AUC dev médiane": model.development_metrics.get("ROCAUCMedian"),
                "AUC holdout": model.holdout_metrics.get("FinalUpROCAUC"),
            }
            for model in models
        ],
        hide_index=True, use_container_width=True,
    )
    selected_id = st.selectbox("Modèle", [model.model_id for model in models])
    selected = next(model for model in models if model.model_id == selected_id)
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
    st.title("Historique")
    tabs = st.tabs(["Backtest / walk-forward", "Holdout", "Production réelle"])
    with tabs[0]:
        _history(_service())
    with tabs[1]:
        st.caption("Les métriques holdout restent attachées aux runs expérimentaux et aux modèles promus.")
        models = ModelService(st.session_state.lab_config.project_root).models()
        st.dataframe(
            [{"model_id": model.model_id, **model.holdout_metrics} for model in models],
            hide_index=True, use_container_width=True,
        )
    with tabs[2]:
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
                signaux=("result_id", "count"), taux_succes=("up_target", "mean"),
                retour_moyen=("intraday_return", "mean"), mfe_moyenne=("mfe", "mean"),
                mae_moyenne=("mae", "mean"), fortes_baisses=("down_target", "mean"),
            ).reset_index()
            st.dataframe(summary, hide_index=True, use_container_width=True)
            if (summary["signaux"] < 30).any():
                st.warning(
                    "Au moins un modèle compte moins de 30 signaux réalisés; "
                    "ces statistiques restent descriptives."
                )
            distribution = pd.cut(
                results["intraday_return"], bins=10, duplicates="drop"
            ).value_counts(sort=False)
            st.bar_chart(distribution.rename("Nombre de signaux"))
            st.dataframe(results.tail(100), hide_index=True, use_container_width=True)
        with st.expander("Historique des prédictions de production"):
            st.dataframe(predictions.tail(200), hide_index=True, use_container_width=True)
        with st.expander("Historique des signaux de production"):
            st.dataframe(signals.tail(200), hide_index=True, use_container_width=True)


def _primary_pages() -> list[st.Page]:
    """Flat V1 navigation; this factory can later return grouped page mappings."""

    return [
        st.Page(_dashboard_page, title="Dashboard", icon=":material/dashboard:", default=True),
        st.Page(_surveillance_page, title="Surveillance", icon=":material/monitoring:"),
        st.Page(_experiments_page, title="Expériences", icon=":material/science:"),
        st.Page(_models_page, title="Modèles", icon=":material/model_training:"),
        st.Page(_history_page, title="Historique", icon=":material/history:"),
        st.Page(_settings_page, title="Paramètres", icon=":material/settings:"),
    ]


_state()
selected_page = st.navigation(_primary_pages(), position="top")
selected_page.run()
