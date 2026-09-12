"""Thin Streamlit interface for RStock Laboratory."""

from __future__ import annotations

import json
from dataclasses import asdict, replace

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
from rstock.config import DEFAULT_CONFIG


st.set_page_config(page_title="RStock Laboratory", page_icon="🧪", layout="wide")


def _state() -> None:
    st.session_state.setdefault("lab_config", DEFAULT_CONFIG)
    cached = MarketDataService().available_symbols(DEFAULT_CONFIG)
    st.session_state.setdefault("lab_symbols", cached or ["AAPL", "MSFT"])
    st.session_state.setdefault("lab_calendar", "XNYS")
    st.session_state.setdefault("lab_combinations_per_target", 3)
    st.session_state.setdefault("lab_evaluate_holdout", True)
    st.session_state.setdefault("max_concurrent_heavy_jobs", 1)


def _service() -> ExperimentService:
    return ExperimentService.local(
        DEFAULT_CONFIG.project_root,
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
    st.caption("La configuration enregistrée dans Paramètres sera figée avant le lancement.")
    if st.button("Soumettre l’expérience", type="primary"):
        spec = ExperimentSpec(
            job_type=labels[choice],
            config=st.session_state.lab_config,
            symbols=tuple(st.session_state.lab_symbols),
            calendar=st.session_state.lab_calendar,
            combinations_per_target=st.session_state.lab_combinations_per_target,
            evaluate_final_holdout=st.session_state.lab_evaluate_holdout,
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
    with st.form("settings"):
        symbols = st.text_area(
            "Symboles (séparés par virgule)", value=", ".join(st.session_state.lab_symbols)
        )
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

        if st.form_submit_button("Enregistrer les paramètres", type="primary"):
            parsed_symbols = tuple(item.strip().upper() for item in symbols.split(",") if item.strip())
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
            st.session_state.lab_calendar = calendar
            st.session_state.lab_combinations_per_target = int(combinations)
            st.session_state.max_concurrent_heavy_jobs = int(max_jobs)
            st.session_state.lab_evaluate_holdout = evaluate_holdout
            st.success("Paramètres enregistrés pour les prochaines soumissions.")


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


def _placeholder(title: str, text: str) -> None:
    st.title(title)
    st.info(text)


def _dashboard_page() -> None:
    _dashboard(_service())


def _surveillance_page() -> None:
    capabilities = SignalService().capabilities() | PredictionService().capabilities()
    _placeholder(
        "Surveillance",
        f"Préparée pour les workflows quotidiens. Capacités V1: {capabilities}",
    )


def _experiments_page() -> None:
    _experiments(_service())


def _models_page() -> None:
    _placeholder(
        "Modèles",
        f"Catalogue futur. Paramètres actifs: {ModelService().parameters(st.session_state.lab_config)}",
    )


def _history_page() -> None:
    _history(_service())


def _settings_page() -> None:
    _settings()


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
