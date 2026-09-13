from pathlib import Path


APP = (
    Path(__file__).resolve().parents[1]
    / "rstock"
    / "application"
    / "streamlit_app.py"
)
LOGO = APP.parents[1] / "assets" / "rstock_logo.png"


def test_laboratory_uses_native_top_navigation_without_sidebar_radio():
    source = APP.read_text(encoding="utf-8")

    assert "st.sidebar.radio" not in source
    assert 'st.navigation(_primary_pages(), position="top")' in source
    assert "selected_page.run()" in source


def test_primary_native_pages_cover_the_laboratory_sections():
    source = APP.read_text(encoding="utf-8")

    for title in (
        "Surveillance",
        "Expériences",
        "Modèles",
        "Historique",
        "Univers",
        "Paramètres",
    ):
        assert f'title="{title}"' in source
    assert source.count("st.Page(") == 6


def test_primary_pages_use_one_compact_logo_header_with_a_safe_fallback():
    source = APP.read_text(encoding="utf-8")

    assert LOGO.is_file()
    assert LOGO.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
    header = source.split("def _page_header", 1)[1].split("def _state", 1)[0]
    assert 'rstock_logo.png' in source
    assert 'display: flex' in header
    assert 'align-items: center' in header
    assert 'gap: 8px' in header
    assert 'width: 120px' in header
    assert 'height: auto' in header
    assert 'transform: translateY(4px)' in header
    assert 'except OSError:' in header
    assert 'st.title(title)' in header
    for title in (
        "Surveillance", "Expériences", "Modèles", "Historique", "Univers", "Paramètres",
    ):
        assert f'_page_header("{title}")' in source


def test_surveillance_is_the_default_page_without_dashboard_navigation():
    source = APP.read_text(encoding="utf-8")

    assert 'st.Page(_surveillance_page, title="Surveillance", icon=":material/monitoring:", default=True)' in source
    assert "title=\"Dashboard\"" not in source
    assert "_dashboard_page" not in source


def test_universe_management_is_separate_from_settings_and_selection_is_in_experiments():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings()", 1)[1].split(
        "def _history_model_contexts", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert "Univers des titres" not in settings
    assert "lab_universe_selection" not in settings
    assert "_experiment_universe_selector()" in experiments
    assert 'st.Page(_universes_page, title="Univers"' in source
    assert "+ Créer un univers" in source
    assert "Importer un CSV" in source
    assert "Dupliquer" in source
    assert "Supprimer" in source


def test_user_universe_deletion_requires_confirmation_and_resets_the_draft():
    source = APP.read_text(encoding="utf-8")
    detail = source.split("def _universe_detail", 1)[1].split(
        "def _universes_page", 1
    )[0]

    assert 'button("Supprimer"' in detail
    assert "Confirmer la suppression de l’univers" in detail
    assert 'button(\n                "Annuler"' in detail
    assert "service.delete(universe_id)" in detail
    assert "was_current = current.universe == universe_id" in detail
    assert "lab_universe_selection = UniverseSelection" in detail
    assert "st.rerun()" in detail


def test_experiments_only_selects_saved_universes_without_manual_entry():
    source = APP.read_text(encoding="utf-8")
    selector = source.split("def _experiment_universe_selector", 1)[1].split(
        "def _experiments", 1
    )[0]

    assert "Univers complet" in selector
    assert "Échantillon d’un univers" in selector
    assert "Liste personnalisée" not in selector
    assert "experiment-manual-symbols" not in selector
    assert "Symboles (séparés par virgule)" not in selector
    assert "service.standard_universe_names()" in selector
    assert '"Univers principal"' in selector
    assert '"Univers de contexte"' in selector
    assert "record.type == CONTEXT_UNIVERSE_TYPE" in selector
    assert "context_names = service.universe_names()" not in selector
    assert "st.multiselect(" in selector
    assert '"Mode d’utilisation du contexte"' in selector
    assert '"Échantillon d’un univers": SAMPLE_SOURCE' in selector
    assert '"Méthode de sélection du contexte"' in selector
    assert "context_sample_size=context_sample_size" in selector
    assert "context_selection_method=context_method" in selector
    assert "resolve_experiment(" in selector
    assert "Cibles résolues" in selector
    assert "Symboles contexte" in selector
    assert "Prédicteurs disponibles" in selector


def test_universe_page_exposes_persisted_type_without_weakening_system_protection():
    source = APP.read_text(encoding="utf-8")
    creation = source.split("def _create_universe_panel", 1)[1].split(
        "def _universe_selection_preview", 1
    )[0]
    detail = source.split("def _universe_detail", 1)[1].split(
        "def _universes_page", 1
    )[0]
    page = source.split("def _universes_page", 1)[1].split(
        "def _selected_rows", 1
    )[0]

    assert '"Type d’univers", ["Standard", "Contexte"]' in creation
    assert "universe_type=universe_type" in creation
    assert "edit-universe-type" in detail
    assert "universe_type=edited_type" in detail
    assert '"Type": "Standard"' in page
    assert "Univers système protégé" in detail


def test_experiment_submission_and_history_expose_frozen_universe_roles():
    source = APP.read_text(encoding="utf-8")
    experiments = source.split("def _experiments(", 1)[1].split(
        "def _settings", 1
    )[0]
    detail = source.split("def _render_run_detail_view", 1)[1].split(
        "def _render_run_comparison_view", 1
    )[0]

    for field in (
        "primary_universe_id", "context_universe_ids", "target_symbols",
        "context_symbols", "predictor_symbols",
    ):
        assert f"{field}=" in experiments
    assert "run_universe_summary" in detail
    assert "Univers principal" in detail
    assert "Cibles" in detail
    assert "Univers de contexte" in detail
    assert "Prédicteurs disponibles" in detail


def test_models_and_surveillance_pages_expose_the_operational_flow():
    source = APP.read_text(encoding="utf-8")
    models_page = source.split("def _models_page", 1)[1].split(
        "def _history_page", 1
    )[0]

    assert "Catalogue futur" not in source
    for label in (
        "Promouvoir comme candidat production",
        "Entraîner",
        "Activer",
        "Désactiver",
        "Retirer",
        "Mettre à jour le marché",
        "Prédictions quotidiennes",
        "Détecter les signaux",
        "Résultats réalisés",
        "Exécution complète",
    ):
        assert label in source
    for history_kind in (
        "Backtest / walk-forward",
        "Holdout",
        "Production réelle",
    ):
        assert history_kind in source
    assert 'selection_mode="single-row"' in models_page
    assert 'key="models-grid"' in models_page
    assert 'st.selectbox("Modèle"' not in models_page
    assert '"selected-model-id"' in models_page
    assert "Sélectionnez un modèle dans la grille pour afficher les actions." in models_page
    assert "selected.model_id" in models_page
    assert "Calibration du signal haussier" in models_page
    assert "calibrated_signal_threshold" in models_page
    assert "Seuil de signal : seuil global de décision" in models_page


def test_settings_and_run_detail_expose_predictor_prefilter_controls_and_summary():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split(
        "def _history_filters", 1
    )[0]
    detail = source.split("def _render_run_detail_view", 1)[1].split(
        "def _render_run_comparison_view", 1
    )[0]

    assert 'st.subheader("Pré-filtrage des prédicteurs")' in settings
    for name in (
        "predictor_prefilter_enabled",
        "predictor_prefilter_top_n",
        "predictor_prefilter_min_median_auc",
        "predictor_prefilter_min_pct_above_random",
        "predictor_prefilter_min_worst_auc",
        "predictor_prefilter_max_auc_std",
        "predictor_prefilter_correlation_threshold",
    ):
        assert name in settings
    assert settings.count("help=") >= 7
    assert "predictor_prefilter_summary" in detail
    assert 'st.subheader("Pré-filtrage des prédicteurs")' in detail


def test_surveillance_uses_one_conditional_page_level_polling_fragment():
    source = APP.read_text(encoding="utf-8")

    assert "_polling_surveillance_page = st.fragment(run_every=2)" in source
    assert "surveillance_refresh_decision(runs, polling=False)" in source
    assert "st.rerun(scope=\"app\")" in source
    assert "_realized_results_panel = st.fragment" not in source


def test_surveillance_uses_internal_tabs_and_on_demand_technical_details():
    source = APP.read_text(encoding="utf-8")

    assert 'st.tabs(["Prédictions", "Signaux", "Résultats réalisés"])' in source
    assert "_render_predictions_tab(predictions)" in source
    assert "_render_signals_tab(signals, predictions, models)" in source
    assert "_realized_results_panel(predictions, signals)" in source
    assert "Voir les prédictions sans signal" in source
    assert "Détails techniques" in source
    assert "Pourquoi ce signal ?" in source


def test_surveillance_kpis_keep_last_update_wide_and_prediction_secondary():
    source = APP.read_text(encoding="utf-8")
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "if hasattr(st, \"fragment\")", 1
    )[0]

    assert 'columns = st.columns([1, 1.15, 1.2, 1.9, 0.7])' in surveillance
    assert 'columns[3].metric("Dernière mise à jour", _compact_datetime(last_market))' in surveillance
    assert 'columns[4].metric("Erreurs", len(errors))' in surveillance
    assert 'st.caption(f"Dernière prédiction : {_compact_datetime(last_prediction)}")' in surveillance
    assert 'columns[4].metric("Dernière prédiction"' not in surveillance
    assert 'timestamp.strftime("%Y-%m-%d %H:%M")' in source


def test_realized_results_grid_is_always_rendered_with_readable_audit_details():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _realized_results_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert "main_table = realized_main_table(view.table)" in panel
    assert "if view.table.empty:" not in panel
    assert 'key="surveillance-realized"' in panel
    assert "_render_prediction_audit_details(record)" in panel
    assert "pending_columns = st.columns(2)" not in panel
    assert 'st.caption(' in panel
    assert "Prochaine validation" in panel
    assert "Données jusqu’au" in panel
    assert "Aucun résultat réalisé disponible pour l’instant." not in panel
    assert "if new_results:" in panel


def test_prediction_audit_ui_is_shared_by_predictions_signals_and_results():
    source = APP.read_text(encoding="utf-8")
    helper = source.split("def _render_prediction_audit_details", 1)[1].split(
        "def _render_predictions_tab", 1
    )[0]
    predictions = source.split("def _render_predictions_tab", 1)[1].split(
        "def _render_signals_tab", 1
    )[0]
    signals = source.split("def _render_signals_tab", 1)[1].split(
        "def _realized_results_panel", 1
    )[0]
    realized = source.split("def _realized_results_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert "prediction_feature_tables(record)" in helper
    assert "source_observation_tables(record)" in helper
    assert "Entrées du modèle au moment de la prédiction" in helper
    assert "Observations sources" in helper
    assert "Non disponible pour cette prédiction historique." in helper
    assert "_render_prediction_audit_details(technical)" in predictions
    assert "_render_prediction_audit_details(" in signals
    assert "_render_prediction_audit_details(record)" in realized
    assert source.count("def _render_prediction_audit_details") == 1


def test_history_uses_filtered_paginated_row_selection_without_guid_dropdown():
    source = APP.read_text(encoding="utf-8")

    assert "Ouvrir un run" not in source
    assert "selection_mode=\"single-row\"" in source
    assert "Runs par page" in source
    assert "Type de run" in source
    assert "Promouvoir comme candidat production" in source


def test_history_supports_single_run_detail_and_multi_run_comparison_navigation():
    source = APP.read_text(encoding="utf-8")

    assert 'selection_mode="multi-row"' in source
    assert "Ouvrir le run" in source
    assert "Comparer les runs" in source
    assert "Historique > Détail du run" in source
    assert "Historique > Comparaison de runs" in source
    assert "← Retour à Historique" in source
    assert 'st.tabs(["Résumé", "Analyse", "Combinaisons", "Validation", "Technique"])' in source
    assert 'st.tabs(["Synthèse", "Métriques", "Combinaisons", "Validation", "Technique"])' in source


def test_history_can_duplicate_one_walk_forward_run_into_experiments():
    source = APP.read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert "Dupliquer l’expérience" in history
    assert "JobType.WALK_FORWARD.value" in history
    assert "_start_walk_forward_duplication" in history
    assert "Paramètres à utiliser" in experiments
    assert 'button("Annuler"' in source
    assert "duplication_submission_values" in experiments
    assert "st.switch_page(target_page)" in source


def test_history_grid_clears_persisted_selection_and_uses_compact_actions():
    source = APP.read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]

    assert 'st.session_state[selected_key] = [rows[index].run_id for index in selected_rows]' in history
    assert 'if selected_rows:' not in history
    assert 'actions = st.columns([1.2, 2.4, 6])' in history
    actions = history.split('actions = st.columns([1.2, 2.4, 6])', 1)[1].split(
        'if action == "comparison"', 1
    )[0]
    assert 'width="stretch"' not in actions


def test_duplication_draft_is_only_offered_for_one_run_and_can_be_cancelled():
    source = APP.read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]
    controls = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]

    assert 'if action == "detail"' in history
    assert "JobType.WALK_FORWARD.value" in history
    assert 'st.session_state.pop(DUPLICATION_DRAFT_KEY, None)' in controls


def test_duplication_mode_bypasses_editable_universe_form():
    source = APP.read_text(encoding="utf-8")
    locked = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert "_experiment_universe_selector()" not in locked
    assert "Paramètres à utiliser" in locked
    assert "Soumettre la duplication" in locked
    assert 'button("Annuler"' in locked
    assert "experiment_spec_from_duplication" in locked
    assert "if _render_locked_duplication_mode(service):" in experiments


def test_successful_duplication_returns_to_the_regular_experiments_view():
    source = APP.read_text(encoding="utf-8")
    locked = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert 'st.session_state["duplication-submitted-run-id"] = submitted.run_id' in locked
    assert "st.rerun()" in locked
    assert 'st.session_state.pop("duplication-submitted-run-id", None)' in experiments


def test_comparison_metrics_keeps_quality_and_duration_in_separate_charts():
    source = APP.read_text(encoding="utf-8")
    comparison = source.split("def _render_run_comparison_view", 1)[1].split(
        "def _history_runs_panel", 1
    )[0]

    assert "Qualité prédictive" in comparison
    assert "Durée d’exécution" in comparison
    assert "comparison_chart_frames(analytics, labels)" in comparison
    assert "scale=alt.Scale(domain=[0, 1])" in comparison
    assert "Delta dev→holdout" in comparison
