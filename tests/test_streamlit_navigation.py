from pathlib import Path
from types import SimpleNamespace

import rstock.application.streamlit_app as streamlit_app
from rstock.application.streamlit_app import _history_purge_preview, _selected_rows


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
        "Rendement",
        "Expériences",
        "Modèles",
        "Historique",
        "Univers",
        "Paramètres",
        "Documentation",
    ):
        assert f'title="{title}"' in source
    assert 'title="Simulation"' in source
    assert source.count("st.Page(") == 9


def test_temporal_validation_renders_descriptive_candidate_stability():
    source = APP.read_text(encoding="utf-8")
    renderer = source.split(
        "def _render_candidate_identity_stability", 1
    )[1].split("def _render_temporal_validation", 1)[0]

    assert "Stabilité des candidats" in renderer
    assert "candidate_identity_stability" in renderer
    assert "Analyse de stabilité des candidats indisponible" in renderer
    assert "reference_candidate_count" in renderer
    assert "validation_candidate_count" in renderer
    assert "common_candidate_count" in renderer
    assert "lost_candidate_count" in renderer
    assert "new_candidate_count" in renderer
    assert "candidate_survival_rate" in renderer
    assert "validation_overlap_rate" in renderer
    assert "jaccard_index" in renderer
    assert "st.tabs([\"Communs\", \"Perdus\", \"Nouveaux\"])" in renderer
    assert "Aucun candidat commun entre les deux périodes." in renderer
    assert "ne modifie pas la" in renderer


def test_primary_pages_use_one_compact_logo_header_with_a_safe_fallback():
    source = APP.read_text(encoding="utf-8")

    assert LOGO.is_file()
    assert LOGO.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
    header = source.split("def _page_header", 1)[1].split("def _state", 1)[0]
    assert 'rstock_logo.png' in source
    assert 'page_title="RStock"' in source
    assert 'display: flex' in header
    assert 'align-items: center' in header
    assert 'gap: 8px' in header
    assert 'width: 130px' in header
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


def test_simulation_page_offers_both_modes_with_evaluated_predictions_as_default():
    source = APP.read_text(encoding="utf-8")
    page = source.split("def _simulation_page", 1)[1].split(
        "def _primary_pages", 1
    )[0]
    page += source.split("def _render_simulation_main", 1)[1].split(
        "def _documentation_sections", 1
    )[0]

    assert '_page_header("Simulation")' in page
    assert 'SignalService(project_root).active_history()' in page
    assert "SimulationService.local(" in page
    assert '"simulation-mode": "Prédictions évaluées"' in page
    assert 'st.session_state.get("simulation-mode") == "Résultats réalisés"' in page
    assert '"Mode de simulation"' in page
    assert '["Historique", "Prédictions évaluées"]' in page
    assert "service.run_historical(" in page
    assert "service.run(start_date, end_date" in page
    assert '"Lancer la simulation"' in page
    assert 'key="simulation-exit-mode"' in page
    assert "Chaque signal Up représente une " in page
    assert "transaction indépendante. Tous les modèles actifs sont utilisés." in page
    assert "**Tous les modèles actifs**" not in page
    assert 'st.Page(_simulation_page, title="Simulation"' in source


def test_simulation_page_exposes_persisted_history_sidebar_without_replacing_main_controls():
    source = APP.read_text(encoding="utf-8")
    assert "SimulationRepository(project_root)" in source
    assert 'font-size:1.4rem' in source
    assert 'margin-top:50px' in source
    assert 'margin-bottom:10px' in source
    assert 'placeholder="Recherche"' in source
    assert "Simulations précédentes" in source
    assert '"+ Nouvelle simulation", key="new-simulation"' not in source
    assert 'key=f"delete-simulation-{simulation_id}"' in source
    assert 'icon=":material/delete:"' in source
    assert '"Ouvrir la simulation",' in source
    assert 'position: absolute;' in source
    assert 'div[class*="st-key-open-simulation-"] button' in source
    assert '"completed": "Terminée"' in source
    assert 'pnl_text = f"P/L {pnl:+,.0f} $"' in source
    assert 'simulation-card-selected-{simulation_id}' in source
    assert 'SimulationRepository(project_root).save(' in source
    assert 'st.session_state["simulation-record"] = metadata\n                st.rerun()' in source
    assert 'st.columns([1, 4], gap="medium")' in source


def test_simulation_result_distribution_matches_data_quality_panel_width():
    source = APP.read_text(encoding="utf-8")
    results = source.split("def _render_simulation_results", 1)[1].split(
        "def _simulation_model_snapshots", 1
    )[0]

    assert "charts = st.columns([3, 1])" in results
    assert 'st.subheader("Détail des trades")' in results
    assert 'st.subheader("Qualité des données")' in results


def test_simulation_trade_symbol_filter_supports_multiple_symbols_by_default():
    source = APP.read_text(encoding="utf-8")
    results = source.split("def _render_simulation_results", 1)[1].split(
        "def _simulation_model_snapshots", 1
    )[0]

    assert 'filter_columns[0].multiselect(' in results
    assert 'default=["Tous"]' in results
    assert 'filtered_trades["Symbole"].astype(str).isin(active_symbols)' in results


def test_settings_sections_follow_the_experiment_pipeline_order():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split(
        "def _render_qualification", 1
    )[0]
    labels = (
        "Préparation des données et génération",
        "Pré-filtrage des prédicteurs",
        "Walk-forward",
        "Qualification et exécution",
        "Classement des modèles",
        "XGBoost",
        "Calibration des seuils",
        "Validation temporelle",
        "Promotion",
    )

    positions = [settings.index(label) for label in labels]
    assert positions == sorted(positions)
    assert "La promotion automatique se choisit au lancement" in settings


def test_existing_promotion_settings_section_exposes_the_five_frozen_policy_fields():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split(
        "def _render_qualification", 1
    )[0]

    assert settings.count('st.subheader("Promotion")') == 1
    for name in (
        "promotion_min_holdout_signals",
        "promotion_min_holdout_auc",
        "promotion_min_holdout_precision",
        "promotion_min_mean_directional_return",
        "promotion_max_opposite_movement_frequency",
    ):
        assert f"current.{name}" in settings
        assert f"{name}=" in settings
    assert "Le rendement doit être strictement supérieur à cette valeur." in settings


def test_manual_promotion_of_a_non_candidate_requires_an_explicit_override():
    source = APP.read_text(encoding="utf-8")
    promotion = source.split("def _render_threshold_calibration_promotion", 1)[1].split(
        "def _render_threshold_sensitivity_analysis", 1
    )[0]

    assert 'chosen.get("Statut promotion") == "Candidat"' in promotion
    assert "Je confirme une promotion manuelle malgré ces critères." in promotion
    assert "Promouvoir malgré les critères" in promotion


def test_simulation_sidebar_does_not_offer_a_new_simulation_button():
    source = APP.read_text(encoding="utf-8")
    sidebar = source.split("def _simulation_sidebar", 1)[1].split(
        "def _render_simulation_main", 1
    )[0]

    assert 'key="new-simulation"' not in sidebar
    assert '"+ Nouvelle simulation"' not in sidebar


def test_simulation_sidebar_derives_card_selection_from_the_persisted_record():
    source = APP.read_text(encoding="utf-8")
    sidebar = source.split("def _simulation_sidebar", 1)[1].split(
        "def _render_simulation_main", 1
    )[0]

    assert 'selected_record = st.session_state.get("simulation-record")' in sidebar
    assert 'raw_selected_id = (' in sidebar
    assert 'selected_record.get("simulation_id")' in sidebar
    assert 'selected_id = str(raw_selected_id) if raw_selected_id else None' in sidebar
    assert 'selected = (selected_id == simulation_id)' in sidebar
    assert sidebar.index('selected_id = str(raw_selected_id) if raw_selected_id else None') < sidebar.index(
        "for record in visible:"
    )
    assert 'st.session_state["simulation-record"] = metadata' in sidebar
    assert 'st.session_state.pop("simulation-record", None)' in sidebar


def test_persisted_simulation_widget_values_are_restored_with_widget_types():
    source = APP.read_text(encoding="utf-8")
    restoration = source.split("def _parse_simulation_date", 1)[1].split(
        "def _simulation_sidebar", 1
    )[0]

    assert "date.fromisoformat(value[:10])" in restoration
    assert 'st.session_state["simulation-start-date"] = start_date' in restoration
    assert 'st.session_state["simulation-end-date"] = end_date' in restoration
    assert 'st.session_state["simulation-amount"] = float(amount)' in restoration
    assert 'metadata.get("parameters")' in source


def test_documentation_follows_simulation_and_loads_the_markdown_guide_read_only():
    source = APP.read_text(encoding="utf-8")

    assert 'st.Page(_simulation_page, title="Simulation"' in source
    assert 'st.Page(_documentation_page, title="Documentation"' in source
    assert source.index('st.Page(_simulation_page, title="Simulation"') < source.index(
        'st.Page(_documentation_page, title="Documentation"'
    )
    page = source.split("def _documentation_page", 1)[1].split(
        "def _primary_pages", 1
    )[0]
    assert "USER_GUIDE_PATH.read_text(encoding=\"utf-8\")" in page
    assert "st.expander(" in page
    assert "st.markdown(content)" in page


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
    assert "Univers principal : {universe_id}" in selector
    assert "Contexte : {context_label}" in selector
    assert 'with st.expander("Voir les symboles sélectionnés")' in selector
    assert "Cibles résolues" not in selector
    assert "Symboles contexte" not in selector
    assert "Prédicteurs disponibles" not in selector


def test_experiment_sampling_defaults_to_reproducible_for_both_universes():
    source = APP.read_text(encoding="utf-8")
    selector = source.split("def _experiment_universe_selector", 1)[1].split(
        "def _experiments", 1
    )[0]

    assert "index=0 if current.selection_method == TOP_N else 1" in selector
    assert "st.session_state.lab_context_selection_method == TOP_N" in selector
    assert 'key="experiment-sample-method"' in selector
    assert 'key="experiment-context-sample-method"' in selector


def test_experiment_summary_is_compact_and_submission_help_is_secondary():
    source = APP.read_text(encoding="utf-8")
    selector = source.split("def _experiment_universe_selector", 1)[1].split(
        "def _experiments", 1
    )[0]
    experiments = source.split("def _experiments", 1)[1].split(
        "def _settings", 1
    )[0]

    assert "primary_column, context_column = st.columns(2)" in selector
    assert selector.count("st.container(border=True)") == 2
    assert selector.count("st.caption(") == 4  # block summaries, empty context, global summary
    assert selector.index('"Sélection de l’univers principal"') < selector.index(
        'key="experiment-universe-mode"'
    )
    assert "La création et la modification des listes" not in selector
    assert "La liste résolue et la configuration seront figées" not in experiments
    assert "Les listes se gèrent dans Univers; la sélection résolue" in experiments
    assert experiments.index('button("Soumettre l’expérience"') < experiments.index(
        "Les listes se gèrent dans Univers;"
    )


def test_walk_forward_preview_uses_shared_lazy_combination_plan():
    source = APP.read_text(encoding="utf-8")
    preview = source.split("def _combination_plan_preview", 1)[1].split(
        "def _experiments", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split(
        "def _settings", 1
    )[0]

    assert "build_combination_plan(" in preview
    assert "build_combination_preview(" in preview
    assert "JobType.WALK_FORWARD, JobType.END_TO_END" in preview
    assert "Combinaisons brutes exactes" in preview
    assert "Maximum après préfiltrage" in preview
    assert "Batchs max après préfiltrage" in preview
    assert "Batchs requis (preview)" not in preview
    assert "selected_job_type = labels[choice]" in experiments
    assert "_combination_plan_preview(selected_job_type)" in experiments


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
    assert "Benchmark de marché" in creation
    assert "Benchmark de marché" in detail
    assert "benchmark_symbol=benchmark_symbol" in detail
    assert "_market_benchmark_options" in creation
    assert '"Type": "Standard"' in page
    assert '"Benchmark": record.benchmark_symbol' in page
    assert "Univers système protégé" in detail


def test_experiment_submission_and_history_expose_frozen_universe_roles():
    source = APP.read_text(encoding="utf-8")
    experiments = source.split("def _experiments(", 1)[1].split(
        "def _settings", 1
    )[0]
    detail = source.split("def _render_run_detail_view", 1)[1].split(
        "def _render_run_comparison_view", 1
    )[0]
    walk_forward_views = source.split("def _render_walk_forward_summary", 1)[1].split(
        "def _render_pipeline_summary", 1
    )[0]

    for field in (
        "primary_universe_id", "context_universe_ids", "target_symbols",
        "context_symbols", "predictor_symbols",
    ):
        assert f"{field}=" in experiments
    assert "run_universe_summary" in detail
    assert '_page_header("Historique")' in detail
    assert "Historique > Détail du run" in detail
    assert "st.subheader(" in detail
    assert "{universe_summary['target_count']} cibles" in detail
    assert "{universe_summary['predictor_count']} prédicteurs" in detail
    assert "Univers de contexte" not in detail


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
        "Mettre à jour RStock",
        "Prédictions quotidiennes",
        "Détection des signaux",
        "Évaluation des prédictions",
        "Lance la mise à jour quotidienne de bout en bout.",
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
    assert '"Statut"' in models_page
    assert '"Cible"' in models_page
    assert 'filter_models' in models_page
    assert 'models-status-filter' in models_page
    assert 'models-target-filter' in models_page
    assert 'models-predictor-filter' in models_page
    assert 'st.selectbox("Modèle"' not in models_page
    assert '"selected-model-id"' in models_page
    assert "Sélectionnez un modèle dans la grille pour afficher les actions." in models_page
    assert "selected.model_id" in models_page
    assert "Calibration du signal haussier" in models_page
    assert "calibrated_signal_threshold" in models_page
    assert "Seuil de signal : seuil global de décision" in models_page


def test_settings_and_run_detail_expose_predictor_prefilter_controls_and_summary():
    source = APP.read_text(encoding="utf-8")
    walk_forward_views = source.split("def _render_walk_forward_summary", 1)[1].split(
        "def _render_pipeline_summary", 1
    )[0]
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
    assert "predictor_prefilter_summary" in walk_forward_views
    assert 'st.subheader("Pré-filtrage des prédicteurs")' in walk_forward_views


def test_settings_duplication_and_run_detail_expose_walk_forward_window_mode():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split(
        "def _history_filters", 1
    )[0]
    duplication = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]
    summary = source.split("def _render_walk_forward_summary", 1)[1].split(
        "def _render_walk_forward_analysis", 1
    )[0]

    assert '"Mode de fenêtre"' in settings
    assert '["Expansive", "Glissante"]' in settings
    assert '"Taille du train glissant"' in settings
    assert "walk_forward_window_mode=window_mode" in settings
    assert "walk_forward_train_size=int(rolling_train)" in settings
    assert '"Mode de fenêtre WF"' in duplication
    assert '"Taille du train glissant",' not in duplication
    for field in (
        "walk_forward_window_mode",
        "walk_forward_min_train_size",
        "walk_forward_train_size",
        "walk_forward_test_size",
        "walk_forward_step_size",
        "final_holdout_size",
        "walk_forward_end_offset_sessions",
    ):
        assert field in summary


def test_experiment_launch_exposes_local_walk_forward_controls_for_wf_and_end_to_end():
    source = APP.read_text(encoding="utf-8")
    launch = source.split("def _experiments", 1)[1].split("def _settings", 1)[0]
    confirmation = source.split(
        "def _render_experiment_submission_confirmation", 1
    )[1].split("def _experiment_universe_selector", 1)[0]

    assert '"Mode de fenêtre Walk-forward"' in launch
    assert '["Expansive", "Glissante"]' in launch
    assert '"Taille du train glissant",' not in launch
    assert "walk_forward_launch_controls_visible(selected_job_type)" in launch
    assert "launch_walk_forward_config(" in launch
    assert "config=run_config" in launch
    assert "st.session_state.lab_config =" not in launch
    assert "walk_forward_confirmation_text(spec.config)" in confirmation


def test_settings_and_history_expose_resumable_walk_forward_controls():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split("def _models", 1)[0]
    detail = source.split("def _render_resume_controls", 1)[1].split(
        "def _render_history_detail", 1
    )[0]

    for label in (
        "Batch préfiltre",
        "Batch walk-forward",
        "Batch holdout final",
        "Taille maximale d’un batch de combinaisons",
    ):
        assert label in settings
    for field in (
        "predictor_prefilter_batch_size",
        "walk_forward_batch_size",
        "final_holdout_batch_size",
        "walk_forward_max_combinations_per_batch",
    ):
        assert field in settings
    assert (
        "Contrôle uniquement le découpage des combinaisons en batches" in settings
    )
    assert "Reprendre le run" in detail
    assert "Relancer depuis le début" in detail
    assert "checkpoint_error" in detail
    assert "completed_phases" in detail


def test_settings_and_combination_detail_expose_model_selection_scores():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings", 1)[1].split(
        "def _history_filters", 1
    )[0]
    detail = source.split("def _render_run_detail_view", 1)[1].split(
        "def _render_run_comparison_view", 1
    )[0]
    walk_forward_views = source.split("def _render_walk_forward_summary", 1)[1].split(
        "def _render_pipeline_summary", 1
    )[0]

    assert 'st.subheader("Classement des modèles")' in settings
    for name in (
        "model_selection_predictive_quality_weight",
        "model_selection_stability_weight",
        "model_selection_holdout_weight",
        "model_selection_signal_quality_weight",
        "model_selection_sample_adequacy_weight",
    ):
        assert name in settings
    for label in (
        "Seuil calibré", "Qualité signal", "Sous-scores", "Score final", "Rang"
    ):
        assert label in walk_forward_views


def test_surveillance_uses_one_conditional_page_level_polling_fragment():
    source = APP.read_text(encoding="utf-8")

    assert "_polling_surveillance_page = st.fragment(run_every=2)" in source
    assert "surveillance_refresh_decision(runs, polling=False)" in source
    assert "st.rerun(scope=\"app\")" in source
    assert "_evaluated_predictions_panel = st.fragment" not in source


def test_surveillance_is_flat_and_keeps_on_demand_technical_details():
    source = APP.read_text(encoding="utf-8")
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "def _surveillance_page", 1
    )[0]

    assert 'st.tabs(["Prédictions", "Signaux", "Prédictions évaluées"])' not in source
    assert "_render_predictions_tab" not in source
    assert "_render_signals_card(" in surveillance
    assert "_render_signals_followup(" in surveillance
    assert "_evaluated_predictions_panel(evaluated_view, runs, project_root=project_root)" in surveillance
    assert "main, sidebar = st.columns([2.25, 1], gap=\"large\")" in surveillance
    assert (
        "_render_priorities_panel(priority_view.signals, priority_model_metrics)"
        in surveillance
    )
    assert "Autres prédictions sans signal" in source
    assert "Prédictions évaluées récemment" in source
    assert "Détails techniques" in source
    assert "Pourquoi ce signal ?" in source
    assert surveillance.index("_render_signals_card") < surveillance.index(
        "_render_signals_followup"
    ) < surveillance.index("_evaluated_predictions_panel") < surveillance.index(
        '_live_job_panel(_service(), domain="production")'
    )
    assert 'expanded=False' in source


def test_real_trade_action_is_rendered_before_pending_predictions():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _evaluated_predictions_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert panel.index('key="surveillance-evaluated-predictions"') < panel.index(
        "_render_real_trade_from_prediction(selected_record, project_root=project_root)"
    ) < panel.index("if not displayed_view.pending.empty:")


def test_real_trade_form_uses_prices_with_two_decimals_and_integer_quantity():
    source = APP.read_text(encoding="utf-8")
    form = source.split("def _render_real_trade_from_prediction", 1)[1].split(
        "def _evaluated_predictions_panel", 1
    )[0]

    assert form.count('format="%.2f"') == 2
    assert '"Quantité", min_value=1,' in form
    assert "step=1," in form
    panel = source.split("def _evaluated_predictions_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]
    assert "displayed_view.pending" in panel


def test_surveillance_uses_one_daily_operational_update_card():
    source = APP.read_text(encoding="utf-8")
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "def _surveillance_page", 1
    )[0]
    card = source.split("def _render_daily_update_card", 1)[1].split(
        "def _load_evaluated_predictions_view", 1
    )[0]
    job_panel = source.split("def _job_panel", 1)[1].split(
        "def _live_job_panel", 1
    )[0]

    assert "Mise à jour quotidienne" in card
    assert card.count('"Mettre à jour RStock"') == 1
    assert card.count("st.button(") == 1
    assert "Lance la mise à jour quotidienne de bout en bout." in card
    assert "JobType.OPERATIONAL_RUN" in card
    assert "_DAILY_UPDATE_STAGES" in card
    assert "st.error" in card
    assert "Mise à jour quotidienne terminée." in card
    assert "_render_daily_update_card(" in surveillance
    assert "active_model_count=len(universe.model_ids), stretch=True" in surveillance
    assert 'top_main, top_sidebar = st.columns([2.25, 1], gap="large")' in surveillance
    assert surveillance.index("_render_daily_update_card") < surveillance.index(
        "_render_priorities_panel"
    )
    assert "_render_production_actions(" not in surveillance
    assert surveillance.count('_live_job_panel(_service(), domain="production")') == 1
    assert 'domain="experiment"' not in surveillance
    assert 'domain="model"' not in surveillance
    assert job_panel.index("if not runs:") < job_panel.index("title = job_domain_title")


def test_surveillance_top_cards_use_an_equal_height_row():
    source = APP.read_text(encoding="utf-8")
    styles = source.split("def _surveillance_styles", 1)[1].split(
        "def _render_page_header", 1
    )[0]
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "def _surveillance_page", 1
    )[0]

    row_selector = (
        'div[data-testid="stHorizontalBlock"]'
        ':has(.rstock-signals-card-marker)'
        ':has(.rstock-daily-update-card-marker)'
    )
    assert row_selector in styles
    assert "align-items: stretch;" in styles
    assert (
        'div[data-testid="stLayoutWrapper"]'
        ":has(.rstock-signals-card-marker)"
    ) in styles
    assert (
        'div[data-testid="stLayoutWrapper"]'
        ":has(.rstock-daily-update-card-marker)"
    ) in styles
    assert "height: 100%;" in styles
    assert "stretch=True" in surveillance
    assert "active_model_count=len(universe.model_ids), stretch=True" in surveillance


def test_surveillance_expander_counts_match_their_displayed_grids():
    source = APP.read_text(encoding="utf-8")
    signals = source.split("def _render_signals_followup", 1)[1].split(
        "def _render_signals_section", 1
    )[0]
    evaluated = source.split("def _evaluated_predictions_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert 'f"Autres prédictions sans signal ({len(displayed.no_signal.table)})"' in signals
    assert 'f"Prédictions évaluées récemment ({len(view.table)})"' in evaluated


def test_surveillance_uses_active_views_and_clears_stale_row_selections():
    source = APP.read_text(encoding="utf-8")
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "def _surveillance_page", 1
    )[0]
    models = source.split("def _models_page", 1)[1].split(
        "def _history_page", 1
    )[0]

    assert ".active_history()" in surveillance
    assert "active_realized_results()" in source
    assert "models.active_models()" in source
    assert models.count("_invalidate_surveillance_selection_state()") == 3


def test_surveillance_uses_four_compact_kpi_cards_and_a_header_status():
    source = APP.read_text(encoding="utf-8")
    surveillance = source.split("def _render_surveillance_page", 1)[1].split(
        "if hasattr(st, \"fragment\")", 1
    )[0]
    kpis = source.split("def _render_surveillance_kpis", 1)[1].split(
        "def _styled_signal_table", 1
    )[0]
    header = source.split("def _render_surveillance_header", 1)[1].split(
        "def _render_surveillance_kpis", 1
    )[0]

    assert "_render_surveillance_header(" in surveillance
    assert "_render_surveillance_kpis(" in surveillance
    assert 'columns = st.columns(4, gap="small")' in kpis
    for label in (
        "Signaux haussiers aujourd’hui",
        "Prédictions en attente",
        "Modèles actifs",
        "Dernière mise à jour",
    ):
        assert label in kpis
    assert "with st.container(border=True):" in kpis
    assert "error_count" in header
    assert "_freshness_state(freshness)" in header
    assert 'timestamp.strftime("%Y-%m-%d %H:%M")' in source


def test_surveillance_priorities_are_compact_limited_and_have_an_empty_state():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _render_priorities_panel", 1)[1].split(
        "def _render_operational_info", 1
    )[0]
    card = source.split("def _priority_card_html", 1)[1].split(
        "def _render_priorities_panel", 1
    )[0]

    assert '"Priorités du jour",' in panel
    assert '"Signaux classés par pertinence opérationnelle."' in panel
    assert "model_metrics_by_id=model_metrics_by_id" in panel
    assert "limit=3" in panel
    assert "_priority_card_html(index + 1, row)" in panel
    assert "Aucune priorité pour le moment." in panel
    assert "Voir tous les signaux" in panel
    assert "Score {score}" in card
    assert "P(Up) {probability}" in card
    assert "pts vs seuil" in card
    assert "Précision {precision}" in card
    assert "Rend. {directional_return}" in card
    assert "Opposé {opposite}" in card


def test_surveillance_css_is_scoped_to_its_page_marker():
    source = APP.read_text(encoding="utf-8")
    styles = source.split("def _surveillance_styles", 1)[1].split(
        "def _freshness_state", 1
    )[0]

    assert "rstock-surveillance-scope" in styles
    assert ':has(.rstock-surveillance-scope)' in styles
    assert "rstock-priority-card" in styles
    assert "max-width: 1520px" in styles


def test_evaluated_predictions_grid_is_always_rendered_with_readable_audit_details():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _evaluated_predictions_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert "main_table = evaluated_predictions_main_table(displayed_view.table)" in panel
    assert "if view.table.empty:" not in panel
    assert 'key="surveillance-evaluated-predictions"' in panel
    assert "_render_prediction_audit_details(selected_record)" in panel
    assert "pending_columns = st.columns(2)" not in panel
    assert 'st.caption(' in panel
    assert "Prochaine validation" in panel
    assert "Données jusqu’au" in panel
    assert "evaluation_feedback(new_results, view)" in panel
    assert "if new_results:" in panel


def test_prediction_audit_ui_is_shared_by_signals_and_results():
    source = APP.read_text(encoding="utf-8")
    helper = source.split("def _render_prediction_audit_details", 1)[1].split(
        "def _render_signals_section", 1
    )[0]
    signals = source.split("def _render_signals_section", 1)[1].split(
        "def _priority_card_html", 1
    )[0]
    followup = source.split("def _render_signals_followup", 1)[1].split(
        "def _render_signals_section", 1
    )[0]
    realized = source.split("def _evaluated_predictions_panel", 1)[1].split(
        "def _render_surveillance_page", 1
    )[0]

    assert "prediction_feature_tables(record)" in helper
    assert "source_observation_tables(record)" in helper
    assert "Entrées du modèle au moment de la prédiction" in helper
    assert "Observations sources" in helper
    assert "Non disponible pour cette prédiction historique." in helper
    assert "_render_signals_followup(displayed, selected_signal, models)" in signals
    assert "_render_prediction_audit_details(" in followup
    assert "_render_prediction_audit_details(selected_record)" in realized
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


def test_history_comparison_header_uses_compact_parent_identity_and_run_summaries():
    source = APP.read_text(encoding="utf-8")
    comparison = source.split("def _render_run_comparison_view", 1)[1].split(
        "def _history_runs_panel", 1
    )[0]

    assert '_page_header("Historique")' in comparison
    assert "Historique > Comparaison de runs" in comparison
    assert 'st.subheader("Comparaison de runs — Walk-forward")' in comparison
    assert "Run {chr(64 + index)}" in comparison
    assert "symboles · profondeur" in comparison
    assert "column.caption(" in comparison
    assert "ID technique : {analysis.run_id}" in comparison
    assert "st.title(\"Comparaison de runs" not in comparison


def test_history_comparison_uses_an_arrow_safe_display_matrix():
    source = APP.read_text(encoding="utf-8")
    comparison = source.split("def _render_run_comparison_view", 1)[1].split(
        "def _history_runs_panel", 1
    )[0]

    assert "comparison_display_table(summary)" in comparison


def test_generic_history_detail_uses_the_persisted_summary_as_its_title():
    source = APP.read_text(encoding="utf-8")
    detail = source.split("def _render_history_detail", 1)[1].split(
        "def _history_navigation", 1
    )[0]

    assert 'st.subheader(summary_text if summary_text != "—" else "Détail du run")' in detail
    assert 'st.caption(f"ID technique : {run_id}")' in detail


def test_non_walk_forward_details_hide_generic_run_metadata():
    source = APP.read_text(encoding="utf-8")
    detail = source.split("def _render_history_detail", 1)[1].split(
        "def _history_navigation", 1
    )[0]

    assert 'columns = st.columns(5)' not in detail
    assert 'columns[0].metric("Type"' not in detail
    assert 'columns[4].metric("Contexte"' not in detail
    assert 'st.caption(f"ID technique : {run_id}")' in detail


def test_history_can_duplicate_one_walk_forward_run_into_experiments():
    source = APP.read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert "Dupliquer l’expérience" in history
    assert "JobType.WALK_FORWARD.value" in history
    assert "_start_walk_forward_duplication" in history
    assert "_render_locked_duplication_mode(service)" in experiments
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
    assert 'actions = st.columns([1.2, 2.4, 2.6, 4])' in history
    assert '"Purger les données lourdes"' in history
    assert "_history_purge_preview(service, selected_run_id)" in history
    assert history.index('"Purger les données lourdes"') < history.index(
        "_history_purge_preview(service, selected_run_id)"
    )
    actions = history.split('actions = st.columns([1.2, 2.4, 2.6, 4])', 1)[1].split(
        'if action == "comparison"', 1
    )[0]
    assert 'width="stretch"' not in actions


def test_history_uses_lazy_native_tabs_for_expensive_content():
    source = APP.read_text(encoding="utf-8")
    page = source.split("def _history_page", 1)[1].split(
        "def _history_runs_panel", 1
    )[0]

    assert 'key="history-tabs"' in page
    assert 'on_change="rerun"' in page
    assert "if tabs[0].open:" in page
    assert "if tabs[1].open:" in page
    assert "if tabs[2].open:" in page
    assert page.index("if tabs[0].open:") < page.index("_history_runs_panel(")
    assert page.index("if tabs[2].open:") < page.index("predictions = PredictionService")


def test_history_grid_uses_lightweight_records_and_defers_purge_eligibility():
    source = APP.read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]
    grid_setup = history.split("selection = st.dataframe", 1)[0]

    assert "service.history_runs(job_types=allowed_types)" in history
    assert "details_by_run_id" in history
    assert "service.run(" not in grid_setup
    assert "service.purge_eligibility" not in history
    assert history.index('actions[2].button(') < history.index(
        "_history_purge_preview(service, selected_run_id)"
    )


def test_history_purge_is_checked_only_after_explicit_request(monkeypatch):
    calls = []

    class Service:
        def purge_eligibility(self, run_id):
            calls.append(("eligibility", run_id))
            return SimpleNamespace(eligible=True, reason=None)

        def purge_preview(self, run_id):
            calls.append(("preview", run_id))
            return SimpleNamespace(reclaimable_bytes=123)

    assert _history_purge_preview(Service(), "run-1") == 123
    assert calls == [("eligibility", "run-1"), ("preview", "run-1")]


def test_history_purge_ineligible_request_is_blocked_with_its_reason(monkeypatch):
    messages = []

    class Service:
        def purge_eligibility(self, run_id):
            return SimpleNamespace(eligible=False, reason="Un enfant est actif.")

        def purge_preview(self, run_id):
            raise AssertionError("preview must not run when eligibility fails")

    monkeypatch.setattr(streamlit_app.st, "info", messages.append)

    assert _history_purge_preview(Service(), "run-1") is None
    assert messages == ["Un enfant est actif."]


def test_stale_grid_selection_is_dropped_after_row_population_shrinks():
    event = SimpleNamespace(selection=SimpleNamespace(rows=[7]))

    assert _selected_rows(event, row_count=2) == []


def test_valid_grid_selection_is_preserved():
    event = SimpleNamespace(selection=SimpleNamespace(rows=[1]))

    assert _selected_rows(event, row_count=2) == [1]


def test_history_grid_uses_the_run_provenance_display_columns():
    source = APP.read_text(encoding="utf-8")
    history_ui = (APP.parent / "history_ui.py").read_text(encoding="utf-8")
    history = source.split("def _history_runs_panel", 1)[1].split(
        "def _experiments_page", 1
    )[0]

    assert '"Run ID": self.run_id' in history_ui
    assert '"Lignée": self.lineage' in history_ui
    assert "pd.DataFrame([row.display() for row in rows])" in history


def test_forward_detail_offers_recovery_only_through_the_central_diagnosis():
    source = APP.read_text(encoding="utf-8")
    controls = source.split("def _render_resume_controls", 1)[1].split(
        "def _render_history_detail", 1
    )[0]

    assert "JobType.FORWARD_SIMULATION.value" in controls
    assert "forward_recovery_diagnosis(run_id)" in controls
    assert '"Reprendre cette simulation"' in controls
    assert "resume_forward_simulation(run_id)" in controls


def test_forward_results_expose_persisted_data_quality_kpis_and_diagnostics():
    source = APP.read_text(encoding="utf-8")
    results = source.split("def _render_standard_results", 1)[1].split(
        "def _render_standard_job_tabs", 1
    )[0]

    for label in (
        "Observations évaluées",
        "Observations exclues",
        "Taux d’observations évaluables",
        "Anomalies de données uniques",
        "evaluability_rate",
        "Certaines observations n’ont pas pu être évaluées",
        "forward_exclusions.csv",
    ):
        assert label in results
    assert "head(500)" in results


def test_normal_experiment_submission_persists_context_sampling_metadata():
    source = APP.read_text(encoding="utf-8")
    selector = source.split("def _experiment_universe_selector", 1)[1].split(
        "def _experiments", 1
    )[0]
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]

    assert "lab_context_sample_size = context_sample_size" in selector
    assert "lab_context_selection_method = context_method" in selector
    assert "lab_context_seed = context_seed" in selector
    assert "context_sample_size=st.session_state.lab_context_sample_size" in experiments
    assert "context_selection_method=st.session_state.lab_context_selection_method" in experiments
    assert "context_seed=st.session_state.lab_context_seed" in experiments


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
    assert "Paramètres de calibration XGBoost" in locked
    assert "Paramètres de calibration des seuils" in locked
    assert "paramètres XGBoost du walk-forward source seront conservés" in locked
    assert "configurations XGBoost Up/Down sélectionnées" in locked
    assert "current_combinations_per_target" in locked
    assert "Soumettre la duplication" in locked
    assert 'button("Annuler"' in locked
    assert "experiment_spec_from_duplication" in locked
    assert 'st.selectbox(\n            "Type de job"' in locked
    assert "DUPLICATION_JOB_TYPE_KEY" in locked
    assert "JOB_TYPE_BY_LABEL" in locked
    assert "if _render_locked_duplication_mode(service):" in experiments


def test_threshold_parameter_calibration_has_experiment_option_and_detail_grid():
    source = APP.read_text(encoding="utf-8")
    experiments = source.split("def _experiments(", 1)[1].split("def _settings", 1)[0]
    detail = source.split(
        "def _render_threshold_parameter_calibration_selection", 1
    )[1].split("def _promote_combination_action", 1)[0]

    assert "Calibration des paramètres de seuils" in experiments
    assert "JobType.THRESHOLD_PARAMETER_CALIBRATION" in experiments
    assert "Paramètres du processus de calibration des seuils" in source
    assert "Configuration gagnante" in detail
    assert "Candidats testés" in detail
    assert "Candidats admissibles" in detail
    assert "Écart avec #2" in detail
    assert "Parent direct" in detail
    assert "Source XGBoost" in detail
    assert "threshold_parameter_calibration_table" in detail
    assert "Paramètres gagnants complets" in detail


def test_duplication_summary_uses_arrow_safe_display_values_and_safe_job_fallback():
    source = APP.read_text(encoding="utf-8")
    locked = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]

    assert 'summary["Valeur"] = summary["Valeur"].astype(str)' in locked
    assert "normalize_duplication_job_type" in locked
    assert "job_type_fallback_message" in locked
    assert "next(" not in locked


def test_duplication_job_type_selectbox_uses_session_state_without_an_index_default():
    source = APP.read_text(encoding="utf-8")
    locked = source.split("def _render_locked_duplication_mode", 1)[1].split(
        "def _service", 1
    )[0]
    widget = locked.split('st.selectbox(\n            "Type de job"', 1)[1].split(
        ")\n        selected_job_type", 1
    )[0]

    assert "key=DUPLICATION_JOB_TYPE_KEY" in widget
    assert "index=" not in widget


def test_threshold_calibration_filter_defaults_are_session_safe():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _render_threshold_calibration_promotion", 1)[1].split(
        "def _render_history_detail", 1
    )[0]

    assert "policy = promotion_policy(rstock_config)" in panel
    for value in (
        '"Up"',
        'policy["promotion_min_holdout_signals"]',
        'policy["promotion_min_holdout_precision"]',
        'policy["promotion_min_holdout_auc"]',
        'policy["promotion_max_opposite_movement_frequency"]',
        'policy[\n            "promotion_min_mean_directional_return"\n        ]',
    ):
        assert value in panel
    assert "st.session_state.setdefault(key, value)" in panel
    assert '"Signaux holdout minimum", min_value=0, step=1' in panel
    assert '"Précision holdout minimale", min_value=0.0, max_value=1.0,' in panel


def test_threshold_calibration_selected_row_exposes_read_only_holdout_sensitivity():
    source = APP.read_text(encoding="utf-8")
    panel = source.split("def _render_threshold_calibration_promotion", 1)[1].split(
        "def _render_history_detail", 1
    )[0]

    assert "_render_threshold_sensitivity_analysis(" in panel
    assert "Analyse de sensibilité au seuil — Holdout" in panel
    assert "load_threshold_holdout_predictions" in panel
    assert "threshold_sensitivity_table(" in panel
    assert "threshold_sensitivity_summary(" in panel
    assert "Synthèse de sensibilité des seuils" in panel
    assert "Diagnostic du choix du seuil — calibration" in panel
    assert "threshold_calibration_choice_diagnostic_table(" in panel
    assert "Analyse de sensibilité indisponible pour ce run historique." in panel
    assert "Sélection calibration · seuil" in panel
    assert "directional_return_mean" in panel
    assert "opposite_move_frequency" in panel
    assert "selection_reason" in panel


def test_sensitivity_threshold_settings_are_rendered_and_used_for_holdout_analysis():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings()", 1)[1].split(
        "def _history_model_contexts", 1
    )[0]
    analysis = source.split("def _render_threshold_sensitivity_analysis", 1)[1].split(
        "def _render_history_detail", 1
    )[0]

    for label in (
        "Seuil min — analyse de sensibilité",
        "Seuil max — analyse de sensibilité",
        "Pas — analyse de sensibilité",
    ):
        assert label in settings
    for key in (
        "sensitivity_threshold_min",
        "sensitivity_threshold_max",
        "sensitivity_threshold_step",
    ):
        assert key in settings
        assert key in analysis


def test_up_precision_tolerance_setting_is_rendered_and_saved_with_calibration():
    source = APP.read_text(encoding="utf-8")
    settings = source.split("def _settings()", 1)[1].split(
        "def _history_model_contexts", 1
    )[0]

    assert "Tolérance de précision — sélection Up" in settings
    assert "threshold_calibration_precision_tolerance" in settings
    assert "1 point de pourcentage" in settings


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
