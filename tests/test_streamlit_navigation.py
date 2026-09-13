from pathlib import Path


APP = (
    Path(__file__).resolve().parents[1]
    / "rstock"
    / "application"
    / "streamlit_app.py"
)


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


def test_models_and_surveillance_pages_expose_the_operational_flow():
    source = APP.read_text(encoding="utf-8")

    assert "Catalogue futur" not in source
    for label in (
        "Promouvoir comme candidat production",
        "Entraîner",
        "Activer",
        "Désactiver",
        "Retirer",
        "Mettre à jour le marché",
        "Prédictions quotidiennes",
        "Screening",
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
    assert "_render_signals_tab(signals, models)" in source
    assert "_realized_results_panel(predictions, signals)" in source
    assert "Voir les prédictions sans signal" in source
    assert "Détails techniques" in source
    assert "Pourquoi ce signal ?" in source


def test_history_uses_filtered_paginated_row_selection_without_guid_dropdown():
    source = APP.read_text(encoding="utf-8")

    assert "Ouvrir un run" not in source
    assert "selection_mode=\"single-row\"" in source
    assert "Runs par page" in source
    assert "Type de run" in source
    assert "Promouvoir comme candidat production" in source
