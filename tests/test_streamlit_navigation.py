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
        "Dashboard",
        "Surveillance",
        "Expériences",
        "Modèles",
        "Historique",
        "Paramètres",
    ):
        assert f'title="{title}"' in source
    assert source.count("st.Page(") == 6


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
