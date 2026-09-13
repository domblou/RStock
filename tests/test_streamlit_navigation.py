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
