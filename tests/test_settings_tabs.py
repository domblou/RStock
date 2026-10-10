"""Exercise the settings page without importing the application entry or any job."""
import ast
from collections import Counter
from dataclasses import replace
import json
import inspect
from pathlib import Path

from streamlit.testing.v1 import AppTest

from rstock.config import DEFAULT_CONFIG, load_user_settings, user_settings_path

APP = Path(__file__).parents[1] / "rstock/application/streamlit_app.py"
TAB_LABELS = (
    "Univers et données", "Modèles et combinaisons", "Préfiltre", "Walk-forward",
    "Calibrations", "Holdout et promotion", "Forward et production", "Avancé",
)


def settings_app(tmp_path):
    # Render the actual functions, with their real persistence and validation.
    # The entry module also starts navigation/services; avoid that side effect.
    script = '''
import ast
from dataclasses import asdict, replace
from pathlib import Path
import streamlit as st
from rstock.config import DEFAULT_CONFIG, RStockConfig, load_user_settings
from rstock.config import save_user_settings
from rstock.application.prefilter_experiments import PREFILTER_XGBOOST_FIELDS
from rstock.modeling import PREFILTER_ROUND_SELECTION_FIELDS
DEFAULT_CONFIG = replace(DEFAULT_CONFIG, project_root=Path(ROOT))
if "lab_config" not in st.session_state:
    config, ui, warning = load_user_settings(DEFAULT_CONFIG)
    st.session_state.update(ui)
    st.session_state.lab_config = config
    if warning:
        st.session_state._settings_load_warning = warning
tree = ast.parse(Path(SOURCE).read_text(encoding="utf-8"))
names = {"_settings", "_prefilter_temporal_settings", "_round_selection_input",
         "_prefilter_round_selection_inputs", "_prefilter_xgboost_settings"}
functions = ast.Module(body=[node for node in tree.body
    if isinstance(node, ast.FunctionDef) and node.name in names], type_ignores=[])
_page_header = st.title
exec(compile(functions, SOURCE, "exec"), globals())
_settings()
'''.replace("ROOT", repr(str(tmp_path))).replace("SOURCE", repr(str(APP)))
    return AppTest.from_string(script, default_timeout=20).run()


def widget(app, kind, label):
    matches = [item for item in getattr(app, kind) if item.label == label]
    assert len(matches) == 1, (kind, label, len(matches))
    return matches[0]


def test_tabs_preserve_all_existing_controls_and_defaults(tmp_path):
    app = settings_app(tmp_path)
    assert not app.exception
    assert tuple(tab.label for tab in app.tabs) == TAB_LABELS
    actual = Counter((kind, item.label) for kind in
                     ("number_input", "text_input", "selectbox", "checkbox", "button")
                     for item in getattr(app, kind))
    # Frozen inventory from the settings page before its reorganisation.
    assert actual == Counter([*EXPECTED_CONTROLS, ("selectbox", "Type de modèle prédictif")])
    before = replace(DEFAULT_CONFIG, project_root=tmp_path)
    assert app.session_state["lab_config"] == before
    assert not user_settings_path(tmp_path).exists()
    assert not app.tabs[6].number_input and not app.tabs[6].selectbox
    assert "Types de modèles" in [item.value for item in app.tabs[1].subheader]
    assert widget(app, "selectbox", "Type de modèle prédictif").value == "external_only"
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert not app.exception
    assert app.session_state["lab_config"] == before
    loaded, _, warning = load_user_settings(before)
    assert loaded == before and warning is None


def test_cross_tab_edits_save_and_reload_with_independent_xgboost_modes(tmp_path):
    app = settings_app(tmp_path)
    original = app.session_state["lab_config"]
    widget(app, "number_input", "Historique (jours)").set_value(2400).run()
    widget(app, "number_input", "Permutation depth").set_value(3).run()
    widget(app, "selectbox", "Sélection des tours XGBoost du préfiltre").select("chronological").run()
    widget(app, "number_input", "Tours maximum (préfiltre)").set_value(350).run()
    widget(app, "selectbox", "Sélection des tours XGBoost (Walk-forward)").select("chronological").run()
    widget(app, "number_input", "Tours maximum").set_value(400).run()
    widget(app, "number_input", "Holdout final").set_value(100).run()
    widget(app, "text_input", "Quantiles de la grille").set_value("0.25, 0.5, 0.75").run()
    widget(app, "number_input", "Threads XGBoost").set_value(2).run()
    # All eight containers remain rendered on every rerun. Tab navigation is
    # client-side (no lazy callbacks), so it cannot clean up hidden widgets.
    app.run()
    assert not app.exception
    assert tuple(tab.label for tab in app.tabs) == TAB_LABELS
    assert widget(app, "number_input", "Historique (jours)").value == 2400
    assert widget(app, "number_input", "Tours maximum (préfiltre)").value == 350
    assert widget(app, "number_input", "Tours maximum").value == 400
    assert app.session_state["lab_config"] == original
    assert not user_settings_path(tmp_path).exists()
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert not app.exception and app.success
    expected = replace(original, model_history_days=2400, permutation_depth=3,
        prefilter_xgb_round_selection_mode="chronological",
        prefilter_xgb_early_stopping_max_rounds=350,
        xgb_round_selection_mode="chronological", xgb_early_stopping_max_rounds=400,
        final_holdout_size=100, threshold_calibration_quantiles=(0.25, 0.5, 0.75),
        xgb_nthread=2)
    assert app.session_state["lab_config"] == expected
    reloaded = settings_app(tmp_path)
    assert not reloaded.exception
    assert reloaded.session_state["lab_config"] == expected
    assert widget(reloaded, "selectbox", "Sélection des tours XGBoost (Walk-forward)").value == "chronological"
    assert widget(reloaded, "number_input", "Tours maximum (préfiltre)").value == 350


def test_dependent_controls_and_cross_tab_validation(tmp_path):
    app = settings_app(tmp_path)
    widget(app, "selectbox", "Mode de fenêtre").select("Glissante").run()
    assert widget(app, "number_input", "Train minimal").disabled
    assert not widget(app, "number_input", "Taille du train glissant").disabled
    app.selectbox(key="settings-prefilter-selection-mode").select("temporal_consensus").run()
    assert not app.number_input(key="settings-consensus-origins").disabled
    widget(app, "checkbox", "Sans plafond de modèles directionnels").check().run()
    assert widget(app, "number_input", "Nombre maximal de modèles directionnels").disabled
    widget(app, "number_input", "Holdout final").set_value(100).run()
    original = app.session_state["lab_config"]
    # Keep the existing validation path: invalid quantiles cannot apply/save
    # otherwise valid edits from another tab.
    widget(app, "text_input", "Quantiles de la grille").set_value("invalid, 0.5").run()
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert app.exception
    assert app.session_state["lab_config"] == original
    assert not user_settings_path(tmp_path).exists()
    widget(app, "text_input", "Quantiles de la grille").set_value("0.25, 0.5").run()
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert not app.exception
    assert app.session_state["lab_config"].threshold_parameter_calibration_max_models is None
    assert app.session_state["lab_config"].walk_forward_window_mode == "rolling"


def test_historical_settings_open_without_implicit_write(tmp_path):
    path = user_settings_path(tmp_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"config": {"xgb_rounds": 80, "xgb_max_depth": 4},
               "ui": {"lab_calendar": "XNYS"}}
    path.write_text(json.dumps(payload), encoding="utf-8")
    before = path.read_bytes()
    app = settings_app(tmp_path)
    assert not app.exception
    assert widget(app.tabs[3], "number_input", "num_boost_round").value == 80
    assert widget(app, "selectbox", "Sélection des tours XGBoost (Walk-forward)").value == "fixed"
    assert widget(app, "selectbox", "Sélection des tours XGBoost du préfiltre").value == "fixed"
    app.run()
    assert not app.exception and path.read_bytes() == before


def test_persistence_failure_preserves_existing_session_semantics(tmp_path, monkeypatch):
    def fail(*args, **kwargs):
        raise OSError("test: write unavailable")
    monkeypatch.setattr("rstock.config.save_user_settings", fail)
    app = settings_app(tmp_path)
    widget(app, "number_input", "Holdout final").set_value(100).run()
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert not app.exception and app.error
    assert app.session_state["lab_config"].final_holdout_size == 100
    assert not user_settings_path(tmp_path).exists()


def test_real_streamlit_entry_starts_on_settings_without_key_errors(tmp_path, monkeypatch):
    from rstock.application.services import MarketDataService

    class SettingsPage:
        def run(self):
            # Exercise the real script globals and the existing page wrapper.
            namespace = inspect.currentframe().f_back.f_globals
            namespace["_settings_page"]()

    monkeypatch.setattr("streamlit.navigation", lambda *a, **k: SettingsPage())
    monkeypatch.setattr(MarketDataService, "available_symbols", lambda *a: [])
    app = AppTest.from_file(str(APP), default_timeout=20)
    app.session_state["lab_config"] = replace(DEFAULT_CONFIG, project_root=tmp_path)
    app.run()
    assert not app.exception
    assert tuple(tab.label for tab in app.tabs) == TAB_LABELS
    assert not user_settings_path(tmp_path).exists()


# Historical rendered inventory (including shared Prefilter controls).
EXPECTED_CONTROLS = (
    ("number_input", "Tendance SPY (séances)"),
    ("number_input", "Drawdown SPY (séances)"),
    ("number_input", "Volatilité SPY (séances)"),
    ("number_input", "Historique (jours)"),
    ("number_input", "Lag depth"),
    ("number_input", "Permutation depth"),
    ("number_input", "Seuil intraday hausse"),
    ("number_input", "Max generated sets"),
    ("number_input", "Seuil intraday baisse"),
    ("number_input", "Top N prédicteurs"),
    ("number_input", "Worst AUC minimal"),
    ("number_input", "AUC médiane minimale"),
    ("number_input", "Dispersion AUC maximale"),
    ("number_input", "Part minimale de fenêtres > 0,50"),
    ("number_input", "Seuil de corrélation"),
    ("number_input", "Nombre d'origines"),
    ("number_input", "Espacement en séances"),
    ("number_input", "Occurrences minimales"),
    ("number_input", "max_depth"),
    ("number_input", "min_child_weight"),
    ("number_input", "gamma"),
    ("number_input", "seed"),
    ("number_input", "eta"),
    ("number_input", "subsample"),
    ("number_input", "reg_alpha"),
    ("number_input", "num_boost_round"),
    ("number_input", "colsample_bytree"),
    ("number_input", "reg_lambda"),
    ("number_input", "Tours maximum (préfiltre)"),
    ("number_input", "Validation interne — séances utilisables distinctes"),
    ("number_input", "Patience — log loss (préfiltre)"),
    ("number_input", "Apprentissage interne minimum — séances utilisables distinctes"),
    ("number_input", "Train minimal"),
    ("number_input", "Taille du train glissant"),
    ("number_input", "Taille test"),
    ("number_input", "Step"),
    ("number_input", "Holdout final"),
    ("number_input", "Décalage de fin (jours de marché)"),
    ("number_input", "Batch préfiltre"),
    ("number_input", "Batch walk-forward"),
    ("number_input", "Batch holdout final"),
    ("number_input", "Taille maximale d’un batch de combinaisons"),
    ("number_input", "Fenêtres minimales"),
    ("number_input", "Pire ROC-AUC minimal"),
    ("number_input", "ROC-AUC confirmation finale"),
    ("number_input", "ROC-AUC médian minimal"),
    ("number_input", "Observations positives minimales"),
    ("number_input", "Seuil de décision standard"),
    ("number_input", "Part fenêtres > hasard"),
    ("number_input", "Écart-type ROC-AUC maximal"),
    ("number_input", "Poids qualité prédictive"),
    ("number_input", "Poids qualité signal"),
    ("number_input", "Poids stabilité"),
    ("number_input", "Poids adéquation échantillon"),
    ("number_input", "Poids holdout"),
    ("number_input", "max_depth"),
    ("number_input", "min_child_weight"),
    ("number_input", "gamma"),
    ("number_input", "eta"),
    ("number_input", "subsample"),
    ("number_input", "reg_alpha"),
    ("number_input", "num_boost_round"),
    ("number_input", "colsample_bytree"),
    ("number_input", "reg_lambda"),
    ("number_input", "Tours maximum"),
    ("number_input", "Validation interne (séances utilisables)"),
    ("number_input", "Patience (log loss)"),
    ("number_input", "Apprentissage interne minimum"),
    ("number_input", "Signaux minimaux par fenêtre"),
    ("number_input", "Fraction minimale de fenêtres"),
    ("number_input", "Signaux totaux minimum pour un seuil robuste"),
    ("number_input", "Nombre maximal de modèles directionnels"),
    ("number_input", "Tolérance de précision — sélection Up"),
    ("number_input", "Seuil min — analyse de sensibilité"),
    ("number_input", "Seuil max — analyse de sensibilité"),
    ("number_input", "Pas — analyse de sensibilité"),
    ("number_input", "Combinaisons par cible (calibrations)"),
    ("number_input", "Seed"),
    ("number_input", "Ratio minimal de candidats"),
    ("number_input", "Rendement directionnel minimal"),
    ("number_input", "Dégradation maximale de l’AUC"),
    ("number_input", "Niveau de confiance"),
    ("number_input", "Écart de précision minimal"),
    ("number_input", "Largeur maximale de l’IC pour l’écart de précision"),
    ("number_input", "Signaux holdout minimum"),
    ("number_input", "AUC holdout minimum"),
    ("number_input", "Précision holdout minimum"),
    ("number_input", "Rendement directionnel minimum"),
    ("number_input", "Mouvements opposés maximum"),
    ("number_input", "Workers marché"),
    ("number_input", "Jobs lourds concurrents"),
    ("number_input", "Workers combinaisons"),
    ("number_input", "Threads XGBoost"),
    ("text_input", "Calendrier"),
    ("text_input", "Quantiles de la grille"),
    ("selectbox", "Mode de sélection"),
    ("selectbox", "Sélection des tours XGBoost du préfiltre"),
    ("selectbox", "Mode de fenêtre"),
    ("selectbox", "Sélection des tours XGBoost (Walk-forward)"),
    ("checkbox", "Activer le diagnostic descriptif SPY"),
    ("checkbox", "Terciles descriptifs figés sur le développement initial"),
    ("checkbox", "Activer le pré-filtrage"),
    ("checkbox", "Évaluer le holdout final"),
    ("checkbox", "Sans plafond de modèles directionnels"),
    ("button", "Enregistrer les paramètres"),
)



def test_constant_type_disables_prefilter_and_xgboost_and_persists(tmp_path):
    app = settings_app(tmp_path)
    widget(app, "selectbox", "Type de modèle prédictif").select("constant_probability").run()
    assert not app.exception
    assert widget(app, "checkbox", "Activer le pré-filtrage").disabled
    assert widget(app, "number_input", "Top N prédicteurs").disabled
    assert widget(app, "selectbox", "Mode de sélection").disabled
    assert widget(app.tabs[3], "number_input", "max_depth").disabled
    assert widget(app, "selectbox", "Sélection des tours XGBoost (Walk-forward)").disabled
    widget(app, "button", "Enregistrer les paramètres").click().run()
    assert not app.exception
    assert app.session_state["lab_config"].predictive_model_type == "constant_probability"
    assert app.session_state["lab_config"].predictor_prefilter_enabled is False
