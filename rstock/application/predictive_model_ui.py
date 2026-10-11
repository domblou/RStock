"""Shared selector for the existing Streamlit configuration forms."""
PREDICTIVE_MODEL_LABELS = {
    "external_only": "Externes seulement", "constant_probability": "Probabilité constante",
    "target_only": "Titre cible seul", "target_and_external": "Titre cible + externes",
}


def predictive_model_input(current: str, key: str, widget=None) -> str:
    if widget is None:
        import streamlit as widget
    options = tuple(PREDICTIVE_MODEL_LABELS)
    return widget.selectbox("Type de modèle prédictif", options, index=options.index(current),
                            format_func=PREDICTIVE_MODEL_LABELS.get, key=key)
