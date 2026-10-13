"""Shared algorithm and variable selectors for research configuration forms."""
from dataclasses import replace

ALGORITHM_LABELS = {
    "xgboost": "XGBoost", "logistic_l2": "Régression logistique L2",
    "constant_probability": "Probabilité constante",
}
VARIABLE_LABELS = {
    "external_only": "Externes seulement", "target_only": "Titre cible seul",
    "target_and_external": "Titre cible + externes",
}

def logistic_parameters_input(config, key, widget, *, disabled=False, prefilter=False):
    fields = (
        ("logistic_l2_penalty", "Pénalité L2"),
        ("logistic_max_iterations", "Maximum d’itérations"),
        ("logistic_gradient_tolerance", "Tolérance du gradient"),
        ("logistic_objective_tolerance", "Tolérance de l’objectif"),
    )
    changes = {}
    for name, label in fields:
        if prefilter:
            name = "prefilter_" + name
            label += " du préfiltre"
        value = getattr(config, name)
        changes[name] = widget.number_input(
            label, min_value=1 if isinstance(value, int) else 1e-15,
            value=value, step=1 if isinstance(value, int) else .01 if name.endswith('penalty') else value/10,
            format='%d' if isinstance(value, int) else '%.8g', key=key+'-'+name,
            disabled=disabled)
    return changes


def prefilter_configuration_input(config, key, widget=None, *, boxed_logistic=False, disabled=False):
    if widget is None:
        import streamlit as widget
    algorithms = ('xgboost','logistic_l2')
    changes = {'prefilter_algorithm':widget.selectbox('Algorithme du préfiltre',algorithms,
        index=algorithms.index(config.prefilter_algorithm),format_func=ALGORITHM_LABELS.get,key=key+'-algorithm', disabled=disabled)}
    if boxed_logistic:
        with widget.container(border=True):
            widget.subheader('Régression logistique L2 du préfiltre')
            changes.update(logistic_parameters_input(config, key, widget, prefilter=True,
                disabled=disabled or changes['prefilter_algorithm'] != 'logistic_l2'))
    elif changes['prefilter_algorithm'] == 'logistic_l2':
        changes.update(logistic_parameters_input(config, key, widget, prefilter=True, disabled=disabled))
    widget.caption('Sélection Up seulement. Probabilité constante : référence train, sans classement.')
    return replace(config,**changes)
def predictive_configuration_input(config, key, widget=None, *, show_variables=True,
                                   boxed_logistic=False, algorithm_label="Algorithme prédictif"):
    if widget is None:
        import streamlit as widget
    current = "constant_probability" if config.predictive_model_type == "constant_probability" else config.predictive_algorithm
    algorithm = widget.selectbox(algorithm_label, tuple(ALGORITHM_LABELS),
        index=tuple(ALGORITHM_LABELS).index(current), format_func=ALGORITHM_LABELS.get, key=key+"-algorithm")
    variables = config.predictive_model_type if config.predictive_model_type in VARIABLE_LABELS else "external_only"
    selected = widget.selectbox("Type de variables", tuple(VARIABLE_LABELS),
        index=tuple(VARIABLE_LABELS).index(variables), format_func=VARIABLE_LABELS.get,
        disabled=algorithm == "constant_probability", key=key+"-variables") if show_variables else variables
    if algorithm == "constant_probability" and show_variables:
        widget.caption("Variables : sans objet pour la probabilité constante.")
    changes = {"predictive_algorithm": algorithm,
               "predictive_model_type": "constant_probability" if algorithm == "constant_probability" else selected}
    if boxed_logistic:
        with widget.container(border=True):
            widget.subheader('Régression logistique L2')
            changes.update(logistic_parameters_input(config, key, widget, disabled=algorithm != 'logistic_l2'))
    elif algorithm == "logistic_l2":
        changes.update(logistic_parameters_input(config, key, widget))
    return replace(config, **changes)
