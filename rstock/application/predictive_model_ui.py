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

def prefilter_configuration_input(config, key, widget=None):
    if widget is None:
        import streamlit as widget
    algorithms = ('xgboost','logistic_l2')
    changes = {'prefilter_algorithm':widget.selectbox('Algorithme du préfiltre',algorithms,
        index=algorithms.index(config.prefilter_algorithm),format_func=ALGORITHM_LABELS.get,key=key+'-algorithm')}
    if changes['prefilter_algorithm'] == 'logistic_l2':
        from rstock.prefilter_audit import PREFILTER_MODEL_FIELDS
        for name,label in zip(PREFILTER_MODEL_FIELDS[1:],('Pénalité L2 du préfiltre','Maximum d’itérations du préfiltre',
                'Tolérance du gradient du préfiltre','Tolérance de l’objectif du préfiltre')):
            value = getattr(config,name)
            changes[name] = widget.number_input(label,min_value=1 if isinstance(value,int) else 1e-15,
                value=value,step=1 if isinstance(value,int) else .01 if name.endswith('penalty') else value/10,
                format='%d' if isinstance(value,int) else '%.8g',
                key=key+'-'+name)
    widget.caption('Sélection Up seulement. Probabilité constante : référence train, sans classement.')
    return replace(config,**changes)
def predictive_configuration_input(config, key, widget=None):
    if widget is None:
        import streamlit as widget
    current = "constant_probability" if config.predictive_model_type == "constant_probability" else config.predictive_algorithm
    algorithm = widget.selectbox("Algorithme prédictif", tuple(ALGORITHM_LABELS),
        index=tuple(ALGORITHM_LABELS).index(current), format_func=ALGORITHM_LABELS.get, key=key+"-algorithm")
    variables = config.predictive_model_type if config.predictive_model_type in VARIABLE_LABELS else "external_only"
    selected = widget.selectbox("Type de variables", tuple(VARIABLE_LABELS),
        index=tuple(VARIABLE_LABELS).index(variables), format_func=VARIABLE_LABELS.get,
        disabled=algorithm == "constant_probability", key=key+"-variables")
    if algorithm == "constant_probability":
        widget.caption("Variables : sans objet pour la probabilité constante.")
    changes = {"predictive_algorithm": algorithm,
               "predictive_model_type": "constant_probability" if algorithm == "constant_probability" else selected}
    if algorithm == "logistic_l2":
        for name, label in (("logistic_l2_penalty", "Pénalité L2"),
                            ("logistic_max_iterations", "Maximum d’itérations"),
                            ("logistic_gradient_tolerance", "Tolérance du gradient"),
                            ("logistic_objective_tolerance", "Tolérance de l’objectif")):
            if name == "logistic_max_iterations":
                changes[name] = int(widget.number_input(label, min_value=1, value=getattr(config, name), step=1, key=key+"-"+name))
            else:
                changes[name] = float(widget.number_input(label, min_value=1e-15, value=getattr(config, name),
                    step=0.01 if name == "logistic_l2_penalty" else getattr(config, name)/10,
                    format="%.8g", key=key+"-"+name))
    return replace(config, **changes)
