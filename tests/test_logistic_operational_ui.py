from pathlib import Path
import pytest
from streamlit.testing.v1 import AppTest


@pytest.mark.parametrize('algorithm,variables,disabled', [
    ('logistic_l2','external_only',False), ('xgboost','external_only',False),
    ('logistic_l2','target_only',True), ('logistic_l2','target_and_external',True),
    ('constant_probability','external_only',True),
])
def test_actual_e2e_form_operational_controls(algorithm, variables, disabled):
    # The fixture calls the actual form; only external services are replaced.
    fixture = Path(__file__).resolve().parent/'fixtures/logistic_operational_form.py'
    app = AppTest.from_file(str(fixture)).run()
    assert not app.exception
    select = next(x for x in app.selectbox if x.label == 'Algorithme prédictif')
    select.select(algorithm).run()
    if algorithm != 'constant_probability':
        next(x for x in app.selectbox if x.label == 'Type de variables').select(variables).run()
    assert not app.exception
    controls = [x for x in app.checkbox if x.label in {
        'Lancer une Forward Simulation après succès', 'Promouvoir automatiquement les candidats admissibles'}]
    assert len(controls) == 2 and all(x.disabled == disabled for x in controls)


def test_logistic_temporal_control_is_enabled_and_keeps_cutoff_guard():
    fixture = Path(__file__).resolve().parent/'fixtures/logistic_operational_form.py'
    app = AppTest.from_file(str(fixture)).run()
    temporal = next(x for x in app.checkbox if x.label == 'Exécuter une validation temporelle')
    assert not temporal.disabled
    temporal.check().run()
    assert any('cutoff historique' in x.value for x in app.error)
    app.session_state['fixture_cutoff'] = None
    app.run()
    assert not app.exception and not app.error
    assert next(x for x in app.checkbox if x.label == 'Exécuter une validation temporelle').value
