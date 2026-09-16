from types import SimpleNamespace

from rstock.application.model_ui import (
    filter_models,
    job_domain,
    job_domain_title,
    model_filter_options,
    reconcile_selected_model_id,
)


def _models():
    return [
        SimpleNamespace(model_id="active-a", target="AAA", predictors=("SPY", "VIX"), status="active"),
        SimpleNamespace(model_id="candidate-b", target="BBB", predictors=("QQQ",), status="candidate"),
        SimpleNamespace(model_id="retired-c", target="AAA", predictors=("VIX",), status="retired"),
    ]


def test_retired_models_are_excluded_by_default_and_can_be_shown():
    models = _models()

    assert [model.model_id for model in filter_models(models)] == ["active-a", "candidate-b"]
    assert [model.model_id for model in filter_models(models, statuses={"retired"})] == ["retired-c"]
    assert model_filter_options(models) == ["active", "candidate", "retired"]


def test_target_and_case_insensitive_predictor_filters_can_be_combined():
    visible = filter_models(
        _models(),
        statuses={"active", "candidate", "retired"},
        targets={"AAA"},
        predictor_query="vIx",
    )

    assert [model.model_id for model in visible] == ["active-a", "retired-c"]


def test_empty_results_and_selection_reconciliation_are_safe():
    models = _models()

    assert filter_models(models, statuses={"active"}, targets={"ZZZ"}) == []
    assert filter_models(models, statuses=[]) == []
    assert reconcile_selected_model_id("active-a", models[1:]) == "candidate-b"
    assert reconcile_selected_model_id("candidate-b", models[1:]) == "candidate-b"
    assert reconcile_selected_model_id("active-a", []) is None


def test_job_domains_are_contextual_and_have_page_titles():
    assert job_domain("walk_forward") == "experiment"
    assert job_domain("daily_prediction") == "production"
    assert job_domain("production_training") == "model"
    assert job_domain_title("production") == "Jobs actifs — Production"
    assert job_domain_title("experiment") == "Jobs actifs — Expériences"
    assert job_domain_title("model") == "Jobs actifs — Modèles"
    assert job_domain("unknown_job") is None
