"""Persisted synthetic summaries only; no real archive or scientific execution."""
from copy import deepcopy
from types import SimpleNamespace

import pytest

from rstock.application import rare_relations_analysis as rare, scientific_analysis as analysis, scientific_analysis_ui as ui


def evidence(selected=0, evaluable=0, indeterminate=0, sustained=0, stop="aucune relation sélectionnée"):
    result = {"complete": True, "definition_version":"3", "rules":dict(rare.RULES),
        "temporal_summary":{"selection_sessions":180,"evaluation_sessions":120,"total_sessions":300},
        "population_count":5, "admissible_count":3, "selected_count":selected,
        "evaluable_count":evaluable,"indeterminate_count":indeterminate,"sustained_count":sustained,
        "sustained_fraction":sustained/selected if selected else None, "stop_reason":stop,
        "reason_counts":{"selection":{"gain relatif inférieur à 1 %":2}}}
    manifest = {"analysis_id":rare.ANALYSIS_ID,"definition_version":"3","parameters":{"rules":dict(rare.RULES)}}
    return result, manifest, {"status":"completed"}


@pytest.mark.parametrize("counts,expected", [
    ((0,0,0,0,"aucune relation sélectionnée"), "Cela ne démontre pas l'absence de signal"),
    ((2,0,2,0,"aucune évaluation concluante"), "insuffisantes pour conclure"),
    ((2,1,1,0,None), "Persistance non démontrée"),
    ((2,1,1,1,None), "soutenue pour 1 relation"),
])
def test_scientific_branches_are_deterministic_and_use_the_full_denominator(counts, expected):
    args = evidence(*counts)
    before = deepcopy(args)
    value = rare.interpret_results(*args)
    assert expected in value["verdict"]
    assert value == rare.interpret_results(*args) and args == before
    analysis.validate_interpretation(value)
    findings = " ".join(value["findings"])
    if counts[0] == 0:
        assert "sans objet" in findings and "grille de persistance vide" in findings
        assert "0.0%" not in findings
    if counts[3]:
        assert "50.0%" in findings  # 1/2 selected, not 1/1 evaluable
    assert "gain relatif inférieur à 1 %" in findings
    assert "non validés empiriquement" in " ".join(value["limits"])


def test_no_admissible_relation_differs_from_no_selection():
    result, manifest, status = evidence(stop="aucune relation admissible")
    result["admissible_count"] = 0
    value = rare.interpret_results(result, manifest, status)
    assert "insuffisantes" in value["verdict"]
    assert "Aucune des 5 relations" in " ".join(value["findings"])


@pytest.mark.parametrize("mutation", [
    lambda r,m,s: r.pop("selected_count"),
    lambda r,m,s: r.update(selected_count=6),
    lambda r,m,s: r.update(evaluable_count=1),
    lambda r,m,s: r.update(sustained_fraction=0),
    lambda r,m,s: r.update(reason_counts=None),
    lambda r,m,s: r.update(admissible_count=-1),
    lambda r,m,s: r.pop("rules"),
    lambda r,m,s: r.pop("sustained_fraction"),
    lambda r,m,s: r.pop("stop_reason"),
    lambda r,m,s: r.update(reason_counts={"evaluation":{"couverture incomplète":99}}),
    lambda r,m,s: m.update(parameters=None),
    lambda r,m,s: m.update(definition_version="2"),
    lambda r,m,s: m["parameters"]["rules"].update(alpha=.1),
    lambda r,m,s: m["parameters"].update(resolved_split={"selection_sessions":10}),
])
def test_incompatible_or_missing_evidence_never_becomes_zero_or_success(mutation):
    args = evidence()
    mutation(*args)
    value = rare.interpret_results(*args)
    assert "indisponible" in value["verdict"]
    assert "interprétation fiable" in " ".join(value["findings"])


@pytest.mark.parametrize("state", ["pending","running","failed","cancelled","interrupted"])
def test_incomplete_jobs_have_no_final_verdict(state):
    result, manifest, status = evidence()
    status["status"] = state
    assert "final indisponible" in rare.interpret_results(result, manifest, status)["verdict"]


def test_old_supported_summaries_and_persisted_rules_do_not_use_modern_defaults(monkeypatch):
    args = evidence()
    args[0].pop("admissible_count")
    args[0].pop("reason_counts")
    monkeypatch.setattr(rare, "RULES", {**rare.RULES,"alpha":.2})
    value = rare.interpret_results(*args)
    assert "Holm ≤ 5%" in " ".join(value["understanding"])
    assert "résumé ancien" in " ".join(value["findings"])


def test_summary_renders_all_three_sections_without_scientific_reads_or_ai(monkeypatch):
    args = evidence()
    shown = []
    st = SimpleNamespace(subheader=shown.append,markdown=shown.append,write=shown.append)
    def forbidden(*a, **k):
        pytest.fail("No CSV/snapshot calculation in interpretation")
    monkeypatch.setattr(rare.pd, "read_csv", forbidden)
    monkeypatch.setattr(rare, "score_chunk", forbidden)
    ui.render_scientific_interpretation(st, rare.definition(), *args)
    assert "Interprétation scientifique" in shown
    for label in ("Comprendre les résultats","Interprétation des résultats","Limites et verdict scientifique"):
        assert f"**{label}**" in shown
    assert any("grille de persistance vide" in text for text in shown)


def test_persisted_text_requires_matching_evidence_digest():
    args = evidence()
    saved = {**rare.interpret_results(*args), "input_sha256":analysis.interpretation_inputs_hash(*args)}
    shown = []
    st = SimpleNamespace(subheader=shown.append,markdown=shown.append,write=shown.append)
    ui.render_scientific_interpretation(st, rare.definition(), *args, saved)
    assert any("aucune relation" in text.casefold() for text in shown)
    args[0]["population_count"] = 6
    shown.clear()
    ui.render_scientific_interpretation(st, rare.definition(), *args, saved)
    assert any("indisponible" in text for text in shown)
    args = evidence()
    saved = {**rare.interpret_results(*args), "input_sha256":analysis.interpretation_inputs_hash(*args), "verdict":"Avantage futur prouvé"}
    shown.clear()
    ui.render_scientific_interpretation(st, rare.definition(), *args, saved)
    assert "Verdict : Avantage futur prouvé" not in shown
    assert any("indisponible" in text for text in shown)


def test_analysis_registration_requires_a_specific_interpreter():
    from dataclasses import replace
    with pytest.raises(ValueError,match="interpretation_required"):
        analysis.register_analysis(replace(rare.definition(), interpret_results=None))


def test_summary_interpretation_is_outside_closed_json_expander(tmp_path, monkeypatch):
    import json
    from contextlib import contextmanager
    result, manifest, status = evidence()
    root = tmp_path / "results"
    root.mkdir()
    (root / "analysis_result.json").write_text(json.dumps(result), encoding="utf-8")
    (root / "analysis_preparation.json").write_text(json.dumps(manifest), encoding="utf-8")
    manifest["owner_run_id"] = "source"
    inside = [False]
    rendered = []
    @contextmanager
    def expander(label, expanded):
        assert expanded is False
        inside[0] = True
        yield
        inside[0] = False
    def write(text):
        assert not inside[0], "Interpretation must remain visible without opening JSON"
        rendered.append(text)
    st = SimpleNamespace(subheader=write,markdown=write,write=write,expander=expander,
                         json=lambda *_: None,button=lambda *a, **k: False)
    service = SimpleNamespace(run_service=SimpleNamespace(repository=SimpleNamespace(run_directory=lambda _: tmp_path)))
    monkeypatch.setattr(ui, "get_analysis", lambda _: rare.definition())
    monkeypatch.setattr(ui, "render_lazy_tabs", lambda st, tabs, renderers, **kw: renderers["summary"]())
    ui.render_analysis_detail(st, service, "fixture", status, {"configuration":{"scientific_analysis":manifest}},
                              render_technical=lambda *a,**kw:None,navigate=lambda *_: None)
    assert "Interprétation scientifique" in rendered
    assert any("grille de persistance vide" in text for text in rendered)
