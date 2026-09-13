from dataclasses import replace

import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.application.universes import (
    MANUAL_SOURCE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    TOP_N,
    UniverseSelection,
    UniverseService,
)
from rstock.config import DEFAULT_CONFIG


@pytest.fixture
def service():
    return UniverseService({"TEST": ("AAA", "BBB", "CCC", "DDD", "EEE")})


def test_manual_list_is_preserved(service):
    resolved = service.resolve(
        UniverseSelection(source=MANUAL_SOURCE), manual_symbols=("aaa", "BBB")
    )
    assert resolved.symbols == ("AAA", "BBB")


def test_saved_universe_and_top_n_are_source_ordered(service):
    saved = service.resolve(UniverseSelection(source=SAVED_SOURCE, universe="TEST"))
    top = service.resolve(
        UniverseSelection(
            source=SAMPLE_SOURCE, universe="TEST", sample_size=3, selection_method=TOP_N
        )
    )
    assert saved.symbols == ("AAA", "BBB", "CCC", "DDD", "EEE")
    assert top.symbols == ("AAA", "BBB", "CCC")


def test_seeded_sample_is_reproducible_and_seed_sensitive(service):
    selection = UniverseSelection(
        source=SAMPLE_SOURCE, universe="TEST", sample_size=3,
        selection_method=SEEDED_SAMPLE, seed=1234,
    )
    first = service.resolve(selection)
    assert first == service.resolve(selection)
    changed = service.resolve(replace(selection, seed=4321))
    assert changed.symbols != first.symbols


def test_sample_rejects_request_larger_than_source(service):
    with pytest.raises(ValueError, match="exceeds universe size"):
        service.resolve(
            UniverseSelection(
                source=SAMPLE_SOURCE, universe="TEST", sample_size=6,
                selection_method=TOP_N,
            )
        )


def test_run_config_freezes_resolved_symbols_independent_of_later_universe_changes(tmp_path, service):
    selection = UniverseSelection(
        source=SAMPLE_SOURCE, universe="TEST", sample_size=3,
        selection_method=SEEDED_SAMPLE, seed=1234,
    )
    resolved = service.resolve(selection)
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.symbols,
        universe_selection=selection,
    )
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(spec)
    saved = repository.read_json(run_id, "config.json")

    assert saved["symbols"] == list(resolved.symbols)
    assert saved["universe_selection"] == selection.as_dict()
    changed_source = UniverseService({"TEST": ("ZZZ", "YYY", "XXX")})
    assert changed_source.resolve(
        UniverseSelection(source=SAVED_SOURCE, universe="TEST")
    ).symbols != resolved.symbols
    restored = repository.load_spec(run_id)
    assert restored.symbols == resolved.symbols
    assert restored.universe_selection == selection
