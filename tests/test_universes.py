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


def test_persistent_manual_universe_normalizes_deduplicates_and_reloads(tmp_path):
    service = UniverseService(root=tmp_path)

    created = service.create("Tech US", " aapl, MSFT\naapl\n nvda ")
    reloaded = UniverseService(root=tmp_path).record(created.universe_id)

    assert created.symbols == ("AAPL", "MSFT", "NVDA")
    assert reloaded == created
    assert (tmp_path / "data" / "universes" / f"{created.universe_id}.csv").exists()
    assert (tmp_path / "data" / "universes" / "universes.json").exists()


def test_csv_import_supports_symbol_or_selected_column(tmp_path):
    service = UniverseService(root=tmp_path)
    standard = service.create_from_csv("Standard", b"symbol,name\naapl,Apple\nMSFT,Microsoft\n")
    selected = service.create_from_csv(
        "Ticker column", "ticker,label\namd,AMD\nNVDA,Nvidia\n", column="ticker"
    )

    assert standard.symbols == ("AAPL", "MSFT")
    assert standard.source == "Import CSV"
    assert selected.symbols == ("AMD", "NVDA")


def test_update_duplicate_and_delete_persistent_universe(tmp_path):
    service = UniverseService(root=tmp_path)
    created = service.create("Initial", ("AAA", "BBB"))

    updated = service.update(created.universe_id, name="Modifié", symbols="CCC, ddd, CCC")
    copied = service.duplicate(created.universe_id)
    service.delete(created.universe_id)
    reloaded = UniverseService(root=tmp_path)

    assert updated.name == "Modifié"
    assert updated.symbols == ("CCC", "DDD")
    assert copied.name == "Modifié (copie)"
    assert copied.symbols == updated.symbols
    assert created.universe_id not in reloaded.universe_names()
    assert copied.universe_id in reloaded.universe_names()


def test_system_demo_is_compatible_protected_and_duplicable(tmp_path):
    service = UniverseService(root=tmp_path)
    demo = service.record("US_STOCKS_DEMO")

    assert demo.name == "Test — 15 titres"
    assert demo.system is True
    assert len(demo.symbols) == 15
    assert service.duplicate(demo.universe_id).symbols == demo.symbols
    with pytest.raises(ValueError, match="protégés"):
        service.delete(demo.universe_id)


def test_persisted_run_stays_frozen_after_saved_universe_is_edited(tmp_path):
    service = UniverseService(root=tmp_path)
    universe = service.create("Research", ("AAA", "BBB", "CCC"))
    selection = UniverseSelection(source=SAVED_SOURCE, universe=universe.universe_id)
    resolved = service.resolve(selection)
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.symbols,
        universe_selection=selection,
    )
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(spec)

    service.update(universe.universe_id, name="Research", symbols=("ZZZ", "YYY"))

    assert repository.load_spec(run_id).symbols == ("AAA", "BBB", "CCC")
    assert service.resolve(selection).symbols == ("ZZZ", "YYY")


def test_persisted_run_stays_frozen_after_saved_universe_is_deleted(tmp_path):
    service = UniverseService(root=tmp_path)
    universe = service.create("Disposable", ("AAA", "BBB", "CCC"))
    selection = UniverseSelection(source=SAVED_SOURCE, universe=universe.universe_id)
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=service.resolve(selection).symbols,
        universe_selection=selection,
    )
    repository = RunRepository(tmp_path / "runs")
    run_id = repository.create(spec)

    service.delete(universe.universe_id)

    assert universe.universe_id not in service.universe_names()
    assert repository.load_spec(run_id).symbols == ("AAA", "BBB", "CCC")
