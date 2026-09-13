from dataclasses import replace

import pytest

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.repository import RunRepository
from rstock.application.universes import (
    CONTEXT_UNIVERSE_TYPE,
    MANUAL_SOURCE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    STANDARD_UNIVERSE_TYPE,
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


def test_historical_universe_without_type_defaults_to_standard(tmp_path):
    directory = tmp_path / "data" / "universes"
    directory.mkdir(parents=True)
    (directory / "LEGACY.csv").write_text("symbol\nAAA\nBBB\n", encoding="utf-8")
    (directory / "universes.json").write_text(
        '{"universes":[{"universe_id":"LEGACY","name":"Legacy","source":"Manuel"}]}',
        encoding="utf-8",
    )

    record = UniverseService(root=tmp_path).record("LEGACY")

    assert record.type == STANDARD_UNIVERSE_TYPE


def test_context_universe_is_persisted_editable_and_cannot_be_primary(tmp_path):
    service = UniverseService(root=tmp_path)
    created = service.create(
        "Market context", ("SPY", "QQQ"), universe_type=CONTEXT_UNIVERSE_TYPE
    )
    reloaded = UniverseService(root=tmp_path).record(created.universe_id)

    assert reloaded.type == CONTEXT_UNIVERSE_TYPE
    with pytest.raises(ValueError, match="context universe"):
        service.resolve(
            UniverseSelection(source=SAVED_SOURCE, universe=created.universe_id)
        )
    updated = service.update(
        created.universe_id,
        name="Market context",
        symbols=("SPY", "TLT"),
        universe_type=STANDARD_UNIVERSE_TYPE,
    )
    assert updated.type == STANDARD_UNIVERSE_TYPE
    assert service.resolve(
        UniverseSelection(source=SAVED_SOURCE, universe=created.universe_id)
    ).symbols == ("SPY", "TLT")


def test_experiment_resolution_accepts_multiple_contexts_and_deduplicates_symbols():
    service = UniverseService({
        "PRIMARY": ("AAA", "BBB"),
        "STANDARD_CONTEXT": ("BBB", "CCC"),
    })
    contextual = service.create(
        "Context only", ("CCC", "DDD"), universe_type=CONTEXT_UNIVERSE_TYPE
    )

    resolved = service.resolve_experiment(
        UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY"),
        ("STANDARD_CONTEXT", contextual.universe_id, "STANDARD_CONTEXT"),
    )

    assert resolved.primary_universe_id == "PRIMARY"
    assert resolved.context_universe_ids == (
        "STANDARD_CONTEXT", contextual.universe_id,
    )
    assert resolved.target_symbols == ("AAA", "BBB")
    assert resolved.context_symbols == ("CCC", "DDD")
    assert resolved.predictor_symbols == ("AAA", "BBB", "CCC", "DDD")


def test_context_sampling_uses_the_same_top_n_policy_without_creating_targets():
    service = UniverseService({
        "PRIMARY": ("AAA", "BBB"),
        "CONTEXT": ("BBB", "CCC", "DDD", "EEE"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")

    full_context = service.resolve_experiment(primary, ("CONTEXT",))
    top_two = service.resolve_experiment(
        primary,
        ("CONTEXT",),
        context_sample_size=2,
        context_selection_method=TOP_N,
    )

    assert full_context.context_symbols == ("CCC", "DDD", "EEE")
    assert top_two.target_symbols == ("AAA", "BBB")
    assert top_two.context_symbols == ("CCC", "DDD")
    assert top_two.predictor_symbols == ("AAA", "BBB", "CCC", "DDD")
    assert len(top_two.predictor_symbols) == 4


def test_context_sampling_supports_the_same_reproducible_sample_policy():
    service = UniverseService({
        "PRIMARY": ("AAA",),
        "CONTEXT": ("BBB", "CCC", "DDD", "EEE"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")

    first = service.resolve_experiment(
        primary, ("CONTEXT",), context_sample_size=2,
        context_selection_method=SEEDED_SAMPLE, context_seed=1234,
    )
    second = service.resolve_experiment(
        primary, ("CONTEXT",), context_sample_size=2,
        context_selection_method=SEEDED_SAMPLE, context_seed=1234,
    )

    assert first.context_symbols == second.context_symbols
    assert len(first.context_symbols) == 2


def test_context_sampling_never_falls_back_to_the_complete_universe():
    service = UniverseService({
        "PRIMARY": ("AAA",),
        "CONTEXT": ("BBB", "CCC", "DDD"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")

    with pytest.raises(ValueError, match="sample_size must be positive"):
        service.resolve_experiment(
            primary,
            ("CONTEXT",),
            context_selection_method=TOP_N,
        )


def test_context_top_n_snapshot_keeps_sampling_metadata_and_resolved_lists(tmp_path):
    service = UniverseService({
        "PRIMARY": ("AAA", "BBB"),
        "CONTEXT": ("CCC", "DDD", "EEE"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")
    resolved = service.resolve_experiment(
        primary,
        ("CONTEXT",),
        context_sample_size=2,
        context_selection_method=TOP_N,
    )
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.predictor_symbols,
        universe_selection=primary,
        primary_universe_id=resolved.primary_universe_id,
        context_universe_ids=resolved.context_universe_ids,
        context_sample_size=2,
        context_selection_method=TOP_N,
        target_symbols=resolved.target_symbols,
        context_symbols=resolved.context_symbols,
        predictor_symbols=resolved.predictor_symbols,
    )

    snapshot = spec.to_dict()

    assert snapshot["context_sample_size"] == 2
    assert snapshot["context_selection_method"] == TOP_N
    assert snapshot["context_seed"] is None
    assert snapshot["target_symbols"] == ["AAA", "BBB"]
    assert snapshot["context_symbols"] == ["CCC", "DDD"]
    assert snapshot["predictor_symbols"] == ["AAA", "BBB", "CCC", "DDD"]


def test_full_context_snapshot_keeps_null_sampling_metadata(tmp_path):
    service = UniverseService({
        "PRIMARY": ("AAA", "BBB"),
        "CONTEXT": ("CCC", "DDD"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")
    resolved = service.resolve_experiment(primary, ("CONTEXT",))
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.predictor_symbols,
        universe_selection=primary,
        primary_universe_id=resolved.primary_universe_id,
        context_universe_ids=resolved.context_universe_ids,
        target_symbols=resolved.target_symbols,
        context_symbols=resolved.context_symbols,
        predictor_symbols=resolved.predictor_symbols,
    )

    snapshot = spec.to_dict()

    assert snapshot["context_sample_size"] is None
    assert snapshot["context_selection_method"] is None
    assert snapshot["context_seed"] is None
    assert snapshot["context_symbols"] == ["CCC", "DDD"]


def test_seeded_context_snapshot_keeps_seed_and_resolved_lists(tmp_path):
    service = UniverseService({
        "PRIMARY": ("AAA",),
        "CONTEXT": ("BBB", "CCC", "DDD", "EEE"),
    })
    primary = UniverseSelection(source=SAVED_SOURCE, universe="PRIMARY")
    resolved = service.resolve_experiment(
        primary,
        ("CONTEXT",),
        context_sample_size=2,
        context_selection_method=SEEDED_SAMPLE,
        context_seed=1234,
    )
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.predictor_symbols,
        universe_selection=primary,
        primary_universe_id=resolved.primary_universe_id,
        context_universe_ids=resolved.context_universe_ids,
        context_sample_size=2,
        context_selection_method=SEEDED_SAMPLE,
        context_seed=1234,
        target_symbols=resolved.target_symbols,
        context_symbols=resolved.context_symbols,
        predictor_symbols=resolved.predictor_symbols,
    )

    snapshot = spec.to_dict()
    restored = ExperimentSpec.from_dict(snapshot)

    assert snapshot["context_sample_size"] == 2
    assert snapshot["context_selection_method"] == SEEDED_SAMPLE
    assert snapshot["context_seed"] == 1234
    assert restored.context_symbols == spec.context_symbols
    assert restored.predictor_symbols == spec.predictor_symbols


def test_no_context_keeps_legacy_symbols_behavior_and_context_run_is_frozen(tmp_path):
    service = UniverseService(root=tmp_path)
    primary = service.create("Primary", ("AAA", "BBB"))
    context = service.create(
        "Context", ("BBB", "CCC"), universe_type=CONTEXT_UNIVERSE_TYPE
    )
    selection = UniverseSelection(source=SAVED_SOURCE, universe=primary.universe_id)
    without_context = service.resolve_experiment(selection)
    assert without_context.target_symbols == without_context.predictor_symbols

    resolved = service.resolve_experiment(selection, (context.universe_id,))
    spec = ExperimentSpec(
        job_type=JobType.WALK_FORWARD,
        config=replace(DEFAULT_CONFIG, project_root=tmp_path),
        symbols=resolved.predictor_symbols,
        universe_selection=selection,
        primary_universe_id=resolved.primary_universe_id,
        context_universe_ids=resolved.context_universe_ids,
        target_symbols=resolved.target_symbols,
        context_symbols=resolved.context_symbols,
        predictor_symbols=resolved.predictor_symbols,
    )
    run_id = RunRepository(tmp_path / "runs").create(spec)
    saved = RunRepository(tmp_path / "runs").read_json(run_id, "config.json")
    service.update(context.universe_id, name="Context", symbols=("ZZZ",))
    restored = RunRepository(tmp_path / "runs").load_spec(run_id)

    assert saved["primary_universe_id"] == primary.universe_id
    assert saved["context_universe_ids"] == [context.universe_id]
    assert saved["target_symbols"] == ["AAA", "BBB"]
    assert saved["context_symbols"] == ["CCC"]
    assert saved["predictor_symbols"] == ["AAA", "BBB", "CCC"]
    assert restored.target_symbols == ("AAA", "BBB")
    assert restored.context_symbols == ("CCC",)
    assert restored.predictor_symbols == ("AAA", "BBB", "CCC")
