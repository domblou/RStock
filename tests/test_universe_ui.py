from rstock.application.universe_ui import universe_display_name, universe_ui_preview
from rstock.application.universes import (
    MANUAL_SOURCE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    TOP_N,
    UniverseSelection,
    UniverseService,
)


def _service() -> UniverseService:
    return UniverseService({"TEST": ("AAA", "BBB", "CCC", "DDD", "EEE")})


def test_manual_mode_shows_only_the_custom_symbol_source():
    preview = universe_ui_preview(
        UniverseSelection(source=MANUAL_SOURCE), _service(), manual_symbols=("AAA", "BBB")
    )
    assert preview.visible_controls == {"mode", "manual_symbols"}
    assert preview.resolved_symbols == ("AAA", "BBB")


def test_complete_universe_hides_manual_and_sample_controls():
    preview = universe_ui_preview(
        UniverseSelection(source=SAVED_SOURCE, universe="TEST"), _service()
    )
    assert preview.visible_controls == {"mode", "universe", "preview"}
    assert preview.resolved_symbols == ("AAA", "BBB", "CCC", "DDD", "EEE")


def test_sample_mode_shows_size_and_method_but_seed_only_when_needed():
    top_n = universe_ui_preview(
        UniverseSelection(
            source=SAMPLE_SOURCE, universe="TEST", sample_size=2, selection_method=TOP_N
        ),
        _service(),
    )
    seeded = universe_ui_preview(
        UniverseSelection(
            source=SAMPLE_SOURCE, universe="TEST", sample_size=2,
            selection_method=SEEDED_SAMPLE, seed=1234,
        ),
        _service(),
    )
    assert {"universe", "sample_size", "selection_method"} <= top_n.visible_controls
    assert "seed" not in top_n.visible_controls
    assert "seed" in seeded.visible_controls


def test_preview_symbols_are_the_exact_symbols_to_submit_after_mode_change():
    service = _service()
    manual = universe_ui_preview(
        UniverseSelection(source=MANUAL_SOURCE), service, manual_symbols=("AAA", "EEE")
    )
    sampled = universe_ui_preview(
        UniverseSelection(
            source=SAMPLE_SOURCE, universe="TEST", sample_size=3,
            selection_method=SEEDED_SAMPLE, seed=99,
        ),
        service,
    )
    assert manual.resolved_symbols == ("AAA", "EEE")
    assert sampled.resolved_symbols == service.resolve(sampled.selection).symbols
    assert sampled.resolved_symbols != manual.resolved_symbols


def test_demo_universe_uses_a_human_readable_label():
    service = UniverseService()
    assert universe_display_name("US_STOCKS_DEMO", service) == "Test — 15 titres"
