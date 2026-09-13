"""Presentation model for the conditional universe-selection controls."""

from __future__ import annotations

from dataclasses import dataclass

from .universes import (
    MANUAL_SOURCE,
    SAMPLE_SOURCE,
    SAVED_SOURCE,
    SEEDED_SAMPLE,
    UniverseSelection,
    UniverseService,
)


@dataclass(frozen=True, slots=True)
class UniverseUiPreview:
    selection: UniverseSelection
    resolved_symbols: tuple[str, ...]
    visible_controls: frozenset[str]


def universe_display_name(name: str, service: UniverseService) -> str:
    """Return a human-readable saved-universe label while retaining its ID."""

    if name == "US_STOCKS_DEMO":
        return f"Test \u2014 {len(service.universe_symbols(name))} titres"
    return f"{name} \u2014 {len(service.universe_symbols(name))} titres"


def universe_ui_preview(
    selection: UniverseSelection,
    service: UniverseService,
    *,
    manual_symbols: tuple[str, ...] | list[str] = (),
) -> UniverseUiPreview:
    """Return the one visible source and its exact pre-submit symbol list."""

    visible = {"mode"}
    if selection.source == MANUAL_SOURCE:
        visible.add("manual_symbols")
    elif selection.source == SAVED_SOURCE:
        visible.update({"universe", "preview"})
    elif selection.source == SAMPLE_SOURCE:
        visible.update({"universe", "sample_size", "selection_method", "preview"})
        if selection.selection_method == SEEDED_SAMPLE:
            visible.add("seed")
    else:
        raise ValueError(f"Unknown universe source: {selection.source}")
    resolved = service.resolve(selection, manual_symbols=manual_symbols)
    return UniverseUiPreview(selection, resolved.symbols, frozenset(visible))
