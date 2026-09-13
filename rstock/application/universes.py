"""Small, deterministic universe resolution service for experiment submission."""

from __future__ import annotations

from dataclasses import dataclass
from random import Random
from typing import Mapping


MANUAL_SOURCE = "manual_list"
SAVED_SOURCE = "saved_universe"
SAMPLE_SOURCE = "universe_sample"
TOP_N = "top_n"
SEEDED_SAMPLE = "seeded_sample"


# This is deliberately a local demonstration registry: no provider, ranking or
# mutable market-data dependency is involved. New static universes can be added
# here (or injected into UniverseService) without changing the UI or workflows.
DEFAULT_UNIVERSES: dict[str, tuple[str, ...]] = {
    "US_STOCKS_DEMO": (
        "AAPL", "MSFT", "AMZN", "AMD", "GOOGL", "META", "NVDA", "TSLA",
        "JPM", "JNJ", "KO", "MCD", "PEP", "V", "WMT",
    ),
}


@dataclass(frozen=True, slots=True)
class UniverseSelection:
    """Serializable inputs used to resolve an experiment universe before submit."""

    source: str = MANUAL_SOURCE
    universe: str | None = None
    sample_size: int | None = None
    selection_method: str | None = None
    seed: int | None = None

    def as_dict(self) -> dict[str, object]:
        return {
            "source": self.source,
            "universe": self.universe,
            "sample_size": self.sample_size,
            "selection_method": self.selection_method,
            "seed": self.seed,
        }

    @classmethod
    def from_dict(cls, values: Mapping[str, object] | None) -> "UniverseSelection":
        if values is None:
            return cls()
        return cls(
            source=str(values.get("source", MANUAL_SOURCE)),
            universe=(None if values.get("universe") is None else str(values["universe"])),
            sample_size=(None if values.get("sample_size") is None else int(values["sample_size"])),
            selection_method=(
                None if values.get("selection_method") is None
                else str(values["selection_method"])
            ),
            seed=None if values.get("seed") is None else int(values["seed"]),
        )


@dataclass(frozen=True, slots=True)
class ResolvedUniverse:
    selection: UniverseSelection
    symbols: tuple[str, ...]


class UniverseService:
    """Resolve static universes without exposing universe logic to ML workflows."""

    def __init__(self, universes: Mapping[str, tuple[str, ...]] | None = None) -> None:
        supplied = DEFAULT_UNIVERSES if universes is None else universes
        self._universes = {
            str(name): self._normalise_symbols(symbols)
            for name, symbols in supplied.items()
        }

    @staticmethod
    def _normalise_symbols(symbols: object) -> tuple[str, ...]:
        result = tuple(str(symbol).strip().upper() for symbol in symbols if str(symbol).strip())
        if len(set(result)) != len(result):
            raise ValueError("Universe symbols must be unique")
        return result

    def universe_names(self) -> tuple[str, ...]:
        return tuple(sorted(self._universes))

    def universe_symbols(self, name: str) -> tuple[str, ...]:
        try:
            return self._universes[name]
        except KeyError as error:
            raise ValueError(f"Unknown saved universe: {name}") from error

    def resolve(
        self,
        selection: UniverseSelection,
        *,
        manual_symbols: tuple[str, ...] | list[str] = (),
    ) -> ResolvedUniverse:
        """Resolve once, preserving declared source order for deterministic Top N."""

        manual = self._normalise_symbols(manual_symbols)
        if selection.source == MANUAL_SOURCE:
            return ResolvedUniverse(selection, manual)
        if selection.source not in {SAVED_SOURCE, SAMPLE_SOURCE}:
            raise ValueError(f"Unknown universe source: {selection.source}")
        if not selection.universe:
            raise ValueError("A saved universe must be selected")
        source = self.universe_symbols(selection.universe)
        if selection.source == SAVED_SOURCE:
            return ResolvedUniverse(selection, source)
        if selection.sample_size is None or selection.sample_size < 1:
            raise ValueError("sample_size must be positive")
        if selection.sample_size > len(source):
            raise ValueError(
                f"Requested sample size {selection.sample_size} exceeds universe size {len(source)}"
            )
        if selection.selection_method == TOP_N:
            return ResolvedUniverse(selection, source[: selection.sample_size])
        if selection.selection_method == SEEDED_SAMPLE:
            if selection.seed is None:
                raise ValueError("A seed is required for a reproducible sample")
            # sample() preserves its generated order and is stable for a fixed
            # source tuple, size and explicit seed.
            return ResolvedUniverse(
                selection,
                tuple(Random(selection.seed).sample(source, selection.sample_size)),
            )
        raise ValueError("A supported sample selection_method is required")
