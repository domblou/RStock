"""Small, deterministic universe resolution service for experiment submission."""

from __future__ import annotations

import csv
import io
import json
import os
import re
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from random import Random
from typing import Iterable, Mapping


MANUAL_SOURCE = "manual_list"
SAVED_SOURCE = "saved_universe"
SAMPLE_SOURCE = "universe_sample"
TOP_N = "top_n"
SEEDED_SAMPLE = "seeded_sample"
STANDARD_UNIVERSE_TYPE = "standard"
CONTEXT_UNIVERSE_TYPE = "context"
UNIVERSE_TYPES = {STANDARD_UNIVERSE_TYPE, CONTEXT_UNIVERSE_TYPE}


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
class UniverseRecord:
    universe_id: str
    name: str
    symbols: tuple[str, ...]
    source: str
    updated_at: str | None
    system: bool = False
    type: str = STANDARD_UNIVERSE_TYPE


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


@dataclass(frozen=True, slots=True)
class ResolvedExperimentUniverse:
    primary_selection: UniverseSelection
    primary_universe_id: str | None
    context_universe_ids: tuple[str, ...]
    target_symbols: tuple[str, ...]
    context_symbols: tuple[str, ...]
    predictor_symbols: tuple[str, ...]


class UniverseService:
    """Persist and resolve universes without exposing their origin to workflows."""

    def __init__(
        self,
        universes: Mapping[str, tuple[str, ...]] | None = None,
        *,
        root: Path | None = None,
    ) -> None:
        self._root = None if root is None else Path(root) / "data" / "universes"
        supplied = DEFAULT_UNIVERSES if universes is None else universes
        self._system = {
            str(identifier): UniverseRecord(
                universe_id=str(identifier),
                name=("Test — 15 titres" if identifier == "US_STOCKS_DEMO" else str(identifier)),
                symbols=self._normalise_symbols(symbols),
                source="Système",
                updated_at=None,
                system=True,
                type=STANDARD_UNIVERSE_TYPE,
            )
            for identifier, symbols in supplied.items()
        }
        self._memory_records: dict[str, UniverseRecord] = {}

    @staticmethod
    def _normalise_symbols(symbols: Iterable[object]) -> tuple[str, ...]:
        result: list[str] = []
        seen: set[str] = set()
        for symbol in symbols:
            normalized = str(symbol).strip().upper()
            if normalized and normalized not in seen:
                result.append(normalized)
                seen.add(normalized)
        return tuple(result)

    @staticmethod
    def parse_symbols(value: str | Iterable[object]) -> tuple[str, ...]:
        """Normalize comma/newline text or an iterable and remove duplicates."""

        if isinstance(value, str):
            values = re.split(r"[,;\r\n]+", value)
        else:
            values = value
        symbols = UniverseService._normalise_symbols(values)
        if not symbols:
            raise ValueError("Un univers doit contenir au moins un symbole")
        return symbols

    @staticmethod
    def _normalise_type(value: object) -> str:
        universe_type = str(value or STANDARD_UNIVERSE_TYPE).strip().lower()
        if universe_type not in UNIVERSE_TYPES:
            raise ValueError(f"Unknown universe type: {universe_type}")
        return universe_type

    @property
    def directory(self) -> Path | None:
        return self._root

    def _metadata_path(self) -> Path:
        if self._root is None:
            raise RuntimeError("Universe persistence is not configured")
        return self._root / "universes.json"

    @staticmethod
    def _atomic_write(path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
        )
        temporary = Path(temporary_name)
        try:
            with os.fdopen(descriptor, "w", encoding="utf-8", newline="") as stream:
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(temporary, path)
        finally:
            temporary.unlink(missing_ok=True)

    def _load_user_records(self) -> dict[str, UniverseRecord]:
        if self._root is None:
            return dict(self._memory_records)
        path = self._metadata_path()
        if not path.exists():
            return {}
        payload = json.loads(path.read_text(encoding="utf-8"))
        records: dict[str, UniverseRecord] = {}
        for item in payload.get("universes", []):
            identifier = str(item["universe_id"])
            csv_path = self._root / f"{identifier}.csv"
            if not csv_path.exists():
                continue
            with csv_path.open("r", encoding="utf-8-sig", newline="") as stream:
                symbols = self.parse_symbols(row["symbol"] for row in csv.DictReader(stream))
            records[identifier] = UniverseRecord(
                universe_id=identifier,
                name=str(item["name"]),
                symbols=symbols,
                source=str(item.get("source", "Manuel")),
                updated_at=None if item.get("updated_at") is None else str(item["updated_at"]),
                type=self._normalise_type(item.get("type", STANDARD_UNIVERSE_TYPE)),
            )
        return records

    def records(self) -> tuple[UniverseRecord, ...]:
        combined = {**self._system, **self._load_user_records()}
        return tuple(sorted(combined.values(), key=lambda item: item.name.casefold()))

    def record(self, universe_id: str) -> UniverseRecord:
        records = {item.universe_id: item for item in self.records()}
        try:
            return records[universe_id]
        except KeyError as error:
            raise ValueError(f"Unknown saved universe: {universe_id}") from error

    def _save_user_records(self, records: Mapping[str, UniverseRecord]) -> None:
        if self._root is None:
            self._memory_records = dict(records)
            return
        payload = {
            "universes": [
                {
                    "universe_id": record.universe_id,
                    "name": record.name,
                    "source": record.source,
                    "updated_at": record.updated_at,
                    "type": record.type,
                }
                for record in sorted(records.values(), key=lambda item: item.universe_id)
            ]
        }
        self._atomic_write(
            self._metadata_path(), json.dumps(payload, ensure_ascii=False, indent=2) + "\n"
        )

    def _write_symbols(self, record: UniverseRecord) -> None:
        if self._root is None:
            return
        rows = "symbol\n" + "".join(f"{symbol}\n" for symbol in record.symbols)
        self._atomic_write(self._root / f"{record.universe_id}.csv", rows)

    @staticmethod
    def _identifier(name: str, existing: set[str]) -> str:
        base = re.sub(r"[^A-Z0-9]+", "_", name.upper()).strip("_") or "UNIVERSE"
        candidate = base
        suffix = 2
        while candidate in existing:
            candidate = f"{base}_{suffix}"
            suffix += 1
        return candidate

    def create(
        self,
        name: str,
        symbols: str | Iterable[object],
        *,
        source: str = "Manuel",
        universe_type: str = STANDARD_UNIVERSE_TYPE,
    ) -> UniverseRecord:
        clean_name = name.strip()
        if not clean_name:
            raise ValueError("Le nom de l’univers est obligatoire")
        normalized = self.parse_symbols(symbols)
        users = self._load_user_records()
        identifier = self._identifier(clean_name, set(users) | set(self._system))
        record = UniverseRecord(
            identifier,
            clean_name,
            normalized,
            source,
            datetime.now(timezone.utc).isoformat(),
            type=self._normalise_type(universe_type),
        )
        self._write_symbols(record)
        users[identifier] = record
        self._save_user_records(users)
        return record

    def create_from_csv(
        self,
        name: str,
        content: bytes | str,
        *,
        column: str = "symbol",
        universe_type: str = STANDARD_UNIVERSE_TYPE,
    ) -> UniverseRecord:
        text = content.decode("utf-8-sig") if isinstance(content, bytes) else content
        reader = csv.DictReader(io.StringIO(text))
        if not reader.fieldnames:
            raise ValueError("Le CSV ne contient aucun en-tête")
        matching = next(
            (field for field in reader.fieldnames if field.casefold() == column.casefold()), None
        )
        if matching is None:
            raise ValueError(f"Colonne CSV introuvable : {column}")
        return self.create(
            name,
            (row.get(matching, "") for row in reader),
            source="Import CSV",
            universe_type=universe_type,
        )

    def update(
        self,
        universe_id: str,
        *,
        name: str,
        symbols: str | Iterable[object],
        universe_type: str | None = None,
    ) -> UniverseRecord:
        if universe_id in self._system:
            raise ValueError("Les univers système sont protégés")
        users = self._load_user_records()
        if universe_id not in users:
            raise ValueError(f"Unknown saved universe: {universe_id}")
        clean_name = name.strip()
        if not clean_name:
            raise ValueError("Le nom de l’univers est obligatoire")
        record = UniverseRecord(
            universe_id,
            clean_name,
            self.parse_symbols(symbols),
            users[universe_id].source,
            datetime.now(timezone.utc).isoformat(),
            type=self._normalise_type(universe_type or users[universe_id].type),
        )
        self._write_symbols(record)
        users[universe_id] = record
        self._save_user_records(users)
        return record

    def duplicate(self, universe_id: str) -> UniverseRecord:
        source = self.record(universe_id)
        return self.create(
            f"{source.name} (copie)",
            source.symbols,
            source="Copie",
            universe_type=source.type,
        )

    def delete(self, universe_id: str) -> None:
        if universe_id in self._system:
            raise ValueError("Les univers système sont protégés")
        users = self._load_user_records()
        if universe_id not in users:
            raise ValueError(f"Unknown saved universe: {universe_id}")
        users.pop(universe_id)
        self._save_user_records(users)
        if self._root is not None:
            (self._root / f"{universe_id}.csv").unlink(missing_ok=True)

    def universe_names(self) -> tuple[str, ...]:
        return tuple(record.universe_id for record in self.records())

    def standard_universe_names(self) -> tuple[str, ...]:
        return tuple(
            record.universe_id
            for record in self.records()
            if record.type == STANDARD_UNIVERSE_TYPE
        )

    def universe_symbols(self, name: str) -> tuple[str, ...]:
        return self.record(name).symbols

    def resolve(
        self,
        selection: UniverseSelection,
        *,
        manual_symbols: tuple[str, ...] | list[str] = (),
    ) -> ResolvedUniverse:
        """Resolve once, preserving declared source order for deterministic Top N."""

        manual = self._normalise_symbols(manual_symbols)
        if selection.source == MANUAL_SOURCE:
            if not manual:
                raise ValueError("Un univers doit contenir au moins un symbole")
            return ResolvedUniverse(selection, manual)
        if selection.source not in {SAVED_SOURCE, SAMPLE_SOURCE}:
            raise ValueError(f"Unknown universe source: {selection.source}")
        if not selection.universe:
            raise ValueError("A saved universe must be selected")
        record = self.record(selection.universe)
        if record.type != STANDARD_UNIVERSE_TYPE:
            raise ValueError("A context universe cannot be selected as the primary universe")
        source = record.symbols
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

    def resolve_experiment(
        self,
        primary: UniverseSelection,
        context_universe_ids: Iterable[str] = (),
        *,
        manual_symbols: tuple[str, ...] | list[str] = (),
    ) -> ResolvedExperimentUniverse:
        """Resolve and freeze distinct target and predictor roles for one run."""

        resolved_primary = self.resolve(primary, manual_symbols=manual_symbols)
        context_ids = tuple(dict.fromkeys(str(item) for item in context_universe_ids))
        target_set = set(resolved_primary.symbols)
        context_symbols = tuple(dict.fromkeys(
            symbol
            for context_id in context_ids
            for symbol in self.record(context_id).symbols
            if symbol not in target_set
        ))
        return ResolvedExperimentUniverse(
            primary_selection=resolved_primary.selection,
            primary_universe_id=primary.universe,
            context_universe_ids=context_ids,
            target_symbols=resolved_primary.symbols,
            context_symbols=context_symbols,
            predictor_symbols=tuple((*resolved_primary.symbols, *context_symbols)),
        )
