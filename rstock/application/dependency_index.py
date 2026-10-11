"""Reconstructible file-backed dependency index, independent of History.

Each operation discovers sources again, including unknown/external additions.
Only complete analyses of unchanged files are reused. Publication is atomic and
serialized by the existing graph lock; no scientific artifact is rewritten.
"""
from __future__ import annotations

from collections import OrderedDict
import hashlib
import json
import logging
import os
import stat
from pathlib import Path
import threading
import time
from typing import Callable, Iterable, Iterator

from .run_integrity import graph_lock

LOGGER = logging.getLogger(__name__)
_VERSION = 2  # Bump when reference extraction/exclusion rules change.
_MEMORY = OrderedDict()
_MEMORY_LOCK = threading.RLock()


def identity(path: Path) -> tuple[int, ...]:
    stat = path.stat(follow_symlinks=False)
    return (stat.st_mtime_ns, stat.st_ctime_ns, stat.st_ino, stat.st_dev, stat.st_mode, stat.st_size)


def _canonical(value) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


class DependencyIndex:
    def __init__(self, repository, *, force: bool = False, persist: bool = True):
        self.repository = repository
        self.root = repository.root.resolve()
        name = hashlib.sha256(str(self.root).encode()).hexdigest()[:24]
        self.directory = self.root.parent / ".rstock-dependency-index" / name
        self.path = self.directory / "index.json"
        for directory in (self.directory.parent, self.directory):
            if directory.is_symlink() or getattr(directory, "is_junction", lambda: False)():
                raise ValueError("Dependency index directory must be local and cannot be a link")
        if self.path.is_symlink() or getattr(self.path, "is_junction", lambda: False)():
            raise ValueError("Dependency index file cannot be a link")
        self.force = force
        self.persist = persist
        self.entries = {} if force else self._load()
        self.pending = {}
        self.observed = {}
        self.directory_entries = {}
        self.invalidated = set()
        self.seen = set()
        self.stats = {"hits": 0, "parsed": 0}

    @staticmethod
    def _key(path):
        return os.path.normcase(os.path.abspath(path))

    def _load(self, *, use_memory: bool = True):
        try:
            stamp = identity(self.path)
            with _MEMORY_LOCK:
                cached = _MEMORY.get(str(self.path))
                if use_memory and cached is not None and cached[0] == stamp:
                    return cached[1]
            payload = json.loads(self.path.read_text(encoding="utf-8"))
            entries = payload["files"]
            if (payload["schema_version"] != _VERSION or payload["root"] != str(self.root)
                    or not isinstance(entries, dict)
                    or payload["sha256"] != hashlib.sha256(_canonical(entries).encode()).hexdigest()):
                return {}
            for key, record in entries.items():
                if (not isinstance(key, str) or not isinstance(record, dict)
                        or not isinstance(record.get("identity"), list)
                        or len(record["identity"]) != 6
                        or not all(isinstance(value, int) for value in record["identity"])
                        or not isinstance(record.get("edges"), list)
                        or not all(isinstance(edge, list) and len(edge) == 2
                                   and all(isinstance(part, str) for part in edge)
                                   for edge in record["edges"])):
                    return {}
            if stamp != identity(self.path):
                return {}
            with _MEMORY_LOCK:
                _MEMORY[str(self.path)] = (stamp, entries)
                while len(_MEMORY) > 4:
                    _MEMORY.popitem(last=False)
            return entries
        except (OSError, ValueError, KeyError, TypeError):
            return {}

    def observe(self, path: Path):
        stamp = identity(path)
        if stat.S_ISLNK(stamp[4]):
            raise ValueError(f"Lien interdit dans les sources de dépendances : {path}")
        if stat.S_ISDIR(stamp[4]):
            names = tuple(sorted(os.listdir(path)))
            previous_names = self.directory_entries.setdefault(path, names)
            if previous_names != names:
                raise RuntimeError(f"Source de dépendances modifiée pendant la vérification : {path}")
        previous = self.observed.setdefault(path, stamp)
        if previous != stamp:
            raise RuntimeError(f"Source de dépendances modifiée pendant la vérification : {path}")
        return stamp

    def iter_edges(self, path: Path, loader: Callable[[], Iterable[tuple[str, str]]],
                   *, targets: set[str] | None = None) -> Iterator[tuple[str, str]]:
        stamp = self.observe(path)
        key = self._key(path)
        self.seen.add(key)
        record = self.pending.get(key) or self.entries.get(key)
        # Windows can coalesce last-write timestamps for rapid in-place edits.
        # Do not reuse analyses of recently modified files, even if metadata
        # appears identical. Old/backdated external imports can be force rebuilt.
        recent = time.time_ns() - stamp[0] < 2_000_000_000
        if not recent and not self.force and record is not None and tuple(record["identity"]) == stamp:
            self.stats["hits"] += 1
            for field, target in record["edges"]:
                if targets is None or target in targets:
                    yield field, target
            return
        if record is not None:
            self.invalidated.add(key)
        self.stats["parsed"] += 1
        unique = {}
        # A caller can stop at its first blocker. In that case this generator
        # never reaches publication: incomplete CSV analyses aren't cached.
        for field, target in loader():
            unique.setdefault((field.rsplit(" : ", 1)[-1] if path.suffix == ".csv" else field, target),
                              (field, target))
            if targets is None or target in targets:
                yield field, target
        if stamp != identity(path):
            raise RuntimeError(f"Source de dépendances modifiée pendant la lecture : {path}")
        # Never persist a recent timestamp: otherwise a same-tick edit could
        # outlive the conservative window and revive an obsolete analysis.
        if not recent:
            self.pending[key] = {"identity": list(stamp), "edges": list(unique.values())}

    def validate(self):
        for path, names in self.directory_entries.items():
            if tuple(sorted(os.listdir(path))) != names:
                raise RuntimeError(f"Source de dépendances modifiée pendant la vérification : {path}")
        for path, stamp in self.observed.items():
            if identity(path) != stamp:
                raise RuntimeError(f"Source de dépendances modifiée pendant la vérification : {path}")

    def publish(self, *, complete: bool):
        if not self.persist:
            self.validate()
            return
        with graph_lock(self.root):
            self.validate()
            # Reload immediately before merging: another component can have
            # published since this object was constructed. Never overwrite it
            # using only the possibly stale in-memory snapshot.
            current = self._load(use_memory=False)
            merged = {} if self.force and complete else dict(current)
            if complete:
                merged = {key: record for key, record in merged.items() if key in self.seen}
            for key in self.invalidated:
                merged.pop(key, None)
            merged.update(self.pending)
            if merged == current:
                return
            payload = {"schema_version": _VERSION, "root": str(self.root), "files": merged,
                       "sha256": hashlib.sha256(_canonical(merged).encode()).hexdigest()}
            try:
                # The directory contains only derived data, outside runs and
                # production so it cannot become a scientific dependency.
                self.directory.mkdir(parents=True, exist_ok=True)
                self.repository._atomic_json_write(self.path, _canonical(payload))
            except OSError as error:
                LOGGER.warning("Dependency index publication failed; sources remain authoritative: %s", error)


def clear_dependency_memory():
    with _MEMORY_LOCK:
        _MEMORY.clear()
