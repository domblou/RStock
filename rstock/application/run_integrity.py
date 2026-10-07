"""Shared serialization and reference checks for run deletion and publication."""

from __future__ import annotations

import json
import csv
import io
import threading
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator, Mapping

_held = threading.local()


def reference_free_run_document(relative: Path) -> bool:
    """Known numerical/hash-only exports; unknown files are never excluded.

    Producers: threshold_calibration.export_threshold_calibration and
    CheckpointStore.commit_batch. Keep publication and deletion in agreement.
    """
    return (
        relative.parent == Path("results") and relative.name in {
            "threshold_diagnostics.json", "threshold_diagnostics_by_set.json",
        }
        or len(relative.parts) == 5 and relative.parts[:2] == ("checkpoints", "batches")
        and relative.name in {"metadata.json", "complete.json"}
    )


def text_references(name: str, content: str) -> Iterator[tuple[str, str]]:
    if name.endswith(".json"):
        yield from references(json.loads(content))
    elif name.endswith(".csv"):
        for row in csv.DictReader(io.StringIO(content)):
            yield from references(row)


def file_references(path: Path) -> Iterator[tuple[str, str]]:
    """CSV files without reference columns need only a header read."""
    if path.suffix == ".json":
        yield from references(json.loads(path.read_text(encoding="utf-8")))
    elif path.suffix == ".csv":
        with path.open(encoding="utf-8-sig", newline="") as stream:
            reader = csv.DictReader(stream)
            if not any(any(references({field: "candidate"})) for field in (reader.fieldnames or ())):
                return
            for index, row in enumerate(reader, start=2):
                for field, target in references(row):
                    yield f"ligne {index} : {field}", target


def references(value: Any, prefix: str = "") -> Iterator[tuple[str, str]]:
    """Extract persisted run references, including nested derivation records."""
    if isinstance(value, Mapping):
        for key, item in value.items():
            name = str(key)
            field = f"{prefix}.{name}" if prefix else name
            if name == "run_id" or name.endswith(("_run_id", "_run")):
                if isinstance(item, str):
                    yield field, item
            if (name == "run_ids" or name.endswith("_run_ids")) and isinstance(item, (list, tuple)):
                for entry in item:
                    if isinstance(entry, str):
                        yield field, entry
            yield from references(item, field)
    elif isinstance(value, (list, tuple)):
        for index, item in enumerate(value):
            yield from references(item, f"{prefix}[{index}]")


@contextmanager
def graph_lock(root: Path) -> Iterator[None]:
    """Crash-safe, reentrant per-thread mutex; acquired after submission locks."""
    from .runner import _try_submission_mutex

    key = str(root.resolve())
    held = getattr(_held, "roots", set())
    if key in held:
        yield
        return
    deadline = time.monotonic() + 10
    while True:
        with _try_submission_mutex(root / ".run-integrity.guard") as acquired:
            if acquired:
                _held.roots = held | {key}
                try:
                    yield
                finally:
                    _held.roots = held
                return
        if time.monotonic() >= deadline:
            raise TimeoutError("Could not acquire run integrity lock")
        time.sleep(0.02)


def journals(root: Path) -> Iterator[tuple[Path, dict[str, Any]]]:
    directory = root / ".deletions"
    if (directory.is_symlink() or getattr(directory, "is_junction", lambda: False)()
            or directory.resolve().parent != root.resolve()):
        raise ValueError("Invalid deletion journal directory")
    for path in sorted(directory.glob("*/journal.json")):
        if (path.parent.is_symlink() or path.is_symlink()
                or getattr(path.parent, "is_junction", lambda: False)()
                or path.parent.resolve().parent != directory.resolve()):
            raise ValueError("Invalid deletion journal path")
        payload = json.loads(path.read_text(encoding="utf-8"))
        if (not isinstance(payload, dict) or payload.get("schema_version") != 1
                or payload.get("state") not in {"staging", "committed", "complete", "rolled_back"}
                or not isinstance(payload.get("run_ids"), list)
                or not payload["run_ids"]
                or any(not isinstance(item, str) or not item or Path(item).name != item
                       or item in {".", ".."} for item in payload["run_ids"])
                or len(set(payload["run_ids"])) != len(payload["run_ids"])):
            raise ValueError(f"Invalid deletion journal: {path}")
        yield path, payload


def validate_publication(root: Path, payload: Any, run_id: str | None = None) -> None:
    """Prevent stale objects from recreating deleted runs or references."""
    wanted = {target for _, target in references(payload)}
    if run_id:
        wanted.add(run_id)
    for path, journal in journals(root):
        if journal["state"] == "rolled_back":
            continue
        overlap = wanted.intersection(journal["run_ids"])
        if overlap:
            raise ValueError(
                f"Publication refusée : run en suppression ou supprimé {sorted(overlap)} "
                f"(opération {path.parent.name})."
            )
