"""Atomic, partitioned persistence for Production quality observations."""

from __future__ import annotations

import hashlib
import json
import os
import tempfile
import time
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterator
from urllib.parse import quote

import pandas as pd

from .production_quality import (
    OBSERVATION_KEY,
    empty_quality_observations,
    normalize_quality_observations,
    validate_quality_observations,
)


QUALITY_STORAGE_SCHEMA_VERSION = 1
QUALITY_METRICS_VERSION = 1


@dataclass(frozen=True, slots=True)
class ObservationUpsertResult:
    model_id: str
    added: int
    updated: int
    unchanged: int
    total: int

    @property
    def changed(self) -> bool:
        return bool(self.added or self.updated)


class ProductionQualityRepository:
    def __init__(self, project_root: Path) -> None:
        self.root = Path(project_root) / "production" / "quality"
        self.observations_root = self.root / "observations"
        self.snapshots_root = self.root / "snapshots"
        self.series_root = self.root / "series"
        self.baselines_root = self.root / "baselines"
        self.lineage_root = self.root / "lineage"
        self.generations_root = self.root / "generations"
        self.current_generation_path = self.root / "current_generation.json"
        self.manifest_path = self.root / "manifest.json"

    @staticmethod
    def _model_token(model_id: str) -> str:
        if not str(model_id):
            raise ValueError("model_id is required")
        return quote(str(model_id), safe="")

    def observation_path(self, model_id: str) -> Path:
        return (
            self.observations_root
            / f"model_id={self._model_token(model_id)}"
            / "observations.parquet"
        )

    def snapshot_path(self) -> Path:
        return self._derived_root() / "snapshots" / "models.parquet"

    def model_snapshot_path(self, model_id: str) -> Path:
        return self._derived_root() / "snapshots" / f"{self._model_token(model_id)}.json"

    def series_path(self, model_id: str) -> Path:
        return self._derived_root() / "series" / f"{self._model_token(model_id)}.parquet"

    def _derived_root(self) -> Path:
        if not self.current_generation_path.exists():
            return self.root
        payload = self._load_json(self.current_generation_path)
        name = str(payload.get("generation") or "")
        candidate = self.generations_root / name
        if not name or not candidate.is_dir():
            raise ValueError("Invalid current production quality generation")
        return candidate

    def generation_path(self, name: str) -> Path:
        if not name or "/" in name or "\\" in name or name in {".", ".."}:
            raise ValueError("Invalid quality generation name")
        return self.generations_root / name

    def publish_generation(self, name: str) -> None:
        path = self.generation_path(name)
        if not path.is_dir():
            raise ValueError("Cannot publish a missing quality generation")
        with self.transaction():
            self._atomic_text(
                self.current_generation_path,
                json.dumps({"generation": name, "published_at": self._now()}, indent=2) + "\n",
            )

    def baseline_path(self, model_id: str) -> Path:
        return self.baselines_root / f"{self._model_token(model_id)}.json"

    def lineage_path(self, model_id: str) -> Path:
        return self.lineage_root / f"{self._model_token(model_id)}.json"

    def baseline_exists(self, model_id: str) -> bool:
        return self.baseline_path(model_id).exists()

    def lineage_exists(self, model_id: str) -> bool:
        return self.lineage_path(model_id).exists()

    @staticmethod
    def _load_json(path: Path) -> dict[str, Any]:
        return json.loads(path.read_text(encoding="utf-8"))

    def load_baseline(self, model_id: str) -> dict[str, Any] | None:
        path = self.baseline_path(model_id)
        return None if not path.exists() else self._load_json(path)

    def load_lineage(self, model_id: str) -> dict[str, Any] | None:
        path = self.lineage_path(model_id)
        return None if not path.exists() else self._load_json(path)

    def _write_quality_json(
        self, path: Path, payload: dict[str, Any], *, overwrite: bool
    ) -> bool:
        content = json.dumps(payload, indent=2, ensure_ascii=False, sort_keys=True, default=str) + "\n"
        with self.transaction():
            if path.exists() and not overwrite:
                return False
            self._atomic_text(path, content)
            return True

    def write_baseline(
        self, model_id: str, payload: dict[str, Any], *, overwrite: bool = False
    ) -> bool:
        if str(payload.get("model_id")) != str(model_id):
            raise ValueError("Baseline model_id does not match its path")
        return self._write_quality_json(
            self.baseline_path(model_id), payload, overwrite=overwrite
        )

    def write_lineage(
        self, model_id: str, payload: dict[str, Any], *, overwrite: bool = False
    ) -> bool:
        if str(payload.get("model_id")) != str(model_id):
            raise ValueError("Lineage model_id does not match its path")
        return self._write_quality_json(
            self.lineage_path(model_id), payload, overwrite=overwrite
        )

    @contextmanager
    def transaction(self) -> Iterator[None]:
        self.root.mkdir(parents=True, exist_ok=True)
        lock = self.root / ".write.lock"
        deadline = time.monotonic() + 5.0
        while True:
            try:
                lock.mkdir()
                break
            except FileExistsError:
                if time.monotonic() >= deadline:
                    raise TimeoutError("Could not acquire production quality lock")
                time.sleep(0.02)
        try:
            yield
        finally:
            lock.rmdir()

    @staticmethod
    def _now() -> str:
        return datetime.now(timezone.utc).isoformat()

    def _default_manifest(self) -> dict[str, Any]:
        return {
            "schema_version": QUALITY_STORAGE_SCHEMA_VERSION,
            "quality_metrics_version": QUALITY_METRICS_VERSION,
            "health_policy_version": None,
            "generated_at": self._now(),
            "source_generation": 0,
            "dirty_model_ids": [],
            "last_completed_model_id": None,
            "snapshot_generation": 0,
            "status": "ready",
            "observation_generations": {},
        }

    def load_manifest(self) -> dict[str, Any]:
        if not self.manifest_path.exists():
            return self._default_manifest()
        values = json.loads(self.manifest_path.read_text(encoding="utf-8"))
        if values.get("schema_version") != QUALITY_STORAGE_SCHEMA_VERSION:
            raise ValueError("Unsupported production quality manifest schema")
        manifest = self._default_manifest()
        manifest.update(values)
        manifest["dirty_model_ids"] = sorted(
            {str(item) for item in manifest.get("dirty_model_ids", [])}
        )
        manifest["observation_generations"] = {
            str(key): str(value)
            for key, value in manifest.get("observation_generations", {}).items()
        }
        return manifest

    @staticmethod
    def _atomic_text(path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
        )
        temporary = Path(temporary_name)
        try:
            with os.fdopen(descriptor, "w", encoding="utf-8", newline="\n") as stream:
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(temporary, path)
        finally:
            temporary.unlink(missing_ok=True)

    @staticmethod
    def _atomic_parquet(path: Path, frame: pd.DataFrame) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
        )
        os.close(descriptor)
        temporary = Path(temporary_name)
        try:
            frame.to_parquet(temporary, engine="pyarrow", index=False)
            # Windows rejects fsync on a read-only descriptor.  Reopen the
            # fully closed PyArrow output read/write before atomic replace.
            with temporary.open("r+b") as stream:
                os.fsync(stream.fileno())
            os.replace(temporary, path)
        finally:
            temporary.unlink(missing_ok=True)

    def _write_manifest(self, manifest: dict[str, Any]) -> None:
        manifest = dict(manifest)
        manifest["generated_at"] = self._now()
        self._atomic_text(
            self.manifest_path,
            json.dumps(manifest, indent=2, ensure_ascii=False, sort_keys=True) + "\n",
        )

    @staticmethod
    def _digest(path: Path) -> str:
        digest = hashlib.sha256()
        with path.open("rb") as stream:
            for block in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(block)
        return digest.hexdigest()

    def load_observations(self, model_id: str) -> pd.DataFrame:
        path = self.observation_path(model_id)
        if not path.exists():
            return empty_quality_observations()
        return normalize_quality_observations(
            pd.read_parquet(path, engine="pyarrow")
        )

    def load_model_series(self, model_id: str) -> pd.DataFrame:
        path = self.series_path(model_id)
        return pd.DataFrame() if not path.exists() else pd.read_parquet(path, engine="pyarrow")

    def write_model_series(self, model_id: str, series: pd.DataFrame) -> None:
        with self.transaction():
            self._atomic_parquet(self.series_path(model_id), series)

    def load_model_snapshot(self, model_id: str) -> dict[str, Any] | None:
        path = self.model_snapshot_path(model_id)
        return None if not path.exists() else self._load_json(path)

    def write_model_snapshot(self, model_id: str, snapshot: dict[str, Any]) -> None:
        if str(snapshot.get("model_id")) != str(model_id):
            raise ValueError("Snapshot model_id does not match its path")
        with self.transaction():
            self._atomic_text(
                self.model_snapshot_path(model_id),
                json.dumps(snapshot, indent=2, ensure_ascii=False, sort_keys=True, default=str) + "\n",
            )

    def load_master_snapshot(self) -> pd.DataFrame:
        path = self.snapshot_path()
        return pd.DataFrame() if not path.exists() else pd.read_parquet(path, engine="pyarrow")

    def upsert_master_snapshot(self, row: dict[str, Any]) -> pd.DataFrame:
        model_id = str(row.get("model_id") or "")
        if not model_id:
            raise ValueError("Master snapshot row requires model_id")
        with self.transaction():
            current = self.load_master_snapshot()
            if not current.empty and "model_id" in current:
                current = current[current["model_id"].astype(str) != model_id]
            result = pd.concat([current, pd.DataFrame([row])], ignore_index=True)
            result = result.sort_values("model_id", kind="stable").reset_index(drop=True)
            self._atomic_parquet(self.snapshot_path(), result)
            return result

    @staticmethod
    def _comparable(frame: pd.DataFrame) -> pd.DataFrame:
        values = frame.drop(columns=["quality_ingested_at"], errors="ignore").copy()
        for name in values:
            if pd.api.types.is_datetime64_any_dtype(values[name]):
                values[name] = values[name].astype("string")
        # Nullable numeric extension arrays reject a string fill value.  Convert
        # every column to the comparison representation first, then encode nulls.
        return values.astype("string").fillna("<NA>")

    def reconcile_manifest(self) -> dict[str, Any]:
        """Re-read disk and mark partitions changed outside the manifest dirty."""

        with self.transaction():
            return self._reconcile_manifest_locked()

    def _reconcile_manifest_locked(self) -> dict[str, Any]:
        manifest = self.load_manifest()
        persisted = dict(manifest.get("observation_generations", {}))
        actual: dict[str, str] = {}
        if self.observations_root.exists():
            for path in self.observations_root.glob("model_id=*/observations.parquet"):
                frame = pd.read_parquet(path, columns=["model_id"], engine="pyarrow")
                if frame.empty or frame["model_id"].nunique(dropna=False) != 1:
                    raise ValueError(f"Invalid observation partition: {path}")
                model_id = str(frame.iloc[0]["model_id"])
                actual[model_id] = self._digest(path)
        changed = {
            model_id
            for model_id, digest in actual.items()
            if persisted.get(model_id) != digest
        } | (set(persisted) - set(actual))
        dirty = set(manifest.get("dirty_model_ids", [])) | changed
        if changed or actual != persisted:
            manifest["dirty_model_ids"] = sorted(dirty)
            manifest["observation_generations"] = actual
            manifest["source_generation"] = int(manifest.get("source_generation", 0)) + 1
            manifest["status"] = "dirty" if dirty else "ready"
            self._write_manifest(manifest)
        elif not self.manifest_path.exists():
            self._write_manifest(manifest)
        return manifest

    def _reconcile_model_locked(self, model_id: str) -> dict[str, Any]:
        """Repair one partition without scanning every other production model."""

        manifest = self.load_manifest()
        persisted = dict(manifest.get("observation_generations", {}))
        path = self.observation_path(model_id)
        actual = self._digest(path) if path.exists() else None
        if persisted.get(model_id) == actual:
            if not self.manifest_path.exists():
                self._write_manifest(manifest)
            return manifest
        dirty = set(manifest.get("dirty_model_ids", []))
        dirty.add(model_id)
        if actual is None:
            persisted.pop(model_id, None)
        else:
            persisted[model_id] = actual
        manifest["dirty_model_ids"] = sorted(dirty)
        manifest["observation_generations"] = persisted
        manifest["source_generation"] = int(manifest.get("source_generation", 0)) + 1
        manifest["status"] = "dirty"
        self._write_manifest(manifest)
        return manifest

    def upsert_observations(
        self, model_id: str, rows: pd.DataFrame
    ) -> ObservationUpsertResult:
        incoming = normalize_quality_observations(rows)
        validate_quality_observations(incoming)
        if incoming.empty:
            with self.transaction():
                self._reconcile_model_locked(str(model_id))
            current = self.load_observations(model_id)
            return ObservationUpsertResult(model_id, 0, 0, 0, len(current))
        if set(incoming["model_id"].dropna().astype(str)) != {str(model_id)}:
            raise ValueError("Observation rows must belong to the requested model_id")
        if incoming[list(OBSERVATION_KEY)].isna().any().any():
            raise ValueError("Canonical observation key cannot contain null values")
        incoming = incoming.drop_duplicates(list(OBSERVATION_KEY), keep="last")
        with self.transaction():
            # This repairs a prior interruption between Parquet publication and
            # manifest publication before deciding whether the upsert is a no-op.
            self._reconcile_model_locked(str(model_id))
            current = self.load_observations(model_id)
            current_indexed = current.set_index(list(OBSERVATION_KEY), drop=False)
            incoming_indexed = incoming.set_index(list(OBSERVATION_KEY), drop=False)
            added_keys = incoming_indexed.index.difference(current_indexed.index)
            common_keys = incoming_indexed.index.intersection(current_indexed.index)
            updated = 0
            unchanged = 0
            updated_keys: list[object] = []
            for key in common_keys:
                old = self._comparable(current_indexed.loc[[key]]).reset_index(drop=True)
                new = self._comparable(incoming_indexed.loc[[key]]).reset_index(drop=True)
                if old.equals(new):
                    unchanged += 1
                else:
                    updated += 1
                    updated_keys.append(key)
            if not len(added_keys) and not updated:
                return ObservationUpsertResult(
                    model_id, 0, 0, unchanged, len(current)
                )
            retained = current_indexed.drop(
                index=updated_keys, errors="ignore"
            ).reset_index(drop=True)
            changed_incoming = incoming_indexed.loc[
                incoming_indexed.index.isin([*added_keys, *updated_keys])
            ].reset_index(drop=True)
            combined = normalize_quality_observations(
                pd.concat([retained, changed_incoming], ignore_index=True)
            ).sort_values(
                ["session_date", "model_id", "model_version", "prediction_id"],
                kind="stable",
                na_position="last",
            ).reset_index(drop=True)
            path = self.observation_path(model_id)
            self._atomic_parquet(path, combined)

            # Never update a stale manifest object: reload after publishing the
            # observation and merge the latest persistent dirty set.
            manifest = self.load_manifest()
            dirty = set(manifest.get("dirty_model_ids", []))
            dirty.add(str(model_id))
            generations = dict(manifest.get("observation_generations", {}))
            generations[str(model_id)] = self._digest(path)
            manifest["dirty_model_ids"] = sorted(dirty)
            manifest["observation_generations"] = generations
            manifest["source_generation"] = int(manifest.get("source_generation", 0)) + 1
            manifest["status"] = "dirty"
            self._write_manifest(manifest)
            return ObservationUpsertResult(
                model_id,
                len(added_keys),
                updated,
                unchanged,
                len(combined),
            )

    def mark_model_clean(self, model_id: str) -> dict[str, Any]:
        """Explicit future-phase hook; observations never clear dirty implicitly."""

        with self.transaction():
            manifest = self.load_manifest()
            dirty = set(manifest.get("dirty_model_ids", []))
            dirty.discard(str(model_id))
            manifest["dirty_model_ids"] = sorted(dirty)
            manifest["last_completed_model_id"] = str(model_id)
            manifest["status"] = "dirty" if dirty else "ready"
            self._write_manifest(manifest)
            return manifest


def reconcile_quality_observations(
    quality_repository: ProductionQualityRepository,
    observations: pd.DataFrame,
) -> dict[str, ObservationUpsertResult]:
    """Explicit migration/reconciliation entry point; never runs automatically."""

    normalized = normalize_quality_observations(observations)
    results: dict[str, ObservationUpsertResult] = {}
    for model_id, group in normalized.groupby("model_id", sort=True, dropna=False):
        if pd.isna(model_id):
            raise ValueError("Canonical observation is missing model_id")
        results[str(model_id)] = quality_repository.upsert_observations(
            str(model_id), group
        )
    return results
