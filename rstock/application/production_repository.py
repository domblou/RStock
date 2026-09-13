"""Atomic local persistence for production models and operational history."""

from __future__ import annotations

import json
import os
import shutil
import tempfile
import time
import uuid
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator

import pandas as pd

from .production_domain import ProductionModel


WRITE_ATTEMPTS = 5
WRITE_BACKOFF_SECONDS = 0.02


class ProductionRepository:
    def __init__(self, project_root: Path) -> None:
        self.root = Path(project_root) / "production"
        self.registry_path = self.root / "model_registry.json"
        self.artifacts_root = self.root / "artifacts"
        self.history_root = self.root / "history"

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
                    raise TimeoutError("Could not acquire production repository lock")
                time.sleep(0.02)
        try:
            yield
        finally:
            lock.rmdir()

    @staticmethod
    def _atomic_text(path: Path, text: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        for attempt in range(WRITE_ATTEMPTS):
            descriptor, temporary_name = tempfile.mkstemp(
                prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
            )
            temporary = Path(temporary_name)
            try:
                with os.fdopen(descriptor, "w", encoding="utf-8", newline="\n") as stream:
                    stream.write(text)
                    stream.flush()
                    os.fsync(stream.fileno())
                os.replace(temporary, path)
                return
            except OSError as error:
                temporarily_locked = isinstance(error, PermissionError) or getattr(
                    error, "winerror", None
                ) in {5, 32, 33}
                if not temporarily_locked or attempt == WRITE_ATTEMPTS - 1:
                    raise
                time.sleep(WRITE_BACKOFF_SECONDS * (2**attempt))
            finally:
                if temporary.exists():
                    try:
                        temporary.unlink()
                    except OSError:
                        # A failed critical publication is still raised. Cleanup
                        # is best effort when an external Windows process also
                        # holds the temporary file itself.
                        pass
        raise AssertionError("Atomic production write loop exited unexpectedly")

    def models(self) -> list[ProductionModel]:
        if not self.registry_path.exists():
            return []
        payload = json.loads(self.registry_path.read_text(encoding="utf-8"))
        if payload.get("schema_version") != 1:
            raise ValueError("Unsupported production registry schema")
        return [ProductionModel.from_dict(item) for item in payload["models"]]

    def _write_models(self, models: list[ProductionModel]) -> None:
        payload = {
            "schema_version": 1,
            "models": [model.to_dict() for model in models],
        }
        self._atomic_text(
            self.registry_path,
            json.dumps(payload, indent=2, ensure_ascii=False, default=str) + "\n",
        )

    def add(self, model: ProductionModel) -> ProductionModel:
        with self.transaction():
            models = self.models()
            if any(item.model_id == model.model_id for item in models):
                raise ValueError(f"Duplicate model_id: {model.model_id}")
            models.append(model)
            self._write_models(models)
        return model

    def get(self, model_id: str) -> ProductionModel:
        for model in self.models():
            if model.model_id == model_id:
                return model
        raise KeyError(f"Unknown production model: {model_id}")

    def update(self, model: ProductionModel) -> None:
        with self.transaction():
            models = self.models()
            positions = [index for index, item in enumerate(models) if item.model_id == model.model_id]
            if not positions:
                raise KeyError(f"Unknown production model: {model.model_id}")
            models[positions[0]] = model
            self._write_models(models)

    def artifact_directory(self, model_id: str) -> Path:
        return self.artifacts_root / model_id

    def read_table(self, name: str) -> pd.DataFrame:
        path = self.history_root / f"{name}.csv"
        legacy = self.root / f"{name}.csv"
        if not path.exists() and legacy.exists():
            path = legacy
        if not path.exists():
            return pd.DataFrame()
        try:
            return pd.read_csv(path)
        except pd.errors.EmptyDataError:
            return pd.DataFrame()

    def write_table(self, name: str, frame: pd.DataFrame) -> None:
        with self.transaction():
            self._atomic_text(
                self.history_root / f"{name}.csv", frame.to_csv(index=False)
            )

    def append_table(self, name: str, rows: pd.DataFrame, *, key: str) -> pd.DataFrame:
        return self.append_tables({name: (rows, key)})[name]

    def append_tables(
        self, updates: dict[str, tuple[pd.DataFrame, str]]
    ) -> dict[str, pd.DataFrame]:
        """Publish one or several operational histories as one directory swap."""

        if not updates:
            return {}
        with self.transaction():
            self.root.mkdir(parents=True, exist_ok=True)
            staging = Path(
                tempfile.mkdtemp(prefix=".history.staging-", dir=self.root)
            )
            backup: Path | None = None
            combined_tables: dict[str, pd.DataFrame] = {}
            try:
                if self.history_root.exists():
                    shutil.copytree(self.history_root, staging, dirs_exist_ok=True)
                for name, (rows, key) in updates.items():
                    staged_path = staging / f"{name}.csv"
                    if staged_path.exists():
                        try:
                            previous = pd.read_csv(staged_path)
                        except pd.errors.EmptyDataError:
                            previous = pd.DataFrame()
                    else:
                        legacy = self.root / f"{name}.csv"
                        previous = pd.read_csv(legacy) if legacy.exists() else pd.DataFrame()
                    combined = (
                        pd.concat([previous, rows], ignore_index=True)
                        if not previous.empty
                        else rows.copy()
                    )
                    if key in combined:
                        combined = combined.drop_duplicates(key, keep="first")
                    combined.to_csv(staged_path, index=False)
                    combined_tables[name] = combined
                if self.history_root.exists():
                    backup = self.root / f".history.backup-{uuid.uuid4().hex}"
                    os.replace(self.history_root, backup)
                try:
                    os.replace(staging, self.history_root)
                except Exception:
                    if backup is not None and not self.history_root.exists():
                        os.replace(backup, self.history_root)
                    raise
                if backup is not None:
                    shutil.rmtree(backup, ignore_errors=True)
                return combined_tables
            finally:
                if staging.exists():
                    shutil.rmtree(staging, ignore_errors=True)
