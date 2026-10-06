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
from typing import Any, Callable, Iterator

import pandas as pd

from .production_domain import ProductionModel, ProductionModelStatus
from .production_quality import PredictionModelStatus, resolve_prediction_model_status


WRITE_ATTEMPTS = 5
WRITE_BACKOFF_SECONDS = 0.02


class ProductionRepository:
    def __init__(self, project_root: Path) -> None:
        self.root = Path(project_root) / "production"
        self.registry_path = self.root / "model_registry.json"
        self.artifacts_root = self.root / "artifacts"
        self.history_root = self.root / "history"
        self.real_trades_path = self.root / "real_trades.json"

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
        from .run_integrity import graph_lock, text_references, validate_publication
        production = next((parent for parent in path.parents if parent.name == "production"), None)
        if production is None:
            ProductionRepository._write_atomic_text(path, text)
            return
        runs_root = production.parent / "runs"
        with graph_lock(runs_root):
            if (runs_root / ".deletions").exists():
                validate_publication(runs_root, {"dependency_run_ids": [
                    target for _, target in text_references(path.name, text)
                ]})
            ProductionRepository._write_atomic_text(path, text)

    @staticmethod
    def _write_atomic_text(path: Path, text: str) -> None:
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

    def _registry_models(self) -> list[dict[str, Any]]:
        if not self.registry_path.exists():
            return []
        payload = json.loads(self.registry_path.read_text(encoding="utf-8"))
        if payload.get("schema_version") != 1:
            raise ValueError("Unsupported production registry schema")
        models = payload.get("models")
        if not isinstance(models, list):
            raise ValueError("Invalid production registry models")
        return [dict(item) for item in models]

    def model_summaries(self) -> list[dict[str, Any]]:
        """Read the registry once without materializing every ProductionModel."""

        return [
            {
                "model_id": str(item.get("model_id") or ""),
                "artifact_version": item.get("artifact_version"),
                "target": item.get("target"),
                "predictors": list(item.get("predictors") or []),
                "status": item.get("status"),
                "created_at": item.get("created_at"),
                "registry_payload": item,
            }
            for item in self._registry_models()
        ]

    @staticmethod
    def model_from_summary(summary: dict[str, Any]) -> ProductionModel:
        payload = summary.get("registry_payload")
        if not isinstance(payload, dict):
            raise ValueError("Production model summary has no registry payload")
        return ProductionModel.from_dict(dict(payload))

    def models(self) -> list[ProductionModel]:
        return [ProductionModel.from_dict(item) for item in self._registry_models()]

    def active_models(self) -> list[ProductionModel]:
        """Return the sole model population allowed in active surveillance."""

        return [model for model in self.models() if model.is_active]

    def tracked_models(self) -> list[ProductionModel]:
        """Models receiving observations; only ACTIVE is production eligible."""

        return [
            model for model in self.models()
            if model.status in {ProductionModelStatus.WATCHING, ProductionModelStatus.ACTIVE}
        ]

    def tracked_model_ids(self) -> frozenset[str]:
        return frozenset(model.model_id for model in self.tracked_models())

    def active_model_ids(self) -> frozenset[str]:
        return frozenset(model.model_id for model in self.active_models())

    def _write_models(self, models: list[ProductionModel]) -> None:
        payload = {
            "schema_version": 1,
            "models": [model.to_dict() for model in models],
        }
        from .run_integrity import graph_lock, validate_publication
        runs_root = self.root.parent / "runs"
        with graph_lock(runs_root):
            validate_publication(runs_root, payload)
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

    def add_promoted_idempotently(
        self, model: ProductionModel, promotion_fingerprint: str
    ) -> tuple[ProductionModel, bool]:
        """Atomically return an existing promotion or publish it once."""

        if not promotion_fingerprint:
            raise ValueError("promotion_fingerprint is required")
        with self.transaction():
            models = self.models()
            for existing in models:
                if (
                    existing.training_metadata.get("promotion_fingerprint")
                    == promotion_fingerprint
                ):
                    return existing, False
            if any(item.model_id == model.model_id for item in models):
                raise ValueError(f"Duplicate model_id: {model.model_id}")
            models.append(model)
            self._write_models(models)
        return model, True

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

    def mutate_model(
        self, model_id: str, mutation: Callable[[ProductionModel], ProductionModel]
    ) -> ProductionModel:
        """Apply a lifecycle change to the latest persisted registry record."""

        with self.transaction():
            models = self.models()
            for index, current in enumerate(models):
                if current.model_id == model_id:
                    updated = mutation(current)
                    if updated.model_id != model_id:
                        raise ValueError("A lifecycle change cannot replace model_id")
                    models[index] = updated
                    self._write_models(models)
                    return updated
        raise KeyError(f"Unknown production model: {model_id}")

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

    def read_active_model_table(self, name: str) -> pd.DataFrame:
        """Read a surveillance view without altering persisted history."""

        return self._read_model_table(
            name, self.active_model_ids(),
            {PredictionModelStatus.ACTIVE.value, PredictionModelStatus.LEGACY_UNKNOWN.value},
        )

    def read_tracked_model_table(self, name: str) -> pd.DataFrame:
        """Read observation and active events for the current tracked population."""

        return self._read_model_table(
            name, self.tracked_model_ids(),
            {item.value for item in PredictionModelStatus},
        )

    def read_watching_model_table(self, name: str) -> pd.DataFrame:
        watching_ids = frozenset(
            model.model_id for model in self.tracked_models()
            if model.status == ProductionModelStatus.WATCHING
        )
        return self._read_model_table(
            name, watching_ids, {PredictionModelStatus.WATCHING.value},
        )

    def _read_model_table(
        self, name: str, model_ids: frozenset[str], allowed_contexts: set[str]
    ) -> pd.DataFrame:
        frame = self.read_table(name)
        if frame.empty:
            return frame
        if "model_id" not in frame:
            return frame.iloc[0:0].copy()
        frame = frame[frame["model_id"].astype(str).isin(model_ids)].copy()
        if frame.empty:
            return frame
        if name == "realized_results" and "prediction_id" in frame:
            predictions = self.read_table("predictions")
            if "prediction_id" not in predictions:
                return frame.iloc[0:0].copy()
            contexts = {
                str(row["prediction_id"]): resolve_prediction_model_status(row)
                for row in predictions.to_dict("records")
            }
            return frame[frame["prediction_id"].astype(str).map(contexts).isin(allowed_contexts)].copy()
        if name in {"predictions", "signals"}:
            contexts = frame.apply(resolve_prediction_model_status, axis=1)
            return frame[contexts.isin(allowed_contexts)].copy()
        return frame

    def write_table(self, name: str, frame: pd.DataFrame) -> None:
        with self.transaction():
            self._atomic_text(
                self.history_root / f"{name}.csv", frame.to_csv(index=False)
            )

    def append_table(
        self, name: str, rows: pd.DataFrame, *, key: str,
        expected_models: tuple[ProductionModel, ...] | None = None,
    ) -> pd.DataFrame:
        return self.append_tables(
            {name: (rows, key)}, expected_models=expected_models
        )[name]

    def append_tables(
        self, updates: dict[str, tuple[pd.DataFrame, str]], *,
        expected_models: tuple[ProductionModel, ...] | None = None,
    ) -> dict[str, pd.DataFrame]:
        """Publish one or several operational histories as one directory swap."""

        if not updates:
            return {}
        from .run_integrity import graph_lock, references, validate_publication
        runs_root = self.root.parent / "runs"
        with self.transaction(), graph_lock(runs_root):
            if expected_models is not None:
                current = {model.model_id: model for model in self.models()}
                for expected in expected_models:
                    actual = current.get(expected.model_id)
                    if (
                        actual is None
                        or actual.status != expected.status
                        or actual.artifact_version != expected.artifact_version
                        or len(actual.status_history) != len(expected.status_history)
                    ):
                        raise ValueError(
                            "Tracked model lifecycle changed during prediction; rerun with current models"
                        )
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
                        if name == "predictions" and "prediction_origin" in previous:
                            original_origins = previous.drop_duplicates(key, keep="first").set_index(key)[
                                "prediction_origin"
                            ]
                            original_origins = original_origins[
                                original_origins.notna() & original_origins.astype(str).ne("")
                            ]
                            persisted_origin = combined[key].map(original_origins)
                            if "prediction_origin" not in combined:
                                combined["prediction_origin"] = None
                            combined["prediction_origin"] = persisted_origin.where(
                                persisted_origin.notna(), combined["prediction_origin"]
                            )
                        if (
                            "model_status_at_prediction" in combined
                            and name in {"predictions", "signals"}
                            and key == (
                                "prediction_id" if name == "predictions" else "signal_id"
                            )
                        ):
                            # A retry may correct an event's payload, but a later
                            # activation must never relabel its original scope.
                            first_context = {
                                str(item[key]): resolve_prediction_model_status(item)
                                for item in reversed(combined.to_dict("records"))
                            }
                            combined["model_status_at_prediction"] = combined[key].astype(str).map(
                                first_context
                            )
                        # Stable event identities are correction keys: a later
                        # publication replaces the prior payload atomically.
                        # This lets derived quality detect late corrected
                        # realized outcomes without duplicating a trade.
                        combined = combined.drop_duplicates(key, keep="last")
                    if (runs_root / ".deletions").exists():
                        columns = [column for column in combined if any(references({str(column): "candidate"}))]
                        validate_publication(runs_root, combined[columns].to_dict("records"))
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

    def update_real_trades(
        self,
        updater: Callable[[list[dict[str, Any]]], list[dict[str, Any]]],
    ) -> list[dict[str, Any]]:
        """Atomically update the separate user-entered real-trade register."""

        with self.transaction():
            current: list[dict[str, Any]] = []
            if self.real_trades_path.exists():
                payload = json.loads(self.real_trades_path.read_text(encoding="utf-8"))
                if payload.get("schema_version") != 1 or not isinstance(
                    payload.get("trades"), list
                ):
                    raise ValueError("Unsupported real trade registry schema")
                current = [dict(item) for item in payload["trades"] if isinstance(item, dict)]
            updated = updater(current)
            self._atomic_text(
                self.real_trades_path,
                json.dumps(
                    {"schema_version": 1, "trades": updated},
                    indent=2,
                    ensure_ascii=False,
                )
                + "\n",
            )
            return [dict(item) for item in updated]

    def read_real_trades(self) -> list[dict[str, Any]]:
        if not self.real_trades_path.exists():
            return []
        payload = json.loads(self.real_trades_path.read_text(encoding="utf-8"))
        if payload.get("schema_version") != 1 or not isinstance(payload.get("trades"), list):
            raise ValueError("Unsupported real trade registry schema")
        return [dict(item) for item in payload["trades"] if isinstance(item, dict)]
