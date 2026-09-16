"""Durable storage for completed simulations."""

from __future__ import annotations

import json
import logging
import os
import tempfile
import uuid
from dataclasses import asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import pandas as pd

from .simulation import SimulationMetrics, SimulationResult


LOGGER = logging.getLogger(__name__)


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
        if temporary.exists():
            temporary.unlink(missing_ok=True)


class SimulationRepository:
    """Persist completed simulations below the configured application root."""

    def __init__(self, project_root: Path) -> None:
        self.root = Path(project_root) / "simulations"

    @staticmethod
    def _simulation_id() -> str:
        return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S") + "_" + uuid.uuid4().hex[:10]

    def _directory(self, simulation_id: str) -> Path:
        if not simulation_id or Path(simulation_id).name != simulation_id:
            raise ValueError("Invalid simulation_id")
        return self.root / simulation_id

    def save(
        self,
        result: SimulationResult,
        *,
        parameters: dict[str, Any],
        models: list[dict[str, Any]],
    ) -> dict[str, Any]:
        simulation_id = self._simulation_id()
        directory = self._directory(simulation_id)
        directory.mkdir(parents=True, exist_ok=False)
        metadata = {
            "schema_version": 1,
            "simulation_id": simulation_id,
            "created_at": datetime.now(timezone.utc).isoformat(),
            "status": "completed",
            "parameters": parameters,
            "models": models,
            "metrics": asdict(result.metrics),
        }
        try:
            _atomic_text(directory / "simulation.json", json.dumps(metadata, indent=2, ensure_ascii=False, default=str) + "\n")
            _atomic_text(directory / "trades.csv", result.trades.to_csv(index=False))
            _atomic_text(directory / "cumulative_results.csv", result.cumulative_results.to_csv(index=False))
            _atomic_text(directory / "result_distribution.csv", result.result_distribution.to_csv(index=False))
        except Exception:
            LOGGER.exception("Unable to persist simulation %s", simulation_id)
            for child in directory.iterdir():
                child.unlink(missing_ok=True)
            directory.rmdir()
            raise
        return metadata

    def list_simulations(self) -> list[dict[str, Any]]:
        if not self.root.exists():
            return []
        records: list[dict[str, Any]] = []
        for directory in self.root.iterdir():
            if not directory.is_dir():
                continue
            try:
                metadata = json.loads((directory / "simulation.json").read_text(encoding="utf-8"))
                if metadata.get("schema_version") != 1:
                    raise ValueError("Unsupported simulation schema")
                records.append(metadata)
            except (OSError, ValueError, json.JSONDecodeError) as error:
                LOGGER.warning("Ignoring invalid simulation %s: %s", directory.name, error)
        return sorted(records, key=lambda item: str(item.get("created_at", "")), reverse=True)

    def load(self, simulation_id: str) -> tuple[dict[str, Any], SimulationResult]:
        directory = self._directory(simulation_id)
        metadata = json.loads((directory / "simulation.json").read_text(encoding="utf-8"))
        metrics = SimulationMetrics(**metadata["metrics"])
        result = SimulationResult(
            trades=pd.read_csv(directory / "trades.csv"),
            metrics=metrics,
            cumulative_results=pd.read_csv(directory / "cumulative_results.csv"),
            result_distribution=pd.read_csv(directory / "result_distribution.csv"),
        )
        return metadata, result

    def delete(self, simulation_id: str) -> None:
        directory = self._directory(simulation_id)
        if not directory.exists():
            return
        for child in directory.iterdir():
            if child.is_file():
                child.unlink()
        directory.rmdir()
