"""XGBoost model and metadata persistence."""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any


@dataclass(slots=True)
class ModelMetadata:
    schema_version: int
    set_name: str
    observation: str
    features: list[str]
    predictor_columns: list[str]
    error: float
    error_metric: str
    model_file: str
    classification_metrics: dict[str, Any] = field(default_factory=dict)


def prepare_models_directory(path: Path, *, clear_existing: bool = True) -> None:
    path.mkdir(parents=True, exist_ok=True)
    if clear_existing:
        # Phase-1 compatibility: legacy_r/CreateModels.R deletes model files first.
        for child in path.iterdir():
            if child.is_file():
                child.unlink()


def save_model_bundle(model: Any, metadata: ModelMetadata, directory: Path, stem: str) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    model_path = directory / f"{stem}.ubj"
    metadata_path = directory / f"{stem}.metadata.json"
    metadata.model_file = model_path.name
    model.save_model(model_path)
    metadata_path.write_text(
        json.dumps(asdict(metadata), indent=2, sort_keys=True), encoding="utf-8"
    )
    return metadata_path


def iter_model_metadata(directory: Path) -> list[tuple[Path, ModelMetadata]]:
    bundles: list[tuple[Path, ModelMetadata]] = []
    for path in sorted(directory.glob("*.metadata.json")):
        data = json.loads(path.read_text(encoding="utf-8"))
        metadata = ModelMetadata(**data)
        bundles.append((directory / metadata.model_file, metadata))
    return bundles


def load_booster(path: Path) -> Any:
    try:
        import xgboost as xgb
    except ImportError as exc:  # pragma: no cover - depends on runtime install
        raise RuntimeError("Install xgboost to load RStock models") from exc
    booster = xgb.Booster()
    booster.load_model(path)
    return booster
