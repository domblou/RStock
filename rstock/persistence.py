"""XGBoost model and metadata persistence."""

from __future__ import annotations

import json
import shutil
import tempfile
import uuid
from contextlib import contextmanager
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Iterator


MODEL_STORE_MARKER = ".rstock-model-store"


@dataclass(slots=True)
class ModelMetadata:
    schema_version: int
    set_name: str
    observation: str
    features: list[str]
    predictor_columns: list[str]
    error: float
    model_file: str
    market_calendar: str
    train_start: str
    train_end: str
    test_start: str
    test_end: str
    classification_metrics: dict[str, Any] = field(default_factory=dict)


def _validate_replaceable_store(path: Path) -> None:
    if path.is_symlink() or not path.is_dir():
        raise ValueError(f"Model store is not a directory: {path}")
    allowed = {MODEL_STORE_MARKER}
    unexpected = [
        child.name
        for child in path.iterdir()
        if child.is_dir()
        or (
            child.name not in allowed
            and not child.name.endswith(".ubj")
            and not child.name.endswith(".metadata.json")
        )
    ]
    if unexpected:
        raise ValueError(
            f"Refusing to replace model directory containing unmanaged files: {unexpected}"
        )


def _publish_model_store(staging: Path, destination: Path) -> None:
    backup: Path | None = None
    if destination.exists():
        _validate_replaceable_store(destination)
        backup = destination.with_name(f".{destination.name}.backup-{uuid.uuid4().hex}")
        destination.rename(backup)
    try:
        staging.rename(destination)
    except Exception:
        if backup is not None and not destination.exists():
            backup.rename(destination)
        raise
    if backup is not None:
        shutil.rmtree(backup)


@contextmanager
def model_store_transaction(destination: Path) -> Iterator[Path]:
    """Build a complete store beside the destination and publish it atomically."""

    destination = destination.resolve()
    destination.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(
        tempfile.mkdtemp(prefix=f".{destination.name}.staging-", dir=destination.parent)
    )
    (staging / MODEL_STORE_MARKER).write_text("RStock model store\n", encoding="utf-8")
    try:
        yield staging
        _publish_model_store(staging, destination)
    except Exception:
        if staging.exists():
            shutil.rmtree(staging)
        raise


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
        if data.get("schema_version") != 3:
            raise ValueError(
                f"Unsupported model metadata schema in {path}; retrain the models"
            )
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
