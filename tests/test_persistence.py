import json

import pytest

from rstock.persistence import (
    MODEL_STORE_MARKER,
    iter_model_metadata,
    model_store_transaction,
)


def _managed_store(path, value="old"):
    path.mkdir()
    (path / MODEL_STORE_MARKER).write_text("RStock model store\n", encoding="utf-8")
    (path / "model_000000.ubj").write_text(value, encoding="utf-8")


def test_failed_model_build_keeps_existing_store(tmp_path):
    destination = tmp_path / "Models"
    _managed_store(destination)

    with pytest.raises(RuntimeError):
        with model_store_transaction(destination) as staging:
            (staging / "model_000000.ubj").write_text("new", encoding="utf-8")
            raise RuntimeError("training failed")

    assert (destination / "model_000000.ubj").read_text(encoding="utf-8") == "old"
    assert not list(tmp_path.glob(".Models.staging-*"))


def test_successful_model_build_atomically_replaces_managed_store(tmp_path):
    destination = tmp_path / "Models"
    _managed_store(destination)

    with model_store_transaction(destination) as staging:
        (staging / "model_000000.ubj").write_text("new", encoding="utf-8")

    assert (destination / "model_000000.ubj").read_text(encoding="utf-8") == "new"
    assert not list(tmp_path.glob(".Models.backup-*"))


def test_unmanaged_files_prevent_model_store_replacement(tmp_path):
    destination = tmp_path / "Models"
    destination.mkdir()
    (destination / "notes.txt").write_text("keep me", encoding="utf-8")

    with pytest.raises(ValueError, match="unmanaged files"):
        with model_store_transaction(destination):
            pass

    assert (destination / "notes.txt").read_text(encoding="utf-8") == "keep me"


def test_obsolete_model_metadata_requires_retraining(tmp_path):
    (tmp_path / "old.metadata.json").write_text(
        json.dumps({"schema_version": 2}), encoding="utf-8"
    )

    with pytest.raises(ValueError, match="retrain"):
        iter_model_metadata(tmp_path)
