from pathlib import Path

from rstock.application.production_quality_phase9 import (
    _inventory,
    backup_quality,
    rollback_quality,
)


def test_phase9_backup_and_rollback_restore_exact_quality_tree(tmp_path):
    quality = tmp_path / "production" / "quality"
    (quality / "lineage").mkdir(parents=True)
    (quality / "lineage" / "model.json").write_text("before\n", encoding="utf-8")
    expected = _inventory(quality)
    backup = tmp_path / "phase9_backups" / "quality_before"

    manifest = backup_quality(tmp_path, backup)
    (quality / "lineage" / "model.json").write_text("after\n", encoding="utf-8")
    (quality / "new.txt").write_text("new\n", encoding="utf-8")
    rollback_quality(tmp_path, backup)

    assert manifest["inventory"] == expected
    assert _inventory(quality) == expected


def test_phase9_rollback_restores_absent_quality_state(tmp_path):
    backup = tmp_path / "phase9_backups" / "quality_absent"
    backup_quality(tmp_path, backup)
    quality = tmp_path / "production" / "quality"
    quality.mkdir(parents=True)
    (quality / "generated.txt").write_text("generated\n", encoding="utf-8")

    rollback_quality(tmp_path, backup)

    assert not quality.exists()
