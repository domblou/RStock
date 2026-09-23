"""Small, deterministic provenance records for prepared RStock datasets."""

from __future__ import annotations

import hashlib
import subprocess
from pathlib import Path

import pandas as pd


def current_git_commit(project_root: Path) -> str | None:
    """Return the current commit when Git is available, otherwise ``None``."""

    try:
        completed = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=project_root,
            check=True,
            capture_output=True,
            text=True,
            timeout=2,
        )
    except (OSError, subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return None
    commit = completed.stdout.strip()
    return commit or None


def prepared_dataset_hash(prepared: pd.DataFrame) -> str:
    """Hash data, index and ordered columns without serializing the dataset."""

    digest = hashlib.sha256()
    digest.update(b"rstock-prepared-dataset-v1\0")
    digest.update(f"rows={len(prepared)};columns={len(prepared.columns)}\0".encode())
    digest.update(
        pd.util.hash_pandas_object(prepared.index, index=False).values.tobytes()
    )
    digest.update(
        pd.util.hash_pandas_object(prepared.columns, index=False).values.tobytes()
    )
    # Some supported pandas releases cannot hash a DataFrame with rows but no
    # columns. Its data contribution is empty; shape, index and columns are
    # already represented above.
    if len(prepared.columns):
        digest.update(
            pd.util.hash_pandas_object(prepared, index=False).values.tobytes()
        )
    else:
        digest.update(b"empty-data-columns\0")
    return digest.hexdigest()


def verify_prepared_dataset_digest(
    prepared: pd.DataFrame,
    *,
    expected_digest: str | None,
    required: bool,
    run_id: str | None,
    source_run_id: str | None,
    cutoff: object | None,
    stage: str,
) -> dict[str, object]:
    """Compare a reconstructed dataset with its persisted source population."""

    actual_digest = prepared_dataset_hash(prepared)
    details: dict[str, object] = {
        "source_prepared_dataset_sha256": expected_digest,
        "actual_prepared_dataset_sha256": actual_digest,
        "prepared_dataset_digest_verified": False,
    }
    if expected_digest is None:
        if required:
            raise ValueError(
                "prepared_dataset_digest_missing "
                f"run_id={run_id or 'unknown'} source_run_id={source_run_id or 'unknown'} "
                f"cutoff={cutoff or 'unknown'} stage={stage}"
            )
        return details
    if expected_digest != actual_digest:
        raise ValueError(
            "prepared_dataset_digest_mismatch "
            f"run_id={run_id or 'unknown'} source_run_id={source_run_id or 'unknown'} "
            f"cutoff={cutoff or 'unknown'} stage={stage} "
            f"expected_digest={expected_digest} actual_digest={actual_digest}"
        )
    details["prepared_dataset_digest_verified"] = True
    return details


def prepared_dataset_traceability(
    prepared: pd.DataFrame,
    *,
    project_root: Path,
    symbols_used: int,
    source_prepared_dataset_sha256: str | None = None,
    digest_verification: dict[str, object] | None = None,
) -> dict[str, object]:
    """Build compact provenance metadata for one prepared dataset."""

    last_date = None
    if len(prepared.index):
        last_date = pd.Timestamp(prepared.index.max()).isoformat()
    traceability: dict[str, object] = {
        "git_commit": current_git_commit(project_root),
        "prepared_market_last_date": last_date,
        "symbols_used": int(symbols_used),
        "prepared_dataset_sha256": prepared_dataset_hash(prepared),
    }
    if source_prepared_dataset_sha256 is not None:
        traceability["source_prepared_dataset_sha256"] = (
            source_prepared_dataset_sha256
        )
    if digest_verification is not None:
        traceability.update(digest_verification)
    return traceability
