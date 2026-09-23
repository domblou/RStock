import pandas as pd
import pytest

from rstock.traceability import prepared_dataset_hash, verify_prepared_dataset_digest


def _prepared() -> pd.DataFrame:
    return pd.DataFrame({"AAA.close": [10.0, 11.0]}, index=pd.to_datetime(["2026-01-02", "2026-01-05"]))


def test_matching_source_digest_is_verified():
    prepared = _prepared()
    result = verify_prepared_dataset_digest(
        prepared, expected_digest=prepared_dataset_hash(prepared), required=True,
        run_id="child", source_run_id="wf", cutoff="2026-01-05", stage="xgboost_calibration",
    )
    assert result["prepared_dataset_digest_verified"] is True


def test_mismatch_fails_with_full_diagnostic():
    with pytest.raises(ValueError, match="prepared_dataset_digest_mismatch") as error:
        verify_prepared_dataset_digest(
            _prepared(), expected_digest="expected", required=True,
            run_id="child", source_run_id="wf", cutoff="2026-01-05", stage="threshold_calibration",
        )
    message = str(error.value)
    assert "expected_digest=expected" in message
    assert "actual_digest=" in message
    assert "source_run_id=wf" in message
    assert "cutoff=2026-01-05" in message


def test_legacy_missing_digest_is_explicitly_unverified():
    result = verify_prepared_dataset_digest(
        _prepared(), expected_digest=None, required=False,
        run_id="legacy", source_run_id="wf", cutoff="2026-01-05", stage="xgboost_calibration",
    )
    assert result["prepared_dataset_digest_verified"] is False


def test_new_descendant_missing_digest_is_rejected():
    with pytest.raises(ValueError, match="prepared_dataset_digest_missing"):
        verify_prepared_dataset_digest(
            _prepared(), expected_digest=None, required=True,
            run_id="child", source_run_id="wf", cutoff="2026-01-05", stage="xgboost_calibration",
        )
