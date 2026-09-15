from pathlib import Path

import pandas as pd

import rstock.traceability as traceability


def _prepared() -> pd.DataFrame:
    return pd.DataFrame(
        {"AAA.Open": [100.0, 101.0], "BBB.Close": [200.0, 201.0]},
        index=pd.DatetimeIndex(["2026-01-02", "2026-01-05"]),
    )


def test_prepared_dataset_hash_is_stable_for_identical_data():
    prepared = _prepared()

    assert traceability.prepared_dataset_hash(prepared) == traceability.prepared_dataset_hash(
        prepared.copy()
    )


def test_prepared_dataset_hash_changes_with_data_index_columns_or_order():
    prepared = _prepared()
    changed_value = prepared.copy()
    changed_value.iloc[0, 0] = 99.0
    changed_index = prepared.copy()
    changed_index.index = pd.DatetimeIndex(["2026-01-01", "2026-01-05"])
    changed_column_names = prepared.rename(columns={"AAA.Open": "CCC.Open"})
    changed_columns = prepared.loc[:, ["BBB.Close", "AAA.Open"]]

    source = traceability.prepared_dataset_hash(prepared)
    assert traceability.prepared_dataset_hash(changed_value) != source
    assert traceability.prepared_dataset_hash(changed_index) != source
    assert traceability.prepared_dataset_hash(changed_column_names) != source
    assert traceability.prepared_dataset_hash(changed_columns) != source


def test_traceability_remains_valid_without_git(monkeypatch, tmp_path):
    def git_is_unavailable(*args, **kwargs):
        raise FileNotFoundError

    monkeypatch.setattr(traceability.subprocess, "run", git_is_unavailable)

    values = traceability.prepared_dataset_traceability(
        _prepared(), project_root=Path(tmp_path), symbols_used=2
    )

    assert values["git_commit"] is None
    assert values["symbols_used"] == 2
    assert values["prepared_market_last_date"] == "2026-01-05T00:00:00"
    assert len(values["prepared_dataset_sha256"]) == 64
