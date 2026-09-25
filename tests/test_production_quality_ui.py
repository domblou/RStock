import json

import pandas as pd
import pyarrow as pa
import pytest

from rstock.application.production_domain import ProductionModel, ProductionModelStatus
from rstock.application.production_quality_repository import ProductionQualityRepository
from rstock.application.production_repository import ProductionRepository
from rstock.application.production_quality_ui import (
    baseline_comparison_display_table,
    baseline_comparison_rows,
    evaluated_bullish_signals_display_table,
    evaluated_bullish_signals,
    excluded_observations_display_table,
    filter_quality_models,
    global_quality_kpis,
    load_model_quality_detail,
    load_models_master,
    models_grid,
    performance_windows_display_table,
)


def _production_model(
    model_id="model_0", *, version=1, status=ProductionModelStatus.ACTIVE,
    target="AAA", predictors=("BBB", "P0"),
):
    return ProductionModel(
        model_id=model_id, target=target, predictors=predictors, lag_depth=1,
        target_definition="targets", up_target_threshold=0.01,
        down_target_threshold=0.01,
        xgboost_parameters={"max_depth": 1}, up_threshold=0.6,
        down_threshold=0.4, qualification_rules={},
        source_walk_forward_run="wf", source_xgboost_calibration_run=None,
        source_threshold_calibration_run=None, development_metrics={},
        holdout_metrics={}, created_at="2026-01-01T00:00:00Z",
        status=status, artifact_version=version,
    )


def _master_row(index=0):
    return {
        "model_id": f"model_{index}", "model_version": 1, "target": "AAA",
        "predictors": json.dumps(["BBB", f"P{index}"]), "status": "active",
        "universe_id": "U1", "universe_name": "Univers 1",
        "source_end_to_end_run_id": "e2e_1", "predictor_prefilter_top_n": 12,
        "promotion_date": "2026-01-01", "signal_count_20": 2,
        "signal_count_63": 4, "signal_count_126": 8,
        "mean_return_20": 0.01, "mean_return_63": 0.02, "mean_return_126": 0.03,
        "win_rate_20": 0.5, "win_rate_63": 0.75, "win_rate_126": 0.625,
        "pnl_since_promotion": 500.0, "max_drawdown_dollars": -200.0,
        "max_drawdown_return_points": -0.02, "last_signal_date": "2026-01-09",
        "last_evaluated_date": "2026-01-09", "baseline_status": "available",
        "health_status": "not_evaluated", "quality_updated_at": "2026-01-10T12:00:00Z",
        "trend_63": json.dumps([0.0, 100.0, 50.0]),
    }


def test_master_view_kpis_filters_windows_and_precomputed_trend(tmp_path):
    ProductionRepository(tmp_path).add(_production_model())
    repository = ProductionQualityRepository(tmp_path)
    repository.upsert_master_snapshot(_master_row())

    frame = load_models_master(tmp_path)
    filtered = filter_quality_models(
        frame, statuses=["active"], universes=["Univers 1"], sources=["e2e_1"],
        health=["Données insuffisantes"], query="bbb",
    )

    assert len(filtered) == 1
    assert global_quality_kpis(frame, window=63) == {
        "active_models": 1, "data_insufficient": 1, "mean_return": pytest.approx(0.02),
        "pnl": pytest.approx(500.0), "win_rate": pytest.approx(0.75), "signals": 4,
    }
    assert models_grid(frame, window=20).iloc[0]["Signaux"] == "2"
    assert models_grid(frame, window=126).iloc[0]["Rendement moyen"] == "3,00 %"
    assert models_grid(frame, window=63).iloc[0]["Tendance 63"] == [0.0, 100.0, 50.0]


def test_models_grid_normalizes_mixed_enriched_columns_for_arrow():
    top_n_values = [12, 20, float("nan"), None, "—"]
    rows = []
    for index, top_n in enumerate(top_n_values):
        row = _master_row(index)
        row["predictor_prefilter_top_n"] = top_n
        row["promotion_date"] = (
            pd.Timestamp("2026-01-01 16:45:00+00:00") if index == 0 else row["promotion_date"]
        )
        row["last_signal_date"] = None if index == 1 else row["last_signal_date"]
        row["signal_count_63"] = "4" if index == 2 else row["signal_count_63"]
        rows.append(row)
    rows[3].update(
        universe_name=None, source_end_to_end_run_id=None, mean_return_63=None,
        win_rate_63=None, pnl_since_promotion=None, max_drawdown_dollars=None,
    )
    frame = pd.DataFrame(rows)
    frame["health_label"] = "Données insuffisantes"
    frame["trend_63_values"] = frame["trend_63"].map(json.loads)

    grid = models_grid(frame, window=63)
    arrow = pa.Table.from_pandas(grid, preserve_index=False)

    assert grid["Top-N"].tolist() == ["12", "20", "—", "—", "—"]
    assert str(grid["Top-N"].dtype) == "string"
    assert str(grid["Promotion"].dtype) == "string"
    assert str(grid["Dernier signal"].dtype) == "string"
    assert grid["Promotion"].iloc[0] == "2026-01-01"
    assert grid["Dernier signal"].iloc[1] == "—"
    assert grid["Univers"].iloc[3] == "—"
    assert grid["Source"].iloc[3] == "—"
    assert grid["Rendement moyen"].iloc[0] == "2,00 %"
    assert grid["Trades gagnants"].iloc[0] == "75,00 %"
    assert grid["P&L cumulé"].iloc[0] == "500,00 $"
    assert grid["Drawdown"].iloc[0] == "-200,00 $"
    assert grid["Rendement moyen"].iloc[3] == "—"
    assert grid["Signaux"].tolist() == ["4", "4", "4", "4", "4"]
    assert arrow.num_rows == 5


def test_registry_population_survives_missing_quality_and_keeps_status_filters(tmp_path):
    production = ProductionRepository(tmp_path)
    production.add(_production_model("active", predictors=("BBB",)))
    production.add(_production_model(
        "inactive", status=ProductionModelStatus.INACTIVE,
        target="CCC", predictors=("DDD",),
    ))
    production.add(_production_model(
        "retired", status=ProductionModelStatus.RETIRED,
        target="EEE", predictors=("FFF",),
    ))

    frame = load_models_master(tmp_path)

    assert frame["model_id"].tolist() == ["active", "inactive", "retired"]
    assert frame["status"].tolist() == ["active", "inactive", "retired"]
    assert frame["health_label"].tolist() == ["Non calculé"] * 3
    assert frame.attrs["quality_snapshot_available"] is False
    assert models_grid(frame, window=63)["Santé"].tolist() == ["Non calculé"] * 3
    assert filter_quality_models(frame, statuses=["inactive"])["model_id"].tolist() == [
        "inactive"
    ]


def test_new_registry_model_is_visible_before_next_quality_snapshot(tmp_path):
    production = ProductionRepository(tmp_path)
    production.add(_production_model("model_0"))
    ProductionQualityRepository(tmp_path).upsert_master_snapshot(_master_row())
    production.add(_production_model(
        "new_model", target="NEW", predictors=("SPY",),
        status=ProductionModelStatus.CANDIDATE,
    ))

    frame = load_models_master(tmp_path).set_index("model_id")

    assert set(frame.index) == {"model_0", "new_model"}
    assert frame.loc["new_model", "status"] == "candidate"
    assert frame.loc["new_model", "target"] == "NEW"
    assert frame.loc["new_model", "health_label"] == "Non calculé"
    assert isinstance(frame.loc["new_model", "registry_payload"], dict)
    selected = production.model_from_summary({
        "registry_payload": frame.loc["new_model", "registry_payload"]
    })
    assert selected.model_id == "new_model"
    assert selected.status == ProductionModelStatus.CANDIDATE


def test_orphan_quality_is_excluded_from_operational_population(tmp_path):
    ProductionRepository(tmp_path).add(_production_model("registry_model"))
    ProductionQualityRepository(tmp_path).upsert_master_snapshot(_master_row(99))

    frame = load_models_master(tmp_path)

    assert frame["model_id"].tolist() == ["registry_model"]
    assert frame.attrs["orphan_quality_count"] == 1
    assert frame.iloc[0]["health_label"] == "Non calculé"


def test_quality_from_a_different_model_version_is_neutralized(tmp_path):
    ProductionRepository(tmp_path).add(_production_model("model_0", version=2))
    ProductionQualityRepository(tmp_path).upsert_master_snapshot(_master_row())

    row = load_models_master(tmp_path).iloc[0]

    assert row["model_version"] == 2
    assert row["status"] == "active"
    assert row["quality_available"]
    assert not row["quality_version_matches"]
    assert row["health_label"] == "Non calculé — version différente"
    assert pd.isna(row["mean_return_63"])
    assert pd.isna(row["pnl_since_promotion"])


def test_registry_identity_and_status_override_matching_quality_snapshot(tmp_path):
    ProductionRepository(tmp_path).add(_production_model(
        "model_0", status=ProductionModelStatus.INACTIVE,
        target="CURRENT", predictors=("NOW",),
    ))
    stale_identity = _master_row()
    stale_identity.update(target="STALE", predictors=json.dumps(["OLD"]), status="active")
    ProductionQualityRepository(tmp_path).upsert_master_snapshot(stale_identity)

    row = load_models_master(tmp_path).iloc[0]

    assert row["target"] == "CURRENT"
    assert json.loads(row["predictors"]) == ["NOW"]
    assert row["status"] == "inactive"
    assert row["mean_return_63"] == pytest.approx(0.02)


def test_detail_neutralizes_an_incompatible_quality_partition(tmp_path):
    quality = ProductionQualityRepository(tmp_path)
    quality.write_model_snapshot("model_0", {
        "model_id": "model_0", "identity": {"model_version": 1},
        "health_status": "not_evaluated",
    })
    quality.write_lineage("model_0", {"model_id": "model_0", "model_version": 1})

    detail = load_model_quality_detail(tmp_path, "model_0", model_version=2)

    assert detail.quality_state == "version_mismatch"
    assert detail.snapshot is None
    assert detail.lineage is None
    assert detail.series.empty
    assert detail.observations.empty


def test_catalogue_load_does_not_materialize_production_models(monkeypatch, tmp_path):
    production = ProductionRepository(tmp_path)
    production.add(_production_model())

    def fail_materialization(cls, values):
        raise AssertionError("grid must not materialize ProductionModel rows")

    monkeypatch.setattr(ProductionModel, "from_dict", classmethod(fail_materialization))

    frame = load_models_master(tmp_path)

    assert frame["model_id"].tolist() == ["model_0"]


def test_model_detail_loads_only_requested_partition(monkeypatch, tmp_path):
    repository = ProductionQualityRepository(tmp_path)
    repository.write_lineage("model_A", {"model_id": "model_A", "target": "AAA"})
    repository.write_model_snapshot("model_A", {"model_id": "model_A", "health_status": "not_evaluated"})
    repository.write_model_series("model_A", pd.DataFrame({"session_date": [pd.Timestamp("2026-01-05")]}))
    loaded = []
    original = ProductionQualityRepository.load_observations

    def record_load(self, model_id):
        loaded.append(model_id)
        return original(self, model_id)

    monkeypatch.setattr(ProductionQualityRepository, "load_observations", record_load)
    detail = load_model_quality_detail(tmp_path, "model_A")

    assert detail.lineage["target"] == "AAA"
    assert loaded == ["model_A"]


def test_latest_signals_exclude_backfill_pending_and_use_strict_win_rule():
    rows = []
    for index, (origin, status, value) in enumerate([
        ("scheduled_live", "evaluated", 0.01),
        ("legacy_inferred_live", "evaluated", 0.0),
        ("scheduled_live", "evaluated", -0.01),
        ("operational_backfill", "evaluated", 0.50),
        ("scheduled_live", "pending", 0.50),
    ]):
        rows.append({
            "prediction_origin": origin, "evaluation_status": status,
            "is_bullish_signal": True, "session_date": pd.Timestamp("2026-01-05") + pd.Timedelta(days=index),
            "intraday_return": value, "target": "AAA", "up_probability": 0.7,
            "up_threshold": 0.6, "down_probability": 0.1, "down_threshold": 0.4,
            "mfe": 0.02, "mae": -0.01,
        })
    result = evaluated_bullish_signals(pd.DataFrame(rows))

    assert result["verdict"].tolist() == ["Perdant", "Nul", "Gagnant"]
    assert sorted(result["pnl"].tolist()) == [-100.0, 0.0, 100.0]


def test_detail_display_tables_use_compact_dates_and_french_formats():
    baseline = pd.DataFrame([{
        "Métrique": "Rendement moyen", "À la promotion": 0.0042,
        "Actuel": None, "Écart": -0.0099,
    }])
    signals = pd.DataFrame([{
        "session_date": "2026-09-25T01:15:42.424300+00:00", "target": "AMD",
        "up_probability": 0.7, "up_threshold": 0.6, "down_probability": 0.1,
        "down_threshold": 0.4, "intraday_return": 0.0123, "pnl": 123.45,
        "mfe": 0.02, "mae": -0.01, "verdict": "Gagnant",
    }])
    excluded = pd.DataFrame([{
        "session_date": "2026-09-24T14:00:00+00:00",
        "exclusion_reason": None,
    }])

    baseline_display = baseline_comparison_display_table(baseline)
    signals_display = evaluated_bullish_signals_display_table(signals)
    excluded_display = excluded_observations_display_table(excluded)

    assert baseline_display.iloc[0].to_dict() == {
        "Métrique": "Rendement moyen", "À la promotion": "0,42 %",
        "Actuel": "—", "Écart": "-0,99 %",
    }
    assert signals_display.iloc[0].to_dict() == {
        "Date": "2026-09-25", "Cible": "AMD", "Prob. Up": "70,00 %",
        "Seuil Up": "60,00 %", "Prob. Down": "10,00 %", "Seuil Down": "40,00 %",
        "Rendement": "1,23 %", "P&L": "123,45 $", "MFE": "2,00 %",
        "MAE": "-1,00 %", "Verdict": "Gagnant",
    }
    assert excluded_display.iloc[0].to_dict() == {
        "Date": "2026-09-24", "Raison": "—", "Champs invalides": "—",
    }


def test_performance_windows_display_table_formats_existing_metrics_only():
    table = performance_windows_display_table({
        20: {"mean_intraday_return": 0.0012, "win_rate": 0.5, "signal_count": 3},
        63: {"mean_intraday_return": None, "win_rate": None, "signal_count": None},
        126: {"mean_intraday_return": -0.0041, "win_rate": 0.6667, "signal_count": 12},
    })

    assert table.to_dict("records") == [
        {"Fenêtre": "20 séances", "Rendement moyen": "0,12 %", "Trades gagnants": "50,00 %", "Signaux": "3"},
        {"Fenêtre": "63 séances", "Rendement moyen": "—", "Trades gagnants": "—", "Signaux": "—"},
        {"Fenêtre": "126 séances", "Rendement moyen": "-0,41 %", "Trades gagnants": "66,67 %", "Signaux": "12"},
    ]


def test_missing_baseline_and_large_master_transform_are_safe():
    assert baseline_comparison_rows({"since_promotion": {}}, None).empty
    frame = pd.DataFrame([_master_row(index) for index in range(5000)])
    frame["health_label"] = "Données insuffisantes"
    frame["target_search"] = frame["target"].str.casefold()
    frame["predictors_search"] = frame["predictors"].str.casefold()
    frame["trend_63_values"] = frame["trend_63"].map(json.loads)

    filtered = filter_quality_models(frame, query="p4999")

    assert filtered["model_id"].tolist() == ["model_4999"]


def test_baseline_comparison_uses_persisted_values_without_recalculation():
    snapshot = {
        "since_promotion": {
            "mean_intraday_return": 0.02,
            "median_intraday_return": 0.01,
            "win_rate": 0.60,
            "signal_rate": 0.25,
            "mean_mfe": 0.03,
            "mean_mae": -0.01,
        },
        "baseline_comparison": {
            "delta_mean_return": 0.005,
            "delta_median_return": 0.002,
            "delta_win_rate": 0.05,
            "delta_mean_mfe": 0.004,
            "delta_mean_mae": -0.001,
        },
    }
    baseline = {
        "availability_status": "available",
        "metrics": {
            "mean_intraday_return": 0.015,
            "median_intraday_return": 0.008,
            "win_rate_strict_gt_0": 0.55,
            "signal_rate": 0.20,
            "mean_mfe": 0.026,
            "mean_mae": -0.009,
        },
    }

    comparison = baseline_comparison_rows(snapshot, baseline)

    win_rate = comparison.loc[comparison["Métrique"] == "Taux de trades > 0"].iloc[0]
    assert win_rate["À la promotion"] == pytest.approx(0.55)
    assert win_rate["Actuel"] == pytest.approx(0.60)
    assert win_rate["Écart"] == pytest.approx(0.05)
