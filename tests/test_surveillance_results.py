import json
from types import SimpleNamespace

import pandas as pd

from rstock.application.surveillance import (
    PREDICTION_MAIN_COLUMNS,
    EVALUATED_PREDICTIONS_DISPLAY_COLUMNS,
    EVALUATED_PREDICTIONS_MAIN_COLUMNS,
    SIGNAL_MAIN_COLUMNS,
    build_predictions_view,
    build_evaluated_predictions_view,
    build_signals_view,
    compute_signal_priority_score,
    filter_evaluated_predictions_view,
    prediction_feature_tables,
    prediction_features_table,
    evaluated_predictions_main_table,
    source_observation_tables,
    source_observations_table,
    evaluation_feedback,
    filter_signal_results_view,
    prioritize_signals_view,
    SIGNAL_PRIORITY_POLICY_VERSION,
    signal_priority_model_lookup,
)


def _prediction(
    identifier="prediction-1",
    date="2026-09-14",
    signal_status="bullish_signal",
):
    return {
        "prediction_id": identifier,
        "prediction_date": date,
        "target": "AAA",
        "model_id": "model_complete_identifier",
        "model_version": 3,
        "predictors": '["BBB"]',
        "up_probability": 0.72,
        "down_probability": 0.18,
        "status": "predicted",
        "signal_status": signal_status,
    }


def _signal(identifier="prediction-1"):
    return {
        "signal_id": f"signal-{identifier}",
        "prediction_id": identifier,
        "category": "bullish_signal",
    }


def _realized(identifier="prediction-1", date="2026-09-14", intraday=0.02):
    return {
        "result_id": identifier,
        "prediction_id": identifier,
        "prediction_date": date,
        "target": "AAA",
        "model_id": "model_complete_identifier",
        "open": 100.0,
        "high": 103.0,
        "low": 98.0,
        "close": 102.0,
        "intraday_return": intraday,
        "mfe": 0.03,
        "mae": -0.02,
        "up_target": 1,
        "down_target": 0,
        "recorded_at": "2026-09-14T22:31:45.123456+00:00",
    }


def test_zero_results_and_zero_pending_predictions():
    view = build_evaluated_predictions_view(
        pd.DataFrame(), pd.DataFrame(), pd.DataFrame(), {}
    )

    assert view.table.empty
    assert tuple(view.table.columns) == EVALUATED_PREDICTIONS_DISPLAY_COLUMNS
    empty_main = evaluated_predictions_main_table(view.table)
    assert empty_main.empty
    assert tuple(empty_main.columns) == EVALUATED_PREDICTIONS_MAIN_COLUMNS
    assert view.pending_count == 0
    assert view.next_validation_date is None
    assert evaluation_feedback(0, view) == (
        "info", "Aucune prédiction en attente de validation."
    )


def test_zero_results_with_multiple_pending_predictions_and_market_date():
    predictions = pd.DataFrame([
        _prediction("prediction-1", "2026-09-14"),
        _prediction("prediction-2", "2026-09-15", "no_signal"),
    ])
    signals = pd.DataFrame([
        _signal("prediction-1"),
        {**_signal("prediction-2"), "category": "no_signal"},
    ])

    view = build_evaluated_predictions_view(
        predictions,
        signals,
        pd.DataFrame(),
        {"AAA": "2026-09-11T00:00:00"},
    )
    level, message = evaluation_feedback(0, view)

    assert view.pending_count == 2
    assert view.next_validation_date == "2026-09-14"
    assert view.latest_market_date == "2026-09-11"
    assert level == "info"
    assert "2 prédictions sont encore en attente" in message
    assert "séance du 2026-09-14" in message
    assert "2026-09-11" in message


def test_future_no_signal_prediction_is_pending_without_realized_result():
    prediction = _prediction(
        "future-no-signal", "2026-09-14", signal_status="no_signal"
    )
    prediction["as_of_date"] = "2026-09-11"

    view = build_evaluated_predictions_view(
        pd.DataFrame([prediction]),
        pd.DataFrame(),
        pd.DataFrame(),
        {"AAA": "2026-09-11"},
    )

    assert view.pending_count == 1
    assert view.next_validation_date == "2026-09-14"
    assert "1 prédiction est encore en attente" in evaluation_feedback(0, view)[1]


def test_bullish_and_no_signal_predictions_are_both_pending():
    predictions = pd.DataFrame([
        _prediction("bullish", "2026-09-16", "bullish_signal"),
        _prediction("no-signal", "2026-09-15", "no_signal"),
    ])
    signals = pd.DataFrame([
        _signal("bullish"),
        {**_signal("no-signal"), "category": "no_signal"},
    ])

    view = build_evaluated_predictions_view(
        predictions, signals, pd.DataFrame()
    )

    assert view.pending_count == 2
    assert set(view.pending["prediction_id"]) == {"bullish", "no-signal"}
    assert view.next_validation_date == "2026-09-15"


def test_next_validation_date_is_the_closest_unrealized_prediction():
    predictions = pd.DataFrame([
        _prediction("later", "2026-09-18", "no_signal"),
        _prediction("realized", "2026-09-13", "bullish_signal"),
        _prediction("closest", "2026-09-14", "no_signal"),
    ])
    realized = pd.DataFrame([_realized("realized", "2026-09-13")])

    view = build_evaluated_predictions_view(
        predictions, pd.DataFrame(), realized
    )

    assert view.pending_count == 2
    assert view.next_validation_date == "2026-09-14"


def test_realized_table_joins_history_and_formats_returns_targets_and_dates():
    view = build_evaluated_predictions_view(
        pd.DataFrame([_prediction()]),
        pd.DataFrame([_signal()]),
        pd.DataFrame([_realized()]),
    )
    row = view.table.iloc[0]

    assert row["model_version"] == 3
    assert row["open"] == 100.0
    assert row["high"] == 103.0
    assert row["low"] == 98.0
    assert row["close"] == 102.0
    assert row["intraday_return"] == "2.00%"
    assert row["MFE"] == "3.00%"
    assert row["MAE"] == "-2.00%"
    assert row["up_target_hit"] == "Oui"
    assert row["down_target_hit"] == "Non"
    assert row["realized_at"] == "2026-09-14 22:31"
    assert row["signal_id"] == "signal-prediction-1"
    assert row["category"] == "bullish_signal"
    assert row["predictors"] == '["BBB"]'
    assert row["up_probability"] == "72.00%"
    assert row["down_probability"] == "18.00%"
    assert view.pending_count == 0


def test_evaluated_predictions_include_no_signal_rows_and_filter_display_only():
    predictions = pd.DataFrame([
        _prediction("bullish", signal_status="bullish_signal"),
        _prediction("no-signal", signal_status="no_signal"),
    ])
    realized = pd.DataFrame([
        _realized("bullish"),
        _realized("no-signal"),
    ])
    original_predictions = predictions.copy(deep=True)
    original_realized = realized.copy(deep=True)

    view = build_evaluated_predictions_view(predictions, pd.DataFrame(), realized)
    signals_only = filter_evaluated_predictions_view(view, "Signaux seulement")
    without_signal = filter_evaluated_predictions_view(view, "Sans signal")
    main = evaluated_predictions_main_table(view.table)

    assert set(view.table["category"]) == {"bullish_signal", "no_signal"}
    assert signals_only.technical["prediction_id"].tolist() == ["bullish"]
    assert without_signal.technical["prediction_id"].tolist() == ["no-signal"]
    assert main["Statut initial"].tolist() == ["Signal haussier", "Sans signal"]
    pd.testing.assert_frame_equal(predictions, original_predictions)
    pd.testing.assert_frame_equal(realized, original_realized)


def test_evaluated_predictions_filter_also_applies_to_pending_predictions():
    predictions = pd.DataFrame([
        _prediction("evaluated", signal_status="bullish_signal"),
        _prediction("pending-up", "2026-09-15", "bullish_signal"),
        _prediction("pending-none", "2026-09-16", "no_signal"),
    ])
    realized = pd.DataFrame([_realized("evaluated")])

    view = build_evaluated_predictions_view(predictions, pd.DataFrame(), realized)
    signals_only = filter_evaluated_predictions_view(view, "Signaux seulement")
    without_signal = filter_evaluated_predictions_view(view, "Sans signal")

    assert signals_only.pending["prediction_id"].tolist() == ["pending-up"]
    assert signals_only.pending_count == 1
    assert without_signal.pending["prediction_id"].tolist() == ["pending-none"]


def test_prediction_view_is_concise_formatted_and_keeps_technical_details():
    prediction = _prediction()
    prediction.update({"created_at": "2026-09-12T22:30:00+00:00", "error": None})

    view = build_predictions_view(pd.DataFrame([prediction]))

    assert tuple(view.table.columns) == PREDICTION_MAIN_COLUMNS
    assert view.table.iloc[0].to_dict() == {
        "Date": "2026-09-14",
        "Cible": "AAA",
        "Predictors": "BBB",
        "P(Up)": "72.00%",
        "P(Down)": "18.00%",
        "Signal": "Signal haussier",
    }
    assert "prediction_id" not in view.table
    assert view.technical.iloc[0]["prediction_id"] == "prediction-1"
    assert view.technical.iloc[0]["model_id"] == "model_complete_identifier"


def test_signal_view_separates_real_signals_from_folded_no_signal_rows():
    rows = [
        {**_signal("actual"), **_prediction("actual")},
        {
            **_signal("quiet"),
            **_prediction("quiet", signal_status="no_signal"),
            "category": "no_signal",
        },
    ]

    view = build_signals_view(pd.DataFrame(rows))

    assert tuple(view.signals.table.columns) == SIGNAL_MAIN_COLUMNS
    assert len(view.signals.table) == 1
    assert view.signals.table.iloc[0]["Catégorie"] == "Signal haussier"
    assert len(view.no_signal.table) == 1
    assert view.no_signal.table.iloc[0]["Catégorie"] == "Sans signal"
    assert view.no_signal.technical.iloc[0]["prediction_id"] == "quiet"


def test_signal_view_is_sorted_by_descending_date_and_keeps_ties_stable():
    rows = [
        {**_signal("older"), **_prediction("older", "2026-09-12")},
        {**_signal("latest-a"), **_prediction("latest-a", "2026-09-15")},
        {**_signal("latest-b"), **_prediction("latest-b", "2026-09-15")},
        {**_signal("middle"), **_prediction("middle", "2026-09-14")},
    ]

    view = build_signals_view(pd.DataFrame(rows))

    assert view.signals.technical["prediction_id"].tolist() == [
        "latest-a", "latest-b", "middle", "older",
    ]


def test_signal_period_filter_applies_to_signals_and_no_signal_counter_scope():
    rows = [
        {**_signal("today-bullish"), **_prediction("today-bullish", "2026-09-15")},
        {
            **_signal("today-quiet"),
            **_prediction("today-quiet", "2026-09-15", "no_signal"),
            "category": "no_signal",
        },
        {**_signal("week-bullish"), **_prediction("week-bullish", "2026-09-10")},
        {
            **_signal("old-quiet"),
            **_prediction("old-quiet", "2026-09-08", "no_signal"),
            "category": "no_signal",
        },
    ]
    view = build_signals_view(pd.DataFrame(rows))

    today = filter_signal_results_view(view, "Aujourd’hui", today="2026-09-15")
    week = filter_signal_results_view(view, "7 derniers jours", today="2026-09-15")
    all_rows = filter_signal_results_view(view, "Tous", today="2026-09-15")

    assert today.signals.technical["prediction_id"].tolist() == ["today-bullish"]
    assert today.no_signal.technical["prediction_id"].tolist() == ["today-quiet"]
    assert week.signals.technical["prediction_id"].tolist() == [
        "today-bullish", "week-bullish",
    ]
    assert week.no_signal.technical["prediction_id"].tolist() == ["today-quiet"]
    assert all_rows is view


def test_signal_period_filter_has_an_empty_today_state_without_hiding_other_predictions():
    rows = [
        {**_signal("old-bullish"), **_prediction("old-bullish", "2026-09-14")},
        {
            **_signal("today-quiet"),
            **_prediction("today-quiet", "2026-09-15", "no_signal"),
            "category": "no_signal",
        },
    ]

    today = filter_signal_results_view(
        build_signals_view(pd.DataFrame(rows)),
        "Aujourd’hui",
        today="2026-09-15",
    )

    assert today.signals.table.empty
    assert len(today.no_signal.table) == 1


def _priority_metrics(
    *,
    threshold=0.50,
    precision=0.55,
    directional_return=0.01,
    opposite=0.15,
    sample_size=25,
):
    return {
        "calibrated_threshold": threshold,
        "holdout_precision": precision,
        "holdout_directional_return": directional_return,
        "opposite_move_frequency": opposite,
        "signal_sample_size": sample_size,
    }


def test_signal_priority_score_components_are_monotonic():
    baseline = {
        "signal_edge": 0.05,
        "holdout_precision": 0.50,
        "holdout_directional_return": 0.005,
        "opposite_move_frequency": 0.20,
        "signal_sample_size": 10,
    }
    base_score = compute_signal_priority_score(**baseline)

    improvements = (
        {"signal_edge": 0.15},
        {"holdout_precision": 0.65},
        {"holdout_directional_return": 0.018},
        {"opposite_move_frequency": 0.05},
        {"signal_sample_size": 50},
    )
    for improvement in improvements:
        assert compute_signal_priority_score(
            **{**baseline, **improvement}
        ) > base_score


def test_signal_priority_score_is_bounded_and_missing_metrics_are_safe():
    assert compute_signal_priority_score(
        signal_edge=-100,
        holdout_precision=-100,
        holdout_directional_return=-100,
        opposite_move_frequency=100,
        signal_sample_size=-100,
    ) == 0
    assert compute_signal_priority_score(
        signal_edge=100,
        holdout_precision=100,
        holdout_directional_return=100,
        opposite_move_frequency=-100,
        signal_sample_size=10000,
    ) == 100
    assert compute_signal_priority_score(
        signal_edge=None,
        holdout_precision=None,
        holdout_directional_return=float("nan"),
        opposite_move_frequency=None,
        signal_sample_size=None,
    ) == 30


def test_signal_priority_lookup_uses_frozen_threshold_and_holdout_metrics():
    lookup = signal_priority_model_lookup(
        [
            SimpleNamespace(
                model_id="model-1",
                calibrated_signal_threshold=0.61,
                holdout_signal_metrics={
                    "Precision": 0.69,
                    "DirectionalReturnMean": 0.021,
                    "OppositeMoveFrequency": 0.15,
                    "SignalCount": 42,
                },
            )
        ]
    )

    assert lookup["model-1"] == {
        "calibrated_threshold": 0.61,
        "holdout_precision": 0.69,
        "holdout_directional_return": 0.021,
        "opposite_move_frequency": 0.15,
        "signal_sample_size": 42.0,
    }


def test_signal_priorities_use_operational_score_instead_of_probability_alone():
    rows = [
        {
            **_signal("high-probability"),
            **_prediction("high-probability", "2026-09-15"),
            "model_id": "weak-model",
            "target": "ZZZ",
            "up_probability": 0.90,
        },
        {
            **_signal("relevant"),
            **_prediction("relevant", "2026-09-15"),
            "model_id": "strong-model",
            "target": "AAA",
            "up_probability": 0.65,
        },
    ]
    view = build_signals_view(pd.DataFrame(rows)).signals
    lookup = {
        "weak-model": _priority_metrics(
            threshold=0.89,
            precision=0.40,
            directional_return=0.0,
            opposite=0.30,
            sample_size=5,
        ),
        "strong-model": _priority_metrics(
            threshold=0.50,
            precision=0.70,
            directional_return=0.02,
            opposite=0.0,
            sample_size=50,
        ),
    }

    priorities = prioritize_signals_view(view, model_metrics_by_id=lookup)

    assert priorities.technical["prediction_id"].tolist() == [
        "relevant",
        "high-probability",
    ]
    assert priorities.technical["priority_policy_version"].unique().tolist() == [
        SIGNAL_PRIORITY_POLICY_VERSION
    ]


def test_signal_priorities_are_deterministic_and_respect_limit():
    rows = [
        {
            **_signal(identifier),
            **_prediction(identifier, "2026-09-15"),
            "model_id": f"model-{identifier}",
            "target": target,
            "up_probability": 0.7,
        }
        for identifier, target in (
            ("first", "CCC"),
            ("second", "AAA"),
            ("third", "BBB"),
            ("fourth", "DDD"),
        )
    ]
    view = build_signals_view(pd.DataFrame(rows)).signals
    lookup = {
        f"model-{identifier}": _priority_metrics()
        for identifier in ("first", "second", "third", "fourth")
    }

    priorities = prioritize_signals_view(
        view, model_metrics_by_id=lookup, limit=3
    )
    repeated = prioritize_signals_view(
        view, model_metrics_by_id=lookup, limit=3
    )
    empty = prioritize_signals_view(
        view, model_metrics_by_id=lookup, limit=0
    )

    assert priorities.technical["prediction_id"].tolist() == [
        "second",
        "third",
        "first",
    ]
    assert repeated.technical["prediction_id"].tolist() == [
        "second",
        "third",
        "first",
    ]
    assert len(view.table) == 4
    assert empty.table.empty
    assert empty.technical.empty


def test_signal_view_joins_prediction_audit_for_bullish_and_no_signal_rows():
    bullish = _prediction("actual")
    quiet = _prediction("quiet", signal_status="no_signal")
    for prediction, value in ((bullish, 0.03), (quiet, -0.01)):
        prediction.update({
            "feature_names": json.dumps(["BBB_intraday_J-1"]),
            "features": json.dumps({"BBB_intraday_J-1": value}),
            "source_observations": json.dumps({
                "BBB": [{"date": "2026-09-11", "intraday_return": value}]
            }),
        })
    signals = pd.DataFrame([
        {**_signal("actual"), "category": "bullish_signal"},
        {**_signal("quiet"), "category": "no_signal"},
    ])

    view = build_signals_view(signals, pd.DataFrame([bullish, quiet]))

    for table_view, expected in ((view.signals, 0.03), (view.no_signal, -0.01)):
        record = table_view.technical.iloc[0].to_dict()
        lagged, other = prediction_feature_tables(record)
        assert lagged.iloc[0].to_dict() == {
            "Prédicteur": "BBB",
            "J-1": "3,00 %" if expected > 0 else "-1,00 %",
        }
        assert other.empty
        assert source_observations_table(record).iloc[0].to_dict() == {
            "Symbole source": "BBB", "Date": "2026-09-11",
            "Open": "—", "High": "—", "Low": "—", "Close": "—",
            "Rendement intraday": "3,00 %" if expected > 0 else "-1,00 %",
        }


def test_signal_view_keeps_legacy_prediction_without_snapshot_readable():
    view = build_signals_view(
        pd.DataFrame([{**_signal("legacy"), "category": "bullish_signal"}]),
        pd.DataFrame([_prediction("legacy")]),
    )
    record = view.signals.technical.iloc[0].to_dict()

    assert prediction_features_table(record).empty
    assert source_observations_table(record).empty


def test_prediction_features_are_pivoted_by_symbol_and_numeric_lag():
    record = {
        "feature_names": json.dumps([
            "NVDA_intraday_J-3", "CVX_intraday_J-2", "CVX_intraday_J-1",
            "NVDA_intraday_J-1", "CVX_intraday_J-3", "NVDA_intraday_J-2",
            "wday",
        ]),
        "features": json.dumps({
            "NVDA_intraday_J-3": -0.0071,
            "CVX_intraday_J-2": -0.0213,
            "CVX_intraday_J-1": 0.0096,
            "NVDA_intraday_J-1": -0.0134,
            "CVX_intraday_J-3": 0.0044,
            "NVDA_intraday_J-2": -0.0098,
            "wday": 1,
        }),
    }

    pivoted, fallback = prediction_feature_tables(record)

    assert pivoted.to_dict("records") == [
        {"Prédicteur": "CVX", "J-1": "0,96 %", "J-2": "-2,13 %", "J-3": "0,44 %"},
        {"Prédicteur": "NVDA", "J-1": "-1,34 %", "J-2": "-0,98 %", "J-3": "-0,71 %"},
    ]
    assert fallback.to_dict("records") == [{"Feature": "wday", "Valeur": 1}]


def test_nonconforming_features_fall_back_to_feature_value_table():
    record = {"features": json.dumps({
        "custom_intraday_momentum": 0.0125,
        "calendar_feature": 9,
    })}

    pivoted, fallback = prediction_feature_tables(record)

    assert pivoted.empty
    assert fallback.to_dict("records") == [
        {"Feature": "custom_intraday_momentum", "Valeur": "1,25 %"},
        {"Feature": "calendar_feature", "Valeur": 9},
    ]


def test_source_observations_are_pivoted_sorted_and_readably_formatted():
    record = {"source_observations": json.dumps({
        "NVDA": [{
            "date": "2026-09-09", "open": 180.12345, "high": 182.0,
            "low": 179.5, "close": 181.25, "intraday_return": 0.00625,
        }],
        "CVX": [
            {
                "date": "2026-09-10", "open": 217.4, "high": 218.1234,
                "low": 211.0, "close": 212.769, "intraday_return": -0.0213,
            },
            {
                "date": "2026-09-09", "open": 212.88, "high": 215.28,
                "low": 212.23, "close": 213.81, "intraday_return": 0.0044,
            },
        ],
    })}

    pivoted, fallback = source_observation_tables(record)

    assert tuple(pivoted.columns) == (
        "Symbole source", "Date", "Open", "High", "Low", "Close",
        "Rendement intraday",
    )
    assert list(zip(pivoted["Symbole source"], pivoted["Date"], strict=True)) == [
        ("CVX", "2026-09-09"),
        ("CVX", "2026-09-10"),
        ("NVDA", "2026-09-09"),
    ]
    assert pivoted.iloc[0].to_dict() == {
        "Symbole source": "CVX", "Date": "2026-09-09", "Open": "212.88",
        "High": "215.28", "Low": "212.23", "Close": "213.81",
        "Rendement intraday": "0,44 %",
    }
    assert pivoted.iloc[1]["Open"] == "217.40"
    assert pivoted.iloc[1]["High"] == "218.1234"
    assert pivoted.iloc[1]["Close"] == "212.769"
    assert pivoted.iloc[1]["Rendement intraday"] == "-2,13 %"
    assert fallback.empty


def test_unknown_source_fields_use_the_previous_vertical_fallback():
    pivoted, fallback = source_observation_tables({
        "source_observations": json.dumps({
            "CVX": [{"date": "2026-09-09", "volume": 123456}]
        })
    })

    assert pivoted.empty
    assert fallback.to_dict("records") == [{
        "Symbole source": "CVX", "Date": "2026-09-09",
        "Champ": "volume", "Valeur": 123456,
    }]


def test_missing_snapshot_returns_empty_audit_tables():
    lagged, other_features = prediction_feature_tables({})
    observations, other_observations = source_observation_tables({})

    assert lagged.empty
    assert other_features.empty
    assert observations.empty
    assert other_observations.empty


def test_evaluated_predictions_main_table_hides_technical_columns_and_preserves_formatting():
    view = build_evaluated_predictions_view(
        pd.DataFrame([_prediction()]),
        pd.DataFrame([_signal()]),
        pd.DataFrame([_realized()]),
    )

    main = evaluated_predictions_main_table(view.table)

    assert tuple(main.columns) == EVALUATED_PREDICTIONS_MAIN_COLUMNS
    assert main.iloc[0]["Rendement"] == "2.00%"
    assert main.iloc[0]["Predictors"] == "BBB"
    assert main.iloc[0]["P(Up)"] == "72.00%"
    assert main.iloc[0]["P(Down)"] == "18.00%"
    assert main.iloc[0]["MFE"] == "3.00%"
    assert main.iloc[0]["MAE"] == "-2.00%"
    assert main.iloc[0]["UpTarget"] == "Oui"
    assert main.iloc[0]["DownTarget"] == "Non"
    assert "prediction_id" not in main
    assert "High" not in main
    assert "Low" not in main
    assert view.technical.iloc[0]["prediction_id"] == "prediction-1"


def test_realized_audit_tables_show_snapshot_and_keep_legacy_predictions_readable():
    prediction = _prediction()
    prediction.update({
        "feature_names": json.dumps(["BBB_intraday_J-1"]),
        "features": json.dumps({"BBB_intraday_J-1": 0.03}),
        "source_observations": json.dumps({
            "BBB": [{
                "date": "2026-09-11", "open": 100.0, "close": 103.0,
                "intraday_return": 0.03,
            }]
        }),
    })
    view = build_evaluated_predictions_view(
        pd.DataFrame([prediction]), pd.DataFrame([_signal()]), pd.DataFrame([_realized()])
    )
    record = view.technical.iloc[0].to_dict()

    assert prediction_features_table(record).to_dict("records") == [
        {"Feature": "BBB_intraday_J-1", "Valeur": 0.03}
    ]
    assert source_observations_table(record).iloc[0].to_dict() == {
        "Symbole source": "BBB", "Date": "2026-09-11", "Open": "100.00",
        "High": "—", "Low": "—", "Close": "103.00",
        "Rendement intraday": "3,00 %",
    }

    legacy_view = build_evaluated_predictions_view(
        pd.DataFrame([_prediction("legacy")]),
        pd.DataFrame([_signal("legacy")]),
        pd.DataFrame([_realized("legacy")]),
    )
    legacy_record = legacy_view.technical.iloc[0].to_dict()
    assert prediction_features_table(legacy_record).empty
    assert source_observations_table(legacy_record).empty


def test_new_and_multiple_realized_results_feedback():
    predictions = pd.DataFrame([
        _prediction("prediction-1", "2026-09-14"),
        _prediction("prediction-2", "2026-09-15"),
    ])
    signals = pd.DataFrame([_signal("prediction-1"), _signal("prediction-2")])
    realized = pd.DataFrame([
        _realized("prediction-1", "2026-09-14"),
        _realized("prediction-2", "2026-09-15", -0.01),
    ])
    view = build_evaluated_predictions_view(predictions, signals, realized)

    assert len(view.table) == 2
    assert evaluation_feedback(1, view) == (
        "success", "1 nouvelle prédiction évaluée."
    )
    assert evaluation_feedback(2, view) == (
        "success", "2 nouvelles prédictions évaluées."
    )
