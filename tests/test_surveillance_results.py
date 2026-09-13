import json

import pandas as pd

from rstock.application.surveillance import (
    PREDICTION_MAIN_COLUMNS,
    REALIZED_DISPLAY_COLUMNS,
    REALIZED_MAIN_COLUMNS,
    SIGNAL_MAIN_COLUMNS,
    build_predictions_view,
    build_realized_results_view,
    build_signals_view,
    prediction_feature_tables,
    prediction_features_table,
    realized_main_table,
    source_observation_tables,
    source_observations_table,
    validation_feedback,
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
    view = build_realized_results_view(
        pd.DataFrame(), pd.DataFrame(), pd.DataFrame(), {}
    )

    assert view.table.empty
    assert tuple(view.table.columns) == REALIZED_DISPLAY_COLUMNS
    empty_main = realized_main_table(view.table)
    assert empty_main.empty
    assert tuple(empty_main.columns) == REALIZED_MAIN_COLUMNS
    assert view.pending_count == 0
    assert view.next_validation_date is None
    assert validation_feedback(0, view) == (
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

    view = build_realized_results_view(
        predictions,
        signals,
        pd.DataFrame(),
        {"AAA": "2026-09-11T00:00:00"},
    )
    level, message = validation_feedback(0, view)

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

    view = build_realized_results_view(
        pd.DataFrame([prediction]),
        pd.DataFrame(),
        pd.DataFrame(),
        {"AAA": "2026-09-11"},
    )

    assert view.pending_count == 1
    assert view.next_validation_date == "2026-09-14"
    assert "1 prédiction est encore en attente" in validation_feedback(0, view)[1]


def test_bullish_and_no_signal_predictions_are_both_pending():
    predictions = pd.DataFrame([
        _prediction("bullish", "2026-09-16", "bullish_signal"),
        _prediction("no-signal", "2026-09-15", "no_signal"),
    ])
    signals = pd.DataFrame([
        _signal("bullish"),
        {**_signal("no-signal"), "category": "no_signal"},
    ])

    view = build_realized_results_view(
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

    view = build_realized_results_view(
        predictions, pd.DataFrame(), realized
    )

    assert view.pending_count == 2
    assert view.next_validation_date == "2026-09-14"


def test_realized_table_joins_history_and_formats_returns_targets_and_dates():
    view = build_realized_results_view(
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


def test_realized_main_table_hides_technical_columns_and_preserves_formatting():
    view = build_realized_results_view(
        pd.DataFrame([_prediction()]),
        pd.DataFrame([_signal()]),
        pd.DataFrame([_realized()]),
    )

    main = realized_main_table(view.table)

    assert tuple(main.columns) == REALIZED_MAIN_COLUMNS
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
    view = build_realized_results_view(
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

    legacy_view = build_realized_results_view(
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
    view = build_realized_results_view(predictions, signals, realized)

    assert len(view.table) == 2
    assert validation_feedback(1, view) == (
        "success", "1 nouveau résultat réalisé ajouté."
    )
    assert validation_feedback(2, view) == (
        "success", "2 nouveaux résultats réalisés ajoutés."
    )
