import pandas as pd

from rstock.application.temporal_validation_trace import (
    resolve_lost_candidate_traces,
)


def _candidate(set_id, target, predictors):
    return {
        "symbol_set_id": set_id,
        "target": target,
        "predictors": predictors,
        "direction": "Up",
    }


def _prefilter(target, predictors, statuses=None):
    statuses = statuses or {}
    return pd.DataFrame([
        {
            "Observation": target,
            "Predictor": predictor,
            "Eligible": True,
            "PrefilterStatus": statuses.get(predictor, "retained"),
            "IneligibilityReasons": "[]",
            "RedundantWith": statuses.get(f"{predictor}:with"),
        }
        for predictor in predictors
    ])


def test_ddog_regn_trace_stops_at_wf_qualification():
    lost = [_candidate('["DDOG","REGN"]', "DDOG", ["REGN"])]
    qualification = pd.DataFrame([{
        "Set": '["DDOG","REGN"]', "Eligible": False,
        "IneligibilityReasons": '["worst_window_auc"]', "ROCAUCWorst": 0.402173913,
    }])

    traces = resolve_lost_candidate_traces(
        lost, prefilter=_prefilter("DDOG", ["REGN"]), qualification=qualification,
        final_holdout=pd.DataFrame(),
        config={"qualification_min_worst_window_auc": 0.45},
    )

    assert traces[('["DDOG","REGN"]', "Up")] == {
        "last_stage": "Qualification WF",
        "elimination_reason": "Worst AUC 0.402 < 0.45",
    }


def test_qualification_reasons_are_humanized_in_persisted_order():
    set_id = '["STX","HPE"]'
    traces = resolve_lost_candidate_traces(
        [_candidate(set_id, "STX", ["HPE"])], prefilter=_prefilter("STX", ["HPE"]),
        qualification=pd.DataFrame([{"Set": set_id, "Eligible": False,
            "IneligibilityReasons": '["median_auc","worst_window_auc"]',
            "ROCAUCMedian": 0.538194, "ROCAUCWorst": 0.437361}]),
        final_holdout=pd.DataFrame(),
        config={"qualification_min_median_auc": 0.55, "qualification_min_worst_window_auc": 0.45},
    )
    assert traces[(set_id, "Up")]["elimination_reason"] == "AUC médiane 0.538 < 0.55 · Worst AUC 0.437 < 0.45"


def test_prefilter_reasons_use_prefilter_thresholds_and_preserve_order():
    set_id = '["SPGI","LITE","LMT"]'
    predictors = ["LITE"] + [f"P{index}" for index in range(1, 36)] + ["LMT"]
    prefilter = _prefilter("SPGI", predictors, {"LITE": "rejected_threshold", "LMT": "rejected_top_n"})
    prefilter.loc[prefilter["Predictor"] == "LITE", "Eligible"] = False
    prefilter.loc[prefilter["Predictor"] == "LITE", "IneligibilityReasons"] = '["median_auc","windows_above_random","worst_window_auc"]'
    prefilter.loc[prefilter["Predictor"] == "LITE", "ROCAUCMedian"] = 0.495455
    prefilter.loc[prefilter["Predictor"] == "LITE", "PctWindowsAboveRandom"] = 2 / 7
    prefilter.loc[prefilter["Predictor"] == "LITE", "ROCAUCWorst"] = 0.329404
    traces = resolve_lost_candidate_traces(
        [_candidate(set_id, "SPGI", ["LITE", "LMT"])], prefilter=prefilter,
        qualification=pd.DataFrame(), final_holdout=pd.DataFrame(),
        config={"predictor_prefilter_top_n": 12, "predictor_prefilter_min_median_auc": 0.50, "predictor_prefilter_min_pct_above_random": 0.50, "predictor_prefilter_min_worst_auc": 0.35},
    )
    assert traces[(set_id, "Up")]["elimination_reason"] == "AUC médiane 0.495 < 0.50 · Fenêtres > 0.50 : 28.6 % < 50.0 % · Worst AUC 0.329 < 0.35 · LMT rang 36 > Top 12"


def test_intc_bny_gs_and_intc_gs_use_the_persisted_prefilter_worst_auc():
    prefilter = _prefilter("INTC", ["BNY", "GS"], {"GS": "rejected_threshold"})
    prefilter.loc[prefilter["Predictor"] == "GS", "IneligibilityReasons"] = '["worst_window_auc"]'
    prefilter.loc[prefilter["Predictor"] == "GS", "ROCAUCWorst"] = 0.329404
    lost = [
        _candidate('["INTC","BNY","GS"]', "INTC", ["BNY", "GS"]),
        _candidate('["INTC","GS"]', "INTC", ["GS"]),
    ]
    traces = resolve_lost_candidate_traces(
        lost, prefilter=prefilter, qualification=pd.DataFrame(), final_holdout=pd.DataFrame(),
        config={"predictor_prefilter_min_worst_auc": 0.35},
    )
    assert traces[( '["INTC","BNY","GS"]', "Up")] == {
        "last_stage": "Préfiltre", "elimination_reason": "Worst AUC 0.329 < 0.35"
    }
    assert traces[( '["INTC","GS"]', "Up")] == {
        "last_stage": "Préfiltre", "elimination_reason": "Worst AUC 0.329 < 0.35"
    }


def test_remaining_reason_codes_and_missing_values_have_safe_fallback():
    set_id = '["AAA","BBB"]'
    qualification = pd.DataFrame([{"Set": set_id, "Eligible": False,
        "IneligibilityReasons": '["auc_std","insufficient_windows","insufficient_auc_windows","positive_observations"]',
        "ROCAUCStd": 0.123, "WindowsEvaluated": 3, "AUCWindows": 3, "PositiveObservations": 12}])
    config = {"qualification_max_auc_std": 0.10, "qualification_min_windows": 4, "qualification_min_positive_observations": 20}
    traces = resolve_lost_candidate_traces(
        [_candidate(set_id, "AAA", ["BBB"])], prefilter=_prefilter("AAA", ["BBB"]),
        qualification=qualification, final_holdout=pd.DataFrame(), config=config)
    assert traces[(set_id, "Up")]["elimination_reason"] == "Écart-type AUC 0.123 > 0.10 · Fenêtres valides 3 < 4 · Fenêtres AUC 3 < 4 · Observations positives 12 < 20"
    qualification.loc[0, "ROCAUCStd"] = None
    fallback = resolve_lost_candidate_traces(
        [_candidate(set_id, "AAA", ["BBB"])], prefilter=_prefilter("AAA", ["BBB"]),
        qualification=qualification, final_holdout=pd.DataFrame(), config=config)
    assert fallback[(set_id, "Up")]["elimination_reason"].startswith("auc_std · ")


def test_vlo_tmo_mmm_uses_persisted_prefilter_order_for_local_ranks():
    predictors = [f"P{index}" for index in range(1, 13)] + ["MMM"] + [
        f"P{index}" for index in range(14, 19)
    ] + ["TMO"]
    statuses = {"MMM": "rejected_top_n", "TMO": "rejected_top_n"}
    traces = resolve_lost_candidate_traces(
        [_candidate('["VLO","TMO","MMM"]', "VLO", ["TMO", "MMM"])],
        prefilter=_prefilter("VLO", predictors, statuses),
        qualification=pd.DataFrame(), final_holdout=pd.DataFrame(),
        config={"predictor_prefilter_top_n": 12},
    )

    assert traces[('["VLO","TMO","MMM"]', "Up")] == {
        "last_stage": "Préfiltre",
        "elimination_reason": "MMM rang 13 > Top 12 · TMO rang 19 > Top 12",
    }


def test_prefilter_redundancy_and_missing_artifact_are_descriptive_only():
    traces = resolve_lost_candidate_traces(
        [_candidate('["AAA","BBB"]', "AAA", ["BBB"])],
        prefilter=_prefilter("AAA", ["BBB"], {"BBB": "removed_redundancy", "BBB:with": "CCC"}),
        qualification=pd.DataFrame(), final_holdout=pd.DataFrame(), config={},
    )
    assert traces[('["AAA","BBB"]', "Up")] == {
        "last_stage": "Préfiltre", "elimination_reason": "BBB redondant avec CCC"
    }
    assert resolve_lost_candidate_traces([], prefilter=None, qualification=None, final_holdout=None, config={}) == {}


def test_holdout_and_non_candidate_stops_use_existing_statuses():
    lost = [_candidate('["AAA","BBB"]', "AAA", ["BBB"])]
    qualification = pd.DataFrame([{"Set": '["AAA","BBB"]', "Eligible": True}])
    holdout = pd.DataFrame([{"Set": '["AAA","BBB"]', "FinalConfirmed": False}])
    holdout_trace = resolve_lost_candidate_traces(
        lost, prefilter=_prefilter("AAA", ["BBB"]), qualification=qualification,
        final_holdout=holdout, config={},
    )
    assert holdout_trace[('["AAA","BBB"]', "Up")]["last_stage"] == "Holdout final"

    candidate_trace = resolve_lost_candidate_traces(
        lost, prefilter=_prefilter("AAA", ["BBB"]), qualification=qualification,
        final_holdout=pd.DataFrame([{"Set": '["AAA","BBB"]', "FinalConfirmed": True}]),
        config={}, promotion_lookup={('["AAA","BBB"]', "Up"): {
            "promotion_status": "Non candidat", "promotion_reason": "AUC 0,58 < 0,60"
        }},
    )
    assert candidate_trace[('["AAA","BBB"]', "Up")] == {
        "last_stage": "Calibration seuils", "elimination_reason": "AUC 0,58 < 0,60"
    }
