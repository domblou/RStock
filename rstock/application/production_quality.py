"""Versioned contracts for canonical Production quality observations.

This module deliberately contains no rolling metrics, health policy or UI
logic.  It turns persisted operational events into a typed, reconstructible
observation ledger for the later quality phases.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from datetime import datetime, timezone
from enum import Enum
from typing import Any, Mapping, Sequence

import pandas as pd
import numpy as np
import exchange_calendars as xcals


OBSERVATION_SCHEMA_VERSION = 1
ORIGIN_INFERENCE_VERSION = 1
MONITORING_NOTIONAL_PER_SIGNAL = 10_000.0
MONITORING_NOTIONAL_RULE_VERSION = 1
QUALITY_METRICS_VERSION = 1
NOTIONAL_POLICY_VERSION = 1
DRAW_DOWN_POLICY_VERSION = 1
WINDOW_POLICY_VERSION = 1
BASELINE_COMPARISON_VERSION = 1
MONITORING_NOTIONAL = 10_000.0
LIVE_PREDICTION_ORIGINS = frozenset({
    "scheduled_live", "legacy_inferred_live",
})
QUALITY_WINDOWS = (20, 63, 126)


class PredictionOrigin(str, Enum):
    SCHEDULED_LIVE = "scheduled_live"
    OPERATIONAL_BACKFILL = "operational_backfill"
    LEGACY_INFERRED_LIVE = "legacy_inferred_live"
    LEGACY_UNKNOWN = "legacy_unknown"


class EvaluationStatus(str, Enum):
    EVALUATED = "evaluated"
    PENDING = "pending"
    EXCLUDED = "excluded"


@dataclass(frozen=True, slots=True)
class OriginInference:
    origin: PredictionOrigin
    rule_version: int
    reason: str


@dataclass(frozen=True, slots=True)
class RealizedEvaluationBatch:
    """New operational results plus the observable state of each prediction."""

    results: pd.DataFrame
    evaluations: pd.DataFrame


OBSERVATION_COLUMNS = (
    "observation_schema_version",
    "observation_id",
    "prediction_id",
    "model_id",
    "model_version",
    "session_date",
    "as_of_date",
    "target",
    "set_id",
    "prediction_origin",
    "origin_inference_version",
    "prediction_status",
    "signal_category",
    "is_bullish_signal",
    "up_probability",
    "down_probability",
    "up_threshold",
    "down_threshold",
    "evaluation_status",
    "exclusion_reason",
    "open",
    "high",
    "low",
    "close",
    "intraday_return",
    "mfe",
    "mae",
    "is_winner",
    "up_target",
    "down_target",
    "prediction_created_at",
    "result_recorded_at",
    "quality_ingested_at",
)

OBSERVATION_KEY = ("model_id", "model_version", "prediction_id")

_STRING_COLUMNS = {
    "observation_id",
    "prediction_id",
    "model_id",
    "target",
    "set_id",
    "prediction_origin",
    "prediction_status",
    "signal_category",
    "evaluation_status",
    "exclusion_reason",
}
_FLOAT_COLUMNS = {
    "up_probability",
    "down_probability",
    "up_threshold",
    "down_threshold",
    "open",
    "high",
    "low",
    "close",
    "intraday_return",
    "mfe",
    "mae",
}
_INTEGER_COLUMNS = {
    "observation_schema_version",
    "origin_inference_version",
    "model_version",
    "up_target",
    "down_target",
}
_BOOLEAN_COLUMNS = {"is_bullish_signal", "is_winner"}
_DATE_COLUMNS = {"session_date", "as_of_date"}
_TIMESTAMP_COLUMNS = {
    "prediction_created_at",
    "result_recorded_at",
    "quality_ingested_at",
}


def empty_quality_observations() -> pd.DataFrame:
    """Return an empty observation frame with the canonical nullable dtypes."""

    frame = pd.DataFrame({name: pd.Series(dtype="object") for name in OBSERVATION_COLUMNS})
    return normalize_quality_observations(frame)


def normalize_quality_observations(frame: pd.DataFrame) -> pd.DataFrame:
    """Project a frame onto the stable Parquet schema without inventing values."""

    normalized = frame.copy()
    for name in OBSERVATION_COLUMNS:
        if name not in normalized:
            normalized[name] = pd.NA
    normalized = normalized.loc[:, OBSERVATION_COLUMNS]
    for name in _STRING_COLUMNS:
        normalized[name] = normalized[name].astype("string")
    for name in _FLOAT_COLUMNS:
        normalized[name] = pd.to_numeric(normalized[name], errors="coerce").astype("Float64")
    for name in _INTEGER_COLUMNS:
        normalized[name] = pd.to_numeric(normalized[name], errors="coerce").astype("Int64")
    for name in _BOOLEAN_COLUMNS:
        normalized[name] = normalized[name].astype("boolean")
    for name in _DATE_COLUMNS:
        normalized[name] = (
            pd.to_datetime(normalized[name], errors="coerce")
            .dt.normalize()
            .astype("datetime64[ns]")
        )
    for name in _TIMESTAMP_COLUMNS:
        normalized[name] = pd.to_datetime(
            normalized[name], errors="coerce", utc=True
        ).astype("datetime64[ns, UTC]")
    return normalized


def validate_quality_observations(frame: pd.DataFrame) -> None:
    """Reject a malformed ledger row before it reaches persistent storage."""

    if frame.empty:
        return
    versions = set(frame["observation_schema_version"].dropna().astype(int))
    if versions != {OBSERVATION_SCHEMA_VERSION}:
        raise ValueError("Unsupported canonical observation schema version")
    origins = set(frame["prediction_origin"].dropna().astype(str))
    allowed_origins = {item.value for item in PredictionOrigin}
    if not origins <= allowed_origins:
        raise ValueError("Unsupported prediction_origin")
    statuses = set(frame["evaluation_status"].dropna().astype(str))
    allowed_statuses = {item.value for item in EvaluationStatus}
    if not statuses <= allowed_statuses:
        raise ValueError("Unsupported evaluation_status")
    winners = frame["is_winner"].notna()
    invalid_winners = winners & ~(
        frame["evaluation_status"].eq(EvaluationStatus.EVALUATED.value)
        & frame["is_bullish_signal"].eq(True)
    )
    if invalid_winners.any():
        raise ValueError("is_winner requires an evaluated bullish signal")


def canonical_observation_id(
    model_id: object, model_version: object, prediction_id: object
) -> str:
    """Stable identifier for the non-deduplicating per-model logical key."""

    token = json.dumps(
        [str(model_id), None if pd.isna(model_version) else int(model_version), str(prediction_id)],
        separators=(",", ":"),
        ensure_ascii=False,
    )
    return hashlib.sha256(token.encode("utf-8")).hexdigest()


def canonical_set_id(target: object, predictors: object) -> str:
    """Canonical target/predictor identity, independent from display formatting."""

    values: Sequence[object]
    if isinstance(predictors, str):
        try:
            decoded = json.loads(predictors)
        except json.JSONDecodeError:
            decoded = [item.strip() for item in predictors.split(",") if item.strip()]
        values = decoded if isinstance(decoded, list) else [decoded]
    elif isinstance(predictors, Sequence):
        values = predictors
    else:
        values = ()
    return json.dumps(
        [str(target), *[str(item) for item in values]],
        separators=(",", ":"),
        ensure_ascii=False,
    )


def infer_legacy_prediction_origin(
    *,
    created_at: object,
    prediction_date: object,
    train_end: object,
    as_of_date: object,
) -> OriginInference:
    """Conservatively classify a legacy prediction with no explicit origin.

    Midnight UTC is intentionally used as a conservative lower bound for the
    target session.  A legitimate prediction made later in the preceding local
    evening may remain unknown; an ambiguous row is never promoted to live.
    """

    created = pd.to_datetime(created_at, errors="coerce", utc=True)
    target = pd.to_datetime(prediction_date, errors="coerce", utc=True)
    trained = pd.to_datetime(train_end, errors="coerce", utc=True)
    as_of = pd.to_datetime(as_of_date, errors="coerce", utc=True)
    if any(pd.isna(value) for value in (created, target, trained, as_of)):
        return OriginInference(
            PredictionOrigin.LEGACY_UNKNOWN,
            ORIGIN_INFERENCE_VERSION,
            "missing_or_invalid_temporal_evidence",
        )
    target = target.normalize()
    trained = trained.normalize()
    as_of = as_of.normalize()
    if trained >= target:
        return OriginInference(
            PredictionOrigin.OPERATIONAL_BACKFILL,
            ORIGIN_INFERENCE_VERSION,
            "artifact_training_reaches_target_session",
        )
    if created >= target + pd.Timedelta(days=1):
        return OriginInference(
            PredictionOrigin.OPERATIONAL_BACKFILL,
            ORIGIN_INFERENCE_VERSION,
            "prediction_created_after_target_day",
        )
    if created < target and trained < target and as_of < target:
        return OriginInference(
            PredictionOrigin.LEGACY_INFERRED_LIVE,
            ORIGIN_INFERENCE_VERSION,
            "created_before_target_with_strictly_prior_training_and_as_of",
        )
    return OriginInference(
        PredictionOrigin.LEGACY_UNKNOWN,
        ORIGIN_INFERENCE_VERSION,
        "temporal_evidence_is_ambiguous",
    )


def resolve_prediction_origin(
    prediction: Mapping[str, Any], *, train_end: object
) -> OriginInference:
    explicit = prediction.get("prediction_origin")
    try:
        origin = PredictionOrigin(str(explicit))
    except ValueError:
        return infer_legacy_prediction_origin(
            created_at=prediction.get("created_at"),
            prediction_date=prediction.get("prediction_date"),
            train_end=train_end,
            as_of_date=prediction.get("as_of_date"),
        )
    return OriginInference(origin, ORIGIN_INFERENCE_VERSION, "explicit_origin")


def _optional_value(row: Mapping[str, Any], name: str) -> Any:
    value = row.get(name)
    return None if value is None or (not isinstance(value, (list, dict)) and pd.isna(value)) else value


def build_quality_observations(
    predictions: pd.DataFrame,
    signals: pd.DataFrame,
    realized_results: pd.DataFrame,
    evaluations: pd.DataFrame,
    *,
    train_end_by_model: Mapping[str, object] | None = None,
    ingested_at: object | None = None,
) -> pd.DataFrame:
    """Build canonical observations from operational provenance without metrics."""

    if predictions.empty:
        return empty_quality_observations()
    train_ends = train_end_by_model or {}
    signal_lookup = {
        str(row["prediction_id"]): row
        for row in signals.to_dict("records")
        if row.get("prediction_id") is not None
    }
    result_lookup = {
        str(row["prediction_id"]): row
        for row in realized_results.to_dict("records")
        if row.get("prediction_id") is not None
    }
    evaluation_lookup = {
        str(row["prediction_id"]): row
        for row in evaluations.to_dict("records")
        if row.get("prediction_id") is not None
    }
    timestamp = ingested_at or datetime.now(timezone.utc).isoformat()
    rows: list[dict[str, Any]] = []
    for prediction in predictions.to_dict("records"):
        prediction_id = str(prediction.get("prediction_id"))
        model_id = str(prediction.get("model_id"))
        model_version = prediction.get("model_version")
        origin = resolve_prediction_origin(
            prediction, train_end=train_ends.get(model_id)
        )
        signal = signal_lookup.get(prediction_id, {})
        result = result_lookup.get(prediction_id, {})
        evaluation = evaluation_lookup.get(prediction_id, {})
        status_value = evaluation.get("evaluation_status")
        if status_value is None:
            status_value = (
                EvaluationStatus.EVALUATED.value
                if result
                else EvaluationStatus.PENDING.value
            )
        category = signal.get("category", prediction.get("signal_status"))
        bullish = str(category) == "bullish_signal"
        evaluated = str(status_value) == EvaluationStatus.EVALUATED.value
        intraday_return = _optional_value(result, "intraday_return")
        is_winner = (
            bool(float(intraday_return) > 0)
            if evaluated and bullish and intraday_return is not None
            else None
        )
        rows.append({
            "observation_schema_version": OBSERVATION_SCHEMA_VERSION,
            "observation_id": canonical_observation_id(
                model_id, model_version, prediction_id
            ),
            "prediction_id": prediction_id,
            "model_id": model_id,
            "model_version": model_version,
            "session_date": prediction.get("prediction_date"),
            "as_of_date": prediction.get("as_of_date"),
            "target": prediction.get("target"),
            "set_id": canonical_set_id(
                prediction.get("target"), prediction.get("predictors")
            ),
            "prediction_origin": origin.origin.value,
            "origin_inference_version": origin.rule_version,
            "prediction_status": prediction.get("status"),
            "signal_category": category,
            "is_bullish_signal": bullish,
            "up_probability": prediction.get("up_probability"),
            "down_probability": prediction.get("down_probability"),
            "up_threshold": prediction.get("up_threshold"),
            "down_threshold": prediction.get("down_threshold"),
            "evaluation_status": status_value,
            "exclusion_reason": evaluation.get("exclusion_reason"),
            "open": _optional_value(result, "open"),
            "high": _optional_value(result, "high"),
            "low": _optional_value(result, "low"),
            "close": _optional_value(result, "close"),
            "intraday_return": intraday_return,
            "mfe": _optional_value(result, "mfe"),
            "mae": _optional_value(result, "mae"),
            "is_winner": is_winner,
            "up_target": _optional_value(result, "up_target"),
            "down_target": _optional_value(result, "down_target"),
            "prediction_created_at": prediction.get("created_at"),
            "result_recorded_at": result.get("recorded_at"),
            "quality_ingested_at": timestamp,
        })
    return normalize_quality_observations(pd.DataFrame(rows))


def _calendar_date(value: object) -> pd.Timestamp:
    """Normalize persisted timestamps to a timezone-naive calendar date."""
    timestamp = pd.Timestamp(value)
    if timestamp.tzinfo is not None:
        timestamp = timestamp.tz_convert("UTC").tz_localize(None)
    return timestamp.normalize()


def _market_sessions(as_of_session: object, count: int, calendar_name: str) -> pd.DatetimeIndex:
    calendar = xcals.get_calendar(calendar_name)
    end = calendar.date_to_session(_calendar_date(as_of_session), direction="previous")
    if count == 1:
        return pd.DatetimeIndex([pd.Timestamp(end).tz_localize(None) if pd.Timestamp(end).tz is not None else pd.Timestamp(end)])
    values = calendar.sessions_window(end, -count)
    result = pd.DatetimeIndex(values)
    return result.tz_localize(None) if result.tz is not None else result


def eligible_live_observations(observations: pd.DataFrame) -> pd.DataFrame:
    """Live KPI population; backfills and temporally ambiguous legacy rows stay visible but excluded."""
    frame = normalize_quality_observations(observations)
    return frame[frame["prediction_origin"].astype("string").isin(LIVE_PREDICTION_ORIGINS)].copy()


def evaluated_observations(observations: pd.DataFrame) -> pd.DataFrame:
    return observations[observations["evaluation_status"].astype("string") == EvaluationStatus.EVALUATED.value].copy()


def monitoring_trades(observations: pd.DataFrame) -> pd.DataFrame:
    evaluated = evaluated_observations(observations)
    return evaluated[evaluated["is_bullish_signal"].fillna(False).astype(bool)].copy()


def _none_metrics() -> dict[str, Any]:
    return {
        "market_sessions": 0, "live_predictions": 0, "evaluated_observations": 0,
        "pending_observations": 0, "excluded_observations": 0,
        "evaluability_rate": None, "signal_count": 0, "signal_rate": None,
        "positive_trade_count": 0, "negative_trade_count": 0, "flat_trade_count": 0,
        "win_rate": None, "mean_intraday_return": None, "median_intraday_return": None,
        "cumulative_return_sum": None, "pnl": None, "mean_mfe": None,
        "median_mfe": None, "mean_mae": None, "median_mae": None,
        "best_trade_return": None, "worst_trade_return": None, "last_signal_date": None,
        "max_drawdown_dollars": None, "current_drawdown_dollars": None,
        "max_drawdown_return_points": None, "current_drawdown_return_points": None,
    }


def _drawdown(trades: pd.DataFrame) -> dict[str, float | None]:
    if trades.empty:
        return {key: None for key in (
            "max_drawdown_dollars", "current_drawdown_dollars",
            "max_drawdown_return_points", "current_drawdown_return_points",
        )}
    daily = trades.groupby("session_date", sort=True)["intraday_return"].sum().astype(float)
    cumulative_return = daily.cumsum()
    cumulative_pnl = (daily * MONITORING_NOTIONAL).cumsum()
    drawdown_return = cumulative_return - cumulative_return.cummax()
    drawdown_pnl = cumulative_pnl - cumulative_pnl.cummax()
    return {
        "max_drawdown_dollars": float(drawdown_pnl.min()),
        "current_drawdown_dollars": float(drawdown_pnl.iloc[-1]),
        "max_drawdown_return_points": float(drawdown_return.min()),
        "current_drawdown_return_points": float(drawdown_return.iloc[-1]),
    }


def _finite_summary(values: pd.Series, reducer: str) -> float | None:
    numeric = pd.to_numeric(values, errors="coerce")
    numeric = numeric[np.isfinite(numeric.to_numpy(dtype=float))]
    if numeric.empty:
        return None
    return float(numeric.mean() if reducer == "mean" else numeric.median())


def compute_quality_metrics(observations: pd.DataFrame, market_sessions: pd.DatetimeIndex) -> dict[str, Any]:
    """Metrics for calendar sessions, never a rolling number of trades."""
    metrics = _none_metrics()
    metrics["market_sessions"] = len(market_sessions)
    if not len(market_sessions):
        return metrics
    frame = eligible_live_observations(observations)
    sessions = pd.DatetimeIndex(market_sessions).normalize()
    frame = frame[frame["session_date"].isin(sessions)].copy()
    metrics["live_predictions"] = len(frame)
    evaluated = evaluated_observations(frame)
    pending = frame[frame["evaluation_status"].astype("string") == EvaluationStatus.PENDING.value]
    excluded = frame[frame["evaluation_status"].astype("string") == EvaluationStatus.EXCLUDED.value]
    metrics["evaluated_observations"] = len(evaluated)
    metrics["pending_observations"] = len(pending)
    metrics["excluded_observations"] = len(excluded)
    denominator = len(evaluated) + len(excluded)
    metrics["evaluability_rate"] = None if not denominator else len(evaluated) / denominator
    trades = monitoring_trades(frame)
    metrics["signal_count"] = len(trades)
    metrics["signal_rate"] = None if not len(frame) else len(trades) / len(frame)
    if trades.empty:
        return metrics
    returns = pd.to_numeric(trades["intraday_return"], errors="coerce")
    finite = np.isfinite(returns.to_numpy(dtype=float))
    trades = trades.loc[finite].copy()
    returns = returns.loc[finite].astype(float)
    if trades.empty:
        return metrics
    metrics.update({
        "signal_count": len(trades),
        "signal_rate": len(trades) / len(frame),
        "positive_trade_count": int((returns > 0).sum()),
        "negative_trade_count": int((returns < 0).sum()),
        "flat_trade_count": int((returns == 0).sum()),
        "win_rate": float((returns > 0).mean()),
        "mean_intraday_return": float(returns.mean()),
        "median_intraday_return": float(returns.median()),
        "cumulative_return_sum": float(returns.sum()),
        "pnl": float((returns * MONITORING_NOTIONAL).sum()),
        "mean_mfe": _finite_summary(trades["mfe"], "mean"),
        "median_mfe": _finite_summary(trades["mfe"], "median"),
        "mean_mae": _finite_summary(trades["mae"], "mean"),
        "median_mae": _finite_summary(trades["mae"], "median"),
        "best_trade_return": float(returns.max()),
        "worst_trade_return": float(returns.min()),
        "last_signal_date": str(pd.Timestamp(trades["session_date"].max()).date()),
    })
    metrics.update(_drawdown(trades))
    return metrics


def _baseline_comparison(metrics: Mapping[str, Any], baseline: Mapping[str, Any] | None) -> dict[str, Any]:
    if not baseline or baseline.get("availability_status") != "available":
        return {"version": BASELINE_COMPARISON_VERSION, "baseline_status": "unavailable",
                "delta_mean_return": None, "delta_median_return": None,
                "delta_win_rate": None, "signal_rate_ratio": None,
                "delta_mean_mfe": None, "delta_mean_mae": None}
    source = baseline.get("metrics", {}) if isinstance(baseline.get("metrics"), Mapping) else {}
    def delta(left: str, right: str) -> float | None:
        a, b = metrics.get(left), source.get(right)
        return None if a is None or b is None else float(a) - float(b)
    rate, baseline_rate = metrics.get("signal_rate"), source.get("signal_rate")
    return {
        "version": BASELINE_COMPARISON_VERSION, "baseline_status": "available",
        "delta_mean_return": delta("mean_intraday_return", "mean_intraday_return"),
        "delta_median_return": delta("median_intraday_return", "median_intraday_return"),
        "delta_win_rate": delta("win_rate", "win_rate_strict_gt_0"),
        "signal_rate_ratio": None if rate is None or baseline_rate in (None, 0) else float(rate) / float(baseline_rate),
        "delta_mean_mfe": delta("mean_mfe", "mean_mfe"),
        "delta_mean_mae": delta("mean_mae", "mean_mae"),
    }


def build_quality_series(observations: pd.DataFrame, sessions: pd.DatetimeIndex) -> pd.DataFrame:
    live = eligible_live_observations(observations)
    rows: list[dict[str, Any]] = []
    for session in sessions:
        metrics = compute_quality_metrics(live, pd.DatetimeIndex([session]))
        rows.append({
            "session_date": pd.Timestamp(session), "live_prediction_count": metrics["live_predictions"],
            "evaluated_count": metrics["evaluated_observations"], "excluded_count": metrics["excluded_observations"],
            "pending_count": metrics["pending_observations"], "signal_count": metrics["signal_count"],
            "daily_return_sum": metrics["cumulative_return_sum"], "daily_mean_return": metrics["mean_intraday_return"],
            "daily_pnl": metrics["pnl"],
        })
    series = pd.DataFrame(rows)
    if series.empty:
        return series
    returns = series["daily_return_sum"].fillna(0.0).astype(float)
    pnl = series["daily_pnl"].fillna(0.0).astype(float)
    series["cumulative_return_sum"] = returns.cumsum()
    series["cumulative_pnl"] = pnl.cumsum()
    series["running_peak_pnl"] = series["cumulative_pnl"].cummax()
    series["drawdown_pnl"] = series["cumulative_pnl"] - series["running_peak_pnl"]
    series["running_peak_return"] = series["cumulative_return_sum"].cummax()
    series["drawdown_return_points"] = series["cumulative_return_sum"] - series["running_peak_return"]
    for width in (20, 63):
        signals = series["signal_count"].rolling(width, min_periods=1).sum()
        numerator = returns.rolling(width, min_periods=1).sum()
        positive = ((series["daily_return_sum"] > 0) * series["signal_count"]).rolling(width, min_periods=1).sum()
        series[f"rolling_mean_return_{width}"] = numerator.div(signals.where(signals != 0))
        series[f"rolling_win_rate_{width}"] = positive.div(signals.where(signals != 0))
        series[f"rolling_signal_count_{width}"] = signals.astype(int)
    return series


def compute_model_quality(
    observations: pd.DataFrame,
    baseline: Mapping[str, Any] | None,
    lineage: Mapping[str, Any] | None,
    as_of_session: object,
    *,
    calendar_name: str = "XNYS",
) -> tuple[dict[str, Any], pd.DataFrame]:
    as_of = _market_sessions(as_of_session, 1, calendar_name)[-1]
    live = eligible_live_observations(observations)
    live = live[live["session_date"] <= as_of].copy()
    ingested = pd.to_datetime(live.get("quality_ingested_at"), errors="coerce", utc=True)
    generated_at = (
        ingested.max().isoformat()
        if len(ingested) and not pd.isna(ingested.max())
        else pd.Timestamp(as_of).tz_localize("UTC").isoformat()
    )
    windows = {str(width): compute_quality_metrics(live, _market_sessions(as_of, width, calendar_name)) for width in QUALITY_WINDOWS}
    promotion = (lineage or {}).get("promotion_date")
    if promotion:
        calendar = xcals.get_calendar(calendar_name)
        start = calendar.date_to_session(_calendar_date(promotion), direction="next")
    elif not live.empty:
        start = live["session_date"].min()
    else:
        start = as_of
    calendar = xcals.get_calendar(calendar_name)
    sessions = pd.DatetimeIndex(calendar.sessions_in_range(start, as_of))
    if sessions.tz is not None:
        sessions = sessions.tz_localize(None)
    since = compute_quality_metrics(live, sessions)
    series = build_quality_series(live, sessions)
    last_evaluated = evaluated_observations(live)
    snapshot = {
        # Derived publication is idempotent: this provenance timestamp changes
        # only when its canonical observation source changes.
        "quality_metrics_version": QUALITY_METRICS_VERSION, "generated_at": generated_at,
        "as_of_session": str(pd.Timestamp(as_of).date()), "model_id": (lineage or {}).get("model_id"),
        "identity": dict(lineage or {}), "baseline_status": "available" if baseline and baseline.get("availability_status") == "available" else "unavailable",
        "window_20": windows["20"], "window_63": windows["63"], "window_126": windows["126"],
        "since_promotion": since, "last_signal_date": since["last_signal_date"],
        "last_evaluated_date": None if last_evaluated.empty else str(pd.Timestamp(last_evaluated["session_date"].max()).date()),
        "excluded_observations": since["excluded_observations"], "pending_observations": since["pending_observations"],
        "evaluability_rate": since["evaluability_rate"], "baseline_comparison": _baseline_comparison(since, baseline),
        "health_status": "not_evaluated", "health_policy": None,
        "metadata": {"promotion_date_convention": "lineage_promotion_date", "notional": MONITORING_NOTIONAL,
                     "notional_policy_version": NOTIONAL_POLICY_VERSION, "drawdown_policy_version": DRAW_DOWN_POLICY_VERSION,
                     "window_policy_version": WINDOW_POLICY_VERSION},
    }
    return snapshot, series


def master_snapshot_row(snapshot: Mapping[str, Any], series: pd.DataFrame) -> dict[str, Any]:
    identity = snapshot.get("identity", {})
    since = snapshot["since_promotion"]
    row = {
        "model_id": snapshot["model_id"], "model_version": identity.get("model_version"), "target": identity.get("target"),
        "predictors": json.dumps(identity.get("predictors", [])), "status": identity.get("status"),
        "universe_id": identity.get("primary_universe_id"), "universe_name": identity.get("primary_universe_name_at_promotion"),
        "source_end_to_end_run_id": identity.get("source_end_to_end_run_id"), "predictor_prefilter_top_n": identity.get("predictor_prefilter_top_n"),
        "promotion_date": identity.get("promotion_date"), "baseline_status": snapshot["baseline_status"],
        "health_status": snapshot["health_status"], "quality_updated_at": snapshot["generated_at"],
        "pnl_since_promotion": since["pnl"], "max_drawdown_dollars": since["max_drawdown_dollars"],
        "max_drawdown_return_points": since["max_drawdown_return_points"], "last_signal_date": snapshot["last_signal_date"],
        "last_evaluated_date": snapshot["last_evaluated_date"],
    }
    for width in QUALITY_WINDOWS:
        metrics = snapshot[f"window_{width}"]
        row.update({f"signal_count_{width}": metrics["signal_count"], f"mean_return_{width}": metrics["mean_intraday_return"], f"win_rate_{width}": metrics["win_rate"]})
    row["trend_63"] = json.dumps([] if series.empty else [None if pd.isna(v) else float(v) for v in series["cumulative_pnl"].tail(32)])
    return row


class ProductionQualityMetricsService:
    """Explicit phase-4 rebuild service; intentionally not wired to daily workflow."""
    def __init__(self, repository: Any, *, calendar_name: str = "XNYS") -> None:
        self.repository = repository
        self.calendar_name = calendar_name

    def rebuild_model(self, model_id: str, as_of_session: object) -> dict[str, Any]:
        observations = self.repository.load_observations(model_id)
        baseline = self.repository.load_baseline(model_id)
        lineage = self.repository.load_lineage(model_id)
        snapshot, series = compute_model_quality(observations, baseline, lineage, as_of_session, calendar_name=self.calendar_name)
        if snapshot["model_id"] is None:
            snapshot["model_id"] = model_id
        self.repository.write_model_series(model_id, series)
        self.repository.write_model_snapshot(model_id, snapshot)
        self.repository.upsert_master_snapshot(master_snapshot_row(snapshot, series))
        self.repository.mark_model_clean(model_id)
        return snapshot
