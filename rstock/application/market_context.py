"""Versioned SPY adjusted-price context, known before each session's open.

No fitting, qualification decisions or prediction duplication. Provider vintage
is explicit: adjusted histories are retrospective data, not a PIT data archive.
Forward extensions freeze the old prefix and splice new adjusted returns.
"""

from __future__ import annotations

import itertools
import json
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

import exchange_calendars as xcals
import numpy as np
import pandas as pd

from rstock.config import RStockConfig
from rstock.data import YahooFinanceProvider
from .forward_diagnostic import (
    _bool, _clean, _json, _key, _publish, _stage_ids, digest, probability_metrics,
)


AXES = ("trend", "drawdown", "volatility")
LABELS = ("low", "intermediate", "high")
STANDARD_WINDOWS = (63, 252, 21)
CONTEXT_MANIFEST = "market_context_manifest.json"
DIAGNOSTIC_MANIFEST = "context_diagnostic_manifest.json"
METRICS_FILE = "context_metrics.csv"
ROBUSTNESS_FILE = "context_robustness.csv"


@dataclass(frozen=True)
class ContextProtocol:
    trend_sessions: int = 63
    drawdown_sessions: int = 252
    volatility_sessions: int = 21
    terciles: bool = True
    version: str = "spy_adjusted_context_v1"
    benchmark: str = "SPY"
    price_convention: str = "yahoo_adjusted_close_return_index"
    calendar: str = "XNYS"
    regime_version: str | None = None

    def __post_init__(self):
        if self.regime_version not in {None, "rstock_spy_regimes_v1"}:
            raise ValueError("context_unsupported_regime_version")
        if self.version != "spy_adjusted_context_v1":
            raise ValueError("context_unsupported_protocol_version")
        if self.benchmark != "SPY" or self.calendar != "XNYS":
            raise ValueError("context_requires_SPY_XNYS")
        for window in (self.trend_sessions, self.drawdown_sessions, self.volatility_sessions):
            if isinstance(window, bool) or not isinstance(window, int) or window < 2:
                raise ValueError("context_window_must_be_integer_ge_2")

    @classmethod
    def from_config(cls, config: RStockConfig):
        return cls(config.market_context_trend_sessions, config.market_context_drawdown_sessions,
                   config.market_context_volatility_sessions, config.market_context_terciles,
                   version=config.market_context_protocol_version, regime_version=config.market_context_regime_version)

    @property
    def identifier(self):
        import hashlib
        values = asdict(self)
        if self.regime_version is None:
            values.pop("regime_version")  # Preserve immutable V1 protocol identifiers.
        return hashlib.sha256(json.dumps(values, sort_keys=True).encode()).hexdigest()[:20]

    @property
    def standard(self):
        return (self.trend_sessions, self.drawdown_sessions, self.volatility_sessions) == STANDARD_WINDOWS and self.terciles


def adjusted_snapshot(prices: pd.DataFrame) -> pd.DataFrame:
    """Require the explicit adjusted field; never fall back to raw Close."""
    column = "Adjusted" if "Adjusted" in prices else "SPY.Adjusted"
    if column not in prices:
        raise ValueError("context_adjusted_close_unavailable")
    dates = pd.DatetimeIndex(pd.to_datetime(prices.index)).tz_localize(None).normalize()
    if dates.has_duplicates or dates.hasnans:
        raise ValueError("context_duplicate_or_invalid_dates")
    values = pd.Series(pd.to_numeric(prices[column], errors="coerce").to_numpy(), index=dates).sort_index()
    if values.empty or not np.isfinite(values).all() or not values.gt(0).all():
        raise ValueError("context_invalid_adjusted_close")
    return pd.DataFrame({"adjusted_close": values})


def build_context(snapshot: pd.DataFrame, sessions: pd.DatetimeIndex, protocol: ContextProtocol,
                  reference_start: Any, reference_end: Any,
                  boundaries: dict[str, list[float] | None] | None = None):
    """Calculate on complete exchange-session windows, then shift by one session."""
    calendar = xcals.get_calendar(protocol.calendar)
    dates = pd.DatetimeIndex(sessions).tz_localize(None).normalize()
    if dates.empty or dates.has_duplicates or not dates.is_monotonic_increasing:
        raise ValueError("context_invalid_sessions")
    first = pd.Timestamp(snapshot.index.min())
    complete = pd.DatetimeIndex(calendar.sessions_in_range(first, dates.max())).tz_localize(None)
    values = snapshot["adjusted_close"].reindex(complete)
    log_returns = np.log(values / values.shift(1))
    historical = pd.DataFrame({
        "trend": values / values.shift(protocol.trend_sessions) - 1,
        "drawdown": 1 - values / values.rolling(protocol.drawdown_sessions, min_periods=protocol.drawdown_sessions).max(),
        "volatility": log_returns.rolling(protocol.volatility_sessions, min_periods=protocol.volatility_sessions).std(ddof=1) * np.sqrt(252),
    }, index=complete)
    # Do not bridge a missing session when calculating trend.
    historical.loc[values.rolling(protocol.trend_sessions + 1).count() != protocol.trend_sessions + 1, "trend"] = np.nan
    if protocol.regime_version:
        from .market_regimes import classify_regimes
        historical = historical.join(classify_regimes(values))
    shifted = historical.shift(1)
    shifted["context_as_of_date"] = pd.Series(complete, index=complete).shift(1)
    if boundaries is None:
        prefix = shifted.loc[(shifted.index >= pd.Timestamp(reference_start)) & (shifted.index < pd.Timestamp(reference_end))]
        boundaries = {}
        for axis in AXES:
            usable = prefix[axis].dropna()
            cuts = usable.quantile([1/3, 2/3]).tolist()
            boundaries[axis] = cuts if protocol.terciles and len(usable) >= 60 and len(cuts) == 2 and cuts[0] < cuts[1] else None
    context = shifted.reindex(dates).copy()
    if protocol.regime_version:
        context["regime"] = context.regime.fillna("Indisponible")
        context["regime_availability"] = context.regime_availability.fillna("unavailable_history_or_gap")
    context.insert(0, "session_date", dates.strftime("%Y-%m-%d"))
    context["context_as_of_date"] = pd.to_datetime(context["context_as_of_date"]).dt.strftime("%Y-%m-%d")
    for axis in AXES:
        context[f"{axis}_availability"] = np.where(context[axis].notna(), "available", "unavailable_history_or_gap")
        cuts = boundaries.get(axis)
        context[f"{axis}_band"] = pd.cut(context[axis], [-np.inf, *cuts, np.inf], labels=LABELS).astype(object) if cuts else None
    return context.reset_index(drop=True), boundaries


EXTENSION_COMPARISON_PROTOCOL = "spy_extension_float32_precision_v1"
# Yahoo adjusted prices are quantized to float32. Four ULPs per observed price
# accommodate the arithmetic of adjustment factors, independently of outcomes.
EXTENSION_PRICE_ULPS = 4


def extension_comparison(frozen: pd.DataFrame, incoming: pd.DataFrame) -> pd.DataFrame:
    last = frozen.index.max()
    overlap = frozen.index.intersection(incoming.index)
    if last not in overlap or len(overlap) < 2:
        raise ValueError("context_extension_missing_overlap")
    expected = frozen.index[(frozen.index >= incoming.index.min()) & (frozen.index <= last)]
    old = frozen.reindex(expected).adjusted_close
    new = incoming.reindex(expected).adjusted_close
    ratio = old / new
    factor = float(ratio.median())
    def uncertainty(prices):
        values = prices.to_numpy(dtype=float)
        with np.errstate(invalid="ignore", over="ignore"):
            step = np.abs(np.spacing(values.astype(np.float32))).astype(float)
        return pd.Series(EXTENSION_PRICE_ULPS * step / prices.to_numpy(), index=prices.index)
    relative = uncertainty(old) + uncertainty(new)
    old_return = old.pct_change(fill_method=None)
    new_return = new.pct_change(fill_method=None)
    return_tolerance = (1 + old_return.abs()) * (relative + relative.shift(1)) + 1e-12
    scale_residual = (ratio / factor - 1).abs()
    # Both dates and levels must match a SINGLE factor, not just adjacent returns.
    scale_tolerance = relative + float(relative.max()) + 1e-12
    return_ok = (old_return - new_return).abs().le(return_tolerance)
    return_ok.iloc[0] = True
    report = pd.DataFrame({"Date": expected, "frozen_adjusted_close": old.to_numpy(),
        "incoming_adjusted_close": new.to_numpy(), "scale_ratio": ratio.to_numpy(),
        "frozen_return": old_return.to_numpy(), "incoming_return": new_return.to_numpy(),
        "return_delta": (new_return-old_return).to_numpy(), "return_tolerance": return_tolerance.to_numpy(),
        "scale_residual": scale_residual.to_numpy(), "scale_tolerance": scale_tolerance.to_numpy(),
        "consistent": (new.notna() & scale_residual.le(scale_tolerance) & return_ok).to_numpy()})
    consistent = bool(report.consistent.all())
    exact = old.equals(new)
    report.attrs = {"protocol": EXTENSION_COMPARISON_PROTOCOL, "price_ulps": EXTENSION_PRICE_ULPS,
        "scale_factor": factor, "overlap_start": str(expected.min().date()),
        "overlap_end": str(expected.max().date()), "compared_sessions": len(expected),
        "inconsistent_sessions": int((~report.consistent).sum()),
        "classification": "material_revision" if not consistent else "identical" if exact else
            "precision_compatible" if abs(factor-1) <= float(scale_tolerance.max()) else "uniform_scale_with_precision",
        "max_absolute_return_delta": float(report.return_delta.abs().max()),
        "max_scale_residual": float(scale_residual.max())}
    return report


def extend_adjusted_snapshot(frozen: pd.DataFrame, incoming: pd.DataFrame) -> pd.DataFrame:
    """Preserve the prefix; only append a checked, anchored tail."""
    report = extension_comparison(frozen, incoming)
    if not report.consistent.all():
        raise ValueError("context_extension_historical_returns_changed")
    last = frozen.index.max()
    additions = incoming.loc[incoming.index > last].copy()
    if not additions.empty:
        additions["adjusted_close"] *= frozen.at[last, "adjusted_close"] / incoming.at[last, "adjusted_close"]
    result = pd.concat([frozen, additions])
    if not result.iloc[:len(frozen)].equals(frozen):
        raise ValueError("context_extension_prefix_changed")
    return result


def publish_context(store: Path, snapshot: pd.DataFrame, sessions: pd.DatetimeIndex,
                    protocol: ContextProtocol, reference_start: str, reference_end: str,
                    *, parent_manifest: Path | None = None, provider: str = "Yahoo Finance",
                    acquisition_kind: str = "explicit_provider_acquisition",
                    acquisition: dict[str, Any] | None = None) -> Path:
    """Immutable revisions; a final manifest is the only commit marker."""
    import hashlib
    prior = None
    frozen_context = None
    boundaries = None
    if parent_manifest:
        prior = _json(parent_manifest)
        validate_context(parent_manifest)
        if prior["protocol_id"] != protocol.identifier:
            raise ValueError("context_extension_protocol_changed")
        frozen = pd.read_csv(parent_manifest.parent / "spy_adjusted_snapshot.csv", index_col="Date", parse_dates=True, float_precision="round_trip")
        snapshot = extend_adjusted_snapshot(frozen, snapshot)
        boundaries = prior["boundaries"]
        frozen_context = pd.read_csv(parent_manifest.parent / "market_context.csv", float_precision="round_trip")
        reference_start, reference_end = prior["reference_start"], prior["reference_end_exclusive"]
    context, boundaries = build_context(snapshot, sessions, protocol, reference_start, reference_end, boundaries)
    if frozen_context is not None:
        common = context.session_date.isin(frozen_context.session_date)
        old = frozen_context.set_index("session_date").reindex(context.loc[common, "session_date"])
        for axis in AXES:
            if not np.allclose(context.loc[common, axis], old[axis], equal_nan=True, rtol=1e-10, atol=1e-12):
                raise ValueError("context_extension_prefix_changed")
        if protocol.regime_version:
            for column in frozen_context.columns.difference(["session_date", *AXES]):
                actual = context.loc[common, column].reset_index(drop=True)
                expected = old[column].reset_index(drop=True)
                equal = actual.eq(expected) | (actual.isna() & expected.isna())
                if not equal.all(): raise ValueError("context_extension_episode_prefix_changed")
        # Preserve exact serialized values, including availability and labels.
        context.loc[common, frozen_context.columns] = old.reset_index().to_numpy()
    identity = json.dumps({"protocol": asdict(protocol), "reference_start": reference_start,
                           "reference_end": reference_end, "sessions": context.session_date.tolist(),
                           "snapshot": snapshot.to_csv(index=True)}, sort_keys=True)
    revision = hashlib.sha256(identity.encode()).hexdigest()[:24]
    directory = store / protocol.identifier / "revisions" / revision
    manifest_path = directory / CONTEXT_MANIFEST
    if manifest_path.exists():
        validate_context(manifest_path)
        return manifest_path
    # Per-store mutex protects concurrent publication and crash recovery.
    from .runner import _try_submission_mutex
    with _try_submission_mutex(store / ".context.lock") as acquired:
        if not acquired:
            raise ValueError("context_already_building")
        if manifest_path.exists():
            validate_context(manifest_path)
            return manifest_path
        payload = snapshot.copy()
        payload.index.name = "Date"
        _publish(directory / "spy_adjusted_snapshot.csv", payload.reset_index())
        _publish(directory / "market_context.csv", context)
        extra_artifacts = {}
        regime_rules = None
        if protocol.regime_version:
            from .market_regimes import episode_table, RULES
            regime_rules = RULES
            _publish(directory / "market_episodes.csv", episode_table(context))
            extra_artifacts["market_episodes.csv"] = digest(directory / "market_episodes.csv")
        _publish(manifest_path, {
            "schema_version": 1, "protocol": asdict(protocol), "protocol_id": protocol.identifier,
            "standard_protocol": protocol.standard, "revision": revision, "provider": provider,
            "acquired_at_utc": datetime.now(timezone.utc).isoformat(), "acquisition_kind": acquisition_kind,
            "acquisition": acquisition,
            "historical_vintage": "retrospective_adjusted_history_not_point_in_time_archive",
            "reference_start": reference_start, "reference_end_exclusive": reference_end,
            "reference_context_sha256": prior.get("reference_context_sha256") if prior and prior.get("reference_context_sha256") else hashlib.sha256(context.loc[context.session_date.ge(reference_start) & context.session_date.lt(reference_end), [c for c in ("session_date", *AXES, "regime", "episode_id", "reference_peak", "episode_max_drawdown") if c in context]].to_csv(index=False).encode()).hexdigest(),
            "regime_rules": regime_rules,
            "boundaries": boundaries, "band_semantics": "descriptive_terciles_not_economic_regimes",
            "parent_revision": prior["revision"] if prior else None,
            "first_session": context.session_date.min(), "last_session": context.session_date.max(),
            "artifact_digests": {name: digest(directory / name) for name in ("spy_adjusted_snapshot.csv", "market_context.csv")} | extra_artifacts,
        })
    return manifest_path


def validate_context(path: Path) -> dict[str, Any]:
    manifest = _json(path)
    if not {"spy_adjusted_snapshot.csv", "market_context.csv"}.issubset(manifest.get("artifact_digests", {})):
        raise ValueError("context_manifest_incomplete")
    for name, expected in manifest["artifact_digests"].items():
        if not (path.parent / name).is_file() or digest(path.parent / name) != expected:
            raise ValueError("context_artifact_changed")
    if ContextProtocol(**manifest["protocol"]).identifier != manifest["protocol_id"]:
        raise ValueError("context_protocol_mismatch")
    return manifest


def normalize_predictions(frame: pd.DataFrame, *, signal_rule: str,
                          thresholds: dict[str, Any] | None = None) -> pd.DataFrame:
    """One normalized row per existing prediction; no prediction file is written."""
    if "source_model_id" in frame:
        if frame.duplicated(["source_model_id", "session_date"]).any():
            raise ValueError("context_ambiguous_forward_predictions")
        rows = frame.copy()
        rows["model_key"] = rows["source_model_id"].astype(str)
        rows["Date"] = rows["session_date"]
        rows["Window"] = 0
        return rows
    if "UpProbability" in frame:
        if frame.duplicated(["Set", "Date", "Window"] if "Window" in frame else ["Set", "Date"]).any():
            raise ValueError("context_ambiguous_predictions")
        rows = frame.copy()
        rows["prediction_probability"] = rows["UpProbability"]
        rows["outcome"] = rows["UpTarget"]
        rows["signal"] = rows["UpPrediction"]
    else:
        if frame.duplicated(["Set", "Date", "Window", "Direction"]).any():
            raise ValueError("context_ambiguous_predictions")
        up = frame.loc[frame.Direction.eq("Up")].copy()
        down = frame.loc[frame.Direction.eq("Down"), ["Set", "Date", "Window", "Probability"]].rename(columns={"Probability": "DownProbability"})
        rows = up.merge(down, on=["Set", "Date", "Window"], how="left", validate="one_to_one")
        rows["prediction_probability"] = rows["Probability"]
        rows["outcome"] = rows["Target"]
        rows["signal"] = rows.get("Prediction", pd.Series(False, index=rows.index))
    rows["directional_return"] = rows.get("IntradayReturn", np.nan)
    rows["model_key"] = rows.Set.map(lambda value: _key(value, "Up"))
    if "Window" not in rows:
        rows["Window"] = 0
    if signal_rule == "frozen_combined_up_down":
        selections = thresholds or {}
        def select(row):
            choices = selections.get(row["Set"], {})
            up, down = choices.get("Up", {}).get("threshold"), choices.get("Down", {}).get("threshold")
            return (row["prediction_probability"] >= up and row["DownProbability"] < down) if up is not None and down is not None and pd.notna(row.get("DownProbability")) else None
        rows["signal"] = rows.apply(select, axis=1)
    return rows


def aggregate_context(rows: pd.DataFrame, context: pd.DataFrame, *, stage: str,
                      signal_rule: str, horizons: dict[int, tuple[str, str]] | None = None):
    rows = rows.copy()
    rows["session_date"] = pd.to_datetime(rows["Date"]).dt.strftime("%Y-%m-%d")
    if context.session_date.duplicated().any():
        raise ValueError("context_duplicate_session")
    joined = rows.merge(context, on="session_date", how="left", validate="many_to_one", suffixes=("", "_context"))
    for axis in AXES:
        ordered = context.sort_values("session_date")
        episodes = ordered[f"{axis}_band"].fillna("unavailable")
        episode_ids = episodes.ne(episodes.shift()).cumsum()
        joined[f"{axis}_episode"] = joined.session_date.map(dict(zip(ordered.session_date, episode_ids)))
    periods = [("full_run", 0, joined)]
    if horizons:
        previous_end = None
        for horizon, (start, end) in sorted(horizons.items()):
            periods.append(("cumulative", horizon, joined.loc[joined.session_date.between(start, end)]))
            periods.append(("interval", horizon, joined.loc[joined.session_date.between(start if previous_end is None else previous_end, end) & (True if previous_end is None else joined.session_date.gt(previous_end))]))
            previous_end = end
    axes = (*AXES, "regime", "episode") if "regime" in context else AXES
    if "regime" in context:
        joined["regime_band"] = joined.regime.fillna("Indisponible")
        joined["regime_episode"] = joined.get("episode_id", pd.Series(index=joined.index, dtype=object))
        joined["episode_band"] = joined.get("episode_id", pd.Series(index=joined.index, dtype=object)).fillna("Hors épisode")
        joined["episode_episode"] = joined["episode_band"]
    records = []
    for kind, horizon, observations in periods:
        for (model, window), model_rows in observations.groupby(["model_key", "Window"], dropna=False):
            for axis in axes:
                bands = model_rows[f"{axis}_band"].fillna("unavailable")
                for band, group in model_rows.groupby(bands, dropna=False):
                    valid = group.loc[group.signal.notna()]
                    probability = probability_metrics(group.assign(signal=group.signal.fillna(False)))
                    if len(valid) != len(group):
                        for name in ("precision", "recall", "f1", "signal_rate", "signals", "signal_mean_probability"):
                            probability[name] = None
                    signals = valid.loc[_bool(valid.signal)]
                    returns = pd.to_numeric(signals.directional_return, errors="coerce")
                    n, positives, negatives = len(valid), probability.get("positives", 0), probability.get("negatives", 0)
                    calendar = context.loc[context.session_date.between(model_rows.session_date.min(), model_rows.session_date.max())]
                    calendar_bands = (calendar[f"{axis}_band"] if axis in AXES else calendar["regime"] if axis == "regime" else calendar.get("episode_id", pd.Series(index=calendar.index, dtype=object)).fillna("Hors épisode"))
                    market_share = float(calendar_bands.eq(band).mean()) if len(calendar) else None
                    model_signals = _bool(model_rows.signal.fillna(False)).sum()
                    signal_share = len(signals) / model_signals if model_signals and len(valid) == len(group) else None
                    known_band = band not in {"unavailable", "Indisponible"}
                    records.append({"model_key": model, "stage": stage, "signal_rule": signal_rule,
                        "Window": window, "period_kind": kind, "horizon": horizon, "axis": axis, "band": band,
                        "observations": len(group), "evaluated_observations": n, "missing_signal_rule": len(group)-n,
                        "unique_dates": group.session_date.nunique(), "total_model_observations": len(model_rows),
                        "episodes": group[f"{axis}_episode"].nunique(),
                        "market_sessions": int(calendar_bands.eq(band).sum()), "market_exposure_share": market_share,
                        "observation_share": len(group) / len(model_rows), "signal_share": signal_share,
                        "exposure_adjusted_signal_concentration": signal_share / market_share if signal_share is not None and market_share else None,
                        "context_coverage": float(model_rows[axis].notna().mean()) if axis in AXES else float(model_rows.regime.fillna("Indisponible").ne("Indisponible").mean()),
                        **{k:v for k,v in probability.items() if k != "observations"},
                        "mean_return": float(returns.mean()) if len(returns) and np.isfinite(returns).all() else None,
                        "auc_supported": known_band and probability.get("availability") == "available" and len(group) >= 30 and positives >= 10 and negatives >= 10,
                        "signal_supported": known_band and n == len(group) and len(signals) >= 10,
                        "signal_count": len(signals) if n == len(group) else None,
                        "signal_return_sum": float(returns.sum()) if len(returns) and np.isfinite(returns).all() else None})
    metrics = pd.DataFrame(_clean(records))
    summaries = []
    if metrics.empty:
        return metrics, pd.DataFrame()
    for keys, group in metrics.groupby(["model_key", "stage", "period_kind", "horizon", "axis"]):
        if keys[-1] == "episode":
            continue
        known = group.loc[group.band.isin(LABELS)] if keys[-1] in AXES else group.loc[group.band.ne("Indisponible")]
        supported = known.loc[known.auc_supported]
        medians = supported.groupby("band").auc.median() if "auc" in supported else pd.Series(dtype=float)
        counts = supported.groupby("band").Window.nunique()
        minimum_windows = 2 if stage in {"walk_forward", "development_calibrated"} else 1
        required_bands = 3 if keys[-1] in AXES else known.band.nunique()
        robust = required_bands >= 2 and len(medians) == required_bands and counts.ge(minimum_windows).all()
        signal_counts = known.groupby("band").signal_count.sum()
        dominant = signal_counts.idxmax() if len(signal_counts) and signal_counts.sum() else None
        outside = known.loc[known.band.ne(dominant)]
        total = known.signal_count.sum()
        signals_available = not group.missing_signal_rule.gt(0).any()
        outside_returns = outside.signal_return_sum.sum() if "signal_return_sum" in outside else np.nan
        returns_available = not outside.loc[outside.signal_count.gt(0), "signal_return_sum"].isna().any()
        outside_signals = outside.signal_count.sum()
        summaries.append(dict(zip(["model_key", "stage", "period_kind", "horizon", "axis"], keys),
            robustness_status="available" if robust else "unavailable_insufficient_context_support",
            worst_context_auc=float(medians.min()) if robust else None,
            auc_dispersion=float(medians.std(ddof=0)) if robust else None,
            supported_bands=len(medians), observed_bands=known.band.nunique(),
            context_coverage=group.context_coverage.min(),
            dominant_signal_band=dominant if signals_available else None, dominant_signal_share=float(signal_counts.max()/total) if total and signals_available else None,
            mean_return_outside_dominant=float(outside_returns/outside_signals) if outside_signals and returns_available and signals_available else None,
            interpretation="descriptive_not_qualification; windows and episodes are not independent"))
    return metrics, pd.DataFrame(_clean(summaries))


def descriptive_variants(snapshot: pd.DataFrame, sessions: pd.DatetimeIndex,
                         episodes: dict[str, tuple[str, str]]) -> pd.DataFrame:
    """Only benchmark data; deliberately accepts no model-performance input."""
    result = []
    for trend, drawdown, volatility in itertools.product((42, 63, 84), (126, 252), (21, 42)):
        protocol = ContextProtocol(trend, drawdown, volatility, False)
        context, _ = build_context(snapshot, sessions, protocol, sessions[0], sessions[0])
        for episode, (start, end) in episodes.items():
            part = context.loc[context.session_date.between(start, end)]
            if part.empty:
                continue
            result.append({"episode": episode, "trend_sessions": trend, "drawdown_sessions": drawdown,
                           "volatility_sessions": volatility, "sessions": len(part),
                           "coverage": part[list(AXES)].notna().all(axis=1).mean(),
                           "median_trend": part.trend.median(), "maximum_drawdown": part.drawdown.max(),
                           "median_volatility": part.volatility.median(), "maximum_volatility": part.volatility.max()})
    return pd.DataFrame(result)
