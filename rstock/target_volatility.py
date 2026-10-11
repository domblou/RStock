"""One versioned, strictly pre-session volatility contract for every consumer."""
from __future__ import annotations

import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path

import exchange_calendars as xcals
import numpy as np
import pandas as pd

VERSION = "target_adjusted_log_volatility_v1"


def realized_volatility(adjusted, sessions):
    returns = np.log(adjusted / adjusted.shift(1))
    return returns.rolling(sessions, min_periods=sessions).std(ddof=1).mul(np.sqrt(252))


def enabled(config):
    return config.target_volatility_enabled and config.predictive_model_type != "constant_probability"


def column(symbol, sessions):
    return f"{symbol}_target_volatility_{sessions}"


def contract(config):
    return {"enabled": enabled(config), "sessions": config.target_volatility_sessions,
            "definition_version": VERSION, "return": "adjusted_close_log_return",
            "ddof": 1, "annualization": 252, "availability": "previous_market_session_close",
            "missing_policy": "complete_expected_sessions_no_imputation"}


def prices_payload(prices):
    clean = prices.replace([np.inf, -np.inf], np.nan)
    return {"dates": [pd.Timestamp(d).isoformat() for d in clean.index], "columns": list(clean.columns),
            "values": clean.astype(object).where(clean.notna(), None).values.tolist()}


def prices_frame(payload):
    if isinstance(payload, pd.DataFrame):
        return payload
    if not isinstance(payload, dict):
        raise ValueError("Volatility prices unavailable")
    return pd.DataFrame(payload["values"], columns=payload["columns"], index=pd.DatetimeIndex(payload["dates"]).as_unit("ns"))


def historical_volatility(adjusted, dates, sessions=21, calendar="XNYS"):
    """N valid log returns require N+1 consecutive closes strictly before J."""
    if isinstance(sessions, bool) or not isinstance(sessions, int) or sessions < 2:
        raise ValueError("Volatility sessions must be an integer >= 2")
    values = pd.to_numeric(adjusted, errors="coerce").copy()
    values.index = pd.DatetimeIndex(values.index).tz_localize(None).normalize()
    dates = pd.DatetimeIndex(dates).tz_localize(None).normalize()
    if values.index.has_duplicates or values.index.hasnans or dates.has_duplicates:
        raise ValueError("Invalid volatility source calendar")
    if values.empty or dates.empty:
        return pd.Series(np.nan, index=dates, dtype=float)
    complete = pd.DatetimeIndex(xcals.get_calendar(calendar).sessions_in_range(
        min(values.index.min(), dates.min()), max(values.index.max(), dates.max()))).tz_localize(None)
    values = values.reindex(complete).where(lambda x: np.isfinite(x) & x.gt(0))
    return realized_volatility(values, sessions).shift(1).reindex(dates)


def operational_features(prepared, model, prices=None):
    from dataclasses import replace
    from rstock.config import DEFAULT_CONFIG
    source = model.source_configuration.get("rstock_config", {})
    config = replace(DEFAULT_CONFIG, target_volatility_enabled=source.get("target_volatility_enabled", False),
                     target_volatility_sessions=source.get("target_volatility_sessions", 21))
    if not enabled(config):
        result = prepared.copy()
        result.attrs.pop("target_volatility", None)
        result.attrs.pop("target_volatility_prices", None)
        return result
    if prices is None:
        prices = prepared.attrs.get("target_volatility_prices")
    if prices is None:
        raise ValueError("Operational target volatility prices unavailable")
    prices = prices_frame(prices)
    return attach(prepared, prices, [model.target], config, model.training_metadata.get("calendar", "XNYS"),
                  {"provider": "operational_input", "source_quality": "retrospective_not_pit"})


def attach(prepared, prices, targets, config, calendar="XNYS", source=None):
    """Add only volatility; preserve the parent's labels, lags and date boundaries."""
    if not enabled(config):
        return prepared
    result = prepared.copy()
    closes = {}
    coverage = {}
    for symbol in targets:
        name = f"{symbol}.Adjusted"
        if name not in prices:
            raise ValueError(f"Adjusted target closes unavailable: {symbol}")
        closes[name] = prices[name]
        values = historical_volatility(prices[name], result.index, config.target_volatility_sessions, calendar)
        result[column(symbol, config.target_volatility_sessions)] = values
        coverage[symbol] = {"valid": int(values.notna().sum()), "excluded": int(values.isna().sum())}
    result.attrs["target_volatility"] = {**contract(config), "source": source,
        "source_quality": "retrospective_not_pit", "calendar": calendar, "coverage": coverage}
    result.attrs["target_volatility_prices"] = prices_payload(pd.DataFrame(closes))
    return result


def future_values(prepared, as_of, target):
    metadata = prepared.attrs.get("target_volatility", {})
    if not metadata.get("enabled"):
        return {}
    if metadata.get("definition_version") != VERSION:
        raise ValueError("Unsupported target volatility definition")
    prices = prepared.attrs.get("target_volatility_prices")
    if prices is None:
        raise ValueError("Frozen volatility source unavailable for prediction")
    prices = prices_frame(prices)
    prices = prices.loc[prices.index <= as_of]
    return {column(name.removesuffix(".Adjusted"), metadata["sessions"]): float(
        historical_volatility(prices[name], pd.DatetimeIndex([target]), metadata["sessions"], metadata["calendar"]).iloc[0])
        for name in prices}


def freeze_prices(project, prices, calendar):
    """Content-addressed immutable input, outside historical runs. No cache refresh."""
    from rstock.checkpoints import _atomic_json
    payload = {"schema_version": 1, "definition_version": VERSION, "calendar": calendar,
        "source_quality": "retrospective_not_pit", "provider": "local_market_cache",
        **prices_payload(prices)}
    raw = json.dumps(payload, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
    digest = hashlib.sha256(raw).hexdigest()
    relative = Path("data/metadata/target_volatility_sources") / f"{digest}.json"
    path = Path(project) / relative
    if path.exists():
        if json.dumps(json.loads(path.read_text(encoding="utf-8")), sort_keys=True, separators=(",", ":"), allow_nan=False).encode() != raw:
            raise ValueError("Frozen volatility source changed")
    else:
        path.parent.mkdir(parents=True, exist_ok=True)
        _atomic_json(path, payload)
    return {"path": relative.as_posix(), "sha256": digest, "definition_version": VERSION,
            "frozen_at": datetime.now(timezone.utc).isoformat(),
            "acquisition_vintage": "local_cache_vintages_not_archived",
            "source_quality": "retrospective_not_pit"}


def load_prices(project, source):
    if source.get("definition_version") != VERSION:
        raise ValueError("Incompatible volatility source definition")
    root = Path(project).resolve()
    path = (root / source["path"]).resolve()
    if not path.is_relative_to(root / "data/metadata/target_volatility_sources"):
        raise ValueError("Invalid volatility source path")
    payload = json.loads(path.read_text(encoding="utf-8"))
    raw = json.dumps(payload, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
    if hashlib.sha256(raw).hexdigest() != source["sha256"]:
        raise ValueError("Frozen volatility source digest differs")
    return prices_frame(payload)


def freeze_cached_source(project, targets, calendar, cutoff, sessions, first, config=None):
    """Pin compatible cached closes at experiment creation, never during resume."""
    frames = []
    exchange = xcals.get_calendar(calendar)
    prior = pd.DatetimeIndex(exchange.sessions_in_range(pd.Timestamp(first) - pd.Timedelta(days=3 * sessions + 100), first))
    start = prior[prior < pd.Timestamp(first)][-(sessions + 1)]
    for symbol in targets:
        from rstock.config import RStockConfig
        from rstock.market_cache import ParquetMarketDataStore
        effective = config or RStockConfig(project_root=Path(project))
        frame = ParquetMarketDataStore(effective.market_data_path, effective.market_cache_metadata_path).read(symbol)
        if frame is None:
            raise ValueError(f"Adjusted target cache unavailable: {symbol}")
        if "Adjusted" not in frame:
            raise ValueError(f"Adjusted target cache unavailable: {symbol}")
        values = frame.loc[(frame.index >= start) & (frame.index <= pd.Timestamp(cutoff)), "Adjusted"]
        frames.append(values.rename(f"{symbol}.Adjusted"))
    return freeze_prices(project, pd.concat(frames, axis=1).sort_index(), calendar)


def derivation_source(prepared, targets, config, calendar, cutoff, current=None):
    """Reuse compatible immutable closes before consulting the existing cache."""
    if not enabled(config):
        return current
    candidate = current or prepared.attrs.get("target_volatility", {}).get("source")
    if candidate and candidate.get("sha256"):
        prices = load_prices(config.project_root, candidate)
        first = prepared.index.min()
        exchange = xcals.get_calendar(calendar)
        prior = pd.DatetimeIndex(exchange.sessions_in_range(
            first - pd.Timedelta(days=3 * config.target_volatility_sessions + 100), first))
        required = prior[prior < first][-(config.target_volatility_sessions + 1)]
        last_session = pd.Timestamp(exchange.date_to_session(pd.Timestamp(cutoff), direction="previous")).tz_localize(None)
        if (prices.index.min() <= required and prices.index.max() >= last_session
                and all(f"{t}.Adjusted" in prices for t in targets)):
            return candidate
    return freeze_cached_source(config.project_root, targets, calendar, cutoff,
                                config.target_volatility_sessions, prepared.index.min(), config)
