"""Immutable audit trail for dedicated SPY acquisitions, including rejected data."""
from datetime import datetime, timezone
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from uuid import uuid4

from .forward_diagnostic import _json, _publish, digest
from .market_context import adjusted_snapshot, extension_comparison


class ContextAcquisitionError(ValueError):
    def __init__(self, reason, evidence):
        super().__init__(reason)
        self.evidence = evidence


def _version(package):
    try:
        return version(package)
    except PackageNotFoundError:
        return None


def acquisition_evidence(path, runs):
    return {"manifest": str(path.relative_to(runs)), "sha256": digest(path)}


def acquire_spy(provider, start, end_exclusive, archive, runs, *, parent=None):
    """Archive before interpretation; never consult or mutate the market cache."""
    directory = Path(archive) / uuid4().hex
    path = directory / "acquisition_manifest.json"
    parent_meta = _json(parent) if parent else {}
    request = {"schema_version": 1, "status": "requested", "symbol": "SPY",
        "provider": provider.source_name, "price_convention": "adjusted_close_return_index",
        "download_options": {"auto_adjust": False, "actions": False, "threads": False} if type(provider).__name__ == "YahooFinanceProvider" else None, "provider_class": type(provider).__module__ + "." + type(provider).__name__,
        "requested_start": str(start), "requested_end_exclusive": str(end_exclusive),
        "requested_at_utc": datetime.now(timezone.utc).isoformat(),
        "versions": {name: _version(name) for name in ("yfinance", "pandas", "numpy")},
        "parent_revision": parent_meta.get("revision"),
        "parent_manifest": str(parent.relative_to(runs)) if parent else None,
        "parent_manifest_sha256": digest(parent) if parent else None,
        "artifact_digests": {}}
    _publish(path, request)
    artifacts = {}
    comparison = None
    try:
        raw = provider.fetch("SPY", start, end_exclusive)
        _publish(directory / "response.csv", raw.rename_axis("Date").reset_index())
        artifacts["response.csv"] = digest(directory / "response.csv")
        # Save reception metadata even if normalization subsequently rejects it.
        _publish(path, {**_json(path), "status": "received", "received_at_utc": datetime.now(timezone.utc).isoformat(),
            "rows": len(raw), "artifact_digests": artifacts})
        snapshot = adjusted_snapshot(raw)
        received = {"received_start": str(snapshot.index.min().date()),
            "received_end": str(snapshot.index.max().date()), "rows": len(snapshot)}
        _publish(path, {**_json(path), **received})
        if parent:
            import pandas as pd
            frozen = pd.read_csv(parent.parent / "spy_adjusted_snapshot.csv", index_col="Date",
                                 parse_dates=True, float_precision="round_trip")
            report = extension_comparison(frozen, snapshot)
            comparison = report.attrs
            _publish(directory / "overlap_comparison.csv", report)
            artifacts["overlap_comparison.csv"] = digest(directory / "overlap_comparison.csv")
            if not report.consistent.all():
                raise ValueError("context_extension_historical_returns_changed")
        _publish(path, {**_json(path), "status": "validated", "comparison": comparison,
            "artifact_digests": artifacts, "validated_at_utc": datetime.now(timezone.utc).isoformat()})
        return snapshot, acquisition_evidence(path, runs)
    except Exception as exc:
        _publish(path, {**_json(path), "status": "rejected", "reason": str(exc),
            "comparison": comparison, "artifact_digests": artifacts,
            "finished_at_utc": datetime.now(timezone.utc).isoformat()})
        raise ContextAcquisitionError(str(exc), acquisition_evidence(path, runs)) from exc
