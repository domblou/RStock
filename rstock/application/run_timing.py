"""Bounded reads of observed worker lifetime, distinct from disappearance detection."""
from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Mapping, Any


def _timestamp(value):
    try:
        stamp = datetime.fromisoformat(str(value))
        return stamp if stamp.tzinfo is not None else None
    except (TypeError, ValueError):
        return None


def observed_run_timing(directory: Path, status: Mapping[str, Any],
                        progress: Mapping[str, Any] | None = None) -> dict[str, Any]:
    """Observed elapsed time is evidence of lifetime, not CPU-active compute time.

    Historical status JSON remains authoritative for its original wall duration.
    Missing telemetry never turns delayed detection into observed execution.
    """
    interrupted = status.get("status") == "interrupted"
    wall = status.get("duration_seconds")
    result = {"duration_seconds": wall, "wall_duration_seconds": wall,
              "last_observed_at": None, "observed_duration_seconds": None,
              "interruption_detected_at": status.get("finished_at") if interrupted else None,
              "detection_delay_seconds": None, "observation_source": None}
    if not interrupted:
        return result
    result["duration_seconds"] = None
    started, detected = _timestamp(status.get("started_at")), _timestamp(status.get("finished_at"))
    if started is None or detected is None:
        return result
    candidates = []
    def consider(value, source):
        stamp = _timestamp(value)
        if stamp is not None and started <= stamp <= detected:
            candidates.append((stamp, source))
    consider(status.get("worker_last_observed_at"), "persisted")
    path = directory / "telemetry/samples.jsonl"
    try:
        # Avoid parsing millions of samples on every History refresh. Ignore
        # a partial final line left by an interrupted writer.
        with path.open("rb") as stream:
            stream.seek(0, 2)
            size = stream.tell()
            stream.seek(max(0, size - 65536))
            tail = stream.read().splitlines()
        for line in reversed(tail):
            try:
                sample = json.loads(line)
            except (ValueError, UnicodeDecodeError):
                continue
            if not isinstance(sample, dict):
                continue
            stamp = _timestamp(sample.get("at"))
            if stamp is not None and started <= stamp <= detected:
                consider(sample["at"], "telemetry")
                break
    except OSError:
        pass
    if progress is None:
        try:
            progress = json.loads((directory / "progress.json").read_text(encoding="utf-8"))
        except (OSError, ValueError):
            progress = {}
    if isinstance(progress, Mapping):
        consider(progress.get("updated_at"), "progress")
    if candidates:
        last, source = max(candidates, key=lambda candidate: candidate[0])
        elapsed = (last - started).total_seconds()
        result.update(duration_seconds=elapsed, observed_duration_seconds=elapsed,
                      last_observed_at=last.isoformat(), observation_source=source,
                      detection_delay_seconds=(detected - last).total_seconds())
    return result
