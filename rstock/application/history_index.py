"""Read-through History index. No snapshots or scientific state are rewritten.

File identities are checked on every lookup, including across repository/UI
instances. Only display projections are retained; expensive summaries are lazy.
"""
from collections import OrderedDict
from collections.abc import Mapping
from copy import deepcopy
from dataclasses import dataclass
import json
import os
from threading import RLock

from .domain import RunMetadata

_CONFIG_FIELDS = frozenset({
    "job_type", "model_id", "symbols", "target_symbols", "run_description",
    "primary_universe_id", "universe_selection", "derivation",
    "prefilter_derivation", "walk_forward_derivation", "source_prefilter_run",
    "source_end_to_end_run", "source_experiment_run", "source_walk_forward_run",
    "source_xgboost_calibration_run", "source_threshold_parameter_calibration_run",
    "source_forced_candidate_validation_run", "historical_forced_validation_backfill",
    "forward_simulation_mode", "forward_simulation_enabled", "forward_simulation_end_date",
    "resolved_market_session_cutoff", "historical_data_cutoff", "requested_historical_cutoff",
})
_SUMMARY_FIELDS = frozenset({
    "model_id", "requested_symbols", "updated_symbols", "predictions", "signals",
    "categories", "realized_results", "eligible_combinations", "outcome",
    "holdout_combination_counts", "stage_run_ids", "prepared_dataset_as_of",
})
_CACHE = OrderedDict()
_LOCK = RLock()
_MAX_FILES = 4096


def clear_history_cache():
    with _LOCK:
        _CACHE.clear()


def _identity(path):
    try:
        stat = path.stat()
        return (stat.st_mtime_ns, stat.st_ctime_ns, stat.st_size, stat.st_ino)
    except FileNotFoundError:
        return None


def _cached(path, kind, loader):
    key = (os.path.normcase(os.path.abspath(path)), kind)
    # Serialize cache fills so two sessions never publish an older read over a
    # newer one. Recheck the file after reading; unstable reads aren't cached.
    with _LOCK:
        before = _identity(path)
        cached = _CACHE.get(key)
        if cached is not None and cached[0] == before:
            _CACHE.move_to_end(key)
            return deepcopy(cached[1])
        value = loader()
        if before == _identity(path):
            _CACHE[key] = (before, value)
            _CACHE.move_to_end(key)
            while len(_CACHE) > _MAX_FILES:
                _CACHE.popitem(last=False)
        else:
            _CACHE.pop(key, None)
        return deepcopy(value)


def _configuration(repository, run_id):
    def load():
        # Keep the existing historical deserialization, including its explicit
        # compatibility rules, before projecting the display fields.
        config = repository.load_spec(run_id).to_dict()
        result = {key: config[key] for key in _CONFIG_FIELDS if key in config}
        raw = config.get("rstock_config", {})
        result["rstock_config"] = {key: raw[key] for key in (
            "walk_forward_window_mode", "walk_forward_train_size", "permutation_depth"
        ) if key in raw}
        return result
    return _cached(repository.run_directory(run_id) / "config.json", "config", load)


def _summary(repository, run_id):
    def load():
        summary = repository.summary(run_id)
        result = {key: summary[key] for key in _SUMMARY_FIELDS if key in summary}
        trace = summary.get("traceability")
        if isinstance(trace, Mapping):
            result["traceability"] = {key: trace[key] for key in (
                "prepared_market_last_date", "prepared_dataset_as_of"
            ) if key in trace}
        if "stages" in summary:
            result["stages"] = [{} for _ in summary["stages"]]
        missing = summary.get("missing_frozen_thresholds")
        if isinstance(missing, list):
            result["missing_frozen_thresholds"] = [
                {"direction": item.get("direction")} if isinstance(item, Mapping) else None
                for item in missing
            ]
        return result
    return _cached(repository.run_directory(run_id) / "summary.json", "summary", load)


class HistoryDetail(Mapping):
    """Filters read config/storage without materializing a scientific summary."""
    def __init__(self, repository, run_id):
        self.repository = repository
        self.run_id = run_id

    def __iter__(self):
        return iter(("configuration", "metadata", "storage", "summary"))

    def __len__(self):
        return 4

    def __getitem__(self, key):
        repository, run_id = self.repository, self.run_id
        directory = repository.run_directory(run_id)
        if key == "configuration":
            return _configuration(repository, run_id)
        if key == "summary":
            return _summary(repository, run_id)
        if key == "metadata":
            return _cached(directory / "metadata.json", key,
                           lambda: repository.run_metadata(run_id).to_dict())
        if key == "storage":
            return _cached(directory / "storage.json", key,
                           lambda: repository.storage(run_id))
        raise KeyError(key)


@dataclass(frozen=True)
class HistoryIndexRecord:
    status: dict
    _detail: HistoryDetail

    def detail(self):
        return self._detail


def history_index(run_service, *, job_types=None):
    repository = run_service.repository
    records = []
    for run_id in repository.list_run_ids():
        detail = HistoryDetail(repository, run_id)
        if not RunMetadata.from_dict(detail["metadata"]).visible_in_history:
            continue
        status = _cached(repository.run_directory(run_id) / "status.json", "status",
                         lambda: repository.status(run_id))
        if status.get("status") == "running":
            # The existing heartbeat/lease/interruption reconciliation must run
            # even when the persisted status file hasn't changed.
            status = run_service._refresh_interrupted(run_id)
        else:
            run_service._forget_interruption_observation(run_id)
        if job_types is None or status.get("job_type") in job_types:
            records.append(HistoryIndexRecord(status, detail))
    return records


def cached_display_json(path):
    """Cache small source labels/contracts, invalidated by their own file."""
    return _cached(path, "display", lambda: json.loads(path.read_text(encoding="utf-8")))
