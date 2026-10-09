"""Small, per-run resource record built from observed process counters."""

from __future__ import annotations

import json
import os
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from rstock.telemetry import available_logical_processors, descendant_pids, process_sample


SCHEMA_VERSION = 1
SAMPLE_SECONDS = 5.0


def _utc() -> str:
    return datetime.now(timezone.utc).isoformat()


def _atomic_json(path: Path, value: dict[str, Any]) -> None:
    temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)


class ResourceRecorder:
    """One writer per run; previous attempts survive an interrupted resume."""

    def __init__(self, run_directory: Path, run_id: str, configuration: dict[str, Any],
                 *, started_monotonic: float | None = None,
                 started_at: str | None = None):
        self.directory = run_directory / "telemetry"
        self.directory.mkdir(parents=True, exist_ok=True)
        self.summary_path = self.directory / "resource_summary.json"
        self.samples_path = self.directory / "samples.jsonl"
        self.batches_path = self.directory / "batches.jsonl"
        self.run_id = run_id
        self.pid = os.getpid()
        self.logical_processors = available_logical_processors()
        self.lock = threading.RLock()
        self.stop_event = threading.Event()
        self.previous: dict[tuple[int, int], float] = {}
        self.previous_at: float | None = None
        self.active_phase: str | None = None
        self.active_subphase: str | None = None
        self.subphase_measurements: dict[str, dict[str, Any]] = {}
        self.phase_starts: dict[str, float] = {}
        self.last_units: dict[str, int] = {}
        self.started = time.monotonic() if started_monotonic is None else started_monotonic
        try:
            persisted = json.loads(self.summary_path.read_text(encoding="utf-8"))
            attempts = persisted["attempts"] if persisted.get("schema_version") == 1 else []
            if not isinstance(attempts, list):
                attempts = []
        except (FileNotFoundError, OSError, ValueError, KeyError, TypeError):
            attempts = []
        if attempts and isinstance(attempts[-1], dict) and attempts[-1].get("status") == "running":
            # A previous worker vanished without its final checkpoint. Preserve
            # measured counters, but do not claim that its open interval ended.
            attempts[-1]["status"] = "interrupted"
            attempts[-1]["reconciled_at"] = _utc()
        self.document: dict[str, Any] = {
            "schema_version": SCHEMA_VERSION, "run_id": run_id, "attempts": attempts,
        }
        self.attempt: dict[str, Any] = {
            "attempt_id": uuid.uuid4().hex,
            "started_at": started_at or _utc(), "finished_at": None, "status": "running",
            "configuration": configuration,
            "logical_processors": self.logical_processors,
            "wait_seconds": {}, "phase_rows": [], "sample_count": 0,
            "cpu_seconds": None, "cpu_covered_seconds": 0.0,
            "cpu_max_sampled_cores": None,
            "rss_peak_sampled_bytes": None,
            "parent_rss_peak_sampled_bytes": None,
            "children_rss_peak_sampled_bytes": None,
            "max_child_processes_observed": None,
            "max_cpu_active_children_observed": None,
            "exited_children_between_samples": 0,
            "measurement_scope": "process_tree_inclusive",
        }
        self.document["attempts"].append(self.attempt)
        with self.lock:
            self._sample_locked()
            self._persist_locked()
        self.thread = threading.Thread(target=self._sampling_loop, daemon=True)
        self.thread.start()

    def _append(self, path: Path, value: dict[str, Any]) -> None:
        with path.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(value, ensure_ascii=False) + "\n")

    def _persist_locked(self) -> None:
        self.attempt["elapsed_seconds"] = max(0.0, time.monotonic() - self.started)
        cpu = self.attempt["cpu_seconds"]
        covered = self.attempt["cpu_covered_seconds"]
        self.attempt["cpu_mean_cores"] = (
            None if cpu is None or covered <= 0 else cpu / covered
        )
        _atomic_json(self.summary_path, self.document)

    def _sample_locked(self) -> None:
        now = time.monotonic()
        root = process_sample(self.pid)
        child_pids = descendant_pids(self.pid)
        children = [sample for pid in child_pids if (sample := process_sample(pid))]
        all_found = len(children) == len(child_pids)
        total_rss = (
            root.rss_bytes + sum(item.rss_bytes for item in children)
            if root is not None and all_found else None
        )
        current = {
            (item.pid, item.identity): item.cpu_seconds
            for item in ([root] if root is not None else []) + children
        }
        elapsed = None if self.previous_at is None else max(0.0, now - self.previous_at)
        exited = {key for key in self.previous if key[0] != self.pid} - set(current)
        self.attempt["exited_children_between_samples"] += len(exited)
        cpu_delta = None
        if elapsed is not None and elapsed > 0 and root is not None:
            root_key = (root.pid, root.identity)
            if root_key in self.previous:
                cpu_delta = sum(
                    max(0.0, value - self.previous.get(key, 0.0))
                    for key, value in current.items()
                )
        cpu_cores = None if cpu_delta is None or elapsed is None else cpu_delta / elapsed
        active_children = (
            None if cpu_delta is None else sum(
                item.cpu_seconds > self.previous.get((item.pid, item.identity), 0.0)
                for item in children
            )
        )
        if cpu_delta is not None:
            self.attempt["cpu_seconds"] = (self.attempt["cpu_seconds"] or 0.0) + cpu_delta
            self.attempt["cpu_covered_seconds"] += elapsed
            old = self.attempt["cpu_max_sampled_cores"]
            self.attempt["cpu_max_sampled_cores"] = max(old or 0.0, cpu_cores)
            if self.active_phase is not None:
                phase = self._open_phase(self.active_phase)
                if phase is not None:
                    phase["cpu_seconds"] = (phase["cpu_seconds"] or 0.0) + cpu_delta
                    phase["cpu_covered_seconds"] += elapsed
                    phase["cpu_max_sampled_cores"] = max(
                        phase["cpu_max_sampled_cores"] or 0.0, cpu_cores
                    )
            if self.active_subphase is not None:
                subphase = self.subphase_measurements[self.active_subphase]
                subphase["cpu_seconds_sampled"] = (
                    subphase["cpu_seconds_sampled"] or 0.0
                ) + cpu_delta
                subphase["cpu_covered_seconds"] += elapsed
                subphase["cpu_max_sampled_cores"] = max(
                    subphase["cpu_max_sampled_cores"] or 0.0, cpu_cores
                )
        if root is not None:
            old = self.attempt["parent_rss_peak_sampled_bytes"]
            self.attempt["parent_rss_peak_sampled_bytes"] = max(old or 0, root.rss_bytes)
        if all_found:
            child_rss = sum(item.rss_bytes for item in children)
            old = self.attempt["children_rss_peak_sampled_bytes"]
            self.attempt["children_rss_peak_sampled_bytes"] = max(old or 0, child_rss)
        if total_rss is not None:
            old = self.attempt["rss_peak_sampled_bytes"]
            self.attempt["rss_peak_sampled_bytes"] = max(old or 0, total_rss)
            if self.active_phase is not None:
                phase = self._open_phase(self.active_phase)
                if phase is not None:
                    phase["rss_peak_sampled_bytes"] = max(
                        phase["rss_peak_sampled_bytes"] or 0, total_rss
                    )
            if self.active_subphase is not None:
                subphase = self.subphase_measurements[self.active_subphase]
                subphase["rss_peak_sampled_bytes"] = max(
                    subphase["rss_peak_sampled_bytes"] or 0, total_rss
                )
        if root is not None:
            old = self.attempt["max_child_processes_observed"]
            self.attempt["max_child_processes_observed"] = max(old or 0, len(children))
            if active_children is not None:
                old_active = self.attempt["max_cpu_active_children_observed"]
                self.attempt["max_cpu_active_children_observed"] = max(
                    old_active or 0, active_children
                )
            if self.active_phase is not None:
                phase = self._open_phase(self.active_phase)
                if phase is not None:
                    phase["max_child_processes_observed"] = max(
                        phase["max_child_processes_observed"] or 0, len(children)
                    )
                    if active_children is not None:
                        phase["max_cpu_active_children_observed"] = max(
                            phase["max_cpu_active_children_observed"] or 0,
                            active_children,
                        )
        self._append(self.samples_path, {
            "schema_version": SCHEMA_VERSION, "attempt_id": self.attempt["attempt_id"],
            "at": _utc(), "elapsed_seconds": now - self.started,
            "phase": self.active_phase, "cpu_cores": cpu_cores,
            "parent_rss_bytes": None if root is None else root.rss_bytes,
            "children_rss_bytes": sum(item.rss_bytes for item in children) if all_found else None,
            "total_rss_bytes": total_rss,
            "child_processes": len(children) if all_found else None,
            "cpu_active_children": active_children,
        })
        self.attempt["sample_count"] += 1
        self.previous, self.previous_at = current, now

    def _sampling_loop(self) -> None:
        while not self.stop_event.wait(SAMPLE_SECONDS):
            with self.lock:
                try:
                    self._sample_locked()
                except (OSError, ValueError):
                    pass  # A failed reading remains absent; it must not fail ML.

    def _open_phase(self, name: str) -> dict[str, Any] | None:
        return next((row for row in reversed(self.attempt["phase_rows"])
                     if row["name"] == name and row["finished_at"] is None), None)

    def phase_started(self, name: str, details: dict[str, object] | None = None) -> None:
        with self.lock:
            self._sample_locked()
            self.phase_starts[name] = time.monotonic()
            self.active_phase = name
            self.attempt["phase_rows"].append({
                "name": name, "started_at": _utc(), "finished_at": None,
                "duration_seconds": None, "details": details or {},
                "completed_items": None, "item_kind": None,
                "cpu_seconds": None, "cpu_covered_seconds": 0.0,
                "cpu_mean_cores": None, "cpu_max_sampled_cores": None,
                "rss_peak_sampled_bytes": None,
                "max_child_processes_observed": None,
                "max_cpu_active_children_observed": None,
            })
            self._persist_locked()

    def subphase_started(self, phase: str, name: str) -> None:
        with self.lock:
            if self.active_phase != phase:
                return
            self._sample_locked()
            self.active_subphase = name
            self.subphase_measurements[name] = {
                "cpu_seconds_sampled": None,
                "cpu_covered_seconds": 0.0,
                "cpu_max_sampled_cores": None,
                "rss_peak_sampled_bytes": None,
            }

    def subphase_completed(self, phase: str, name: str) -> None:
        with self.lock:
            if self.active_phase != phase or self.active_subphase != name:
                return
            self._sample_locked()
            self.active_subphase = None
            row = self.subphase_measurements[name]
            cpu, covered = row["cpu_seconds_sampled"], row["cpu_covered_seconds"]
            row["cpu_mean_sampled_cores"] = (
                None if cpu is None or covered <= 0 else cpu / covered
            )

    def phase_completed(self, name: str, details: dict[str, object] | None = None) -> None:
        with self.lock:
            self._sample_locked()
            phase = self._open_phase(name)
            if phase is None:
                return
            phase["finished_at"] = _utc()
            phase["duration_seconds"] = max(0.0, time.monotonic() - self.phase_starts.pop(name))
            if details:
                phase["details"].update(details)
            for row in phase["details"].get("subphases", []):
                if isinstance(row, dict) and row.get("name") in self.subphase_measurements:
                    row.update(self.subphase_measurements[row["name"]])
            for key, kind in (
                ("combinations_processed", "combinaisons"),
                ("combinations", "combinaisons"),
                ("models", "modèles"), ("symbols", "symboles"),
                ("predictions", "prédictions"), ("rows", "lignes"),
            ):
                value = (details or {}).get(key)
                if isinstance(value, int) and not isinstance(value, bool):
                    phase["completed_items"] = value
                    phase["item_kind"] = kind
                    break
            cpu, covered = phase["cpu_seconds"], phase["cpu_covered_seconds"]
            phase["cpu_mean_cores"] = None if cpu is None or covered <= 0 else cpu / covered
            if self.active_phase == name:
                self.active_phase = None
                self.active_subphase = None
                self.subphase_measurements.clear()
            self._persist_locked()

    def batch_completed(self, phase: str, details: dict[str, object],
                        completed_units: int | None) -> None:
        if not details.get("checkpoint_written") or "batch_id" not in details:
            return
        with self.lock:
            count = details.get("models") if phase == "forward_simulation" else details.get("combinations")
            if not isinstance(count, int) and completed_units is not None:
                previous = self.last_units.get(phase)
                count = None if previous is None else max(0, completed_units - previous)
            if completed_units is not None:
                self.last_units[phase] = completed_units
            elapsed = details.get("elapsed_seconds")
            self._append(self.batches_path, {
                "schema_version": SCHEMA_VERSION, "attempt_id": self.attempt["attempt_id"],
                "phase": phase, "batch_id": details["batch_id"], "at": _utc(),
                "item_kind": (
                    "modèles" if phase == "forward_simulation" else
                    "combinaisons" if phase == "walk_forward" else None
                ),
                "completed_items": count if isinstance(count, int) else None,
                "duration_seconds": elapsed if isinstance(elapsed, (int, float)) else None,
                "calculation_seconds": details.get("calculation_seconds"),
                "rows": details.get("rows"), "checkpoint_written": True,
                **{key: details[key] for key in (
                    "selection_worker_seconds", "refit_worker_seconds",
                    "selection_rounds_run", "refit_rounds") if key in details},
            })

    def wait_completed(self, name: str, seconds: float) -> None:
        with self.lock:
            self.attempt["wait_seconds"][name] = (
                self.attempt["wait_seconds"].get(name, 0.0) + max(0.0, seconds)
            )
            self._persist_locked()

    def close(self, status: str, checkpoint_manifest: dict[str, Any] | None = None,
              error: str | None = None) -> None:
        self.stop_event.set()
        self.thread.join(timeout=1)
        with self.lock:
            self._sample_locked()
            self.attempt["status"] = status
            self.attempt["error"] = error
            self.attempt["finished_at"] = _utc()
            if isinstance(checkpoint_manifest, dict):
                self.attempt["checkpoint"] = {
                    "attempt_count": checkpoint_manifest.get("attempt_count"),
                    "resume_count": checkpoint_manifest.get("resume_count"),
                    "completed_batches": {
                        phase: len(value.get("completed", []))
                        for phase, value in checkpoint_manifest.get("batches", {}).items()
                        if isinstance(value, dict)
                    },
                }
            self._persist_locked()
