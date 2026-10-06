"""Telemetry adapter for multiple unchanged autonomous Prefilter evaluations."""
from time import monotonic

from rstock.progress import ProgressEvent

PHASE = "predictor_prefilter_walk_forward"


class TemporalPrefilterProgress:
    def __init__(self, callback, cutoffs, units_per_origin, checkpoints, batch_size):
        self.callback = callback
        self.cutoffs = list(cutoffs)
        self.units = units_per_origin
        self.completed = []
        for checkpoint in checkpoints:
            if checkpoint.artifact_exists("prefilter_selection"):
                checkpoint.load_artifact("prefilter_selection")
                checkpoint.load_artifact("prefilter_qualification")
                completed = self.units
            else:
                completed = 0
                for batch_id in checkpoint.completed_batch_ids(PHASE):
                    checkpoint.load_batch(PHASE, batch_id)  # Verify committed content.
                    completed += max(0, min(batch_size, self.units - batch_id * batch_size))
            self.completed.append(completed)
        self.baseline = sum(self.completed)
        self.started = monotonic()
        if callback:
            callback(ProgressEvent(PHASE, details={"phase_event": "started"}))

    def emit(self, index, event):
        if event.stage == PHASE and event.completed_units is not None:
            self.completed[index] = max(self.completed[index], min(self.units, event.completed_units))
        details = dict(event.details)
        details.pop("phase_event", None)  # Individual origins cannot complete the whole phase.
        # Preparation counts bytes/symbols, not univariate candidates.
        if event.stage != PHASE:
            details.pop("combinations", None)
        details.update(
            progress_scope="temporal_prefilter", origin_number=index + 1,
            origin_count=len(self.cutoffs), origin_cutoff=self.cutoffs[index],
            origin_completed_units=self.completed[index], origin_total_units=self.units,
            progress_rate_completed_units=sum(self.completed) - self.baseline,
            progress_rate_elapsed_seconds=max(0.0, monotonic() - self.started),
        )
        if self.callback:
            self.callback(ProgressEvent(
                PHASE, event.substage or event.stage,
                sum(self.completed), len(self.cutoffs) * self.units, details,
            ))

    def origin_callback(self, index):
        self.emit(index, ProgressEvent(PHASE, "origin_started"))
        return lambda event: self.emit(index, event)

    def origin_completed(self, index):
        self.completed[index] = self.units
        self.emit(index, ProgressEvent(PHASE, "origin_completed"))

    def finish(self):
        if self.callback:
            self.callback(ProgressEvent(PHASE, details={"phase_event": "completed"}))
        self.emit(len(self.cutoffs) - 1, ProgressEvent(PHASE, "completed"))
