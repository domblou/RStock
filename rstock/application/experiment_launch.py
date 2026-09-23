"""Pure helpers for run-local experiment launch settings."""

from __future__ import annotations

from dataclasses import replace

from rstock.config import RStockConfig

from .domain import JobType


_WALK_FORWARD_LAUNCH_JOB_TYPES = frozenset({
    JobType.WALK_FORWARD,
    JobType.END_TO_END,
})


def walk_forward_launch_controls_visible(job_type: JobType) -> bool:
    """Return whether a launch form owns the Walk-forward geometry controls."""

    return job_type in _WALK_FORWARD_LAUNCH_JOB_TYPES


def launch_walk_forward_config(
    base: RStockConfig,
    window_mode: str,
) -> RStockConfig:
    """Build an immutable run-local config without mutating global settings."""

    return replace(
        base,
        walk_forward_window_mode=window_mode,
    )


def walk_forward_confirmation_text(config: RStockConfig) -> str:
    """Describe the exact Walk-forward protocol frozen into a pending run."""

    if config.walk_forward_window_mode == "rolling":
        train = f"Glissante {config.walk_forward_train_size}"
    else:
        train = f"Expansive · train min {config.walk_forward_min_train_size}"
    return (
        f"Walk-forward : {train} · test {config.walk_forward_test_size} · "
        f"step {config.walk_forward_step_size}"
    )
