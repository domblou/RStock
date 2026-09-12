"""Application services exposed to user interfaces and CLI clients."""

from __future__ import annotations

from dataclasses import replace
from pathlib import Path
from typing import Any

import pandas as pd

from rstock.config import DEFAULT_CONFIG, RStockConfig
from rstock.market_cache import MarketDataResult, market_data_service
from rstock.modeling import historical_xgboost_parameters
from rstock.progress import CancellationCheck, ProgressCallback

from .domain import ExperimentSpec, JobType
from .repository import RunRepository
from .runner import RunService, SubmissionResult


class MarketDataService:
    """Application-facing market data facade."""

    def available_symbols(self, config: RStockConfig = DEFAULT_CONFIG) -> list[str]:
        try:
            metadata = market_data_service(config).store.read_metadata()
        except (FileNotFoundError, ValueError):
            return []
        return sorted(str(symbol) for symbol in metadata.get("symbols", {}))

    def load(
        self,
        spec: ExperimentSpec,
        *,
        progress_callback: ProgressCallback | None = None,
        cancellation_check: CancellationCheck | None = None,
    ) -> tuple[MarketDataResult, dict[str, str]]:
        universe = pd.DataFrame(
            {
                "Symbol": spec.symbols,
                "ProviderSymbol": spec.symbols,
                "Exchange": spec.calendar,
                "Calendar": spec.calendar,
            }
        )
        result = market_data_service(spec.config).get_market_data(
            universe,
            spec.config.model_history_days,
            progress_callback=progress_callback,
            cancellation_check=cancellation_check,
        )
        calendars = {symbol: spec.calendar for symbol in result.symbols}
        return result, calendars


class ModelService:
    """Stable facade for current model configuration and future model catalogues."""

    def parameters(self, config: RStockConfig) -> dict[str, int | float]:
        return historical_xgboost_parameters(config).as_dict()

    def capabilities(self) -> dict[str, bool]:
        return {"catalogue": False, "activation": False, "comparison": False}


class PredictionService:
    """Extension point for scheduled and on-demand prediction workflows."""

    def capabilities(self) -> dict[str, bool]:
        return {"daily_prediction": False, "batch_prediction": False}


class SignalService:
    """Extension point for persisted directional signals and future monitoring."""

    def capabilities(self) -> dict[str, bool]:
        return {"signal_history": False, "screening": False, "broker_orders": False}


class ExperimentService:
    """Submit and inspect experiments without exposing ML internals."""

    def __init__(self, run_service: RunService) -> None:
        self.run_service = run_service

    @classmethod
    def local(
        cls,
        project_root: Path,
        *,
        max_concurrent_heavy_jobs: int = 1,
    ) -> "ExperimentService":
        repository = RunRepository(Path(project_root) / "runs")
        return cls(
            RunService(
                repository,
                max_concurrent_heavy_jobs=max_concurrent_heavy_jobs,
            )
        )

    def submit(self, spec: ExperimentSpec) -> SubmissionResult:
        return self.run_service.submit(spec)

    def cancel(self, run_id: str) -> dict[str, object]:
        return self.run_service.cancel(run_id)

    def runs(self) -> list[dict[str, object]]:
        return self.run_service.list()

    def run(self, run_id: str) -> dict[str, object]:
        return self.run_service.get(run_id)


def default_experiment_spec(
    job_type: JobType,
    symbols: list[str] | tuple[str, ...],
    *,
    project_root: Path = DEFAULT_CONFIG.project_root,
) -> ExperimentSpec:
    return ExperimentSpec(
        job_type=job_type,
        config=replace(DEFAULT_CONFIG, project_root=Path(project_root).resolve()),
        symbols=tuple(symbols),
    )
