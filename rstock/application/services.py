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
from .production_repository import ProductionRepository
from .production_services import (
    OperationalUniverseService,
    ProductionLifecycleService,
    PromotionService,
)
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

    def freshness(
        self, symbols: tuple[str, ...] | list[str], config: RStockConfig = DEFAULT_CONFIG
    ) -> dict[str, str | None]:
        try:
            entries = market_data_service(config).store.read_metadata().get("symbols", {})
        except (FileNotFoundError, ValueError):
            entries = {}
        return {
            symbol: (
                None
                if symbol not in entries
                else str(entries[symbol].get("last_date") or "") or None
            )
            for symbol in symbols
        }

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

    def __init__(self, project_root: Path = DEFAULT_CONFIG.project_root) -> None:
        self.repository = ProductionRepository(project_root)

    def parameters(self, config: RStockConfig) -> dict[str, int | float]:
        return historical_xgboost_parameters(config).as_dict()

    def capabilities(self) -> dict[str, bool]:
        return {"catalogue": True, "activation": True, "comparison": True}

    def models(self):
        return self.repository.models()

    def promote(
        self, run_id: str, set_name: str, *,
        xgboost_calibration_run: str | None = None,
        threshold_calibration_run: str | None = None,
    ):
        return PromotionService(
            RunRepository(self.repository.root.parent / "runs"), self.repository
        ).promote(
            run_id, set_name,
            xgboost_calibration_run=xgboost_calibration_run,
            threshold_calibration_run=threshold_calibration_run,
        )

    def activate(self, model_id: str):
        return ProductionLifecycleService(self.repository).activate(model_id)

    def deactivate(self, model_id: str):
        return ProductionLifecycleService(self.repository).deactivate(model_id)

    def retire(self, model_id: str):
        return ProductionLifecycleService(self.repository).retire(model_id)

    def operational_universe(self):
        return OperationalUniverseService(self.repository).current()


class PredictionService:
    """Extension point for scheduled and on-demand prediction workflows."""

    def __init__(self, project_root: Path = DEFAULT_CONFIG.project_root) -> None:
        self.repository = ProductionRepository(project_root)

    def capabilities(self) -> dict[str, bool]:
        return {"daily_prediction": True, "batch_prediction": True}

    def history(self) -> pd.DataFrame:
        return self.repository.read_table("predictions")


class SignalService:
    """Extension point for persisted directional signals and future monitoring."""

    def __init__(self, project_root: Path = DEFAULT_CONFIG.project_root) -> None:
        self.repository = ProductionRepository(project_root)

    def capabilities(self) -> dict[str, bool]:
        return {"signal_history": True, "screening": True, "broker_orders": False}

    def history(self) -> pd.DataFrame:
        return self.repository.read_table("signals")

    def realized_results(self) -> pd.DataFrame:
        return self.repository.read_table("realized_results")


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
    model_id: str | None = None,
) -> ExperimentSpec:
    return ExperimentSpec(
        job_type=job_type,
        config=replace(DEFAULT_CONFIG, project_root=Path(project_root).resolve()),
        symbols=tuple(symbols),
        model_id=model_id,
    )
