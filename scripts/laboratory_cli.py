"""Command-line client for the same RStock Laboratory application services."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.services import (
    ExperimentService,
    ModelService,
    default_experiment_spec,
)
from rstock.config import DEFAULT_CONFIG


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--max-concurrent-heavy-jobs", type=int, default=1)
    commands = parser.add_subparsers(dest="command", required=True)

    create = commands.add_parser("create-config")
    create.add_argument("--job-type", choices=[job.value for job in JobType if job.implemented], required=True)
    create.add_argument(
        "--symbols",
        nargs="+",
        help="Required for experiments; operational jobs derive them when omitted",
    )
    create.add_argument(
        "--model-id",
        help="Required when creating a production_training configuration",
    )
    create.add_argument("--output", type=Path, required=True)

    submit = commands.add_parser("submit")
    submit.add_argument("--config", type=Path, required=True)

    commands.add_parser("list")
    status = commands.add_parser("status")
    status.add_argument("run_id")
    cancel = commands.add_parser("cancel")
    cancel.add_argument("run_id")
    return parser


def main() -> None:
    args = _parser().parse_args()
    root = args.project_root.resolve()
    if args.command == "create-config":
        job_type = JobType(args.job_type)
        symbols = tuple(args.symbols or ())
        model_service = ModelService(root)
        if job_type == JobType.PRODUCTION_TRAINING:
            if not args.model_id:
                _parser().error("--model-id is required for production_training")
            derived = model_service.repository.get(args.model_id).symbols
            if symbols and symbols != derived:
                _parser().error("--symbols must match the selected production candidate")
            symbols = derived
        elif job_type in {
            JobType.MARKET_UPDATE,
            JobType.DAILY_PREDICTION,
            JobType.DAILY_SCREENING,
            JobType.REALIZED_VALIDATION,
            JobType.OPERATIONAL_RUN,
        }:
            derived = model_service.operational_universe().symbols
            if symbols and symbols != derived:
                _parser().error("--symbols must match the current operational universe")
            symbols = derived
        elif not symbols:
            _parser().error("--symbols is required for experimental jobs")
        spec = default_experiment_spec(
            job_type,
            symbols,
            project_root=root,
            model_id=args.model_id,
        )
        args.output.write_text(
            json.dumps(spec.to_dict(), indent=2, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
        print(args.output)
        return

    service = ExperimentService.local(
        root, max_concurrent_heavy_jobs=args.max_concurrent_heavy_jobs
    )
    if args.command == "submit":
        spec = ExperimentSpec.from_dict(
            json.loads(args.config.read_text(encoding="utf-8"))
        )
        submitted = service.submit(spec)
        print(json.dumps({"run_id": submitted.run_id, "created": submitted.created}))
    elif args.command == "list":
        print(json.dumps(service.runs(), indent=2, ensure_ascii=False))
    elif args.command == "status":
        print(json.dumps(service.run(args.run_id), indent=2, ensure_ascii=False))
    elif args.command == "cancel":
        print(json.dumps(service.cancel(args.run_id), indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
