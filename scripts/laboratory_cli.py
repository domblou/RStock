"""Command-line client for the same RStock Laboratory application services."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from rstock.application.domain import ExperimentSpec, JobType
from rstock.application.services import ExperimentService, default_experiment_spec
from rstock.config import DEFAULT_CONFIG


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=DEFAULT_CONFIG.project_root)
    parser.add_argument("--max-concurrent-heavy-jobs", type=int, default=1)
    commands = parser.add_subparsers(dest="command", required=True)

    create = commands.add_parser("create-config")
    create.add_argument("--job-type", choices=[job.value for job in JobType if job.implemented], required=True)
    create.add_argument("--symbols", nargs="+", required=True)
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
        spec = default_experiment_spec(
            JobType(args.job_type), args.symbols, project_root=root
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
