"""Compare History loading pipelines on existing terminal runs, read-only.

Times include indexing, filters, pagination and row formatting, excluding the
browser/network and external model/universe catalogues. Legacy remains available
for reproducible comparisons after deployment.
"""
import argparse
from collections import Counter
import json
from pathlib import Path
import statistics
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from rstock.application.history_index import clear_history_cache, history_index
from rstock.application.history_ui import EXPERIMENT_JOB_TYPES, filter_runs, paginate_runs, history_row
from rstock.application.history_grid import history_grid_row
from rstock.application.repository import RunRepository
from rstock.application.runner import RunService


class ReadOnlyRepository(RunRepository):
    def list_run_ids(self):
        # Benchmark must never perform recovery or acquire/create a disk mutex.
        return self._list_run_ids()


def navigation(service, indexed, page=0):
    records = (history_index(service, job_types=EXPERIMENT_JOB_TYPES) if indexed
               else service.history_summaries(job_types=EXPERIMENT_JOB_TYPES))
    details = {item.status["run_id"]: item.detail() for item in records}
    filtered = filter_runs([item.status for item in records], allowed_types=EXPERIMENT_JOB_TYPES,
                          detail_loader=details.__getitem__)
    visible, _ = paginate_runs(filtered, page=page, page_size=25)
    rows = [history_grid_row(history_row(status, details[status["run_id"]], {}, related_details=details),
                             details[status["run_id"]], universe_labels={}, related_details=details,
                             runs_root=service.repository.root) for status in visible]
    return rows, len(filtered)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path("runs"))
    parser.add_argument("--output", type=Path)
    parser.add_argument("--repeats", type=int, default=3)
    args = parser.parse_args()
    repository = ReadOnlyRepository(args.root)
    statuses = repository.list_runs()
    if any(status["status"] in {"running", "pending"} for status in statuses):
        raise RuntimeError("Benchmark requires terminal runs to avoid altering live reconciliation.")
    service = RunService(repository)
    results = {}
    reference = {}
    original = RunRepository._read_json_path
    for name, indexed, clear in (("before", False, False), ("after_cold", True, True),
                                  ("after_cached", True, False)):
        samples = []
        for _ in range(args.repeats):
            if clear:
                clear_history_cache()
            reads = Counter()
            def read(path):
                reads["json_reads"] += 1
                reads["bytes_read"] += path.stat().st_size
                if path.name == "summary.json":
                    reads["summary_reads"] += 1
                return original(path)
            RunRepository._read_json_path = staticmethod(read)
            try:
                started = time.perf_counter()
                rows, count = navigation(service, indexed)
                elapsed = time.perf_counter() - started
            finally:
                RunRepository._read_json_path = staticmethod(original)
            if not reference:
                reference = rows
            assert rows == reference, "Grid display changed between loading pipelines"
            samples.append({"seconds": elapsed, **reads})
        results[name] = {"median_seconds": statistics.median(s["seconds"] for s in samples),
                         "samples": samples, "filtered_runs": count, "visible_rows": len(rows)}
    # Page navigation correctness after cache population.
    assert navigation(service, False, page=1) == navigation(service, True, page=1)
    report = {"python": sys.version.split()[0], "root": str(args.root.resolve()),
              "terminal_runs": len(statuses), "measurement": "index + filters + pagination + grid row formatting",
              "results": results}
    text = json.dumps(report, indent=2)
    print(text)
    if args.output:
        args.output.write_text(text + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
