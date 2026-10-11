"""Non-destructive comparison of complete dependency scans; never deletes a run."""
import argparse
from collections import Counter
import json
from pathlib import Path
import statistics
import subprocess
import sys
import time
import types
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from rstock.application import run_delete
from rstock.application.dependency_index import clear_dependency_memory
from rstock.application.repository import RunRepository
from rstock.application.run_integrity import graph_lock


def measure(repository, module, *, force=False, persist=True, runs_only=False):
    reads = Counter()
    original = Path.open
    def opened(path, mode="r", *args, **kwargs):
        if "r" in mode and path.is_relative_to(repository.root.parent):
            reads["file_reads"] += 1
            if path.suffix == ".json":
                reads["json_reads"] += 1
            elif path.suffix == ".csv":
                reads["csv_reads"] += 1
        return original(path, mode, *args, **kwargs)
    started = time.perf_counter()
    with patch.object(Path, "open", opened), graph_lock(repository.root):
        graph = (module._DeletionGraph(repository, force_index=force, persist_index=persist)
                 if module is run_delete else module._DeletionGraph(repository))
        # Empty selection deliberately scans every retained source. No eligibility
        # check, quarantine, renaming or physical deletion is performed.
        if runs_only:
            for directory in graph.directories:
                options = {"scientific_run": True}
                if module is run_delete:
                    options["observer"] = graph.index.observe
                for path in module._reference_paths(directory, **options):
                    tuple(graph.iter_file_edges(path))
        else:
            blocker = module.RunDeletionService(repository)._external_dependency(set(), graph)
            assert blocker is None
        if module is run_delete:
            graph.index.publish(complete=not runs_only)
        result = {"seconds": time.perf_counter() - started,
                  "file_reads": reads["file_reads"], "json_reads": reads["json_reads"],
                  "csv_reads": reads["csv_reads"], "files_with_json_edges": len(graph.edges)}
        if module is run_delete:
            result.update(graph.index.stats)
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--runs-root", type=Path, default=Path("runs"))
    parser.add_argument("--legacy-source", type=Path)
    parser.add_argument("--baseline-ref", default="16f20d3fada3ac178e8cf6c7085ecc411b9a1628")
    parser.add_argument("--output", type=Path, default=Path("reports/dependency-index/benchmark.json"))
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--runs-only", action="store_true")
    args = parser.parse_args()
    repository = RunRepository(args.runs_root.resolve())
    legacy = types.ModuleType("rstock.application._legacy_run_delete_benchmark")
    legacy.__package__ = "rstock.application"
    sys.modules[legacy.__name__] = legacy
    source = (args.legacy_source.read_text(encoding="utf-8") if args.legacy_source else
              subprocess.run(["git", "show", f"{args.baseline_ref}:rstock/application/run_delete.py"],
                             check=True, capture_output=True, text=True, encoding="utf-8").stdout)
    exec(compile(source, "legacy_run_delete_benchmark", "exec"), legacy.__dict__)
    results = {}
    for name, module, force in (("before", legacy, False), ("after_cold", run_delete, True),
                                 ("after_persistent_reload", run_delete, False),
                                 ("after_warm", run_delete, False)):
        samples = []
        for _ in range(args.repeats):
            if name == "after_persistent_reload":
                clear_dependency_memory()
            samples.append(measure(repository, module, force=force, runs_only=args.runs_only))
        results[name] = {"median_seconds": statistics.median(sample["seconds"] for sample in samples),
                         "samples": samples}
    report = {"python": sys.version.split()[0], "runs_root": str(repository.root),
              "measurement": "complete source discovery + dependency parsing/checks + index synchronization; no deletion",
              "baseline_ref": args.baseline_ref, "scope": "runs only" if args.runs_only else "runs + production + simulations", "results": results}
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
