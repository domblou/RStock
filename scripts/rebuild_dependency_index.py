"""Explicitly reconstruct the dependency index from the current artifacts."""
import argparse
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from rstock.application.repository import RunRepository
from rstock.application.run_delete import RunDeletionService


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs-root", type=Path, default=Path("runs"))
    args = parser.parse_args()
    result = RunDeletionService(RunRepository(args.runs_root.resolve())).rebuild_dependency_index()
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
