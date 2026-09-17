"""Worker-local bridge used by workflow code to execute technical children."""

from __future__ import annotations

from collections.abc import Callable, Iterator
from contextlib import contextmanager
from contextvars import ContextVar


ChildExecutor = Callable[[str], None]
_child_executor: ContextVar[ChildExecutor | None] = ContextVar(
    "rstock_child_executor", default=None
)


@contextmanager
def child_executor_context(executor: ChildExecutor) -> Iterator[None]:
    token = _child_executor.set(executor)
    try:
        yield
    finally:
        _child_executor.reset(token)


def execute_child(run_id: str) -> None:
    executor = _child_executor.get()
    if executor is None:
        raise RuntimeError("Aucun exécuteur d'enfant n'est attaché à ce worker.")
    executor(run_id)
