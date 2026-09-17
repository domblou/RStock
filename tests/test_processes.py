from __future__ import annotations

import os

from rstock.application.processes import process_alive
from rstock.application.runner import _pid_alive
from rstock.application.worker import _process_alive
from rstock.application.workflows import _worker_pid_alive


def test_liveness_probes_are_safe_for_the_calling_process():
    current_pid = os.getpid()

    assert process_alive(current_pid)
    assert _pid_alive(current_pid)
    assert _process_alive(current_pid)
    assert _worker_pid_alive(current_pid)


def test_liveness_probe_rejects_invalid_pids():
    for value in (None, "invalid", 0, -1):
        assert process_alive(value) is False
