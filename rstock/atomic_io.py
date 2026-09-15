"""Shared policy for resilient atomic local-file publication."""

from __future__ import annotations

import errno


ATOMIC_WRITE_ATTEMPTS = 5
ATOMIC_WRITE_BACKOFF_SECONDS = 0.02


def is_temporary_file_lock(error: OSError) -> bool:
    """Recognise transient Windows sharing/access-denied errors."""

    return isinstance(error, PermissionError) or getattr(error, "winerror", None) in {
        5,   # ERROR_ACCESS_DENIED
        32,  # ERROR_SHARING_VIOLATION
        33,  # ERROR_LOCK_VIOLATION
    } or getattr(error, "errno", None) == errno.EACCES
