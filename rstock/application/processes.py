"""Cross-platform, non-destructive process liveness checks."""

from __future__ import annotations

import os
import math
from datetime import datetime, timezone
from pathlib import Path


def process_creation_time(pid: object) -> float | None:
    """OS creation time (Unix seconds), or unknown; never signal the process."""
    try:
        numeric = int(pid)
        if numeric <= 0:
            return None
        if os.name != "nt":
            # Linux: field 22 is start time in ticks since boot. The executable
            # name may contain spaces/parentheses, so split after its final ')'.
            fields = Path(f"/proc/{numeric}/stat").read_text().rsplit(")", 1)[1].split()
            boot = next(line.split()[1] for line in Path("/proc/stat").read_text().splitlines() if line.startswith("btime "))
            return float(boot) + int(fields[19]) / os.sysconf("SC_CLK_TCK")
        import ctypes
        from ctypes import wintypes
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        open_process = kernel32.OpenProcess
        open_process.argtypes = (wintypes.DWORD, wintypes.BOOL, wintypes.DWORD)
        open_process.restype = wintypes.HANDLE
        get_times = kernel32.GetProcessTimes
        get_times.argtypes = (wintypes.HANDLE, *([ctypes.POINTER(wintypes.FILETIME)] * 4))
        get_times.restype = wintypes.BOOL
        close_handle = kernel32.CloseHandle
        close_handle.argtypes = (wintypes.HANDLE,)
        close_handle.restype = wintypes.BOOL
        handle = open_process(0x1000, False, numeric)
        if not handle:
            return None
        try:
            created, exited, kernel, user = (wintypes.FILETIME() for _ in range(4))
            if not get_times(handle, ctypes.byref(created), ctypes.byref(exited), ctypes.byref(kernel), ctypes.byref(user)):
                return None
            ticks = (created.dwHighDateTime << 32) | created.dwLowDateTime
            return ticks / 10_000_000 - 11_644_473_600
        finally:
            close_handle(handle)
    except (OSError, ValueError, TypeError, IndexError, StopIteration):
        return None


def process_identity_matches(pid: object, *, expected_created_at: object = None,
                             existed_by: object = None) -> bool:
    """Reject proven PID reuse, retaining protection when identity is unknown.

    Old snapshots have no creation identity. A process born after their saved
    completion/acquisition time cannot be their worker. Unknown OS metadata or
    ambiguous older processes remain conservatively protected.
    """
    actual = process_creation_time(pid)
    if actual is None:
        return True
    if expected_created_at is not None:
        try:
            expected = float(expected_created_at)
            return abs(actual - expected) <= 1e-6 if math.isfinite(expected) else True
        except (TypeError, ValueError):
            return True
    if existed_by:
        try:
            boundary = datetime.fromisoformat(str(existed_by))
            if boundary.tzinfo is None:
                return True
            return actual <= boundary.astimezone(timezone.utc).timestamp() + 1e-6
        except (TypeError, ValueError):
            pass
    return True


def _windows_process_alive(pid: int) -> bool:
    """Query a Windows process without sending it a terminating signal."""

    import ctypes
    from ctypes import wintypes

    process_query_limited_information = 0x1000
    still_active = 259
    access_denied = 5

    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    open_process = kernel32.OpenProcess
    open_process.argtypes = (wintypes.DWORD, wintypes.BOOL, wintypes.DWORD)
    open_process.restype = wintypes.HANDLE
    get_exit_code = kernel32.GetExitCodeProcess
    get_exit_code.argtypes = (wintypes.HANDLE, ctypes.POINTER(wintypes.DWORD))
    get_exit_code.restype = wintypes.BOOL
    close_handle = kernel32.CloseHandle
    close_handle.argtypes = (wintypes.HANDLE,)
    close_handle.restype = wintypes.BOOL

    ctypes.set_last_error(0)
    handle = open_process(process_query_limited_information, False, pid)
    if not handle:
        # Access denied still proves that the process exists. Other failures,
        # notably ERROR_INVALID_PARAMETER for an absent PID, mean not alive.
        return ctypes.get_last_error() == access_denied
    try:
        exit_code = wintypes.DWORD()
        if not get_exit_code(handle, ctypes.byref(exit_code)):
            return True
        return exit_code.value == still_active
    finally:
        close_handle(handle)


def process_alive(pid: object) -> bool:
    """Return whether *pid* exists without changing the target process."""

    try:
        numeric = int(pid)
    except (TypeError, ValueError):
        return False
    if numeric <= 0:
        return False
    if os.name == "nt":
        return _windows_process_alive(numeric)
    try:
        os.kill(numeric, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True
