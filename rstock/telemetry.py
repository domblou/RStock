"""Lightweight process telemetry without a mandatory third-party dependency."""

from __future__ import annotations

import os
import sys


def process_rss_bytes() -> int | None:
    """Return the current process RSS when the platform exposes it cheaply."""

    if sys.platform == "win32":
        try:
            import ctypes
            from ctypes import wintypes

            class _MemoryCounters(ctypes.Structure):
                _fields_ = [
                    ("cb", wintypes.DWORD),
                    ("PageFaultCount", wintypes.DWORD),
                    ("PeakWorkingSetSize", ctypes.c_size_t),
                    ("WorkingSetSize", ctypes.c_size_t),
                    ("QuotaPeakPagedPoolUsage", ctypes.c_size_t),
                    ("QuotaPagedPoolUsage", ctypes.c_size_t),
                    ("QuotaPeakNonPagedPoolUsage", ctypes.c_size_t),
                    ("QuotaNonPagedPoolUsage", ctypes.c_size_t),
                    ("PagefileUsage", ctypes.c_size_t),
                    ("PeakPagefileUsage", ctypes.c_size_t),
                ]

            counters = _MemoryCounters()
            counters.cb = ctypes.sizeof(counters)
            handle = ctypes.windll.kernel32.GetCurrentProcess()
            if ctypes.windll.psapi.GetProcessMemoryInfo(
                handle, ctypes.byref(counters), counters.cb
            ):
                return int(counters.WorkingSetSize)
        except (AttributeError, OSError, TypeError):
            return None
        return None

    try:
        with open(f"/proc/{os.getpid()}/statm", encoding="ascii") as stream:
            resident_pages = int(stream.read().split()[1])
        return resident_pages * int(os.sysconf("SC_PAGE_SIZE"))
    except (FileNotFoundError, OSError, ValueError, IndexError, AttributeError):
        return None


def dataframe_bytes(frame: object) -> int | None:
    """Return a deep pandas memory estimate without importing pandas here."""

    try:
        return int(frame.memory_usage(index=True, deep=True).sum())  # type: ignore[attr-defined]
    except (AttributeError, TypeError, ValueError):
        return None
