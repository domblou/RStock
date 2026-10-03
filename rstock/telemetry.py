"""Lightweight process telemetry without a mandatory third-party dependency."""

from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class ProcessSample:
    pid: int
    identity: int
    cpu_seconds: float
    rss_bytes: int
    thread_count: int | None


def available_logical_processors() -> int | None:
    """Return CPUs available to this process, respecting a basic affinity mask."""
    if hasattr(os, "sched_getaffinity"):
        try:
            return len(os.sched_getaffinity(0))
        except OSError:
            pass
    if sys.platform == "win32":
        try:
            import ctypes
            from ctypes import wintypes

            kernel = ctypes.WinDLL("kernel32", use_last_error=True)
            kernel.GetCurrentProcess.restype = wintypes.HANDLE
            kernel.GetProcessAffinityMask.argtypes = [
                wintypes.HANDLE, ctypes.POINTER(ctypes.c_size_t),
                ctypes.POINTER(ctypes.c_size_t),
            ]
            kernel.GetProcessAffinityMask.restype = wintypes.BOOL
            process_mask = ctypes.c_size_t()
            system_mask = ctypes.c_size_t()
            if kernel.GetProcessAffinityMask(
                kernel.GetCurrentProcess(), ctypes.byref(process_mask),
                ctypes.byref(system_mask),
            ):
                count = process_mask.value.bit_count()
                return count if count else None
        except (AttributeError, OSError, TypeError):
            pass
    return os.cpu_count()


def _windows_process_sample(pid: int) -> ProcessSample | None:
    import ctypes
    from ctypes import wintypes

    class MemoryCounters(ctypes.Structure):
        _fields_ = [
            ("cb", wintypes.DWORD), ("PageFaultCount", wintypes.DWORD),
            ("PeakWorkingSetSize", ctypes.c_size_t),
            ("WorkingSetSize", ctypes.c_size_t),
            ("QuotaPeakPagedPoolUsage", ctypes.c_size_t),
            ("QuotaPagedPoolUsage", ctypes.c_size_t),
            ("QuotaPeakNonPagedPoolUsage", ctypes.c_size_t),
            ("QuotaNonPagedPoolUsage", ctypes.c_size_t),
            ("PagefileUsage", ctypes.c_size_t),
            ("PeakPagefileUsage", ctypes.c_size_t),
        ]

    kernel = ctypes.WinDLL("kernel32", use_last_error=True)
    psapi = ctypes.WinDLL("psapi", use_last_error=True)
    kernel.OpenProcess.argtypes = [wintypes.DWORD, wintypes.BOOL, wintypes.DWORD]
    kernel.OpenProcess.restype = wintypes.HANDLE
    kernel.CloseHandle.argtypes = [wintypes.HANDLE]
    kernel.GetProcessTimes.argtypes = [
        wintypes.HANDLE, ctypes.POINTER(wintypes.FILETIME),
        ctypes.POINTER(wintypes.FILETIME), ctypes.POINTER(wintypes.FILETIME),
        ctypes.POINTER(wintypes.FILETIME),
    ]
    kernel.GetProcessTimes.restype = wintypes.BOOL
    psapi.GetProcessMemoryInfo.argtypes = [
        wintypes.HANDLE, ctypes.POINTER(MemoryCounters), wintypes.DWORD,
    ]
    psapi.GetProcessMemoryInfo.restype = wintypes.BOOL
    handle = kernel.OpenProcess(0x0400 | 0x0010, False, pid)
    if not handle:
        return None
    try:
        counters = MemoryCounters()
        counters.cb = ctypes.sizeof(counters)
        created, exited, kernel_time, user_time = (wintypes.FILETIME() for _ in range(4))
        if not psapi.GetProcessMemoryInfo(handle, ctypes.byref(counters), counters.cb):
            return None
        if not kernel.GetProcessTimes(
            handle, ctypes.byref(created), ctypes.byref(exited),
            ctypes.byref(kernel_time), ctypes.byref(user_time),
        ):
            return None

        def ticks(value: wintypes.FILETIME) -> int:
            return (int(value.dwHighDateTime) << 32) | int(value.dwLowDateTime)

        return ProcessSample(
            pid, ticks(created),
            (ticks(kernel_time) + ticks(user_time)) / 10_000_000,
            int(counters.WorkingSetSize), None,
        )
    finally:
        kernel.CloseHandle(handle)


def process_sample(pid: int | None = None) -> ProcessSample | None:
    """Read real CPU and RSS counters, or report that they are unavailable."""
    pid = os.getpid() if pid is None else pid
    if sys.platform == "win32":
        try:
            return _windows_process_sample(pid)
        except (AttributeError, OSError, TypeError):
            return None
    try:
        stat = Path(f"/proc/{pid}/stat").read_text(encoding="ascii")
        fields = stat[stat.rfind(")") + 2:].split()
        ticks_per_second = os.sysconf("SC_CLK_TCK")
        pages = Path(f"/proc/{pid}/statm").read_text(encoding="ascii").split()
        return ProcessSample(
            pid, int(fields[19]),
            (int(fields[11]) + int(fields[12])) / ticks_per_second,
            int(pages[1]) * os.sysconf("SC_PAGE_SIZE"), int(fields[17]),
        )
    except (FileNotFoundError, OSError, ValueError, IndexError, AttributeError):
        return None


def descendant_pids(root_pid: int) -> set[int]:
    """Return current descendants without requiring WMI or a Python package."""
    parents: dict[int, int] = {}
    if sys.platform == "win32":
        try:
            import ctypes
            from ctypes import wintypes

            class ProcessEntry(ctypes.Structure):
                _fields_ = [
                    ("dwSize", wintypes.DWORD), ("cntUsage", wintypes.DWORD),
                    ("th32ProcessID", wintypes.DWORD),
                    ("th32DefaultHeapID", ctypes.c_size_t),
                    ("th32ModuleID", wintypes.DWORD), ("cntThreads", wintypes.DWORD),
                    ("th32ParentProcessID", wintypes.DWORD),
                    ("pcPriClassBase", wintypes.LONG), ("dwFlags", wintypes.DWORD),
                    ("szExeFile", wintypes.WCHAR * 260),
                ]

            kernel = ctypes.WinDLL("kernel32", use_last_error=True)
            kernel.CreateToolhelp32Snapshot.argtypes = [wintypes.DWORD, wintypes.DWORD]
            kernel.CreateToolhelp32Snapshot.restype = wintypes.HANDLE
            kernel.Process32FirstW.argtypes = [wintypes.HANDLE, ctypes.POINTER(ProcessEntry)]
            kernel.Process32FirstW.restype = wintypes.BOOL
            kernel.Process32NextW.argtypes = [wintypes.HANDLE, ctypes.POINTER(ProcessEntry)]
            kernel.Process32NextW.restype = wintypes.BOOL
            kernel.CloseHandle.argtypes = [wintypes.HANDLE]
            handle = kernel.CreateToolhelp32Snapshot(0x00000002, 0)
            if handle == ctypes.c_void_p(-1).value:
                return set()
            try:
                entry = ProcessEntry()
                entry.dwSize = ctypes.sizeof(entry)
                found = kernel.Process32FirstW(handle, ctypes.byref(entry))
                while found:
                    parents[int(entry.th32ProcessID)] = int(entry.th32ParentProcessID)
                    found = kernel.Process32NextW(handle, ctypes.byref(entry))
            finally:
                kernel.CloseHandle(handle)
        except (AttributeError, OSError, TypeError):
            return set()
    else:
        try:
            for name in os.listdir("/proc"):
                if not name.isdigit():
                    continue
                try:
                    with open(f"/proc/{name}/stat", encoding="ascii") as stream:
                        stat = stream.read()
                    parents[int(name)] = int(stat[stat.rfind(")") + 2:].split()[1])
                except (FileNotFoundError, PermissionError, ValueError, IndexError):
                    continue
        except OSError:
            return set()
    descendants: set[int] = set()
    frontier = {root_pid}
    while frontier:
        children = {pid for pid, parent in parents.items() if parent in frontier}
        children -= descendants
        children.discard(root_pid)
        descendants.update(children)
        frontier = children
    return descendants


def process_rss_bytes() -> int | None:
    """Return the current process RSS when the platform exposes it cheaply."""

    if sys.platform == "win32":
        sample = process_sample()
        return None if sample is None else sample.rss_bytes

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
