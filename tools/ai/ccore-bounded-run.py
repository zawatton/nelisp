#!/usr/bin/env python3
"""Run a command with a bounded deadline and clean up its Linux process tree."""

import argparse
import ctypes
import errno
import json
import os
import signal
import subprocess
import sys
import time
from pathlib import Path


PR_SET_CHILD_SUBREAPER = 36
DEFAULT_TIMEOUT = 55.0
MAX_TIMEOUT = 55.0
POLL_INTERVAL = 0.02
CLEANUP_GRACE = 0.25
CLEANUP_LIMIT = 2.0


def proc_entry(pid):
    """Return (ppid, starttime, state), or None if the process vanished."""
    try:
        raw = Path("/proc", str(pid), "stat").read_bytes().decode("ascii", errors="replace")
        end = raw.rfind(")")
        fields = raw[end + 2 :].split()
        return int(fields[1]), int(fields[19]), fields[0]
    except (OSError, ValueError, IndexError):
        return None


def enable_subreaper():
    libc = ctypes.CDLL(None, use_errno=True)
    if libc.prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0:
        err = ctypes.get_errno()
        raise OSError(err, os.strerror(err))
    if not hasattr(os, "pidfd_open") or not hasattr(signal, "pidfd_send_signal"):
        raise RuntimeError("Linux pidfd_open and pidfd_send_signal are required")


class Tree:
    def __init__(self, root_pid):
        self.root_pid = root_pid
        self.self_pid = os.getpid()
        self.owned = {}
        self.errors = []
        self.discovery_complete = True
        initial_root = proc_entry(root_pid)
        if initial_root is None:
            raise RuntimeError("could not establish initial command process identity")
        self.root_starttime = initial_root[1]
        self.owned[root_pid] = self.root_starttime

    def scan(self):
        entries = {}
        try:
            pids = [int(p.name) for p in Path("/proc").iterdir() if p.name.isdigit()]
        except Exception as exc:
            self.discovery_complete = False
            self.errors.append(f"proc enumeration: {type(exc).__name__}: {exc}")
            return entries
        for pid in pids:
            try:
                raw = Path("/proc", str(pid), "stat").read_bytes().decode("ascii", errors="replace")
                end = raw.rfind(")")
                fields = raw[end + 2 :].split()
                entries[pid] = (int(fields[1]), int(fields[19]), fields[0])
            except (FileNotFoundError, ProcessLookupError):
                continue
            except Exception as exc:
                self.discovery_complete = False
                self.errors.append(f"proc stat {pid}: {type(exc).__name__}: {exc}")

        changed = True
        while changed:
            changed = False
            for pid, (ppid, started, _state) in entries.items():
                if pid == self.self_pid:
                    continue
                if pid == self.root_pid:
                    if started != self.root_starttime:
                        self.owned.pop(pid, None)
                    continue
                if pid in self.owned and self.owned[pid] != started:
                    del self.owned[pid]
                if pid not in self.owned and (
                    ppid == self.self_pid
                    or (ppid in self.owned and self.owned[ppid] == entries.get(ppid, (None, None, None))[1])
                ):
                    self.owned[pid] = started
                    changed = True
        return entries

    def signal_one(self, pid, started, sig):
        before = proc_entry(pid)
        if not before or before[1] != started:
            return False
        try:
            fd = os.pidfd_open(pid, 0)
        except ProcessLookupError:
            return False
        except OSError as exc:
            if exc.errno in (errno.ESRCH, errno.ENOENT):
                return False
            raise
        try:
            after = proc_entry(pid)
            if not after or after[1] != started:
                return False
            try:
                signal.pidfd_send_signal(fd, sig)
                return True
            except ProcessLookupError:
                return False
        finally:
            os.close(fd)

    def signal_known(self, sig):
        for pid, started in list(self.owned.items()):
            try:
                self.signal_one(pid, started, sig)
            except Exception as exc:
                self.errors.append(f"signal {pid}: {type(exc).__name__}: {exc}")

    def reap_adopted(self):
        for pid in list(self.owned):
            if pid == self.root_pid:
                continue
            try:
                os.waitpid(pid, os.WNOHANG)
            except (ChildProcessError, ProcessLookupError):
                pass
            except Exception as exc:
                self.errors.append(f"waitpid {pid}: {type(exc).__name__}: {exc}")

    def snapshot_survivors(self, entries):
        return [
            {"pid": pid, "starttime": started, "state": entries[pid][2]}
            for pid, started in self.owned.items()
            if pid in entries and entries[pid][1] == started
        ]


def _cleanup_tree_impl(tree, process, mode):
    if tree is None:
        if process is not None and process.poll() is None:
            try:
                process.kill()
            except OSError:
                pass
            try:
                process.wait(timeout=CLEANUP_LIMIT)
            except (OSError, subprocess.TimeoutExpired):
                pass
        alive = process is not None and process.poll() is None
        return [], ["process tree identity unavailable"], False

    entries = {}
    if mode != "normal":
        tree.signal_known(signal.SIGKILL)
    try:
        entries = tree.scan()
    except BaseException as exc:
        tree.discovery_complete = False
        tree.errors.append(f"discovery: {type(exc).__name__}: {exc}")
    if mode == "normal":
        tree.signal_known(signal.SIGTERM)
    else:
        tree.signal_known(signal.SIGKILL)

    started = time.monotonic()
    kill_at = started + (CLEANUP_GRACE if mode == "normal" else 0)
    end = started + CLEANUP_LIMIT
    survivors = tree.snapshot_survivors(entries)
    while time.monotonic() < end:
        if process is not None:
            try:
                process.poll()
            except Exception as exc:
                tree.errors.append(f"root poll: {type(exc).__name__}: {exc}")
                try:
                    process.kill()
                except Exception as kill_exc:
                    tree.errors.append(f"root fallback kill: {type(kill_exc).__name__}: {kill_exc}")
        try:
            entries = tree.scan()
        except BaseException as exc:
            tree.discovery_complete = False
            tree.errors.append(f"discovery: {type(exc).__name__}: {exc}")
        try:
            tree.reap_adopted()
        except BaseException as exc:
            tree.errors.append(f"reaping: {type(exc).__name__}: {exc}")
        survivors = tree.snapshot_survivors(entries)
        try:
            root_alive = process is not None and process.poll() is None
        except Exception as exc:
            tree.errors.append(f"root poll: {type(exc).__name__}: {exc}")
            root_alive = True
            try:
                process.kill()
            except Exception as kill_exc:
                tree.errors.append(f"root fallback kill: {type(kill_exc).__name__}: {kill_exc}")
        if not survivors and not root_alive:
            break
        if time.monotonic() >= kill_at:
            tree.signal_known(signal.SIGKILL)
        time.sleep(POLL_INTERVAL)

    try:
        entries = tree.scan()
    except BaseException as exc:
        tree.discovery_complete = False
        tree.errors.append(f"final discovery: {type(exc).__name__}: {exc}")
    tree.signal_known(signal.SIGKILL)
    try:
        tree.reap_adopted()
    except BaseException as exc:
        tree.errors.append(f"final reaping: {type(exc).__name__}: {exc}")
    survivors = tree.snapshot_survivors(entries)
    try:
        root_alive = process is not None and process.poll() is None
    except Exception as exc:
        tree.errors.append(f"final root poll: {type(exc).__name__}: {exc}")
        root_alive = True
    if root_alive:
        try:
            process.wait(timeout=0.1)
        except (OSError, subprocess.TimeoutExpired) as exc:
            tree.errors.append(f"root wait: {type(exc).__name__}: {exc}")
    return survivors, list(tree.errors), tree.discovery_complete and not survivors and not root_alive and not tree.errors


def cleanup_tree(tree, process, mode):
    """Bound cleanup failures and return receipt-ready cached process state."""
    try:
        return _cleanup_tree_impl(tree, process, mode)
    except BaseException as exc:
        errors = [f"cleanup: {type(exc).__name__}: {exc}"]
        if tree is not None:
            tree.discovery_complete = False
            tree.errors.extend(errors)
            tree.signal_known(signal.SIGKILL)
            try:
                entries = tree.scan()
            except BaseException as scan_exc:
                tree.errors.append(f"fallback discovery: {type(scan_exc).__name__}: {scan_exc}")
                entries = {}
            try:
                tree.reap_adopted()
            except BaseException as reap_exc:
                tree.errors.append(f"fallback reaping: {type(reap_exc).__name__}: {reap_exc}")
            return tree.snapshot_survivors(entries), list(tree.errors), False
        if process is not None:
            try:
                process.kill()
            except BaseException as kill_exc:
                errors.append(f"fallback root kill: {type(kill_exc).__name__}: {kill_exc}")
            try:
                process.wait(timeout=CLEANUP_LIMIT)
            except BaseException as wait_exc:
                errors.append(f"fallback root wait: {type(wait_exc).__name__}: {wait_exc}")
        return [], errors, False


def write_receipt(path, data):
    tmp = path.with_suffix(".json.tmp")
    tmp.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
    os.replace(tmp, path)


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--cwd", type=Path)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv)
    if not 0 < args.timeout <= MAX_TIMEOUT:
        parser.error("--timeout must be greater than 0 and at most 55 seconds")
    if args.command and args.command[0] == "--":
        args.command = args.command[1:]
    if not args.command:
        parser.error("a command is required after --")
    return args


def main(argv=None):
    args = parse_args(sys.argv[1:] if argv is None else argv)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    deadline = started + args.timeout
    out_path = args.output_dir / "stdout"
    err_path = args.output_dir / "stderr"
    receipt_path = args.output_dir / "receipt.json"
    caught = {"signal": None}
    old_handlers = {}
    for sig in (signal.SIGINT, signal.SIGTERM):
        old_handlers[sig] = signal.signal(sig, lambda number, _frame: caught.update(signal=number))

    process = None
    tree = None
    timed_out = False
    command_rc = None
    original_error = None
    mode = "normal"
    survivors = []
    cleanup_errors = []
    cleanup_ok = False
    try:
        enable_subreaper()
        with out_path.open("wb") as stdout, err_path.open("wb") as stderr:
            process = subprocess.Popen(
                args.command,
                cwd=args.cwd,
                stdin=subprocess.DEVNULL,
                stdout=stdout,
                stderr=stderr,
                start_new_session=True,
                close_fds=True,
            )
            tree = Tree(process.pid)
            while True:
                command_rc = process.poll()
                if command_rc is not None:
                    break
                if caught["signal"]:
                    mode = "immediate"
                    break
                if time.monotonic() >= deadline:
                    timed_out = True
                    mode = "immediate"
                    break
                time.sleep(POLL_INTERVAL)
    except BaseException as exc:
        original_error = exc
        mode = "immediate"
    finally:
        try:
            survivors, cleanup_errors, cleanup_ok = cleanup_tree(tree, process, mode)
        except BaseException as cleanup_exc:
            cleanup_errors.append(f"cleanup: {type(cleanup_exc).__name__}: {cleanup_exc}")
            cleanup_ok = False
        for sig, handler in old_handlers.items():
            signal.signal(sig, handler)

    if process is not None:
        command_rc = process.returncode
    result = {
        "command": args.command,
        "command_rc": command_rc,
        "timed_out": timed_out,
        "duration_seconds": round(time.monotonic() - started, 6),
        "owned_processes": (
            [{"pid": p, "starttime": s} for p, s in sorted(tree.owned.items())]
            if tree
            else []
        ),
        "cleanup_verified": cleanup_ok,
        "survivors": survivors,
        "cleanup_errors": cleanup_errors,
        "signal": caught["signal"],
    }
    if original_error is not None:
        result["error"] = f"{type(original_error).__name__}: {original_error}"
    try:
        write_receipt(receipt_path, result)
    except BaseException:
        if original_error is not None:
            raise original_error
        raise
    if original_error is not None:
        raise original_error
    if caught["signal"]:
        return 128 + caught["signal"]
    if timed_out:
        return 124
    return command_rc if command_rc is not None else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as exc:
        print(str(exc), file=sys.stderr)
        raise SystemExit(1)
