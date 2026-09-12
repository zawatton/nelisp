#!/usr/bin/env python3
"""Exercise the standalone NeLisp GC through one persistent REPL process."""
import argparse
import ctypes
import ctypes.util
import hashlib
import json
import math
import os
import selectors
import struct
import subprocess
import sys
import time
from pathlib import Path


# PROC_PIDTASKINFO; struct proc_taskinfo begins with two uint64 fields,
# pti_virtual_size then pti_resident_size.  sys/proc_info.h.
_PROC_PIDTASKINFO = 4
_PROC_TASKINFO_BYTES = 256
_LIBPROC = None


def _libproc():
    """Return the cached libproc handle, or False when it cannot be loaded."""
    global _LIBPROC
    if _LIBPROC is None:
        try:
            _LIBPROC = ctypes.CDLL(
                ctypes.util.find_library("proc") or "/usr/lib/libproc.dylib")
        except OSError:
            _LIBPROC = False
    return _LIBPROC


def _rss_kib_darwin(pid):
    """Resident set size in KiB on Darwin, or 0 when it cannot be read.

    Darwin has no procfs, so there is no VmRSS to read.  libproc's
    PROC_PIDTASKINFO carries the same number as pti_resident_size and needs
    no subprocess -- which matters at one sample per second for an hour.
    Cross-checked against `ps -o rss=' on macos 26.6.2 arm64: both reported
    65184 KiB for the same pid at the same moment, so the `ps' fallback
    below is an equivalent second source rather than a different metric.
    """
    lib = _libproc()
    if lib:
        buf = ctypes.create_string_buffer(_PROC_TASKINFO_BYTES)
        try:
            filled = lib.proc_pidinfo(pid, _PROC_PIDTASKINFO, 0, buf,
                                      _PROC_TASKINFO_BYTES)
        except (OSError, ctypes.ArgumentError):
            filled = 0
        if filled >= 16:
            _virtual, resident = struct.unpack_from("=QQ", buf.raw, 0)
            return resident // 1024
    try:
        probe = subprocess.run(["/bin/ps", "-o", "rss=", "-p", str(pid)],
                               capture_output=True, text=True, timeout=10)
        text = probe.stdout.strip()
        return int(text) if text.isdigit() else 0
    except (OSError, ValueError, subprocess.SubprocessError):
        return 0


def rss_kib(pid):
    """Resident set size in KiB, or 0 when it cannot be read.

    0 is what `sample_rss' turns into `FAIL: RSS is unavailable', so an
    unsupported host fails loudly rather than soaking against a metric it
    never actually sampled.  That is exactly what happened on macOS before
    this branch existed: the Linux-only /proc read returned 0, the very
    first `sample_rss' -- the one taken BEFORE the timing loop starts --
    raised, and the run reported batches=0 elapsed_seconds=0.000 for a
    requested --duration 3600.  A soak that exercised nothing is neither a
    pass nor a leak; it is no measurement at all.  Measured 2026-09-12 on
    macos 26.6.2 arm64.
    """
    if sys.platform == "darwin":
        return _rss_kib_darwin(pid)
    try:
        with open(f"/proc/{pid}/status", encoding="ascii") as stream:
            for line in stream:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1])
    except (FileNotFoundError, ProcessLookupError, ValueError):
        return 0
    return 0


def smaps_rollup_kib(pid):
    """Return selected Linux smaps_rollup KiB fields, or None if unavailable.

    Darwin has no smaps_rollup and no transparent huge pages, so there is
    nothing to map `AnonHugePages' onto; None is the honest answer there and
    the caller already prints `smaps_rollup=unavailable' for it.  The
    Darwin-side equivalent of the dirty/swapped breakdown is captured as
    `vmmap -summary' text by `save_diagnostic' instead.
    """
    wanted = ("Rss", "Anonymous", "AnonHugePages", "Private_Dirty")
    values = {}
    try:
        with open(f"/proc/{pid}/smaps_rollup", encoding="ascii") as stream:
            for line in stream:
                name, _, rest = line.partition(":")
                if name in wanted:
                    fields = rest.split()
                    if not fields or not fields[0].isdigit():
                        return None
                    values[name] = int(fields[0])
    except (OSError, ValueError):
        return None
    if any(name not in values for name in wanted):
        return None
    return values


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default=str(Path(__file__).resolve().parents[1] / "target/nelisp"))
    parser.add_argument("--duration", type=float, default=3600.0)
    parser.add_argument("--batch-interval", type=float, default=1.0)
    parser.add_argument("--batch-size", type=int, default=5000)
    parser.add_argument("--timeout", type=float, default=10.0)
    parser.add_argument("--rss-growth-ceiling-kib", type=int, default=5120)
    parser.add_argument("--rss-ceiling-kib", type=int, default=524288)
    parser.add_argument("--diagnostic-dir", type=Path,
                        help="save child smaps and metrics snapshots here")
    args = parser.parse_args()
    numeric = (args.duration, args.batch_interval, args.timeout,
               args.rss_growth_ceiling_kib, args.rss_ceiling_kib)
    if (any(not math.isfinite(value) for value in numeric)
            or args.duration <= 0 or args.batch_interval < 0 or args.timeout <= 0
            or args.batch_size <= 0 or args.rss_growth_ceiling_kib < 0
            or args.rss_ceiling_kib <= 0):
        parser.error("finite positive duration/timeout/size/absolute ceiling required; interval and growth ceiling must be nonnegative")
    binary = os.path.abspath(args.binary)
    if not os.path.isfile(binary) or not os.access(binary, os.X_OK):
        print(f"FAIL: standalone binary is not executable: {binary}", file=sys.stderr)
        return 2
    digest = hashlib.sha256()
    with open(binary, "rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    print(f"binary={binary} sha256={digest.hexdigest()}")
    command = [binary, "--repl", "--no-prompt", "--no-print"]
    child = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, bufsize=0)
    selector = selectors.DefaultSelector()
    selector.register(child.stdout.fileno(), selectors.EVENT_READ, "stdout")
    selector.register(child.stderr.fileno(), selectors.EVENT_READ, "stderr")
    os.set_blocking(child.stdout.fileno(), False)
    os.set_blocking(child.stderr.fileno(), False)
    stdout_buffer = bytearray()
    start_rss = None
    last_rss = None
    peak_rss = None
    started = None
    batches = 0

    def save_diagnostic(label):
        """Best-effort full smaps/metrics capture; never changes the result."""
        if args.diagnostic_dir is None or child.poll() is not None:
            return
        try:
            args.diagnostic_dir.mkdir(parents=True, exist_ok=True)
            if sys.platform == "darwin":
                # No smaps on Darwin.  `vmmap -summary' is the nearest thing
                # and, unlike RSS, it does show the compressor: it reports
                # "Physical footprint" plus DIRTY/SWAPPED columns, so a leak
                # that macOS compresses instead of keeping resident is still
                # visible here.  Diagnostic only -- the pass/fail metric
                # stays resident-set growth, so the ceiling keeps the same
                # meaning it has on Linux.
                probe = subprocess.run(
                    ["/usr/bin/vmmap", "-summary", str(child.pid)],
                    capture_output=True, text=True, timeout=60)
                (args.diagnostic_dir / f"{label}.vmmap").write_text(
                    probe.stdout, encoding="utf-8", errors="replace")
            else:
                with open(f"/proc/{child.pid}/smaps", encoding="ascii") as stream:
                    smaps_text = stream.read()
                (args.diagnostic_dir / f"{label}.smaps").write_text(
                    smaps_text, encoding="ascii")
            metrics = {
                "label": label, "pid": child.pid, "start_rss_kib": start_rss,
                "current_rss_kib": last_rss, "peak_rss_kib": peak_rss,
                "batches": batches,
                "elapsed_seconds": ((time.monotonic() - started)
                                     if started is not None else 0.0),
            }
            (args.diagnostic_dir / f"{label}.json").write_text(
                json.dumps(metrics, sort_keys=True) + "\n", encoding="ascii")
        except (OSError, ValueError, TypeError, subprocess.SubprocessError):
            return

    def sample_rss():
        nonlocal last_rss
        value = rss_kib(child.pid)
        last_rss = value
        if value <= 0:
            raise RuntimeError("RSS is unavailable")
        if value > args.rss_ceiling_kib:
            raise RuntimeError("absolute RSS ceiling exceeded")
        return value

    def read_chunks(wait):
        for key, _ in selector.select(wait):
            try:
                chunk = os.read(key.fd, 4096)
            except BlockingIOError:
                continue
            if not chunk:
                selector.unregister(key.fd)
                continue
            if key.data == "stderr":
                raise RuntimeError("standalone REPL wrote to stderr: "
                                   + chunk.decode("utf-8", "replace"))
            stdout_buffer.extend(chunk)
            if len(stdout_buffer) > 4096:
                raise RuntimeError("REPL output exceeded 4096-byte response budget")

    def send(form):
        child.stdin.write((form + "\n").encode("utf-8"))
        child.stdin.flush()

    def read_marker(expected, timeout):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if child.poll() is not None:
                raise RuntimeError(f"REPL child died with status {child.returncode}")
            read_chunks(max(0, deadline - time.monotonic()))
            if b"\n" in stdout_buffer:
                raw, _, rest = stdout_buffer.partition(b"\n")
                stdout_buffer[:] = rest
                if raw.decode("utf-8", "replace") != expected:
                    raise RuntimeError("unexpected REPL response: " + repr(bytes(raw)))
                read_chunks(0)
                if stdout_buffer:
                    raise RuntimeError("unexpected trailing REPL output")
                return
        raise RuntimeError(f"timeout waiting for marker {expected}")

    def allocation_batch(marker):
        # Sentinel is initialized once, outside all batches. Check live payload
        # AFTER collection, then release the workload and collect once more.
        send("(progn (setq nelisp-soak-junk nil nelisp-soak-i 0) "
             f"(while (< nelisp-soak-i {args.batch_size}) "
             "(setq nelisp-soak-junk (cons (make-string 256 65) nelisp-soak-junk) "
             "nelisp-soak-i (1+ nelisp-soak-i))) (garbage-collect) "
             "(setq nelisp-soak-count (length nelisp-soak-junk)) "
             f"(unless (= nelisp-soak-count {args.batch_size}) (error \"batch length\")) "
             "(unless (and (equal (car nelisp-soak-junk) (make-string 256 65)) "
             "(equal (car (last nelisp-soak-junk)) (make-string 256 65)) "
             "(equal nelisp-soak-sentinel '(\"retained\" [17 29]))) "
             "(error \"post-GC batch value\")) "
             "(setq nelisp-soak-junk nil) (garbage-collect) "
             "(unless (equal nelisp-soak-sentinel '(\"retained\" [17 29])) "
             "(error \"released-batch sentinel\")) "
             f'(nelisp--write-stdout-bytes (format "{marker}_%d\\n" nelisp-soak-count)))')
        read_marker(f"{marker}_{args.batch_size}", args.timeout)
        return sample_rss()

    try:
        send('(progn (setq nelisp-soak-sentinel (list "retained" (vector 17 29))) '
             '(nelisp--write-stdout-bytes "NELISP_SOAK_READY\\n"))')
        read_marker("NELISP_SOAK_READY", args.timeout)
        sample_rss()
        # Establish the arena after normal startup allocation, then measure
        # growth caused by the sustained workload.
        for warmup in range(2):
            allocation_batch(f"NELISP_SOAK_WARMUP_{warmup}")
        start_rss = sample_rss()
        peak_rss = start_rss
        started = time.monotonic()
        batches = 0
        save_diagnostic("baseline")
        while time.monotonic() - started < args.duration:
            current_rss = allocation_batch(f"NELISP_SOAK_BATCH_{batches}")
            batches += 1
            peak_rss = max(peak_rss, current_rss)
            if peak_rss > start_rss + args.rss_growth_ceiling_kib:
                raise RuntimeError("RSS growth ceiling exceeded")
            if args.batch_interval:
                time.sleep(min(args.batch_interval, max(0, args.duration - (time.monotonic() - started))))
        if batches == 0:
            raise RuntimeError("zero completed allocation batches")
        save_diagnostic("final")
        print(f"batches={batches} start_rss_kib={start_rss} sampled_peak_rss_kib={peak_rss} elapsed_seconds={time.monotonic() - started:.3f}")
        if child.poll() is not None:
            raise RuntimeError(f"REPL child exited with status {child.returncode}")
        send("(exit)")
        child.stdin.close()
        exit_deadline = time.monotonic() + args.timeout
        while child.poll() is None and time.monotonic() < exit_deadline:
            read_chunks(0.05)
        read_chunks(0)
        if child.poll() != 0 or stdout_buffer:
            raise RuntimeError(f"unclean REPL exit: {child.poll()}")
        print("nelisp-standalone-soak: PASS")
        return 0
    except (OSError, RuntimeError) as error:
        save_diagnostic("failure")
        smaps = (smaps_rollup_kib(child.pid)
                 if child.poll() is None else None)
        smaps_text = ("unavailable" if smaps is None
                      else " ".join(f"{name}_kib={smaps[name]}"
                                   for name in ("Rss", "Anonymous",
                                                "AnonHugePages", "Private_Dirty")))
        elapsed = (time.monotonic() - started) if started is not None else 0.0
        growth = (peak_rss - start_rss
                  if peak_rss is not None and start_rss is not None else None)
        print(
            "FAIL: "
            f"{error}; start_rss_kib={start_rss!r} "
            f"current_rss_kib={last_rss!r} peak_rss_kib={peak_rss!r} "
            f"rss_growth_kib={growth!r} "
            f"growth_ceiling_kib={args.rss_growth_ceiling_kib} "
            f"absolute_ceiling_kib={args.rss_ceiling_kib} batches={batches} "
            f"elapsed_seconds={elapsed:.3f} smaps_rollup={smaps_text}",
            file=sys.stderr,
        )
        return 1
    finally:
        try:
            if not child.stdin.closed:
                child.stdin.write(b"(exit)\n")
                child.stdin.close()
        except (OSError, ValueError):
            pass
        try:
            child.wait(timeout=2)
        except subprocess.TimeoutExpired:
            try:
                child.terminate()
                child.wait(timeout=2)
            except subprocess.TimeoutExpired:
                child.kill()
                child.wait()
            except OSError:
                pass
        selector.close()


if __name__ == "__main__":
    sys.exit(main())
