"""Process-level benchmarks over one immutable application source snapshot."""
import hashlib
from datetime import datetime, timezone
import math
import os
from pathlib import Path
import platform
import signal
import statistics
import subprocess
import tempfile
import threading
import time
import uuid


def measure(binary, content, entry, root, samples, warmup, timeout):
    """Measure complete native invocations; do not silently accept early exits."""
    if os.name != "posix":
        raise ValueError("bench currently requires POSIX process-group cleanup")
    if not 1 <= samples <= 10000 or not 0 <= warmup <= 10000:
        raise ValueError("samples must be 1..10000 and warmup 0..10000")
    if not math.isfinite(timeout) or not 0 < timeout <= 3600:
        raise ValueError("timeout must be finite and in (0, 3600] seconds")
    runtime_hash = hashlib.sha256(binary.read_bytes()).hexdigest()
    marker = f"NELISP_BENCH_{uuid.uuid4().hex}".encode("ascii")
    suffix = b'\n(' + entry.encode("ascii") + b')\n(princ "\\n' + marker + b'\\n")\n(exit 0)\n'
    durations = []
    outputs = []
    with tempfile.TemporaryDirectory(prefix="nelisp-bench-") as directory:
        bundle = Path(directory) / "program.el"
        bundle.write_bytes(b"(setq command-line-args-left nil)\n" + content + suffix)
        for index in range(warmup + samples):
            with tempfile.TemporaryFile() as stdout, tempfile.TemporaryFile() as stderr:
                armed = threading.Event()
                finished = threading.Event()
                expired = threading.Event()
                child = []

                def watchdog():
                    armed.wait()
                    if not finished.wait(timeout):
                        expired.set()
                        try:
                            os.killpg(child[0].pid, signal.SIGKILL)
                        except ProcessLookupError:
                            pass

                watcher = threading.Thread(target=watchdog, daemon=True)
                watcher.start()
                started = time.perf_counter_ns()
                try:
                    process = subprocess.Popen([str(binary), "--load", str(bundle)], cwd=root,
                                               stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                                               start_new_session=True)
                except BaseException:
                    finished.set()
                    armed.set()
                    watcher.join()
                    raise
                child.append(process)
                armed.set()
                try:
                    # wait(timeout=...) polls with growing sleeps on POSIX,
                    # which quantizes fast samples. Block here and enforce the
                    # deadline from a watchdog outside the measurement thread.
                    code = process.wait()
                    elapsed = time.perf_counter_ns() - started
                finally:
                    finished.set()
                    watcher.join()
                    # Finish the owned group before another sample; surviving
                    # workers would contaminate later measurements.
                    try:
                        os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    process.wait()
                if expired.is_set():
                    raise ValueError(f"bench run {index + 1}: timeout after {timeout:g}s")
                stdout.seek(0, 2)
                size = stdout.tell()
                stderr.seek(0, 2)
                errors = stderr.tell()
                if code or errors or size > 16 * 1024 * 1024:
                    stderr.seek(0)
                    detail = stderr.read(4096).decode("utf-8", errors="replace").strip()
                    raise ValueError(f"bench run {index + 1}: failed (exit={code}, stderr={errors} bytes, stdout={size} bytes)"
                                     + (f"\n{detail}" if detail else ""))
                stdout.seek(0)
                output = stdout.read()
                ending = b"\n" + marker + b"\n"
                if not output.endswith(ending) or output.count(marker) != 1:
                    raise ValueError(f"bench run {index + 1}: entry did not complete")
                if index >= warmup:
                    durations.append(elapsed)
                    outputs.append(hashlib.sha256(output[:-len(ending)]).hexdigest())
    if hashlib.sha256(binary.read_bytes()).hexdigest() != runtime_hash:
        raise ValueError("runtime changed during benchmark; results discarded")
    return {"schema_version": 1, "scope": "process-startup-load-entry-exit",
            "recorded_at": datetime.now(timezone.utc).isoformat(),
            "runtime": str(binary), "runtime_sha256": runtime_hash,
            "platform": platform.platform(), "machine": platform.machine(),
            "clock": "perf_counter_ns", "warmup_runs": warmup,
            "samples_ns": durations, "min_ns": min(durations),
            "median_ns": statistics.median(durations), "max_ns": max(durations),
            "stdout_sha256": outputs, "timeout_seconds": timeout,
            "bundle_sha256": hashlib.sha256(content).hexdigest(), "entry": entry}
