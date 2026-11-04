#!/usr/bin/env python3
"""Doc 240 D2: launch a saved NeLisp application image inside the lane sandbox.

Drop-in engine for the paired bench: `launch.py --load DRIVER_PLACEHOLDER'.
The placeholder is ignored; the row's saved image, its main file and manifest
come from NELISP_APP_IMAGE_DIR (as written by nelisp-application-image-produce.sh):

  DIR/<row>.img  DIR/<row>.img.main.el  DIR/<row>.img.manifest

Environment: NELISP_APP_IMAGE_DIR, NELISP_LANE_IMAGE (binary),
NELISP_LANE_IMAGE_SHA256, BENCH_ROW, BENCH_* (passed through),
NELISP_SCRATCH_ROOT (optional), NELISP_APP_LAUNCH_TIMING=1 (stage timings on
stderr), NELISP_APP_LAUNCH_SEAL=1 (hash while copying into a sealed memfd).

Trust: the manifest pins the binary and the image and main-file sha256.  The
image and main file are opened O_RDONLY once, hashed from those descriptors,
and the SAME inodes are handed to the binary as /dev/fd/N (inherited
descriptors, new procfs in the sandbox).  A rename or replacement of the path
after the open cannot change what the binary reads.  A writer with write
access could still modify the inode in place during the run; that is detected
afterwards (size/mtime/ctime change -> exit 2) but not prevented, unless
NELISP_APP_LAUNCH_SEAL=1 is set (sealed memfd, about 20 ms dearer).
Exit codes: 2 refusal (manifest/hash/env), 124 timeout, 125 launcher failure,
otherwise the child's code.
"""
import fcntl
import hashlib
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROWS = {
    "integer-sum", "vector-read", "vector-write", "vector-allocate64",
    "string-concat16", "string-slice16", "hash-get1024", "function-call2",
    "nested-function-call2",
}
FORMAT = "nelisp-app-image-v1"
HEX64 = re.compile(r"^[0-9a-f]{64}$")
KEYS = {"format", "image-sha256", "binary-sha256", "driver-sha256", "row",
        "main-sha256", "rows", "prep-sha256", "bootstrap-sha256"}
SHA_KEYS = ("image-sha256", "binary-sha256", "driver-sha256", "main-sha256", "prep-sha256")
ALLOWED_ENV = {"HOME", "USER", "LOGNAME", "PATH", "LANG", "LC_ALL", "LC_CTYPE",
               "NELISP_V8_ROOT", "NELISP_SCRATCH_ROOT", "NELISP_PRODUCER_IMAGE_SHA256",
               "NELISP_OWNED_SCRATCH", "NELISP_FXIR_ROOT"}
TIMEOUT = 600


class Refusal(Exception):
    """Policy refusal: exit 2 with a message."""


def parse_manifest(path):
    try:
        text = Path(path).read_text(encoding="utf-8")
    except OSError as exc:
        raise Refusal(f"cannot read manifest {path}: {exc}")
    m = {}
    for n, line in enumerate(text.split("\n"), 1):
        if line == "":
            continue
        key, sep, val = line.partition(" ")
        if not sep or not val or val != val.strip():
            raise Refusal(f"manifest line {n} malformed: {line!r}")
        if key not in KEYS:
            raise Refusal(f"manifest has unknown key {key!r}")
        if key in m:
            raise Refusal(f"manifest has duplicate key {key!r}")
        m[key] = val
    missing = sorted(KEYS - m.keys())
    if missing:
        raise Refusal(f"manifest is missing keys: {' '.join(missing)}")
    if m["format"] != FORMAT:
        raise Refusal(f"manifest format {m['format']!r}, expected {FORMAT!r}")
    for k in SHA_KEYS:
        if not HEX64.match(m[k]):
            raise Refusal(f"manifest {k} is not a sha256: {m[k]!r}")
    if m["bootstrap-sha256"] != "none" and not HEX64.match(m["bootstrap-sha256"]):
        raise Refusal("manifest bootstrap-sha256 is neither a sha256 nor 'none'")
    bad = [r for r in m["rows"].split(" ") if r not in ROWS]
    if bad or m["row"] not in ROWS or m["row"] not in m["rows"].split(" "):
        raise Refusal(f"manifest rows/row invalid: row={m['row']!r} rows={m['rows']!r}")
    return m


def open_and_hash(path, want, label, seal):
    """Open PATH read-only, hash from that descriptor, compare with WANT.

    Returns (fd handed to the child, stat of the source or None, bytes)."""
    try:
        fd = os.open(path, os.O_RDONLY | os.O_CLOEXEC)
    except OSError as exc:
        raise Refusal(f"cannot open {label} {path}: {exc}")
    st = os.fstat(fd)
    h = hashlib.sha256()
    out = None
    if seal:
        out = os.memfd_create(label, os.MFD_ALLOW_SEALING | os.MFD_CLOEXEC)
    while True:
        b = os.read(fd, 1 << 22)
        if not b:
            break
        h.update(b)
        if out is not None:
            view = memoryview(b)
            while view:
                view = view[os.write(out, view):]
    got = h.hexdigest()
    if got != want:
        raise Refusal(f"{label} sha256 {got} differs from manifest {want}")
    if out is not None:
        os.close(fd)
        fcntl.fcntl(out, fcntl.F_ADD_SEALS, fcntl.F_SEAL_WRITE | fcntl.F_SEAL_GROW
                    | fcntl.F_SEAL_SHRINK | fcntl.F_SEAL_SEAL)
        return out, None
    return fd, st


def same_state(fd, st):
    now = os.fstat(fd)
    return (now.st_size, now.st_mtime_ns, now.st_ctime_ns, now.st_ino) == \
           (st.st_size, st.st_mtime_ns, st.st_ctime_ns, st.st_ino)


def main():
    timing = os.environ.get("NELISP_APP_LAUNCH_TIMING") == "1"
    marks = [("start", time.perf_counter_ns())]

    def mark(name):
        marks.append((name, time.perf_counter_ns()))

    if len(sys.argv) != 3 or sys.argv[1] != "--load":
        print("usage: launch.py --load DRIVER_PLACEHOLDER", file=sys.stderr)
        return 2
    env = os.environ
    row = env.get("BENCH_ROW", "")
    if row not in ROWS:
        print(f"launch: invalid BENCH_ROW: {row!r}", file=sys.stderr)
        return 2
    seal = env.get("NELISP_APP_LAUNCH_SEAL") == "1"
    scratch = None
    proc = None
    fds = []
    try:
        for k in ("NELISP_APP_IMAGE_DIR", "NELISP_LANE_IMAGE", "NELISP_LANE_IMAGE_SHA256"):
            if not env.get(k):
                raise Refusal(f"{k} is required")
        if not HEX64.match(env["NELISP_LANE_IMAGE_SHA256"]):
            raise Refusal("NELISP_LANE_IMAGE_SHA256 is not a lowercase sha256")
        d = Path(env["NELISP_APP_IMAGE_DIR"])
        img_path, main_path = d / f"{row}.img", d / f"{row}.img.main.el"
        man = parse_manifest(d / f"{row}.img.manifest")
        if man["row"] != row:
            raise Refusal(f"manifest row {man['row']!r} differs from BENCH_ROW {row!r}")
        if man["binary-sha256"] != env["NELISP_LANE_IMAGE_SHA256"]:
            raise Refusal("manifest binary-sha256 differs from NELISP_LANE_IMAGE_SHA256")
        binary = Path(env["NELISP_LANE_IMAGE"]).resolve()
        if not binary.is_file():
            raise Refusal(f"binary not found: {binary}")
        mark("manifest")
        main_fd, main_st = open_and_hash(main_path, man["main-sha256"], "main file", seal)
        fds.append(main_fd)
        mark("hash-main")
        img_fd, img_st = open_and_hash(img_path, man["image-sha256"], "image", seal)
        fds.append(img_fd)
        mark("hash-image")

        scratch_root = Path(env.get("NELISP_SCRATCH_ROOT", tempfile.gettempdir())).resolve()
        scratch_root.mkdir(parents=True, exist_ok=True)
        scratch = Path(tempfile.mkdtemp(prefix="nelisp-exp-", dir=scratch_root))
        (scratch / "cache").mkdir()
        child_env = {k: v for k, v in env.items() if k in ALLOWED_ENV or k.startswith("BENCH_")}
        child_env.update({
            "NELISP_OWNED_SCRATCH": str(scratch), "TMPDIR": str(scratch),
            "XDG_CACHE_HOME": str(scratch / "cache"), "BENCH_ENGINE": "nelisp",
            "NELISP_PRODUCER_IMAGE_SHA256": env["NELISP_LANE_IMAGE_SHA256"],
        })
        home = env.get("HOME", str(Path.home()))
        cmd = [
            "bwrap", "--ro-bind", "/", "/", "--bind", str(scratch), str(scratch),
            "--proc", "/proc", "--dev", "/dev", "--unshare-user", "--unshare-pid",
            "--unshare-net", "--die-with-parent", "--new-session",
            "--setenv", "HOME", home,
            "--setenv", "TMPDIR", str(scratch), "--setenv", "XDG_CACHE_HOME", str(scratch / "cache"),
            "--", "/bin/bash", "-c", "ulimit -v 8388608; ulimit -c 0; exec \"$@\"", "lane",
            str(binary), "--cold-load-from", f"/dev/fd/{img_fd}", "--load", f"/dev/fd/{main_fd}",
        ]

        def terminate(signum, _frame):
            if proc is not None:
                try:
                    os.killpg(proc.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
            raise SystemExit(128 + signum)
        signal.signal(signal.SIGTERM, terminate)
        signal.signal(signal.SIGINT, terminate)
        mark("prepare")
        proc = subprocess.Popen(cmd, env=child_env, stdin=subprocess.DEVNULL,
                                start_new_session=True, pass_fds=[img_fd, main_fd])
        mark("spawn")
        try:
            rc = proc.wait(timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, signal.SIGTERM)
            try:
                proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait()
            print("launch: lane process timed out; process group terminated", file=sys.stderr)
            return 124
        mark("child")
        for fd, st, label in ((img_fd, img_st, "image"), (main_fd, main_st, "main file")):
            if st is not None and not same_state(fd, st):
                print(f"launch: {label} was modified in place while the child ran", file=sys.stderr)
                return 2
        return rc
    except Refusal as exc:
        print(f"launch: refused: {exc}", file=sys.stderr)
        return 2
    except Exception as exc:  # report, never swallow: nonzero exit with the cause
        print(f"launch: failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 125
    finally:
        if proc is not None and proc.poll() is None:
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            proc.wait()
        for fd in fds:
            os.close(fd)
        if scratch is not None:
            shutil.rmtree(scratch, ignore_errors=True)
        mark("cleanup")
        if timing:
            parts = " ".join(f"{b[0]}={(b[1] - a[1]) / 1e6:.2f}ms"
                             for a, b in zip(marks, marks[1:]))
            print(f"LAUNCH-TIMING {parts}", file=sys.stderr)


if __name__ == "__main__":
    raise SystemExit(main())
