#!/usr/bin/env python3
"""Run one experimental NeLisp lane in an owned, read-only-root sandbox.

PHASED variant (Doc 235).  Without `--phase-out DIR' it behaves exactly like
the original runner.  With it, the ON lane runs the phased driver, the wrapper
records SPAWN / CHILD_EXIT / WRAPPER_CLEANUP_COMPLETE with time.monotonic_ns
(CLOCK_MONOTONIC), the driver event file is copied out of the scratch BEFORE
it is removed, and a JSON sidecar keyed by run id is written to DIR.
"""
import os
import shutil
import signal
import subprocess
import tempfile
import sys
import time
import json
import uuid
from pathlib import Path

IMAGE_SHA = {
    "off": "bb23e4638ff18cc22d74382b1a5c7586b7b48865049b1105ae0bfb55bf171058",
    "on": "fe7f6b1e308725b765b9e32e63f702311182fb5b2e6aafc9df9facd908dc951e",
}
ROWS = {
    "integer-sum", "vector-read", "vector-write", "vector-allocate64",
    "string-concat16", "string-slice16", "hash-get1024", "function-call2",
    "nested-function-call2",
}


def root_dir(env=None):
    env = os.environ if env is None else env
    return Path(env.get("NELISP_V8_ROOT", Path.home() / ".cache/tmp/nelisp-v8-native-20261029")).resolve()


def image_path(lane, env=None):
    env = os.environ if env is None else env
    # An explicit image (with its SHA-256) selects a cohort other than the
    # recorded r3 images; both must be given together.
    override = env.get("NELISP_LANE_IMAGE")
    if override:
        return Path(override).resolve()
    return root_dir(env) / "out/gnu-native-batch6/final-build-startup-r3" / f"build-{lane}" / "nelisp-dynamic"


def image_sha(lane, env=None):
    env = os.environ if env is None else env
    if env.get("NELISP_LANE_IMAGE"):
        sha = env.get("NELISP_LANE_IMAGE_SHA256", "")
        if len(sha) != 64:
            raise ValueError("NELISP_LANE_IMAGE requires NELISP_LANE_IMAGE_SHA256")
        return sha
    return IMAGE_SHA[lane]


def driver_path(lane, row, env=None):
    env = os.environ if env is None else env
    if row == "nested-function-call2":
        return Path(__file__).resolve().with_name(f"nested-primary-{lane}.el")
    return Path(__file__).resolve().with_name("benchmark-driver-native-on-gcwitness.el")


def phased_driver_path(lane):
    if lane != "on":
        raise ValueError("phase mode supports the ON lane only")
    return Path(__file__).resolve().with_name("benchmark-driver-native-on-phased.el")


def command_for(lane, driver, env=None, phase=False):
    """Build sandbox argv; DRIVER is the pair-runner placeholder.

    The row and lane select the matching generated driver because its embedded
    producer-image pin differs between OFF and ON.
    """
    env = os.environ if env is None else env
    row = env.get("BENCH_ROW", "")
    if row not in ROWS:
        raise ValueError(f"invalid BENCH_ROW: {row!r}")
    scratch_root = Path(env.get("NELISP_SCRATCH_ROOT", tempfile.gettempdir())).resolve()
    scratch_root.mkdir(parents=True, exist_ok=True)
    scratch = Path(tempfile.mkdtemp(prefix="nelisp-exp-", dir=scratch_root))
    (scratch / "cache").mkdir()
    env["NELISP_OWNED_SCRATCH"] = str(scratch)
    env["TMPDIR"] = str(scratch)
    env["XDG_CACHE_HOME"] = str(scratch / "cache")
    env["BENCH_ENGINE"] = "nelisp"
    env["NELISP_PRODUCER_IMAGE_SHA256"] = image_sha(lane, env)
    if phase:
        if row == "nested-function-call2":
            raise ValueError("phase mode does not support nested-function-call2")
        target = phased_driver_path(lane)
        env["NELISP_PHASE_RUN_ID"] = str(uuid.uuid4())
        env["NELISP_PHASE_EVENTS"] = str(scratch / "phase-events.sexp")
        env["NELISP_PHASE_LIB"] = str(Path(__file__).resolve().with_name("phase-events.el"))
        env["NELISP_PHASE_DRIVER"] = str(target)
    else:
        target = driver_path(lane, row, env)
    if not target.is_file():
        raise FileNotFoundError(f"driver unavailable for {lane}/{env.get('BENCH_ROW')}: {target}")
    image = image_path(lane, env)
    env["NELISP_OWNED_SCRATCH"] = str(scratch)
    return [
        "bwrap", "--ro-bind", "/", "/", "--bind", str(scratch), str(scratch), "--bind", str(Path(__file__).resolve().parent/"gcw"), str(Path(__file__).resolve().parent/"gcw"),
        "--proc", "/proc", "--dev", "/dev", "--unshare-user", "--unshare-pid",
        "--unshare-net", "--die-with-parent", "--new-session",
        "--setenv", "HOME", env.get("HOME", str(Path.home())),
        "--setenv", "TMPDIR", str(scratch), "--setenv", "XDG_CACHE_HOME", str(scratch / "cache"),
        "--", "/bin/bash", "-c", "ulimit -v 8388608; ulimit -c 0; exec \"$@\"", "lane",
        str(image), "--load", str(target),
    ]


def main():
    lane = os.environ.get("NELISP_LANE", "")
    if lane not in IMAGE_SHA:
        print("invoke as lane-off or lane-on", file=sys.stderr)
        return 2
    phase_out = None
    if len(sys.argv) == 5 and sys.argv[1] == "--load" and sys.argv[3] == "--phase-out":
        phase_out = Path(sys.argv[4])
    elif len(sys.argv) != 3 or sys.argv[1] != "--load":
        print("usage: lane-{off,on} --load DRIVER [--phase-out DIR]", file=sys.stderr)
        return 2
    row = os.environ.get("BENCH_ROW", "")
    if row not in ROWS:
        print(f"invalid BENCH_ROW: {row!r}", file=sys.stderr)
        return 2
    scratch = None
    proc = None
    wrapper = {}
    rc = None
    try:
        cmd = command_for(lane, sys.argv[2], phase=phase_out is not None)
        scratch = Path(os.environ["NELISP_OWNED_SCRATCH"])
        allowed = {"HOME", "USER", "LOGNAME", "PATH", "LANG", "LC_ALL", "LC_CTYPE",
                   "NELISP_V8_ROOT", "NELISP_SCRATCH_ROOT", "NELISP_PRODUCER_IMAGE_SHA256",
                   "NELISP_OWNED_SCRATCH", "NELISP_FXIR_ROOT", "NELISP_GC_WITNESS_OUT"}
        if phase_out is not None:
            allowed |= {"NELISP_PHASE_EVENTS", "NELISP_PHASE_RUN_ID",
                        "NELISP_PHASE_LIB", "NELISP_PHASE_DRIVER"}
        clean_env = {k: v for k, v in os.environ.items()
                     if k in allowed or k.startswith("BENCH_")}
        def terminate(signum, _frame):
            if proc is not None:
                try:
                    os.killpg(proc.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
            raise SystemExit(128 + signum)
        signal.signal(signal.SIGTERM, terminate)
        signal.signal(signal.SIGINT, terminate)
        try:
            if phase_out is not None:
                wrapper["SPAWN"] = time.monotonic_ns()
            proc = subprocess.Popen(cmd, env=clean_env, stdin=subprocess.DEVNULL, start_new_session=True)
            try:
                rc = proc.wait(timeout=600)
                if phase_out is not None:
                    wrapper["CHILD_EXIT"] = time.monotonic_ns()
                return rc
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGTERM)
                try:
                    proc.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    os.killpg(proc.pid, signal.SIGKILL)
                    proc.wait()
                print("lane process timed out; owned process group terminated", file=sys.stderr)
                return 124
        finally:
            if proc is not None and proc.poll() is None:
                try:
                    os.killpg(proc.pid, signal.SIGTERM)
                    proc.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    os.killpg(proc.pid, signal.SIGKILL)
                    proc.wait()
                except ProcessLookupError:
                    proc.wait()
            if scratch is not None:
                if phase_out is not None:
                    # Copy the driver event file out BEFORE the scratch goes away.
                    run_id = os.environ.get("NELISP_PHASE_RUN_ID", "")
                    copied = False
                    try:
                        phase_out.mkdir(parents=True, exist_ok=True)
                        ev = scratch / "phase-events.sexp"
                        if run_id and ev.is_file():
                            shutil.copyfile(ev, phase_out / f"{run_id}.events.sexp")
                            copied = True
                    except OSError as exc:
                        print(f"phase event copy failed: {exc}", file=sys.stderr)
                    wrapper.setdefault("CHILD_EXIT", time.monotonic_ns())
                shutil.rmtree(scratch, ignore_errors=True)
                if phase_out is not None:
                    wrapper["WRAPPER_CLEANUP_COMPLETE"] = time.monotonic_ns()
                    try:
                        sidecar = {run_id: {**wrapper, "clock": "CLOCK_MONOTONIC",
                                            "exit_code": rc, "events_copied": copied,
                                            "lane": lane, "row": row,
                                            # Source identity of the image (e.g. the pinned tree hash).
                                            "source_id": os.environ.get("NELISP_PHASE_SOURCE_ID", ""),
                                            "image": str(image_path(lane)),
                                            "image_sha256": image_sha(lane)}}
                        (phase_out / f"{run_id}.wrapper.json").write_text(json.dumps(sidecar, indent=1))
                    except OSError as exc:
                        print(f"phase sidecar write failed: {exc}", file=sys.stderr)
    except Exception as exc:
        print(f"lane launch failed: {exc}", file=sys.stderr)
        return 125


if __name__ == "__main__":
    raise SystemExit(main())
