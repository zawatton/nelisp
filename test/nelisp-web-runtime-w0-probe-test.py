#!/usr/bin/env python3
"""Controlled negative test for W0 timeout cleanup; run under heavy-run.sh."""

import importlib.util
import ctypes
import json
import os
import signal
from pathlib import Path
import sys

sys.dont_write_bytecode = True
root = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("w0_probe", root / "tools/ai/web-runtime-w0-probe.py")
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)
probe.OUTPUT.mkdir(parents=True, exist_ok=True)
pid_file = probe.OUTPUT / "timeout-child.pid"
pid_file.unlink(missing_ok=True)
program = """
import os, signal, subprocess, sys, time
from pathlib import Path
child = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])
def terminate(signum, frame):
    try:
        child.wait(timeout=0.2)
    except subprocess.TimeoutExpired:
        pass
    sys.exit(0)
signal.signal(signal.SIGTERM, terminate)
Path(sys.argv[1]).write_text(str(child.pid))
time.sleep(60)
"""
# Adopt an orphan from the deliberately broken control so this harness can
# terminate and reap it itself, rather than relying on an unrelated init.
assert ctypes.CDLL(None).prctl(36, 1, 0, 0, 0) == 0  # Linux PR_SET_CHILD_SUBREAPER
group_kill = os.killpg
rows = []
for broken in (False, True):
    try:
        if broken:
            probe.os.killpg = lambda pid, sig: os.kill(pid, sig)
        row = probe.run("timeout-parent-only" if broken else "timeout-control",
                        [sys.executable, "-c", program, str(pid_file)], timeout=1)
    finally:
        probe.os.killpg = group_kill
    assert row["rc"] == 124, row
    child_pid = int(pid_file.read_text())
    try:
        os.kill(child_pid, 0)
    except ProcessLookupError:
        child_survives = False
    else:
        child_survives = True
        os.kill(child_pid, signal.SIGKILL)
        os.waitpid(child_pid, 0)
    assert child_survives == broken, (broken, child_survives)
    rows.append(dict(parent_only_mutation=broken, child_survives=child_survives,
                     deadline_rc=row["rc"], elapsed_seconds=row["elapsed_seconds"]))
result = dict(rc=0, checked=2, controls=rows, surviving_children_after_cleanup=0)
(probe.OUTPUT / "timeout-negative.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result))
