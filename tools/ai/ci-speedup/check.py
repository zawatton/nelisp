#!/usr/bin/env python3
"""Machine checks for the CI speed-up ledger (tools/ai/ci-speedup-progress.org).

Usage: check.py {timeouts|concurrency|shards|parity-split|tests}
Exit 0 iff the named property holds on the working-tree workflows.
"""
import glob, os, subprocess, sys
import yaml

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
WF = os.path.join(ROOT, ".github", "workflows")


def load(name):
    with open(os.path.join(WF, name), encoding="utf-8") as f:
        return yaml.safe_load(f)


def fail(msg):
    print("FAIL:", msg)
    sys.exit(1)


def timeouts():
    for name, cap in (("ci.yml", 180), ("stage-d-v3.0-standalone.yml", 60)):
        for job, spec in load(name)["jobs"].items():
            t = spec.get("timeout-minutes")
            if not isinstance(t, int):
                fail(f"{name}:{job} has no integer job-level timeout-minutes")
            if t > cap:
                fail(f"{name}:{job} timeout {t} > cap {cap}")


def concurrency():
    for name in ("ci.yml", "stage-d-v3.0-standalone.yml"):
        c = load(name).get("concurrency")
        if not isinstance(c, dict) or "group" not in c:
            fail(f"{name}: no top-level concurrency group")
        if c.get("cancel-in-progress") is not True:
            fail(f"{name}: cancel-in-progress is not literally true")


def shards():
    m = load("ci.yml")["jobs"]["mutation"]
    s = m["strategy"]["matrix"]["shard"]
    if list(s) != list(range(12)):
        fail(f"mutation shards = {s}, want 0..11")
    if "/12" not in m.get("name", ""):
        fail("mutation job name does not say /12")


def parity_split():
    jobs = load("ci.yml")["jobs"]
    owners = [j for j, spec in jobs.items()
              if any("parity-fuzz" in str(st.get("run", "")) + str(st.get("name", ""))
                     for st in spec.get("steps", []))]
    if not owners:
        fail("no job runs parity-fuzz")
    if "smoke" in owners:
        fail("smoke still runs parity-fuzz")


def tests():
    rc = 0
    for t in sorted(glob.glob(os.path.join(ROOT, "test", "nelisp-ci-*-test.py"))):
        r = subprocess.run([sys.executable, t], cwd=ROOT,
                           stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
        if r.returncode:
            print("FAIL:", os.path.basename(t))
            rc = 1
    sys.exit(rc)


if __name__ == "__main__":
    {"timeouts": timeouts, "concurrency": concurrency, "shards": shards,
     "parity-split": parity_split, "tests": tests}[sys.argv[1]]()
