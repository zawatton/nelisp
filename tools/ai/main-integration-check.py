#!/usr/bin/env python3
"""Checks for tools/ai/main-integration-progress.org.

Each subcommand exits 0 when its criterion holds and prints one line
saying why it does or does not.  Run from the repository root.

  test-files-load BASE      every host test file added since BASE loads
                            alone in a fresh `emacs --batch'
  ert-log LOG MIN           LOG is a full `nelisp-ai.sh test' run with at
                            least MIN tests and 0 unexpected results
  ci-parity [JOB-REGEX]     every job that succeeded in main's latest CI run
                            also succeeded in the latest CI run for HEAD
                            (optionally only the jobs matching JOB-REGEX)
  ledger LEDGER ROOT ID     criterion ID of LEDGER measures PASS with ROOT
                            as the repository root
"""

import concurrent.futures
import json
import os
import re
import subprocess
import sys


def run(args, **kw):
    return subprocess.run(args, capture_output=True, text=True, **kw)


def load_path_args():
    args = []
    for d in ("lisp", "src", "test", "bench", "scripts"):
        args += ["-L", d]
    for pkg in sorted(os.listdir("packages")):
        for sub in ("src", "test"):
            d = os.path.join("packages", pkg, sub)
            if os.path.isdir(d):
                args += ["-L", d]
    return args


def host_test_file_p(path):
    name = os.path.basename(path)
    if name == "nelisp-worker-soak-test.el" or not name.endswith("-test.el"):
        return False
    if path.startswith("test/"):
        return name.startswith("nelisp")
    return re.match(r"packages/[^/]+/test/(nelisp|nl-)", path) is not None


def test_files_load(base):
    out = run(["git", "diff", "--name-only", "--diff-filter=AR",
               base + "...HEAD", "--", "test", "packages"])
    if out.returncode != 0:
        print("git diff failed: " + out.stderr.strip())
        return 2
    files = [f for f in out.stdout.split() if host_test_file_p(f)]
    lp = load_path_args()

    def load(path):
        try:
            res = run(["emacs", "--batch", "-Q"] + lp +
                      ["--eval", "(setq load-prefer-newer t)",
                       "-l", "ert", "-l", path], timeout=300)
        except subprocess.TimeoutExpired:
            return path, "timeout"
        if res.returncode == 0:
            return path, None
        lines = [l for l in res.stderr.splitlines()
                 if re.match(r"(Error|Cannot|Wrong|Symbol|Invalid)", l)]
        return path, (lines[0] if lines else "rc=%d" % res.returncode)[:160]

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        failures = [r for r in pool.map(load, files) if r[1]]
    for path, why in failures:
        print("LOAD-FAIL %s: %s" % (path, why))
    print("test-files-load: %d of %d added test files fail to load alone"
          % (len(failures), len(files)))
    return 1 if failures else 0


def ert_log(log, minimum):
    try:
        text = open(log, encoding="utf-8", errors="replace").read()
    except OSError as err:
        print("ert-log: %s" % err)
        return 1
    m = re.findall(r"^Ran (\d+) tests, (\d+) results as expected, "
                   r"(\d+) unexpected", text, re.M)
    if not m:
        print("ert-log: %s has no ERT summary (the run died before it)" % log)
        return 1
    ran, _, unexpected = map(int, m[-1])
    ok = ran >= int(minimum) and unexpected == 0
    print("ert-log: ran=%d (need >= %s) unexpected=%d" % (ran, minimum, unexpected))
    return 0 if ok else 1


def latest_ci_jobs(branch=None, sha=None):
    args = ["gh", "run", "list", "--workflow", "CI", "--limit", "20",
            "--json", "databaseId,headSha,headBranch,status"]
    if branch:
        args += ["--branch", branch]
    res = run(args)
    if res.returncode != 0:
        raise RuntimeError(res.stderr.strip())
    runs = [r for r in json.loads(res.stdout)
            if r["status"] == "completed" and (sha is None or r["headSha"] == sha)]
    if not runs:
        return None, {}
    rid = runs[0]["databaseId"]
    res = run(["gh", "run", "view", str(rid), "--json", "jobs"])
    if res.returncode != 0:
        raise RuntimeError(res.stderr.strip())
    jobs = {j["name"]: j["conclusion"] for j in json.loads(res.stdout)["jobs"]}
    return rid, jobs


def ci_parity(pattern=None):
    head = run(["git", "rev-parse", "HEAD"]).stdout.strip()
    try:
        main_id, main_jobs = latest_ci_jobs(branch="main")
        head_id, head_jobs = latest_ci_jobs(sha=head)
    except RuntimeError as err:
        print("ci-parity: gh failed: %s" % err)
        return 2
    if head_id is None:
        print("ci-parity: no completed CI run for HEAD %s" % head[:12])
        return 1
    wanted = [n for n, c in main_jobs.items() if c == "success"
              and (pattern is None or re.search(pattern, n))]
    missing = [n for n in wanted if head_jobs.get(n) != "success"]
    for n in missing:
        print("REGRESSED %s: main=success head=%s" % (n, head_jobs.get(n)))
    print("ci-parity: main run %s, head run %s: %d of %d main-green jobs not green"
          % (main_id, head_id, len(missing), len(wanted)))
    return 1 if missing or not wanted else 0


def ledger(path, root, ident):
    here = os.path.dirname(os.path.abspath(__file__))
    meter = os.path.normpath(os.path.join(here, "..", "..", "..", "..",
                                          "bin", "progress-meter.sh"))
    meter = os.environ.get("PROGRESS_METER", meter)
    stage = ident.split(".")[0]
    res = run([meter, "--ledger", path, "--root", root, "--stage", stage,
               "--json"])
    m = re.search(r"^json: (.+)$", res.stdout, re.M)
    if not m:
        print("ledger: meter produced no json (rc=%d)" % res.returncode)
        return 2
    data = json.load(open(m.group(1)))
    for st in data["stages"]:
        for crit in st["criteria"]:
            if crit["id"] == ident:
                print("ledger: %s %s (%s)" % (ident, crit["status"], crit["detail"][:120]))
                return 0 if crit["status"] == "PASS" else 1
    print("ledger: %s not found" % ident)
    return 1


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    cmd, args = argv[1], argv[2:]
    if cmd == "test-files-load" and len(args) == 1:
        return test_files_load(*args)
    if cmd == "ert-log" and len(args) == 2:
        return ert_log(*args)
    if cmd == "ci-parity" and len(args) <= 1:
        return ci_parity(*args)
    if cmd == "ledger" and len(args) == 3:
        return ledger(*args)
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
