#!/usr/bin/env python3
"""Structure test of the shared standalone-reader build in .github/workflows/ci.yml.

One Linux `build' job builds target/nelisp and uploads it; every Linux consumer
(smoke, ert, mutation, subsystems) needs it, downloads it AFTER checkout and
before its first build-dependent step, marks it executable and logs the stamp
status without being able to fail the job.  The stamp tool recomputes its key on
the consumer, so inputs that enter the key must match between build and
consumers: pinned pip packages and a full git history.  The workflow is parsed
with pyyaml; no step is executed.

Run:  python3 -I test/nelisp-ci-build-job-test.py
"""
import copy
import os
import re
import sys
import unittest

try:
    import yaml
except ImportError:
    # `python3 -I' drops the per-user site directory, where pyyaml often
    # lives.  Look there once for yaml only, then fail loudly if absent.
    import site
    sys.path.append(site.getusersitepackages())
    try:
        import yaml
    except ImportError as exc:
        sys.exit("nelisp-ci-build-job-test: pyyaml is required (%s)" % exc)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# NELISP_CI_WORKFLOW points the test at a damaged copy (negative control).
WORKFLOW = os.environ.get("NELISP_CI_WORKFLOW") or os.path.join(ROOT, ".github", "workflows", "ci.yml")

ARTIFACT = "standalone-reader-linux"
CONSUMERS = ["smoke", "ert", "mutation", "subsystems"]
# Files the binary and the gates read; the first group is the stamp tool's
# COMPANIONS plus the stamp itself.
REQUIRED = [
    "target/nelisp",
    "target/nelisp.cold",
    "target/nelisp.build-stamp",
    "target/nelisp.version.json",
    "target/nelisp-artifact-runtime.el",
    "target/nelisp-artifact-runtime.el.nelc",
    "target/nelisp-artifact-runtime.el.nelc.enable",
    "target/nelisp-artifact-runtime.el.nelc.manifest.el",
]
PIN = re.compile(r"capstone==\d+(?:\.\d+)+\s+pyelftools==\d+(?:\.\d+)+")
# A step that can start a build: a make target, preflight, or a tier run.
BUILD_STEP = re.compile(r"\bmake\b|preflight\.sh|nelisp-ai\.sh")


def load(path=None):
    with open(path or WORKFLOW) as f:
        return yaml.safe_load(f)


def code_of(step):
    run = step.get("run", "")
    return "\n".join(l for l in run.splitlines() if not l.lstrip().startswith("#"))


def is_build_step(s):
    run = s.get("run", "")
    if "GATE-SKIP" in run or "standalone-reader-status" in run:
        return False
    return bool(BUILD_STEP.search(code_of(s)))


def uses(s, prefix):
    return str(s.get("uses", "")).startswith(prefix)


def idx(steps, pred):
    for i, s in enumerate(steps):
        if pred(s):
            return i
    return None


def check(doc):
    """Return a list of problems; empty means the wiring is right."""
    bad = []
    jobs = doc["jobs"]
    # PYTHONDONTWRITEBYTECODE keeps tracked .pyc files from dirtying the tree.
    if str((doc.get("env") or {}).get("PYTHONDONTWRITEBYTECODE")) != "1":
        bad.append("workflow env lacks PYTHONDONTWRITEBYTECODE=1")
    b = jobs.get("build")
    if not b:
        return bad + ["no build job"]
    if b.get("runs-on") != "ubuntu-latest":
        bad.append("build job is not ubuntu-latest")
    if "needs" in b:
        bad.append("build job must not wait for another job")
    steps = b["steps"]
    co = steps[0]
    if not uses(co, "actions/checkout@v4") or (co.get("with") or {}).get("fetch-depth") != 0:
        bad.append("build: first step must be checkout with fetch-depth 0")
    if idx(steps, lambda s: "make standalone-reader " in s.get("run", "") + " ") is None:
        bad.append("build: no `make standalone-reader' step")
    up = idx(steps, lambda s: uses(s, "actions/upload-artifact@v4") and (s.get("with") or {}).get("name") == ARTIFACT)
    if up is None:
        bad.append("build: no upload of " + ARTIFACT)
    else:
        w = steps[up]["with"]
        paths = w.get("path", "").split()
        for r in REQUIRED:
            if r not in paths:
                bad.append("build: upload lacks " + r)
        if w.get("if-no-files-found") != "error":
            bad.append("build: upload lacks if-no-files-found: error")
        mk = idx(steps, lambda s: "standalone-reader" in s.get("run", "") and "make standalone-reader " in s["run"] + " ")
        if mk is not None and mk > up:
            bad.append("build: upload happens before the build")
        chk = idx(steps, lambda s: all(r in s.get("run", "") for r in REQUIRED))
        if chk is None or chk > up:
            bad.append("build: no pre-upload existence check of every required path")
    # pip pins, identical everywhere
    pins = set()
    for name, j in jobs.items():
        for s in j.get("steps", []):
            r = s.get("run", "")
            if "capstone" in r and "pip install" in r:
                m = PIN.search(r)
                if not m:
                    bad.append("%s: pip step does not pin capstone/pyelftools" % name)
                else:
                    pins.add(re.sub(r"\s+", " ", m.group(0)))
    if len(pins) != 1:
        bad.append("pip pins differ between jobs: %r" % sorted(pins))
    # consumers
    for name in CONSUMERS:
        j = jobs.get(name)
        if not j:
            bad.append("no job " + name)
            continue
        needs = j.get("needs")
        needs = [needs] if isinstance(needs, str) else (needs or [])
        if "build" not in needs:
            bad.append("%s: does not need build" % name)
        st = j["steps"]
        co = idx(st, lambda s: uses(s, "actions/checkout@v4"))
        if co is None or (st[co].get("with") or {}).get("fetch-depth") != 0:
            bad.append("%s: checkout lacks fetch-depth 0" % name)
        dl = idx(st, lambda s: uses(s, "actions/download-artifact@v4") and (s.get("with") or {}).get("name") == ARTIFACT)
        if dl is None:
            bad.append("%s: no download of %s" % (name, ARTIFACT))
            continue
        if (st[dl]["with"].get("path")) != "target":
            bad.append("%s: download path is not `target'" % name)
        if co is not None and dl < co:
            bad.append("%s: download before checkout" % name)
        fb = idx(st, is_build_step)
        if fb is not None and dl > fb:
            bad.append("%s: download after the first build-dependent step" % name)
        pip = idx(st, lambda s: "pip install" in s.get("run", "") and "capstone" in s.get("run", ""))
        if pip is not None and dl < pip:
            bad.append("%s: download before the pip step" % name)
        ch = idx(st, lambda s: "chmod +x target/nelisp" in s.get("run", ""))
        if ch is None or ch < dl:
            bad.append("%s: no chmod after the download" % name)
        sts = idx(st[dl:], lambda s: "make -s standalone-reader-status" in s.get("run", ""))
        if sts is None:
            bad.append("%s: no status step after the download" % name)
        else:
            s = st[dl + sts]
            if dl + sts < (ch or 0):
                bad.append("%s: status before chmod" % name)
            if not (s.get("continue-on-error") is True and "|| true" in s["run"]):
                bad.append("%s: status step can fail the job" % name)
        # an end-of-job status with if: always()
        fin = [s for s in st[dl + 1:] if "make -s standalone-reader-status" in s.get("run", "")
               and "always()" in str(s.get("if", ""))]
        if not fin:
            bad.append("%s: no end-of-job status step with if: always()" % name)
        elif not (fin[0].get("continue-on-error") is True and "|| true" in fin[0]["run"]):
            bad.append("%s: end-of-job status step can fail the job" % name)
        # Linux-only (smoke/ert are matrix jobs with macOS and Windows lanes)
        for s in (st[dl], st[ch] if ch is not None else {}, st[dl + sts] if sts is not None else {}):
            cond = str(s.get("if", ""))
            if name in ("smoke", "ert") and "runner.os == 'Linux'" not in cond:
                bad.append("%s: shared-binary step %r is not Linux-only" % (name, s.get("name")))
            if name == "mutation" and "steps.policy.outputs.run == 'true'" not in cond:
                bad.append("mutation: shared-binary step %r is not gated on the policy" % s.get("name"))
    # jobs that must stay independent of build
    for name, j in jobs.items():
        n = j.get("needs")
        n = [n] if isinstance(n, str) else (n or [])
        if name not in CONSUMERS and "build" in n:
            bad.append("%s must not need build" % name)
    if jobs.get("verify", {}).get("needs") != ["smoke", "ert", "mutation", "subsystems"]:
        bad.append("verify needs changed")
    return bad


class BuildJobWiring(unittest.TestCase):
    def setUp(self):
        self.doc = load()

    def test_wiring_is_complete(self):
        self.assertEqual(check(self.doc), [])

    def test_gates_job_is_independent(self):
        self.assertNotIn("needs", self.doc["jobs"]["gates"])

    def test_workflow_structure_tests_run_in_smoke(self):
        run = "\n".join(s.get("run", "") for s in self.doc["jobs"]["smoke"]["steps"])
        for t in ("nelisp-ci-unit-cache-test.py", "nelisp-ci-mutation-trigger-test.py",
                  "nelisp-variant-outputs-test.py", "nelisp-ci-build-job-test.py"):
            self.assertIn("test/" + t, run)
        self.assertIn("pip install --user --break-system-packages pyyaml", run)


class NegativeControls(unittest.TestCase):
    """A damaged copy of the parsed workflow must be reported."""

    def setUp(self):
        self.doc = load()

    def drop(self, job, pred):
        d = copy.deepcopy(self.doc)
        st = d["jobs"][job]["steps"]
        d["jobs"][job]["steps"] = [s for s in st if not pred(s)]
        return d

    def test_removed_download_is_reported(self):
        for job in CONSUMERS:
            d = self.drop(job, lambda s: uses(s, "actions/download-artifact@v4")
                          and (s.get("with") or {}).get("name") == ARTIFACT)
            self.assertTrue(any("no download" in p for p in check(d)), job)

    def test_unpinned_pip_is_reported(self):
        d = copy.deepcopy(self.doc)
        for s in d["jobs"]["ert"]["steps"]:
            if "capstone" in s.get("run", ""):
                s["run"] = s["run"].replace("==5.0.9", "").replace("==0.33", "")
        self.assertTrue(any("pin" in p for p in check(d)))

    def test_missing_needs_is_reported(self):
        d = copy.deepcopy(self.doc)
        del d["jobs"]["subsystems"]["needs"]
        self.assertTrue(any("does not need build" in p for p in check(d)))

    def test_status_that_can_fail_is_reported(self):
        d = copy.deepcopy(self.doc)
        for s in d["jobs"]["smoke"]["steps"]:
            if "make -s standalone-reader-status" in s.get("run", ""):
                s.pop("continue-on-error", None)
                s["run"] = s["run"].replace(" || true", "")
        self.assertTrue(any("can fail" in p for p in check(d)))

    def test_upload_losing_a_path_is_reported(self):
        d = copy.deepcopy(self.doc)
        for s in d["jobs"]["build"]["steps"]:
            if uses(s, "actions/upload-artifact@v4"):
                s["with"]["path"] = s["with"]["path"].replace("target/nelisp.cold", "")
        self.assertTrue(any("lacks target/nelisp.cold" in p for p in check(d)))

    def test_upload_without_error_mode_is_reported(self):
        d = copy.deepcopy(self.doc)
        for s in d["jobs"]["build"]["steps"]:
            if uses(s, "actions/upload-artifact@v4"):
                s["with"]["if-no-files-found"] = "warn"
        self.assertTrue(any("if-no-files-found" in p for p in check(d)))

    def test_shallow_checkout_is_reported(self):
        d = copy.deepcopy(self.doc)
        d["jobs"]["ert"]["steps"][0]["with"] = {}
        self.assertTrue(any("fetch-depth" in p for p in check(d)))

    def test_download_moved_after_build_step_is_reported(self):
        d = copy.deepcopy(self.doc)
        st = d["jobs"]["subsystems"]["steps"]
        i = idx(st, lambda s: uses(s, "actions/download-artifact@v4"))
        j = idx(st, lambda s: "nelisp-ai.sh" in s.get("run", ""))
        st.insert(j + 1, st.pop(i))
        self.assertTrue(any("after the first build" in p for p in check(d)))

    def test_mutation_download_not_gated_is_reported(self):
        d = copy.deepcopy(self.doc)
        for s in d["jobs"]["mutation"]["steps"]:
            if uses(s, "actions/download-artifact@v4"):
                s.pop("if", None)
        self.assertTrue(any("not gated on the policy" in p for p in check(d)))

    def test_build_for_gates_is_reported(self):
        d = copy.deepcopy(self.doc)
        d["jobs"]["gates"]["needs"] = "build"
        self.assertTrue(any("gates must not need build" in p for p in check(d)))



class StatusTargetMatchesBuild(unittest.TestCase):
    """`standalone-reader-status' must pass the stamp tool the same build
    command as `standalone-reader'; a drift only gives false `stale' lines,
    never a false green, but it would make the CI status step useless."""

    @staticmethod
    def recipe_after_dashes(makefile_text, target):
        lines = makefile_text.splitlines()
        start = lines.index(target + ":")
        body = []
        for line in lines[start + 1:]:
            if not line.startswith("\t"):
                break
            body.append(line.strip())
        joined = " ".join(body)
        if " -- " not in joined:
            raise AssertionError(f"{target}: no ' -- ' in recipe: {joined!r}")
        return joined.split(" -- ", 1)[1]

    def test_same_build_command(self):
        text = open(os.path.join(ROOT, "Makefile"), encoding="utf-8").read()
        self.assertEqual(self.recipe_after_dashes(text, "standalone-reader"),
                         self.recipe_after_dashes(text, "standalone-reader-status"))

    def test_drift_is_caught(self):
        text = open(os.path.join(ROOT, "Makefile"), encoding="utf-8").read()
        drifted = text.replace("standalone-reader-status:\n",
                               "standalone-reader-status:\n\tbash x --status -- emacs -Q\n", 1)
        self.assertNotEqual(self.recipe_after_dashes(drifted, "standalone-reader"),
                            self.recipe_after_dashes(drifted, "standalone-reader-status"))

if __name__ == "__main__":
    unittest.main()
