#!/usr/bin/env python3
"""Structure test of the standalone unit cache wiring in .github/workflows/ci.yml.

Every Linux job that builds the standalone reader must restore the
content-addressed unit cache before its first build step, save it only from a
push to main, and run with NELISP_STANDALONE_JOBS=4.  macOS and Windows must
not get JOBS=4.  The workflow is parsed with pyyaml; no step is executed.

Run:  python3 -I test/nelisp-ci-unit-cache-test.py
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
        sys.exit("nelisp-ci-unit-cache-test: pyyaml is required (%s)" % exc)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# NELISP_CI_WORKFLOW points the test at a damaged copy (negative control).
WORKFLOW = os.environ.get("NELISP_CI_WORKFLOW") or os.path.join(ROOT, ".github", "workflows", "ci.yml")

# Jobs whose Linux lane builds the reader: gates (preflight's emacs-parity),
# smoke and ert (standalone-reader-checked and the gate battery), mutation
# (warm-up rebuild) and subsystems (the tiers).  verify only reads reports.
BUILD_JOBS = ["gates", "smoke", "ert", "mutation", "subsystems"]
NON_BUILD_JOBS = ["verify"]
PATHS = ["target/standalone-units", "target/nelisp-vendor-bytecode"]
GLOBS = ["lisp/**", "src/**", "scripts/**", "packages/**", "vendor/**", "templates/**"]
MAIN_PUSH = ["github.event_name == 'push'", "github.ref == 'refs/heads/main'"]
LINUX_ONLY_EXPR = "${{ matrix.os == 'ubuntu-latest' && '4' || '1' }}"
# A step that can start a build: a make target, preflight, or a tier run.
BUILD_STEP = re.compile(r"\bmake\b|preflight\.sh|nelisp-ai\.sh (?:test|check|extras|smokes|perf|\$\{\{ matrix\.tier)|nelisp-ai\.sh \$\{\{ matrix\.tier")


def load():
    with open(WORKFLOW) as f:
        return yaml.safe_load(f)


def step_name(s):
    return s.get("name") or s.get("uses") or ""


def is_build_step(s):
    run = s.get("run", "")
    if "GATE-SKIP" in run:
        return False  # the policy skip path prints a line, it builds nothing
    code = "\n".join(l for l in run.splitlines() if not l.lstrip().startswith("#"))
    return bool(BUILD_STEP.search(code)) or "matrix.tier" in code


def find(steps, uses):
    return [i for i, s in enumerate(steps) if s.get("uses", "").startswith(uses)]


def check_job(doc, name):
    """Return a list of problems for one build job (empty when fine)."""
    job = doc["jobs"][name]
    steps = job["steps"]
    bad = []
    restore = find(steps, "actions/cache/restore@")
    save = find(steps, "actions/cache/save@")
    if len(restore) != 1:
        bad.append("expected exactly one restore step, found %d" % len(restore))
    if len(save) != 1:
        bad.append("expected exactly one save step, found %d" % len(save))
    if bad:
        return bad
    r, sv = steps[restore[0]], steps[save[0]]
    builds = [i for i, s in enumerate(steps) if is_build_step(s)]
    if not builds:
        bad.append("no build step recognised (test out of date)")
    elif restore[0] > builds[0]:
        bad.append("restore step %d is after the first build step %d" % (restore[0], builds[0]))
    for label, st in (("restore", r), ("save", sv)):
        p = [x.strip() for x in st["with"]["path"].splitlines() if x.strip()]
        if sorted(p) != sorted(PATHS):
            bad.append("%s paths are %r" % (label, p))
        key = st["with"]["key"]
        if not key.startswith("units-${{ runner.os }}-"):
            bad.append("%s key does not start with units-<os>-: %s" % (label, key))
        m = re.search(r"hashFiles\(([^)]*)\)", key)
        got = re.findall(r"'([^']+)'", m.group(1)) if m else []
        if sorted(got) != sorted(GLOBS):
            bad.append("%s key hashes %r" % (label, got))
    if r["with"]["key"] != sv["with"]["key"]:
        bad.append("restore and save keys differ")
    rk = [x.strip() for x in r["with"].get("restore-keys", "").splitlines() if x.strip()]
    if "units-${{ runner.os }}-" not in rk:
        bad.append("restore-keys lack the generic units-<os>- fallback")
    if len(rk) < 2 or not rk[0].startswith("units-${{ runner.os }}-") or rk[0] == "units-${{ runner.os }}-":
        bad.append("restore-keys lack a job-specific prefix first")
    cond = sv.get("if", "")
    for part in MAIN_PUSH + ["always()"]:
        if part not in cond:
            bad.append("save condition lacks %r: %r" % (part, cond))
    if name == "mutation":
        for st, label in ((r, "restore"), (sv, "save")):
            if "steps.policy.outputs.run == 'true'" not in st.get("if", ""):
                bad.append("mutation %s is not gated on the trigger policy" % label)
    if name in ("smoke", "ert"):
        for st, label in ((r, "restore"), (sv, "save")):
            if "runner.os == 'Linux'" not in st.get("if", ""):
                bad.append("%s step of a matrix job is not Linux-only" % label)
    jobs_env = str(job.get("env", {}).get("NELISP_STANDALONE_JOBS", ""))
    if name in ("smoke", "ert"):
        if jobs_env != LINUX_ONLY_EXPR:
            bad.append("JOBS expression is %r" % jobs_env)
    elif jobs_env != "4":
        bad.append("NELISP_STANDALONE_JOBS is %r, expected 4" % jobs_env)
    return bad


def evaluate_jobs(expr, os_name):
    """Value of a JOBS setting for one matrix.os value (only the two forms used)."""
    expr = str(expr)
    if expr == LINUX_ONLY_EXPR:
        return "4" if os_name == "ubuntu-latest" else "1"
    return expr


class UnitCacheWiring(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.doc = load()

    def test_every_build_job_is_wired(self):
        for name in BUILD_JOBS:
            with self.subTest(job=name):
                self.assertEqual(check_job(self.doc, name), [])

    def test_non_build_jobs_have_no_cache_and_no_jobs(self):
        for name in NON_BUILD_JOBS:
            job = self.doc["jobs"][name]
            self.assertEqual(find(job["steps"], "actions/cache"), [], name)
            self.assertNotIn("NELISP_STANDALONE_JOBS", job.get("env", {}), name)
            self.assertFalse(any(is_build_step(s) for s in job["steps"]), name)

    def test_job_list_is_complete(self):
        self.assertEqual(sorted(self.doc["jobs"]), sorted(BUILD_JOBS + NON_BUILD_JOBS))

    def test_no_save_on_pull_request(self):
        for name, job in self.doc["jobs"].items():
            for i in find(job["steps"], "actions/cache/save@"):
                cond = job["steps"][i].get("if", "")
                self.assertIn("github.event_name == 'push'", cond, name)
                self.assertIn("github.ref == 'refs/heads/main'", cond, name)
                self.assertNotIn("pull_request", cond, name)
            # the combined actions/cache@ action also saves, and has no condition
            self.assertEqual(find(job["steps"], "actions/cache@"), [], name)

    def test_macos_and_windows_do_not_get_jobs_4(self):
        for name in ("smoke", "ert"):
            expr = self.doc["jobs"][name]["env"]["NELISP_STANDALONE_JOBS"]
            for os_name in self.doc["jobs"][name]["strategy"]["matrix"]["os"]:
                want = "4" if os_name == "ubuntu-latest" else "1"
                self.assertEqual(evaluate_jobs(expr, os_name), want, (name, os_name))
        for name, job in self.doc["jobs"].items():
            if "macos" in str(job.get("runs-on", "")) or "windows" in str(job.get("runs-on", "")):
                self.assertNotEqual(str(job.get("env", {}).get("NELISP_STANDALONE_JOBS")), "4", name)
            for s in job["steps"]:
                self.assertNotIn("NELISP_STANDALONE_JOBS=4", s.get("run", "") + str(s.get("env", "")), name)

    def test_actions_are_pinned(self):
        for job in self.doc["jobs"].values():
            for i in find(job["steps"], "actions/cache"):
                self.assertRegex(job["steps"][i]["uses"], r"@v\d+(\.\d+)*$")


class NegativeControl(unittest.TestCase):
    """The checks must be able to fail on a damaged copy of the workflow."""

    def damaged(self, mutate):
        doc = copy.deepcopy(load())
        mutate(doc)
        return doc

    def save_step(self, doc, job):
        return doc["jobs"][job]["steps"][find(doc["jobs"][job]["steps"], "actions/cache/save@")[0]]

    def test_dropping_the_save_condition_is_caught(self):
        def m(doc):
            del self.save_step(doc, "subsystems")["if"]
        self.assertNotEqual(check_job(self.damaged(m), "subsystems"), [])

    def test_save_on_pull_request_is_caught(self):
        def m(doc):
            self.save_step(doc, "gates")["if"] = "always()"
        self.assertTrue(any("save condition" in b for b in check_job(self.damaged(m), "gates")))

    def test_restore_after_build_is_caught(self):
        def m(doc):
            steps = doc["jobs"]["gates"]["steps"]
            i = find(steps, "actions/cache/restore@")[0]
            steps.append(steps.pop(i))
        self.assertTrue(any("after the first build" in b for b in check_job(self.damaged(m), "gates")))

    def test_wrong_path_and_jobs_value_are_caught(self):
        def m(doc):
            self.save_step(doc, "mutation")["with"]["path"] = "target/standalone-units"
            doc["jobs"]["mutation"]["env"]["NELISP_STANDALONE_JOBS"] = "2"
        bad = check_job(self.damaged(m), "mutation")
        self.assertTrue(any("paths" in b for b in bad) and any("expected 4" in b for b in bad), bad)

    def test_unconditional_jobs_4_on_matrix_job_is_caught(self):
        def m(doc):
            doc["jobs"]["ert"]["env"]["NELISP_STANDALONE_JOBS"] = "4"
        self.assertTrue(any("JOBS expression" in b for b in check_job(self.damaged(m), "ert")))


if __name__ == "__main__":
    unittest.main()
