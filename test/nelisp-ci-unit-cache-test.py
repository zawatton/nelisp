#!/usr/bin/env python3
"""Structure test of the standalone unit cache wiring in .github/workflows/ci.yml.

Every Linux job that builds the standalone reader must restore the
content-addressed unit cache before its first build step, save it only from a
push to main or a workflow_dispatch, and run with NELISP_STANDALONE_JOBS=4.  macOS and Windows must
not get JOBS=4.  The gates job must not set JOBS at all: its host ERT tests
compile units, and with JOBS > 1 they start prewarm children that slow them.  The workflow is parsed with pyyaml; no step is executed.

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

# Jobs whose Linux lane builds the reader or compiles units: gates (host ERT
# tests call nelisp-standalone--compile-to-unit), smoke and ert (standalone-reader-checked and the gate battery), mutation
# (warm-up rebuild) and subsystems (the tiers).  verify only reads reports.
BUILD_JOBS = ["gates", "build", "smoke", "smoke-linux-b", "ert", "mutation", "subsystems", "parity-fuzz"]
NON_BUILD_JOBS = ["verify"]
PATHS = ["target/standalone-units", "target/nelisp-vendor-bytecode"]
GLOBS = ["lisp/**", "src/**", "scripts/**", "packages/**", "vendor/**", "templates/**"]

# Save-condition scenarios: (event, ref, expected).  runner.os is Linux.
SAVE_SCENARIOS = [
    ("pull_request", "refs/pull/1/merge", False),
    ("pull_request", "refs/heads/main", False),
    ("push", "refs/heads/main", True),
    ("workflow_dispatch", "refs/heads/fix/x", True),
    ("push", "refs/heads/fix/x", False),
]
_TOKEN = re.compile(r"\s*(?:(?P<str>'[^']*')|(?P<op>==|!=|&&|\|\||\(|\))|(?P<call>always\(\))|(?P<ident>[A-Za-z_][A-Za-z0-9_.]*))")


def eval_condition(expr, event, ref, runner_os="Linux"):
    """Evaluate a GitHub `if:' expression from the subset used for save steps.

    Supports ==, !=, &&, ||, parentheses, always(), string literals and the
    contexts github.event_name, github.ref and runner.os.  Any other token
    raises ValueError.
    """
    ctx = {"github.event_name": event, "github.ref": ref, "runner.os": runner_os}
    toks, pos = [], 0
    expr = expr.strip()
    while pos < len(expr):
        m = _TOKEN.match(expr, pos)
        if not m:
            raise ValueError("unknown token at %d in %r" % (pos, expr))
        pos = m.end()
        kind = m.lastgroup
        text = m.group(kind)
        if kind == "ident" and text not in ctx:
            raise ValueError("unknown identifier %r in %r" % (text, expr))
        toks.append((kind, text))
    i = 0

    def peek():
        return toks[i] if i < len(toks) else (None, None)

    def operand():
        nonlocal i
        kind, text = peek()
        i += 1
        if kind == "str":
            return text[1:-1]
        if kind == "call":
            return True
        if kind == "ident":
            return ctx[text]
        if kind == "op" and text == "(":
            v = parse_or()
            if peek() != ("op", ")"):
                raise ValueError("missing ) in %r" % expr)
            i += 1
            return v
        raise ValueError("unexpected %r in %r" % (text, expr))

    def compare():
        nonlocal i
        v = operand()
        if peek()[0] == "op" and peek()[1] in ("==", "!="):
            op = peek()[1]
            i += 1
            w = operand()
            return (v == w) if op == "==" else (v != w)
        return v

    def parse_and():
        nonlocal i
        v = compare()
        while peek() == ("op", "&&"):
            i += 1
            w = compare()
            v = bool(v) and bool(w)
        return v

    def parse_or():
        nonlocal i
        v = parse_and()
        while peek() == ("op", "||"):
            i += 1
            w = parse_and()
            v = bool(v) or bool(w)
        return v

    result = parse_or()
    if i != len(toks):
        raise ValueError("trailing tokens in %r" % expr)
    return bool(result)


def save_condition_problems(cond, label):
    bad = []
    for event, ref, want in SAVE_SCENARIOS:
        try:
            got = eval_condition(cond, event, ref)
        except ValueError as exc:
            return ["%s save condition not evaluable: %s" % (label, exc)]
        if got != want:
            bad.append("save condition %r is %s for %s on %s, want %s" % (cond, got, event, ref, want))
    return bad
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
    # The mutation shards are restore-only: private unit-cache copies leave
    # the restored cache unchanged, so a save would be four near-copies.
    want_saves = 0 if name == "mutation" else 1
    if len(save) != want_saves:
        bad.append("expected %d save step(s), found %d" % (want_saves, len(save)))
    if bad:
        return bad
    if name == "mutation":
        r = steps[restore[0]]
        if "steps.policy.outputs.run == 'true'" not in r.get("if", ""):
            bad.append("mutation restore is not gated on the trigger policy")
        if sorted(x.strip() for x in r["with"]["path"].splitlines() if x.strip()) != sorted(PATHS):
            bad.append("restore paths are wrong")
        builds = [i for i, st in enumerate(steps) if is_build_step(st)]
        if builds and restore[0] > builds[0]:
            bad.append("restore step %d is after the first build step %d" % (restore[0], builds[0]))
        if str(job.get("env", {}).get("NELISP_STANDALONE_JOBS", "")) != "4":
            bad.append("NELISP_STANDALONE_JOBS is not 4, expected 4")
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
    bad.extend(save_condition_problems(cond, name))
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
    elif name == "gates":
        if "NELISP_STANDALONE_JOBS" in job.get("env", {}):
            bad.append("gates sets NELISP_STANDALONE_JOBS (%r), expected unset" % jobs_env)
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
                self.assertEqual(save_condition_problems(cond, name), [], name)
                self.assertFalse(eval_condition(cond, "pull_request", "refs/heads/main"), name)
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

    def test_save_condition_negative_controls(self):
        main_push = "(github.event_name == 'push' && github.ref == 'refs/heads/main')"
        dispatch = "github.event_name == 'workflow_dispatch'"
        controls = {
            "always() alone": "always()",
            "no workflow_dispatch arm": "always() && " + main_push,
            "pull_request exclusion only": "always() && github.event_name != 'pull_request'",
        }
        for label, cond in controls.items():
            def m(doc, cond=cond):
                self.save_step(doc, "gates")["if"] = cond
            with self.subTest(control=label):
                self.assertTrue(any("save condition" in b for b in check_job(self.damaged(m), "gates")), label)
        # the intended condition passes, and an unknown token fails loudly
        self.assertEqual(save_condition_problems("always() && (%s || %s)" % (main_push, dispatch), "x"), [])
        self.assertTrue(any("not evaluable" in b for b in save_condition_problems("always() && github.actor == 'x'", "x")))

    def test_restore_after_build_is_caught(self):
        def m(doc):
            steps = doc["jobs"]["gates"]["steps"]
            i = find(steps, "actions/cache/restore@")[0]
            steps.append(steps.pop(i))
        self.assertTrue(any("after the first build" in b for b in check_job(self.damaged(m), "gates")))

    def test_wrong_path_and_jobs_value_are_caught(self):
        def m(doc):
            steps = doc["jobs"]["mutation"]["steps"]
            steps[find(steps, "actions/cache/restore@")[0]]["with"]["path"] = "target/standalone-units"
            doc["jobs"]["mutation"]["env"]["NELISP_STANDALONE_JOBS"] = "2"
        bad = check_job(self.damaged(m), "mutation")
        self.assertTrue(any("paths" in b for b in bad) and any("expected 4" in b for b in bad), bad)

    def test_jobs_on_the_gates_job_is_caught(self):
        def m(doc):
            doc["jobs"]["gates"].setdefault("env", {})["NELISP_STANDALONE_JOBS"] = "4"
        bad = check_job(self.damaged(m), "gates")
        self.assertTrue(any("expected unset" in b for b in bad), bad)

    def test_a_save_step_in_the_mutation_job_is_caught(self):
        def m(doc):
            steps = doc["jobs"]["mutation"]["steps"]
            steps.append({"uses": "actions/cache/save@v4", "with": {"path": "x", "key": "y"}})
        self.assertTrue(any("save step" in b for b in check_job(self.damaged(m), "mutation")))

    def test_unconditional_jobs_4_on_matrix_job_is_caught(self):
        def m(doc):
            doc["jobs"]["ert"]["env"]["NELISP_STANDALONE_JOBS"] = "4"
        self.assertTrue(any("JOBS expression" in b for b in check_job(self.damaged(m), "ert")))


if __name__ == "__main__":
    unittest.main()
