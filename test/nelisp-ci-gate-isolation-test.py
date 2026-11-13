#!/usr/bin/env python3
"""Structure test: one red gate must not mask the gates after it.

In the `gates', `smoke' and `ert' jobs, every check step after the
`setup-done' step must carry `!cancelled()' and depend on
`steps.setup-done.outcome == 'success'' in its `if', so a failing gate no
longer skips later gates (default `if: success()'), while a broken setup
still stops them.  No check may use `continue-on-error', so the job still
ends red.  The workflow is parsed with pyyaml; no step is executed.

Run:  python3 -I test/nelisp-ci-gate-isolation-test.py
NELISP_CI_WORKFLOW points the test at another copy (negative control).
"""
import os
import re
import sys
import unittest

try:
    import yaml
except ImportError:
    import site
    sys.path.append(site.getusersitepackages())
    try:
        import yaml
    except ImportError as exc:
        sys.exit("nelisp-ci-gate-isolation-test: pyyaml is required (%s)" % exc)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WORKFLOW = os.environ.get("NELISP_CI_WORKFLOW") or os.path.join(ROOT, ".github", "workflows", "ci.yml")
JOBS = ["gates", "smoke", "ert"]
SETUP_ID = "setup-done"
# A step that runs a gate or test: make, nelisp-ai.sh, preflight, python tests, npm/xvfb.
GATE_RUN = re.compile(r"\bmake\b|nelisp-ai\.sh|preflight\.sh|python3 test/|npm run|xvfb-run")
# Informational make calls that are not gates.
NOT_GATE = re.compile(r"standalone-reader-status")


def load():
    with open(WORKFLOW) as f:
        return yaml.safe_load(f)


def checks(job):
    steps = job["steps"]
    ids = [s.get("id") for s in steps]
    assert SETUP_ID in ids, "no `%s' step" % SETUP_ID
    out = []
    for s in steps[ids.index(SETUP_ID) + 1:]:
        run = s.get("run") or ""
        if GATE_RUN.search(run) and not NOT_GATE.search(run):
            out.append(s)
    return out


class GateIsolation(unittest.TestCase):
    def test_setup_step_present_and_plain(self):
        for name in JOBS:
            steps = load()["jobs"][name]["steps"]
            setup = [s for s in steps if s.get("id") == SETUP_ID]
            self.assertEqual(len(setup), 1, name)
            self.assertNotIn("if", setup[0], name)
            self.assertFalse(setup[0].get("continue-on-error"), name)

    def test_checks_run_after_earlier_failure(self):
        for name in JOBS:
            cs = checks(load()["jobs"][name])
            self.assertTrue(cs, name)
            for s in cs:
                cond = str(s.get("if", ""))
                label = "%s: %s" % (name, s.get("name"))
                self.assertIn("!cancelled()", cond, label)
                self.assertIn("steps.%s.outcome == 'success'" % SETUP_ID, cond, label)

    def test_no_check_hides_failure(self):
        for name in JOBS:
            for s in checks(load()["jobs"][name]):
                self.assertNotIn(s.get("continue-on-error"), (True, "true"),
                                 "%s: %s" % (name, s.get("name")))

    def test_binary_size_ratchet_depends_on_producer(self):
        steps = load()["jobs"]["smoke"]["steps"]
        by = {s.get("id"): s for s in steps if s.get("id")}
        self.assertIn("reader-test", by)
        rat = [s for s in steps if "binary-size-ratchet --" in (s.get("run") or "")]
        self.assertEqual(len(rat), 1)
        self.assertIn("steps.reader-test.outcome == 'success'", rat[0]["if"])


if __name__ == "__main__":
    unittest.main()
