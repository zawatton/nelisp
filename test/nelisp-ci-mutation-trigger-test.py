#!/usr/bin/env python3
"""Tests for the gate-mutation trigger policy in .github/workflows/ci.yml.

The policy step's real `run:' script is extracted from the workflow and
executed under bash with a fake GITHUB_OUTPUT and a controlled env, so the
tests exercise the shipped script rather than a Python re-implementation.

Run:  python3 -I test/nelisp-ci-mutation-trigger-test.py [CLASS...]
"""
import json
import os
import subprocess
import sys
import tempfile
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
        sys.exit("nelisp-ci-mutation-trigger-test: pyyaml is required (%s)" % exc)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WORKFLOW = os.path.join(ROOT, ".github", "workflows", "ci.yml")

# Steps of the mutation job that existed before the policy was added, in order.
ORIGINAL_STEPS = [
    "actions/checkout@v4",
    "Set up GNU bytecode compiler (Linux / macOS)",
    "Record GNU bytecode compiler executable",
    "Set up Emacs",
    "Install rooted-protocol decoder dependencies (Linux)",
    "gate-mutation, shard ${{ matrix.shard }} of 4",
    "Upload gate report",
]
COSTLY = ORIGINAL_STEPS[1:6]


def load():
    with open(WORKFLOW) as f:
        return yaml.safe_load(f)


def steps_of(doc):
    return doc["jobs"]["mutation"]["steps"]


def label(step):
    return step.get("name") or step.get("uses")


def policy_step(doc):
    found = [s for s in steps_of(doc) if s.get("id") == "policy"]
    assert len(found) == 1, "expected exactly one policy step"
    return found[0]


def run_policy(script, event, ref, base_ref="", labels=(), dispatch_input=""):
    """Execute SCRIPT; return (run, reason, completed process)."""
    with tempfile.TemporaryDirectory() as tmp:
        out = os.path.join(tmp, "output")
        open(out, "w").close()
        env = {
            "PATH": os.environ["PATH"],
            "GITHUB_OUTPUT": out,
            "POLICY_EVENT": event,
            "POLICY_REF": ref,
            "POLICY_BASE_REF": base_ref,
            "POLICY_LABELS_JSON": json.dumps(list(labels)) if event == "pull_request" else "",
            "POLICY_DISPATCH_INPUT": dispatch_input,
        }
        p = subprocess.run(["bash", "-c", script], env=env, capture_output=True,
                           text=True, timeout=20)
        kv = {}
        with open(out) as f:
            lines = f.read().splitlines()
        for line in lines:
            k, _, v = line.partition("=")
            kv[k] = v
        return kv.get("run"), kv.get("reason"), p


class MainAndDispatch(unittest.TestCase):
    def setUp(self):
        self.doc = load()
        self.script = policy_step(self.doc)["run"]

    def check(self, expected, **kw):
        run, reason, p = run_policy(self.script, **kw)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(run, expected, (kw, reason))
        self.assertTrue(reason, "a reason line must be recorded")
        self.assertIn("gate-mutation policy: run=%s" % expected, p.stdout)

    def test_push_to_main_runs_full_sweep(self):
        self.check("true", event="push", ref="refs/heads/main")
        run, reason, _ = run_policy(self.script, event="push", ref="refs/heads/main")
        self.assertIn("full sweep", reason)

    def test_dispatch_on_main_runs(self):
        self.check("true", event="workflow_dispatch", ref="refs/heads/main")
        self.check("true", event="workflow_dispatch", ref="refs/heads/main",
                   dispatch_input="false")

    def test_dispatch_on_branch_input_false_skips(self):
        self.check("false", event="workflow_dispatch", ref="refs/heads/feature",
                   dispatch_input="false")
        self.check("false", event="workflow_dispatch", ref="refs/heads/feature",
                   dispatch_input="")

    def test_dispatch_on_branch_input_true_runs(self):
        self.check("true", event="workflow_dispatch", ref="refs/heads/feature",
                   dispatch_input="true")

    def test_dispatch_input_declared_boolean_default_false(self):
        # PyYAML reads the bare key `on' as boolean True.
        on = self.doc.get("on", self.doc.get(True))
        inp = on["workflow_dispatch"]["inputs"]["gate_mutation"]
        self.assertEqual(inp["type"], "boolean")
        self.assertIs(inp["default"], False)
        self.assertTrue(inp["description"])


class FeaturePR(unittest.TestCase):
    PR_REF = "refs/pull/7/merge"

    def setUp(self):
        self.doc = load()
        self.script = policy_step(self.doc)["run"]

    def pr(self, base, labels=()):
        run, reason, p = run_policy(self.script, event="pull_request",
                                    ref=self.PR_REF, base_ref=base, labels=labels)
        self.assertEqual(p.returncode, 0, p.stderr)
        return run

    def test_pr_into_main_or_master_runs_without_label(self):
        self.assertEqual(self.pr("main"), "true")
        self.assertEqual(self.pr("master"), "true")

    def test_pr_into_other_base_skips_without_label(self):
        self.assertEqual(self.pr("develop"), "false")
        self.assertEqual(self.pr("develop", ["bug", "gate-mutation-x"]), "false")
        # exact match only: a comma inside one name must not fake the label
        self.assertEqual(self.pr("develop", ["a,gate-mutation"]), "false")

    def test_pr_into_other_base_runs_with_label(self):
        self.assertEqual(self.pr("develop", ["gate-mutation"]), "true")
        self.assertEqual(self.pr("develop", ["x", "gate-mutation"]), "true")

    def test_policy_env_does_not_interpolate_github_into_body(self):
        step = policy_step(self.doc)
        self.assertNotIn("${{", step["run"])
        env = step["env"]
        joined = " ".join(env.values())
        for needle in ("github.event_name", "github.ref", "github.base_ref",
                       "pull_request.labels", "inputs.gate_mutation"):
            self.assertIn(needle, joined)
        # the skip step takes its reason through env as well
        skip = [s for s in steps_of(self.doc) if "trigger policy" in s.get("name", "")
                and s.get("name", "").startswith("gate-mutation skipped")][0]
        self.assertNotIn("${{", skip["run"])

    def test_costly_steps_are_gated(self):
        by_name = {label(s): s for s in steps_of(self.doc)}
        for name in COSTLY:
            cond = by_name[name].get("if", "")
            self.assertIn("steps.policy.outputs.run == 'true'", cond, name)

    def test_skip_path_still_uploads_a_report(self):
        steps = steps_of(self.doc)
        skip = [s for s in steps if s.get("name", "").startswith("gate-mutation skipped")]
        self.assertEqual(len(skip), 1)
        self.assertEqual(skip[0]["if"], "steps.policy.outputs.run != 'true'")
        self.assertIn("nelisp-ai.sh gate gate-mutation", skip[0]["run"])
        self.assertIn("GATE-SKIP", skip[0]["run"])
        up = steps[-1]
        self.assertEqual(label(up), "Upload gate report")
        self.assertEqual(up["if"], "always()")
        self.assertEqual(up["with"]["if-no-files-found"], "error")
        self.assertEqual(up["with"]["path"].split(), ["target/gates/*.json", "target/gate-mutation-rows/*.json"])

    def test_verify_needs_unchanged(self):
        self.assertEqual(self.doc["jobs"]["verify"]["needs"],
                         ["smoke", "ert", "mutation", "subsystems"])
        self.assertNotIn("if", self.doc["jobs"]["mutation"])

    def test_original_steps_present_in_order(self):
        names = [label(s) for s in steps_of(self.doc)]
        pos = [names.index(n) for n in ORIGINAL_STEPS]
        self.assertEqual(pos, sorted(pos))

    def test_pull_request_types_not_extended(self):
        on = self.doc.get("on", self.doc.get(True))
        self.assertNotIn("types", on["pull_request"])


class NegativeControl(unittest.TestCase):
    """The tests must be able to fail: a mutated policy script is caught."""

    def test_script_without_label_check_is_detected(self):
        script = policy_step(load())["run"]
        mutated = script.replace('"gate-mutation" in', '"gate-mutation-zz" in')
        self.assertNotEqual(mutated, script)
        run, _, _ = run_policy(mutated, event="pull_request", ref=FeaturePR.PR_REF,
                               base_ref="develop", labels=["gate-mutation"])
        self.assertEqual(run, "false", "mutated script should lose the label branch")


if __name__ == "__main__":
    names = [a for a in sys.argv[1:]]
    if names:
        suite = unittest.defaultTestLoader.loadTestsFromNames(names, sys.modules[__name__])
        res = unittest.TextTestRunner(verbosity=1).run(suite)
        sys.exit(0 if res.wasSuccessful() else 1)
    unittest.main(verbosity=1)
