#!/usr/bin/env python3
"""Tests for the gate-mutation budget wiring in .github/workflows/ci.yml.

Run:  python3 -I test/nelisp-ci-mutation-budget-test.py
"""
import copy
import os
import re
import sys
import unittest

try:
    import yaml
except ImportError:
    # `python3 -I' drops the per-user site directory, where pyyaml often lives.
    import site
    sys.path.append(site.getusersitepackages())
    try:
        import yaml
    except ImportError as exc:
        sys.exit("nelisp-ci-mutation-budget-test: pyyaml is required (%s)" % exc)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WORKFLOW = os.path.join(ROOT, ".github", "workflows", "ci.yml")
HARNESS = os.path.join(ROOT, "tools", "nelisp-gate-mutation.sh")
VARIANT_GATES = ["standalone-chunk-growth-test", "standalone-reader-ffi-smoke",
                 "ffi-dsl", "standalone-reader-tls-smoke-linux"]


def load():
    with open(WORKFLOW) as f:
        return yaml.safe_load(f)


def problems(doc, harness_text):
    bad = []
    job = doc["jobs"]["mutation"]
    steps = job["steps"]
    budget = None
    for s in steps:
        m = re.search(r"NELISP_GATE_MUTATION_SHARD_BUDGET=(\d+)", s.get("run", ""))
        if m:
            budget = int(m.group(1))
    if budget is None:
        bad.append("no step sets NELISP_GATE_MUTATION_SHARD_BUDGET")
    sweep = [s for s in steps if "gate-mutation, shard" in str(s.get("name", ""))]
    if not sweep or "NELISP_GATE_MUTATION_SHARD_BUDGET=" not in sweep[0].get("run", ""):
        bad.append("the sweep step does not carry the shard budget")
    tm = job.get("timeout-minutes")
    if tm is None:
        bad.append("mutation job has no timeout-minutes")
    elif budget is not None and tm <= budget / 60:
        bad.append("timeout-minutes %s is not above budget/60 = %s" % (tm, budget / 60))
    if any(str(s.get("uses", "")).startswith("actions/cache/save@") for s in steps):
        bad.append("mutation job has a cache/save step")
    m = re.search(r"gate_builds_variant\(\) \{(.*?)\n\}", harness_text, re.S)
    listed = re.findall(r"^\s*([A-Za-z0-9._-]+)\)\s*return 0", m.group(1), re.M) if m else []
    for g in VARIANT_GATES:
        if g not in listed:
            bad.append("gate_builds_variant lacks %s" % g)
    return bad


def harness_set(text):
    m = re.search(r"# BASELINE-SET-BEGIN\nbaseline_set='(.*?)'\n# BASELINE-SET-END", text, re.S)
    return sorted(l.strip() for l in m.group(1).split() if l.strip()) if m else []


def upload_set(doc):
    for s in doc["jobs"]["build"]["steps"]:
        if str(s.get("uses", "")).startswith("actions/upload-artifact@") and \
           s.get("with", {}).get("name") == "standalone-reader-linux":
            return sorted(l.strip() for l in s["with"]["path"].split() if l.strip())
    return []


class MutationBudget(unittest.TestCase):
    def setUp(self):
        self.doc = load()
        self.harness = open(HARNESS).read()

    def test_workflow_is_wired(self):
        self.assertEqual(problems(self.doc, self.harness), [])

    def test_missing_timeout_is_caught(self):
        d = copy.deepcopy(self.doc)
        del d["jobs"]["mutation"]["timeout-minutes"]
        self.assertTrue(any("no timeout-minutes" in b for b in problems(d, self.harness)))

    def test_too_small_timeout_is_caught(self):
        d = copy.deepcopy(self.doc)
        d["jobs"]["mutation"]["timeout-minutes"] = 60
        self.assertTrue(any("not above" in b for b in problems(d, self.harness)))

    def test_cache_save_is_caught(self):
        d = copy.deepcopy(self.doc)
        d["jobs"]["mutation"]["steps"].append({"uses": "actions/cache/save@v4"})
        self.assertTrue(any("cache/save" in b for b in problems(d, self.harness)))

    def test_variant_list_loss_is_caught(self):
        h = self.harness.replace("    ffi-dsl) return 0 ;;\n", "")
        self.assertTrue(any("ffi-dsl" in b for b in problems(self.doc, h)))

    def test_baseline_set_equals_the_build_upload_list(self):
        a, b = harness_set(self.harness), upload_set(self.doc)
        self.assertTrue(a and a == b, (a, b))

    def test_baseline_set_drift_is_caught(self):
        a = harness_set(self.harness.replace("target/nelisp.cold\n", "", 1))
        self.assertNotEqual(a, upload_set(self.doc))

    def test_sidecar_stays_out_of_verify(self):
        # The verify job copies only reports whose path lacks `mutation'; the
        # sidecar lives in the shard artifact and its name has `mutation' too.
        text = open(WORKFLOW).read()
        self.assertIn("-not -path '*mutation*'", text)
        self.assertIn("gate-mutation-rows-", self.harness)
        self.assertIn("target/gate-mutation-rows", self.harness)
        up = [x for x in self.doc["jobs"]["mutation"]["steps"]
              if x.get("with", {}).get("name") == "gate-reports-mutation-${{ matrix.shard }}"]
        self.assertTrue(up and "target/gate-mutation-rows/*.json" in up[0]["with"]["path"])


if __name__ == "__main__":
    unittest.main()
