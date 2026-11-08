"""Hermetic tests for the per-row budgets and timing report of the
gate-mutation harness.  A fixture repository with a fake Makefile and a fake
row table stands in for the real tree; no real gate or build runs.

Usage: python3 test/nelisp-gate-mutation-row-timeout-test.py [RowTimeout|RowTimingReport|RebuildLog]

NELISP_GATE_MUTATION_UNDER_TEST may name another copy of the harness; it
exists so a deliberately broken copy can prove these tests are able to fail.
"""
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
HARNESS = Path(os.environ.get("NELISP_GATE_MUTATION_UNDER_TEST",
                              ROOT / "tools/nelisp-gate-mutation.sh"))

# Recipe lines start with a TAB.  Behaviour is chosen by the injected text
# ("NEW" in the mutated file), so a clean run is always fast and green.
MAKEFILE = r"""
standalone-reader:
	@echo b >> builds.log; eval "$$FAKE_REBUILD"
hang:
	@echo hang >> gates.log; if grep -q NEW lisp/hang.el; then sleep 300; fi
okred:
	@echo okred >> gates.log; ! grep -q NEW lisp/okred.el
spawn:
	@echo spawn >> gates.log; if grep -q NEW lisp/spawn.el; then sh -c 'trap "" TERM; echo $$$$ > child.pid; sleep 300' & sleep 300; fi
skipbase:
	@echo skipbase >> gates.log; if grep -q NEW lisp/skipbase.el; then echo "GATE-SKIP injected"; else sleep 300; fi
green:
	@echo green >> gates.log
emacs-parity:
	@echo emacs-parity >> gates.log; ! grep -q NEW lisp/rb.el
s0:
	@echo s0 >> gates.log; ! grep -q NEW lisp/okred.el
s1:
	@sleep 1; ! grep -q NEW lisp/okred.el
s2:
	@sleep 2; ! grep -q NEW lisp/okred.el
s3:
	@sleep 4; ! grep -q NEW lisp/okred.el
"""

ORIGINAL = b"OLD\nlocal uncommitted content\n"


def row(gate, path):
    return f"{gate}|{path}|s/OLD/NEW/|injected defect for {gate}"


class Fixture(unittest.TestCase):
    def build(self, rows, rebuild=":", env=None, files=None):
        directory = tempfile.TemporaryDirectory(prefix="nelisp-row-timeout-")
        self.addCleanup(directory.cleanup)
        root = Path(directory.name)
        for name in ["tools", "lisp", "tmp"]:
            (root / name).mkdir()
        shutil.copy2(HARNESS, root / "tools/nelisp-gate-mutation.sh")
        (root / "tools/gate-mutations.txt").write_text("\n".join(rows) + "\n")
        (root / "Makefile").write_text(MAKEFILE)
        for name in files or ["hang", "okred", "spawn", "skipbase", "green", "rb"]:
            (root / "lisp" / f"{name}.el").write_bytes(ORIGINAL)
        full = {k: v for k, v in os.environ.items()
                if not k.startswith("NELISP_GATE_MUTATION_")}
        full.update(TMPDIR=str(root / "tmp"), FAKE_REBUILD=rebuild,
                    NELISP_GATE_MUTATION_ROW_TIMEOUT="2",
                    NELISP_GATE_MUTATION_BUILD_TIMEOUT="60")
        full.update(env or {})
        self.root, self.env = root, full
        return root

    def run_harness(self, limit=40):
        result = subprocess.run(["bash", str(self.root / "tools/nelisp-gate-mutation.sh")],
                                cwd=self.root, env=self.env, capture_output=True,
                                text=True, timeout=limit)
        self.out = result.stdout + result.stderr
        return result

    def assertRestored(self, *names):
        for name in names:
            self.assertEqual((self.root / "lisp" / f"{name}.el").read_bytes(), ORIGINAL, name)

    def builds(self):
        log = self.root / "builds.log"
        return len(log.read_text().split()) if log.exists() else 0

    def gates_run(self):
        log = self.root / "gates.log"
        return log.read_text().split() if log.exists() else []


class RowTimeout(Fixture):
    def test_hanging_gate_is_a_named_failure_and_sweep_continues(self):
        self.build([row("hang", "lisp/hang.el"), row("okred", "lisp/okred.el")])
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("  hang: TIMEOUT (gate run with injection after 2s; file lisp/hang.el)", self.out)
        self.assertNotIn("hang: went red", self.out)
        self.assertRestored("hang", "okred")
        self.assertEqual(self.gates_run(), ["hang", "okred"], self.out)
        self.assertIn("okred: went red as it should", self.out)
        self.assertIn("passed=1 failed=1", self.out)

    def test_hanging_rebuild_is_a_named_failure_and_later_rows_get_a_clean_binary(self):
        self.build([row("emacs-parity", "lisp/rb.el"), row("okred", "lisp/okred.el")],
                   rebuild="if grep -q NEW lisp/rb.el; then sleep 300; fi",
                   env={"NELISP_GATE_MUTATION_BUILD_TIMEOUT": "2"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("  emacs-parity: TIMEOUT (rebuild with injection after 2s; file lisp/rb.el)", self.out)
        self.assertRestored("rb", "okred")
        # The gate itself never ran under injection; the next row ran.
        self.assertEqual(self.gates_run(), ["okred"], self.out)
        self.assertIn("okred: went red as it should", self.out)
        # warm-up, the timed-out mutated build, and a recovery rebuild that
        # keeps the partial binary away from the next row.
        self.assertEqual(self.builds(), 3, self.out)
        self.assertIn("recovery rebuild took", self.out)

    def test_term_ignoring_child_does_not_survive(self):
        self.build([row("spawn", "lisp/spawn.el")])
        result = self.run_harness(limit=45)
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("spawn: TIMEOUT", self.out)
        pid = int((self.root / "child.pid").read_text())
        deadline = time.time() + 5
        while time.time() < deadline and Path(f"/proc/{pid}").exists():
            state = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()[0]
            if state == "Z":
                break
            time.sleep(0.2)
        alive = Path(f"/proc/{pid}").exists() and \
            Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()[0] != "Z"
        if alive:
            os.kill(pid, 9)
        self.assertFalse(alive, "TERM-ignoring gate child survived the row budget")
        self.assertRestored("spawn")

    def test_clean_baseline_timeout_fails(self):
        self.build([row("skipbase", "lisp/skipbase.el"), row("okred", "lisp/okred.el")])
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("  skipbase: TIMEOUT (clean-baseline gate run after 2s; file lisp/skipbase.el)", self.out)
        self.assertNotIn("skipbase: SKIP", self.out)
        self.assertRestored("skipbase", "okred")
        self.assertIn("okred: went red as it should", self.out)

    def test_ordinary_verdicts_are_unchanged(self):
        self.build([row("green", "lisp/green.el"), row("okred", "lisp/okred.el")])
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("green: STAYED GREEN", self.out)
        self.assertIn("okred: went red as it should", self.out)
        self.assertNotIn("TIMEOUT", self.out)
        self.assertRestored("green", "okred")

    def test_invalid_budget_values_are_rejected(self):
        for name in ["NELISP_GATE_MUTATION_ROW_TIMEOUT", "NELISP_GATE_MUTATION_BUILD_TIMEOUT",
                     "NELISP_GATE_MUTATION_SLOW_ROW"]:
            for value in ["0", "abc", "-1", "1.5"]:
                with self.subTest(name=name, value=value):
                    self.build([row("okred", "lisp/okred.el")], env={name: value})
                    result = self.run_harness()
                    self.assertEqual(result.returncode, 1, self.out)
                    self.assertIn(f"FAIL ({name} must be a positive integer", self.out)
                    self.assertEqual(self.gates_run(), [])
                    self.assertEqual(self.builds(), 0)
                    self.assertRestored("okred")


class RowTimingReport(Fixture):
    def build(self, rows, **kw):
        # Real sleeps of up to 3 s must not trip the row budget here, except
        # in the test that wants a timeout (it passes its own value).
        kw.setdefault("env", {}).setdefault("NELISP_GATE_MUTATION_ROW_TIMEOUT", "60")
        return super().build(rows, **kw)

    def test_slow_row_line_appears_and_fast_row_prints_none(self):
        self.build([row("s2", "lisp/okred.el"), row("s0", "lisp/okred.el")],
                   env={"NELISP_GATE_MUTATION_SLOW_ROW": "2"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"gate-mutation: row 1/2 s2 file=lisp/okred\.el [2-9]s \[slow\]")
        self.assertNotRegex(self.out, r"row 2/2 s0")

    def test_summary_is_sorted_and_limited_to_ten(self):
        gates = ["s0"] * 5 + ["s1", "s3", "s0", "s2", "s0", "s0", "s0"]
        self.build([row(g, "lisp/okred.el") for g in gates],
                   env={"NELISP_GATE_MUTATION_SLOW_ROW": "100"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        block = self.out.split("gate-mutation: slowest rows\n", 1)[1]
        lines = block.splitlines()
        entries = [l for l in lines if re.match(r"\s+\d+s  ", l)]
        self.assertEqual(len(entries), 10, self.out)
        secs = [int(re.match(r"\s+(\d+)s", l).group(1)) for l in entries]
        self.assertEqual(secs, sorted(secs, reverse=True))
        self.assertIn(" s3 ", entries[0])
        self.assertRegex(self.out, r"gate-mutation: total wall \d+s, warm-up \d+s")
        self.assertNotRegex(self.out, r"\[slow\]")

    def test_summary_present_when_rows_failed(self):
        self.build([row("green", "lisp/green.el"), row("hang", "lisp/hang.el")],
                   env={"NELISP_GATE_MUTATION_ROW_TIMEOUT": "2"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"row 2/2 hang file=lisp/hang\.el \d+s \[TIMEOUT\]")
        self.assertLess(self.out.index("slowest rows"), self.out.index("gate-mutation: FAIL ("))
        self.assertIn("  hang: TIMEOUT", self.out.split("slowest rows")[0])
        self.assertRegex(self.out, r"total wall \d+s, warm-up \d+s")

    def test_warm_up_is_reported_and_runs_before_any_row(self):
        self.build([row("okred", "lisp/okred.el")], rebuild="sleep 1; echo built >> order.log")
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"gate-mutation: warm-up rebuild took [1-9]\d*s")
        self.assertRegex(self.out, r"warm-up [1-9]\d*s")
        self.assertEqual(self.builds(), 1)
        self.assertLess(self.out.index("warm-up rebuild took"), self.out.index("okred: went red"))

    def test_warm_up_failure_stops_with_no_file_touched(self):
        self.build([row("okred", "lisp/okred.el")], rebuild="exit 1")
        result = self.run_harness()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("FAIL (warm-up rebuild failed; no row was run)", self.out)
        self.assertEqual(self.gates_run(), [])
        self.assertRestored("okred")
        self.assertEqual(list((self.root / "tmp").iterdir()), [])

    def test_warm_up_timeout_stops_loudly(self):
        self.build([row("okred", "lisp/okred.el")], rebuild="sleep 300",
                   env={"NELISP_GATE_MUTATION_BUILD_TIMEOUT": "1"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("warm-up rebuild: TIMEOUT after 1s; no row was run", self.out)
        self.assertEqual(self.gates_run(), [])
        self.assertRestored("okred")

    def test_empty_selection_does_no_warm_up(self):
        # More shards than rows: skipped before the warm-up.
        self.build([row("okred", "lisp/okred.el")], env={"NELISP_GATE_MUTATION_SHARD": "5/9"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertEqual(self.builds(), 0, self.out)
        # Every selected row scoped to another target: nothing in scope.
        self.build([row("okred", "lisp/okred.el") + "|windows-aarch64"])
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertEqual(self.builds(), 0, self.out)
        self.assertEqual(self.gates_run(), [])
        self.assertNotIn("warm-up rebuild took", self.out)


class RebuildLog(Fixture):
    """Each rebuild attempt keeps its output in a log; a failed or timed-out
    rebuild shows the tail of that log under a header naming gate and file."""

    def logs(self):
        return sorted((self.root / "target/gate-mutation-logs").glob("rebuild-*.log"))

    def test_failing_rebuild_prints_tail_with_gate_and_file(self):
        self.build([row("emacs-parity", "lisp/rb.el")],
                   rebuild='if grep -q NEW lisp/rb.el; then '
                           'i=1; while [ $i -le 60 ]; do echo "cc-line-$i"; i=$((i+1)); done; '
                           'echo "compile error xyz"; exit 1; fi')
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("HARNESS ERROR (rebuild with the injection failed", self.out)
        self.assertRegex(self.out, r"rebuild log tail \(gate emacs-parity, file lisp/rb\.el, failed twice")
        self.assertIn("    compile error xyz\n", self.out)
        self.assertIn("    cc-line-23\n", self.out)       # last 40 lines: 23..60, message, make error
        self.assertNotIn("cc-line-22\n", self.out)
        names = [p.name for p in self.logs()]
        self.assertTrue(any("emacs-parity-attempt2" in n for n in names), names)
        failed = [p for p in self.logs() if "compile error xyz" in p.read_text()]
        self.assertEqual(len(failed), 2, names)        # both attempts kept
        self.assertRestored("rb")

    def test_timed_out_rebuild_prints_tail(self):
        self.build([row("emacs-parity", "lisp/rb.el")],
                   rebuild='if grep -q NEW lisp/rb.el; then echo "linking forever"; sleep 300; fi',
                   env={"NELISP_GATE_MUTATION_BUILD_TIMEOUT": "2"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"rebuild log tail \(gate emacs-parity, file lisp/rb\.el, TIMEOUT after 2s")
        self.assertIn("    linking forever\n", self.out)
        self.assertIn("emacs-parity: TIMEOUT (rebuild with injection after 2s", self.out)
        self.assertRestored("rb")

    def test_successful_rebuild_prints_no_tail_but_keeps_a_log(self):
        self.build([row("emacs-parity", "lisp/rb.el")], rebuild='echo quiet-build-output')
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("rebuild log tail", self.out)
        self.assertNotIn("quiet-build-output", self.out)
        logs = self.logs()
        self.assertGreaterEqual(len(logs), 2, logs)     # warm-up, injected, restored
        self.assertTrue(all("quiet-build-output" in p.read_text() for p in logs))

    def test_warm_up_failure_names_the_warm_up(self):
        self.build([row("okred", "lisp/okred.el")], rebuild='echo warm broke; exit 1')
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("rebuild log tail (warm-up rebuild", self.out)
        self.assertIn("    warm broke\n", self.out)


if __name__ == "__main__":
    unittest.main()
