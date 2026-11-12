"""Hermetic tests for the per-row budgets and timing report of the
gate-mutation harness.  A fixture repository with a fake Makefile and a fake
row table stands in for the real tree; no real gate or build runs.

Usage: python3 test/nelisp-gate-mutation-row-timeout-test.py [RowTimeout|RowTimingReport|RebuildLog|
       RestoreByCopy|InjectionReachedBinary|PrivateUnitCache|ShardBudget]

NELISP_GATE_MUTATION_UNDER_TEST may name another copy of the harness; it
exists so a deliberately broken copy can prove these tests are able to fail
(NELISP_MERGE_UNDER_TEST does the same for the shard merge script).
"""
import json
import signal
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
MERGE = Path(os.environ.get("NELISP_MERGE_UNDER_TEST",
                            ROOT / "tools/nelisp-merge-gate-reports.sh"))
HARNESS = Path(os.environ.get("NELISP_GATE_MUTATION_UNDER_TEST",
                              ROOT / "tools/nelisp-gate-mutation.sh"))

# Recipe lines start with a TAB.  Behaviour is chosen by the injected text
# ("NEW" in the mutated file), so a clean run is always fast and green.
MAKEFILE = r"""
standalone-reader:
	@echo b >> builds.log; eval "$$FAKE_REBUILD" && mkdir -p target && { if [ -n "$$FAKE_BIN" ]; then printf %s "$$FAKE_BIN" > target/nelisp; else cat lisp/*.el > target/nelisp; fi; } && { if [ -n "$$FAKE_COLD" ]; then printf %s "$$FAKE_COLD" > target/nelisp.cold; else cat lisp/*.el > target/nelisp.cold; fi; } && { [ -n "$$FAKE_NO_VERSION" ] || printf '{"version":"%s"}' "$${NELISP_VERSION:-fakever-dirty}" > target/nelisp.version.json; } && echo "$${NELISP_VERSION:-unset}" >> version.log && echo stamp > target/nelisp.build-stamp
standalone-reader-status:
	@echo s >> status.log; if [ -n "$$FAKE_STATUS" ]; then echo "$$FAKE_STATUS"; else echo "standalone-reader: up to date (fake)"; fi
binary-size-ratchet:
	@echo ratchet >> gates.log; ! grep -q "status: #" Makefile
ffi-dsl:
	@echo ffi-dsl >> gates.log; mkdir -p target; echo variant > target/nelisp-artifact-runtime.el; sleep $${FAKE_VARIANT_SLEEP:-0}; ! grep -q NEW lisp/okred.el
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
	@echo emacs-parity >> gates.log; echo "$${NELISP_STANDALONE_UNIT_CACHE_DIR:-unset}" >> gateenv.log; echo "$$(grep -c NEW target/nelisp)" >> bins.log; ls target/gate-mutation-units 2>/dev/null | wc -l >> scratchseen.log; ! grep -q NEW lisp/rb.el
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
        shutil.copy2(ROOT / "tools/nelisp-standalone-reader-build.sh",
                     root / "tools/nelisp-standalone-reader-build.sh")
        shutil.copy2(ROOT / "tools/gate-mutation-shard.awk", root / "tools/gate-mutation-shard.awk")
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
        # warm-up and the timed-out mutated build.  The clean binary is put
        # back by copy (no recovery rebuild), so the next row never sees the
        # partial one.
        self.assertEqual(self.builds(), 2, self.out)
        self.assertNotIn("RESTORE-BY-COPY NOT CONFIRMED", self.out)
        self.assertEqual((self.root / "target/nelisp").read_bytes(),
                         (self.root / "target/gate-mutation-baseline/target/nelisp").read_bytes())

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


CLEAN_NAMES = ["hang", "okred", "spawn", "skipbase", "green", "rb"]


class RestoreByCopy(Fixture):
    """S7.1: a rebuild row puts the clean binary back by copy, not by a second build."""

    def test_restore_does_not_rebuild_and_gives_the_baseline_binary(self):
        self.build([row("emacs-parity", "lisp/rb.el"), row("okred", "lisp/okred.el")])
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertEqual(self.builds(), 2, self.out)        # warm-up + injected, no restore build
        self.assertNotIn("RESTORE-BY-COPY NOT CONFIRMED", self.out)
        baseline = (self.root / "target/gate-mutation-baseline/target/nelisp").read_bytes()
        self.assertNotIn(b"NEW", baseline)
        self.assertEqual((self.root / "target/nelisp").read_bytes(), baseline)
        self.assertGreaterEqual(len((self.root / "status.log").read_text().split()), 1)
        # Plain cp: the restored binary is newer than the restored source.
        self.assertGreaterEqual((self.root / "target/nelisp").stat().st_mtime_ns,
                                (self.root / "lisp/rb.el").stat().st_mtime_ns)
        self.assertRestored("rb", "okred")

    def test_status_failure_falls_back_to_a_rebuild_loudly(self):
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"FAKE_STATUS": "stale: key changed (fake)"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"RESTORE-BY-COPY NOT CONFIRMED \(after the row, gate emacs-parity\): "
                                   r"stale: key changed \(fake\); falling back to a full rebuild")
        self.assertEqual(self.builds(), 3, self.out)        # the fallback rebuild
        self.assertNotIn(b"NEW", (self.root / "target/nelisp").read_bytes())

    def test_variant_gate_companions_are_put_back_absent(self):
        self.build([row("ffi-dsl", "lisp/okred.el")])
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertFalse((self.root / "target/nelisp-artifact-runtime.el").exists(),
                         "a companion that was absent in the baseline must be absent again")
        self.assertNotIn(b"NEW", (self.root / "target/nelisp").read_bytes())
        self.assertIn("ffi-dsl: went red as it should", self.out)

    def test_failed_injected_build_restores_the_clean_binary_by_copy(self):
        self.build([row("emacs-parity", "lisp/rb.el"), row("okred", "lisp/okred.el")],
                   rebuild="if grep -q NEW lisp/rb.el; then echo partial > target/nelisp; exit 1; fi")
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("HARNESS ERROR (rebuild with the injection failed", self.out)
        self.assertNotIn(b"partial", (self.root / "target/nelisp").read_bytes())
        self.assertIn("okred: went red as it should", self.out)


class InjectionReachedBinary(Fixture):
    """S7.2: an injected build that gives the baseline bytes is a harness error."""

    def test_injected_build_with_baseline_bytes_is_a_harness_error(self):
        self.build([row("emacs-parity", "lisp/rb.el"), row("okred", "lisp/okred.el")],
                   env={"FAKE_BIN": "fixed-binary", "FAKE_COLD": "fixed-cold"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("  emacs-parity: HARNESS ERROR (injection did not reach the binary; file lisp/rb.el)", self.out)
        self.assertNotIn("emacs-parity: went red", self.out)
        self.assertNotIn("emacs-parity: STAYED GREEN", self.out)
        self.assertEqual(self.gates_run(), ["okred"], self.out)     # the gate never ran on it
        self.assertIn("okred: went red as it should", self.out)
        self.assertIn("passed=1 failed=1", self.out)
        self.assertRestored("rb", "okred")

    def test_injected_build_is_pinned_to_the_baseline_version(self):
        # the fake build embeds NELISP_VERSION or "fakever-dirty"; the baseline
        # carries the latter, so the injected build must be handed exactly it
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"FAKE_BIN": "fixed-binary", "FAKE_COLD": "fixed-cold"})
        result = self.run_harness()
        self.assertEqual((self.root / "version.log").read_text().split(), ["unset", "fakever-dirty"], self.out)
        # pinned version + identical bytes: the check fires
        self.assertIn("HARNESS ERROR (injection did not reach the binary", self.out)

    def test_missing_baseline_version_is_a_harness_error(self):
        self.build([row("emacs-parity", "lisp/rb.el"), row("okred", "lisp/okred.el")],
                   env={"FAKE_NO_VERSION": "1"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn('emacs-parity: HARNESS ERROR (no readable "version"', self.out)
        self.assertEqual(self.builds(), 1, self.out)                 # warm-up only
        self.assertIn("okred: went red as it should", self.out)
        self.assertRestored("rb")

    def test_same_binary_but_a_changed_companion_is_not_an_error(self):
        # the check covers the whole set except the stamp, not the binary alone
        self.build([row("emacs-parity", "lisp/rb.el")], env={"FAKE_BIN": "fixed-binary"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("HARNESS ERROR", self.out)
        self.assertIn("emacs-parity: went red as it should", self.out)

    def test_key_file_row_with_an_identical_set_passes_the_check(self):
        # Makefile is a stamp KEY_FILE: it need not change the build output.
        self.build(["binary-size-ratchet|Makefile|s/^standalone-reader-status:/standalone-reader-status: #/|key file"],
                   env={"FAKE_BIN": "fixed-binary", "FAKE_COLD": "fixed-cold"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("HARNESS ERROR", self.out)
        self.assertIn("binary-size-ratchet: injection check skipped (Makefile is a stamp KEY_FILE", self.out)
        self.assertIn("binary-size-ratchet: went red as it should", self.out)

    def test_variant_gate_row_does_no_default_rebuild_but_still_restores(self):
        self.build([row("ffi-dsl", "lisp/okred.el")])
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertEqual(self.builds(), 1, self.out)                # warm-up only
        self.assertIn("ffi-dsl: no default rebuild", self.out)
        self.assertTrue((self.root / "status.log").exists())         # the restore ran its status check
        self.assertFalse((self.root / "target/nelisp-artifact-runtime.el").exists())

    def test_row_on_a_file_that_is_not_a_build_input_does_not_rebuild(self):
        (Path(self.build([row("emacs-parity", "tools/side.txt")])) / "tools/side.txt").write_text("OLD\n")
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)         # gate reads lisp/rb.el: stays green
        self.assertIn("  emacs-parity: no rebuild (tools/side.txt is not a build input)", self.out)
        self.assertEqual(self.builds(), 1, self.out)                # warm-up only
        self.assertEqual((self.root / "tools/side.txt").read_text(), "OLD\n")


class PrivateUnitCache(Fixture):
    """S7.3: each injected build gets a hardlink copy of the unit cache."""

    REBUILD = (
        'echo "${NELISP_STANDALONE_UNIT_CACHE_DIR:-unset}" >> cachedir.log; '
        'D="$NELISP_STANDALONE_UNIT_CACHE_DIR"; '
        'if [ -n "$D" ]; then '
        'stat -c %h "$D/linux-x86_64-tco/a.unit" >> links.log; '
        'echo built > "$D/linux-x86_64-tco/b.unit"; rm -f "$D/linux-x86_64-tco/a.unit"; '
        'if grep -q NEW lisp/rb.el && [ -n "$HANG_IN_BUILD" ]; then sleep 300; fi; fi')

    def setUpCache(self, **env):
        self.build([row("emacs-parity", "lisp/rb.el")], rebuild=self.REBUILD, env=env)
        part = self.root / "target/standalone-units/linux-x86_64-tco"
        part.mkdir(parents=True)
        (part / "a.unit").write_bytes(b"unit-a\n")
        return part

    def test_injected_build_uses_a_private_hardlink_copy(self):
        part = self.setUpCache()
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        dirs = (self.root / "cachedir.log").read_text().split()
        self.assertEqual(dirs[0], "unset")                          # the warm-up build is not redirected
        self.assertEqual(len(dirs), 2, dirs)
        self.assertTrue(dirs[1].endswith("/target/gate-mutation-units/row-1"), dirs)
        self.assertEqual((self.root / "links.log").read_text().split(), ["2"])   # hardlinked, not copied
        # the baseline cache is byte-identical afterwards: no new file, a.unit intact
        self.assertEqual(sorted(p.name for p in part.iterdir()), ["a.unit"])
        self.assertEqual((part / "a.unit").read_bytes(), b"unit-a\n")
        self.assertFalse((self.root / "target/gate-mutation-units").exists())
        # ... and the scratch was already gone when the row's gate ran
        self.assertEqual((self.root / "scratchseen.log").read_text().split(), ["0"])
        # the variable does not leak into the row's gate or the later status
        self.assertEqual((self.root / "gateenv.log").read_text().split(), ["unset"])

    def test_scratch_is_removed_after_an_interrupt(self):
        part = self.setUpCache(HANG_IN_BUILD="1")
        proc = subprocess.Popen(["bash", str(self.root / "tools/nelisp-gate-mutation.sh")],
                                cwd=self.root, env=self.env, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        try:
            deadline = time.time() + 30
            log = self.root / "cachedir.log"
            while time.time() < deadline and not (log.exists() and len(log.read_text().split()) >= 2):
                time.sleep(0.2)
            self.assertTrue(log.exists() and len(log.read_text().split()) >= 2, "injected build never started")
            time.sleep(0.5)
            self.assertTrue((self.root / "target/gate-mutation-units/row-1").is_dir())
            proc.send_signal(signal.SIGINT)
            self.out, _ = proc.communicate(timeout=30)
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.communicate()
        self.assertEqual(proc.returncode, 130, self.out)
        self.assertFalse((self.root / "target/gate-mutation-units").exists(), self.out)
        self.assertRestored("rb")
        self.assertEqual(sorted(p.name for p in part.iterdir()), ["a.unit"])
        self.assertEqual((part / "a.unit").read_bytes(), b"unit-a\n")


class ShardBudget(Fixture):
    """S7.4: a shard budget with NOT RUN accounting, and the merge that checks it."""

    def build(self, rows, **kw):
        env = kw.setdefault("env", {})
        env.setdefault("NELISP_GATE_MUTATION_ROW_TIMEOUT", "5")
        return super().build(rows, **kw)

    def sidecar(self):
        return json.loads((self.root / "target/gate-mutation-rows/gate-mutation-rows-0.json").read_text())

    def test_rows_past_the_budget_are_not_run_and_the_shard_fails(self):
        self.build([row("s3", "lisp/okred.el")] * 4, env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "12"})
        result = self.run_harness(limit=60)
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertEqual(self.out.count("NOT RUN (estimate 5s does not fit the "), 2, self.out)
        self.assertEqual(self.out.count("s3: went red as it should"), 2, self.out)
        self.assertRegex(self.out, r"GATE-COUNT checked=2 findings=2 passed=2 failed=0 skipped=0 not_run=2 "
                                   r"selected_total=4 assigned=4")
        self.assertRegex(self.out, r"total wall \d+s, warm-up \d+s, not run 2")
        self.assertIn("FAIL (0 failed, 2 passed, 0 skipped, 2 not run)", self.out)
        self.assertEqual(self.sidecar(), {"schema": "gate-mutation-rows/1", "shard": "0/1",
                                          "selected_total": 4, "assigned": 4, "passed": 2,
                                          "failed": 0, "skipped": 0, "not_run": 2})
        self.assertRestored("okred")

    def test_an_unfit_rebuild_row_does_not_stop_later_cheap_rows(self):
        # worst cases: okred 5 s, emacs-parity (rebuild) 65 s; budget 30 s
        self.build([row("okred", "lisp/okred.el"), row("emacs-parity", "lisp/rb.el"),
                    row("green", "lisp/green.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "30"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertEqual(self.out.count("NOT RUN"), 1, self.out)
        self.assertRegex(self.out, r"emacs-parity: NOT RUN \(estimate 65s does not fit the \d+s left of the 30s shard budget; file lisp/rb\.el\)")
        self.assertIn("okred: went red as it should", self.out)
        self.assertIn("green:", self.out)
        self.assertEqual(self.gates_run(), ["okred", "green"], self.out)   # the row after it ran
        self.assertRegex(self.out, r"GATE-COUNT checked=\d+ findings=\d+ passed=\d+ failed=\d+ skipped=0 not_run=1 "
                                   r"selected_total=3 assigned=3")
        self.assertIn("1 not run)", self.out)
        c = self.sidecar()
        self.assertEqual(c["not_run"], 1)
        self.assertEqual(c["passed"] + c["failed"] + c["skipped"] + c["not_run"], 3)
        # The skipped rebuild row did no work: only the warm-up build ran, and
        # its file was never injected, rebuilt around, or restored.
        self.assertEqual(self.builds(), 1, self.out)
        self.assertNotIn("emacs-parity: went red", self.out)
        self.assertNotIn("emacs-parity:", self.out.replace(
            [l for l in self.out.splitlines() if "emacs-parity: NOT RUN" in l][0], ""), self.out)
        self.assertFalse((self.root / "gateenv.log").exists(), self.out)   # its gate never ran
        self.assertRestored("rb")

    def test_the_check_stays_per_row_when_even_a_cheap_row_does_not_fit(self):
        # s3 takes >= 4 s, so fewer than its 5 s worst case is left afterwards
        self.build([row("s3", "lisp/okred.el"), row("okred", "lisp/okred.el"),
                    row("green", "lisp/green.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "8"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertIn("s3: went red as it should", self.out)
        self.assertRegex(self.out, r"okred: NOT RUN \(estimate 5s does not fit the \d+s left of the 8s shard budget; file lisp/okred\.el\)")
        self.assertRegex(self.out, r"green: NOT RUN \(estimate 5s does not fit the \d+s left of the 8s shard budget; file lisp/green\.el\)")
        self.assertEqual(self.sidecar()["not_run"], 2)

    def test_budget_below_the_first_rows_worst_case_runs_nothing(self):
        self.build([row("okred", "lisp/okred.el"), row("green", "lisp/green.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "3"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertEqual(self.out.count("NOT RUN"), 2, self.out)
        self.assertEqual(self.gates_run(), [])
        self.assertIn("not_run=2", self.out)
        self.assertEqual(self.sidecar()["not_run"], 2)               # the report is still written
        self.assertRestored("okred", "green")

    def test_a_rebuild_row_needs_build_plus_row_budget(self):
        # worst case 60 + 5 = 65 s: does not fit in 30 s, so it is never started
        self.build([row("emacs-parity", "lisp/rb.el")], env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "30"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"emacs-parity: NOT RUN \(estimate 65s does not fit the \d+s left of the 30s shard budget; file lisp/rb\.el\)")
        self.assertEqual(self.builds(), 1, self.out)                 # warm-up only
        self.build([row("emacs-parity", "lisp/rb.el")], env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "100"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("NOT RUN", self.out)

    def test_a_rebuild_row_is_charged_the_measured_estimate(self):
        # build 60 + row 5 = 65 s hard limit; estimate 20 s fits the 30 s budget
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "30",
                        "NELISP_GATE_MUTATION_REBUILD_ESTIMATE": "20"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("NOT RUN", self.out)
        self.assertIn("emacs-parity: went red as it should", self.out)
        # an estimate that does not fit is still refused, with the estimate in the text
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "30",
                        "NELISP_GATE_MUTATION_REBUILD_ESTIMATE": "40"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"emacs-parity: NOT RUN \(estimate 40s does not fit the \d+s left of the 30s shard budget")

    def test_the_estimate_never_exceeds_the_hard_limits(self):
        # override 9999 s > build 60 + row 5: charged 65 s, so 30 s refuses and 100 s runs
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "30",
                        "NELISP_GATE_MUTATION_REBUILD_ESTIMATE": "9999"})
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"emacs-parity: NOT RUN \(estimate 65s does not fit")
        self.build([row("emacs-parity", "lisp/rb.el")],
                   env={"NELISP_GATE_MUTATION_SHARD_BUDGET": "100",
                        "NELISP_GATE_MUTATION_REBUILD_ESTIMATE": "9999"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("NOT RUN", self.out)

    def test_invalid_rebuild_estimate_is_rejected(self):
        for value in ["0", "abc", "-1", "1.5"]:
            self.build([row("okred", "lisp/okred.el")], env={"NELISP_GATE_MUTATION_REBUILD_ESTIMATE": value})
            result = self.run_harness()
            self.assertEqual(result.returncode, 1, self.out)
            self.assertIn("FAIL (NELISP_GATE_MUTATION_REBUILD_ESTIMATE must be a positive integer", self.out)
            self.assertEqual(self.gates_run(), [])

    def test_no_budget_means_no_limit_and_counts_add_up(self):
        self.build([row("okred", "lisp/okred.el"), row("green", "lisp/green.el"),
                    row("okred", "lisp/okred.el") + "|windows-aarch64"])
        result = self.run_harness()
        self.assertNotEqual(result.returncode, 0, self.out)          # green stays green
        self.assertNotIn("NOT RUN", self.out)
        c = self.sidecar()
        self.assertEqual((c["selected_total"], c["assigned"]), (3, 3))
        self.assertEqual(c["passed"] + c["failed"] + c["skipped"] + c["not_run"], 3)

    def test_sharded_run_reports_selected_total_and_its_own_assigned(self):
        self.build([row("okred", "lisp/okred.el")] * 5, env={"NELISP_GATE_MUTATION_SHARD": "0/2"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertEqual(self.sidecar()["selected_total"], 5)
        self.assertEqual(self.sidecar()["assigned"], 3)              # LPT, equal weights: ties go to the lowest bin, so shard 0 gets 3 of 5

    def test_seconds_sidecar_has_every_row_under_its_weight_key(self):
        self.build([row("okred", "lisp/okred.el"), row("green", "lisp/green.el"),
                    row("okred", "lisp/okred.el")])
        self.run_harness()
        side = json.loads((self.root / "target/gate-mutation-rows/gate-mutation-seconds-0.json").read_text())
        self.assertEqual(sorted(side["row_seconds"]),
                         ["green|lisp/green.el|1", "okred|lisp/okred.el|1", "okred|lisp/okred.el|2"])

    def test_invalid_budget_is_rejected(self):
        for value in ["0", "abc", "-1", "1.5"]:
            self.build([row("okred", "lisp/okred.el")], env={"NELISP_GATE_MUTATION_SHARD_BUDGET": value})
            result = self.run_harness()
            self.assertEqual(result.returncode, 1, self.out)
            self.assertIn("FAIL (NELISP_GATE_MUTATION_SHARD_BUDGET must be a positive integer", self.out)
            self.assertEqual(self.gates_run(), [])

    def test_variant_building_gate_gets_the_build_budget(self):
        self.build([row("ffi-dsl", "lisp/okred.el")],
                   env={"NELISP_GATE_MUTATION_ROW_TIMEOUT": "1", "FAKE_VARIANT_SLEEP": "3"})
        result = self.run_harness()
        self.assertEqual(result.returncode, 0, self.out)
        self.assertNotIn("TIMEOUT", self.out)
        self.assertIn("ffi-dsl: went red as it should", self.out)

    # ---- the merge script --------------------------------------------------
    def shard_dir(self, k, status="pass", rows=None, report=True):
        d = self.merge_in / f"gate-reports-mutation-{k}"
        d.mkdir(parents=True, exist_ok=True)
        if report:
            (d / "gate-mutation.json").write_text(json.dumps(
                {"name": "gate-mutation", "status": status, "ran": 1, "passed": 1, "failed": 0,
                 "skipped": 0, "duration_ms": 1, "finished": "2026-11-08T00:00:00Z"}))
        if rows is not None:
            side = self.merge_in / "rows" if self.flat else d
            side.mkdir(parents=True, exist_ok=True)
            (side / f"gate-mutation-rows-{k}.json").write_text(json.dumps(dict(rows, shard=f"{k}/4")))

    def merge(self, expected=4):
        result = subprocess.run(["bash", str(MERGE), "gate-mutation", str(expected),
                                 str(self.merge_in), str(self.merge_out)],
                                capture_output=True, text=True, timeout=30)
        self.out = result.stdout + result.stderr
        return result

    def counts(self, total, assigned, passed=None, failed=0, skipped=0, not_run=0):
        passed = assigned - failed - skipped - not_run if passed is None else passed
        return {"schema": "gate-mutation-rows/1", "selected_total": total, "assigned": assigned,
                "passed": passed, "failed": failed, "skipped": skipped, "not_run": not_run}

    flat = False

    def merge_fixture(self, flat=False):
        self.flat = flat
        d = tempfile.TemporaryDirectory(prefix="nelisp-merge-")
        self.addCleanup(d.cleanup)
        self.merge_in, self.merge_out = Path(d.name) / "in", Path(d.name) / "out"

    def test_merge_accepts_matching_counts(self):
        self.merge_fixture()
        for k, a in enumerate([3, 3, 2, 2]):
            self.shard_dir(k, rows=self.counts(10, a))
        self.assertEqual(self.merge().returncode, 0, self.out)
        self.assertTrue((self.merge_out / "gate-mutation.json").exists())

    def test_merge_rejects_a_missing_shard(self):
        self.merge_fixture()
        for k, a in enumerate([4, 3, 3]):
            self.shard_dir(k, rows=self.counts(10, a))
        result = self.merge()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("found 3 report(s), expected 4", self.out)

    def test_merge_rejects_disagreeing_totals(self):
        self.merge_fixture()
        for k, (t, a) in enumerate([(10, 3), (10, 3), (11, 2), (10, 2)]):
            self.shard_dir(k, rows=self.counts(t, a))
        result = self.merge()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("disagree on the selected total", self.out)

    def test_merge_rejects_assigned_sum_that_differs_from_the_total(self):
        self.merge_fixture()
        for k, a in enumerate([3, 3, 2, 1]):
            self.shard_dir(k, rows=self.counts(10, a))
        result = self.merge()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("assigned 9 rows in total but 10 were selected", self.out)

    def test_merge_rejects_a_shard_that_did_not_account_for_its_rows(self):
        self.merge_fixture()
        for k, a in enumerate([3, 3, 2, 2]):
            rows = self.counts(10, a)
            if k == 1:
                rows["not_run"] -= 1 if rows["not_run"] else 0
                rows["passed"] -= 1                                  # one row unaccounted for
            self.shard_dir(k, rows=rows)
        result = self.merge()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("accounted 2 rows", self.out)

    def test_merge_works_when_the_download_directory_is_flattened(self):
        self.merge_fixture(flat=True)
        for k, a in enumerate([3, 3, 2, 2]):
            self.shard_dir(k, rows=self.counts(10, a))
        self.assertEqual(self.merge().returncode, 0, self.out)
        self.merge_fixture(flat=True)
        for k, a in enumerate([3, 3, 2, 1]):
            self.shard_dir(k, rows=self.counts(10, a))
        self.assertEqual(self.merge().returncode, 1, self.out)

    def test_merge_rejects_a_ran_shard_without_accounting(self):
        self.merge_fixture()
        for k, a in enumerate([3, 3, 2]):
            self.shard_dir(k, rows=self.counts(10, a))
        self.shard_dir(3)
        result = self.merge()
        self.assertEqual(result.returncode, 1, self.out)
        self.assertIn("non-skip shard report(s) but 3 row-accounting file(s)", self.out)

    def test_four_skip_shards_still_pass_and_an_empty_shard_is_exempt(self):
        self.merge_fixture()
        for k in range(4):
            self.shard_dir(k, status="skip")
        self.assertEqual(self.merge().returncode, 0, self.out)
        self.merge_fixture()
        for k, a in enumerate([2, 2, 1]):
            self.shard_dir(k, rows=self.counts(5, a))
        self.shard_dir(3, status="skip")                             # more shards than rows
        self.assertEqual(self.merge().returncode, 0, self.out)


if __name__ == "__main__":
    unittest.main()
