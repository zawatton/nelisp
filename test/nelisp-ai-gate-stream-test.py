"""Hermetic test for `nelisp-ai.sh gate': output must stream while the
wrapped command runs, the command's exit code must still reach the report,
and the GATE-SKIP / GATE-COUNT handling must be unchanged.

Usage: python3 test/nelisp-ai-gate-stream-test.py

NELISP_AI_UNDER_TEST may name another copy of nelisp-ai.sh; it exists so a
copy with the old buffered behaviour can prove the streaming test fails.
"""
import json
import os
from pathlib import Path
import select
import shutil
import subprocess
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = Path(os.environ.get("NELISP_AI_UNDER_TEST", ROOT / "tools/ai/nelisp-ai.sh"))
REPORT = ROOT / "tools/ai/gate-report.sh"


class GateStream(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory(prefix="nelisp-ai-gate-")
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        (self.root / "tools/ai").mkdir(parents=True)
        shutil.copy2(SCRIPT, self.root / "tools/ai/nelisp-ai.sh")
        shutil.copy2(REPORT, self.root / "tools/ai/gate-report.sh")
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("NELISP_GATE")}
        self.env["TMPDIR"] = str(self.root)

    def argv(self, name, script):
        return ["sh", str(self.root / "tools/ai/nelisp-ai.sh"), "gate", name,
                "--", "sh", "-c", script]

    def run_gate(self, name, script):
        result = subprocess.run(self.argv(name, script), cwd=self.root, env=self.env,
                                capture_output=True, text=True, timeout=30)
        self.out = result.stdout + result.stderr
        return result

    def report(self, name):
        return json.loads((self.root / "target/gates" / f"{name}.json").read_text())

    def test_output_arrives_before_the_command_ends(self):
        proc = subprocess.Popen(
            self.argv("stream", "echo EARLY-MARKER; sleep 3; echo GATE-COUNT checked=1 findings=0"),
            cwd=self.root, env=self.env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.addCleanup(proc.kill)
        started = time.monotonic()
        seen = b""
        while b"EARLY-MARKER" not in seen:
            ready, _, _ = select.select([proc.stdout], [], [], 2.0 - (time.monotonic() - started))
            if not ready:
                break
            chunk = os.read(proc.stdout.fileno(), 4096)
            if not chunk:
                break
            seen += chunk
        arrived = time.monotonic() - started
        self.assertIn(b"EARLY-MARKER", seen,
                      f"marker not seen within 2s while the command still ran (got {seen!r})")
        self.assertLess(arrived, 2.0)
        self.assertIsNone(proc.poll(), "marker only arrived after the command had ended")
        rest, _ = proc.communicate(timeout=20)
        text = (seen + rest).decode()
        self.assertEqual(text.count("EARLY-MARKER"), 1, "log was printed twice:\n" + text)
        self.assertEqual(text.count("GATE-COUNT checked=1 findings=0"), 1, text)
        self.assertEqual(proc.returncode, 0, text)
        self.assertEqual(self.report("stream")["status"], "pass")

    def test_passing_command(self):
        result = self.run_gate("ok", "echo to-stdout; echo to-stderr >&2; echo GATE-COUNT checked=4 findings=0")
        self.assertEqual(result.returncode, 0, self.out)
        self.assertIn("to-stdout", self.out)
        self.assertIn("to-stderr", self.out)
        report = self.report("ok")
        self.assertEqual((report["status"], report["ran"], report["failed"]), ("pass", 4, 0))

    def test_exit_1_is_a_failure_with_its_own_verdict_line(self):
        result = self.run_gate("one", "echo 'FAIL: baseline exceeded'; echo GATE-COUNT checked=2 findings=1; exit 1")
        self.assertEqual(result.returncode, 1, self.out)
        report = self.report("one")
        self.assertEqual((report["status"], report["failed"]), ("fail", 1))
        self.assertIn("FAIL: baseline exceeded", report["reason"])

    def test_exit_3_reaches_the_report(self):
        result = self.run_gate("three", "echo GATE-COUNT checked=2 findings=0; exit 3")
        self.assertEqual(result.returncode, 1, self.out)
        report = self.report("three")
        self.assertEqual(report["status"], "fail")
        self.assertIn("exit 3", report["reason"])

    def test_exit_3_without_count_names_the_exit_code(self):
        result = self.run_gate("nocount", "echo hello; exit 3")
        self.assertEqual(result.returncode, 1, self.out)
        report = self.report("nocount")
        self.assertEqual(report["status"], "fail")
        self.assertIn("exit 3", report["reason"])

    def test_clean_exit_without_count_is_still_a_failure(self):
        result = self.run_gate("silent", "echo hello")
        self.assertEqual(result.returncode, 1, self.out)
        self.assertEqual(self.report("silent")["status"], "fail")

    def test_gate_skip_gives_a_skip_report(self):
        result = self.run_gate("skipper", "echo 'GATE-SKIP wrong platform'; exit 0")
        self.assertEqual(result.returncode, 0, self.out)
        report = self.report("skipper")
        self.assertEqual(report["status"], "skip")
        self.assertIn("wrong platform", report["reason"])

    def test_log_temp_files_are_removed(self):
        self.run_gate("tidy", "echo GATE-COUNT checked=1 findings=0")
        leftovers = [p.name for p in self.root.iterdir() if p.name.startswith("tmp.")]
        self.assertEqual(leftovers, [])


if __name__ == "__main__":
    unittest.main()
