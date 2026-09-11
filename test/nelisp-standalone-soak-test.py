#!/usr/bin/env python3
"""Failure detection tests for the Linux /proc standalone soak harness."""
import os
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
HARNESS = ROOT / "tools" / "nelisp-standalone-soak.py"


class StandaloneSoakHarnessTests(unittest.TestCase):
    """These tests exercise the Linux /proc-oriented subprocess verifier."""

    def run_fake(self, body, *extra):
        with tempfile.TemporaryDirectory(prefix="nelisp-soak-fake-") as directory:
            fake = Path(directory) / "fake-child.py"
            fake.write_text("#!" + sys.executable + "\n" + textwrap.dedent(body), encoding="utf-8")
            fake.chmod(0o700)
            command = [sys.executable, str(HARNESS), "--binary", str(fake),
                       "--duration", ".1", "--batch-size", "1", "--timeout", ".15"]
            command.extend(extra)
            return subprocess.run(command, capture_output=True, text=True, timeout=5)

    def test_partial_stdout_times_out(self):
        result = self.run_fake("""
            import time
            print("NELISP_SOAK_READY", end="", flush=True)
            time.sleep(2)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("timeout", result.stderr)
        self.assertRegex(
            result.stderr,
            r"start_rss_kib=None .*current_rss_kib=None .*peak_rss_kib=None "
            r".*batches=0 .*elapsed_seconds=0\.\d+",
        )

    def test_stderr_after_ready_is_failure(self):
        result = self.run_fake("""
            import sys
            print("NELISP_SOAK_READY", flush=True)
            sys.stdin.readline()
            print("unexpected diagnostic", file=sys.stderr, flush=True)
            print("NELISP_SOAK_WARMUP_0_1", flush=True)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("stderr", result.stderr)

    def test_child_exit_is_failure(self):
        result = self.run_fake("""
            print("NELISP_SOAK_READY", flush=True)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("died", result.stderr)

    def test_huge_stdout_is_bounded_failure(self):
        result = self.run_fake("""
            import sys
            print("NELISP_SOAK_READY", flush=True)
            sys.stdin.readline()
            sys.stdout.write("X" * 5000 + "\\n")
            sys.stdout.flush()
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("output exceeded", result.stderr)

    def test_wrong_reply_is_failure(self):
        result = self.run_fake("""
            import sys
            print("NELISP_SOAK_READY", flush=True)
            for line in sys.stdin:
                print("WRONG_REPLY", flush=True)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertRegex(result.stderr, "unexpected (REPL response|trailing REPL output)")

    def test_nan_duration_is_rejected(self):
        result = self.run_fake("""
            print("NELISP_SOAK_READY", flush=True)
        """, "--duration", "nan")
        self.assertEqual(result.returncode, 2)


if __name__ == "__main__":
    unittest.main(verbosity=2)
