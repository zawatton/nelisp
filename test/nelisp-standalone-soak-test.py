#!/usr/bin/env python3
"""Failure detection tests for the Linux /proc standalone soak harness."""
import os
import importlib.util
import subprocess
import sys
import tempfile
import textwrap
import unittest
from unittest import mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
HARNESS = ROOT / "tools" / "nelisp-standalone-soak.py"
_SPEC = importlib.util.spec_from_file_location("nelisp_standalone_soak", HARNESS)
SOAK = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(SOAK)


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
            import sys
            sys.stdin.readline()  # consume the startup form
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
        self.assertRegex(
            result.stderr,
            r"smaps_rollup=(?:unavailable|Rss_kib=\d+ Anonymous_kib=\d+ "
            r"AnonHugePages_kib=\d+ Private_Dirty_kib=\d+)",
        )

    def test_stderr_after_ready_is_failure(self):
        result = self.run_fake("""
            import sys
            sys.stdin.readline()  # consume the startup form
            print("NELISP_SOAK_READY", flush=True)
            sys.stdin.readline()
            print("unexpected diagnostic", file=sys.stderr, flush=True)
            print("NELISP_SOAK_WARMUP_0_1", flush=True)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("stderr", result.stderr)

    def test_child_exit_is_failure(self):
        result = self.run_fake("""
            import sys
            sys.stdin.readline()  # consume the startup form
            print("NELISP_SOAK_READY", flush=True)
        """)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("died", result.stderr)

    def test_huge_stdout_is_bounded_failure(self):
        result = self.run_fake("""
            import sys
            sys.stdin.readline()  # consume the startup form
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
            sys.stdin.readline()  # consume the startup form
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

    def test_smaps_rollup_handles_arbitrary_oserror(self):
        """A vanished/protected proc file cannot replace the original failure."""
        with mock.patch("builtins.open", side_effect=OSError("ESRCH")):
            self.assertIsNone(SOAK.smaps_rollup_kib(12345))

    def test_smaps_rollup_reads_selected_fields(self):
        data = ("1234-5678 ---p 00000000 00:00 0\n"
                "Rss:                120 kB\n"
                "Anonymous:            80 kB\n"
                "AnonHugePages:        64 kB\n"
                "Private_Dirty:        72 kB\n")
        with mock.patch("builtins.open", mock.mock_open(read_data=data)):
            self.assertEqual(
                SOAK.smaps_rollup_kib(12345),
                {"Rss": 120, "Anonymous": 80,
                 "AnonHugePages": 64, "Private_Dirty": 72},
            )

    def test_diagnostic_dir_writes_baseline_and_final(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-soak-diag-") as directory:
            fake = Path(directory) / "fake-child.py"
            fake.write_text("#!" + sys.executable + "\n" + textwrap.dedent(r'''
                import re
                import sys
                for line in sys.stdin:
                    if "NELISP_SOAK_READY" in line:
                        print("NELISP_SOAK_READY", flush=True)
                    else:
                        match = re.search(r'format "([^"]+)%d', line)
                        if match:
                            print(match.group(1).rstrip("_") + "_1", flush=True)
            '''), encoding="utf-8")
            fake.chmod(0o700)
            diag = Path(directory) / "diag"
            result = subprocess.run(
                [sys.executable, str(HARNESS), "--binary", str(fake),
                 "--duration", ".1", "--batch-size", "1", "--timeout", ".5",
                 "--diagnostic-dir", str(diag)],
                capture_output=True, text=True, timeout=5)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((diag / "baseline.smaps").is_file())
            self.assertTrue((diag / "baseline.json").is_file())
            self.assertTrue((diag / "final.smaps").is_file())
            self.assertTrue((diag / "final.json").is_file())
            blocked = Path(directory) / "blocked"
            blocked.write_text("file", encoding="ascii")
            result = subprocess.run(
                [sys.executable, str(HARNESS), "--binary", str(fake),
                 "--duration", ".1", "--batch-size", "1", "--timeout", ".5",
                 "--diagnostic-dir", str(blocked)],
                capture_output=True, text=True, timeout=5)
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_diagnostic_dir_preserves_batch_failure_and_saves_failure(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-soak-failure-diag-") as directory:
            fake = Path(directory) / "fake-child.py"
            fake.write_text("#!" + sys.executable + "\n" + textwrap.dedent(r'''
                import re
                import sys
                warmups = 0
                for line in sys.stdin:
                    if "NELISP_SOAK_READY" in line:
                        print("NELISP_SOAK_READY", flush=True)
                    else:
                        match = re.search(r'format "([^"]+)%d', line)
                        if not match:
                            continue
                        prefix = match.group(1).rstrip("_")
                        if "WARMUP" in prefix:
                            warmups += 1
                            print(prefix + "_1", flush=True)
                        else:
                            print("WRONG_REPLY", flush=True)
            '''), encoding="utf-8")
            fake.chmod(0o700)
            diag = Path(directory) / "diag"
            command = [sys.executable, str(HARNESS), "--binary", str(fake),
                       "--duration", ".1", "--batch-size", "1", "--timeout", ".5",
                       "--diagnostic-dir", str(diag)]
            result = subprocess.run(command, capture_output=True, text=True,
                                    timeout=5)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("unexpected REPL response", result.stderr)
            self.assertTrue((diag / "baseline.smaps").is_file())
            self.assertTrue((diag / "baseline.json").is_file())
            self.assertTrue((diag / "failure.smaps").is_file())
            self.assertTrue((diag / "failure.json").is_file())
            import json
            metrics = json.loads((diag / "failure.json").read_text(encoding="ascii"))
            self.assertGreater(metrics["pid"], 0)
            self.assertEqual(metrics["batches"], 0)
            self.assertGreater(metrics["start_rss_kib"], 0)

            blocked = Path(directory) / "blocked"
            blocked.write_text("file", encoding="ascii")
            result = subprocess.run(command[:-1] + [str(blocked)],
                                    capture_output=True, text=True, timeout=5)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("unexpected REPL response", result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
