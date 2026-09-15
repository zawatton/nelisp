"""Benchmark completion, input identity, and bounded execution contracts."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class ProjectBench(unittest.TestCase):
    @unittest.skipUnless(sys.platform.startswith("linux"), "uses /proc to distinguish zombies")
    def test_owned_workers_stop_after_success_and_timeout(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-bench-workers-") as directory:
            root = Path(directory)
            subprocess.run([sys.executable, str(CLI), "new", "sample"], cwd=root,
                           capture_output=True, check=True)
            project = root / "sample"
            fake = root / "runtime"
            fake.write_text('#!/usr/bin/env python3\n'
                            'import os, re, subprocess, sys, time\n'
                            'from pathlib import Path\n'
                            'child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])\n'
                            'Path("worker.pid").write_text(str(child.pid))\n'
                            'if os.environ.get("BENCH_TEST_HANG"): time.sleep(60)\n'
                            'marker = re.search(rb"NELISP_BENCH_[0-9a-f]+", Path(sys.argv[-1]).read_bytes()).group()\n'
                            'print("\\n" + marker.decode())\n')
            fake.chmod(0o755)
            for hanging in (False, True):
                env = dict(os.environ, NELISP_BIN=str(fake), BENCH_TEST_HANG="1" if hanging else "")
                result = subprocess.run([sys.executable, str(CLI), "bench", "--samples", "1",
                                         "--warmup", "0", "--timeout", "0.5", "--json"],
                                        cwd=project, env=env, capture_output=True, text=True, timeout=5)
                self.assertEqual(result.returncode == 0, not hanging, result.stderr)
                pid = int((project / "worker.pid").read_text())
                status = Path("/proc") / str(pid) / "status"
                stopped = False
                for _ in range(100):
                    try:
                        stopped = "\nState:\tZ" in status.read_text()
                    except FileNotFoundError:
                        stopped = True
                    if stopped:
                        break
                    time.sleep(0.01)
                self.assertTrue(stopped, f"benchmark left worker {pid} running")

    def test_native_samples_and_failures(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-bench-test-") as directory:
            root = Path(directory)

            def cli(*args, cwd=root):
                return subprocess.run([sys.executable, str(CLI), *args], cwd=cwd,
                                      capture_output=True, text=True, timeout=20)

            self.assertEqual(cli("new", "sample").returncode, 0)
            project = root / "sample"
            source = project / "src/main.nl"
            source.write_text('(defun main () (princ "measured"))\n')
            result = cli("bench", "--samples", "3", "--warmup", "1", "--json", cwd=project)
            self.assertEqual(result.returncode, 0, result.stderr)
            report = json.loads(result.stdout)
            self.assertEqual(report["scope"], "process-startup-load-entry-exit")
            self.assertEqual(report["source_sha256"], hashlib.sha256(source.read_bytes()).hexdigest())
            self.assertEqual(len(report["samples_ns"]), 3)
            self.assertEqual(report["warmup_runs"], 1)
            self.assertTrue(all(x > 0 for x in report["samples_ns"]))
            self.assertEqual(report["median_ns"], sorted(report["samples_ns"])[1])
            self.assertEqual(len(report["runtime_sha256"]), 64)
            for invalid in ("0", "-1", "10001"):
                self.assertNotEqual(cli("bench", "--samples", invalid, cwd=project).returncode, 0)
            for body in ('(exit 0)', '(error "broken")'):
                source.write_text(f'(defun main () {body})\n')
                failed = cli("bench", "--samples", "1", "--warmup", "0", "--json", cwd=project)
                self.assertNotEqual(failed.returncode, 0)
                self.assertEqual(failed.stdout, "")
                if "broken" in body:
                    self.assertIn("broken", failed.stderr)
            source.write_text('(defun main () (while t nil))\n')
            timed = cli("bench", "--timeout", "0.1", "--warmup", "0", cwd=project)
            self.assertNotEqual(timed.returncode, 0)
            self.assertIn("timeout", timed.stderr)
            for invalid in ("nan", "inf", "0"):
                self.assertNotEqual(cli("bench", "--timeout", invalid, cwd=project).returncode, 0)


if __name__ == "__main__":
    unittest.main()
