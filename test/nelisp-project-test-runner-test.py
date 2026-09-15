"""Native selected-test execution and machine-readable failure contracts."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class ProjectTestRunner(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="nelisp test selection 日本語 ")
        self.addCleanup(self.directory.cleanup)
        home = Path(self.directory.name)
        result = subprocess.run([sys.executable, str(CLI), "new", "hello"], cwd=home,
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.project = home / "hello"

    def run_tests(self, *args):
        return subprocess.run([sys.executable, str(CLI), "test", *args], cwd=self.project,
                              capture_output=True, text=True, timeout=20)

    def test_literal_unicode_selection_and_zero_matches(self):
        (self.project / "test/selection-test.el").write_text('''
(ert-deftest 日本.+-selected () (should (= 42 42)))
(ert-deftest 日本-other () (write-region "ran" nil "unselected") (should nil))
''', encoding="utf-8")
        result = self.run_tests("--filter", "日本.+", "--json")
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual((report["status"], report["passed"], report["failed"], report["total"]),
                         ("passed", 1, 0, 1))
        self.assertFalse((self.project / "unselected").exists())
        self.assertEqual(result.stderr, "")
        for selector in ("NO-MATCH", '\"); (write-region "bad" nil "injected") ;'):
            result = self.run_tests("--filter", selector, "--json")
            self.assertEqual(result.returncode, 1)
            report = json.loads(result.stdout)
            self.assertEqual((report["status"], report["total"]), ("no-tests", 0))
            self.assertFalse((self.project / "injected").exists())

    def test_json_failure_startup_and_input_errors(self):
        source = self.project / "src/main.nl"
        source.write_text(source.read_text() + '\n(princ "startup output\\n")\n')
        (self.project / "test/failure-test.el").write_text(
            '(ert-deftest deliberately-fails () (should nil))\n')
        result = self.run_tests("--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual((report["status"], report["passed"], report["failed"]), ("failed", 1, 1))
        self.assertIn("startup output", report["stdout"])
        self.assertIn("deliberately-fails", report["stdout"])
        source.write_text(source.read_text() + '\n(error "startup failure")\n')
        result = self.run_tests("--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "incomplete")
        self.assertIsNone(report["total"])
        (self.project / "nelisp.toml").write_text("invalid manifest")
        result = self.run_tests("--json")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(json.loads(result.stdout)["status"], "error")

    def test_unfiltered_run_ignores_inherited_selector(self):
        result = subprocess.run([sys.executable, str(CLI), "test"], cwd=self.project,
                                env=dict(os.environ, NELISP_PROJECT_TEST_FILTER="NO-MATCH"),
                                capture_output=True, text=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("1 passed, 0 failed", result.stdout)


if __name__ == "__main__":
    unittest.main()
