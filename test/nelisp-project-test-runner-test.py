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
                                env=dict(os.environ, NELISP_PROJECT_TEST_FILTER="NO-MATCH", NELISP_PROJECT_TEST_EXACT="1"),
                                capture_output=True, text=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("1 passed, 0 failed", result.stdout)
        selected = subprocess.run([sys.executable, str(CLI), "test", "--filter", "greeting"], cwd=self.project,
                                  env=dict(os.environ, NELISP_PROJECT_TEST_EXACT="1"),
                                  capture_output=True, text=True, timeout=20)
        self.assertEqual(selected.returncode, 0, selected.stderr)

    def test_exact_selection_does_not_run_similarly_named_tests(self):
        (self.project / "test/exact-test.nl").write_text('''
(ert-deftest 日本.+ () (should t))
(ert-deftest 日本.+-other () (write-region "ran" nil "unselected") (should nil))
''', encoding="utf-8")
        result = self.run_tests("--exact", "日本.+", "--json")
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual((report["status"], report["total"]), ("passed", 1))
        self.assertFalse((self.project / "unselected").exists())

    def test_batch_selection_keeps_registration_order_and_shared_state(self):
        (self.project / "test/batch-test.nl").write_text('''
(defvar shared 0)
(ert-deftest 日本-setup () (setq shared 42) (should t))
(ert-deftest 日本-check () (should (= shared 42)))
(ert-deftest 日本-fail () (should nil))
(ert-deftest 日本-check-other () (write-region "ran" nil "unselected"))
''', encoding="utf-8")
        result = self.run_tests("--exact", "日本-check", "--exact", "日本-setup", "--exact", "日本-fail", "--json")
        self.assertEqual(result.returncode, 1, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual((report["passed"], report["failed"], report["total"]), (2, 1, 3))
        self.assertEqual([(item["name"], item["status"]) for item in report["cases"]],
                         [("日本-setup", "passed"), ("日本-check", "passed"), ("日本-fail", "failed")])
        self.assertFalse((self.project / "unselected").exists())
        missing = self.run_tests("--exact", "日本-setup", "--exact", "missing", "--json")
        self.assertEqual(missing.returncode, 1)
        self.assertEqual(json.loads(missing.stdout)["status"], "incomplete")
        duplicate = self.run_tests("--exact", "日本-setup", "--exact", "日本-setup", "--json")
        self.assertEqual(duplicate.returncode, 0)
        self.assertEqual(json.loads(duplicate.stdout)["total"], 1)
        # A claimed passing total without corresponding case records is not green.
        incomplete_runner = self.project / "test/incomplete-test.nl"
        incomplete_runner.write_text('(defun nelisp-ert-run-all (&rest ignored) (list 1 0))\n')
        incomplete = self.run_tests("--exact", "日本-setup", "--json")
        self.assertEqual(incomplete.returncode, 1)
        self.assertEqual(json.loads(incomplete.stdout)["status"], "incomplete")

    def test_static_discovery_is_nonexecuting_and_preserves_reader_names(self):
        (self.project / "test/discovery-test.nl").write_text('''
(write-region "ran" nil "discovery-executed")
; (ert-deftest commented () (should nil))
'(ert-deftest quoted () (should nil))
(ert-deftest 日本\\ name () "docs" (should t))
(progn (ert-deftest dynamic () (should t)))
''', encoding="utf-8")
        source = self.project / "src/main.nl"
        source.write_text(source.read_text() + '(error "do not evaluate discovery")\n')
        result = subprocess.run([sys.executable, str(CLI), "test", "--list", "--json"], cwd=self.project,
                                env=dict(os.environ, NELISP_BIN=str(self.project / "missing-runtime")),
                                capture_output=True, text=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual(report["scope"], "source-test-declarations")
        self.assertEqual([item["name"] for item in report["tests"]],
                         ["日本 name", "greeting-returns-message"])
        self.assertFalse((self.project / "discovery-executed").exists())
        self.assertEqual(report["tests"][0]["line"], 5)
        (self.project / "test/discovery-test.nl").write_text('(')
        self.assertEqual(self.run_tests("--list", "--json").returncode, 2)

    def test_batch_output_is_attributed_without_startup_or_neighbor_output(self):
        (self.project / "test/output-test.nl").write_text('''
(princ "SETUP ONLY\\n")
(ert-deftest output-a () (princ "日本 😀 without newline") (should t))
(ert-deftest output-b () (princ "FAILED BODY\\n") (should nil))
''', encoding="utf-8")
        result = self.run_tests("--exact", "output-a", "--exact", "output-b", "--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["cases"][0]["stdout"], "日本 😀 without newline")
        self.assertTrue(report["cases"][1]["stdout"].startswith("FAILED BODY\nFAIL output-b:"))
        self.assertNotIn("SETUP ONLY", report["cases"][1]["stdout"])
        self.assertNotIn("日本", report["cases"][1]["stdout"])
        self.assertIn("SETUP ONLY\n", report["stdout"])
        self.assertEqual(report["before_tests"], "SETUP ONLY\n")
        self.assertEqual(report["stdout"], report["before_tests"] + ''.join(item["before_stdout"] + item["stdout"] for item in report["cases"]) + report["after_tests"])

    def test_output_between_case_boundaries_stays_unattributed(self):
        (self.project / 'test/boundary-test.nl').write_text('''
(defun nelisp-ert-run-all (label filter exact reporter starter)
  (funcall starter (car filter)) (princ "FIRST") (funcall reporter (car filter) t)
  (princ "BETWEEN")
  (funcall starter (cadr filter)) (princ "SECOND") (funcall reporter (cadr filter) t)
  (list 2 0))
''')
        result = self.run_tests('--exact', 'first', '--exact', 'second', '--json')
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual([item['stdout'] for item in report['cases']], ['FIRST', 'SECOND'])
        self.assertEqual(report['cases'][1]['before_stdout'], 'BETWEEN')
        self.assertEqual(report['stdout'], 'FIRSTBETWEENSECOND')

    def test_corrupt_case_boundaries_never_publish_success(self):
        for body in [
            '(funcall reporter (car filter) t)',
            '(funcall starter (car filter)) (funcall starter (car filter)) (funcall reporter (car filter) t)',
            '(funcall starter (car filter)) (funcall reporter "foreign" t)',
            '(funcall starter (car filter))',
        ]:
            with self.subTest(body=body):
                (self.project / 'test/corrupt-test.nl').write_text(
                    '(defun nelisp-ert-run-all (label filter exact reporter starter) ' + body + ' (list 1 0))\n')
                result = self.run_tests('--exact', 'greeting-returns-message', '--json')
                self.assertEqual(result.returncode, 1)
                self.assertEqual(json.loads(result.stdout)['status'], 'incomplete')


if __name__ == "__main__":
    unittest.main()
