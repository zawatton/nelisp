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

    def run_tests(self, *args, env=None):
        return subprocess.run([sys.executable, str(CLI), "test", *args], cwd=self.project,
                              capture_output=True, text=True, timeout=20,
                              env=dict(os.environ, **env) if env else None)

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

    def test_jobs_runs_each_selected_test_once_across_processes(self):
        (self.project / "test/jobs-test.el").write_text('''
(defun nelisp-test-jobs-record (name)
  (write-region (format "%s" (emacs-pid)) nil
                (expand-file-name name (getenv "NELISP_TEST_JOBS_DIR"))))
(ert-deftest jobs-a () (nelisp-test-jobs-record "jobs-a") (should t))
(ert-deftest jobs-b () (nelisp-test-jobs-record "jobs-b") (should t))
(ert-deftest jobs-c () (nelisp-test-jobs-record "jobs-c") (should t))
(ert-deftest jobs-d () (nelisp-test-jobs-record "jobs-d") (should t))
''', encoding="utf-8")
        names = ("jobs-a", "jobs-b", "jobs-c", "jobs-d")
        multi = Path(self.directory.name) / "pids-multi"
        multi.mkdir()
        result = self.run_tests("--jobs", "4", "--json", env={"NELISP_TEST_JOBS_DIR": str(multi)})
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "passed")
        self.assertEqual(report["jobs"], 4)
        self.assertEqual({path.name for path in multi.iterdir()}, set(names))
        pids = {path.name: path.read_text() for path in multi.iterdir()}
        self.assertEqual(len(set(pids.values())), 4, pids)

        single = Path(self.directory.name) / "pids-single"
        single.mkdir()
        result = self.run_tests("--jobs", "1", "--json", env={"NELISP_TEST_JOBS_DIR": str(single)})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual({path.name for path in single.iterdir()}, set(names))
        self.assertEqual(len({path.read_text() for path in single.iterdir()}), 1)

    def test_jobs_reports_shard_failure_and_keeps_other_shards_passing(self):
        (self.project / "test/jobs-fail-test.el").write_text('''
(ert-deftest jobs-fail-ok-1 () (should t))
(ert-deftest jobs-fail-ok-2 () (should t))
(ert-deftest jobs-fail-bad () (should nil))
''', encoding="utf-8")
        result = self.run_tests("--jobs", "3", "--exact", "jobs-fail-ok-1", "--exact", "jobs-fail-ok-2",
                                "--exact", "jobs-fail-bad", "--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "failed")
        self.assertEqual((report["passed"], report["failed"], report["total"]), (2, 1, 3))
        self.assertEqual({case["name"]: case["status"] for case in report["cases"]},
                         {"jobs-fail-ok-1": "passed", "jobs-fail-ok-2": "passed", "jobs-fail-bad": "failed"})

    def test_jobs_fails_with_error_when_shard_registers_undeclared_test(self):
        (self.project / "test/jobs-dynamic-test.el").write_text('''
(ert-deftest jobs-dyn-static () (should t))
(eval (quote (ert-deftest jobs-dyn-hidden () (should t))))
''', encoding="utf-8")
        result = self.run_tests("--jobs", "2", "--json")
        self.assertEqual(result.returncode, 2)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "error")
        self.assertIn("top-level ert-deftest", report["stderr"])

    def test_jobs_reports_missing_exact_name_as_incomplete(self):
        (self.project / "test/jobs-missing-test.el").write_text('''
(ert-deftest jobs-missing-real () (should t))
''', encoding="utf-8")
        result = self.run_tests("--jobs", "2", "--exact", "jobs-missing-real",
                                "--exact", "jobs-missing-typo", "--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "incomplete")
        self.assertEqual(report["missing"], ["jobs-missing-typo"])
        text_result = self.run_tests("--jobs", "2", "--exact", "jobs-missing-real",
                                     "--exact", "jobs-missing-typo")
        self.assertEqual(text_result.returncode, 1)
        self.assertIn("jobs-missing-typo", text_result.stderr)

    def test_jobs_zero_matches_still_surfaces_startup_error(self):
        source = self.project / "src/main.nl"
        source.write_text(source.read_text() + '\n(error "jobs zero-match startup failure")\n')
        result = self.run_tests("--jobs", "2", "--filter", "NO-SUCH-TEST-NAME", "--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "incomplete")
        self.assertIsNone(report["total"])
        self.assertIn("jobs zero-match startup failure", report["stderr"])

    def test_jobs_shard_startup_error_is_incomplete_not_registration_mismatch(self):
        (self.project / "test/jobs-startup-test.el").write_text('''
(ert-deftest jobs-startup-a () (should t))
(ert-deftest jobs-startup-b () (should t))
''', encoding="utf-8")
        source = self.project / "src/main.nl"
        source.write_text(source.read_text() + '\n(error "jobs shard startup failure")\n')
        result = self.run_tests("--jobs", "2", "--exact", "jobs-startup-a",
                                "--exact", "jobs-startup-b", "--json")
        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "incomplete")
        self.assertIn("jobs shard startup failure", report["stderr"])

    def test_jobs_matches_serial_exact_batch(self):
        (self.project / "test/jobs-parity-test.el").write_text('''
(ert-deftest jobs-parity-a () (should t))
(ert-deftest jobs-parity-b () (should t))
(ert-deftest jobs-parity-c () (should nil))
(ert-deftest jobs-parity-d () (should t))
''', encoding="utf-8")
        exact_args = []
        for name in ("jobs-parity-a", "jobs-parity-b", "jobs-parity-c", "jobs-parity-d"):
            exact_args += ["--exact", name]
        serial = self.run_tests(*exact_args, "--json")
        sharded = self.run_tests("--jobs", "2", *exact_args, "--json")
        self.assertEqual(serial.returncode, 1, serial.stderr)
        self.assertEqual(sharded.returncode, 1, sharded.stderr)
        serial_report = json.loads(serial.stdout)
        sharded_report = json.loads(sharded.stdout)
        self.assertEqual((serial_report["passed"], serial_report["failed"], serial_report["total"]),
                         (sharded_report["passed"], sharded_report["failed"], sharded_report["total"]))
        self.assertEqual([(case["name"], case["status"]) for case in serial_report["cases"]],
                         [(case["name"], case["status"]) for case in sharded_report["cases"]])

    def test_jobs_input_errors(self):
        cases = [
            (["--jobs", "0"], "--jobs must be a positive integer"),
            (["--jobs", "-1"], "--jobs must be a positive integer"),
            (["--jobs", "abc"], "invalid int value"),
            (["--jobs", "2", "--list"], "--jobs is not compatible with --list"),
        ]
        for args, expected in cases:
            with self.subTest(args=args):
                result = self.run_tests(*args)
                self.assertEqual(result.returncode, 2)
                self.assertIn(expected, result.stderr)


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
