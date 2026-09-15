"""End-to-end project CLI contracts, against the standalone interpreter."""

import os
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools" / "nelisp-project.py"


class ProjectCLI(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="nelisp project 日本語 ")
        self.addCleanup(self.temp.cleanup)
        self.cwd = Path(self.temp.name)

    def cli(self, *args, cwd=None, env=None, input=None):
        return subprocess.run(
            [sys.executable, str(CLI), *args], cwd=cwd or self.cwd,
            env=env, input=input, capture_output=True, text=True, timeout=20,
        )

    def create(self):
        result = self.cli("new", "hello")
        self.assertEqual(result.returncode, 0, result.stderr)
        return self.cwd / "hello"

    def test_version_identifies_frontend_without_runtime_or_emacs(self):
        result = self.cli("--version", env=dict(
            os.environ, NELISP_BIN=str(self.cwd / "missing-runtime"),
            EMACS=str(self.cwd / "missing-emacs")))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         f"nelisp {ROOT.joinpath('VERSION').read_text().strip()} (project frontend)\n")
        self.assertEqual(result.stderr, "")

    def test_posix_launcher_version_without_python(self):
        result = subprocess.run([str(ROOT / "bin" / "nelisp"), "--version"],
                                cwd=self.cwd, capture_output=True, text=True,
                                env=dict(os.environ, PYTHON=str(self.cwd / "missing-python")),
                                timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         f"nelisp {ROOT.joinpath('VERSION').read_text().strip()} (project frontend)\n")

    def test_create_run_test_and_edit(self):
        project = self.create()
        result = self.cli("run", cwd=project / "src")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "Hello, world!\n")
        result = self.cli("test", cwd=project)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("1 passed, 0 failed", result.stdout)
        source = project / "src" / "main.nl"
        source.write_text(source.read_text().replace("Hello, world!", "こんにちは"))
        result = self.cli("run", cwd=project)
        self.assertEqual(result.stdout, "こんにちは\n")
        result = self.cli("test", cwd=project)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("1 failed", result.stdout)

    def test_new_layout_and_existing_el_manifest(self):
        project = self.create()
        self.assertTrue((project / "src" / "main.nl").is_file())
        (project / "src" / "main.nl").rename(project / "src" / "main.el")
        manifest = project / "nelisp.toml"
        manifest.write_text(manifest.read_text().replace("src/main.nl", "src/main.el"))
        self.assertEqual(self.cli("run", cwd=project).returncode, 0)
        self.assertEqual(self.cli("test", cwd=project).returncode, 0)
        self.assertEqual(self.cli("check", cwd=project).returncode, 0)

    def test_new_refuses_existing_directory(self):
        project = self.create()
        before = (project / "src" / "main.nl").read_bytes()
        result = self.cli("new", "hello")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((project / "src" / "main.nl").read_bytes(), before)

    def test_format_check_fix_idempotence_and_invalid_batch(self):
        project = self.create()
        source = project / "src" / "main.nl"
        ugly = '(defun greeting ()\n"Hello, world!")\n(defun main ()\n(princ (greeting)))\n'
        source.write_text(ugly)
        result = self.cli("fmt", "--check", cwd=project)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(source.read_text(), ugly)
        result = self.cli("fmt", cwd=project)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('\n  "Hello, world!"', source.read_text())
        self.assertEqual(self.cli("fmt", "--check", cwd=project).returncode, 0)
        formatted = source.read_bytes()
        self.assertEqual(self.cli("fmt", cwd=project).returncode, 0)
        self.assertEqual(source.read_bytes(), formatted)
        source.write_text(ugly)
        (project / "test" / "main-test.el").write_text("(broken\n")
        self.assertNotEqual(self.cli("fmt", cwd=project).returncode, 0)
        self.assertEqual(source.read_text(), ugly, "invalid batch must not partly format files")

    def test_crlf_source_diagnostics_and_format_preserve_string_data(self):
        project = self.create()
        source = project / "src/main.nl"
        source.write_bytes(b'(defconst lines "first\r\nsecond")\r\n'
                           b'(defun greeting () "Hello, world!")\r\n'
                           b'(defun main () (princ (greeting)))\r\n; trailing comment\r\n\r\n')
        checked = self.cli("check", "--json", cwd=project)
        self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
        self.assertEqual(json.loads(checked.stdout)["status"], "ok")
        formatted = self.cli("fmt", cwd=project)
        self.assertEqual(formatted.returncode, 0, formatted.stderr)
        self.assertIn(b'"first\r\nsecond"', source.read_bytes())

    def test_check_reports_all_syntax_locations_without_execution(self):
        project = self.create()
        source = project / "src" / "main.nl"
        source.write_text('; 日本語\n(defun broken (\n')
        other = project / "src" / "other.el"
        other.write_text('(write-region "bad" nil "host-ran")\n)\n')
        before = {p: p.read_bytes() for p in (source, other)}
        result = self.cli("check", "--json", cwd=project)
        self.assertEqual(result.returncode, 1, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual(report["scope"], "syntax")
        self.assertEqual(report["checked_files"], 3)
        self.assertEqual(len(report["diagnostics"]), 2)
        for diagnostic in report["diagnostics"]:
            self.assertEqual((diagnostic["line"], diagnostic["column"]), (2, 1))
            self.assertIn(diagnostic["path"], ("src/main.nl", "src/other.el"))
            self.assertEqual(diagnostic["severity"], "error")
        self.assertFalse((project / "host-ran").exists())
        self.assertEqual({p: p.read_bytes() for p in before}, before)
        source.write_text('(defun main () (unknown-function))\n')
        other.unlink()
        result = self.cli("check", "--json", cwd=project)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["diagnostics"], [])
        # Syntax-only success must not be sold as name resolution/type checking.
        self.assertIn("syntax", self.cli("check", cwd=project).stdout)

    def test_check_unicode_column_and_nl_tests(self):
        project = self.create()
        source = project / "src" / "main.nl"
        source.write_text('"日本語" )\n')
        result = self.cli("check", "--json", cwd=project)
        report = json.loads(result.stdout)
        self.assertEqual(result.returncode, 1)
        self.assertEqual((report["diagnostics"][0]["line"], report["diagnostics"][0]["column"]), (1, 7))
        source.write_text('(defun greeting () "Hello, world!")\n')
        (project / "test" / "main-test.el").rename(project / "test" / "main-test.nl")
        self.assertEqual(self.cli("test", cwd=project).returncode, 0)

    def test_check_json_environment_failure_is_structured(self):
        project = self.create()
        result = self.cli("check", "--json", cwd=project,
                          env=dict(os.environ, EMACS=str(self.cwd / "absent-emacs")))
        self.assertEqual(result.returncode, 2)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "error")
        self.assertEqual(report["checked_files"], 0)

    def test_clean_preserves_unrelated_files_and_needs_no_runtime(self):
        project = self.create()
        target = project / "target"
        target.mkdir()
        for name in ("hello", "hello.build.json", "hello.build.log", "keep"):
            (target / name).write_text("owned fixture")
        release = target / "release"
        release.mkdir()
        for name in ("hello", "hello.build.json", "hello.build.log", "keep"):
            (release / name).write_text("owned fixture")
        profiled = target / "profile"
        profiled.mkdir()
        for name in ("hello", "hello.build.json", "hello.build.log", "keep"):
            (profiled / name).write_text("owned fixture")
        debug = target / "debug"
        debug.mkdir()
        for name in ("hello", "hello.build.json", "hello.build.log", "hello.debug.json", "keep"):
            (debug / name).write_text("owned fixture")
        source = project / "src" / "main.nl"
        before = source.read_bytes()
        result = self.cli("clean", cwd=project,
                          env=dict(os.environ, NELISP_BIN=str(self.cwd / "missing")))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(sorted(path.name for path in target.iterdir()), ["debug", "keep", "profile", "release"])
        self.assertEqual([path.name for path in release.iterdir()], ["keep"])
        self.assertEqual([path.name for path in profiled.iterdir()], ["keep"])
        self.assertEqual([path.name for path in debug.iterdir()], ["keep"])
        self.assertEqual(source.read_bytes(), before)
        source.unlink()
        self.assertEqual(self.cli("clean", cwd=project).returncode, 0)

    @unittest.skipIf(os.name != "posix", "POSIX symlink contract")
    def test_clean_rejects_external_target(self):
        project = self.create()
        outside = self.cwd / "external"
        outside.mkdir()
        (outside / "hello").write_text("keep")
        (project / "target").symlink_to(outside, target_is_directory=True)
        self.assertNotEqual(self.cli("clean", cwd=project).returncode, 0)
        self.assertEqual((outside / "hello").read_text(), "keep")

    @unittest.skipIf(os.name != "posix", "POSIX symlink contract")
    def test_release_output_rejects_external_directory_before_writing(self):
        project = self.create()
        target = project / "target"
        target.mkdir()
        (target / "hello").write_text("dev stays")
        outside = self.cwd / "external-release"
        outside.mkdir()
        (outside / "hello").write_text("outside stays")
        for mode in ("release", "profile", "debug"):
            (target / mode).symlink_to(outside, target_is_directory=True)
            for args in (("build", f"--{mode}"), ("clean",)):
                self.assertNotEqual(self.cli(*args, cwd=project).returncode, 0)
                self.assertEqual((target / "hello").read_text(), "dev stays")
                self.assertEqual((outside / "hello").read_text(), "outside stays")
            (target / mode).unlink()

    def test_project_repl_preserves_state_and_does_not_call_main(self):
        project = self.create()
        result = self.cli("repl", cwd=project, input=
                          '(setq saved 17)\n(greeting)\n'
                          '(defun greeting () "変更")\n(greeting)\nsaved\n(exit 0)\n')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('"Hello, world!"', result.stdout)
        self.assertIn('"変更"', result.stdout)
        self.assertEqual(result.stdout.splitlines().count("17"), 2)
        self.assertEqual(result.stderr, "")
        self.assertNotIn("Hello, world!\n", result.stdout)

    def test_project_repl_rejects_startup_errors(self):
        project = self.create()
        for text in ('(error "bad startup")', '(exit 0)', '('):
            (project / "src" / "main.nl").write_text(text)
            result = self.cli("repl", cwd=project, input='(princ "USER-INPUT-RAN")\n')
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertNotIn("USER-INPUT-RAN", result.stdout)

    def test_format_preserves_multiline_data_and_does_not_evaluate(self):
        project = self.create()
        source = project / "src" / "main.nl"
        text = '(defun greeting ()\n"日本語\n  preserved")\n(write-region "bad" nil "host-ran")\n'
        source.write_text(text)
        result = self.cli("fmt", cwd=project)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('"日本語\n  preserved"', source.read_text())
        self.assertFalse((project / "host-ran").exists())

    def test_project_repl_pipe_backpressure_and_error_recovery(self):
        project = self.create()
        payload = '(error "recoverable")\n' + ('(princ "' + 'x' * 300 + '")\n') * 300
        result = self.cli("repl", cwd=project, input=payload + '(+ 40 2)\n(exit 0)\n')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("recoverable", result.stderr)
        self.assertGreater(len(result.stdout), 90000)
        self.assertIn("42\n", result.stdout)

    @unittest.skipIf(os.name != "posix", "POSIX process cleanup")
    def test_project_repl_startup_timeout_reaps_runtime(self):
        project = self.create()
        fake = self.cwd / "sleepy-runtime"
        pid_file = self.cwd / "runtime.pid"
        fake.write_text(f'#!{sys.executable}\nimport os,time,pathlib\n'
                        'pathlib.Path(os.environ["NELISP_TEST_PID_FILE"]).write_text(str(os.getpid()))\n'
                        'time.sleep(60)\n')
        fake.chmod(0o755)
        env = dict(os.environ, NELISP_BIN=str(fake), NELISP_TEST_PID_FILE=str(pid_file))
        result = self.cli("repl", "--startup-timeout", "0.5", cwd=project, env=env, input="")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("timed out", result.stderr)
        pid = int(pid_file.read_text())
        with self.assertRaises(ProcessLookupError):
            os.kill(pid, 0)

    def test_zero_tests_and_premature_exit_fail(self):
        project = self.create()
        test = project / "test" / "main-test.el"
        for source in ("; no tests\n", "(exit 0)\n", '(error "bad registration")\n'):
            with self.subTest(source=source):
                test.write_text(source)
                result = self.cli("test", cwd=project)
                self.assertNotEqual(result.returncode, 0, result.stdout)
        test.unlink()
        self.assertNotEqual(self.cli("test", cwd=project).returncode, 0)

    def test_source_errors_fail(self):
        project = self.create()
        source = project / "src" / "main.nl"
        for content in ('(error "startup failed")\n', '(defun main () (error "boom"))', '('):
            with self.subTest(content=content):
                source.write_text(content)
                self.assertNotEqual(self.cli("run", cwd=project).returncode, 0)
                self.assertNotEqual(self.cli("test", cwd=project).returncode, 0)

    def test_manifest_and_arguments_rejected(self):
        self.assertNotEqual(self.cli("run").returncode, 0)
        self.assertNotEqual(self.cli("new", "../escape").returncode, 0)
        self.assertNotEqual(self.cli("unknown").returncode, 0)
        project = self.create()
        self.assertNotEqual(self.cli("test", "ignored", cwd=project).returncode, 0)
        manifest = project / "nelisp.toml"
        valid = manifest.read_text()
        for bad in ('[package\n', valid + '\n[dependencies]\njson = "1"\n',
                    valid.replace('src/main.nl', '../outside.el'),
                    valid.replace('entry = "main"', 'entry = "(exit 0)"'),
                    valid.replace('version = "0.1.0"', 'version = 1')):
            with self.subTest(manifest=bad):
                manifest.write_text(bad)
                self.assertNotEqual(self.cli("run", cwd=project).returncode, 0)

    def test_missing_runtime_does_not_prevent_new(self):
        env = dict(os.environ, NELISP_BIN=str(self.cwd / "absent"))
        self.assertEqual(self.cli("new", "hello", env=env).returncode, 0)
        result = self.cli("run", cwd=self.cwd / "hello", env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("NELISP_BIN", result.stderr)

    @unittest.skipIf(os.name == "nt", "POSIX launcher contract")
    def test_public_launcher_with_checkout_path(self):
        env = dict(os.environ, PATH=str(ROOT / "bin") + os.pathsep + os.environ["PATH"],
                   PYTHON=sys.executable)
        created = subprocess.run(["nelisp", "new", "hello"], cwd=self.cwd, env=env,
                                 capture_output=True, text=True, timeout=10)
        self.assertEqual(created.returncode, 0, created.stderr)
        evaluated = subprocess.run(["nelisp", "--eval", "(+ 40 2)"], cwd=self.cwd, env=env,
                                   capture_output=True, text=True, timeout=20)
        self.assertEqual(evaluated.returncode, 0, evaluated.stderr)
        self.assertEqual(evaluated.stdout, "42\n")
        for command in ("run", "test"):
            result = subprocess.run(["nelisp", command], cwd=self.cwd / "hello", env=env,
                                    capture_output=True, text=True, timeout=20)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


@unittest.skipIf(os.name == "nt", "POSIX fake-runtime contract")
class ProjectResultContract(unittest.TestCase):
    """Fast host-only sensitivity check; does not require a runtime build."""

    def test_runtime_result_validation(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-result-") as directory:
            root = Path(directory)
            created = subprocess.run([sys.executable, str(CLI), "new", "hello"],
                                     cwd=root, capture_output=True, text=True)
            self.assertEqual(created.returncode, 0, created.stderr)
            fake = root / "runtime"
            fake.write_text(
                f"#!{sys.executable}\n"
                "import os, pathlib, re, sys\n"
                "text = pathlib.Path(sys.argv[sys.argv.index('--load') + 1]).read_text()\n"
                "marker = re.search(r'NELISP_PROJECT_TEST_[0-9a-f]+', text)[0]\n"
                "mode = os.environ['NELISP_FAKE_RESULT']\n"
                "if mode != 'missing':\n"
                "    print(marker + (' 0 0' if mode == 'zero' else ' 1 1' if mode == 'failed' else ' 1 0'))\n"
                "if mode == 'duplicate': print(marker + ' 1 0')\n"
                "if mode == 'stderr': print('late error', file=sys.stderr)\n"
                "sys.exit(1 if mode == 'exit' else 0)\n",
                encoding="utf-8",
            )
            fake.chmod(0o755)
            for mode in ("ok", "missing", "zero", "failed", "duplicate", "stderr", "exit"):
                with self.subTest(mode=mode):
                    env = dict(os.environ, NELISP_BIN=str(fake), NELISP_FAKE_RESULT=mode)
                    for options in ([], ["--json"]):
                        result = subprocess.run([sys.executable, str(CLI), "test", *options],
                                                cwd=root / "hello", env=env,
                                                capture_output=True, text=True, timeout=5)
                        self.assertEqual(result.returncode == 0, mode == "ok", result.stdout + result.stderr)
                        if options:
                            report = json.loads(result.stdout)
                            self.assertEqual(report["status"] == "passed", mode == "ok")
                            self.assertEqual(result.stderr, "")


if __name__ == "__main__":
    result = unittest.main(verbosity=2, exit=False).result
    checked = result.testsRun - len(result.skipped)
    failed = len({getattr(test, "test_case", test).id()
                  for test, _ in result.failures + result.errors})
    print(f"GATE-COUNT checked={checked} findings={failed} passed={checked - failed} failed={failed}")
    sys.exit(0 if result.wasSuccessful() and checked > 0 else 1)
