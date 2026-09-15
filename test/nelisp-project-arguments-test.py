"""Run and isolated ELF share lossless application arguments and environment."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class ProjectArguments(unittest.TestCase):
    def test_low_level_separator_preserves_strict_validation(self):
        binary = ROOT / "target/nelisp"
        form = '(length (nthcdr 3 nelisp-standalone-argv))'
        with tempfile.TemporaryDirectory(prefix="nelisp-raw-argv-") as directory:
            source = Path(directory) / "program.el"
            source.write_text(form)
            for mode, value in (("--eval", form), ("--load", str(source))):
                for extra, code, output in (([], 0, b"0\n"), (["extra"], 2, None),
                                            ([""], 2, None), (["-"], 2, None), (["---"], 2, None),
                                            (["--", "--help"], 0, b"1\n"), (["--"], 0, b"0\n")):
                    result = subprocess.run([str(binary), mode, value, *extra],
                                            capture_output=True, timeout=10)
                    self.assertEqual(result.returncode, code)
                    self.assertEqual(result.stderr, b"")
                    if output is not None:
                        self.assertEqual(result.stdout, output)

    def test_run_arguments_are_data(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-argv-") as directory:
            root = Path(directory)
            subprocess.run([sys.executable, str(CLI), "new", "hello"], cwd=root,
                           capture_output=True, check=True)
            project = root / "hello"
            (project / "src/main.nl").write_text(
                '(defun main () (princ (getenv "ARG_ENV")) (princ "\\0") '
                '(while command-line-args-left (princ (car command-line-args-left)) '
                '(princ "\\0") (setq command-line-args-left (cdr command-line-args-left))))\n')
            arguments = ["", "日本語", "--help", "--", 'quotes " and \\', "line\nbreak",
                         '(write-region "oops" nil "executed")', *map(str, range(30))]
            expected = b"kept\0" + b"".join(arg.encode() + b"\0" for arg in arguments)
            env = dict(os.environ, ARG_ENV="kept")
            result = subprocess.run([sys.executable, str(CLI), "run", "--", *arguments],
                                    cwd=project, env=env, capture_output=True, timeout=15)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, expected, b""))
            self.assertFalse((project / "executed").exists())
            build = subprocess.run([sys.executable, str(CLI), "build"], cwd=project,
                                   capture_output=True, text=True, timeout=300)
            self.assertEqual(build.returncode, 0, build.stderr)
            binary = project / "target/hello"
            moved = root / "app"
            moved.write_bytes(binary.read_bytes())
            moved.chmod(0o755)
            for args, output in (([], b"kept\0"), (arguments, expected)):
                result = subprocess.run([str(moved), *args], cwd=root,
                                        env={"ARG_ENV": "kept"}, capture_output=True, timeout=15)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, output, b""))
            self.assertFalse((root / "executed").exists())


if __name__ == "__main__":
    unittest.main()
