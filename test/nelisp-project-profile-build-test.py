"""Profiled standalone ELF: recursive counts and failure reports."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class ProfileBuild(unittest.TestCase):
    def test_isolated_profile_success_and_error(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-profile-") as directory:
            root = Path(directory)
            subprocess.run([sys.executable, str(CLI), "new", "hello"], cwd=root,
                           capture_output=True, check=True)
            project = root / "hello"
            (project / "src/main.nl").write_text(
                '(write-region "ran" nil "executed")\n'
                '(defun 補助 (n) (if (= n 0) 0 (+ n (補助 (- n 1)))))\n'
                '(defun main () (princ (補助 3)) '
                '(when (getenv "FAIL_PROFILE") (error "profile-error")))\n')
            build = subprocess.run([sys.executable, str(CLI), "build", "--profile"],
                                   cwd=project, capture_output=True, text=True, timeout=300)
            self.assertEqual(build.returncode, 0, build.stderr)
            self.assertFalse((project / "executed").exists())
            binary = project / "target/profile/hello"
            record = json.loads((binary.parent / "hello.build.json").read_text())
            self.assertEqual(record["profile"], "profile")
            isolated = root / "isolated"
            isolated.mkdir()
            moved = isolated / "app"
            shutil.copy2(binary, moved)
            env = dict(os.environ, PATH="")
            env.pop("FAIL_PROFILE", None)
            for failure in (False, True):
                active = dict(env, FAIL_PROFILE="1") if failure else env
                result = subprocess.run([str(moved)], cwd=isolated, env=active,
                                        capture_output=True, text=True, timeout=15)
                self.assertEqual(result.returncode == 0, not failure, result.stderr)
                self.assertEqual(result.stdout, "6")
                lines = [line for line in result.stderr.splitlines() if line.startswith("NELISP_PROFILE_V1 ")]
                self.assertEqual(len(lines), 1, result.stderr)
                report = json.loads(lines[0].split(" ", 1)[1])
                self.assertEqual(report["status"], "error" if failure else "ok")
                rows = {row["name"]: row for row in report["functions"]}
                self.assertEqual(rows["補助"]["calls"], 4)
                self.assertEqual(rows["補助"]["completed"], 4)
                self.assertEqual(rows["main"]["calls"], 1)
                self.assertEqual(rows["main"]["completed"], 0 if failure else 1)
                self.assertGreaterEqual(rows["main"]["elapsed_us"], 0)
                if failure:
                    self.assertIn("profile-error", result.stderr)


if __name__ == "__main__":
    unittest.main()
