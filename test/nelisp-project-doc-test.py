"""Source-derived HTML documentation and atomic failure preservation."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class ProjectDocs(unittest.TestCase):
    def test_doc_sources_escaping_determinism_and_failure(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-doc-") as directory:
            root = Path(directory)
            env = dict(os.environ, NELISP_BIN=str(root / "absent-runtime"))

            def cli(*args, cwd=root):
                return subprocess.run([sys.executable, str(CLI), *args], cwd=cwd, env=env,
                                      capture_output=True, text=True, timeout=20)

            self.assertEqual(cli("new", "hello").returncode, 0)
            project = root / "hello"
            source = project / "src/main.nl"
            source.write_text('(write-region "ran" nil "executed")\n'
                              '(defun 文 (x) "日本語 <script>alert(1)</script>" x)\n'
                              '(defmacro example (&rest forms) "Macro docs" nil)\n'
                              '(defvar setting 1 "Setting docs")\n')
            outside = root / "outside.el"
            outside.write_text("(error \"not documentation input\")")
            (project / "test/outside.el").symlink_to(outside)
            result = cli("doc", cwd=project)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse((project / "executed").exists())
            output = project / "target/doc/index.html"
            page = output.read_text()
            self.assertIn("&lt;script&gt;alert(1)&lt;/script&gt;", page)
            self.assertNotIn("<script>alert(1)</script>", page)
            self.assertIn('id="search"', page)
            report = json.loads((project / "target/doc/api.json").read_text())
            self.assertEqual(report["files"][0]["sha256"], hashlib.sha256(source.read_bytes()).hexdigest())
            self.assertEqual([row["symbol"] for row in report["symbols"]], ["文", "example", "setting"])
            self.assertEqual(report["symbols"][0]["start"]["line"], 2)
            self.assertEqual(report["symbols"][0]["path"], "src/main.nl")
            before = output.read_bytes()
            self.assertEqual(cli("doc", cwd=project).returncode, 0)
            self.assertEqual(output.read_bytes(), before)
            source.write_text("(defun broken (")
            self.assertNotEqual(cli("doc", cwd=project).returncode, 0)
            self.assertEqual(output.read_bytes(), before)
            unrelated = output.parent / "notes.txt"
            unrelated.write_text("keep")
            self.assertEqual(cli("clean", cwd=project).returncode, 0)
            self.assertFalse(output.exists())
            self.assertFalse((output.parent / "api.json").exists())
            self.assertEqual(unrelated.read_text(), "keep")


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
