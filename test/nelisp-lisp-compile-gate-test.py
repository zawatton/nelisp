"""Compiler-process failures cannot be hidden by sibling diagnostics."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class CompileGate(unittest.TestCase):
    def run_gate(self, mode):
        with tempfile.TemporaryDirectory(prefix="nelisp-compile-gate-") as directory:
            root = Path(directory)
            for name in ["tools", "lisp", "src", "scripts", "packages"]:
                (root / name).mkdir()
            script = root / "tools/nelisp-lisp-byte-compile-gate.sh"
            shutil.copy2(ROOT / "tools/nelisp-lisp-byte-compile-gate.sh", script)
            (root / "tools/nelisp-lisp-compile-baseline.txt").write_text("lisp/a.el 1\n")
            (root / "lisp/a.el").write_text("(defvar a 1)\n")
            (root / "lisp/b.el").write_text("(defvar b 1)\n")
            fake = root / "compiler"
            fake.write_text('''#!/bin/sh
for argument do file=$argument; done
case "$MODE:$file" in
  success:*) exit 0 ;;
  mixed:lisp/a.el) echo 'lisp/a.el:1:1: Warning: baselined diagnostic'; exit 0 ;;
  *) echo 'compiler process failed before diagnostic emission'; exit 7 ;;
esac
''')
            fake.chmod(0o755)
            return subprocess.run(["bash", str(script)], cwd=root,
                                  env=dict(os.environ, EMACS=str(fake), MODE=mode),
                                  capture_output=True, text=True, timeout=10)

    def test_successful_compiler_control(self):
        result = self.run_gate("success")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("checked=2 findings=0", result.stdout)

    def test_failed_process_without_diagnostics_is_red(self):
        result = self.run_gate("fail")
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertNotIn("now compiles clean", result.stdout)

    def test_sibling_warning_does_not_mask_failed_process(self):
        result = self.run_gate("mixed")
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertIn("lisp/b.el", result.stdout)


if __name__ == "__main__":
    unittest.main()
