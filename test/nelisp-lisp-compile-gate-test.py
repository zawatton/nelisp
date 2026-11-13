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


class ClassifiedGate(unittest.TestCase):
    """BUG / STYLE classification, exemptions and ceilings."""

    def run_gate(self, message, baseline="", exemptions=""):
        with tempfile.TemporaryDirectory(prefix="nelisp-compile-gate-") as directory:
            root = Path(directory)
            for name in ["tools", "lisp", "src", "scripts", "packages"]:
                (root / name).mkdir()
            script = root / "tools/nelisp-lisp-byte-compile-gate.sh"
            shutil.copy2(ROOT / "tools/nelisp-lisp-byte-compile-gate.sh", script)
            (root / "tools/nelisp-lisp-compile-baseline.txt").write_text(baseline)
            (root / "tools/nelisp-lisp-compile-bug-exemptions.txt").write_text(exemptions)
            (root / "lisp/a.el").write_text("(defvar a 1)\n")
            fake = root / "compiler"
            fake.write_text('''#!/bin/sh
for argument do file=$argument; done
if [ "$file" = lisp/a.el ] && [ -n "$MSG" ]; then
  printf '%s\\n' "$MSG"
fi
exit 0
''')
            fake.chmod(0o755)
            return subprocess.run(["bash", str(script)], cwd=root,
                                  env=dict(os.environ, EMACS=str(fake), MSG=message),
                                  capture_output=True, text=True, timeout=10)

    BUG = "lisp/a.el:3:1: Warning: reference to free variable \u2018zz\u2019"
    STYLE = "lisp/a.el:3:1: Warning: Unused lexical variable \u2018x\u2019"

    def test_clean_control(self):
        r = self.run_gate("")
        self.assertEqual(r.returncode, 0, r.stdout)
        self.assertIn("findings=0", r.stdout)

    def test_bug_warning_in_clean_file_fails(self):
        r = self.run_gate(self.BUG)
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertIn("no exemption", r.stdout)

    def test_bug_warning_not_absorbed_by_style_ceiling(self):
        r = self.run_gate(self.BUG, baseline="lisp/a.el 5\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)

    def test_style_warning_within_ceiling_passes(self):
        r = self.run_gate(self.STYLE, baseline="lisp/a.el 1\n")
        self.assertEqual(r.returncode, 0, r.stdout)

    def test_style_warning_above_ceiling_fails(self):
        r = self.run_gate(self.STYLE + "\n" + self.STYLE, baseline="lisp/a.el 1\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertIn("baseline records 1", r.stdout)

    def test_unclassified_warning_fails(self):
        r = self.run_gate("lisp/a.el:3:1: Warning: some brand new warning kind",
                          baseline="lisp/a.el 5\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertIn("UNCLASSIFIED", r.stdout)

    def test_exemption_absorbs_bug(self):
        r = self.run_gate(self.BUG, exemptions="lisp/a.el|1|free variable|reason text\n")
        self.assertEqual(r.returncode, 0, r.stdout)

    def test_exemption_count_is_a_ceiling(self):
        r = self.run_gate(self.BUG + "\n" + self.BUG,
                          exemptions="lisp/a.el|1|free variable|reason text\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)

    def test_stale_exemption_fails(self):
        r = self.run_gate("", exemptions="lisp/a.el|1|free variable|reason text\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertIn("stale exemption", r.stdout)

    def test_stale_volatile_exemption_passes(self):
        r = self.run_gate("", exemptions="lisp/a.el|1|free variable|reason text|volatile\n")
        self.assertEqual(r.returncode, 0, r.stdout)

    def test_exemption_without_reason_fails(self):
        r = self.run_gate(self.BUG, exemptions="lisp/a.el|1|free variable|\n")
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertIn("malformed", r.stdout)

    def test_last_gate_count_line_is_combined(self):
        r = self.run_gate(self.BUG + "\n" + self.STYLE, baseline="lisp/a.el 0\n")
        counts = [l for l in r.stdout.splitlines() if l.startswith("GATE-COUNT ")]
        self.assertEqual(counts[-1], "GATE-COUNT checked=1 findings=2")


if __name__ == "__main__":
    unittest.main()
