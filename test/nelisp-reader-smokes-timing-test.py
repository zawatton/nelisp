"""Hermetic test for the per-target timing and failure output of
tools/nelisp-reader-smokes.sh.  A fake make (the script honours $MAKE) stands
in for the real build; no real smoke runs.

Usage: python3 test/nelisp-reader-smokes-timing-test.py
"""
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = Path(os.environ.get("NELISP_READER_SMOKES_UNDER_TEST",
                             ROOT / "tools/nelisp-reader-smokes.sh"))

# The target is the one argument that is neither an option nor a variable.
# `standalone-reader' alone is the single pre-build and always succeeds.
FAKE_MAKE = r"""#!/bin/sh
target=""
for a in "$@"; do
  case "$a" in -*|*=*|standalone-reader) ;; *) target=$a ;; esac
done
case "$target" in
  "") exit 0 ;;
  fast-ok) echo fast ;;
  slow-ok) sleep 2; echo slow ;;
  standalone-reader-repl-smoke) sleep 1; echo serial ;;
  fail) i=1; while [ $i -le 50 ]; do echo "fail-line-$i"; i=$((i+1)); done; exit 2 ;;
esac
"""


class ReaderSmokesTiming(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory(prefix="nelisp-smokes-timing-")
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        (self.root / "tools").mkdir()
        (self.root / "tmp").mkdir()
        shutil.copy2(SCRIPT, self.root / "tools/nelisp-reader-smokes.sh")
        fake = self.root / "fake-make"
        fake.write_text(FAKE_MAKE)
        fake.chmod(fake.stat().st_mode | stat.S_IXUSR)
        self.env = {k: v for k, v in os.environ.items()
                    if not k.startswith(("NELISP_", "MAKE"))}
        self.env.update(TMPDIR=str(self.root / "tmp"), MAKE=str(fake), NELISP_SMOKE_JOBS="3")

    def run_smokes(self, *targets):
        result = subprocess.run(["bash", str(self.root / "tools/nelisp-reader-smokes.sh"), *targets],
                                cwd=self.root, env=self.env, capture_output=True,
                                text=True, timeout=40)
        self.out = result.stdout + result.stderr
        return result

    def table(self):
        lines = self.out.split("\n")
        start = lines.index("[reader-smokes] slowest targets")
        rows = []
        for line in lines[start + 1:]:
            m = re.fullmatch(r"\s+(\d+)s\s+(\S+)", line)
            if not m:
                break
            rows.append((int(m.group(1)), m.group(2)))
        return rows

    def test_all_pass_prints_durations_table_and_exits_zero(self):
        result = self.run_smokes("fast-ok", "slow-ok", "standalone-reader-repl-smoke")
        self.assertEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"(?m)^\[reader-smokes\] fast-ok done in 0s \(ok\)$")
        self.assertRegex(self.out, r"(?m)^\[reader-smokes\] slow-ok done in [2-3]s \(ok\)$")
        self.assertRegex(self.out, r"(?m)^\[reader-smokes\] standalone-reader-repl-smoke done in [1-2]s \(ok\)$")
        rows = self.table()
        self.assertEqual([name for _, name in rows], ["slow-ok", "standalone-reader-repl-smoke", "fast-ok"], self.out)
        self.assertEqual(rows, sorted(rows, key=lambda r: (-r[0], r[1])))
        self.assertIn("GATE-COUNT checked=3 findings=0", self.out)
        self.assertIn("PASS: 3 smokes", self.out)
        self.assertLess(self.out.index("slowest targets"), self.out.index("GATE-COUNT"))

    def test_failure_keeps_tail_table_and_nonzero_exit(self):
        result = self.run_smokes("fast-ok", "fail", "slow-ok")
        self.assertNotEqual(result.returncode, 0, self.out)
        self.assertRegex(self.out, r"(?m)^\[reader-smokes\] fail done in \d+s \(FAIL\)$")
        self.assertRegex(self.out, r"(?m)^\[reader-smokes\] fast-ok done in 0s \(ok\)$")
        self.assertIn("[reader-smokes] FAIL: fail\n", self.out)
        # A 40-line tail: lines 11..50 present, line 10 absent.
        self.assertIn("    fail-line-50\n", self.out)
        self.assertIn("    fail-line-11\n", self.out)
        self.assertNotIn("fail-line-10\n", self.out)
        names = [name for _, name in self.table()]
        self.assertEqual(names[0], "slow-ok", self.out)
        self.assertEqual(sorted(names), ["fail", "fast-ok", "slow-ok"])
        self.assertIn("GATE-COUNT checked=3 findings=1", self.out)
        self.assertIn("[reader-smokes] FAIL: fail", self.out)

    def test_done_lines_are_whole_lines(self):
        result = self.run_smokes("fast-ok", "slow-ok", "standalone-reader-repl-smoke")
        done = [l for l in self.out.split("\n") if " done in " in l]
        for line in done:
            self.assertRegex(line, r"^\[reader-smokes\] \S+ done in \d+s \((ok|FAIL)\)$")
        self.assertGreaterEqual(len(done), 3, self.out)


if __name__ == "__main__":
    unittest.main()
