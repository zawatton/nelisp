"""Run the actual mutation harness in an isolated repository with copy faults."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class MutationRecovery(unittest.TestCase):
    def exercise(self, fail_restore):
        with tempfile.TemporaryDirectory(prefix="nelisp-mutation-recovery-") as directory:
            root = Path(directory)
            for name in ["tools", "lisp", "bin", "tmp"]:
                (root / name).mkdir()
            script = root / "tools/nelisp-gate-mutation.sh"
            shutil.copy2(ROOT / "tools/nelisp-gate-mutation.sh", script)
            (root / "tools/gate-mutations.txt").write_text('sample|lisp/sample.el|s/OLD/NEW/|injected defect\n')
            original = b'OLD\nlocal uncommitted content\n'
            source = root / "lisp/sample.el"
            source.write_bytes(original)
            (root / "Makefile").write_text('sample:\n\t@! grep -q NEW lisp/sample.el\n')
            fake = root / "bin/cp"
            fake.write_text('''#!/bin/sh
if [ "$FAIL_RESTORE" = 1 ] && [ "$2" = "lisp/sample.el" ]; then exit 1; fi
exec "$REAL_CP" "$@"
''')
            fake.chmod(0o755)
            env = dict(os.environ, PATH=str(root / "bin") + os.pathsep + os.environ["PATH"],
                       REAL_CP=shutil.which("cp"), FAIL_RESTORE="1" if fail_restore else "0",
                       TMPDIR=str(root / "tmp"))
            for key in list(env):
                if key.startswith("NELISP_GATE_MUTATION_"):
                    env.pop(key)
            result = subprocess.run(["bash", str(script)], cwd=root, env=env,
                                    capture_output=True, text=True, timeout=10)
            if fail_restore:
                self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                backups = [p for p in (root / "tmp").rglob("*") if p.is_file() and p.read_bytes() == original]
                self.assertTrue(backups, "original content must survive failed restoration")
                self.assertIn(str(backups[0]), result.stderr)
            else:
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(source.read_bytes(), original)
                self.assertEqual(list((root / "tmp").iterdir()), [])

    def test_successful_restore_preserves_uncommitted_content(self):
        self.exercise(False)

    def test_failed_restore_keeps_backup_and_stops(self):
        self.exercise(True)


if __name__ == "__main__":
    unittest.main()
