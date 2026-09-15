"""Debug artifacts remain inspectable without source, host Emacs, or execution."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools/nelisp-project.py"


class DebugBuild(unittest.TestCase):
    def test_relocate_inspect_and_reject_stale_map(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-debug-") as directory:
            root = Path(directory)

            def cli(*args, cwd=root, env=None):
                return subprocess.run([sys.executable, str(CLI), *args], cwd=cwd, env=env,
                                      capture_output=True, text=True, timeout=300)

            self.assertEqual(cli("new", "hello").returncode, 0)
            project = root / "hello"
            source = project / "src/main.nl"
            text = (';; 日本語\n(defvar value "original")\n'
                    '(defun 補助 () "First" "こんにちは")\n'
                    '(defun main () (princ (補助)))\n'
                    '(write-region "ran" nil "executed")\n')
            source.write_text(text)
            result = cli("build", "--debug", cwd=project)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse((project / "executed").exists())
            binary = project / "target/debug/hello"
            sidecar = Path(str(binary) + ".debug.json")
            original_binary, original_map = binary.read_bytes(), sidecar.read_bytes()
            self.assertEqual(cli("build", "--debug", cwd=project).returncode, 0)
            self.assertEqual(binary.read_bytes(), original_binary)
            self.assertEqual(sidecar.read_bytes(), original_map)
            source.write_text("(defun broken (")
            self.assertNotEqual(cli("build", "--debug", cwd=project).returncode, 0)
            self.assertEqual(binary.read_bytes(), original_binary)
            self.assertEqual(sidecar.read_bytes(), original_map)

            moved = root / "app"
            moved_map = root / "app.debug.json"
            shutil.copy2(binary, moved)
            shutil.copy2(sidecar, moved_map)
            source.unlink()
            env = dict(os.environ, EMACS=str(root / "absent"), NELISP_BIN=str(root / "absent"))
            started = time.perf_counter()
            result = cli("debug-info", "補助", "--binary", str(moved), "--json", env=env)
            elapsed = time.perf_counter() - started
            self.assertEqual(result.returncode, 0, result.stderr)
            report = json.loads(result.stdout)
            self.assertEqual(report["binary_sha256"], hashlib.sha256(original_binary).hexdigest())
            symbol = report["symbols"][0]
            self.assertEqual(symbol["path"], "src/main.nl")
            self.assertEqual(symbol["start"]["line"], 3)
            self.assertEqual(symbol["start"]["column"], 1)
            self.assertEqual(symbol["text"], '(defun 補助 () "First" "こんにちは")')
            self.assertFalse((root / "executed").exists(), "inspection must not execute the binary")
            print(f"offline debug-info seconds={elapsed:.6f}", flush=True)
            run = subprocess.run([str(moved)], cwd=root, env=dict(env, PATH=""),
                                 capture_output=True, text=True, timeout=10)
            self.assertEqual((run.returncode, run.stdout, run.stderr), (0, "こんにちは", ""))
            changed = json.loads(original_map)
            changed["map"]["symbols"][0]["start"]["line"] = 999
            # Updating the sidecar's own checksum cannot forge the digest in ELF.
            sys.path.insert(0, str(ROOT / "tools"))
            from nelisp_project_debug import digest
            changed["map_sha256"] = digest(changed["map"])
            moved_map.write_text(json.dumps(changed))
            self.assertNotEqual(cli("debug-info", "--binary", str(moved), env=env).returncode, 0)
            moved_map.write_bytes(original_map)
            moved.write_bytes(original_binary + b"changed")
            self.assertNotEqual(cli("debug-info", "--binary", str(moved), env=env).returncode, 0)


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
