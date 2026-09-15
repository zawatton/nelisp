"""Native packaging acceptance; intentionally separate from the fast CLI loop."""

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
CLI = ROOT / "tools" / "nelisp-project.py"


class ProjectBuild(unittest.TestCase):
    def test_build_relocate_rebuild_and_preserve(self):
        with tempfile.TemporaryDirectory(prefix="nelisp build 日本語 ") as directory:
            root = Path(directory)
            result = subprocess.run([sys.executable, str(CLI), "new", "hello"],
                                    cwd=root, capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, 0, result.stderr)
            project = root / "hello"
            source = project / "src" / "main.nl"
            original = source.read_text()
            # Building must not execute top-level project forms on the host.
            source.write_text(original + '\n(write-region "ran" nil "executed")\n'
                              '(when (getenv "NELISP_BUILD_SMOKE_ERROR") (error "build-smoke-error"))\n')
            binary = project / "target" / "hello"
            metadata = project / "target" / "hello.build.json"

            def build(*options):
                start = time.monotonic()
                result = subprocess.run([sys.executable, str(CLI), "build", *options], cwd=project,
                                        capture_output=True, text=True, timeout=300)
                print(f"build seconds={time.monotonic() - start:.3f} exit={result.returncode}", flush=True)
                return result

            result = build()
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse((project / "executed").exists())
            first = binary.read_bytes()
            self.assertTrue(first.startswith(b"\x7fELF"))
            record = json.loads(metadata.read_text())
            self.assertFalse(record["application_aot"])
            self.assertEqual(record["binary_sha256"], hashlib.sha256(first).hexdigest())

            # Only the executable moves; no source, manifest, runtime, Python,
            # Emacs, or repository-relative working directory accompanies it.
            isolated = root / "isolated"
            isolated.mkdir()
            moved = isolated / "app"
            shutil.copy2(binary, moved)
            empty_path = isolated / "empty-path"
            empty_path.mkdir()
            env = dict(os.environ, PATH=str(empty_path))
            env.pop("NELISP_BUILD_SMOKE_ERROR", None)
            result = subprocess.run([str(moved)], cwd=isolated, env=env,
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (0, "Hello, world!\n", ""))
            self.assertEqual((isolated / "executed").read_text(), "ran")
            result = subprocess.run([str(moved)], cwd=isolated,
                                    env=dict(env, NELISP_BUILD_SMOKE_ERROR="1"),
                                    capture_output=True, text=True, timeout=10)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("build-smoke-error", result.stderr)
            result = subprocess.run([str(moved), "unsupported"], cwd=isolated, env=env,
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout), (0, "Hello, world!\n"))

            result = build()
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(binary.read_bytes(), first, "same source must produce identical bytes")
            self.assertIn("0 runtime units recompiled", (project / "target" / "hello.build.log").read_text())

            result = build("--release")
            self.assertEqual(result.returncode, 0, result.stderr)
            release = project / "target/release/hello"
            release_bytes = release.read_bytes()
            self.assertLess(len(release_bytes), len(first))
            self.assertEqual(binary.read_bytes(), first, "release must preserve the dev artifact")
            release_record = json.loads((release.parent / "hello.build.json").read_text())
            self.assertEqual(release_record["profile"], "release")
            self.assertEqual(release_record["native_symbols"], "entry-only")
            shutil.copy2(release, moved)
            result = subprocess.run([str(moved)], cwd=isolated, env=env,
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (0, "Hello, world!\n", ""))
            self.assertEqual(build("--release").returncode, 0)
            self.assertEqual(release.read_bytes(), release_bytes)

            source.write_text(original.replace("Hello, world!", "こんにちは"))
            result = build()
            self.assertEqual(result.returncode, 0, result.stderr)
            result = subprocess.run([str(binary)], cwd=isolated, env=env,
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout), (0, "こんにちは\n"))
            good_binary = binary.read_bytes()
            good_record = metadata.read_bytes()
            source.write_text("(defun broken (\n")
            result = build()
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(binary.read_bytes(), good_binary)
            self.assertEqual(metadata.read_bytes(), good_record)
            self.assertNotEqual(build("--release").returncode, 0)
            self.assertEqual(release.read_bytes(), release_bytes)


if __name__ == "__main__":
    result = unittest.main(verbosity=2, exit=False).result
    checked = result.testsRun - len(result.skipped)
    failed = len(result.failures) + len(result.errors)
    print(f"GATE-COUNT checked={checked} findings={failed}")
    sys.exit(0 if checked and result.wasSuccessful() else 1)
