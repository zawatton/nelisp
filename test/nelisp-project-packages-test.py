"""Locked dependencies through real project CLI commands, using an offline cache."""
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


class ProjectPackages(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="nelisp-packages-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.cache = self.root / "cache"
        self.cache.mkdir()
        self.env = dict(os.environ, NELISP_CACHE=str(self.cache))
        result = self.cli("new", "hello", cwd=self.root)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.project = self.root / "hello"
        self.manifest = self.project / "nelisp.toml"
        self.manifest.write_text(self.manifest.read_text() + '\n[dependencies]\ngreet = "1"\n')
        self.body = b"(require 'core)\n(defun dependency-greeting () (core-greeting))\n(provide 'greet)\n"
        core = b'(defun core-greeting () "Hello, world!")\n(provide \'core)\n'
        core_digest = hashlib.sha256(core).hexdigest()
        (self.cache / (core_digest + ".nl")).write_bytes(core)
        self.digest = hashlib.sha256(self.body).hexdigest()
        (self.cache / (self.digest + ".nl")).write_bytes(self.body)
        self.index = self.root / "index.json"
        self.index.write_text(json.dumps({"schema_version": 1, "packages": {
            "greet": [{"version": "1.0.0", "dependencies": {"core": "1"}, "sha256": self.digest,
                       "url": "https://example.invalid/greet.nl", "yanked": False}],
            "core": [{"version": "1.0.0", "dependencies": {}, "sha256": core_digest,
                      "url": "https://example.invalid/core.nl", "yanked": False}]}}))
        (self.project / "src/main.nl").write_text(
            "(require 'greet)\n(defun greeting () (dependency-greeting))\n"
            '(defun main () (princ (concat (greeting) "\\n")))\n')

    def cli(self, *args, cwd=None, input=None):
        return subprocess.run([sys.executable, str(CLI), *args], cwd=cwd or self.project,
                              env=self.env, input=input, capture_output=True, text=True, timeout=120)

    def update(self):
        return self.cli("update", "--index", str(self.index), "--offline")

    def test_lock_run_test_repl_and_integrity_failure(self):
        self.assertNotEqual(self.cli("run").returncode, 0)
        result = self.update()
        self.assertEqual(result.returncode, 0, result.stderr)
        locked = (self.project / "nelisp.lock").read_bytes()
        self.assertEqual(self.cli("run").stdout, "Hello, world!\n")
        result = self.cli("test")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        result = self.cli("repl", input='(dependency-greeting)\n(exit 0)\n')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('"Hello, world!"', result.stdout)
        self.assertEqual(self.cli("fetch", "--offline").returncode, 0)
        (self.cache / (self.digest + ".nl")).write_bytes(b'(princ "tampered")')
        for command in ["run", "test", "repl", "build", "fetch"]:
            result = self.cli(command)
            self.assertNotEqual(result.returncode, 0, command)
            self.assertIn("integrity", result.stderr)
            self.assertNotIn("tampered", result.stdout)
        self.assertEqual((self.project / "nelisp.lock").read_bytes(), locked)

    def test_update_failure_preserves_lock_and_stale_requirements_fail(self):
        self.assertEqual(self.update().returncode, 0)
        lock = self.project / "nelisp.lock"
        before = lock.read_bytes()
        (self.cache / (self.digest + ".nl")).unlink()
        result = self.update()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(lock.read_bytes(), before)
        self.manifest.write_text(self.manifest.read_text().replace('greet = "1"', 'greet = "2"'))
        result = self.cli("run")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("requirements", result.stderr)

    def test_native_build_embeds_dependency(self):
        self.assertEqual(self.update().returncode, 0)
        result = self.cli("build")
        self.assertEqual(result.returncode, 0, result.stderr)
        executable = self.root / "isolated-app"
        executable.write_bytes((self.project / "target/hello").read_bytes())
        executable.chmod(0o755)
        (self.cache / (self.digest + ".nl")).unlink()
        result = subprocess.run([str(executable)], cwd=self.root,
                                env=dict(self.env, PATH=""), capture_output=True, text=True, timeout=10)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "Hello, world!\n", ""))
        metadata = json.loads((self.project / "target/hello.build.json").read_text())
        self.assertEqual(metadata["dependencies"][-1]["sha256"], self.digest)
        self.assertEqual([item["name"] for item in metadata["dependencies"]], ["core", "greet"])
        self.assertEqual(metadata["source_sha256"], hashlib.sha256((self.project / "src/main.nl").read_bytes()).hexdigest())
        self.assertNotEqual(metadata["source_sha256"], metadata["bundle_sha256"])

    def test_update_validates_syntax_without_evaluating_packages(self):
        self.assertEqual(self.update().returncode, 0)
        lock = self.project / "nelisp.lock"
        before = lock.read_bytes()
        catalog = json.loads(self.index.read_text())
        content = self.body + b'\n(write-region "executed" nil "host-side-effect")\n'
        for body, success in [(content, True), (content + b"\n(", False)]:
            digest = hashlib.sha256(body).hexdigest()
            (self.cache / (digest + ".nl")).write_bytes(body)
            catalog["packages"]["greet"][0]["sha256"] = digest
            self.index.write_text(json.dumps(catalog))
            result = self.update()
            self.assertEqual(result.returncode == 0, success, result.stderr)
            self.assertFalse((self.project / "host-side-effect").exists())
            if success:
                self.assertNotEqual(lock.read_bytes(), before)
                before = lock.read_bytes()
            else:
                self.assertEqual(lock.read_bytes(), before)

    def test_add_preserves_pins_remove_prunes_and_failures_preserve_files(self):
        self.assertEqual(self.update().returncode, 0)
        lock = self.project / "nelisp.lock"
        catalog = json.loads(self.index.read_text())
        newer = dict(catalog["packages"]["greet"][0], version="1.9.0")
        catalog["packages"]["greet"].append(newer)
        extra = b"(provide 'extra)\n"
        digest = hashlib.sha256(extra).hexdigest()
        (self.cache / (digest + ".nl")).write_bytes(extra)
        catalog["packages"]["extra"] = [{"version": "2.0.0", "dependencies": {},
            "sha256": digest, "url": "https://example.invalid/extra.nl", "yanked": False}]
        self.index.write_text(json.dumps(catalog))
        self.manifest.write_text(self.manifest.read_text().replace('greet = "1"', 'greet = "1" # pinned greeting'))
        result = self.cli("add", "extra", "--index", str(self.index), "--offline")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('greet = "1" # pinned greeting', self.manifest.read_text())
        import tomllib
        packages = {item["name"]: item for item in tomllib.loads(lock.read_text())["packages"]}
        self.assertEqual(packages["greet"]["version"], "1.0.0")
        self.assertEqual(packages["extra"]["version"], "2.0.0")
        before = self.manifest.read_bytes(), lock.read_bytes()
        result = self.cli("add", "absent", "--index", str(self.index), "--offline")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.manifest.read_bytes(), lock.read_bytes()), before)
        catalog["packages"]["extra"].append(dict(catalog["packages"]["extra"][0], version="3.0.0", sha256="0" * 64))
        self.index.write_text(json.dumps(catalog))
        result = self.cli("add", "extra", "--version", "3", "--index", str(self.index), "--offline")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing cached", result.stderr)
        self.assertEqual((self.manifest.read_bytes(), lock.read_bytes()), before)
        result = self.cli("remove", "greet")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("# pinned greeting", self.manifest.read_text())
        self.assertEqual([item["name"] for item in tomllib.loads(lock.read_text())["packages"]], ["extra"])
        self.assertEqual(self.cli("remove", "extra").returncode, 0)
        self.assertEqual(tomllib.loads(lock.read_text())["packages"], [])

    def test_remove_only_needs_manifest_and_lock_not_cache_or_emacs(self):
        self.manifest.write_text(self.manifest.read_text() + 'core = "1"\n')
        self.assertEqual(self.update().returncode, 0)
        for artifact in self.cache.glob("*.nl"):
            artifact.unlink()
        self.env["EMACS"] = str(self.root / "absent-emacs")
        self.env["NELISP_BIN"] = str(self.root / "absent-runtime")
        result = self.cli("remove", "greet")
        self.assertEqual(result.returncode, 0, result.stderr)
        import tomllib
        lock = tomllib.loads((self.project / "nelisp.lock").read_text())
        self.assertEqual([item["name"] for item in lock["packages"]], ["core"])


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
