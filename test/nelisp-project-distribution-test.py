"""Build, install, relocate, and use the Linux project toolchain bundle."""
import json
from contextlib import ExitStack
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]


class ProjectDistribution(unittest.TestCase):
    def test_installed_project_workflow(self):
        version = f"v0.0.0-cli-{os.getpid()}"
        artifact = ROOT / "dist" / f"anvil-{version}-linux-x86_64.tar.gz"
        self.addCleanup(artifact.unlink, missing_ok=True)
        self.addCleanup(Path(str(artifact) + ".sha256").unlink, missing_ok=True)
        result = subprocess.run(
            ["bash", "tools/build-standalone-tarball.sh", version, "linux-x86_64", "--project-cli"],
            cwd=ROOT, capture_output=True, text=True, timeout=300)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        result = subprocess.run(
            ["bash", "tools/verify-standalone-tarball.sh", version, "linux-x86_64"],
            cwd=ROOT, capture_output=True, text=True, timeout=300)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        with tempfile.TemporaryDirectory(prefix="nelisp installed 日本語 ") as directory, ExitStack() as permissions:
            home = Path(directory)
            prefix = home / "first install"
            result = subprocess.run(
                ["bash", str(ROOT / "release/stage-d-v3.0/install-v3.sh"),
                 "--from", str(artifact.parent), "--prefix", str(prefix), "--version", version],
                cwd=home, capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            installed = home / "relocated toolchain"
            prefix.rename(installed)
            # Exercise actual filesystem denial, not just a cache-path mock.
            for path in [installed, *installed.rglob("*")]:
                mode = path.stat().st_mode
                permissions.callback(path.chmod, mode)
                path.chmod(mode & ~0o222)
            with self.assertRaises(PermissionError):
                (installed / "write-probe").write_text("must be denied")
            cli = installed / "bin/nelisp"
            self.assertTrue((installed / "docs/nelisp-strategy.org").is_file())
            self.assertIn("NELISP_BIN=libexec/nelisp-runtime", (installed / "README.org").read_text())
            env = dict(os.environ, PYTHON=sys.executable,
                       NELISP_CACHE=str(home / "package-cache"),
                       XDG_CACHE_HOME=str(home / "user-cache"))
            env.pop("NELISP_BIN", None)
            env.pop("PYTHONPATH", None)
            env.pop("NELISP_BUILD_CACHE", None)

            def command(*args, cwd=home, extra=None):
                result = subprocess.run([str(cli), *args], cwd=cwd,
                                        env=dict(env, **(extra or {})),
                                        capture_output=True, text=True, timeout=300)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                return result

            self.assertEqual(command("--version").stdout,
                             f"nelisp {version} (project frontend)\n")
            self.assertEqual(command("--eval", "(+ 40 2)").stdout, "42\n")
            command("new", "hello")
            project = home / "hello"
            absent_emacs = {"EMACS": str(home / "absent-emacs")}
            self.assertEqual(command("run", cwd=project, extra=absent_emacs).stdout, "Hello, world!\n")
            self.assertIn("1 passed, 0 failed", command("test", cwd=project, extra=absent_emacs).stdout)
            selected = json.loads(command("test", "--filter", "greeting", "--json",
                                          cwd=project, extra=absent_emacs).stdout)
            self.assertEqual((selected["status"], selected["total"]), ("passed", 1))
            command("fmt", "--check", cwd=project)
            self.assertEqual(json.loads(command("check", "--json", cwd=project).stdout)["status"], "ok")
            command("doc", cwd=project)
            started = time.monotonic()
            command("build", cwd=project)
            cold_seconds = time.monotonic() - started
            first = (project / "target/hello").read_bytes()
            self.assertTrue(list((home / "user-cache/nelisp/build-v1").rglob("*.unit")))
            self.assertFalse((installed / "target").exists())
            started = time.monotonic()
            command("build", cwd=project,
                    extra={"NELISP_BUILD_CACHE": str(home / "user-cache/nelisp/build-v1")})
            warm_seconds = time.monotonic() - started
            self.assertEqual((project / "target/hello").read_bytes(), first)
            self.assertIn("0 runtime units recompiled", (project / "target/hello.build.log").read_text())
            print(json.dumps({"installed_build_cold_seconds": cold_seconds,
                              "installed_build_warm_seconds": warm_seconds,
                              "same_executable": True}), flush=True)
            isolated = home / "deployed"
            isolated.mkdir()
            binary = isolated / "app"
            shutil.copy2(project / "target/hello", binary)
            result = subprocess.run([str(binary)], cwd=isolated, env={"PATH": str(isolated)},
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "Hello, world!\n", ""))
            # The installer copied the current standalone installer, not the
            # obsolete Stage D host-Emacs installer from the repository root.
            help_result = subprocess.run(["bash", str(installed / "install.sh"), "--help"],
                                         capture_output=True, text=True, timeout=5)
            self.assertEqual(help_result.returncode, 0, help_result.stderr)
            self.assertIn("--from", help_result.stdout)


if __name__ == "__main__":
    unittest.main()
