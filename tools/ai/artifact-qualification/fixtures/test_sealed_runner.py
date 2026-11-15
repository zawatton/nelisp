import hashlib
import json
import os
import re
import shutil
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path

from artifact_qualifier import BundleError
from sealed_runner import BundleTimeout, prepare_bundle, run_bundle


class SealedRunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name)
        self.root = self.base / "bundle"
        self.root.mkdir()
        self.rows = []
        dash = Path(shutil.which("dash")).resolve(strict=True)
        self._add_file(dash, "/bin/dash", "0555")
        deps = subprocess.check_output(["ldd", str(dash)], text=True)
        dep_paths = set(re.findall(r"=>\s*(/[^\s(]+)|^\s*(/[^\s(]+)\s+\(", deps, re.M))
        for pair in sorted(dep_paths):
            path_text = next((part for part in pair if part), None)
            if path_text and Path(path_text).exists():
                mode = "0555" if Path(path_text).name.startswith("ld-") else "0444"
                self._add_file(Path(path_text).resolve(strict=True), path_text, mode)
        share = self.root / "share"
        share.mkdir()
        data = share / "data"
        data.write_bytes(b"sealed payload\n")
        data.chmod(0o444)
        self.rows.append({
            "source": "share/data", "destination": "/bundle/data", "type": "file",
            "mode": "0444", "sha256": hashlib.sha256(data.read_bytes()).hexdigest(),
        })
        (share / "alias").symlink_to("data")
        self.rows.append({
            "source": "share/alias", "destination": "/bundle/alias", "type": "symlink",
            "mode": "0444", "target": "/bundle/data",
        })
        self.manifest_path = self.base / "manifest.json"
        self.sentinel_path = self.base / "host-only-marker"
        self.sentinel_path.write_text("must not enter namespace", encoding="utf-8")
        self.sentinel_fd = os.open(self.sentinel_path, os.O_RDONLY)
        self._write_manifest(self._positive_script())

    def tearDown(self):
        if getattr(self, "sentinel_fd", None) is not None:
            os.close(self.sentinel_fd)
        self.temp.cleanup()

    def _add_file(self, source, destination, mode):
        source = Path(source)
        source_rel = f"deps/{len(self.rows):03d}"
        target = self.root / source_rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(source.read_bytes())
        target.chmod(int(mode, 8))
        self.rows.append({
            "source": source_rel, "destination": destination, "type": "file",
            "mode": mode, "sha256": hashlib.sha256(target.read_bytes()).hexdigest(),
        })

    def _positive_script(self):
        sentinel = self.sentinel_fd
        return "\n".join((
            "IFS= read -r line < /bundle/alias || exit 10",
            '[ "$line" = "sealed payload" ] || exit 11',
            '[ -z "${HOST_SECRET+x}" ] || exit 12',
            '[ -z "${HOME+x}" ] || exit 23',
            '[ "$LANG" = C ] || exit 13',
            '[ "$PATH" = /bin ] || exit 14',
            '[ ! -e /etc/hostname ] || exit 15',
            '[ ! -e /host-only-marker ] || exit 16',
            f'[ ! -e /proc/self/fd/{sentinel} ] || exit 17',
            '[ ! -e /private/out/result ] || exit 18',
            'if printf "tamper\\n" > /bundle/data 2>/dev/null; then exit 22; fi',
            'printf "%s\\n" "$line" > /private/out/result || exit 19',
            'IFS= read -r back < /private/out/result || exit 20',
            '[ "$back" = "$line" ] || exit 21',
            'printf "PASS:%s:%s:%s\\n" "$back" "$LANG" "$PATH"',
        ))

    def _write_manifest(self, script):
        self.manifest = {
            "format": "artifact-bundle-v1",
            "inputs": self.rows,
            "execution": {
                "argv": ["/bin/dash", "-c", script],
                "cwd": "/bundle",
                "env": {"LANG": "C", "PATH": "/bin"},
                "private_outputs": ["/private/out"],
            },
        }
        raw = json.dumps(self.manifest, separators=(",", ":")).encode("utf-8")
        self.manifest_path.write_bytes(raw)
        self.manifest_hash = hashlib.sha256(raw).hexdigest()

    def test_positive_isolated_execution_and_snapshot_survives_backing_change(self):
        manifest_copy = Path(__file__).parents[1] / "results/positive-manifest.json"
        manifest_copy.write_bytes(self.manifest_path.read_bytes())
        with prepare_bundle(self.manifest_path, self.root, self.manifest_hash) as prepared:
            captured_fds = prepared.fds
            data = self.root / "share/data"
            data.chmod(0o644)
            data.write_bytes(b"changed after capture\n")
            data.chmod(0o444)
            result = prepared.run(timeout_seconds=2)
            self.assertEqual(result.returncode, 0, result.stderr.decode(errors="replace"))
            self.assertEqual(result.stdout, b"PASS:sealed payload:C:/bin\n")
        for fd in captured_fds:
            with self.assertRaises(OSError):
                os.fstat(fd)
        self.assertEqual((self.root / "share/data").read_bytes(), b"changed after capture\n")

    def test_changed_input_fails_through_runner_path(self):
        data = self.root / "share/data"
        data.chmod(0o644)
        data.write_bytes(b"mutated before capture\n")
        data.chmod(0o444)
        with self.assertRaisesRegex(BundleError, "^FILE_HASH:"):
            run_bundle(self.manifest_path, self.root, self.manifest_hash, timeout_seconds=2)

    def test_changed_manifest_with_old_external_hash_fails(self):
        raw = self.manifest_path.read_bytes().replace(b"/private/out", b"//private/out")
        self.manifest_path.write_bytes(raw)
        with self.assertRaisesRegex(BundleError, "^MANIFEST_HASH:"):
            run_bundle(self.manifest_path, self.root, self.manifest_hash, timeout_seconds=2)

    def test_timeout_kills_namespace_process(self):
        self._write_manifest("while :; do :; done")
        with self.assertRaises(BundleTimeout):
            run_bundle(self.manifest_path, self.root, self.manifest_hash, timeout_seconds=0.15)

    def test_ambient_stdin_pipe_is_not_visible_or_consumed(self):
        self._write_manifest(
            'if IFS= read -r line; then printf "HOST:%s\\n" "$line"; '
            'else printf "EOF\\n"; fi')
        reader, writer = os.pipe()
        os.write(writer, b"undeclared-stdin-sentinel\n")
        os.close(writer)
        saved_stdin = os.dup(0)
        try:
            os.dup2(reader, 0)
            result = run_bundle(self.manifest_path, self.root, self.manifest_hash, timeout_seconds=2)
        finally:
            os.dup2(saved_stdin, 0)
            os.close(saved_stdin)
        try:
            remaining = os.read(reader, 128)
        finally:
            os.close(reader)
        self.assertEqual(result.returncode, 0, result.stderr.decode(errors="replace"))
        self.assertEqual(result.stdout, b"EOF\n")
        self.assertEqual(remaining, b"undeclared-stdin-sentinel\n")


if __name__ == "__main__":
    unittest.main()
