"""Comment-preserving manifest dependency edits and publication failure checks."""
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from nelisp_project_manifest import edit_dependencies, publish_pair

BASE = b'# project\n[package]\nname = "hello"\nversion = "0.1.0"\n\n[application]\nsource = "src/main.nl"\nentry = "main"\n'


class ManifestEdits(unittest.TestCase):
    def test_add_replace_remove_preserves_unrelated_bytes(self):
        source = BASE + b'\n[dependencies] # libraries\n# keep this note\n"json" = \'1\' # reason\nhttp = "2"\n'
        changed = edit_dependencies(source, "json", "1.2")
        self.assertEqual(changed, source.replace(b"'1'", b'"1.2"'))
        removed = edit_dependencies(changed, "json", None)
        self.assertIn(b"# keep this note\n# reason\nhttp", removed)
        self.assertTrue(removed.startswith(BASE))
        added = edit_dependencies(BASE, "json", "2")
        self.assertTrue(added.startswith(BASE))
        self.assertIn(b'[dependencies]\njson = "2"\n', added)

    def test_crlf_and_existing_table(self):
        source = (BASE + b'\n[dependencies]\nhttp = "2" # keep\n').replace(b'\n', b'\r\n')
        result = edit_dependencies(source, "json", "1")
        self.assertIn(b'http = "2" # keep\r\n', result)
        self.assertIn(b'json = "1"\r\n', result)
        self.assertNotIn(b'\n', result.replace(b'\r\n', b''))

    def test_unsupported_shape_is_rejected_before_edits(self):
        source = b'dependencies = {json = "1"}\n' + BASE
        with self.assertRaisesRegex(ValueError, "table"):
            edit_dependencies(source, "json", "2")

    def test_pair_publication_failure_restores_original_files(self):
        import os
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest, lock = root / "nelisp.toml", root / "nelisp.lock"
            manifest.write_bytes(BASE)
            manifest.chmod(0o640)
            lock.write_bytes(b"old lock")
            replace = os.replace

            def fail_second(source, target):
                if Path(target) == lock:
                    raise OSError("injected lock publication failure")
                return replace(source, target)

            with patch("nelisp_project_manifest.os.replace", side_effect=fail_second), self.assertRaises(OSError):
                publish_pair(root, BASE, b"old lock", b"new manifest", b"new lock")
            self.assertEqual(manifest.read_bytes(), BASE)
            self.assertEqual(lock.read_bytes(), b"old lock")
            self.assertEqual(manifest.stat().st_mode & 0o777, 0o640)
            self.assertEqual(sorted(p.name for p in root.iterdir()), ["nelisp.lock", "nelisp.toml"])

    def test_changed_inputs_are_preserved_and_rollback_failure_keeps_backup(self):
        import os
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest, lock = root / "nelisp.toml", root / "nelisp.lock"
            manifest.write_bytes(BASE)
            lock.write_bytes(b"changed concurrently")
            with self.assertRaisesRegex(ValueError, "inputs changed"):
                publish_pair(root, BASE, b"old lock", b"new manifest", b"new lock")
            self.assertEqual(manifest.read_bytes(), BASE)
            self.assertEqual(lock.read_bytes(), b"changed concurrently")
            replace = os.replace

            def fail_publish_and_restore(source, target):
                if Path(target) == lock or Path(source).name == "original.toml":
                    raise OSError("injected failure")
                return replace(source, target)

            with patch("nelisp_project_manifest.os.replace", side_effect=fail_publish_and_restore):
                with self.assertRaisesRegex(OSError, "recover original"):
                    publish_pair(root, BASE, b"changed concurrently", b"new manifest", b"new lock")
            backups = list(root.glob(".package-edit-*/original.toml"))
            self.assertEqual(len(backups), 1)
            self.assertEqual(backups[0].read_bytes(), BASE)
            self.assertEqual(lock.read_bytes(), b"changed concurrently")


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
