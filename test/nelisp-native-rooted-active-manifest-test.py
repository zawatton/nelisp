"""Active source manifest refusal controls; no compilation or native launch."""
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "rooted_active", Path(__file__).resolve().parents[1] / "scripts/nelisp-native-rooted-prelink-closure.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


class ActiveManifestControls(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.builder = self.root / "builder.el"
        self.builder.write_text(";;; Genuine selected build source\n")
        self.directory = self.root / "units"
        self.directory.mkdir()
        self.helper = self.directory / "ticket.o.unit"
        self.helper.write_text('(:name "ticket.o" :sections ((text :nelisp-cache-bytes-hex "c3")))')
        arena = self.directory / "arena-base.o.unit"
        arena.write_text('(:name "arena-base.o" :sections ((bss . 4096)))')
        self.metadata = self.directory / "metadata.json"
        self.metadata.write_text(json.dumps([dict(name="ticket.o", path=self.helper.name,
                                                  **{"unit-sha256": digest(self.helper)})]))
        self.data = self.directory / "data.json"
        self.data.write_text(json.dumps({"unit": "arena-base.o", "owner-source-sha256": digest(self.builder)}))
        self.manifest = self.directory / "active-build.json"
        self.manifest.write_text(json.dumps({
            "domain": "nelisp-rooted-active-build-v1", "builder-source": "builder.el",
            "builder-sha256": digest(self.builder), "metadata-sha256": digest(self.metadata),
            "generated-data-sha256": digest(self.data), "units": [
                {"name": "ticket.o", "path": self.helper.name, "unit-sha256": digest(self.helper)},
                {"name": "arena-base.o", "path": arena.name, "unit-sha256": digest(arena)}]}))

    def tearDown(self):
        self.temporary.cleanup()

    def check(self):
        return module.verify_manifest(self.manifest, self.metadata, self.directory, self.data, self.root)

    def test_genuine_manifest_and_changed_builder_refusal(self):
        self.assertEqual(self.check(), digest(self.manifest))
        self.builder.write_text(";;; Changed source\n")
        with self.assertRaisesRegex(ValueError, "changed builder"):
            self.check()

    def test_changed_and_missing_helper_refusal(self):
        original = self.helper.read_bytes()
        self.helper.write_bytes(original + b"\n")
        with self.assertRaisesRegex(ValueError, "changed active unit"):
            self.check()
        self.helper.unlink()
        with self.assertRaises(FileNotFoundError):
            self.check()

    def test_changed_metadata_and_dropped_active_record_refusal(self):
        document = json.loads(self.manifest.read_text())
        document["units"].pop(0)
        self.manifest.write_text(json.dumps(document))
        with self.assertRaisesRegex(ValueError, "Incomplete active text"):
            self.check()
        self.metadata.write_text("[]")
        with self.assertRaisesRegex(ValueError, "Changed active metadata"):
            self.check()


if __name__ == "__main__":
    unittest.main()
