import hashlib
import json
import os
import stat
import tempfile
import unittest
from pathlib import Path

from artifact_qualifier import BundleError, validate_bundle


class QualifierTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name)
        self.root = self.base / "bundle"
        (self.root / "bin").mkdir(parents=True)
        (self.root / "share").mkdir()
        (self.root / "bin/tool").write_bytes(b"pinned tool\n")
        (self.root / "bin/tool").chmod(0o555)
        (self.root / "share/data").write_bytes(b"password")
        (self.root / "share/data").chmod(0o444)
        (self.root / "share/alias").symlink_to("data")
        self.manifest = json.loads(Path(__file__).with_name("valid-manifest.json").read_text())
        self.manifest_path = self.base / "manifest.json"
        self._refresh_hashes()

    def tearDown(self):
        self.temp.cleanup()

    def _refresh_hashes(self):
        for row in self.manifest["inputs"]:
            if row["type"] == "file":
                row["sha256"] = hashlib.sha256((self.root / row["source"]).read_bytes()).hexdigest()
        self.manifest_path.write_text(json.dumps(self.manifest), encoding="utf-8")

    def check(self, category=None, expected=None):
        if expected is None:
            expected = hashlib.sha256(self.manifest_path.read_bytes()).hexdigest()
        if category:
            with self.assertRaisesRegex(BundleError, "^" + category + r":"):
                validate_bundle(self.manifest_path, self.root, expected)
        else:
            return validate_bundle(self.manifest_path, self.root, expected)

    def check_raw(self, raw, category):
        self.manifest_path.write_bytes(raw)
        expected = hashlib.sha256(raw).hexdigest()
        with self.assertRaisesRegex(BundleError, "^" + category + r":"):
            validate_bundle(self.manifest_path, self.root, expected)

    def mutate(self):
        self._refresh_hashes()

    def test_valid_sealed_bundle(self):
        self.assertEqual(self.check()["format"], "artifact-bundle-v1")

    def test_changed_file_bytes_fail(self):
        (self.root / "share/data").chmod(0o644)
        (self.root / "share/data").write_bytes(b"changed!")
        (self.root / "share/data").chmod(0o444)
        self.check("FILE_HASH")

    def test_changed_manifest_with_original_expected_hash_fails_first(self):
        original = hashlib.sha256(self.manifest_path.read_bytes()).hexdigest()
        self.manifest["execution"]["argv"].append("tampered")
        self.manifest_path.write_text(json.dumps(self.manifest), encoding="utf-8")
        self.check("MANIFEST_HASH", original)

    def test_missing_input_fails_closed(self):
        (self.root / "share/data").unlink()
        (self.root / "share/alias").unlink()
        self.check("MISSING_OR_ESCAPE")

    def test_extra_file_rejected(self):
        (self.root / "extra").write_text("undeclared")
        self.check("BUNDLE_CONTENTS")

    def test_wrong_mode_or_type_rejected(self):
        (self.root / "share/data").chmod(0o644)
        self.check("TYPE_OR_MODE")

    def test_escaped_symlink_rejected(self):
        (self.root / "share/alias").unlink()
        (self.root / "share/alias").symlink_to("../../outside")
        self.check("MISSING_OR_ESCAPE")

    def test_symlink_cycle_rejected(self):
        (self.root / "share/alias").unlink()
        (self.root / "share/alias").symlink_to("alias")
        self.check("LINK_CYCLE")

    def test_duplicate_destination_rejected(self):
        self.manifest["inputs"].append(dict(self.manifest["inputs"][1]))
        self.mutate()
        self.check("DUPLICATE_PATH")

    def test_unpinned_executable_rejected(self):
        self.manifest["execution"]["argv"][0] = "/bin/sh"
        self.mutate()
        self.check("UNPINNED_EXECUTABLE")

    def test_malformed_schema_rejected(self):
        self.manifest["execution"]["env"]["BAD-KEY"] = "x"
        self.mutate()
        self.check("SCHEMA")

    def test_path_traversal_rejected(self):
        self.manifest["inputs"][1]["destination"] = "/bundle/../escape"
        self.mutate()
        self.check("PATH")

    def test_link_to_undeclared_target_rejected(self):
        (self.root / "share/alias").unlink()
        (self.root / "share/alias").symlink_to("data")
        self.manifest["inputs"][2]["target"] = "/bundle/share/alias"
        self.mutate()
        self.check("LINK_ALLOWLIST")

    def test_double_slash_private_output_overlap_rejected(self):
        self.manifest["execution"]["private_outputs"] = ["//bundle/share"]
        self.mutate()
        self.check("PATH")

    def test_double_slash_executable_destination_alias_rejected(self):
        alias = dict(self.manifest["inputs"][0])
        alias["destination"] = "//bundle/bin/tool"
        self.manifest["inputs"].append(alias)
        self.mutate()
        self.check("PATH")

    def test_double_slash_argv_cwd_and_symlink_target_rejected(self):
        original = json.loads(json.dumps(self.manifest))
        mutations = (
            lambda: self.manifest["execution"]["argv"].__setitem__(0, "//bundle/bin/tool"),
            lambda: self.manifest["execution"].__setitem__("cwd", "//bundle"),
            lambda: self.manifest["inputs"][2].__setitem__("target", "//bundle/share/data"),
        )
        for mutate in mutations:
            with self.subTest():
                self.manifest = json.loads(json.dumps(original))
                mutate()
                self.mutate()
                self.check("PATH")

    def test_duplicate_json_keys_rejected_at_all_object_levels(self):
        raw = self.manifest_path.read_text(encoding="utf-8")
        cases = (
            raw.replace('"format": "artifact-bundle-v1"', '"format": "artifact-bundle-v1", "format": "artifact-bundle-v1"'),
            raw.replace('"source": "bin/tool"', '"source": "bin/tool", "source": "bin/tool"'),
            raw.replace('"env": {"LANG": "C"}', '"env": {"LANG": "X", "LANG": "C"}'),
        )
        for duplicate_json in cases:
            with self.subTest(duplicate_json=duplicate_json):
                self.check_raw(duplicate_json.encode("utf-8"), "DUPLICATE_JSON_KEY")


if __name__ == "__main__":
    unittest.main()
