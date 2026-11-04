#!/usr/bin/env python3
"""Exercise a tiny reviewed literal selector and effective fail-closed controls."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location("runtime_profile", Path(__file__).with_name("runtime-profile-check.py"))
TOOL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TOOL)
FIXTURE_SPEC = importlib.util.spec_from_file_location("provider_fixtures", Path(__file__).with_name("runtime-provider-test.py"))
FIXTURE = importlib.util.module_from_spec(FIXTURE_SPEC)
FIXTURE_SPEC.loader.exec_module(FIXTURE)


class Selection(unittest.TestCase):
    def setup_fixture(self, replacement=False):
        temporary = tempfile.TemporaryDirectory(prefix="runtime-profile-fixture-")
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        FIXTURE.fixture(root)
        selector = root / "lisp/selectors.el"
        selected_entry = "entry-fixed.o" if replacement else "entry.o"
        selector.write_text('(defun select-reader () (quote ("' + selected_entry + '" "leaf.o")))\n'
                            '(defun select-baked () (quote ("entry.o" "leaf.o")))\n'
                            '(defun nelisp-standalone--unit-for (row) row)\n'
                            '(defun make-unit () (quote ("entry-fixed.o")))\n')
        parsed, hashes = TOOL.PROVIDER.read_selected(root, {"lisp/selectors.el": ["select-reader", "select-baked", "nelisp-standalone--unit-for", "make-unit"]})
        definitions = {f["name"]: dict(path="lisp/selectors.el", file_sha256=hashes["lisp/selectors.el"], line=f["line"],
                       form_sha256=TOOL.PROVIDER.sha(TOOL.PROVIDER.canonical(f["form"]).encode())) for f in parsed["lisp/selectors.el"]}
        profiles = {}
        for name in ["reader", "baked"]:
            profiles[name] = dict(selector="select-" + name, base="nelisp-standalone--manifest", exclude_kinds=[],
                                  exclude_units=[], overlays=[], replacements={}, generated=[], membership_conditions=[],
                                  generation_dependencies=[], overlay_transformers=[])
        if replacement:
            profiles["reader"]["replacements"] = {"entry.o": dict(output="entry-fixed.o", transformers=["make-unit"])}
        contract = dict(schema="nelisp-reviewed-native-profile-selectors-v1", sources=hashes, definitions=definitions, profiles=profiles)
        catalog = TOOL.PROVIDER.collect(root)
        return root, contract, catalog

    def test_literal_positive(self):
        root, contract, catalog = self.setup_fixture()
        result = TOOL.generate(root, contract, catalog)
        self.assertTrue(all(p["selection_complete"] for p in result["profiles"].values()))
        self.assertEqual([p["output_unit"] for p in result["profiles"]["reader"]["outputs"]], ["entry.o", "leaf.o"])
        self.assertEqual(TOOL.PROVIDER.canonical(result), TOOL.PROVIDER.canonical(TOOL.generate(root, contract, catalog)))

    def test_replacement_has_no_inherited_ownership(self):
        root, contract, catalog = self.setup_fixture(True)
        profile = TOOL.generate(root, contract, catalog)["profiles"]["reader"]
        self.assertTrue(profile["selection_complete"])
        replacement = next(p for p in profile["outputs"] if p["selection"] == "replacement")
        self.assertIsNone(replacement["output_symbols"])
        self.assertEqual(replacement["ownership"], "unqualified-generated")
        self.assertNotIn("definitions", replacement["original"])

    def test_missing_generator_definition(self):
        root, contract, catalog = self.setup_fixture(True)
        path = root / "lisp/selectors.el"
        path.write_text(path.read_text().replace('(defun make-unit () (quote ("entry-fixed.o")))\n', ''))
        profile = TOOL.generate(root, contract, catalog)["profiles"]["reader"]
        self.assertFalse(profile["selection_complete"])
        self.assertTrue(any(e["reason"] == "missing-or-duplicate-generator-definition" for e in profile["unresolved"]))

    def test_missing_generator_file(self):
        root, contract, catalog = self.setup_fixture(True)
        (root / "lisp/selectors.el").unlink()
        self.assertFalse(TOOL.generate(root, contract, catalog)["profiles"]["reader"]["selection_complete"])

    def test_changed_selector_shape_and_hash(self):
        root, contract, catalog = self.setup_fixture()
        path = root / "lisp/selectors.el"
        path.write_text(path.read_text().replace('(quote ("entry.o" "leaf.o"))', '(quote ("other.o"))'))
        profile = TOOL.generate(root, contract, catalog)["profiles"]["reader"]
        self.assertFalse(profile["selection_complete"])
        self.assertTrue(any(e["reason"] == "changed-selector-or-generator-shape" for e in profile["unresolved"]))

    def test_unresolved_target_condition(self):
        root, contract, catalog = self.setup_fixture()
        contract["profiles"]["reader"]["membership_conditions"] = ["target"]
        self.assertFalse(TOOL.generate(root, contract, catalog)["profiles"]["reader"]["selection_complete"])

    def test_missing_replacement_original(self):
        root, contract, _ = self.setup_fixture(True)
        path = root / TOOL.PROVIDER.AUTHORITY_PATH
        path.write_text(path.read_text().replace('(\"entry.o\" entry entry--source)', ''))
        profile = TOOL.generate(root, contract)["profiles"]["reader"]
        self.assertFalse(profile["selection_complete"])
        self.assertTrue(any(e["reason"] == "missing-replacement-original" for e in profile["unresolved"]))

    def test_stale_or_tampered_catalog(self):
        root, contract, catalog = self.setup_fixture()
        catalog["providers"][0]["unit"] = "forged.o"
        self.assertFalse(TOOL.generate(root, contract, catalog)["profiles"]["reader"]["selection_complete"])

    def test_source_tampering_with_stale_fixture(self):
        root, contract, catalog = self.setup_fixture()
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("(eval '(setq leaf--source '(defun replacement () 0)))\n")
        self.assertFalse(TOOL.generate(root, contract, catalog)["profiles"]["reader"]["selection_complete"])
        self.assertFalse(TOOL.generate(root, contract)["profiles"]["reader"]["selection_complete"])

    def test_stale_complete_report_is_not_current(self):
        root, contract, catalog = self.setup_fixture()
        before = TOOL.generate(root, contract, catalog)
        with (root / "lisp/selectors.el").open("a") as stream:
            stream.write("; source changed\n")
        after = TOOL.generate(root, contract, catalog)
        self.assertNotEqual(TOOL.PROVIDER.canonical(before), TOOL.PROVIDER.canonical(after))
        self.assertFalse(after["profiles"]["reader"]["selection_complete"])
        with self.assertRaisesRegex(ValueError, "Stale or tampered"):
            TOOL.complete_code(after, before)

    def test_tampered_report_cannot_grant_completeness(self):
        root, contract, catalog = self.setup_fixture()
        contract["profiles"]["reader"]["membership_conditions"] = ["target"]
        current = TOOL.generate(root, contract, catalog)
        tampered = copy.deepcopy(current)
        tampered["profiles"]["reader"]["selection_complete"] = True
        self.assertEqual(TOOL.complete_code(current, current), 1)
        with self.assertRaisesRegex(ValueError, "Stale or tampered"):
            TOOL.complete_code(current, tampered)

    def test_generation_dependency_does_not_change_membership(self):
        root, contract, catalog = self.setup_fixture(True)
        contract["profiles"]["reader"]["generation_dependencies"] = ["runtime-reload"]
        profile = TOOL.generate(root, contract, catalog)["profiles"]["reader"]
        self.assertTrue(profile["selection_complete"])
        self.assertTrue(all(p["membership_condition"] is None for p in profile["outputs"]))
        self.assertEqual(profile["generation_unresolved"][0]["dependency"], "runtime-reload")
        replacement = next(p for p in profile["outputs"] if p["selection"] == "replacement")
        self.assertEqual(replacement["ownership"], "unqualified-generated")

    def test_cli_rejects_modified_production_contract(self):
        root, _, _ = self.setup_fixture()
        copied = root / "copied-tools"
        copied.mkdir()
        for name in ["runtime-profile-check.py", "runtime-profile-contract.json", "runtime-provider-check.py"]:
            shutil.copy2(Path(__file__).with_name(name), copied / name)
        contract_path = copied / "runtime-profile-contract.json"
        altered = json.loads(contract_path.read_text())
        altered["profiles"]["reader"]["selector"] = "forged-selector"
        contract_path.write_text(json.dumps(altered))
        output = root / "contract-rejection.json"
        result = subprocess.run([os.environ.get("PYTHON", "python3"), str(copied / "runtime-profile-check.py"),
                                 "report", "--source-root", str(root), "--output", str(output)],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        document = json.loads(output.read_text())
        self.assertFalse(document["selection_complete"])
        self.assertEqual(document["error"], "Reviewed selector contract signature changed")

    def test_reader_process_failures_are_tooling_rc2(self):
        root, _, _ = self.setup_fixture()
        output = root / "reader-tool-failure.json"
        for error in [subprocess.TimeoutExpired(["emacs"], 25), subprocess.SubprocessError("reader process failure")]:
            with self.subTest(error=type(error).__name__), \
                 mock.patch.object(TOOL, "generate", side_effect=error), \
                 mock.patch("sys.argv", ["runtime-profile-check.py", "report", "--output", str(output)]), \
                 mock.patch("builtins.print"):
                self.assertEqual(TOOL.main(), 2)
                document = json.loads(output.read_text())
                self.assertFalse(document["selection_complete"])
                self.assertEqual(document["error"], str(error))


if __name__ == "__main__":
    unittest.main(verbosity=2)
