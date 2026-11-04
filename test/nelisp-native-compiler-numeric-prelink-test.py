"""Independent structural controls; never execute native code or issue seals."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

_spec = importlib.util.spec_from_file_location(
    "numeric_prelink", Path(__file__).resolve().parents[1]
    / "scripts/nelisp-native-compiler-numeric-prelink.py")
module = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(module)


def digest(raw):
    return hashlib.sha256(raw).hexdigest()


class NumericPrelinkControls(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.directory = self.root / "units"
        self.directory.mkdir()
        self.source = self.root / "builder.el"
        self.source.write_text(";; Explicit test-only source intent fixture.\n")
        self.metadata = self.directory / "metadata.json"
        self.data = self.directory / "data.json"
        self.manifest = self.directory / "manifest.json"
        self.binding = self.directory / "binding.json"
        self.make_fixture()

    def make_fixture(self, indirect=False, extra=0, data_ref=False):
        names = list(module.ROOTS) + ["extra_%d" % n for n in range(extra)]
        text, symbols, relocs = bytearray(), [], []
        for index, name in enumerate(names):
            start = len(text)
            symbols.append(dict(name=name, value=start, size=0,
                                section="text", bind="global", type="func"))
            target = (names[len(module.ROOTS)] if index == 0 and extra else
                      names[index + 1] if index >= len(module.ROOTS)
                      and index + 1 < len(names) else None)
            if data_ref and index == 0:
                text.extend(b"\x48\x8d\x05\x00\x00\x00\x00")
                relocs.append(dict(offset=start + 3, type="pc32", symbol="nl_arena_base",
                                   addend=0, section="text"))
            elif indirect and index == 0:
                text.extend(b"\xff\xd0")
            elif target:
                text.extend(b"\xe8\x00\x00\x00\x00")
                relocs.append(dict(offset=start + 1, type="pc32", symbol=target,
                                   addend=0, section="text"))
            text.extend(b"\xc3")
        unit = self.directory / "fixture.unit"
        unit.write_text('(unit (text :nelisp-cache-bytes-hex "%s"))' % text.hex())
        globals_unit = self.directory / "globals.unit"
        globals_unit.write_text("(unit (bss . 64))")
        metadata = [dict(name="fixture.o", path=unit.name,
                         **{"unit-sha256": digest(unit.read_bytes())},
                         symbols=symbols, relocations=relocs)]
        self.metadata.write_text(json.dumps(metadata))
        data = {"unit": "globals.o", "owner-source-sha256": digest(self.source.read_bytes()),
                "owner-forms-sha256": "0" * 64, "bss-size": 64,
                "symbols": [dict(name="nl_arena_base", value=0, size=8,
                                 section="bss", bind="global", type="object")]}
        self.data.write_text(json.dumps(data))
        manifest = {"domain": "nelisp-rooted-active-build-v1", "builder-source": "builder.el",
                    "builder-sha256": digest(self.source.read_bytes()),
                    "metadata-sha256": digest(self.metadata.read_bytes()),
                    "generated-data-sha256": digest(self.data.read_bytes()),
                    "units": [dict(name="fixture.o", path=unit.name,
                                   **{"unit-sha256": digest(unit.read_bytes())}),
                              dict(name="globals.o", path=globals_unit.name,
                                   **{"unit-sha256": digest(globals_unit.read_bytes())})]}
        self.manifest.write_text(json.dumps(manifest))
        binding = {"domain": "nelisp-compiler-numeric-source-binding-v1",
                   "builder_sha256": digest(self.source.read_bytes()),
                   "plus_body_sha256": "1" * 64,
                   "sources": [{"path": "builder.el", "sha256": digest(self.source.read_bytes())}],
                   "helpers": [dict(name=name, params=params, body_sha256="2" * 64,
                                    source="builder.el") for name, params in module.NUMERIC_PARAMS.items()]}
        self.binding.write_text(json.dumps(binding))
        self.reviewed = digest(self.binding.read_bytes())

    def certify(self, **kwargs):
        return module.certify(self.manifest, self.metadata, self.directory, self.data,
                              self.root, self.binding, self.reviewed, **kwargs)

    def test_complete_structural_fixture_has_no_semantic_authority(self):
        result = self.certify()
        self.assertEqual(result["roots"], list(module.ROOTS))
        self.assertEqual(len(result["records"]), len(module.ROOTS))
        self.assertEqual(result["operation_eligibility"], [])
        self.assertEqual(result["runtime_memory"], "pending")
        self.assertEqual(result["generic_call"], "unsupported")
        self.assertEqual(result["domain"], module.DOMAIN)
        self.assertEqual([x["params"] for x in result["semantic_intent"]["helpers"]],
                         list(module.NUMERIC_PARAMS.values()))

    def test_root_policy_and_order_refused(self):
        for roots in [module.ROOTS[:-1], module.ROOTS[::-1], module.ROOTS + ("extra",)]:
            with self.subTest(roots=roots), self.assertRaisesRegex(ValueError, "root policy"):
                self.certify(roots=roots)

    def test_actual_bss_relocation_dependency_is_pinned(self):
        self.make_fixture(data_ref=True)
        result = self.certify()
        self.assertEqual(result["data_dependencies"],
                         [dict(name="nl_arena_base", unit="globals.o", section="bss",
                               offset=0, declared_size=8, owner_bss_size=64,
                               owner_section_size=64, extent_verified=True,
                               declared_size_known=True)])

    def test_invalid_bss_offsets_spans_and_malformed_maps_refused(self):
        for value, size in [(-1, 8), (64, 0), (63, 2), (0, -1), ("0", 8)]:
            self.make_fixture(data_ref=True)
            data = json.loads(self.data.read_text())
            data["symbols"][0].update(value=value, size=size)
            self.data.write_text(json.dumps(data))
            manifest = json.loads(self.manifest.read_text())
            manifest["generated-data-sha256"] = digest(self.data.read_bytes())
            self.manifest.write_text(json.dumps(manifest))
            with self.subTest(value=value, size=size), self.assertRaises(ValueError):
                self.certify()

    def test_ordinary_data_extent_and_crossing_span(self):
        for offset, size, encoding in [(3, 1, True), (3, 2, True), (0, 1, False)]:
            self.make_fixture(data_ref=True)
            path = self.directory / "fixture.unit"
            path.write_text(path.read_text() + (' (rodata :nelisp-cache-bytes-hex "01020304")'
                                              if encoding else ' (rodata . "unknown")'))
            units = json.loads(self.metadata.read_text())
            units[0]["unit-sha256"] = digest(path.read_bytes())
            units[0]["symbols"].append(dict(name="fixture_data", value=offset, size=size,
                                           section="rodata", bind="global", type="object"))
            units[0]["relocations"][0]["symbol"] = "fixture_data"
            self.metadata.write_text(json.dumps(units))
            manifest = json.loads(self.manifest.read_text())
            manifest["units"][0]["unit-sha256"] = digest(path.read_bytes())
            manifest["metadata-sha256"] = digest(self.metadata.read_bytes())
            self.manifest.write_text(json.dumps(manifest))
            if encoding and size == 1:
                self.assertEqual(self.certify()["data_dependencies"][0]["owner_section_size"], 4)
            else:
                with self.subTest(offset=offset, size=size, encoding=encoding), self.assertRaises(ValueError):
                    self.certify()

    def test_ambiguous_data_owner_refused(self):
        self.make_fixture(data_ref=True)
        units = json.loads(self.metadata.read_text())
        units[0]["symbols"].append(dict(name="nl_arena_base", value=0, size=8,
                                       section="bss", bind="global", type="object"))
        self.metadata.write_text(json.dumps(units))
        manifest = json.loads(self.manifest.read_text())
        manifest["metadata-sha256"] = digest(self.metadata.read_bytes())
        self.manifest.write_text(json.dumps(manifest))
        with self.assertRaisesRegex(ValueError, "Ambiguous numeric data owner"):
            self.certify()

    def test_indirect_with_consistently_rehashed_inputs_refused(self):
        self.make_fixture(indirect=True)
        with self.assertRaisesRegex(ValueError, "Indirect control flow"):
            self.certify()

    def test_dropped_record_and_altered_claimed_text_refused(self):
        original = self.certify()
        for mutation in ["drop", "hash", "edge"]:
            claimed = copy.deepcopy(original)
            if mutation == "drop":
                claimed["records"].pop()
            elif mutation == "hash":
                claimed["records"][0]["normalized_sha256"] = "0" * 64
            else:
                claimed["records"][0]["direct"].append(dict(target="counterfeit"))
            with self.subTest(mutation=mutation), self.assertRaisesRegex(ValueError, "drops or changes"):
                self.certify(claimed=claimed)

    def test_manifest_source_metadata_units_and_binding_changes_during_proof(self):
        genuine = module._prelink.prove
        for label in ["source", "metadata", "unit", "manifest", "data", "binding"]:
            self.make_fixture()
            target = {"source": self.source, "metadata": self.metadata,
                      "unit": self.directory / "fixture.unit", "manifest": self.manifest,
                      "data": self.data, "binding": self.binding}[label]

            def change_after_proof(*args, **kwargs):
                result = genuine(*args, **kwargs)
                with target.open("ab") as stream:
                    stream.write(b"\n")
                return result

            with self.subTest(label=label), patch.object(module._prelink, "prove", change_after_proof):
                with self.assertRaises(ValueError):
                    self.certify()
            if label == "source":
                self.source.write_text(";; Explicit test-only source intent fixture.\n")

    def test_missing_helper_record_refused(self):
        units = json.loads(self.metadata.read_text())
        units[0]["symbols"].pop()
        self.metadata.write_text(json.dumps(units))
        manifest = json.loads(self.manifest.read_text())
        manifest["metadata-sha256"] = digest(self.metadata.read_bytes())
        self.manifest.write_text(json.dumps(manifest))
        with self.assertRaises(ValueError):
            self.certify()

    def test_review_pin_and_exact_signature_refused(self):
        original = self.reviewed
        self.reviewed = "0" * 64
        with self.assertRaisesRegex(ValueError, "review pin"):
            self.certify()
        self.reviewed = original
        binding = json.loads(self.binding.read_text())
        binding["helpers"][2]["params"] = ["scratch", "acc_bits", "list_ptr"]
        self.binding.write_text(json.dumps(binding))
        self.reviewed = digest(self.binding.read_bytes())
        with self.assertRaisesRegex(ValueError, "signature"):
            self.certify()

    def test_real_existing_helper_count_bound_refused(self):
        self.make_fixture(extra=193)
        with self.assertRaisesRegex(ValueError, "count bound"):
            self.certify()


if __name__ == "__main__":
    unittest.main()
