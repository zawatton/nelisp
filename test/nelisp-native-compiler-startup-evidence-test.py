"""Source-only constructor policy tests; requires an explicit captured fixture."""
import importlib.util
import json
import os
from pathlib import Path
import unittest

REPO = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location(
    "constructor_prelink", REPO / "scripts/nelisp-native-compiler-constructor-prelink.py")
constructor = importlib.util.module_from_spec(spec)
spec.loader.exec_module(constructor)
FIXTURE = Path(os.environ["NELISP_PRELINK_FIXTURE"])


class ConstructorPolicy(unittest.TestCase):
    def closure(self, roots, **kwargs):
        return constructor.prelink.prove(FIXTURE / "active-unit-metadata.json", FIXTURE,
                                         roots, FIXTURE / "generated-data-owner.json", **kwargs)

    def test_original_ticket_output_identical(self):
        expected = json.loads((FIXTURE / "prelink-closure.json").read_bytes())
        expected.pop("active_manifest_sha256")
        self.assertEqual(expected, self.closure(constructor.ROOTS[:7]))

    def test_constructor_union_explicit_bound(self):
        with self.assertRaisesRegex(ValueError, "Direct helper count bound"):
            self.closure(constructor.ROOTS)
        result = self.closure(constructor.ROOTS, max_functions=192)
        self.assertEqual((137, 44550), (len(result["records"]), result["total_bytes"]))
        self.assertEqual([], result["operation_eligibility"])

    def test_call_indirect_still_refused(self):
        with self.assertRaisesRegex(ValueError, "Indirect control flow refused: nl_apply_native_subr"):
            self.closure(["wf_bytecode_call_gateway"], max_functions=192)

    def test_operation_root_mutation_refused(self):
        with self.assertRaisesRegex(ValueError, "Constructor operation root policy differs"):
            constructor.prove(None, None, None, None, None,
                              roots=constructor.ROOTS[:-1])

    def test_unbounded_policy_refused(self):
        with self.assertRaisesRegex(ValueError, "Unknown direct helper count policy"):
            self.closure(constructor.ROOTS, max_functions=10000)

    def test_stale_builder_source_refused(self):
        with self.assertRaisesRegex(ValueError, "Missing or changed builder source"):
            constructor.prove(FIXTURE / "active-build.json",
                              FIXTURE / "active-unit-metadata.json", FIXTURE,
                              FIXTURE / "generated-data-owner.json", REPO)


if __name__ == "__main__":
    unittest.main()
