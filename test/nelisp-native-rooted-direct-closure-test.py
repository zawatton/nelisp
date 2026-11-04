"""Actual source-unit direct-edge controls; this is not native qualification."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "rooted_direct", Path(__file__).resolve().parents[1] / "scripts/nelisp-native-rooted-direct-closure.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class DirectClosureControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.image = Path(os.environ["NELISP_ROOT_PROOF_IMAGE"])
        cls.metadata = Path(os.environ["NELISP_ROOT_PROOF_METADATA"])
        cls.directory = Path(os.environ["NELISP_ROOT_UNIT_DIRECTORY"])
        cls.roots = ["nl_root_pin_begin_v2"]
        cls.proof = module.prove(cls.image, cls.metadata, cls.directory, cls.roots)

    def test_genuine_same_unit_targets_are_owned(self):
        targets = {edge["target"] for edge in self.proof["records"][0]["direct"]}
        self.assertEqual(targets, {"nl_root_pin_v2_next_token", "nl_root_pin_v2_commit_token"})
        self.assertEqual({record["name"] for record in self.proof["records"]},
                         targets | {"nl_root_pin_begin_v2"})

    def test_dropped_direct_target_cannot_mint_proof(self):
        forged = copy.deepcopy(self.proof)
        forged["records"][0]["direct"].pop(0)
        with self.assertRaisesRegex(ValueError, "drops or changes"):
            module.prove(self.image, self.metadata, self.directory, self.roots, forged)

    def test_dropped_helper_body_cannot_mint_proof(self):
        forged = copy.deepcopy(self.proof)
        forged["records"].pop()
        with self.assertRaisesRegex(ValueError, "drops or changes"):
            module.prove(self.image, self.metadata, self.directory, self.roots, forged)

    def test_duplicate_owner_refused(self):
        units = json.loads(self.metadata.read_text())
        units.append(copy.deepcopy(units[0]))
        with tempfile.TemporaryDirectory(prefix="nelisp-root-owner-") as temporary:
            metadata = Path(temporary) / "duplicate.json"
            metadata.write_text(json.dumps(units))
            with self.assertRaisesRegex(ValueError, "duplicate unit function owner"):
                module.prove(self.image, metadata, self.directory, self.roots)


if __name__ == "__main__":
    unittest.main()
