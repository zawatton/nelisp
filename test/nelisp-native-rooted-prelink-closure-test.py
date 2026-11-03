"""Prelink unit controls; no ELF image or native process is read or launched."""
import copy
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "rooted_prelink", Path(__file__).resolve().parents[1] / "scripts/nelisp-native-rooted-prelink-closure.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class PrelinkControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.metadata = Path(os.environ["NELISP_ROOT_PROOF_METADATA"])
        cls.directory = Path(os.environ["NELISP_ROOT_UNIT_DIRECTORY"])
        cls.data = Path(os.environ["NELISP_ROOT_BSS_OWNER"])
        cls.postlink = json.loads(Path(os.environ["NELISP_ROOT_DIRECT_PROTOCOL"]).read_text())
        cls.proof = module.prove(cls.metadata, cls.directory, cls.postlink["roots"], cls.data)

    def test_actual_prelink_matches_all_postlink_edges_and_text(self):
        self.assertEqual(self.proof["records"], self.postlink["records"])
        self.assertEqual((len(self.proof["records"]), self.proof["total_bytes"]), (84, 33742))
        self.assertEqual(self.proof["operation_eligibility"], [])

    def test_dropped_direct_target_refused(self):
        forged = copy.deepcopy(self.proof)
        forged["records"][0]["direct"].pop()
        with self.assertRaisesRegex(ValueError, "drops or changes"):
            module.prove(self.metadata, self.directory, self.postlink["roots"], self.data, forged)

    def test_stale_unit_and_indirect_call_refused(self):
        units = json.loads(self.metadata.read_text())
        unit = next(x for x in units if x["name"] == "rootstack.o")
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            path = directory / unit["path"]
            raw = (self.directory / unit["path"]).read_bytes()
            path.write_bytes(raw + b"\n")
            metadata = directory / "metadata.json"
            metadata.write_text(json.dumps([unit]))
            with self.assertRaisesRegex(ValueError, "hash differs"):
                module.prove(metadata, directory, ["nl_root_pin_begin_v2"], self.data)
            match = re.search(rb'\(text :nelisp-cache-bytes-hex "([0-9a-f]+)"', raw)
            text = bytearray.fromhex(match[1].decode())
            symbol = next(x for x in unit["symbols"] if x["name"] == "nl_root_pin_begin_v2")
            text[symbol["value"]:symbol["value"] + 2] = b"\xff\xd0"
            mutated = raw[:match.start(1)] + text.hex().encode() + raw[match.end(1):]
            path.write_bytes(mutated)
            unit["unit-sha256"] = hashlib.sha256(mutated).hexdigest()
            metadata.write_text(json.dumps([unit]))
            with self.assertRaisesRegex(ValueError, "Indirect control flow"):
                module.prove(metadata, directory, ["nl_root_pin_begin_v2"], self.data)


if __name__ == "__main__":
    unittest.main()
