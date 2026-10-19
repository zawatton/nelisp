#!/usr/bin/env python3
"""Controls for conversion calls, inline decoding and census certificates."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "census", Path(__file__).resolve().parents[1] / "tools/ai/box-census.py")
census = importlib.util.module_from_spec(spec)
spec.loader.exec_module(census)


class InlineCensusTest(unittest.TestCase):
    def idioms(self, body):
        root = Path(__file__).resolve().parents[1]
        with tempfile.TemporaryDirectory(dir=root / "target/tmp") as directory:
            directory = Path(directory)
            source = directory / "dump.jsonl"
            output = directory / "census.tsv"
            source.write_text(json.dumps({"unit": "synthetic.o", "source": [
                "seq", ["defun", "synthetic", [], body]]}) + "\n")
            subprocess.run([sys.executable, str(root / "tools/ai/box-census.py"),
                            "from-dump", str(source), str(output)], check=True)
            return census.tally(census.load(output))[0]["idioms"]

    def test_direct_conversion_in_first_binding(self):
        self.assertEqual(1, self.idioms([
            "let*", [["word", ["nl_val_store_word", "view"]]], "word"
        ]).get("nl_val_store_word", 0))

    def test_reverse_conversion_in_first_binding(self):
        self.assertEqual(1, self.idioms([
            "let*", [["word", ["nl_abi_rc_to_word", ["producer", "out"], "out"]]], "word"
        ]).get("nl_abi_rc_to_word", 0))

    def test_external_conversion_in_first_binding(self):
        self.assertEqual(1, self.idioms([
            "let*", [["word", ["extern-call", "nl_val_store_word", "view"]]], "word"
        ]).get("nl_val_store_word", 0))

    def test_binding_name_and_quoted_data_are_not_calls(self):
        idioms = self.idioms([
            "let*", [["nl_val_store_word", 3]],
            ["quote", ["nl_abi_rc_to_word", "rc", "out"]]])
        self.assertEqual(0, idioms.get("nl_val_store_word", 0))
        self.assertEqual(0, idioms.get("nl_abi_rc_to_word", 0))

    def test_gate_rejects_older_conversion_walk(self):
        root = Path(__file__).resolve().parents[1]
        with tempfile.TemporaryDirectory(dir=root / "target/tmp") as directory:
            source = Path(directory) / "old.tsv"
            source.write_text("synthetic.o\tf\tdefun\t1\n"
                              "synthetic.o\tf\tinline-box-to-word\t0\n")
            result = subprocess.run([str(root / "tools/ai/abi-merge-gate.sh"),
                                     "store-word", str(source), "0"], capture_output=True)
            self.assertNotEqual(0, result.returncode)
            self.assertIn(b"regenerate", result.stderr)

    def test_gate_accepts_a_certified_zero_source(self):
        root = Path(__file__).resolve().parents[1]
        with tempfile.TemporaryDirectory(dir=root / "target/tmp") as directory:
            source = Path(directory) / "dump.jsonl"
            output = Path(directory) / "census.tsv"
            source.write_text(json.dumps({"unit": "synthetic.o", "source": [
                "seq", ["defun", "f", [], 3]]}) + "\n")
            subprocess.run([sys.executable, str(root / "tools/ai/box-census.py"),
                            "from-dump", str(source), str(output)], check=True)
            result = subprocess.run([str(root / "tools/ai/abi-merge-gate.sh"),
                                     "store-word", str(output), "0"], capture_output=True)
            self.assertEqual(0, result.returncode, result.stderr)

    def count(self, body):
        return census.inline_conversions(["defun", "synthetic", [], body])["synthetic"]

    def test_producer_out_decoded_inline(self):
        self.assertEqual(1, self.count([
            "let*", [["box", ["alloc-bytes", 32, 8]]],
            ["seq", ["producer", "arg", "box"],
             ["if", ["=", ["ptr-read-u8", "box", 0], 2],
              ["+", ["*", ["ptr-read-u64", "box", 8], 4], 1], "box"]]]))

    def test_fresh_heap_object_is_not_a_conversion(self):
        self.assertEqual(0, self.count([
            "let*", [["box", ["alloc-bytes", 32, 8]]],
            ["seq", ["producer", "arg", "box"], "box"]]))

    def test_borrowed_view_decode_is_still_debt(self):
        self.assertEqual(1, self.count([
            "if", ["=", ["ptr-read-u8", "view", 0], 2],
            ["+", ["*", ["ptr-read-u64", "view", 8], 4], 1], "view"]))

    def test_canonical_symbol_name_decode(self):
        self.assertEqual(1, self.count([
            "let*", [["box", ["alloc-bytes", 32, 8]]],
            ["seq", ["nl_producer", "bytes", "n", "box"],
             ["if", ["nl_name_is_nil", "bytes", "n"], 3,
              ["if", ["nl_name_is_t", "bytes", "n"], 7, "box"]]]]))

    def test_unrelated_numeric_payload_is_not_a_conversion(self):
        self.assertEqual(0, self.count([
            "+", ["*", ["ptr-read-u64", "counter", 8], 4], 1]))

    def test_selecting_immediates_before_allocating_the_object_is_not_debt(self):
        self.assertEqual(0, self.count([
            "if", ["nl_name_is_nil", "bytes", "n"], 3,
            ["if", ["nl_name_is_t", "bytes", "n"], 7,
             ["let*", [["box", ["alloc-bytes", 32, 8]]],
              ["seq", ["nl_symbol_producer", "bytes", "n", "box"], "box"]]]]))


if __name__ == "__main__":
    unittest.main()
