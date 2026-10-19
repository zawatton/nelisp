#!/usr/bin/env python3
"""Controls for inline box decoding, independent of adapter call names."""
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location(
    "census", Path(__file__).resolve().parents[1] / "tools/ai/box-census.py")
census = importlib.util.module_from_spec(spec)
spec.loader.exec_module(census)


class InlineCensusTest(unittest.TestCase):
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
