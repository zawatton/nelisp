#!/usr/bin/env python3
"""Negative controls for the Doc 215 four-layout measurement verdict."""
import contextlib
import importlib.util
import io
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location(
    "abi_ir", Path(__file__).resolve().parents[1] / "tools/ai/abi-ir.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class LayoutVerdictTest(unittest.TestCase):
    def verdict(self, samples, mode="regex"):
        with contextlib.redirect_stdout(io.StringIO()):
            return gate.summarize(mode, samples)

    def samples(self):
        return [(k, (100, 10, b"done"), (99, 10, b"done")) for k in gate.LAYOUTS]

    def test_mean_allows_one_regressing_layout(self):
        samples = self.samples()
        samples[0] = (0, (100, 10, b"done"), (102, 10, b"done"))
        self.assertTrue(self.verdict(samples))

    def test_regressing_mean_fails(self):
        samples = self.samples()
        samples[0] = (0, (100, 10, b"done"), (104, 10, b"done"))
        self.assertFalse(self.verdict(samples))

    def test_mismatching_output_in_one_layout_fails(self):
        samples = self.samples()
        samples[1] = (700, (100, 10, b"done"), (99, 10, b"bad"))
        self.assertFalse(self.verdict(samples))

    def test_common_output_difference_between_layouts_fails(self):
        samples = self.samples()
        samples[1] = (700, (100, 10, b"bad"), (99, 10, b"bad"))
        self.assertFalse(self.verdict(samples))

    def test_too_few_gc_cycles_in_one_layout_fails(self):
        samples = self.samples()
        samples[2] = (1400, (100, 9, b"done"), (99, 10, b"done"))
        self.assertFalse(self.verdict(samples))
        self.assertTrue(self.verdict(samples, mode="files"))

    def test_missing_layout_fails(self):
        self.assertFalse(self.verdict(self.samples()[:-1]))


if __name__ == "__main__":
    unittest.main()
