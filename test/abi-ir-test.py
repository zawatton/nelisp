#!/usr/bin/env python3
"""Negative controls for the Doc 215 four-layout measurement verdict."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "abi_ir", Path(__file__).resolve().parents[1] / "tools/ai/abi-ir.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class LayoutVerdictTest(unittest.TestCase):
    def verdict(self, samples, mode="regex-gc"):
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

    def test_plain_regex_keeps_its_own_ir_cap_without_the_gc_floor(self):
        samples = [(k, (100, 4, b"done"), (99, 4, b"done")) for k in gate.LAYOUTS]
        self.assertTrue(self.verdict(samples, mode="regex"))
        samples[0] = (0, (100, 4, b"done"), (104, 4, b"done"))
        self.assertFalse(self.verdict(samples, mode="regex"))


class RecordedEvidenceTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.inputs = {"candidate-path": "candidate", "baseline-path": "baseline"}
        for name in gate.ir_artifacts():
            (self.directory / name).write_text("ABI-REGEX 6000\nABI-REGEX-GC 24000\n")
        self.record = {"schema": 1, "modes": list(gate.IR_MODES),
                       "layouts": list(gate.LAYOUTS), "identities": self.inputs,
                       "completed_jobs": gate.completed_jobs(),
                       "artifacts": {n: gate.fingerprint(self.directory / n)
                                     for n in gate.ir_artifacts()}}
        self.write_record()

    def write_record(self):
        (self.directory / "measurement.json").write_text(json.dumps(self.record))

    def test_complete_unchanged_record_is_accepted(self):
        gate.validate_record(self.directory, self.inputs)

    def test_workload_mutation_during_measurement_is_rejected(self):
        before = {n: gate.fingerprint(self.directory / n)
                  for n in ("regex.el", "regex-gc.el")}
        gate.verify_workloads(self.directory, before)
        (self.directory / "regex-gc.el").write_text("different workload")
        with self.assertRaises(ValueError):
            gate.verify_workloads(self.directory, before)

    def test_swapped_binary_roles_are_rejected(self):
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, {"candidate-path": "baseline",
                                                   "baseline-path": "candidate"})

    def test_changed_raw_profile_is_rejected(self):
        (self.directory / "regex-gc-cand-0.cg").write_text("changed")
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)

    def test_missing_or_failed_job_is_rejected(self):
        self.record["completed_jobs"]["regex-gc-base-0"] = 1
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)
        del self.record["completed_jobs"]["regex-gc-base-0"]
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)

    def test_missing_marker_fails_even_if_its_hash_is_updated(self):
        name = "regex-gc-cand-0.out"
        (self.directory / name).write_text("incomplete")
        self.record["artifacts"][name] = gate.fingerprint(self.directory / name)
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)


if __name__ == "__main__":
    unittest.main()
