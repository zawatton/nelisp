#!/usr/bin/env python3
"""The partition CLI must expose every finding without changing its verdict."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = Path(os.environ.get("ABI_PARTITION_SCRIPT", ROOT / "tools/ai/abi-partition.py"))


class PartitionCLI(unittest.TestCase):
    def run_check(self, *flags, converted=False):
        names = [f"helper_{i}" for i in range(12)]
        source = ["seq", ["defun", "nelisp_apply_function_w", [], 3]]
        source += [["defun", name, ["_pad" if converted else "out"], 3]
                   for name in names]
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            dump, part = directory / "dump.jsonl", directory / "partition.json"
            dump.write_text(json.dumps({"unit": "fixture", "source": source}) + "\n")
            part.write_text(json.dumps({"groups": {"G4": {
                "arms": [], "defuns": [{"defun": name} for name in names]}}}))
            return subprocess.run([sys.executable, str(SCRIPT), "check", "G4",
                                   str(dump), str(part), "--defuns", *flags],
                                  capture_output=True, text=True)

    def test_default_stays_bounded(self):
        result = self.run_check()
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("unconverted defuns 12 FAIL", result.stdout)
        self.assertEqual(len(result.stdout.splitlines()), 11)

    def test_all_reports_every_finding(self):
        result = self.run_check("--all")
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout.splitlines()[1:],
                         [f"  helper_{i}" for i in range(12)])

    def test_converted_passes(self):
        result = self.run_check("--all", converted=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), [
            "ABI-CHECK G4 legacy arms 0, unconverted defuns 0 PASS"])


if __name__ == "__main__":
    unittest.main()
