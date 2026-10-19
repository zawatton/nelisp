#!/usr/bin/env python3
"""Exercise full ABI diagnostics and their unchanged failure verdict."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile

checker = Path(sys.argv[1]) if len(sys.argv) > 1 else (
    Path(__file__).resolve().parents[1] / "tools/ai/abi-partition.py")
with tempfile.TemporaryDirectory() as temporary:
    root = Path(temporary)
    names = ["unconverted_%d" % i for i in range(13)]
    source = [["defun", "nelisp_apply_function_w", [], 3]]
    source += [["defun", name, ["out"], 0] for name in names]
    dump = root / "dump.jsonl"
    dump.write_text(json.dumps({"unit": "fixture", "source": source}) + "\n")
    partition = root / "partition.json"
    partition.write_text(json.dumps({"groups": {"G4": {
        "arms": [], "defuns": [{"defun": n} for n in names[:-1]],
        "tail": [{"defun": names[-1]}]}}}))

    def run(*flags):
        return subprocess.run([sys.executable, str(checker), "check", "G4",
                               str(dump), str(partition), "--defuns", *flags],
                              capture_output=True, text=True, check=False)

    short = run()
    full = run("--all")
    assert short.returncode == full.returncode == 1, (short, full)
    assert "unconverted defuns 13 FAIL" in full.stdout, full.stdout
    assert short.stdout.splitlines()[1:] == ["  " + n for n in names[:10]]
    assert full.stdout.splitlines()[1:] == ["  " + n for n in names], full.stdout
    # A real WORD definition, removal, and a removed OUT slot each convert.
    source += [["defun", n + "_w", [], 3] for n in names[:-2]]
    source = [d for d in source if d[1] != names[-2]]
    next(d for d in source if d[1] == names[-1])[2] = ["word"]
    dump.write_text(json.dumps({"unit": "fixture", "source": source}) + "\n")
    green = run("--all")
    assert green.returncode == 0 and "unconverted defuns 0 PASS" in green.stdout, green
print("ABI-PARTITION-OUTPUT checked=3 PASS")
