#!/usr/bin/env python3
"""Adversarial tests for the hardlink boot verifier; no runtime build needed."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
FAKE = r'''#!/usr/bin/env python3
import json, os, pathlib, re, sys, time
case = os.environ["HARDLINK_FAKE_CASE"]
expression = sys.argv[-1]
cold = pathlib.Path(sys.argv[0] + ".cold")
linked = cold.exists() and cold.stat().st_nlink == 2
if case == "immediate-failure":
    sys.exit(7)
if "nelisp--arena-dump-image-stream" in expression:
    path = pathlib.Path(re.search(r'nelisp--arena-dump-image-stream "([^"]+)"', expression)[1])
    symbol = re.search(r'\(setq ([^ ]+) t\)', expression)[1]
    if case != "missing-dump":
        path.write_text(json.dumps({"symbol": symbol, "value": True}))
    print("t")
elif "boundp" in expression:
    symbol = re.search(r"boundp '([^)]+)", expression)[1]
    image = json.loads(cold.read_text())
    if case == "missing-sentinel" and linked:
        image.pop("symbol")
    if case == "wrong-sentinel" and linked:
        image["value"] = False
    if case == "single-missing" and not linked:
        image.pop("symbol")
    if case == "timeout" and linked:
        time.sleep(60)
    if case == "slow" and linked:
        time.sleep(2.1)
    valid = image.get("symbol") == symbol and image.get("value") is True
    output = "t\n" if valid else "nil\n"
    if linked and case == "empty-stdout": output = ""
    if linked and case == "extra-newline": output += "\n"
    sys.stdout.write(output)
    if linked and case == "stderr": print("unexpected warning", file=sys.stderr)
    if linked and case == "correct-then-crash": sys.exit(7)
else:
    # The original verifier probes arithmetic and ignores both status/streams.
    sys.stdout.write("33\n")
    if case == "correct-then-crash": sys.exit(7)
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--old-verifier", type=Path)
    parser.add_argument("--out", type=Path, default=ROOT / "target/resume-handoff/verifier-tests")
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=True)
    records = []
    started = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="hardlink-verifier-test.", dir=ROOT / "target") as temp:
        fixture = Path(temp)
        (fixture / "tools/ai").mkdir(parents=True)
        (fixture / "target").mkdir()
        binary = fixture / "fake-nelisp"
        binary.write_text(FAKE)
        binary.chmod(0o755)
        Path(str(binary) + ".cold").write_text('{"symbol":"base","value":true}')
        current = fixture / "tools/ai/current.sh"
        shutil.copy2(ROOT / "tools/ai/integ-wh-hardlink-boot.sh", current)

        def run(script, case, expected, label):
            env = dict(os.environ, HARDLINK_FAKE_CASE=case, INTEG_WH_BOOT_TIMEOUT="1")
            if case == "slow": env["INTEG_WH_BOOT_TIMEOUT"] = "4"
            begin = time.monotonic()
            result = subprocess.run(["bash", str(script), str(binary)], env=env,
                                    capture_output=True, timeout=10)
            seconds = time.monotonic() - begin
            (args.out / (label + ".stdout")).write_bytes(result.stdout)
            (args.out / (label + ".stderr")).write_bytes(result.stderr)
            assert result.returncode == expected, (label, expected, result.returncode,
                                                    result.stdout, result.stderr)
            assert not list((fixture / "target").glob("hardlink-boot.*")), "scratch directory leaked"
            records.append(dict(case=label, exit=result.returncode, seconds=round(seconds, 4)))

        if args.old_verifier:
            old = fixture / "tools/ai/old.sh"
            shutil.copy2(args.old_verifier, old)
            run(old, "immediate-failure", 0, "old-immediate-failure")
            run(old, "correct-then-crash", 0, "old-correct-then-crash")
        for case, expected in (("good", 0), ("immediate-failure", 7),
                               ("correct-then-crash", 7), ("single-missing", 1),
                               ("missing-sentinel", 1), ("wrong-sentinel", 1),
                               ("empty-stdout", 1), ("extra-newline", 1),
                               ("stderr", 1), ("missing-dump", 1),
                               ("timeout", 124), ("slow", 1)):
            run(current, case, expected, "new-" + case)
        # A successful ln command that fails to create the link must be detected.
        helpers = fixture / "helpers"
        helpers.mkdir()
        fake_ln = helpers / "ln"
        fake_ln.write_text("#!/bin/sh\nexit 0\n")
        fake_ln.chmod(0o755)
        original_path = os.environ["PATH"]
        try:
            os.environ["PATH"] = str(helpers) + os.pathsep + original_path
            run(current, "good", 1, "new-wrong-link-count")
        finally:
            os.environ["PATH"] = original_path
    (args.out / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    for record in records:
        print(json.dumps(record))
    print(f"HARDLINK-VERIFIER checked={len(records)} failures=0 seconds={time.monotonic() - started:.3f}")


if __name__ == "__main__":
    main()
