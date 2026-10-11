#!/usr/bin/env python3
"""Run the property differential and benchmark without rebuilding the reader.

First save the baseline prelude to target/brief11/prelude-before.el.
Run: python3 tools/nelisp-text-property-check.py
Use --negative-control to prove a mutated query is rejected.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def validate(output, stderr, code, mode="all"):
    return (code == 0 and not stderr and
            output.count("ERT-ASSERTIONS PASS 5\n") == 1 and
            (mode == "bench" or output.count("DIFFERENTIAL PASS 2000 layouts "
             "8000 boundary queries 4000 lookups 2000 rebuilds\n") == 1) and
            (mode == "diff" or output.count("TIMING ") == 10) and
            output.endswith(("TEXT-PROPERTY-CHECK COMPLETE\n",
                             "TEXT-PROPERTY-CHECK COMPLETE\nt\n")))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default="target/ccore-native/nelisp-dynamic")
    parser.add_argument("--before", default="target/brief11/prelude-before.el")
    parser.add_argument("--negative-control", action="store_true")
    parser.add_argument("--mode", choices=("all", "diff", "bench"), default="all")
    args = parser.parse_args()
    if args.negative_control and args.mode == "bench":
        parser.error("The negative control requires differential mode")
    root = Path(__file__).resolve().parent.parent
    binary = root / args.binary
    before = root / args.before
    output_dir = root / "target/brief11"
    output_dir.mkdir(parents=True, exist_ok=True)
    if not before.is_file():
        parser.error("Save the baseline prelude with git show before running")
    env = dict(os.environ, TP_BEFORE=str(before), NELISP_NATIVE_STARTUP_TIER="template",
               TP_CHECK_MODE=args.mode)
    if args.negative_control:
        env["TP_CHECK_MUTATE"] = "1"
    else:
        env.pop("TP_CHECK_MUTATE", None)
    subprocess.run([env.get("EMACS", "emacs"), "-Q", "--batch", "-l",
                    "tools/nelisp-text-property-check.el"], cwd=root, env=env, check=True)
    identities = {str(path.relative_to(root)): digest(path) for path in
                  (binary, before, root / "target/brief11/old.el",
                   root / "target/brief11/new.el")}
    if identities["target/brief11/old.el"] == identities["target/brief11/new.el"]:
        parser.error("Old and new definitions are identical")
    while os.getloadavg()[0] > 6:
        print("Waiting for 1-minute load <= 6", flush=True)
        time.sleep(30)
    load_before = os.getloadavg()
    cpu_before = os.times().children_user + os.times().children_system
    started = time.monotonic()
    prefix = "negative" if args.negative_control else args.mode
    stdout_path = output_dir / (prefix + ".out")
    stderr_path = output_dir / (prefix + ".err")
    with stdout_path.open("w") as stdout, stderr_path.open("w") as stderr:
        result = subprocess.run([str(binary), "--load", "target/brief11/check.el"],
                                cwd=root, env=env, stdout=stdout, stderr=stderr, timeout=9000)
    elapsed = time.monotonic() - started
    cpu = os.times().children_user + os.times().children_system - cpu_before
    output = stdout_path.read_text()
    stderr = stderr_path.read_text()
    passed = validate(output, stderr, result.returncode, args.mode)
    unchanged = digest(binary) == identities[args.binary]
    rejected_mutation = ("Differential" in stderr and not passed)
    ok = unchanged and (rejected_mutation if args.negative_control else passed)
    evidence = dict(identities=identities, load_before=load_before,
                    load_after=os.getloadavg(), wall_seconds=elapsed, cpu_seconds=cpu,
                    returncode=result.returncode, accepted=passed,
                    mutation_rejected=rejected_mutation, binary_unchanged=unchanged)
    (output_dir / (prefix + ".json")).write_text(json.dumps(evidence, indent=2) + "\n")
    print(f"{'PASS' if ok else 'FAIL'} {prefix}: {output_dir / (prefix + '.json')}")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
