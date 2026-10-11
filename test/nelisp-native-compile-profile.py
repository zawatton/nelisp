#!/usr/bin/env python3
"""Profile the existing P3.5 producer without widening its qualification deadline.

Diagnostic compiles have a separate <=600s bound. Qualification still uses
nelisp-native-boundary-test.py's unchanged 290s limit and parity corpus.
Existing fixture inputs are required; generate them with
FP_ROOT=$PWD FP_OUT=<work> emacs -Q --batch -L lisp -L scripts
  --load test/nelisp-native-boundary-fixtures.el
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("boundary", ROOT / "test/nelisp-native-boundary-test.py")
BOUNDARY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BOUNDARY)


def intervals(lines):
    """Pair nested stage markers, retaining incomplete phases at a timeout."""
    starts, spans = {}, []
    for line in lines:
        match = re.match(r"(\S+) seconds=([0-9.]+)", line)
        if not match:
            continue
        name, stamp = match[1], float(match[2])
        if name.endswith("-start"):
            starts.setdefault(name[:-6], []).append(stamp)
        elif name.endswith("-end") and starts.get(name[:-4]):
            spans.append(dict(phase=name[:-4], start=starts[name[:-4]].pop(), end=stamp))
    return spans, {name: values for name, values in starts.items() if values}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--binary", type=Path, required=True)
    ap.add_argument("--work", type=Path, required=True)
    ap.add_argument("--name", choices=BOUNDARY.NAMES, required=True)
    ap.add_argument("--backend", choices=["in-house", "gccjit"], default="in-house")
    ap.add_argument("--phase", choices=["compile", "parity"], default="compile")
    ap.add_argument("--bound", type=int, default=600)
    args = ap.parse_args()
    if not 1 <= args.bound <= 600:
        ap.error("diagnostic bound must be 1..600 seconds")
    binary, work = args.binary.resolve(), args.work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    cache = work / (args.backend + "-cache")
    cache.mkdir(mode=0o700, exist_ok=True)
    prefix = work / (args.backend + "-" + args.name + "-" + args.phase + "-profile")
    stage = prefix.with_suffix(".stages")
    stage.unlink(missing_ok=True)
    inputs = [binary, Path(str(binary) + ".cold"), Path(str(binary) + ".native-startup.el"),
              work / ("input-" + args.name + ".elc"), work / ("source-" + args.name + ".el"),
              ROOT / "test/support/native-compile-profile.el", ROOT / "test/nelisp-native-boundary-driver.el",
              Path(__file__).resolve(), ROOT / "test/nelisp-native-boundary-test.py"]
    pins = {str(p): BOUNDARY.sha(p) for p in inputs}
    while os.getloadavg()[0] > 6:
        print("Waiting for load <=6", os.getloadavg(), flush=True)
        time.sleep(10)
    env = dict(os.environ, FP_OUT=str(work), FP_FIXTURE=str(work / "fixture"),
               FP_NAME=args.name, FP_BACKEND=args.backend, FP_PHASE=args.phase,
               FP_ROOT=str(ROOT), NELISP_HOME=str(ROOT), NELISP_NATIVE_CACHE=str(cache),
               NELISP_ROOTED_CFG_STAGE_LOG=str(stage))
    # The pinned non-PIE ELF's existing BSS telemetry has no loader import.
    # Derive its address from that exact executable; never assume an address.
    env.pop("NATIVE_COMPILE_PROFILE_GC_ADDRESS", None)
    with binary.open("rb") as handle:
        header = handle.read(18)
    if header == b"\x7fELF\x02\x01\x01\x00" + b"\x00" * 8 + b"\x02\x00":
        symbols = subprocess.check_output(["nm", str(binary)], text=True)
        matches = re.findall(r"^([0-9a-f]+) B nl_gc_stats$", symbols, re.M)
        if len(matches) == 1:
            env["NATIVE_COMPILE_PROFILE_GC_ADDRESS"] = str(int(matches[0], 16))
    cmd = [str(binary), "--cold-load-from", str(binary) + ".cold"]
    for directory in ["lisp", "src", "scripts", "packages/nl-ffi/src", "packages/nl-prelude/src"]:
        cmd += ["-L", str(ROOT / directory)]
    cmd += ["--load", str(ROOT / "test/support/native-compile-profile.el"),
            "--load", str(ROOT / "test/nelisp-native-boundary-driver.el")]
    row = BOUNDARY.run(cmd, env, prefix, args.bound)
    spans, unfinished = intervals(stage.read_text().splitlines() if stage.exists() else [])
    output, errors = prefix.with_suffix(".out").read_text(), prefix.with_suffix(".err").read_text()
    row.update(name=args.name, backend=args.backend, phase=args.phase, diagnostic_only=True,
               pins=pins, identity_unchanged=all(BOUNDARY.sha(p) == h for p, h in pins.items()),
               spans=spans, unfinished=unfinished, gc_pause_seconds=None,
               gc_counters=[line for line in (stage.read_text().splitlines() if stage.exists() else [])
                            if "cache-profile-gc-" in line])
    row["jit_counters"] = [line for line in (stage.read_text().splitlines() if stage.exists() else [])
                           if "cache-profile-jit-" in line]
    metadata = re.findall(r"profile-input-code-bytes=(\d+)-constants=(\d+)-blocks=(\d+)",
                          stage.read_text() if stage.exists() else "")
    row["input_metadata"] = [dict(bytecode_length=int(code), constants=int(constants),
                                  cfg_blocks=int(blocks))
                             for code, constants, blocks in metadata]
    row["phase_seconds"] = {}
    for span in spans:
        key = span["phase"]
        row["phase_seconds"][key] = row["phase_seconds"].get(key, 0) + span["end"] - span["start"]
    row["completed"] = row["rc"] == 0 and "P34-DONE\n" in output and not errors and row["identity_unchanged"]
    prefix.with_suffix(".json").write_text(json.dumps(row, indent=2) + "\n")
    print(json.dumps({k: row[k] for k in ["rc", "seconds", "completed", "load_before", "load_after", "phase_seconds", "unfinished"]}, indent=2))
    raise SystemExit(0 if row["completed"] else 1)


if __name__ == "__main__":
    main()
