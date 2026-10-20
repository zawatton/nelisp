#!/usr/bin/env python3
"""Compare natural and fixed-budget GC triggers in fresh processes.

Usage: word-heap-gc.py OUTPUT WORKLOAD CAND BASE HOTFIX BEFORE
The native +24 counter increments once in nl_gc_debt_rearm after each
non-recursive sweep entry. No explicit collection is added to the workload.
Fixed-budget mode uses existing diagnostic switches, not a runtime change.
"""
import concurrent.futures
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time


def main():
    output, workload = map(lambda p: Path(p).resolve(), sys.argv[1:3])
    output.mkdir(parents=True, exist_ok=True)
    binaries = dict(zip(("cand", "base", "hotfix", "before"), map(lambda p: Path(p).resolve(), sys.argv[3:])))
    symbols = {who: {parts[2]: int(parts[0], 16)
                     for line in subprocess.check_output(["nm", "-n", str(binary)], text=True).splitlines()
                     if len(parts := line.split()) == 3} for who, binary in binaries.items()}

    def run(job):
        who, layout, fixed = job
        key = f"{who}-{layout}-{fixed}"
        address = symbols[who]["nl_gc_stats"]
        prefix = output / f"{key}-prefix.el"
        prefix.write_text(("(nelisp--debug-switch 30 16777216)\n(nelisp--debug-switch 31 1)\n" if fixed else "")
                          + f'(princ (format "WH-GC-BEFORE %S\\n" (list (ptr-read-u64 {address} 8) (ptr-read-u64 {address} 16) (ptr-read-u64 {address} 24))))\n')
        expr = f'(princ (format "WH-GC-AFTER %S\\n" (list (ptr-read-u64 {address} 8) (ptr-read-u64 {address} 16) (ptr-read-u64 {address} 24))))'
        cache = output / f"{key}-cache"
        cache.mkdir(exist_ok=True)
        started = time.monotonic()
        result = subprocess.run([str(binaries[who]), "--load", str(prefix), "--load", str(workload), "--eval", expr],
                                env=dict(os.environ, ABI_LAYOUT_PAD="x" * layout,
                                         XDG_CACHE_HOME=str(cache), TMPDIR=str(output)),
                                capture_output=True, timeout=240)
        (output / f"{key}.out").write_bytes(result.stdout)
        (output / f"{key}.err").write_bytes(result.stderr)
        if result.returncode or result.stderr or b"ABI-REGEX-GC 24000" not in result.stdout:
            raise RuntimeError(f"{key}: exit={result.returncode}")
        values = re.findall(rb"WH-GC-(?:BEFORE|AFTER) \((\d+) (\d+) (\d+)\)", result.stdout)
        if len(values) != 2:
            raise ValueError(f"missing counters: {key}")
        row = dict(who=who, layout=layout, fixed_budget=fixed,
                   before=list(map(int, values[0])), after=list(map(int, values[1])),
                   seconds=time.monotonic() - started)
        (output / f"{key}.json").write_text(json.dumps(row) + "\n")
        print(json.dumps(row), flush=True)
        return row

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        rows = list(pool.map(run, [(who, layout, fixed) for fixed in (False, True)
                                   for who in binaries for layout in (0, 700, 1400, 2100)]))
    (output / "results.json").write_text(json.dumps(rows, indent=2) + "\n")


if __name__ == "__main__":
    main()
