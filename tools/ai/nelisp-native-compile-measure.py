#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Measure P1.3 with the unchanged timer and three fresh private caches."""
import argparse
import json
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default="target/nelisp-static")
    parser.add_argument("--work", type=Path, help="New evidence directory; existing paths are refused.")
    parser.add_argument("--backend", choices=("in-house", "gccjit", "template"), default=None)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    if args.backend:
        command = [sys.executable, "test/support/run-native-template-trust.py",
                   "--backend", args.backend, "--samples", "3"]
        if args.work is not None:
            command += ["--work", str(args.work)]
        command += [args.binary]
        return subprocess.run(command, cwd=root).returncode
    if args.work is None:
        work = Path(tempfile.mkdtemp(prefix="p13-meter-", dir=root / "target/p1c"))
    else:
        work = root / args.work
        work.mkdir(mode=0o700, parents=True, exist_ok=False)
    rows = []
    for index in range(1, 4):
        run = work / str(index)
        with (work / f"timer-{index}.out").open("w") as output:
            subprocess.run(
                [sys.executable, "target/p1c/timeline.py", str(run), args.binary,
                 "compile", "target/progress/general-native-r9-codex-20261003/t1-gate-probe.el"],
                cwd=root, stdout=output, stderr=subprocess.STDOUT, check=True)
        checks = json.loads((run / "compile-results.json").read_text())
        receipt = json.loads((run / "compile-timeline.json").read_text())
        if not checks["complete"] or checks["failures"] or receipt["rc"]:
            raise RuntimeError("Incomplete compiler assertions")
        values = [float(n["text"]) for n in checks["notes"] if n["label"] == "compile-seconds"]
        if len(values) != 1 or not 0 < values[0] < 300:
            raise RuntimeError("Missing or invalid compile timing")
        rows.append(dict(run=str(run.relative_to(root)), compile_seconds=values[0],
                         load_before=receipt["load_before"], load_after=receipt["load_after"], load_peak=receipt["load_peak"],
                         binary_sha256=receipt["binary_sha256"], cold_sha256=receipt["cold_sha256"]))
        report = dict(cap_seconds=27, runs=rows, complete=len(rows) == 3,
                      median_seconds=statistics.median(r["compile_seconds"] for r in rows))
        (work / "acceptance.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(rows[-1]), flush=True)
    passed = report["median_seconds"] <= 27 and all(max(r["load_before"][0], r["load_after"][0], r["load_peak"]) < 4 for r in rows)
    print(f"P1.3 median={report['median_seconds']:.6f} pass={passed} evidence={work}", flush=True)
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
