#!/usr/bin/env python3
"""Run the unchanged weak oracle slice; optionally measure full/sliced GNU loops."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native", type=Path)
    parser.add_argument("--sha256", help="Require this exact native executable identity")
    parser.add_argument("--expect-red", action="store_true",
                        help="Require a complete, successful native run that differs from GNU")
    parser.add_argument("--measure", action="store_true")
    parser.add_argument("--out", type=Path, default=Path("target/weak-word"))
    args = parser.parse_args()
    if args.expect_red and not (args.native and args.sha256):
        parser.error("--expect-red requires --native and --sha256")
    resource.setrlimit(resource.RLIMIT_AS, (8 * 1024**3, 8 * 1024**3))
    output = (ROOT / args.out).resolve()
    output.mkdir(parents=True, exist_ok=True)
    gnu = os.environ.get("EMACS", "emacs")
    generated = output / "probe.el"
    binary_sha256 = hashlib.sha256(args.native.read_bytes()).hexdigest() if args.native else None
    if args.sha256 and binary_sha256 != args.sha256:
        raise RuntimeError("Native executable identity changed")
    generation = subprocess.run(
        [gnu, "-Q", "--batch", "-l", str(ROOT / "tools/ai/weak-word-probe.el")],
        capture_output=True, timeout=30, check=True)
    if generation.stderr:
        raise RuntimeError(generation.stderr.decode())
    generated.write_bytes(generation.stdout)
    pinned = (ROOT / "tools/ai/eqhash-semantics.expected").read_bytes()
    rows = [line for line in pinned.splitlines()
            if line.startswith((b"(eqhash weak-", b"(eqhash weakness-t-alias "))]
    if len(rows) != 24:
        raise RuntimeError(f"Oracle structure changed: {len(rows)} weak rows")
    expected = b"\n".join(rows) + b"\n(eqhash-complete 24)\n"
    (output / "expected").write_bytes(expected)
    command = ([str(args.native.resolve()), "--load", str(generated)] if args.native
               else [gnu, "-Q", "--batch", "-l", str(generated)])
    started = time.monotonic()
    result = subprocess.run(command, capture_output=True, timeout=120)
    elapsed = time.monotonic() - started
    (output / "stdout").write_bytes(result.stdout)
    (output / "stderr").write_bytes(result.stderr)
    report = dict(rows=len(rows), seconds=elapsed, rc=result.returncode,
                  match=result.stdout == expected and not result.stderr,
                  binary_sha256=binary_sha256,
                  oracle_sha256=hashlib.sha256(pinned).hexdigest())
    actual_rows = result.stdout.splitlines()
    report["complete"] = (len(actual_rows) == 25 and
                          actual_rows[-1] == b"(eqhash-complete 24)" and
                          all(line.startswith(b"(eqhash ") for line in actual_rows[:-1]))
    report["mismatches"] = [row.decode().split(" ", 2)[1] for row, actual in
                            zip(rows, actual_rows) if row != actual]
    if args.measure:
        samples = {}
        for name, source in (("full", ROOT / "tools/ai/eqhash-semantics.el"),
                             ("focused", generated)):
            times = []
            for _ in range(7):
                started = time.monotonic()
                run = subprocess.run([gnu, "-Q", "--batch", "-l", str(source)],
                                     capture_output=True, timeout=30, check=True)
                times.append(time.monotonic() - started)
                selected = b"\n".join(line for line in run.stdout.splitlines()
                                       if line.startswith((b"(eqhash weak-", b"(eqhash weakness-t-alias ")))
                if selected + b"\n(eqhash-complete 24)\n" != expected or run.stderr:
                    raise RuntimeError(f"Invalid measurement baseline: {name}")
            samples[name] = dict(median_seconds=statistics.median(times), samples=times)
        report["measurement"] = samples
    (output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report))
    if args.expect_red:
        return 0 if (report["rc"] == 0 and report["complete"] and not result.stderr
                     and report["mismatches"] and not report["match"]) else 1
    return 0 if report["rc"] == 0 and report["match"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
