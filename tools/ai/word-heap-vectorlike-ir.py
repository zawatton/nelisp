#!/usr/bin/env python3
"""Measure/replay S4's boot, files and regex6000 total Ir on four layouts.

Uses the existing ABI profiler and its exact regex workload, retaining fresh
S1 baseline profiles separately from S4 candidate profiles.  No old evidence
is overwritten.  Every total includes GC; no helper self-cost is subtracted.
"""
import argparse
import ast
import concurrent.futures
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/word-heap-s4v/ir"
HELPER = ROOT / "tools/ai/abi-ir.py"
SPEC = importlib.util.spec_from_file_location("abi_ir", HELPER)
ABI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ABI)
MODES = ("boot", "files", "regex")
LAYOUTS = (0, 700, 1400, 2100)
WORKERS = 4


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def rel(path):
    return Path(path).resolve().relative_to(ROOT).as_posix()


def binary_for(which, linkage):
    lane = "word-heap-s1" if which == "baseline" else "word-heap-s4v"
    return ROOT / "target" / lane / linkage / f"nelisp-{linkage}"


def recipe():
    """Read the primary harness's literal, instead of maintaining a copy."""
    matches = []
    for node in ast.walk(ast.parse(HELPER.read_text())):
        if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                and node.func.attr == "write_text"
                and isinstance(node.func.value, ast.BinOp)
                and isinstance(node.func.value.right, ast.Constant)
                and node.func.value.right.value == "regex.el"):
            matches.append(ast.literal_eval(node.args[0]))
    need(len(matches) == 1 and "ABI-REGEX 6000" in matches[0], "exact primary regex workload missing")
    return matches[0]


def identities(binary):
    files = [Path(__file__), HELPER, ROOT / "tools/ai/abi-merge-gate.sh"]
    files.extend(ROOT / name for name in ABI.FILE_SOURCES)
    files.extend(Path(str(binary) + suffix) for suffix in ("", ".cold", ".native-startup.el"))
    runtime = {k: v for k, v in os.environ.items()
               if k.startswith(("NELISP_", "M5_", "LD_", "LC_")) or k == "LANG"}
    tools = {name: sha(Path(shutil.which(name)).resolve()) for name in ("valgrind", "nm")}
    return dict(files={rel(path): sha(path) for path in files}, tools=tools,
                runtime_environment=hashlib.sha256(json.dumps(runtime, sort_keys=True).encode()).hexdigest())


def measure(which, linkages):
    need(shutil.disk_usage(ROOT).free >= 50 * 1024 ** 3, "less than50GB disk free")
    OUT.mkdir(parents=True, exist_ok=True)
    dirs, before, binaries = {}, {}, {}
    for linkage in linkages:
        directory = OUT / f"{which}-{linkage}"
        directory.mkdir()  # Refuse to replace any measured profile.
        binary = binary_for(which, linkage)
        before[linkage] = identities(binary)
        binaries[linkage] = binary
        dirs[linkage] = directory
        (directory / "regex.el").write_text(recipe())
    started, cancel = time.monotonic(), threading.Event()

    def terminate(signum, _frame):
        if not cancel.is_set():
            cancel.set()
            raise SystemExit(128 + signum)

    previous = signal.signal(signal.SIGTERM, terminate)
    pool = concurrent.futures.ThreadPoolExecutor(max_workers=WORKERS)
    results = {}
    try:
        jobs = {pool.submit(ABI.run_one, dirs[linkage], mode, which, binaries[linkage], layout, cancel):
                (linkage, mode, layout) for linkage in linkages for mode in MODES for layout in LAYOUTS}
        for job in concurrent.futures.as_completed(jobs):
            key = jobs[job]
            results[key] = job.result()
            print(f"{which} {key[0]} {key[1]} layout={key[2]} Ir={results[key][0]} GC={results[key][1]}", flush=True)
    finally:
        cancel.set()
        try:
            pool.shutdown(wait=True, cancel_futures=True)
        finally:
            signal.signal(signal.SIGTERM, previous)
    for linkage, directory in dirs.items():
        need(before[linkage] == identities(binaries[linkage]), "Ir inputs changed during capture")
        need((directory / "regex.el").read_text() == recipe(), "regex workload changed during capture")
        artifacts = [directory / f"{mode}-{which}-{layout}.{suffix}" for mode in MODES
                     for layout in LAYOUTS for suffix in ("cg", "out", "err")]
        artifacts.append(directory / "regex.el")
        record = dict(schema=1, which=which, linkage=linkage, modes=MODES, layouts=LAYOUTS,
                      identities=before[linkage], profiles=12,
                      seconds=time.monotonic() - started,
                      artifacts={path.name: sha(path) for path in artifacts})
        (directory / "measurement.json").write_text(json.dumps(record, indent=2) + "\n")
    print(f"S4 Ir capture {which} PASS profiles={12 * len(linkages)} seconds={time.monotonic()-started:.3f}", flush=True)


def replay(which, linkage):
    directory = OUT / f"{which}-{linkage}"
    binary = binary_for(which, linkage)
    record = json.loads((directory / "measurement.json").read_text())
    need(record["schema"] == 1 and record["which"] == which and record["linkage"] == linkage
         and record["modes"] == list(MODES) and record["layouts"] == list(LAYOUTS)
         and record["profiles"] == 12, "incomplete Ir measurement")
    need(record["identities"] == identities(binary), "stale Ir input identity")
    expected = {f"{mode}-{which}-{layout}.{suffix}" for mode in MODES
                for layout in LAYOUTS for suffix in ("cg", "out", "err")} | {"regex.el"}
    need(set(record["artifacts"]) == expected, "incomplete Ir profile set")
    need(all(sha(directory / name) == digest for name, digest in record["artifacts"].items()), "changed Ir raw artifact")
    need((directory / "regex.el").read_text() == recipe(), "changed primary regex recipe")
    rows = {}
    for mode in MODES:
        rows[mode] = []
        for layout in LAYOUTS:
            stem = directory / f"{mode}-{which}-{layout}"
            ABI.check_marker(stem, mode)
            total, cycles = ABI.profile_counts(stem.with_suffix(".cg"), binary)
            rows[mode].append((layout, total, cycles, stem.with_suffix(".out").read_bytes()))
    return rows


def check(linkages):
    ok, result = True, {}
    for linkage in linkages:
        need(sha(binary_for("baseline", linkage)) != sha(binary_for("candidate", linkage)), "same executable used as candidate and baseline")
        baseline, candidate = replay("baseline", linkage), replay("candidate", linkage)
        result[linkage] = {}
        for mode in MODES:
            samples = [(layout, tuple(b[1:]), tuple(c[1:]))
                       for layout, b, c in zip(LAYOUTS, baseline[mode], candidate[mode], strict=True)]
            passed = ABI.summarize(mode, samples, 1.0)
            ok &= passed
            base_ir = sum(row[1] for row in baseline[mode]) / 4
            cand_ir = sum(row[1] for row in candidate[mode]) / 4
            result[linkage][mode] = dict(baseline_mean_ir=base_ir, candidate_mean_ir=cand_ir,
                                         change_percent=(cand_ir / base_ir - 1) * 100,
                                         baseline_gc=[row[2] for row in baseline[mode]],
                                         candidate_gc=[row[2] for row in candidate[mode]],
                                         pass_original_cap=passed)
    (OUT / "result.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"S4 Ir replay {'PASS' if ok else 'FAIL'} profiles={24 * len(linkages)} original-cap=1.0 total-includes-GC")
    return 0 if ok else 1


def main():
    # Keep the capture locale stable when progress-meter exports LC_ALL=C.
    os.environ["LC_ALL"] = "C.UTF-8"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("measure", "check", "check-baseline"))
    parser.add_argument("which", nargs="?", choices=("baseline", "candidate"))
    parser.add_argument("--linkage", choices=("dynamic", "static", "both"), default="both")
    args = parser.parse_args()
    linkages = ("dynamic", "static") if args.linkage == "both" else (args.linkage,)
    if args.mode == "measure":
        need(args.which, "measure requires baseline or candidate")
        measure(args.which, linkages)
    elif args.mode == "check-baseline":
        for linkage in linkages:
            replay("baseline", linkage)
        print(f"S4 Ir baseline replay PASS profiles={12 * len(linkages)}")
    else:
        return check(linkages)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        print(f"S4 Ir FAIL: {error}", file=sys.stderr)
        sys.exit(1)
