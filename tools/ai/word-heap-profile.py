#!/usr/bin/env python3
"""Retain instruction-level Doc 216 profiles, with isolated caches and provenance.

Usage: word-heap-profile.py OUTPUT CANDIDATE BASELINE [OLD_BUILD ...]
The fixed workloads are extracted from abi-ir.py rather than copied. Runs
resume only when binary, cold image, workload, environment and profile match.
"""
import ast
import concurrent.futures
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("abi_ir", ROOT / "tools/ai/abi-ir.py")
ABI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ABI)


def digest(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def main():
    output = Path(sys.argv[1]).resolve()
    output.mkdir(parents=True, exist_ok=True)
    source = ROOT / "tools/ai/abi-ir.py"
    for node in ast.walk(ast.parse(source.read_text())):
        if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                and node.func.attr == "write_text" and node.args
                and isinstance(node.args[0], ast.Constant)
                and isinstance(node.args[0].value, str)
                and "ABI-REGEX" in node.args[0].value):
            name = "regex-gc.el" if "ABI-REGEX-GC" in node.args[0].value else "regex.el"
            (output / name).write_text(node.args[0].value)
    binaries = dict(zip(("cand", "base", "hotfix", "before"),
                        map(lambda p: Path(p).resolve(), sys.argv[2:])))
    if os.environ.get("WH_ONLY_BASE") == "1":
        binaries = {"base": binaries["base"]}
    # S0.3 needs the three Ir workloads; GC-count diagnostics have a native
    # counter path and can optionally retain full profiles separately.
    modes = ("boot", "files", "regex")
    if os.environ.get("WH_PROFILE_GC") == "1":
        modes += ("regex-gc",)
    if os.environ.get("WH_MODES"):
        selected = tuple(os.environ["WH_MODES"].split(","))
        if not selected or not set(selected) <= set(modes):
            raise ValueError("WH_MODES must select boot,files,regex (or enabled regex-gc)")
        modes = selected

    def run(who, binary, mode, layout):
        stem = output / f"{mode}-{who}-{layout}"
        workload = ROOT / "tools/ai/abi-file-exec.el" if mode == "files" else output / f"{mode}.el"
        args = ["--eval", "nil"] if mode == "boot" else ["--load", str(workload)]
        env = dict(os.environ, ABI_SOURCE_ROOT=str(ROOT), ABI_LAYOUT_PAD="x" * layout)
        cache = output / f"{mode}-{who}-{layout}-cache"
        cache.mkdir(exist_ok=True)
        env.update(XDG_CACHE_HOME=str(cache), TMPDIR=str(output))
        identity = {"binary": digest(binary), "cold": digest(str(binary) + ".cold"),
                    "workload": None if mode == "boot" else digest(workload),
                    "environment": hashlib.sha256(json.dumps(env, sort_keys=True).encode()).hexdigest(),
                    "harness": digest(__file__)}
        record_path = stem.with_suffix(".json")
        if record_path.exists():
            record = json.loads(record_path.read_text())
            if record["identity"] == identity and record["profile_sha256"] == digest(stem.with_suffix(".cg")):
                return record
            raise ValueError(f"stale profile: {stem}")
        started = time.monotonic()
        with stem.with_suffix(".out").open("w") as stdout, stem.with_suffix(".err").open("w") as stderr:
            result = subprocess.run(["valgrind", "--tool=callgrind", "--dump-instr=yes",
                                     "--error-exitcode=97", f"--callgrind-out-file={stem}.cg",
                                     str(binary), *args], cwd=ROOT, env=env,
                                    stdout=stdout, stderr=stderr, timeout=2400)
        if result.returncode:
            raise RuntimeError(f"{stem} exit={result.returncode}")
        ABI.check_marker(stem, mode)
        ir, gc = ABI.profile_counts(stem.with_suffix(".cg"), binary)
        record = dict(identity=identity, binary=str(binary), mode=mode, layout=layout,
                      who=who, ir=ir, gc=gc, seconds=time.monotonic() - started,
                      profile_sha256=digest(stem.with_suffix(".cg")),
                      stdout_sha256=digest(stem.with_suffix(".out")))
        record_path.write_text(json.dumps(record, indent=2) + "\n")
        print(f"{mode} {who} layout={layout} Ir={ir} GC={gc}", flush=True)
        return record

    jobs = [(who, binary, mode, layout) for who, binary in binaries.items()
            for mode in (modes
                         if who in ("cand", "base") else ("regex-gc",))
            for layout in ABI.LAYOUTS]
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        records = list(pool.map(lambda args: run(*args), jobs))
    (output / "profiles.json").write_text(json.dumps(records, indent=2) + "\n")


if __name__ == "__main__":
    main()
