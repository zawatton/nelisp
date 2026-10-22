#!/usr/bin/env python3
"""Wall-clock call floors against GNU 31.1 (COMMON.md "Measurement", 2026-10-21).

  callpath-perf-wall.py floors TAG CANDIDATE BASE [--gnu EMACS]
  callpath-perf-wall.py procs TAG CANDIDATE BASE [--gnu EMACS]

floors: the Ir campaigns' pinned study and floors fixtures (the gnu-capped
setup.el, so all engines load the same functions.el / bytecode.el), in the
interpreter, VM and bytecode modes.  Each process warms every operation,
collects garbage, then times each loop of N iterations; collections inside
the loops count.  An operation's own cost is (loop - empty while loop) / N in
the same process, the definition the Ir criteria used.  Seven alternating
rounds (candidate, GNU, base; reversed on odd rounds), pinned with taskset,
under the 8 GiB cap; the median per engine/mode/operation is the result.
GNU has no FX VM: its "vm" row repeats the interpreter, as in Doc 219.

procs: whole-process wall time of the S2.4 boot/files/regex workloads
(tools/ai/abi-ir.py), the same seven alternating rounds, NeLisp only plus a
GNU reference where the workload runs there.

Results: target/callpath-perf/wall/TAG/{floors,procs}.json with every run.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf/wall"
ROUNDS = 7
CPUS = os.environ.get("CALLPATH_WALL_CPUS", "12")
MODES = ("interpreter", "vm", "bytecode")
SUITES = {"study": ("pcf-while", "pcf-funcall"),
          "floors": ("pcf-while", "pcf-builtin", "pcf-let")}
FIXTURES = {"study": ("0bb2af9272cb4c3e4a297d3b36c5aff5b90e2bbd27a6de53e3309ee814826370",
                      "135e8597a877451058dc4e6f858c33ad665c6a2ce3edbb57bd42868dac11c225"),
            "floors": ("590f1429b281105f6127f8dfd8107105a6dd8e9cc1447e873347bfe06513fe4d",
                       "1f90701eadbcf9e59d17953e5f6b96503155d56b58b50540a36355ae39eb5a7d")}
# GNU own costs are a few ns (bytecode) to tens of ns; long loops keep the
# GNU denominator well above timer and clock noise.
ITERATIONS = {"interpreter": 300000, "vm": 1000000, "bytecode": 5000000}


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": sha(path)}


def fixture(suite):
    directory = ROOT / "target/callpath-perf" / (suite + "-queued") / "gnu-capped"
    identity = (sha(directory / "functions.el"), sha(directory / "bytecode.el"))
    if identity != FIXTURES[suite]:
        raise ValueError(f"{suite} fixture differs from the pinned Ir corpus")
    return directory / "setup.el", [pin(directory / n) for n in ("setup.el", "functions.el", "bytecode.el")]


def timing_form(ops, n):
    names = " ".join(ops)
    return ("(let ((n %d) (ops '(%s)) (res nil)) "
            "(dolist (op ops) (funcall op (min n 2000))) "
            "(dolist (op ops) (garbage-collect) "
            "(let ((t0 (float-time))) (funcall op n) "
            "(setq res (cons (format \"%%s=%%.9f\" op (- (float-time) t0)) res)))) "
            "(princ (format \"PCF-WALL %%s\\n\" (mapconcat (function identity) (nreverse res) \" \"))))"
            % (n, names))


def cpu_khz():
    """The measurement CPU's current clock (diagnostic: boost varies with load)."""
    first = CPUS.split(",")[0].split("-")[0]
    try:
        return int(Path(f"/sys/devices/system/cpu/cpu{first}/cpufreq/scaling_cur_freq").read_text())
    except (OSError, ValueError):
        return None


def run(argv, env):
    cmd = ["bash", "-c", 'ulimit -v 8388608; exec taskset -c "$0" setarch "$(uname -m)" -R "$@"', CPUS] + argv
    start = time.perf_counter()
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True, timeout=900)
    proc.khz = cpu_khz()
    return proc, time.perf_counter() - start


def mode_env(mode, engine):
    env = dict(os.environ, PCF_MODE=mode, LC_ALL="C", TMPDIR=str(ROOT / "target/tmp"),
               XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
    env.pop("NELISP_FX_DISABLE", None)
    if mode == "interpreter" and engine != "gnu":
        env["NELISP_FX_DISABLE"] = "1"
    return env


def source_identity():
    spec = __import__("importlib.util").util.spec_from_file_location(
        "cpp_measure", ROOT / "tools/ai/callpath-perf-measure.py")
    module = __import__("importlib.util").util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.source_identity()


def floors(args):
    dest = OUT / args.tag
    dest.mkdir(parents=True, exist_ok=True)
    engines = {"candidate": Path(args.candidate).resolve(), "base": Path(args.base).resolve(),
               "gnu": Path(args.gnu).resolve()}
    report = {"schema": 1, "protocol": "wall-7-alternating-v1", "cpus": CPUS, "rounds": ROUNDS,
              "source": source_identity(),
              "engines": {k: pin(v) for k, v in engines.items()}, "iterations": ITERATIONS,
              "producer": pin(__file__), "fixtures": {}, "runs": []}
    for suite, ops in SUITES.items():
        setup, pins = fixture(suite)
        report["fixtures"][suite] = pins
        for mode in MODES:
            n = ITERATIONS[mode]
            for r in range(ROUNDS):
                order = ["candidate", "gnu", "base"] if r % 2 == 0 else ["base", "gnu", "candidate"]
                for engine in order:
                    argv = [str(engines[engine])] + (["-Q", "--batch"] if engine == "gnu" else []) + \
                           ["--load", str(setup), "--eval", timing_form(ops, n)]
                    proc, elapsed = run(argv, mode_env(mode, engine))
                    match = re.search(rb"PCF-WALL (.*)\n", proc.stdout)
                    if proc.returncode != 0 or not match:
                        raise RuntimeError(f"{engine} {suite} {mode} round {r}: rc={proc.returncode} "
                                           f"{proc.stderr[-300:]!r}")
                    seconds = {k: float(v) for k, v in (kv.split("=") for kv in match.group(1).decode().split())}
                    own = {op: (seconds[op] - seconds["pcf-while"]) / n for op in ops if op != "pcf-while"}
                    own["pcf-while"] = seconds["pcf-while"] / n
                    report["runs"].append({"suite": suite, "mode": mode, "round": r, "engine": engine,
                                           "n": n, "seconds": seconds, "own_s": own,
                                           "process_s": elapsed, "cpu_khz_after": proc.khz})
                    (dest / "floors.json").write_text(json.dumps(report, indent=1) + "\n")
                    print(f"{suite}/{mode} r{r} {engine} " +
                          " ".join(f"{k}={v * 1e6:.4f}us" for k, v in own.items()), flush=True)
    report["medians_us"] = medians(report["runs"])
    report["paired_ratios"] = paired_ratios(report["runs"])
    (dest / "floors.json").write_text(json.dumps(report, indent=1) + "\n")
    summarize(report["medians_us"], report["paired_ratios"])


def medians(runs):
    table = {}
    for row in runs:
        for op, value in row["own_s"].items():
            table.setdefault(row["engine"], {}).setdefault(row["mode"], {}).setdefault(op, []).append(value)
    return {e: {m: {op: statistics.median(v) * 1e6 for op, v in ops.items()} for m, ops in modes.items()}
            for e, modes in table.items()}


def paired_ratios(runs):
    """COMMON.md Measurement (2026-10-21): candidate/GNU and candidate/base
    inside each alternating round, then the median over rounds."""
    own = {}
    for row in runs:
        for op, value in row["own_s"].items():
            own.setdefault((row["mode"], op, row["round"]), {})[row["engine"]] = value
    ratios = {}
    for (mode, op, _), engines in sorted(own.items()):
        if {"candidate", "gnu", "base"} <= set(engines):
            c, g, b = engines["candidate"], engines["gnu"], engines["base"]
            slot = ratios.setdefault(mode, {}).setdefault(op, {"gnu": [], "base": []})
            slot["gnu"].append(c / g if g > 0 else float("inf"))
            slot["base"].append(c / b if b > 0 else float("inf"))
    return {mode: {op: {"cand_over_gnu": statistics.median(v["gnu"]),
                        "cand_over_base": statistics.median(v["base"]),
                        "rounds": len(v["gnu"])} for op, v in ops.items()}
            for mode, ops in ratios.items()}


def summarize(med, ratios):
    for mode in MODES:
        for op in ("pcf-funcall", "pcf-builtin", "pcf-let", "pcf-while"):
            row = [med.get(e, {}).get(mode, {}).get(op) for e in ("candidate", "base", "gnu")]
            pr = ratios.get(mode, {}).get(op)
            if None in row or pr is None:
                continue
            c, b, g = row
            print(f"{mode:11s} {op:12s} cand={c:9.4f}us base={b:9.4f}us gnu={g:8.4f}us "
                  f"paired cand/gnu={pr['cand_over_gnu']:7.2f} cand/base={pr['cand_over_base']:5.3f}")


def procs(args):
    dest = OUT / args.tag
    dest.mkdir(parents=True, exist_ok=True)
    regex = ROOT / "target/callpath-perf/ir/base00-dynamic/regex.el"
    workloads = {"boot": ["--eval", "nil"],
                 "files": ["--load", str(ROOT / "tools/ai/abi-file-exec.el")],
                 "regex": ["--load", str(regex)]}
    engines = {"candidate": Path(args.candidate).resolve(), "base": Path(args.base).resolve()}
    report = {"schema": 1, "protocol": "wall-7-alternating-procs-v1", "cpus": CPUS, "rounds": ROUNDS,
              "source": source_identity(),
              "engines": {k: pin(v) for k, v in engines.items()}, "producer": pin(__file__),
              "inputs": [pin(regex), pin(ROOT / "tools/ai/abi-file-exec.el")], "runs": []}
    for mode, extra in workloads.items():
        for r in range(ROUNDS):
            order = ["candidate", "base"] if r % 2 == 0 else ["base", "candidate"]
            for engine in order:
                # As tools/ai/abi-ir.py runs them: an empty cache per process and
                # the source root the file workload resolves its inputs from.
                cache = dest / f"cache-{engine}-{mode}-{r}"
                shutil.rmtree(cache, ignore_errors=True)
                cache.mkdir(parents=True)
                env = dict(os.environ, LC_ALL="C", TMPDIR=str(ROOT / "target/tmp"),
                           XDG_CACHE_HOME=str(cache), ABI_SOURCE_ROOT=str(ROOT), ABI_LAYOUT_PAD="")
                proc, elapsed = run([str(engines[engine])] + extra, env)
                if proc.returncode != 0:
                    raise RuntimeError(f"{engine} {mode} round {r}: rc={proc.returncode}")
                report["runs"].append({"mode": mode, "round": r, "engine": engine, "seconds": elapsed,
                                       "cpu_khz_after": proc.khz,
                                       "stdout_sha256": hashlib.sha256(proc.stdout).hexdigest()})
                (dest / "procs.json").write_text(json.dumps(report, indent=1) + "\n")
                print(f"{mode} r{r} {engine} {elapsed:.3f}s", flush=True)
    med = {}
    for row in report["runs"]:
        med.setdefault(row["engine"], {}).setdefault(row["mode"], []).append(row["seconds"])
    report["medians_s"] = {e: {m: statistics.median(v) for m, v in modes.items()} for e, modes in med.items()}
    report["paired_cand_over_base"] = procs_paired(report["runs"])
    (dest / "procs.json").write_text(json.dumps(report, indent=1) + "\n")
    for mode in workloads:
        c, b = report["medians_s"]["candidate"][mode], report["medians_s"]["base"][mode]
        print(f"{mode:6s} cand={c:.3f}s base={b:.3f}s paired cand/base={report['paired_cand_over_base'][mode]:.3f}")


def procs_paired(runs):
    """Median over rounds of the per-round candidate/base process-time ratio."""
    rounds = {}
    for row in runs:
        rounds.setdefault((row["mode"], row["round"]), {})[row["engine"]] = row["seconds"]
    ratios = {}
    for (mode, _), engines in rounds.items():
        if {"candidate", "base"} <= set(engines):
            ratios.setdefault(mode, []).append(engines["candidate"] / engines["base"])
    return {mode: statistics.median(v) for mode, v in ratios.items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="action", required=True)
    for name in ("floors", "procs"):
        p = sub.add_parser(name)
        p.add_argument("tag")
        p.add_argument("candidate")
        p.add_argument("base")
        p.add_argument("--gnu", default=shutil.which("emacs") or "/usr/local/bin/emacs")
    args = parser.parse_args()
    (floors if args.action == "floors" else procs)(args)


if __name__ == "__main__":
    main()
