#!/usr/bin/env python3
"""S4.2 only: authenticated, GC-isolated in-process callfloor intervals.

Force full GC before every START/zero, then retain every instruction through
END, including automatic collections. Never change the independent S8 total.
The workload, n=300, subtraction and four-layout arithmetic mean are unchanged.
"""
from concurrent.futures import ThreadPoolExecutor
import gzip
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
OUT = ROOT / "target/wh-s4f/frames"
PROBE = HERE / "word-heap-frames-ir-probe.el"
CLIENT = HERE / "word-heap-frames-ir-client.c"
PROCEDURE = "s4.2-in-process-forced-gc-v1"
LAYOUTS = (0, 700, 1400, 2100)
MODES = ("interpreter", "vm", "bytecode")
SPEC = importlib.util.spec_from_file_location("whf_floor_workload", HERE / "prim-callfloor-perf.py")
P = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(P)
OPS = tuple(P.OPS)


def need(value, message):
    if not value:
        raise ValueError(message)


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def read_raw(row):
    path = Path(row["path"])
    need(pin(path) == row, "changed raw profile")
    return gzip.decompress(path.read_bytes()).decode() if path.suffix == ".gz" else path.read_text()


def calls(raw, symbol):
    pattern = (r"^cfn=" + re.escape(symbol) + r"(?:'[0-9]+)?\n"
               r"(?:(?!c?fn=|calls=)[^\n]*\n){0,5}?calls=([1-9][0-9]*) [^\n]*\n[^\n]* [1-9][0-9]*\n")
    return sum(int(n) for n in re.findall(pattern, raw, re.M))


def boundary(raw, label, symbols, part, pid=None, invocation=None):
    need(f"Client Request: {label}\n" in raw, "missing " + label + " bracket")
    need(re.findall(r"^part: (\d+)$", raw, re.M) == [str(part)], "wrong bracket ordering")
    pids = re.findall(r"^pid: (\d+)$", raw, re.M)
    need(len(pids) == 1 and (pid is None or pids == [str(pid)]), "wrong bracket process")
    need(bool(re.search(r"^cmd: .+$", raw, re.M)), "missing bracket command")
    if invocation is not None:
        need([s.strip() for s in re.findall(r"^cmd: (.+)$", raw, re.M)] == [" ".join(invocation)],
             "wrong bracket command")
    if label == "PCF-START":
        need(calls(raw, symbols["entry"]) == 1 and calls(raw, symbols["body"]) > 0,
             "missing executed preparation GC")
    summaries = re.findall(r"^summary: (\d+)$", raw, re.M)
    need(len(summaries) == 1 and int(summaries[0]) > 0, "invalid instruction count")
    return int(summaries[0]), int(pids[0])


def gc_symbols(binary, gnu):
    if gnu:
        return {"entry": "Fgarbage_collect", "body": "garbage_collect"}
    selected = {"bf_garbage_collect_w", "nl_gc_collect_recorded_mark_sweep_body"}
    symbols = {}
    for line in subprocess.check_output(["nm", "-n", binary], text=True).splitlines():
        fields = line.split()
        if len(fields) == 3 and fields[2] in selected:
            symbols[fields[2]] = f"0x{int(fields[0], 16):016x}"
    return {"entry": symbols["bf_garbage_collect_w"],
            "body": symbols["nl_gc_collect_recorded_mark_sweep_body"]}


def command(binary, gnu):
    return [binary, *(["-Q", "--batch"] if gnu else []), "--load", str(PROBE)]


def interval_source(n):
    return ";;; -*- lexical-binding: t -*-\n" + "\n".join(
        f"(garbage-collect)\n(setq pcf-ir-before (float-time))\n(pcf-ir-request 0)\n"
        f"(pcf-{op} {0 if op == 'control' else n})\n(pcf-ir-request 1)\n"
        f'(princ (format "PCF-IR {op} %.9f\\n" (- (float-time) pcf-ir-before)))'
        for op in OPS) + "\n"


def binary_companions(binary, gnu):
    return [] if binary == gnu else [pin(binary + suffix) for suffix in (".cold", ".native-startup.el")]


def authenticate_binary(report):
    need(report["qualified_binary"] == report["binary"], "different qualification binary")
    need(report["binary_companions"] == binary_companions(report["binary"]["path"], report["gnu"]["path"]),
         "incomplete binary companion set")


def validate_program(dest, gnu):
    """Regenerate from the pinned workload generator, including actual bytecode."""
    with tempfile.TemporaryDirectory(dir=ROOT / "target/tmp", prefix="frame-ir-program-") as temp:
        source, compiled, driver = P.prepare(Path(temp), gnu)
        need(source.read_bytes() == (dest / "functions.el").read_bytes(), "changed canonical workload source")
        need(compiled.read_bytes() == (dest / "bytecode.el").read_bytes(), "changed canonical workload bytecode")
        text = driver.read_text()
        setup = text[:text.index("\n(let ((n 0))")] + "\n"
        setup = setup.replace(str(source), str(dest / "functions.el")).replace(str(compiled), str(dest / "bytecode.el"))
        need(setup == (dest / "setup.el").read_text(), "changed canonical workload setup")


def authenticate_paths(run, stem):
    for key, suffix in (("stdout", ".out"), ("stderr", ".err")):
        need(run[key] == pin(str(stem) + suffix), "wrong process output path")
    need(run["profiles"] == [pin(str(stem) + f".cg.{part}.gz") for part in range(1, 24)],
         "wrong numbered bracket profile paths")


def measure(binary, tag, gnu, n=300, jobs=4, emacs_include=None):
    need(n == 300, "S4.2 requires the unchanged n=300")
    dest = OUT / tag
    need(not (dest / "perf.json").exists(), "use a fresh performance tag")
    dest.mkdir(parents=True, exist_ok=True)
    source, compiled, driver = P.prepare(dest, gnu)
    text = driver.read_text()
    setup = dest / "setup.el"
    setup.write_text(text[:text.index("\n(let ((n 0))")] + "\n")
    intervals = dest / "intervals.el"
    intervals.write_text("(defvar pcf-ir-before nil)\n" + interval_source(n))
    binary, gnu = str(Path(binary).resolve()), str(Path(gnu).resolve())
    is_gnu = binary == gnu
    inputs = [pin(path) for path in (__file__, PROBE, CLIENT, P.__file__, P.C.__file__, source, compiled, setup, intervals)]
    companions = binary_companions(binary, gnu)
    client = dest / "client.so"
    client_build = None
    if is_gnu:
        include = emacs_include or os.getenv("PCF_EMACS_INCLUDE")
        header = (Path(include).expanduser().resolve() if include else Path(gnu).parent.parent / "include") / "emacs-module.h"
        need(header.is_file(), "supply --emacs-include or PCF_EMACS_INCLUDE containing emacs-module.h")
        build_command = ["cc", "-shared", "-fPIC", "-O2", "-Wall", "-Wextra", "-Werror",
                         "-I", str(header.parent), "-o", str(client), str(CLIENT)]
        subprocess.run(build_command, check=True)
        client_build = {"header": pin(header), "command": build_command}
        inputs.extend((pin(client), pin(header)))
    symbols = gc_symbols(binary, is_gnu)
    report = {"procedure": PROCEDURE, "isolated": True, "n": n,
              "binary": pin(binary), "qualified_binary": pin(binary), "gnu": pin(gnu),
              "binary_companions": companions, "inputs": inputs, "gc_symbols": symbols, "client_build": client_build,
              "runs": []}

    def trial(pair):
        mode, layout = pair
        stem = dest / f"{mode}-{layout}"
        env = dict(os.environ, PCF_MODE=mode, ABI_LAYOUT_PAD="x" * layout,
                   PCF_SETUP=str(setup), PCF_INTERVALS=str(intervals),
                   TMPDIR=str(ROOT / "target/tmp"), XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
        for name in ("NELISP_FX_DISABLE", "PCF_PROFILE", "PCF_CLIENT"):
            env.pop(name, None)
        if mode == "interpreter":
            env["NELISP_FX_DISABLE"] = "1"
        if is_gnu:
            env["PCF_CLIENT"] = str(client)
        cmd = ["setarch", os.uname().machine, "-R", "valgrind", "--tool=callgrind", "--error-exitcode=97",
               "--dump-instr=no", "--collect-jumps=no", "--compress-strings=no",
               f"--callgrind-out-file={stem}.cg", *command(binary, is_gnu)]
        started = time.monotonic()
        stdout, stderr = Path(str(stem) + ".out"), Path(str(stem) + ".err")
        with stdout.open("wb") as out, stderr.open("wb") as err:
            result = subprocess.run(cmd, cwd=ROOT, env=env, stdout=out, stderr=err, timeout=600)
        need(result.returncode == 0, f"profile process failed: {stem}")
        need(stdout.read_text().count("PCF-IR-DONE 11\n") == 1, "incomplete profile process")
        need("nelisp: uncaught error:" not in stderr.read_text(), "uncaught profile error")
        profiles = []
        for part in range(1, 2 * len(OPS) + 2):
            path = Path(str(stem) + f".cg.{part}")
            compressed = Path(str(path) + ".gz")
            compressed.write_bytes(gzip.compress(path.read_bytes(), mtime=0))
            path.unlink()
            profiles.append(pin(compressed))
        # The termination profile is outside all labeled intervals.
        final = Path(str(stem) + ".cg")
        if final.exists():
            final.unlink()
        row = {"mode": mode, "layout": layout, "command": cmd, "returncode": result.returncode,
               "environment": {key: env[key] for key in ("PCF_MODE", "ABI_LAYOUT_PAD", "PCF_SETUP", "PCF_INTERVALS", "NELISP_FX_DISABLE", "PCF_CLIENT") if key in env},
               "stdout": pin(stdout), "stderr": pin(stderr), "profiles": profiles,
               "elapsed_s": time.monotonic() - started}
        print(f"{tag} {mode} layout={layout} captured {len(profiles)} brackets", flush=True)
        return row

    with ThreadPoolExecutor(max_workers=jobs) as pool:
        report["runs"] = list(pool.map(trial, ((m, layout) for m in MODES for layout in LAYOUTS)))
    (dest / "perf.json").write_text(json.dumps(report, indent=2) + "\n")
    perf(tag)


def costs(rows, n):
    need(len(rows) == len(OPS) and [r["op"] for r in rows] == list(OPS), "incomplete operation coverage")
    control, empty = rows[0], next(r for r in rows if r["op"] == "while")
    result = {}
    for row in rows[1:]:
        base = control if row["op"] in ("while", "dotimes") else empty
        value = (row["ir"] - base["ir"]) / n
        need(math.isfinite(value), "nonfinite performance differential")
        if row["op"] in ("let", "while", "funcall", "builtin"):
            need(value > 0, "nonpositive acceptance measurement")
        result[row["op"]] = {"ir_per_op": value, "us_per_op": 1e6 * (row["seconds"] - base["seconds"]) / n}
    return result


def perf(tag):
    path = OUT / tag / "perf.json"
    report = json.loads(path.read_text())
    need(report.get("procedure") == PROCEDURE, "obsolete S4.2 measurement procedure")
    need(report["n"] == 300 and type(report["n"]) is int, "incorrect S4.2 iteration count")
    evidence = [pin(path), report["binary"], report["gnu"], *report["inputs"], *report["binary_companions"]]
    for item in evidence:
        need(pin(item["path"]) == item, "changed performance input")
    required = [__file__, PROBE, CLIENT, P.__file__, P.C.__file__,
                OUT / tag / "functions.el", OUT / tag / "bytecode.el",
                OUT / tag / "setup.el", OUT / tag / "intervals.el"]
    if report["binary"]["path"] == report["gnu"]["path"]:
        build = report["client_build"]
        header = build["header"]
        required.extend((OUT / tag / "client.so", header["path"]))
        need(build["command"] == ["cc", "-shared", "-fPIC", "-O2", "-Wall", "-Wextra", "-Werror",
                                  "-I", str(Path(header["path"]).parent), "-o", str(OUT / tag / "client.so"), str(CLIENT)],
             "wrong module build command")
    else:
        need(report["client_build"] is None, "unexpected module build")
    need(report["inputs"] == [pin(p) for p in required], "incomplete measurement source manifest")
    need((OUT / tag / "intervals.el").read_text() == "(defvar pcf-ir-before nil)\n" + interval_source(300),
         "changed measured interval procedure")
    binary, gnu = report["binary"]["path"], report["gnu"]["path"]
    authenticate_binary(report)
    validate_program(OUT / tag, gnu)
    need(report["gc_symbols"] == gc_symbols(binary, binary == gnu), "wrong GC symbols")
    expected = {(mode, layout) for mode in MODES for layout in LAYOUTS}
    need(len(report["runs"]) == 12 and {(r["mode"], r["layout"]) for r in report["runs"]} == expected,
         "incomplete four-layout performance coverage")
    means = {mode: {} for mode in MODES}
    pids = set()
    for run in report["runs"]:
        mode, layout = run["mode"], run["layout"]
        stem = OUT / tag / f"{mode}-{layout}"
        authenticate_paths(run, stem)
        expected_command = ["setarch", os.uname().machine, "-R", "valgrind", "--tool=callgrind", "--error-exitcode=97",
                            "--dump-instr=no", "--collect-jumps=no", "--compress-strings=no",
                            f"--callgrind-out-file={stem}.cg", *command(binary, binary == gnu)]
        need(run["command"] == expected_command, "wrong profile command")
        env = {"PCF_MODE": mode, "ABI_LAYOUT_PAD": "x" * layout,
               "PCF_SETUP": str(OUT / tag / "setup.el"), "PCF_INTERVALS": str(OUT / tag / "intervals.el")}
        if mode == "interpreter":
            env["NELISP_FX_DISABLE"] = "1"
        if binary == gnu:
            env["PCF_CLIENT"] = str(OUT / tag / "client.so")
        need(run["environment"] == env, "wrong profile environment")
        for item in (run["stdout"], run["stderr"]):
            need(pin(item["path"]) == item, "changed process output")
            evidence.append(item)
        text = Path(run["stdout"]["path"]).read_text()
        need(run["returncode"] == 0 and text.count("PCF-IR-DONE 11\n") == 1, "incomplete profile process")
        need("nelisp: uncaught error:" not in Path(run["stderr"]["path"]).read_text(), "uncaught profile error")
        if mode == "vm" and binary != gnu:
            for name in ("pcf-empty-body", "pcf-one-body", "pcf-funcall", "pcf-builtin", "pcf-let", "pcf-while"):
                need(f"PCF-COVERAGE {name} t" in text, "VM fallback in acceptance operation")
        clocks = re.findall(r"^PCF-IR ([a-z-]+) ([0-9.]+)$", text, re.M)
        need([op for op, _ in clocks] == list(OPS), "incomplete measured operation output")
        profiles = run["profiles"]
        need(len(profiles) == 23, "missing/extra bracket profile")
        invocation = command(binary, binary == gnu)
        _, pid = boundary(read_raw(profiles[0]), "PCF-BOOT", report["gc_symbols"], 1, invocation=invocation)
        need(pid not in pids, "reused layout/mode process")
        pids.add(pid)
        rows = []
        for i, (op, seconds) in enumerate(clocks):
            prep, end = profiles[2 * i + 1:2 * i + 3]
            boundary(read_raw(prep), "PCF-START", report["gc_symbols"], 2 * i + 2, pid, invocation)
            ir, _ = boundary(read_raw(end), "PCF-END", report["gc_symbols"], 2 * i + 3, pid, invocation)
            rows.append({"op": op, "ir": ir, "seconds": float(seconds)})
        evidence.extend(profiles)
        for op, row in costs(rows, report["n"]).items():
            means[mode].setdefault(op, []).append(row)
    means = {mode: {op: {key: sum(r[key] for r in rows) / 4 for key in ("ir_per_op", "us_per_op")}
                    for op, rows in ops.items()} for mode, ops in means.items()}
    return report, means, evidence
