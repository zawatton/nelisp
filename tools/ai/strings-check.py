#!/usr/bin/env python3
"""Focused acceptance entry points for Doc 227; unfinished checks fail closed."""
from pathlib import Path
import argparse
import json
import re
import os
import subprocess
import time
import hashlib

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/strings"
CENSUS = {
    "transport": ["nl_word_clone", "wf_word_view", "nl_sexp_clone_into", "nl_g4arm_copy_w"],
    "alias": ["nl_sci_copy", "nl_sci_dispatch"],
    "allocation": ["nl_alloc_str_w", "nl_alloc_unibyte_str_w", "nelisp_sexp_write_str"],
    "copy": ["nl_alloc_str_copy_loop", "nl_mut_str_finalize_copy_loop", "nl_mut_str_finalize"],
    "read": ["nl_str_bytes_ptr", "nl_str_char_count"],
    "publication": ["m5_finish_string_w"],
    "retag": ["nelisp_reader_p_build_head_symbol"],
}


def plan():
    doc = (ROOT / "docs/design/227-gnu-string-port.org").read_text()
    rows = []
    for path in sorted((ROOT / "lisp").glob("*.el")) + [ROOT / "scripts/nelisp-standalone-build.el"]:
        source = path.read_text()
        for family, names in CENSUS.items():
            for name in names:
                for match in re.finditer(r"\(defun\s+" + re.escape(name) + r"(?=\s|\()", source):
                    rows.append(dict(family=family, name=name, path=str(path.relative_to(ROOT)),
                                     line=source.count("\n", 0, match.start()) + 1))
    assert len(rows) == 16, f"census changed: {len(rows)} definitions; classify before accepting"
    for token in ["alloc.c:1729", "character.h:", "fns.c:", "nl_sweep_strings",
                  "nl_gc_mark_reset_all", "nl_gc_conserv_typed", "nl_dump_compact_write"]:
        assert token in doc, f"missing plan mechanism: {token}"
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "census.json").write_text(json.dumps(rows, indent=2) + "\n")
    print("PASS plan: GNU mechanism map, 16 classified definitions, GC integration contract")


def kernel():
    env = os.environ.copy()
    env["TMPDIR"] = str(ROOT / "target/tmp")
    commands = [
        ["emacs", "-Q", "--batch", "-L", "lisp", "-L", "src", "-L", "scripts",
         "--eval", "(setq backtrace-on-error-noninteractive nil)",
         "-l", "tools/ai/strings-kernel.el"],
        ["gcc", "-O2", "-no-pie", "-pthread", "tools/ai/strings-kernel.c",
         "target/strings/kernel.o", "-o", "target/strings/kernel"],
        [str(OUT / "kernel")],
    ]
    start = time.monotonic()
    for i, command in enumerate(commands):
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=30)
        (OUT / f"kernel-step-{i}.out").write_bytes(result.stdout)
        (OUT / f"kernel-step-{i}.err").write_bytes(result.stderr)
        assert result.returncode == 0, f"kernel step {i} failed; see target/strings/kernel-step-{i}.err"
    assert result.stdout.startswith(b"PASS string kernel:"), "missing completion assertion"
    assert not result.stderr, "unexpected kernel stderr"
    (OUT / "kernel-result.json").write_text(json.dumps(dict(
        status="PASS", header_bytes=32, sdata_bytes=24, string8_bytes=56,
        wall_seconds=time.monotonic()-start)) + "\n")
    print(result.stdout.decode().strip())


def semantics():
    fixture = "tools/ai/strings-semantics.el"
    outputs = {}
    for kind, command in [("gnu", [os.environ.get("GNU_EMACS", "emacs"), "-Q", "--batch"]),
                          ("dynamic", [str(OUT / "dynamic/nelisp")]),
                          ("static", [str(OUT / "static/nelisp")])]:
        assert Path(command[0]).exists() or kind == "gnu", f"missing {kind} candidate"
        result = subprocess.run(command + ["--load", fixture], cwd=ROOT, capture_output=True, timeout=40)
        (OUT / f"{kind}-semantics.out").write_bytes(result.stdout)
        (OUT / f"{kind}-semantics.err").write_bytes(result.stderr)
        assert result.returncode == 0, f"{kind} fixture exit {result.returncode}"
        assert not result.stderr, f"{kind} unexpected stderr"
        outputs[kind] = [line for line in result.stdout.splitlines() if line.startswith(b"STRINGS-CASE ")]
        assert len(outputs[kind]) >= 10, f"{kind} incomplete fixture"
    assert outputs["dynamic"] == outputs["gnu"], "dynamic GNU fixture differs; see saved outputs"
    assert outputs["static"] == outputs["gnu"], "static GNU fixture differs; see saved outputs"
    print(f"PASS semantics: {len(outputs['gnu'])} GNU cases on dynamic and static")


def layout():
    result = {"status": "PASS", "header_bytes": 32, "sdata_bytes": 24}
    for role in ["base", "dynamic"]:
        binary = OUT / role / "nelisp"
        assert binary.exists(), f"missing {role} binary"
        rows = {}
        for mode in ["population", "aliases"]:
            values = []
            for n in [4000, 8000]:
                env = dict(os.environ, STRINGS_N=str(n), STRINGS_MODE=mode)
                r = subprocess.run([str(binary), "--load", "tools/ai/strings-layout.el"],
                                   cwd=ROOT, env=env, capture_output=True, timeout=60)
                stem = OUT / role / f"layout-{mode}-{n}"
                stem.with_suffix(".out").write_bytes(r.stdout)
                stem.with_suffix(".err").write_bytes(r.stderr)
                assert r.returncode == 0 and not r.stderr, f"{role}/{mode}/{n} failed"
                found = re.findall(rb"^STRINGS-LAYOUT (\w+) (\d+) (-?\d+) (-?\d+) (-?\d+) (nil|t)$", r.stdout, re.M)
                assert len(found) == 1, "missing layout measurement"
                _, count, headers, data, arena, same = found[0]
                assert int(count) == n and (same == b"t") == (mode == "aliases")
                values.append([int(headers), int(data), int(arena)])
            rows[mode] = [(b-a)/4000 for a,b in zip(*values)]
        result[role] = {"sha256": hashlib.sha256(binary.read_bytes()).hexdigest(),
                        "slopes": rows}
    current = result["dynamic"]["slopes"]
    assert current["population"][:2] == [1, 8], "canonical header/data accounting differs"
    assert current["aliases"][:2] == [0, 0], "alias created string headers/data"
    assert current["population"][2] <= 0 and current["aliases"][2] <= 0, "retained arena string views remain"
    kernel_result = json.loads((OUT / "kernel-result.json").read_text())
    assert kernel_result["status"] == "PASS" and kernel_result["string8_bytes"] == 56
    result["before_bytes"] = result["base"]["slopes"]["population"][2]
    result["after_bytes"] = 56
    (OUT / "layout.json").write_text(json.dumps(result, indent=2)+"\n")
    print(f"PASS layout: {result['before_bytes']:g} -> 56 B/string; retained views/aliases 0")


def speed(workload):
    assert workload, "choose producers or pressure"
    report = json.loads((OUT / "timing" / f"{workload}.json").read_text())
    assert report["cpu"] == 14 and report["rounds"] == 7
    for role, path in [("candidate", OUT / "dynamic/nelisp"), ("base", OUT / "base/nelisp")]:
        assert report["identities"][role] == hashlib.sha256(path.read_bytes()).hexdigest(), "stale timing binary"
    for kind, row in report["verdicts"].items():
        assert len(row["paired_gnu_ratios"]) == len(row["paired_base_ratios"]) == 7
        assert row["passed"] and (row["base_ratio"] < 1 if workload == "pressure" else row["gnu_ratio"] <= 1), f"speed target missed: {kind} {row}"
        print(f"PASS {kind}: GNU {row['gnu_ratio']:.4f}x; base {row['base_ratio']:.4f}x")


def final():
    report = json.loads((OUT / "final.json").read_text())
    assert report["groups"] == 11 and report["failures"] == 0
    assert report["standalone_checks"] and report["mcp_tools"] == 40 and report["wine_smoke"]
    for role in ["dynamic", "static"]:
        assert report["identities"][role] == hashlib.sha256((OUT / role / "nelisp").read_bytes()).hexdigest(), "stale final binary"
    print("PASS final: 11 groups, standalone checks, 40 MCP tools, Wine smoke")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("check", choices=["plan", "layout", "compaction", "semantics", "speed", "final"])
    parser.add_argument("workload", nargs="?", choices=["producers", "pressure"])
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    if args.check == "plan":
        plan()
    elif args.check == "compaction":
        kernel()
    elif args.check == "semantics":
        semantics()
    elif args.check == "layout":
        layout()
    elif args.check == "speed":
        speed(args.workload)
    elif args.check == "final":
        final()


if __name__ == "__main__":
    main()
