#!/usr/bin/env python3
"""Compile four canonical N4 cache helpers and execute their native ABI.

The C host supplies only dependencies; cache setup/read/mark/rewrite are
extracted unchanged from the selected canonical source and compiled by NeLisp.
Run from a source checkout or supply --root. A defective source may be passed
with --control to prove that the same native fixture detects its corruption.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time

FUNCTIONS = ["nl_frame_stack_limit_setup", "nl_frame_stack_limit",
             "nl_n4_mark_env", "nl_n4_rewrite_env"]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--root", type=Path, default=next(p for p in Path(__file__).resolve().parents if (p / "AI.md").is_file()))
    ap.add_argument("--source", type=Path,
                    default=Path("scripts/nelisp-standalone-build.el"))
    controls = ap.add_mutually_exclusive_group()
    controls.add_argument("--control", type=Path)
    controls.add_argument("--control-regression", action="store_true",
                          help="compile the original colliding cache layout as a negative control")
    ap.add_argument("--out", type=Path,
                    default=Path("target/wh-s4f/n4-cache-probe"))
    ap.add_argument("--emacs", default="emacs")
    ap.add_argument("--cc", default="cc")
    args = ap.parse_args()
    root = args.root.resolve()
    out = (root / args.out).resolve()
    out.mkdir(parents=True, exist_ok=True)
    if args.control_regression:
        text = (root / args.source).read_text()
        forms = []
        for name in FUNCTIONS:
            start = text.index("    (defun " + name + " ")
            end = text.find("    (defun ", start + 12)
            body = text[start:len(text) if end < 0 else end]
            forms.append(re.sub(r"(\bstate )(32|40|48)(?=[\s)])",
                                lambda m: m[1] + {"32": "16", "40": "24", "48": "32"}[m[2]], body))
        args.control = out / "colliding-layout.el"
        args.control.write_text("\n".join(forms))
    companion = Path(__file__).resolve().parent
    builder = companion / "n4-cache-probe-build.el"
    host = companion / "n4-cache-probe-host.c"
    report = {"schema": 1, "functions": FUNCTIONS,
              "fixture": {str(f): sha(f) for f in (Path(__file__).resolve(), builder, host)},
              "runs": []}
    for label, selected in (("candidate", args.source), ("control", args.control)):
        if selected is None:
            continue
        source = (root / selected).resolve()
        obj = out / (label + ".o")
        binary = out / (label + "-probe")
        env = dict(os.environ, PROBE_SOURCE=str(source), PROBE_OBJECT=str(obj))
        row = {"label": label, "source": str(source), "source_sha256": sha(source), "stages": []}
        commands = [
            [args.emacs, "-Q", "--batch", "-L", "lisp", "-L", "src", "-L", "scripts", "--load", str(builder)],
            [args.cc, "-no-pie", "-o", str(binary), str(host), str(obj)],
            [str(binary)],
        ]
        for stage, command in zip(("compile", "link", "execute"), commands):
            started = time.monotonic()
            run = subprocess.run(command, cwd=root, env=env, capture_output=True, timeout=45)
            elapsed = time.monotonic() - started
            stdout, stderr = out / (label + "-" + stage + ".out"), out / (label + "-" + stage + ".err")
            stdout.write_bytes(run.stdout)
            stderr.write_bytes(run.stderr)
            row["stages"].append({"stage": stage, "command": command, "seconds": elapsed,
                                  "returncode": run.returncode,
                                  "stdout": {"path": str(stdout), "sha256": sha(stdout)},
                                  "stderr": {"path": str(stderr), "sha256": sha(stderr)}})
            if stage != "execute" and run.returncode != 0:
                raise RuntimeError(label + " " + stage + " failed; inspect " + str(stderr))
        match = re.search(rb"N4-CACHE checks=(\d+) failures=(\d+)", run.stdout)
        if match is None:
            raise RuntimeError(label + " fixture did not complete")
        checks, failures = map(int, match.groups())
        expected = (run.returncode == 0 and checks == 10 and failures == 0) if label == "candidate" else (run.returncode == 1 and checks == 10 and failures > 0)
        row.update(checks=checks, failures=failures, detected_expected_result=expected,
                   object_sha256=sha(obj), binary_sha256=sha(binary),
                   seconds=sum(x["seconds"] for x in row["stages"]))
        report["runs"].append(row)
        (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
        print(f"N4-CACHE {label}: checks={checks} failures={failures} expected={expected} seconds={row['seconds']:.6f}")
        if not expected:
            return 1
    print("N4-CACHE report=" + str(out / "report.json"))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
