#!/usr/bin/env python3
"""Measure loop-body minus empty-loop Ir, including collection, in four layouts.

While/dotimes are measured as N empty iterations minus zero iterations. Wall
time uses separate processes so clock formatting cannot contaminate Ir.
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

SPEC = importlib.util.spec_from_file_location("pcf", Path(__file__).with_name("prim-callfloor-check.py"))
C = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(C)
ROOT, OUT = C.ROOT, C.OUT
OPS = {"control": "nil", "form": "17", "funcall": "(funcall pcf-empty)",
       "builtin": "(car pcf-pair)", "apply": "(apply pcf-one '(17))",
       "optional-rest": "(pcf-flex 1 2 3)",
       "let": "(let ((x pcf-pair)) x)",
       "let-special": "(let ((pcf-special 17)) pcf-special)",
       "let-star": "(let* ((x pcf-pair) (y x)) y)",
       "while": "nil", "dotimes": "nil"}


def prepare(dest, gnu):
    source = dest / "functions.el"
    source.write_text(";;; -*- lexical-binding: t -*-\n"
        "(defun pcf-empty-body () nil)\n(defvar pcf-empty nil)\n"
        "(defun pcf-one-body (x) x)\n(defvar pcf-one nil)\n"
        "(defvar pcf-pair '(17))\n(defvar pcf-special 7)\n"
        "(defun pcf-flex (a &optional b &rest r) r)\n" +
        "\n".join(f"(defun pcf-{op} (n) " +
                    ("(dotimes (_ n) nil))" if op == "dotimes" else
                     f"(let ((i 0)) (while (< i n) {body} (setq i (1+ i)))) )")
                    for op, body in OPS.items()) + "\n")
    compiled = dest / "bytecode.el"
    C.compile_forms(source, compiled, gnu)
    driver = dest / "driver.el"
    names = " ".join("pcf-" + op for op in OPS)
    driver.write_text(";;; -*- lexical-binding: t -*-\n" +
        f"(load (if (equal (getenv \"PCF_MODE\") \"bytecode\") "
        f"{json.dumps(str(compiled))} {json.dumps(str(source))}) nil t)\n" +
        "(when (and (equal (getenv \"PCF_MODE\") \"vm\") (fboundp 'nelisp--fx-register))\n"
        f" (dolist (name '({names} pcf-flex pcf-empty-body pcf-one-body)) (nelisp--fx-register name))\n"
        f" (dolist (name '({names} pcf-empty-body pcf-one-body))\n"
        "  (princ (format \"PCF-COVERAGE %s %S\\n\" name "
        "(vectorp (gethash (nthcdr 3 (symbol-function name)) nelisp--fx-table))))))\n"
        "(setq pcf-empty (symbol-function 'pcf-empty-body) pcf-one (symbol-function 'pcf-one-body))\n" +
        "\n".join(f"(pcf-{op} 1)" for op in OPS) + "\n(garbage-collect)\n" +
        "\n".join(f"(let ((n {('0' if op == 'control' else '(string-to-number (getenv \"PCF_N\"))')})) "
                    f"(if (getenv \"PCF_PROFILE\") (pcf-{op} n) "
                    f"(let ((t0 (float-time))) (pcf-{op} n) "
                    f"(princ (format \"PCF-TIME {op} %.9f\\n\" (- (float-time) t0))))))\n"
                    "(garbage-collect)" for op in OPS) +
        f"\n(princ \"PCF-COMPLETE {len(OPS)}\\n\")\n(kill-emacs 0)\n")
    return source, compiled, driver


def measure(binary, tag, gnu, n, layouts, modes):
    dest = OUT / tag
    dest.mkdir(parents=True, exist_ok=True)
    source, compiled, driver = prepare(dest, gnu)
    binary = str(Path(binary).resolve())
    is_gnu = binary == str(Path(gnu).resolve())
    gc = "Fgarbage_collect"
    if not is_gnu:
        symbols = subprocess.check_output(["nm", binary], text=True)
        gc = "0x" + re.search(r"^([0-9a-f]+) T bf_garbage_collect_w$", symbols, re.M)[1]
    report = {"binary": C.pin(binary), "gnu": C.pin(gnu),
              "inputs": [C.pin(__file__), C.pin(source), C.pin(compiled), C.pin(driver)],
              "n": n, "method": __doc__, "runs": []}
    for mode in modes:
        for layout in layouts:
            stem = dest / f"{mode}-{layout}"
            env = dict(os.environ, PCF_MODE=mode, PCF_N=str(n),
                       ABI_LAYOUT_PAD="x" * layout, TMPDIR=str(ROOT / "target/tmp"),
                       XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
            env.pop("NELISP_FX_DISABLE", None)
            env.pop("PCF_PROFILE", None)
            if mode == "interpreter":
                env["NELISP_FX_DISABLE"] = "1"
            invocation = [binary] + (["-Q", "--batch"] if is_gnu else []) + ["--load", str(driver)]
            command = ["valgrind", "--tool=callgrind", "--error-exitcode=97",
                       "--dump-after=" + gc, "--callgrind-out-file=" + str(stem) + ".cg"] + invocation
            started = time.monotonic()
            with Path(str(stem) + ".out").open("wb") as out, Path(str(stem) + ".err").open("wb") as err:
                result = subprocess.run(command, cwd=ROOT, env=dict(env, PCF_PROFILE="1"),
                                        stdout=out, stderr=err, timeout=600)
            if result.returncode or f"PCF-COMPLETE {len(OPS)}" not in Path(str(stem) + ".out").read_text():
                raise ValueError(f"invalid Ir run: {stem}, exit {result.returncode}")
            wall = subprocess.run(invocation, cwd=ROOT, env=env, capture_output=True, timeout=180)
            Path(str(stem) + ".wall.out").write_bytes(wall.stdout)
            Path(str(stem) + ".wall.err").write_bytes(wall.stderr)
            if wall.returncode or wall.stderr or f"PCF-COMPLETE {len(OPS)}" not in wall.stdout.decode():
                raise ValueError(f"invalid wall run: {stem}")
            rows = []
            for i, op in enumerate(OPS, 2):
                profile = Path(str(stem) + f".cg.{i}")
                total = re.search(r"^summary: (\d+)", profile.read_text(), re.M)
                timing = re.search(rf"^PCF-TIME {op} ([0-9.]+)$", wall.stdout.decode(), re.M)
                if not total or not timing:
                    raise ValueError(f"missing profile or clock sample: {op}, {stem}")
                rows.append({"op": op, "ir": int(total[1]), "seconds": float(timing[1]),
                             "profile": C.pin(profile)})
            for row in rows[1:]:
                control = rows[0] if row["op"] in ("while", "dotimes") else rows[-2]
                row["ir_per_op"] = (row["ir"] - control["ir"]) / n
                row["us_per_op"] = 1e6 * (row["seconds"] - control["seconds"]) / n
            report["runs"].append({"mode": mode, "layout": layout, "command": command,
                                    "elapsed_s": time.monotonic() - started, "intervals": rows})
            (dest / "perf.json").write_text(json.dumps(report, indent=2) + "\n")
            print(tag, mode, layout, {r["op"]: round(r["ir_per_op"]) for r in rows[1:]}, flush=True)


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("binary")
    p.add_argument("tag")
    p.add_argument("--gnu", default=shutil.which("emacs"))
    p.add_argument("--n", type=int, default=300)
    p.add_argument("--layout", type=int, action="append")
    p.add_argument("--mode", action="append", choices=C.MODES)
    a = p.parse_args()
    measure(a.binary, a.tag, a.gnu, a.n, a.layout or (0, 700, 1400, 2100), a.mode or C.MODES)
