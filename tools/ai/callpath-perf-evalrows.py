#!/usr/bin/env python3
"""Interpreted builtin-call rows: Ir per loop iteration against GNU 31.1 (ledger stage S5).

  callpath-perf-evalrows.py measure TAG BINARY [--gnu EMACS]
      Runs callgrind on BINARY and on GNU Emacs 31.1, both interpreting the same
      source file, and writes target/callpath-perf/evalrows/TAG/report.json.

Each row is a function whose loop body is `(setq er-sink EXPR)', so a row's cost
above the CONTROL row (`(setq er-sink i)') is the evaluation of EXPR alone:

  plus     (+ i 120)             a MANY-arity builtin with a variable and a constant
  cons     (cons i i)            a two-argument builtin that allocates
  makevec  (make-vector 16 i)    a builtin that allocates a vector

The functions are interpreted in both runtimes (the file is loaded as source with
lexical-binding), so the rows measure the evaluator's cost of calling a builtin
(GNU eval.c eval_sub: the car's function cell, an UNEVALLED test, the arguments
evaluated into an array, a direct call), not the builtin.  The Ir per iteration is
the difference of two runs of N2 and N1 iterations divided by N2 - N1, which
cancels the start-up; the NeLisp run is the standalone reader with the FX VM off
(NELISP_FX_DISABLE=1), so the interpreter evaluates the loop.  The program raises
gc-cons-threshold and collects once before the measured loop, so neither engine
collects inside it (GNU's make-vector row otherwise absorbs a collection at the
longer run).  Each run is its own callgrind process under an 8 GiB address-space
cap; run the whole measurement as one heavy unit.

The same two engines also run three interpreted LOOPS, whose whole iteration is the
figure (no control row): a `while' with a let-bound counter, the same with a second
`setq', and a loop that calls a one-argument closure (GNU eval.c eval_sub, Fsetq,
Fwhile, funcall_lambda).
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf/evalrows"
GNU = "/usr/local/bin/emacs"
N1, N2 = 1000, 6000
ROWS = {
    "control": "i",
    "plus": "(+ i 120)",
    "cons": "(cons i i)",
    "makevec": "(make-vector 16 i)",
}
LN1, LN2 = 1000, 5000
LOOPS = {
    "while": "(defun lp-row (n) (let ((i 0)) (while (< i n) (setq i (1+ i))) i))",
    "while3": "(defun lp-row (n) (let ((i 0) (a 1) (b 2)) (while (< i n) (setq a (+ a b)) (setq i (1+ i))) i))",
    "closure": "(defun lp-f (x) (+ x 1))\n(defun lp-row (n) (let ((i 0)) (while (< i n) (setq i (lp-f i))) i))",
}
CAP = 8 * 1024 ** 3


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pin(path):
    path = Path(path).resolve()
    try:
        shown = str(path.relative_to(ROOT))
    except ValueError:
        shown = str(path)
    return {"path": shown, "sha256": sha(path)}


def source_identity():
    names = subprocess.check_output(["git", "ls-files", "src", "lisp", "packages", "scripts", "Makefile"],
                                    cwd=ROOT, text=True).splitlines()
    rows = [[name, sha(ROOT / name)] for name in names if (ROOT / name).is_file()]
    return {"count": len(rows),
            "sha256": hashlib.sha256(json.dumps(rows, separators=(",", ":")).encode()).hexdigest()}


def program(expr, n):
    """The Lisp source both engines interpret for one row and iteration count."""
    return (";;; -*- lexical-binding: t -*-\n"
            "(defvar er-sink nil)\n"
            "(setq gc-cons-threshold most-positive-fixnum)\n"
            f"(defun er-row (n) (let ((i 0)) (while (< i n) (setq er-sink {expr}) (setq i (1+ i))) er-sink))\n"
            "(er-row 20)\n"
            "(garbage-collect)\n"
            f"(er-row {n})\n"
            "(kill-emacs 0)\n")


def loop_program(name, n):
    """The Lisp source both engines interpret for one loop and iteration count."""
    return (";;; -*- lexical-binding: t -*-\n"
            "(setq gc-cons-threshold most-positive-fixnum)\n"
            f"{LOOPS[name]}\n"
            "(lp-row 20)\n"
            "(garbage-collect)\n"
            f"(lp-row {n})\n"
            "(kill-emacs 0)\n")


def callgrind_total(path):
    total = None
    for line in Path(path).read_text().splitlines()[-40:]:
        if line.startswith("summary:") or line.startswith("totals:"):
            total = int(line.split()[1])
    if total is None:
        raise RuntimeError(f"no callgrind total in {path}")
    return total


def callgrind(binary, source, out, env):
    def bounded():
        import resource
        resource.setrlimit(resource.RLIMIT_AS, (CAP, CAP))
    head = ["valgrind", "--tool=callgrind", f"--callgrind-out-file={out}", "--log-file=" + str(out) + ".log"]
    tail = [str(binary), "-Q", "--batch", "-l", str(source)] if "emacs" in Path(binary).name else \
           [str(binary), "--load", str(source)]
    proc = subprocess.run(head + tail, env=env, preexec_fn=bounded, capture_output=True, text=True, timeout=3000)
    if proc.returncode != 0:
        raise RuntimeError(f"{binary} {source}: rc={proc.returncode} {proc.stderr[-300:]}")
    return callgrind_total(out)


def measure(args):
    if shutil.disk_usage(ROOT).free < 50 * 1024 ** 3:
        raise RuntimeError("stop: less than 50 GiB free")
    dest = OUT / args.tag
    dest.mkdir(parents=True, exist_ok=False)
    binary, gnu = Path(args.binary).resolve(), Path(args.gnu).resolve()
    (dest / "producer.py").write_bytes(Path(__file__).read_bytes())
    report = {"schema": 3, "source": source_identity(), "binary": pin(binary), "gnu": pin(gnu),
              "producer": pin(dest / "producer.py"), "n": [N1, N2], "rows": {}, "per_iter": {}, "files": {},
              "loop_n": [LN1, LN2], "loops": {}}
    env_nelisp = dict(os.environ, NELISP_FX_DISABLE="1")
    env_gnu = dict(os.environ)
    for engine, path, env in (("nelisp", binary, env_nelisp), ("gnu", gnu, env_gnu)):
        report["per_iter"][engine] = {}
        for row, expr in ROWS.items():
            ir = {}
            for n in (N1, N2):
                source = dest / f"{engine}-{row}-{n}.el"
                source.write_text(program(expr, n))
                out = dest / f"{engine}-{row}-{n}.cg"
                ir[n] = callgrind(path, source, out, env)
                report["files"][f"{engine}-{row}-{n}"] = {"src": pin(source), "cg": pin(out)}
                print(f"{engine} {row} N={n}: {ir[n]} Ir", flush=True)
            report["rows"].setdefault(row, {})[engine] = {str(n): ir[n] for n in ir}
            report["per_iter"][engine][row] = (ir[N2] - ir[N1]) / (N2 - N1)
        report["loops"][engine] = {}
        for name in LOOPS:
            ir = {}
            for n in (LN1, LN2):
                source = dest / f"{engine}-loop-{name}-{n}.el"
                source.write_text(loop_program(name, n))
                out = dest / f"{engine}-loop-{name}-{n}.cg"
                ir[n] = callgrind(path, source, out, env)
                report["files"][f"{engine}-loop-{name}-{n}"] = {"src": pin(source), "cg": pin(out)}
                print(f"{engine} loop {name} N={n}: {ir[n]} Ir", flush=True)
            report["loops"][engine][name] = (ir[LN2] - ir[LN1]) / (LN2 - LN1)
    base = {e: report["per_iter"][e]["control"] for e in ("nelisp", "gnu")}
    report["above_control"] = {
        e: {row: report["per_iter"][e][row] - base[e] for row in ROWS if row != "control"} for e in base}
    report["ratio"] = {row: report["above_control"]["nelisp"][row] / report["above_control"]["gnu"][row]
                       for row in ROWS if row != "control"}
    (dest / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    report["loop_ratio"] = {name: report["loops"]["nelisp"][name] / report["loops"]["gnu"][name] for name in LOOPS}
    (dest / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    for name in LOOPS:
        print(f"evalloop {name}: nelisp {report['loops']['nelisp'][name]:.0f} Ir/iter, "
              f"gnu {report['loops']['gnu'][name]:.0f} Ir/iter, ratio {report['loop_ratio'][name]:.2f}x")
    for row in report["ratio"]:
        print(f"evalrows {row}: nelisp {report['above_control']['nelisp'][row]:.0f} Ir/iter, "
              f"gnu {report['above_control']['gnu'][row]:.0f} Ir/iter, ratio {report['ratio'][row]:.2f}x")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("measure")
    p.add_argument("tag")
    p.add_argument("binary")
    p.add_argument("--gnu", default=GNU)
    args = parser.parse_args()
    measure(args)


if __name__ == "__main__":
    sys.exit(main())
