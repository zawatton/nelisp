#!/usr/bin/env python3
"""Ir per loop iteration of the byte-code opcodes Bnegate, Bmax, Bmin, Bquo and Brem against GNU 31.1 (integ-wh S2.34).

  integ-wh-bytecode-arith-rows.py measure --dyn BIN [--gnu EMACS] [--work DIR] [--no-gnu]
  integ-wh-bytecode-arith-rows.py check --dyn BIN [--gnu EMACS] [--work DIR]

One function per opcode, byte-compiled by host GNU Emacs (the fixture is written into the work directory), each a loop
`(while (< i n) (setq s EXPR) (setq i (1+ i)))' over a fixnum counter: the control row has EXPR = i, the plus row (+ i 7), and the
others (- i), (max i 7), (min i 7), (/ i 7) and (% i 7), which compile to Bnegate, Bmax, Bmin, Bquo and Brem (bytecode.c runs the
fixnum case of each in the opcode's own arm).  The plus row is the reference of the four binary rows: it has the same constant
push and the same store into s, so what a row costs above it is the opcode alone; Bnegate's loop differs from the control's by
that opcode alone.  Both engines load the fixture and run each row for 2,500 iterations under callgrind (one
process each, 8 GiB address-space cap), and once for none; the Ir per iteration is the difference over the 2,500 iterations,
which cancels the start-up, and a row's figure is that above the control row's.  `measure' prints one line per row;
`check' is the ledger criterion: Bnegate at most CAP_OPCODE Ir above the control, and each binary opcode at most CAP_OPCODE Ir
above the plus row (the fast loop runs Bplus in place).  GNU's figures come from the same run (they are not pinned).  Run the whole
measurement as one heavy unit (bin/heavy-run.sh).

Batch D, whose VM took each of these through the boxed path or a by-name call of the function (the fast loop stops at all
five): the rows of the table printed by `measure' in the ledger note of S2.34.
"""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
N = 2500
ROWS = {"control": "i", "plus": "(+ i 7)", "neg": "(- i)", "max": "(max i 7)", "min": "(min i 7)", "quo": "(/ i 7)", "rem": "(% i 7)"}
CAP = 8 * 1024 ** 3
CAP_OPCODE = 120  # Ir an opcode may cost above its reference (batch D: 1,200 to 7,200 above it)

FIXTURE_GEN = r"""
(require 'bytecomp)
(setq lexical-binding t)
(let ((out (getenv "AR_OUT")) (rows (read (getenv "AR_ROWS"))))
  (with-temp-file out
    (let ((print-escape-newlines t) (print-escape-nonascii t) (print-length nil) (print-level nil))
      (dolist (row rows)
        (let ((name (intern (concat "ar-" (car row)))))
          (eval `(defun ,name (n) (let ((i 0) (s 0)) (while (< i n) (setq s ,(cdr row)) (setq i (1+ i))) s)) t)
          (byte-compile name)
          (prin1 `(defalias ',name ,(symbol-function name)) (current-buffer))
          (insert "\n"))))))
"""


def write_fixture(gnu, work):
    work.mkdir(parents=True, exist_ok=True)
    fixture = work / "fixture.el"
    gen = work / "fixture-gen.el"
    gen.write_text(FIXTURE_GEN)
    # the rows are read back as Lisp: (("control" . i) ("neg" . (- i)) ...)
    rows = "(" + " ".join(f'("{k}" . {v})' for k, v in ROWS.items()) + ")"
    env = dict(os.environ, AR_OUT=str(fixture), AR_ROWS=rows, LC_ALL="C.UTF-8")
    subprocess.run(["bash", "-c", f'ulimit -v {CAP // 1024}; exec "$@"', "gen", str(gnu), "-Q", "--batch", "-l", str(gen)],
                   env=env, check=True, capture_output=True, text=True)
    return fixture


def program(fixture, row, n):
    return (";;; -*- lexical-binding: t -*-\n"
            "(setq gc-cons-threshold most-positive-fixnum)\n"
            f'(load "{fixture}" nil t)\n'
            f"(ar-{row} 20)\n"
            "(garbage-collect)\n"
            f"(ar-{row} {n})\n"
            "(kill-emacs 0)\n")


def callgrind(binary, source, out, gnu_engine):
    def bounded():
        import resource
        resource.setrlimit(resource.RLIMIT_AS, (CAP, CAP))
    head = ["valgrind", "--tool=callgrind", f"--callgrind-out-file={out}", "--log-file=" + str(out) + ".log"]
    tail = [str(binary), "-Q", "--batch", "-l", str(source)] if gnu_engine else [str(binary), "--load", str(source)]
    env = dict(os.environ, NELISP_FX_DISABLE="1", LC_ALL="C.UTF-8")
    proc = subprocess.run(head + tail, env=env, preexec_fn=bounded, capture_output=True, text=True, timeout=3000)
    if proc.returncode != 0:
        raise SystemExit(f"FAIL {binary} {source}: rc={proc.returncode} {proc.stderr[-300:]}")
    total = None
    for line in Path(out).read_text().splitlines()[-40:]:
        if line.startswith("summary:") or line.startswith("totals:"):
            total = int(line.split()[1])
    if total is None:
        raise SystemExit(f"FAIL no callgrind total in {out}")
    return total


def per_iter(binary, fixture, work, engine):
    # One run with no iterations (the start-up, the fixture's load and the 20 warm-up iterations every run has) and one run of N
    # iterations per row: the difference over N is the Ir per iteration.
    source = work / f"{engine}-start.el"
    source.write_text(program(fixture, "control", 0))
    start = callgrind(binary, source, work / f"{engine}-start.cg", engine == "gnu")
    figures = {}
    for row in ROWS:
        source = work / f"{engine}-{row}.el"
        source.write_text(program(fixture, row, N))
        figures[row] = (callgrind(binary, source, work / f"{engine}-{row}.cg", engine == "gnu") - start) / N
    return {row: figures[row] - figures["control"] for row in ROWS if row != "control"}, figures["control"]


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["measure", "check"])
    ap.add_argument("--dyn", required=True, type=Path)
    ap.add_argument("--gnu", type=Path, default=Path("/usr/local/bin/emacs"))
    ap.add_argument("--work", type=Path, default=ROOT / "target/integ-wh/bytecode-arith-rows")
    ap.add_argument("--no-gnu", action="store_true")
    a = ap.parse_args(argv)
    a.dyn, a.gnu, a.work = a.dyn.resolve(), a.gnu.resolve(), a.work.resolve()
    fixture = write_fixture(a.gnu, a.work)
    mine, mine_control = per_iter(a.dyn, fixture, a.work, "nelisp")
    gnu, gnu_control = (None, None) if a.no_gnu and a.command == "measure" else per_iter(a.gnu, fixture, a.work, "gnu")
    bad = []
    print(f"BC-ARITH control: nelisp {mine_control:.0f} Ir/iter" + (f", gnu {gnu_control:.0f}" if gnu else ""))
    for row in mine:
        above = mine[row] - (mine["plus"] if row in ("max", "min", "quo", "rem") else 0)
        line = f"BC-ARITH {row}: nelisp {mine[row]:.0f} Ir/iter above the control"
        if row in ("max", "min", "quo", "rem"):
            line += f", {above:.0f} above the plus row"
        if gnu:
            gabove = gnu[row] - (gnu["plus"] if row in ("max", "min", "quo", "rem") else 0)
            line += f"; gnu {gnu[row]:.0f}" + (f", {gabove:.0f} above its plus row" if row in ("max", "min", "quo", "rem") else "")
        if row not in ("plus",) and above > CAP_OPCODE:
            bad.append(row)
        print(line)
    if a.command == "check":
        print(("FAIL" if bad else "PASS") + f" bytecode-arith-rows: {'opcodes over the cap: ' + ' '.join(bad) if bad else 'every opcode within %d Ir of its reference' % CAP_OPCODE}")
        return 1 if bad else 0
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
