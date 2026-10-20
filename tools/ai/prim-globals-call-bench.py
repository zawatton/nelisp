#!/usr/bin/env python3
"""Measure the brief's funcall/dotimes call shape in all three body modes.

The zero-iteration control excludes startup and the preexisting heap's GC.
Unlike the diagnostic marginal benchmark, loop and call overhead remain in
the per-operation total. The ending collection remains in every interval.
The Ir process neither reads nor formats the clock: the float formatter's
value-dependent work must not be charged to the primitive. A separate normal
process records wall time with the identical operation loop.
"""
import argparse
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess

SPEC = importlib.util.spec_from_file_location("globals_perf", Path(__file__).with_name("prim-globals-perf.py"))
COMMON = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(COMMON)


def prepare(dest, gnu):
    source = dest / "functions.el"
    source.write_text(";;; -*- lexical-binding: t -*-\n(defvar pgb-global 17)\n" +
        "\n".join(f"(defun pgb-{op} () {('nil' if op == 'funcall' else body)})"
                   for op, body in COMMON.OPS.items()) + "\n")
    compiled = dest / "bytecode.el"
    expr = f'''(progn (require 'bytecomp)
      (with-temp-buffer (insert-file-contents {json.dumps(str(source))})
       (check-parens) (goto-char (point-min))
       (let (forms) (condition-case nil
        (while t (push (read (current-buffer)) forms)) (end-of-file nil))
        (with-temp-file {json.dumps(str(compiled))}
         (insert ";;; -*- lexical-binding: t -*-\\n")
         (dolist (form (nreverse forms)) (eval form t)
          (prin1 (if (eq (car form) 'defun)
           (list 'fset (list 'quote (cadr form)) (byte-compile (cadr form))) form)
           (current-buffer)) (insert "\\n"))))))'''
    subprocess.run([gnu, "-Q", "--batch", "--eval", expr], check=True, capture_output=True)
    driver = dest / "driver.el"
    names = " ".join("pgb-" + op for op in COMMON.OPS)
    driver.write_text(";;; -*- lexical-binding: t -*-\n" +
        f"(load (if (equal (getenv \"PG_MODE\") \"bytecode\") "
        f"{json.dumps(str(compiled))} {json.dumps(str(source))}) nil t)\n" +
        "(defun pgb-run (fn n) (dotimes (_ n) (funcall fn)))\n" +
        "(when (and (equal (getenv \"PG_MODE\") \"vm\") "
        "(fboundp 'nelisp--fx-register))\n" +
        f" (dolist (name '({names})) (nelisp--fx-register name))\n" +
        f" (dolist (name '({names})) "
        "(unless (vectorp (gethash (nthcdr 3 (symbol-function name)) nelisp--fx-table)) "
        "(error \"Missing VM coverage: %S\" name))) (princ \"PERF-VM-COVERED 6\\n\"))\n" +
        "\n".join(f"(pgb-run (symbol-function 'pgb-{op}) 1)" for op in COMMON.OPS) +
        "\n" + "\n".join(
            f"(unless (getenv \"PG_PROFILE\") (format \"PERF {op} %S %.9f\\n\" pgb-global 0.0))"
            for op in COMMON.OPS) +
        "\n(pgb-run (symbol-function 'pgb-control) 0)\n" +
        "(garbage-collect)\n(garbage-collect)\n(garbage-collect)\n" +
        "\n".join(f"(let* ((n {('0' if op == 'control' else '(string-to-number (getenv \"PG_N\"))')}) "
                   f"(fn (symbol-function 'pgb-{op}))) "
                   f"(if (getenv \"PG_PROFILE\") (pgb-run fn n) "
                   f"(let ((t0 (float-time))) (pgb-run fn n) "
                   f"(princ (format \"PERF {op} %S %.9f\\n\" "
                   f"pgb-global (- (float-time) t0))))))\n(garbage-collect)"
                   for op in COMMON.OPS) +
        "\n(princ \"PERF-COMPLETE 6\\n\")\n(kill-emacs 0)\n")
    return driver


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary")
    parser.add_argument("tag")
    parser.add_argument("--gnu", default=shutil.which("emacs"))
    parser.add_argument("--n", type=int, default=1000)
    parser.add_argument("--layout", type=int, action="append")
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()
    COMMON.prepare = prepare
    COMMON.DUMP_START = 4
    result = COMMON.measure(args.binary, args.tag, args.gnu, args.n, args.layout or COMMON.LAYOUTS, args.jobs)
    result["inputs"].append(COMMON.pin(__file__))
    result["method"] = __doc__
    (COMMON.OUT if hasattr(COMMON, "OUT") else COMMON.ROOT / "target/prim-globals").joinpath(
        args.tag, "perf.json").write_text(json.dumps(result, indent=2) + "\n")
