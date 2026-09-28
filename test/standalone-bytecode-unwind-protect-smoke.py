#!/usr/bin/env python3
"""Compare focused unwind-protect bytecode cases with GNU Emacs."""
import os
import subprocess
import sys


EMACS = os.environ.get("EMACS_BIN", "emacs")
NELISP = os.environ.get("NELISP_BIN", "target/nelisp")
CASES = [
    ("symbol-cleanup", "(41 3)", r"""(progn (setq nelisp-cleanup 0) (fset 'nelisp-cleanup-fn (lambda () (setq nelisp-cleanup 3))) (list (funcall (make-byte-code nil (unibyte-string 192 142 193 41 135) [nelisp-cleanup-fn 41] 1)) nelisp-cleanup))"""),
    ("form-list-cleanup", "(41 4)", r"""(progn (setq nelisp-cleanup 0) (list (funcall (make-byte-code nil (unibyte-string 192 142 193 41 135) (vector '((setq nelisp-cleanup 4)) 41) 1)) nelisp-cleanup))"""),
    ("nested-lifo", "(41 2)", r"""(progn (setq nelisp-cleanup 0) (list (funcall (make-byte-code nil (unibyte-string 192 142 193 142 42 194 135) (vector (lambda () (setq nelisp-cleanup 2)) (lambda () (setq nelisp-cleanup 1)) 41) 1)) nelisp-cleanup))"""),
    ("throw-cleanup", "(17 2)", r"""(progn (setq nelisp-cleanup 0) (list (funcall (make-byte-code nil (unibyte-string 192 50 12 0 193 142 194 192 195 34 41 48 135) (vector 'k (lambda () (setq nelisp-cleanup 2)) 'throw 17) 3)) nelisp-cleanup))"""),
    ("cleanup-throw-override", "23", r"""(funcall (make-byte-code nil (unibyte-string 192 50 17 0 193 50 16 0 194 142 195 193 196 34 41 48 48 135) (vector 'outer 'inner (lambda () (throw 'outer 23)) 'throw 17) 3))"""),
    ("cleanup-throw-replaces-pending-error", "23", r"""(condition-case err (funcall (make-byte-code nil (unibyte-string 192 50 10 0 193 142 194 64 41 48 135) (vector 'outer (lambda () (throw 'outer 23)) 1) 1)) (error (list (car err) (cadr err) (caddr err))))"""),
    ("cleanup-error-replaces-pending-throw", '(error "cleanup")', r"""(condition-case err (funcall (make-byte-code nil (unibyte-string 192 50 17 0 193 50 16 0 194 142 195 193 196 34 41 48 48 135) (vector 'outer 'inner (lambda () (error "cleanup")) 'throw 17) 3)) (error (list (car err) (cadr err))))"""),
    ("cleanup-signal-replaces-pending-throw", '(file-missing "x")', r"""(funcall (make-byte-code nil (unibyte-string 193 49 23 0 194 50 21 0 195 50 20 0 196 142 197 195 198 34 41 48 48 48 135 137 24 64 8 65 64 41 68 135) (vector 'err '(file-error) 'outer 'inner (lambda () (signal 'file-missing '("x"))) 'throw 17) 4))"""),
    ("dynamic-binding", "(41 1 9)", r"""(progn (defvar nelisp-dyn 1) (defvar nelisp-seen 0) (list (funcall (make-byte-code nil (unibyte-string 193 24 194 142 42 195 135) (vector 'nelisp-dyn 9 (lambda () (setq nelisp-seen nelisp-dyn)) 41) 1)) nelisp-dyn nelisp-seen))"""),
    ("gc-cleanup", "41", r"""(funcall (make-byte-code nil (unibyte-string 192 142 193 41 135) [(lambda () (garbage-collect)) 41] 1))"""),
    ("32-deep-cleanup", "(41 32)", "(progn (setq nelisp-cleanup-count 0) (list (funcall (make-byte-code nil (unibyte-string " + " ".join([str(x) for x in sum(([192+i,142] for i in range(32)),[]) + [224,135]]) + ") (vector " + " ".join(["(lambda () (setq nelisp-cleanup-count (1+ nelisp-cleanup-count)))"]*32 + ["41"]) + ") 33)) nelisp-cleanup-count))"),
]


def run(executable, args, expression):
    result = subprocess.run(
        [executable, *args, "--eval", "(prin1 " + expression + ")"],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        timeout=30,
    )
    return result.returncode, result.stdout


def main():
    for name, expected, expression in CASES:
        host_rc, host_out = run(EMACS, ["-Q", "--batch"], expression)
        nelisp_rc, nelisp_out = run(NELISP, [], expression)
        if host_rc or nelisp_rc or not host_out.startswith(expected) or not nelisp_out.startswith(expected):
            print(f"FAIL {name}: host=({host_rc}) {host_out[:180]!r}; standalone=({nelisp_rc}) {nelisp_out[:180]!r}")
            return 1
        print(f"PASS {name}: {expected}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
