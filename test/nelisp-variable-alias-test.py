#!/usr/bin/env python3
"""Compare standalone variable-alias behavior with GNU Emacs 31.1."""
import argparse
import json
import os
import subprocess
import sys
import tempfile


def run(argv, script):
    return subprocess.run(argv, input=script.encode(), capture_output=True,
                          timeout=45)


def first_mismatch(left, right):
    for i, (a, b) in enumerate(zip(left, right)):
        if a != b:
            return i
    return min(len(left), len(right))


def forms(provider=None, routing=False):
    setup = []
    if provider:
        setup.append("(load %s nil t)" % json.dumps(os.path.abspath(provider)))
    cases = [
        "(let ((a (make-symbol \"a\")) (b (make-symbol \"b\"))) (set b 12) (defvaralias a b) (set b 23) (list (eq (indirect-variable a) b) (symbol-value a)))",
        "(let ((a (make-symbol \"write-a\")) (b (make-symbol \"write-b\"))) (set b 1) (defvaralias a b) (set a 9) (list (symbol-value b) (symbol-value a) (boundp a)))",
        "(let ((a (make-symbol \"migration-a\")) (b (make-symbol \"migration-b\"))) (set a 5) (defvaralias a b) (list (symbol-value a) (symbol-value b) (eq (indirect-variable a) b)))",
        "(let ((a (make-symbol \"same\")) (b (make-symbol \"same\")) (c (make-symbol \"same\"))) (set b 3) (defvaralias a b) (list (eq (indirect-variable a) b) (eq (indirect-variable c) c) (symbol-value a)))",
        "(let ((a (make-symbol \"chain-a\")) (b (make-symbol \"chain-b\")) (c (make-symbol \"chain-c\"))) (set c 7) (defvaralias b c) (defvaralias a b) (list (eq (indirect-variable a) c) (symbol-value a) (eq (defvaralias a b) b)))",
        "(let ((a (make-symbol \"cell-a\")) (b (make-symbol \"cell-b\"))) (put b 'p 2) (fset b (lambda () 3)) (defvaralias a b) (put a 'p 1) (fset a (lambda () 4)) (list (get a 'p) (get b 'p) (funcall a) (funcall b)))",
        "(let ((a (make-symbol \"del-a\")) (b (make-symbol \"del-b\"))) (set b 8) (defvaralias a b) (internal-delete-indirect-variable a) (list (eq (indirect-variable a) a) (boundp a) (symbol-value b)))",
        "(let ((a (make-symbol \"delete-index-a\")) (b (make-symbol \"delete-index-b\"))) (set b 8) (defvaralias a b) (internal-delete-indirect-variable a) (set b 9) (list (boundp a) (symbol-value b)))",
        "(let ((a (make-symbol \"unbind-a\")) (b (make-symbol \"unbind-b\"))) (set b 8) (defvaralias a b) (makunbound a) (list (eq (indirect-variable a) a) (boundp a) (boundp b)))",
        "(let ((a (make-symbol \"cycle-a\")) (b (make-symbol \"cycle-b\"))) (defvaralias a b) (condition-case e (defvaralias b a) (error (list (car e) (eq (indirect-variable a) b)))))",
        "(let ((a (make-symbol \"nil-target\"))) (list (defvaralias a nil) (boundp a) (symbol-value a)))",
        "(let ((a (make-symbol \"doc-a\")) (b (make-symbol \"doc-b\")) (n 0)) (defvaralias a b (progn (setq n (1+ n)) \"doc\")) (list n (get a 'variable-documentation)))",
        "(condition-case e (defvaralias nil 'alias-test-target) (error (car e)))",
        "(condition-case e (defvaralias 1 'alias-test-target) (error (car e)))",
    ]
    if routing:
        cases = [
            "(progn (set 'nelisp-route-base 17) (list (defvaralias 'nelisp-route-alias 'nelisp-route-base) (boundp 'nelisp-route-alias) (symbol-value 'nelisp-route-alias)))",
            "(let ((was (fboundp 'defvaralias)) (old (and (fboundp 'defvaralias) (symbol-function 'defvaralias)))) (unwind-protect (progn (defalias 'defvaralias (lambda (a b &optional d) (list 'first a b d))) (let ((direct (defvaralias 'x 'y)) (called (funcall (symbol-function 'defvaralias) 'x 'y))) (defalias 'defvaralias (lambda (a b &optional d) (list 'second a b d))) (list direct called (defvaralias 'z 'w)))) (if was (defalias 'defvaralias old) (fmakunbound 'defvaralias))))",
        ]
    return ";;; -*- lexical-binding: t; -*-\n" + "\n".join(setup + ["(condition-case e (prin1 %s) (error (prin1 (list 'error (car e) (cdr e))))) (terpri)" % c for c in cases]) + "\n"


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--expect-defect", metavar="PATH")
    p.add_argument("--provider")
    p.add_argument("--routing", action="store_true")
    a = p.parse_args()
    native_bin = a.expect_defect or os.environ.get("NELISP_BIN", "target/nelisp")
    gnu_bin = os.environ.get("EMACS", "emacs")
    version = run([gnu_bin, "--version"], "")
    if version.returncode or not version.stdout.startswith(b"GNU Emacs 31.1"):
        sys.exit("GNU baseline must be Emacs 31.1; version output head=%r" % version.stdout[:100])
    routing = a.routing or bool(a.expect_defect)
    script = forms(None, routing)
    native_script = forms(None if routing else a.provider, routing) + "(exit 0)\n"
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as f:
        f.write(script)
        path = f.name
    try:
        gnu = run([gnu_bin, "--batch", "-Q", "-l", path], "")
        if gnu.returncode or not gnu.stdout or gnu.stderr:
            sys.exit("invalid GNU baseline: rc=%d stdout-bytes=%d stderr-bytes=%d stderr-head=%r" %
                     (gnu.returncode, len(gnu.stdout), len(gnu.stderr), gnu.stderr[:160]))
        native = run([native_bin, "--repl", "--no-prompt", "--no-print"], native_script)
    finally:
        os.unlink(path)
    known_defect = (b"(nelisp-route-base t 17)\n"
                    b"(y (first x y nil) w)\n")
    if a.expect_defect:
        if native.returncode == 0 and not native.stderr and native.stdout == known_defect:
            print("EXPECTED DEFECT: exact defvaralias dispatch rows")
            return 0
        sys.exit("--expect-defect failed: native did not match the recorded defect")
    if native.stdout == gnu.stdout and native.stderr == b"" and native.returncode == 0:
        print("PASS: exact stdout bytes, zero stderr, matching GNU 31.1")
        return 0
    sys.stderr.write("FAIL: GNU rc=%d stderr-bytes=%d; native rc=%d stderr-bytes=%d\n" %
                     (gnu.returncode, len(gnu.stderr), native.returncode, len(native.stderr)))
    sys.stderr.write("first stdout mismatch at byte %d\n" % first_mismatch(gnu.stdout, native.stdout))
    return 1


if __name__ == "__main__":
    sys.exit(main())
