"""Bounded exact-output GNU 31.1 comparison for standalone regressions."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile


def run(argv, source=""):
    return subprocess.run(argv, input=source.encode(), capture_output=True, timeout=45)


def source(cases, setup=""):
    return ";;; -*- lexical-binding: t; -*-\n(progn " + setup.replace("\n", " ") + ")\n" + "\n".join(
        "(prin1 (condition-case err %s (error (cons 'ERR err)))) (terpri)" % case
        for case in cases) + "\n"


def main(cases, setup="", defect=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--expect-defect", metavar="PATH")
    args = parser.parse_args()
    host = os.environ.get("EMACS", "emacs")
    version = run([host, "--version"])
    if version.returncode or not version.stdout.startswith(b"GNU Emacs 31.1"):
        raise SystemExit("GNU baseline must be Emacs 31.1")
    script = source(cases, setup)
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as stream:
        stream.write(source(cases, "(require 'cl-lib)\n" + setup))
        path = stream.name
    try:
        gnu = run([host, "--batch", "-Q", "-l", path])
    finally:
        Path(path).unlink()
    if gnu.returncode or gnu.stderr or len(gnu.stdout.splitlines()) != len(cases):
        raise SystemExit("invalid GNU baseline rc=%d rows=%d stderr-head=%r" %
                         (gnu.returncode, len(gnu.stdout.splitlines()), gnu.stderr[:160]))
    binary = args.expect_defect or os.environ.get("NELISP_BIN", "target/nelisp")
    native = run([binary, "--repl", "--no-prompt", "--no-print"], script + "(exit 0)\n")
    if args.expect_defect:
        ok = (defect is not None and native.returncode == 0 and not native.stderr
              and hashlib.sha256(native.stdout).hexdigest() == defect
              and native.stdout != gnu.stdout)
    else:
        ok = (native.returncode == 0 and not native.stderr and native.stdout == gnu.stdout)
    if not ok:
        mismatches = [i + 1 for i, (a, b) in enumerate(zip(
            gnu.stdout.splitlines(), native.stdout.splitlines())) if a != b]
        print("FAIL rows=%d rc=%d stderr-bytes=%d differing-rows=%s" %
              (len(native.stdout.splitlines()), native.returncode,
               len(native.stderr), json.dumps(mismatches[:20])))
        return 1
    print("%s exact rows=%d bytes=%d" %
          ("EXPECTED DEFECT" if args.expect_defect else "PASS",
           len(cases), len(native.stdout)))
    return 0
