#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Exercise non-cold autoload and authenticated companion failure boundaries."""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, default=Path("target/nelisp-static"))
    parser.add_argument("--receipt", type=Path)
    args = parser.parse_args()
    binary = (ROOT / args.binary).resolve(strict=True)
    rows = []

    def run(name, executable, expression, expected):
        start = time.monotonic()
        result = subprocess.run([str(executable), "--eval", expression], cwd=ROOT,
                                capture_output=True, text=True, timeout=180)
        row = dict(case=name, seconds=time.monotonic() - start,
                   rc=result.returncode, stdout=result.stdout, stderr=result.stderr,
                   passed=result.returncode == 0 and not result.stderr
                   and result.stdout == expected + "\n")
        rows.append(row)
        if args.receipt:
            args.receipt.write_text(json.dumps(dict(
                binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
                rows=rows), indent=2) + "\n")
        if not row["passed"]:
            raise RuntimeError(json.dumps(row))
        print("PASS " + name, flush=True)

    with tempfile.TemporaryDirectory(prefix="native-startup-lazy-", dir=ROOT / "target") as temporary:
        isolated = Path(temporary) / binary.name
        shutil.copy2(binary, isolated)
        companion = Path(str(isolated) + ".native-startup.el")
        run("ordinary-boot-without-companion", isolated,
            "(list (+ 1 2) (featurep 'nelisp-native-load) "
            "(featurep 'nelisp-bytecode-compiler-input) "
            "(featurep 'nelisp-native-template) "
            "(autoloadp (symbol-function 'nelisp-native-cache-compile)))",
            "(3 nil nil nil t)")
        failure = """(let ((first (condition-case err
                          (progn (nelisp-native-load-rooted-production-contract) nil)
                        (error (error-message-string err))))
                         (refused 0))
                       (dolist (request '(require load call))
                         (condition-case err
                             (cond ((eq request 'require) (require 'nelisp-native-load))
                                   ((eq request 'load) (load "nelisp-native-load"))
                                   (t (nelisp-native-load-rooted-production-contract)))
                           (error (when (equal (error-message-string err)
                                               "Native startup previously failed")
                                    (setq refused (1+ refused))))))
                       (list (and first t) refused))"""
        run("missing-companion-fails-closed", isolated, failure, "(t 3)")
        shutil.copyfile(Path(str(binary) + ".native-startup.el"), companion)
        with companion.open("a") as stream:
            stream.write("\n;; altered after build\n")
        run("tampered-companion-fails-closed", isolated,
            failure.replace("(and first t)",
                            '(equal first "Native startup companion hash mismatch")'), "(t 3)")
        shutil.copyfile(Path(str(binary) + ".native-startup.el"), companion)
        run("autoload-publishes-once", isolated,
            """(let* ((contract (nelisp-native-load-rooted-production-contract))
                      (owner (symbol-function 'nelisp-native-load-rooted-production-contract)))
                 (require 'nelisp-native-load)
                 (list (plist-get (car contract) :domain)
                       (featurep 'nelisp-native-compiler-runtime-capability)
                       (featurep 'nelisp-native-compiler-f1-runtime-proof)
                       (equal contract (nelisp-native-load-rooted-production-contract))
                       (eq owner (symbol-function 'nelisp-native-load-rooted-production-contract))))""",
            '("nelisp-rooted-elf-v2" t t t t)')
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
