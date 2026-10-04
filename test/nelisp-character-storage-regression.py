#!/usr/bin/env python3
"""Compare full-range character storage with GNU, including printer paths."""
import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
BINARY = Path(os.environ.get("NELISP_BIN", str(ROOT / "target/nelisp"))).resolve()
CHARACTERS = (0, 127, 128, 2047, 2048, 65535, 65536, 1114111,
              2097151, 2097152, 4194175, 4194176, 4194239, 4194240, 4194303)


def run(command, form, preload=None):
    with tempfile.TemporaryDirectory(prefix="nelisp-character-storage-") as directory:
        source = Path(directory) / "probe.el"
        source.write_text(";;; -*- lexical-binding: t; -*-\n" + form, encoding="utf-8")
        preloads = []
        if preload is not None:
            preloads = ["--load", str(preload.resolve())]
        result = subprocess.run(command + preloads + ["--load", str(source), "--eval", "nil"], cwd=directory,
                                capture_output=True, timeout=45)
    if result.returncode or result.stderr:
        raise RuntimeError(f"{command[0]} failed: {result.stderr[:300]!r}")
    return result.stdout


def storage_forms():
    forms = []
    for character in CHARACTERS:
        forms.append(
            f"(let ((s (char-to-string {character}))) "
            "(prin1 (list (length s) (aref s 0) (string-bytes s) "
            "(multibyte-string-p s))))")
    for character in (2097152, 4194175, 4194176, 4194303):
        forms.append(
            f"(let* ((s (make-string 3 {character})) (x (concat \"a\" s \"z\"))) "
            "(prin1 (list (length x) (aref x 1) (aref x 3) "
            "(string-bytes (substring x 1 4)) (aref (substring x 2 3) 0))))")
    # Native CLI printer and Lisp serializer must both escape byte8 characters.
    for character in (4194176, 4194303):
        forms.append(f"(prin1 (char-to-string {character}))")
        forms.append(f"(princ (prin1-to-string (char-to-string {character})))")
    return forms


def conversion_forms():
    forms = []
    inputs = ("(unibyte-string)", "(unibyte-string 65)",
              "(unibyte-string 128 255)", "(unibyte-string 194 160)",
              "(unibyte-string 192 128)", "(unibyte-string 224 128 128)",
              "(unibyte-string 224 160 128)", "(unibyte-string 237 160 128)",
              "(unibyte-string 240 128 128 128)", "(unibyte-string 247 191 191 191)",
              "(unibyte-string 248 136 128 128 128)",
              "(unibyte-string 248 143 191 189 191)",
              "(unibyte-string 248 143 191 190 128)",
              "(unibyte-string 195)", "(unibyte-string 195 65)",
              "(unibyte-string 255 128)", "(char-to-string 4194303)",
              "(concat \"a\" (char-to-string 4194176))", "\"あ\"", "(char-to-string 255)")
    for name in ("string-to-multibyte", "string-as-multibyte",
                 "string-to-unibyte", "string-as-unibyte"):
        for value in inputs:
            forms.append(
                f"(let ((s {value})) (prin1 (condition-case e "
                f"(let ((r ({name} s))) (list (append r nil) (string-bytes r) "
                "(multibyte-string-p r) (eq r s))) (error e))))")
    results = ("(concat (unibyte-string 128 255) \"あ\")",
               "(concat \"あ\" (unibyte-string 128 255))",
               "(concat (list 2097152 4194175 4194176 4194303))",
               "(concat (vector 2097152 4194175 4194176 4194303))",
               "(format \"あ%s\" (unibyte-string 128 255))",
               "(format (unibyte-string 255 37 115) \"あ\")",
               "(format \"%s\" (unibyte-string 128 255))")
    for value in results:
        forms.append(f"(prin1 (condition-case e (let ((r {value})) "
                     "(list (append r nil) (string-bytes r) (multibyte-string-p r))) (error e)))")
    forms.extend(("(prin1 (condition-case e (append (unibyte-string 128 255) \"あ\" nil) (error e)))",
                  "(prin1 (condition-case e (vconcat (unibyte-string 128 255) \"あ\") (error e)))"))
    for operation in ("(concat s)", "(substring s 0 1)", "(substring s 0 0)",
                      "(copy-sequence s)", "(format \"%s\" s)", "(format s)",
                      "(string-to-unibyte s)", "(string-as-unibyte s)",
                      "(string-to-multibyte s)", "(string-as-multibyte s)"):
        forms.append("(let* ((s (string-to-multibyte (unibyte-string 65))) "
                     f"(r {operation})) (prin1 (list (append r nil) "
                     "(string-bytes r) (multibyte-string-p r) (eq s r))))")
    forms.append("(let ((s (string-to-multibyte (unibyte-string 65)))) "
                 "(garbage-collect) (aset s 0 66) "
                 "(prin1 (list (aref s 0) (multibyte-string-p s))))")
    for value in ("\"aあ\"", "\"abあ\"", "(concat \"a\" (char-to-string 4194176) \"あ\")"):
        forms.append(f"(prin1 (condition-case e (string-to-unibyte {value}) (error e)))")
    return forms


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--conversions", action="store_true",
                        help="Compare conversion and representation cases.")
    parser.add_argument("--preload", type=Path,
                        help="Load this Lisp provider on both runtimes.")
    options = parser.parse_args()
    unit = "conversion" if options.conversions else "storage"
    forms = conversion_forms() if unit == "conversion" else storage_forms()
    # One startup per runtime, preserving a distinct transcript row per case.
    # Two CLI actions suppress automatic final-value printing, without filtering
    # arbitrary transcript rows out of the comparison.
    source = "(progn " + " (terpri) ".join(forms) + " (terpri) nil)"
    started = time.monotonic()
    expected_rows = run([os.environ.get("EMACS", "emacs"), "-Q", "--batch"],
                        source, options.preload).splitlines()
    actual_rows = run([str(BINARY)], source, options.preload).splitlines()
    if len(expected_rows) != len(forms) or len(actual_rows) != len(forms):
        raise RuntimeError(f"Expected {len(forms)} rows, got GNU={len(expected_rows)} "
                           f"native={len(actual_rows)}; last native rows={actual_rows[-2:]!r}")
    failed = []
    for index, (expected, actual) in enumerate(zip(expected_rows, actual_rows), 1):
        if actual != expected:
            failed.append(index)
            print(f"case {index}: GNU={expected!r} native={actual!r}")
    print(f"character-{unit}: {len(forms) - len(failed)}/{len(forms)} identical; "
          f"elapsed={time.monotonic() - started:.3f}s; runtime-startups=2")
    return bool(failed)


if __name__ == "__main__":
    sys.exit(main())
