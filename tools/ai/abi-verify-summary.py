#!/usr/bin/env python3
"""Validate the ABI verifier's actual logs, including completion evidence."""
from pathlib import Path
import re
import sys


def verify(directory):
    directory = Path(directory)
    errors = []
    checks = 0

    def require(ok, name):
        nonlocal checks
        checks += 1
        if not ok:
            errors.append(name)

    def read(name):
        path = directory / name
        return path.read_text(errors="replace") if path.is_file() else ""

    summary = read("summary.txt")
    statuses = re.findall(r"^[abcd] .*?\brc=(\d+)", summary, re.M)
    require(len(statuses) == 23 and all(s == "0" for s in statuses),
            "23 successful commands")
    require("STANDALONE-CHECKS PASS" in read("a-standalone-checks.log"), "battery completion")
    markers = {
        "standalone-shallow-binding-check": r"SHALLOW-BINDING failures=0 PASS",
        "standalone-shallow-binding-dyn-check": r"SHALLOW-BINDING-DYN failures=0 PASS",
        "standalone-fx-forward-macro-check": r"FX-FORWARD-MACRO failures=0 PASS",
        "standalone-fx-late-special-check": r"FX-LATE-SPECIAL failures=0 PASS",
        "fx-spike-check": r"FX-SPIKE failures=0 PASS",
        "standalone-prn-string-escape-check": r"PRN-ESC cases=[1-9][0-9]* bad=0 PASS",
        "standalone-bytecode-word-check": r"BYTECODE-WORD PASS 145/145 cases match GNU",
        "standalone-abi-char-table-word-check": r"ABI-CHAR-TABLE-WORD checks=10 PASS",
        "standalone-abi-reader-control-word-check": r"ABI-READER-CONTROL-WORD checks=77 PASS",
        "standalone-abi-fx-result-word-check": r"ABI-FX-RESULT checked=44 PASS",
        "standalone-fx-call-shape-check": r"FX-CALL-SHAPE failures=0 PASS",
    }
    for name, marker in markers.items():
        require(bool(re.search(marker, read(f"b-{name}.log"))), name)
    require(bool(re.search(r"N4-REFERENCE checks=[1-9][0-9]* PASS", read("b-n4-reference.log"))),
            "GNU activation API parity")
    require("char-table legacy6 PASS" in read("b-char-table-legacy.log"), "physical char-table ABI")
    require("CHAR-TABLE-IMAGE checks=8 PASS" in read("b-char-table-image.log"), "char-table image relocation")
    require("physical/WORD clone13 PASS" in read("b-value-clone.log"), "physical value ownership and WORD identity")
    require("VM-INTEGER-WORD checks=104 PASS" in read("b-vm-integer-word.log"),
            "boxed/immediate VM integers and checked fallback")
    # Standalone adds a final Lisp nil echo to this otherwise identical corpus.
    actual = read("b-standalone-cl-loop-gnu-check.log").rstrip()
    expected = read("b-cl-loop-gnu.emacs.out").rstrip()
    if actual.endswith("\nnil"):
        actual = actual[:-4].rstrip()
    require(bool(expected) and actual == expected, "GNU cl-loop output")
    for name in ("standalone", "allocating-standalone", "percpu-roots", "mirror-guard"):
        require(f"nelisp-thread-{name}-smoke: PASS" in read(f"c-{name}.log"), name)
    anvil = read("d-anvil.log")
    require(all(re.search(rf"^{n} PASS", anvil, re.M) for n in (1, 2, 3, 4))
            and "tools=40" in anvil, "Anvil replies and registration count")
    require(bool(re.search(r"^e F1 build proof PRELINK_F1_BOUNDARY_PASS records [1-9][0-9]*", summary, re.M)),
            "F1 build certificate")
    runtime = re.findall(r"^e F1 runtime .*", summary, re.M)
    require(len(runtime) == 2 and all("(t t)" in line for line in runtime), "F1 runtime features")
    for error in errors:
        print("ABI-VERIFY FAIL", error)
    print(f"ABI-VERIFY checks={checks} failures={len(errors)} {'FAIL' if errors else 'PASS'}")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(verify(sys.argv[1]))
