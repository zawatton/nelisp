#!/usr/bin/env python3
"""Checks for tools/ai/syntax-rt-progress.org (Doc 225).

Usage:
  syntax-rt-check.py plan
  syntax-rt-check.py semantics
  syntax-rt-check.py speed char-syntax|string-match
  syntax-rt-check.py final

Binaries: SYNTAX_RT_DYN / SYNTAX_RT_STATIC (default target/syntax-rt/bin/
nelisp-dyn and nelisp-static), SYNTAX_RT_BASE (default the integration-D
dynamic base).  GNU: /usr/local/bin/emacs (31.1).
"""

import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "docs/design/225-syntax-and-match-entry-gnu-parity.org"
GNU_SRC = Path("/home/madblack-21/Downloads/emacs-31.1/src")
GNU_BIN = "/usr/local/bin/emacs"
EVIDENCE = ROOT / "target/syntax-rt"
BASE_BIN = Path(os.environ.get(
    "SYNTAX_RT_BASE",
    ROOT.parent / "integ-20261021d/target/integ/nelisp-dyn"))
DYN_BIN = Path(os.environ.get("SYNTAX_RT_DYN", EVIDENCE / "bin/nelisp-dyn"))
STATIC_BIN = Path(os.environ.get("SYNTAX_RT_STATIC", EVIDENCE / "bin/nelisp-static"))
CPUS = os.environ.get("SYNTAX_RT_CPU", "10")
MEMORY_LIMIT_KB = 8388608


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def fail(message):
    print(f"SYNTAX-RT FAIL: {message}")
    return 1


# ---------------------------------------------------------------- plan

SECTIONS = ("char-syntax ?a", "char-syntax ?(", 'string-match "\\\\sw+"',
            'string-match "alpha"')


def doc_tables(text):
    """Return {section-heading: [row-cells...]} for the §1 owner tables."""
    tables, current = {}, None
    for line in text.splitlines():
        if line.startswith("** "):
            current = line[3:].strip()
            continue
        if current and line.startswith("|") and not line.startswith("|-"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if cells and cells[0] != "Owner":
                tables.setdefault(current, []).append(cells)
    return tables


def gnu_reference_ok(ref, token, shift=0):
    match = re.fullmatch(r"([\w.-]+\.[ch]):(\d+)", ref)
    if not match:
        return False
    source = GNU_SRC / match.group(1)
    if not source.is_file():
        return False
    lines = source.read_text(errors="replace").split("\n")
    line = int(match.group(2)) + shift
    window = lines[max(0, line - 4):line + 3]
    return any(token in candidate for candidate in window)


def check_plan():
    if not DOC.is_file():
        return fail(f"missing {DOC.relative_to(ROOT)}")
    text = DOC.read_text()
    tables = doc_tables(text)
    problems, refs = [], 0
    for wanted in SECTIONS:
        heading = next((h for h in tables if h.startswith(wanted)), None)
        if heading is None:
            problems.append(f"no owner table for {wanted}")
            continue
        total = 0
        for cells in tables[heading]:
            if len(cells) != 7:
                problems.append(f"{wanted}: row has {len(cells)} cells")
                continue
            owner, micros, share, mechanism, ref, token, port = cells
            if not re.fullmatch(r"[\d,]+", micros):
                problems.append(f"{wanted}: bad us/call {micros!r}")
            if not re.fullmatch(r"\d+%", share):
                problems.append(f"{wanted}: bad share {share!r}")
            else:
                total += int(share[:-1])
            if not gnu_reference_ok(ref, token):
                problems.append(f"{wanted}: {ref} does not show {token!r}")
            refs += 1
            if not re.match(r"(ported|adapted|kept|not ported)", port):
                problems.append(f"{wanted}: port decision {port!r}")
        if not 95 <= total <= 105:
            problems.append(f"{wanted}: shares sum to {total}%")
    # Negative control: the same references, shifted 40 lines, must fail.
    shifted_ok = sum(gnu_reference_ok(cells[4], cells[5], 40)
                     for rows in tables.values() for cells in rows
                     if len(cells) == 7)
    if shifted_ok:
        problems.append(f"negative control: {shifted_ok} shifted references still match")
    # The measured runs the shares come from.
    for name in ("split-base-1.txt", "split-base-2.txt"):
        run = EVIDENCE / "s0" / name
        if not run.is_file() or "(char-syntax 97) total" not in run.read_text():
            problems.append(f"missing measurement {run}")
    if BASE_BIN.is_file() and sha256(BASE_BIN) not in text:
        problems.append("base binary sha256 not cited")
    for word in ("§2", "§3", "§4", "codegen", "callpath"):
        if word not in text:
            problems.append(f"doc lacks {word}")
    if problems:
        for problem in problems:
            print(f"  {problem}")
        return fail(f"plan: {len(problems)} problem(s)")
    print(f"SYNTAX-RT plan PASS: {len(SECTIONS)} owner tables, {refs} GNU references, "
          f"negative control rejected all shifted references")
    return 0


def main(argv):
    if argv[1:2] == ["plan"]:
        return check_plan()
    print(__doc__.strip())
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
