#!/usr/bin/env python3
"""Validate Doc 211 S3.1's measured bytecomp API need list."""
import csv
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
TSV = ROOT / "tools/ai/doc211-bytecomp-need.tsv"
SOURCES = [ROOT / "vendor/emacs-lisp/emacs-lisp" / (n + ".el")
           for n in ("bytecomp", "byte-opt", "cconv", "macroexp", "byte-run")]
SOURCES += [ROOT / n for n in (
    "test/nelisp-eln-s6-measure.sh", "test/nelisp-eln-s6-measure-driver.el",
    "tools/nelisp-eln-s610-evidence.el")]

def static_vars():
    texts = "\n".join(p.read_text(errors="replace") for p in SOURCES)
    prefix = r"(?:buffer|mark|syntax|default-directory|case-fold|inhibit-read-only)"
    candidates = set(re.findall(r"\(defvar\s+(" + prefix + r"[A-Za-z0-9-]*)", texts))
    candidates |= set(re.findall(r"\b(?:setq|setq-default)\s+(" + prefix + r"[A-Za-z0-9-]*)", texts))
    candidates |= set(re.findall(r"\((?:let|let\*)\s+\(\(\s*(" + prefix + r"[A-Za-z0-9-]*)", texts))
    return sorted(candidates)

def main():
    if len(sys.argv) != 2 or sys.argv[1] != "check":
        raise SystemExit("usage: doc211-bytecomp-needs.py check")
    errors = []
    with TSV.open(newline="") as f:
        rows = list(csv.DictReader((line for line in f if not line.startswith("#")), delimiter="\t"))
    seen = set()
    for row in rows:
        key = (row["name"], row["kind"])
        if key in seen: errors.append(f"duplicate {key}")
        seen.add(key)
        if row["kind"] == "function" and row["evidence"] != "runtime":
            errors.append(f"function lacks runtime evidence: {row['name']}")
        if row["kind"] == "function":
            match = re.fullmatch(r"standalone=(\d+);host=(\d+)", row["count"])
            if not match or int(match.group(1)) + int(match.group(2)) == 0:
                errors.append(f"function lacks positive numeric runtime counts: {row['name']}")
        if row["kind"] == "variable" and row["evidence"] != "static":
            errors.append(f"variable lacks static evidence: {row['name']}")
    actual = set(static_vars())
    recorded = {r["name"] for r in rows if r["kind"] == "variable"}
    if actual != recorded:
        errors.append(f"static variable mismatch missing={sorted(actual-recorded)} extra={sorted(recorded-actual)}")
    if errors:
        print("S3.1 FAIL: " + "; ".join(errors)); return 1
    print(f"S3.1 PASS: {sum(r['kind']=='function' for r in rows)} runtime functions; {len(actual)} static variables")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
