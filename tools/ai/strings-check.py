#!/usr/bin/env python3
"""Focused acceptance entry points for Doc 227; unfinished checks fail closed."""
from pathlib import Path
import argparse
import json
import re

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/strings"
CENSUS = {
    "transport": ["nl_word_clone", "wf_word_view", "nl_sexp_clone_into"],
    "alias": ["nl_sci_copy", "nl_sci_dispatch"],
    "allocation": ["nl_alloc_str_w", "nl_alloc_unibyte_str_w", "nelisp_sexp_write_str"],
    "copy": ["nl_alloc_str_copy_loop", "nl_mut_str_finalize_copy_loop", "nl_mut_str_finalize"],
    "read": ["nl_str_bytes_ptr", "nl_str_char_count"],
    "publication": ["m5_finish_string_w"],
}


def plan():
    doc = (ROOT / "docs/design/227-gnu-string-port.org").read_text()
    rows = []
    for path in sorted((ROOT / "lisp").glob("*.el")) + [ROOT / "scripts/nelisp-standalone-build.el"]:
        source = path.read_text()
        for family, names in CENSUS.items():
            for name in names:
                for match in re.finditer(r"\(defun\s+" + re.escape(name) + r"(?=\s|\()", source):
                    rows.append(dict(family=family, name=name, path=str(path.relative_to(ROOT)),
                                     line=source.count("\n", 0, match.start()) + 1))
    assert len(rows) == 14, f"census changed: {len(rows)} definitions; classify before accepting"
    for token in ["alloc.c:1729", "character.h:", "fns.c:", "nl_sweep_strings",
                  "nl_gc_mark_reset_all", "nl_gc_conserv_typed", "nl_dump_compact_write"]:
        assert token in doc, f"missing plan mechanism: {token}"
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "census.json").write_text(json.dumps(rows, indent=2) + "\n")
    print("PASS plan: GNU mechanism map, 14 classified definitions, GC integration contract")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("check", choices=["plan", "layout", "compaction", "semantics", "speed", "final"])
    parser.add_argument("workload", nargs="?", choices=["producers", "pressure"])
    args = parser.parse_args()
    if args.check == "plan":
        plan()
    else:
        raise SystemExit(f"FAIL {args.check}: implementation and acceptance evidence outstanding")


if __name__ == "__main__":
    main()
