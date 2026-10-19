#!/usr/bin/env python3
"""Reject reader WORD entries that recurse through the old OUT-slot ABI."""
import json
import sys

units = {}
for line in open(sys.argv[1], encoding="utf-8"):
    d = json.loads(line)
    units[d["unit"]] = d["source"]


def walk(x):
    if isinstance(x, list) and x and x[0] not in ("quote", "$str", "$vec"):
        yield x
        for y in x:
            yield from walk(y)


defs = {x[1]: x for x in walk(units["reader-parser.o"]) if x[0] == "defun"}
legacy = {name for name in defs if name + "_w" in defs}
checked = 0
failures = []
for name, fn in defs.items():
    if name.endswith("_w"):
        checked += 1
        for x in walk(fn[3:]):
            head = x[1] if x[0] == "extern-call" else x[0]
            if isinstance(head, str) and (head in legacy or head == "nl_abi_rc_to_word"):
                failures.append((name, head))
for name, callee in failures:
    print(f"{name}: boxed producer {callee}")
print(f"ABI-READER-WORD checked={checked} findings={len(failures)}")
sys.exit(0 if checked >= 20 and not failures else 1)
