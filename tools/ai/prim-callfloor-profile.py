#!/usr/bin/env python3
"""Resolve Callgrind self costs and subtract each operation's actual control.

Every instruction belongs to exactly one category. The self-cost sum must
equal Callgrind's summary before any differential is accepted.
"""
import argparse
import bisect
from collections import Counter
import importlib.util
import json
from pathlib import Path
import re
import subprocess

SPEC = importlib.util.spec_from_file_location("pcf", Path(__file__).with_name("prim-callfloor-check.py"))
C = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(C)


def symbols(binary):
    rows = []
    for line in subprocess.check_output(["nm", "-n", binary], text=True).splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[1] in ("t", "T"):
            rows.append((int(parts[0], 16), parts[2]))
    rows.sort()
    return [row[0] for row in rows], rows


def costs(path, table):
    addresses, names = table
    ids, result = {}, Counter()
    function, edge, summary = "?", False, None
    for line in Path(path).read_text().splitlines():
        if line.startswith("summary:"):
            summary = int(line.split()[1])
        match = re.match(r"(c?fn)=\((\d+)\)(?: (.+))?", line)
        if match:
            if match[3]:
                ids[match[2]] = match[3]
            if match[1] == "fn":
                function = ids.get(match[2], "unresolved-id:" + match[2])
                edge = False
                if function.startswith("0x"):
                    index = bisect.bisect_right(addresses, int(function.split("'")[0], 16)) - 1
                    if index >= 0:
                        function = names[index][1]
        elif line.startswith("calls="):
            edge = True
        elif re.match(r"^[+*\-0-9]", line):
            parts = line.split()
            if len(parts) == 2:
                if edge:
                    edge = False
                else:
                    result[function] += int(parts[-1])
    if summary is None or sum(result.values()) != summary:
        raise ValueError(f"self costs do not reconcile with summary: {path}")
    return result


def category(name):
    if any(s in name for s in ("nl_alloc", "nl_gc", "freelist", "os_alloc", "arena")):
        return "allocation/GC"
    if any(s in name for s in ("nl_root", "rootstack", "pin_")):
        return "GC roots"
    if any(s in name for s in ("mirror", "fnv", "symbol_key", "bytes_byte", "lookup", "frame_stack_find",
                               "intern", "symname", "symbol_name", "symbol_function")):
        return "lookup"
    if any(s in name for s in ("nl_bf_", "bind_formals", "cons_construct", "consbox", "list_copy", "word_list", "apply_list")):
        return "arguments/consing"
    if any(s in name for s in ("frame_", "nl_n3_bind", "nl_n3_pop", "nl_sb_", "bind_frame")):
        return "frames/binding"
    if any(s in name for s in ("finish", "_done", "release")):
        return "return"
    if any(s in name for s in ("eval", "apply", "m5_fx", "bytecode", "nl_val", "word_store", "clone", "cons_car", "cons_cdr")):
        return "dispatch/transport"
    return "other"


def call_edges(path, table):
    """Return native calls/Ir, resolving forward IDs and recursion contexts."""
    addresses, names = table
    lines = Path(path).read_text().splitlines()
    ids, result = {}, {}
    for line in lines:
        match = re.match(r"c?fn=\((\d+)\)(?: (.+))?", line)
        if match and match[2]:
            name = match[2]
            if name.startswith("0x"):
                address, _, context = name.partition("'")
                index = bisect.bisect_right(addresses, int(address, 16)) - 1
                if index >= 0:
                    name = names[index][1] + ("'" + context if context else "")
            ids[match[1]] = name
    caller = callee = "?"
    pending = None
    for line in lines:
        match = re.match(r"(c?fn)=\((\d+)\)", line)
        if match:
            if match[1] == "fn":
                caller = ids.get(match[2], "unresolved-id:" + match[2])
            else:
                callee = ids.get(match[2], "unresolved-id:" + match[2])
        elif line.startswith("calls="):
            pending = int(line.split()[0].split("=")[1])
        elif pending is not None and re.match(r"^[+*\-0-9]", line):
            row = result.setdefault((caller, callee), [0, 0])
            row[0] += pending
            row[1] += int(line.split()[-1])
            pending = None
    return result


def profile(tag):
    source = C.OUT / tag / "perf.json"
    report = json.loads(source.read_text())
    table = symbols(report["binary"]["path"])
    rows = []
    for run in report["runs"]:
        intervals = run["intervals"]
        zero = costs(intervals[0]["profile"]["path"], table)
        empty = costs(intervals[-2]["profile"]["path"], table)
        for operation in intervals[1:]:
            actual = costs(operation["profile"]["path"], table)
            control = zero if operation["op"] in ("while", "dotimes") else empty
            delta = {k: (actual[k] - control[k]) / report["n"] for k in actual.keys() | control.keys()}
            grouped = Counter()
            for name, ir in delta.items():
                grouped[category(name)] += ir
            rows.append({"mode": run["mode"], "layout": run["layout"],
                         "op": operation["op"], "Ir": sum(delta.values()),
                         "categories": dict(grouped),
                         "top_self": sorted(delta.items(), key=lambda row: -row[1])[:25]})
    output = {"inputs": [C.pin(source), report["binary"], C.pin(__file__)], "rows": rows}
    (C.OUT / tag / "profile.json").write_text(json.dumps(output, indent=2) + "\n")
    for mode in C.MODES:
        selected = [r for r in rows if r["mode"] == mode and r["op"] == "funcall"]
        print(mode, {k: round(sum(r["categories"].get(k, 0) for r in selected) / len(selected))
                     for k in sorted({k for r in selected for k in r["categories"]})})


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("tag")
    profile(p.parse_args().tag)
