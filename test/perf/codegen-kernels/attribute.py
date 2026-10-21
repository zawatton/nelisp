#!/usr/bin/env python3
"""Attribute a kernel's per-iteration Ir to instruction categories.

Usage: attribute.py BINARY [N]

Runs BINARY under callgrind (--dump-instr=yes) with N and with 0 iterations
(layout 0), takes the per-instruction Ir difference divided by N, classifies
each executed instruction from objdump's decoding and prints only category
totals (never the instructions themselves).
"""
import collections
import os
import re
import subprocess
import sys
import tempfile


def self_costs(cg_path):
    """Map instruction address -> self Ir from a callgrind --dump-instr file."""
    costs = collections.Counter()
    last = 0
    skip_next = False
    npos = 1
    for line in open(cg_path):
        line = line.rstrip("\n")
        if not line or line[0].isalpha():
            if line.startswith("positions:"):
                npos = len(line.split()) - 1
            if line.startswith("calls="):
                skip_next = True
            continue
        parts = line.split()
        pos = parts[0]
        if pos.startswith("0x"):
            addr = int(pos, 16)
        elif pos.startswith("+"):
            addr = last + int(pos[1:], 16 if pos[1:].startswith("0x") else 10)
        elif pos.startswith("-"):
            addr = last - int(pos[1:], 16 if pos[1:].startswith("0x") else 10)
        elif pos == "*":
            addr = last
        else:
            continue
        last = addr
        if skip_next:
            skip_next = False
            continue
        if len(parts) > npos:
            costs[addr] += int(parts[npos])
    return costs


def run_cg(binary, n, out):
    subprocess.run(["valgrind", "--tool=callgrind", "--dump-instr=yes",
                    f"--callgrind-out-file={out}", binary],
                   input=f"{n}\n", capture_output=True, text=True)
    return self_costs(out)


def decode(binary):
    text = subprocess.run(["objdump", "-d", "-M", "intel", "--no-show-raw-insn", binary],
                          capture_output=True, text=True, check=True).stdout
    insns = {}
    for line in text.splitlines():
        m = re.match(r"\s*([0-9a-f]+):\s+(\S+)\s*(.*)$", line)
        if m:
            insns[int(m.group(1), 16)] = (m.group(2), m.group(3).split("#")[0].strip())
    return insns


def classify(mnem, ops):
    frame = "[rbp" in ops
    if mnem in ("push", "pop"):
        return "redundant loads/stores: stack temporaries (push/pop)"
    if mnem.startswith("set") or (mnem == "movzx" and ops.endswith(",al")):
        return "branch structure: boolean materialization"
    if mnem in ("cmp", "test") and re.match(r"rax,\s*(0x0|0)$", ops):
        return "branch structure: boolean materialization"
    if mnem == "jmp":
        return "branch structure: unconditional jumps"
    if mnem == "mov" and frame:
        return "redundant loads/stores: frame slots"
    if mnem == "mov" and re.match(r"r\w+,\s*r\w+$", ops):
        return "operand shuffles: register moves"
    if mnem == "mov" and re.match(r"(r|e)\w+,\s*(0x[0-9a-f]+|\d+)$", ops):
        return "operand shuffles: immediate loads"
    if mnem in ("and", "or", "xor", "test", "sar", "shl", "shr", "add", "sub",
                "imul", "lea", "inc", "dec", "neg"):
        return "work: arithmetic/logic"
    if mnem in ("cmp",) or mnem.startswith("j") or mnem.startswith("cmov"):
        return "work: compare/branch"
    if mnem in ("mov", "movzx", "movsx", "movsxd"):
        return "work: memory access"
    return "other: " + mnem


def main():
    binary = sys.argv[1]
    n = int(sys.argv[2]) if len(sys.argv) > 2 else 100000
    with tempfile.TemporaryDirectory() as tmp:
        hi = run_cg(binary, n, os.path.join(tmp, "n.cg"))
        lo = run_cg(binary, 0, os.path.join(tmp, "z.cg"))
    insns = decode(binary)
    cats = collections.Counter()
    unknown = 0.0
    for addr in set(hi) | set(lo):
        per = (hi[addr] - lo[addr]) / n
        if abs(per) < 1e-6:
            continue
        if addr in insns:
            cats[classify(*insns[addr])] += per
        else:
            unknown += per
    total = sum(cats.values()) + unknown
    for cat, v in sorted(cats.items(), key=lambda kv: -kv[1]):
        print(f"{v:8.3f}  {cat}")
    if unknown:
        print(f"{unknown:8.3f}  outside the decoded text")
    print(f"{total:8.3f}  total Ir/iter")


if __name__ == "__main__":
    main()
