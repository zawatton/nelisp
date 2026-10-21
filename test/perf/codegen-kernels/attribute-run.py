#!/usr/bin/env python3
"""Instruction-category Ir of a whole NeLisp run (no listing printed).

Usage: attribute-run.py BINARY CALLGRIND_OUT [TOP [CATEGORY-REGEXP]]

CALLGRIND_OUT comes from `valgrind --tool=callgrind --dump-instr=yes'.
Every executed instruction inside BINARY's executable segment is decoded
with capstone from the file bytes and bucketed as in attribute.py; the
script prints category totals and shares, and with TOP > 0 the TOP
functions (callgrind fn names, else the nearest ELF symbol) by self Ir,
restricted to the categories matching CATEGORY-REGEXP when given.
"""
import collections
import re
import sys

import capstone
from elftools.elf.elffile import ELFFile

sys.path.insert(0, __file__.rsplit("/", 1)[0])
from attribute import classify  # noqa: E402


def costs_by_addr(path):
    costs = collections.Counter()
    fn_costs = collections.Counter()
    fn_of = {}
    last = 0
    npos = 1
    skip = False
    fn = None
    names = {}
    for line in open(path):
        line = line.rstrip("\n")
        if not line:
            continue
        if line[0].isalpha():
            if line.startswith("positions:"):
                npos = len(line.split()) - 1
            elif line.startswith("calls="):
                skip = True
            elif line.startswith(("fn=", "cfn=")):
                m = re.match(r"c?fn=\((\d+)\)(?: (.*))?", line)
                if m:
                    if m.group(2):
                        names[m.group(1)] = m.group(2)
                    if line.startswith("fn="):
                        fn = names.get(m.group(1), m.group(1))
            continue
        parts = line.split()
        pos = parts[0]
        if pos.startswith("0x"):
            addr = int(pos, 16)
        elif pos[0] in "+-":
            delta = int(pos[1:], 16 if pos[1:].startswith("0x") else 10)
            addr = last + delta if pos[0] == "+" else last - delta
        elif pos == "*":
            addr = last
        else:
            continue
        last = addr
        if skip:
            skip = False
            continue
        if len(parts) > npos:
            c = int(parts[npos])
            costs[addr] += c
            fn_costs[fn] += c
            fn_of[addr] = fn
    return costs, fn_costs, fn_of


def main():
    binary, cg = sys.argv[1], sys.argv[2]
    top = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    cat_re = re.compile(sys.argv[4]) if len(sys.argv) > 4 else None
    costs, fn_costs, fn_of = costs_by_addr(cg)
    cat_fn = collections.Counter()
    with open(binary, "rb") as f:
        elf = ELFFile(f)
        segs = [(s["p_vaddr"], s["p_filesz"], s["p_offset"])
                for s in elf.iter_segments()
                if s["p_type"] == "PT_LOAD" and s["p_flags"] & 1]
        f.seek(0)
        data = f.read()
    md = capstone.Cs(capstone.CS_ARCH_X86, capstone.CS_MODE_64)
    md.syntax = capstone.CS_OPT_SYNTAX_INTEL
    cats = collections.Counter()
    outside = 0
    for addr, c in costs.items():
        for vaddr, size, off in segs:
            if vaddr <= addr < vaddr + size:
                code = data[off + addr - vaddr: off + addr - vaddr + 16]
                insn = next(md.disasm(code, addr), None)
                if insn is None:
                    cats["undecodable"] += c
                else:
                    ops = re.sub(r"qword ptr |dword ptr |word ptr |byte ptr ", "", insn.op_str)
                    ops = ops.replace(", ", ",")
                    cat = classify(insn.mnemonic, ops)
                    cats[cat] += c
                    if cat_re and cat_re.search(cat):
                        cat_fn[fn_of.get(addr)] += c
                break
        else:
            outside += c
    total = sum(cats.values()) + outside
    for cat, v in sorted(cats.items(), key=lambda kv: -kv[1]):
        print(f"{v:14d} {100.0 * v / total:6.2f}%  {cat}")
    print(f"{outside:14d} {100.0 * outside / total:6.2f}%  outside the binary's text")
    print(f"{total:14d}         total Ir")
    if top:
        syms = []
        with open(binary, "rb") as f:
            symtab = ELFFile(f).get_section_by_name(".symtab")
            if symtab:
                syms = sorted((s["st_value"], s.name) for s in symtab.iter_symbols()
                              if s["st_value"] and s.name)
        addrs = [a for a, _ in syms]

        def label(name):
            m = re.match(r"0x([0-9a-f]+)", name or "")
            if m and syms:
                import bisect
                i = bisect.bisect_right(addrs, int(m.group(1), 16)) - 1
                if i >= 0:
                    return syms[i][1]
            return name
        source = cat_fn if cat_re else fn_costs
        for name, v in source.most_common(top):
            print(f"{v:14d} {100.0 * v / total:6.2f}%  fn {label(name)}")


if __name__ == "__main__":
    main()
