#!/usr/bin/env python3
"""List the functions of an authenticated prelink closure that have indirect control flow.

usage: integ-wh-f1-indirect.py SNAPSHOT DRIVER CLOSURE-DIR [--boundary]

SNAPSHOT     a build snapshot directory (target/integ/src-REV9) holding scripts/
DRIVER       the prelink driver script name, e.g. nelisp-native-compiler-f1-prelink.py
CLOSURE-DIR  the closure directory the build left, e.g. SNAPSHOT/target/standalone-compiler-f1-XXXXXX
--boundary   stop at the evaluator boundary (nl_apply_function*), as the F1 driver does

The prover (scripts/nelisp-native-rooted-prelink-closure.py) refuses a closure at the first
function with an indirect jump or call and names only that one.  This replays its walk,
reports every such function with the call chain from a root, and so shows what a jump
table, a `call-ptr' or a bad edge costs before the next build.  Needs capstone, like the
prover.  Read-only.
"""
import importlib.util
import sys
from pathlib import Path

from capstone import Cs, CS_ARCH_X86, CS_MODE_64, CS_GRP_CALL, CS_GRP_JUMP, CS_OP_IMM


def main(argv):
    if len(argv) < 4:
        print(__doc__)
        return 2
    snap, driver, closure = Path(argv[1]), argv[2], Path(argv[3])
    boundaries = {"nl_apply_function", "nl_apply_function_w"} if "--boundary" in argv[4:] else set()
    spec = importlib.util.spec_from_file_location("drv", snap / "scripts" / driver)
    drv = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(drv)
    owners = drv.prelink._owner.unit_owners(closure / "active-unit-metadata.json", closure)
    roots = list(drv.ROOTS)
    decoder = Cs(CS_ARCH_X86, CS_MODE_64)
    decoder.detail = True
    parent = {root: None for root in roots}
    seen, pending, bad = set(), list(roots), []
    while pending:
        name = pending.pop(0)
        if name in seen or name not in owners:
            continue
        seen.add(name)
        owner = owners[name]
        start, end = owner["start"], owner["end"]
        relocations = {r["offset"] - start: r for r in owner["relocations"]}
        indirect = False
        for ins in decoder.disasm(owner["text"][start:end], start):
            if not (ins.group(CS_GRP_CALL) or ins.group(CS_GRP_JUMP)):
                continue
            if len(ins.operands) != 1 or ins.operands[0].type != CS_OP_IMM:
                indirect = True
                continue
            relocation = relocations.get(ins.address - start + ins.imm_offset)
            target = relocation["symbol"] if relocation else owner["starts"].get(ins.operands[0].imm)
            if target and target not in seen and name not in boundaries:
                parent.setdefault(target, name)
                pending.append(target)
        if indirect:
            bad.append(name)
    total = sum(owners[n]["end"] - owners[n]["start"] for n in seen)
    print(f"{driver}: closure {len(seen)} functions, {total} B text; "
          f"indirect-control-flow functions: {len(bad)}")
    for name in bad:
        chain, cursor = [], name
        while cursor is not None:
            chain.append(cursor)
            cursor = parent.get(cursor)
        print("   ", " <- ".join(chain[:6]), "  [unit %s]" % owners[name]["unit"])
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
