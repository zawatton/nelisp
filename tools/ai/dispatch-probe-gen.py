#!/usr/bin/env python3
"""dispatch-probe-gen.py SRC TABLE LEAVES OPS-PREFIX: the opcode dispatch of the VM's fast loop as two
stand-alone probe functions, for tools/ai/dispatch-probe-driver.c.

  probe_tree  RAW        the comparison tree the fast loop dispatches on (Task 3 step 1, 17 leaves:
                         the 11 arms of the loop and the gaps between them, an optimal alphabetic
                         tree for a weighted op mix, 4.21 comparisons on average);
  probe_dense TBL RAW    the same classification written as a dense equality chain: the opcode
                         is first mapped to its leaf number by a 256-byte table, then a chain of
                         seventeen (= KIND K) tests on that local, which the x86-64 emitter lowers
                         to a jump table (nelisp-aot-compiler--x86-dense-if-plan, at least four
                         contiguous keys on one local);
  probe_nop   RAW        the loop overhead of the driver.

Writes SRC (one (seq (defun ...) ...) form for tools/ai/dispatch-probe-compile.el), TABLE (the
256 leaf numbers), LEAVES (the leaf list as JSON) and OPS-PREFIX-{iid,loop8,loop16,loop32,loop64}.bin
(opcode sequences drawn from the loop's weights: independent draws, and a fixed pattern of 8 to 64
ops repeated, as a real loop executes them).
"""
import json
import random
import sys

LEAVES = [[0, 5, "stackref", 120], [6, 7, "default", 0.5], [8, 13, "varref", 50], [14, 31, "default", 0.5], [32, 37, "call", 100], [38, 63, "default", 0.5], [64, 65, "carcdr", 40], [66, 82, "default", 0.5], [83, 92, "arith", 310], [93, 129, "default", 0.5], [130, 130, "goto", 250], [131, 134, "cgoto", 70], [135, 135, "return", 30], [136, 136, "discard", 60], [137, 137, "dup", 250], [138, 191, "default", 0.5], [192, 255, "const", 200]]
# (name, weight) per arm in the weighted mix: stackref 120, varref 50, call 100, carcdr 40, arith 310,
# goto 250, cgoto 70, return 30, discard 60, dup 250, const 200; the gaps weigh 0.5.
TREE = ["node", 93, ["node", 83, ["node", 32, ["node", 6, ["leaf", 0], ["node", 8, ["leaf", 1], ["node", 14, ["leaf", 2], ["leaf", 3]]]], ["node", 38, ["leaf", 4], ["node", 64, ["leaf", 5], ["node", 66, ["leaf", 6], ["leaf", 7]]]]], ["leaf", 8]], ["node", 137, ["node", 131, ["node", 130, ["leaf", 9], ["leaf", 10]], ["node", 135, ["leaf", 11], ["node", 136, ["leaf", 12], ["leaf", 13]]]], ["node", 138, ["leaf", 14], ["node", 192, ["leaf", 15], ["leaf", 16]]]]]


def emit(t, ind=0):
    pad = " " * ind
    if t[0] == "leaf":
        return f"{pad}{t[1]}"
    return f"{pad}(if (< raw {t[1]})\n{emit(t[2], ind + 2)}\n{emit(t[3], ind + 2)})"


def main(src, table_path, leaves_path, ops_prefix):
    n = len(LEAVES)
    chain = "-1"
    for k in reversed(range(n)):
        chain = f"(if (= kind {k}) {k} {chain})"
    text = ("(seq\n(defun probe_tree (raw)\n" + emit(TREE, 2) + ")\n"
            "(defun probe_dense (tbl raw)\n  (let* ((kind (ptr-read-u8 tbl raw)))\n    " + chain + "))\n"
            "(defun probe_nop (raw) raw))\n")
    open(src, "w").write(text)
    table = bytearray(256)
    for k, (lo, hi, _name, _w) in enumerate(LEAVES):
        for r in range(lo, hi + 1):
            table[r] = k
    open(table_path, "wb").write(bytes(table))
    json.dump(LEAVES, open(leaves_path, "w"))
    arms = [(lo, hi, w) for lo, hi, name, w in LEAVES if name != "default"]
    total = sum(w for _, _, w in arms)
    rnd = random.Random(7)

    def pick():
        x = rnd.random() * total
        for lo, hi, w in arms:
            if x < w:
                return rnd.randint(lo, hi)
            x -= w
        return arms[-1][1]

    count = 1 << 18
    open(f"{ops_prefix}-iid.bin", "wb").write(bytes(pick() for _ in range(count)))
    for length in (8, 16, 32, 64):
        pattern = [pick() for _ in range(length)]
        open(f"{ops_prefix}-loop{length}.bin", "wb").write(bytes(pattern * (count // length)))
    print(f"{n} leaves, 5 op sequences")


if __name__ == "__main__":
    main(*sys.argv[1:5])
