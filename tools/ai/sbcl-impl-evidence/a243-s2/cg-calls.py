#!/usr/bin/env python3
"""Count callgrind calls into the functions at the given addresses.

usage: cg-calls.py CALLGRIND_FILE HEXADDR [HEXADDR...]
Prints the summed `calls=` count of every call edge whose callee (cfn=) is a
function named by one of the addresses (callgrind prints ??? functions of a
stripped-symbol binary as 0x<16 hex digits>).
"""
import re, sys
path, addrs = sys.argv[1], {int(a, 16) for a in sys.argv[2:]}
names = {}          # compressed id -> name
total = 0
tgt = None
rx = re.compile(r'^c?fn=\((\d+)\)(?: (.*))?$')
def norm(n):
    try: return int(n, 16)
    except (ValueError, TypeError): return None
with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if line.startswith(("fn=", "cfn=")):
            m = rx.match(line)
            if not m: continue
            if m.group(2) is not None: names[m.group(1)] = m.group(2)
            if line.startswith("cfn="):
                tgt = norm(names.get(m.group(1)))
        elif line.startswith("calls="):
            if tgt in addrs: total += int(line.split()[0][6:])
            tgt = None
print(total)
