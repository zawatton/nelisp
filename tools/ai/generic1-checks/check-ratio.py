#!/usr/bin/env python3
"""check-ratio.py LOG ENGINE MAX -- PASS iff the paired_bench table in LOG has ENGINE's ratio <= MAX."""
import sys
log, eng, mx = sys.argv[1], sys.argv[2], float(sys.argv[3])
for line in open(log):
    p = line.split()
    if p and p[0] == eng and len(p) >= 6:
        r = float(p[4]); print(f"{eng} ratio {r} (limit {mx})"); sys.exit(0 if r <= mx else 1)
print("engine row not found"); sys.exit(2)
