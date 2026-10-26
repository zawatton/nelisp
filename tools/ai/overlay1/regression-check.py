#!/usr/bin/env python3
"""Fail when a no-overlay regression row of the paired timing run is more than 3 percent slower than v17.
usage: regression-check.py [TIMING_DIR]   (default target/overlay1/timing); reads <row>.log written by paired_bench."""
import re, sys
d = sys.argv[1] if len(sys.argv) > 1 else "target/overlay1/timing"
rows = ["reg-sum-bc", "r3-mix", "buffers-insert-str", "buffers-delete-region"]
bad = 0
for r in rows:
    try:
        text = open("%s/%s.log" % (d, r)).read()
    except OSError:
        print("%-22s no log" % r); bad += 1; continue
    m = re.search(r"^overlay1\s+\S+\s+\S+\s+\S+\s+([0-9.]+)\s", text, re.M)
    if not m:
        print("%-22s no overlay1 row" % r); bad += 1; continue
    ratio = float(m.group(1))
    print("%-22s overlay1/v17 = %.3f %s" % (r, ratio, "ok" if ratio <= 1.03 else "OVER 1.03"))
    bad += ratio > 1.03
sys.exit(1 if bad else 0)
