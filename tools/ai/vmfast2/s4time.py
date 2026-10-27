#!/usr/bin/env python3
"""s4time.py PAIRED.TXT ROW [LIMIT] : exit 0 iff ROW's vmfast2/v30 median ratio in PAIRED.TXT
(lines 'ROW <row> ratio=<r> faster=<k>/<n>', written by agg.py) is at most LIMIT (default 0.75,
i.e. at least 25 percent less time than v30)."""
import sys, re
path, row = sys.argv[1], sys.argv[2]; lim = float(sys.argv[3]) if len(sys.argv) > 3 else 0.75
try: lines = open(path).read().splitlines()
except OSError: print('no', path); sys.exit(1)
for l in lines:
    m = re.match(r'ROW (\S+) ratio=([0-9.]+) faster=(\d+)/(\d+)', l)
    if m and m.group(1) == row:
        r = float(m.group(2)); ok = r <= lim
        print(f'{row} ratio {r:.3f} limit {lim} {"OK" if ok else "SHORT"}'); sys.exit(0 if ok else 1)
print(row, 'missing'); sys.exit(1)
