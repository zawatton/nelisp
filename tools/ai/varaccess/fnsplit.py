#!/usr/bin/env python3
"""fnsplit.py NM CG2000 CG8000 [BASE2000 BASE8000]: per-function Ir per trip (8000 minus 2000, /6000), by nm symbol."""
import sys, re, bisect, subprocess, collections
nm = sorted((int(l.split()[0], 16), l.split()[2]) for l in open(sys.argv[1]) if len(l.split()) == 3 and l.split()[1] in 'Tt')
addrs = [a for a, _ in nm]
def sym(ad):
    i = bisect.bisect_right(addrs, ad) - 1
    return nm[i][1] if i >= 0 else '?'
def prof(cg):
    out = subprocess.run(['callgrind_annotate', cg], capture_output=True, text=True).stdout
    d = collections.Counter()
    for l in out.splitlines():
        m = re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+\?\?\?:(0x[0-9a-f]+)', l)
        if m: d[sym(int(m[2], 16))] += int(m[1].replace(',', ''))
        else:
            m = re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+\S+:(\S+)', l)
            if m: d[m[2].split("'")[0]] += int(m[1].replace(',', ''))
    return d
a, b = prof(sys.argv[2]), prof(sys.argv[3])
res = {k: (b[k] - a.get(k, 0)) / 6000 for k in b}
if len(sys.argv) > 5:
    c, e = prof(sys.argv[4]), prof(sys.argv[5])
    for k in e: res[k] = res.get(k, 0) - (e[k] - c.get(k, 0)) / 6000
tot = sum(res.values())
print(f'total {tot:.0f}')
for k, v in sorted(res.items(), key=lambda x: -abs(x[1]))[:14]:
    if abs(v) >= 1: print(f'  {v:8.1f} {k}')
