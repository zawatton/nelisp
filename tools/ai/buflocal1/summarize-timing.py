#!/usr/bin/env python3
"""summarize-timing.py ROWS.jsonl OUT.md : table of v17 / candidate / GNU ns per step and ratios."""
import json, sys
rows = {}
for l in open(sys.argv[1]):
    r = json.loads(l); rows[r['id']] = r
out = ['| row | v17 ns | cand ns | GNU ns | v17/GNU | cand/GNU | cand vs v17 |', '|---|--:|--:|--:|--:|--:|--:|']
def f(x): return '-' if x is None else ('%.0f' % x if x >= 100 else '%.1f' % x)
for i, r in rows.items():
    if r.get('status') != 'ok':
        out.append('| %s | %s | | | | | |' % (i, r.get('status'))); continue
    v, c, g = r.get('v17_ns'), r.get('cand_ns'), r.get('gnu_ns')
    out.append('| %s | %s | %s | %s | %s | %s | %s |' % (i, f(v), f(c), f(g), f(v / g if v else None), f(c / g), ('%.2fx' % (c / v)) if v else 'n/a (v17 result differs from GNU)'))
open(sys.argv[2], 'w').write('\n'.join(out) + '\n')
# regression rows: candidate must stay within 10% of v17
ok = True
for i in ('reg-sum-bc', 'r3-mix'):
    r = rows.get(i)
    if not r or r.get('status') != 'ok' or not r.get('v17_ns') or r['cand_ns'] / r['v17_ns'] > 1.10:
        ok = False
print('regression rows within 10%%: %s' % ok)
sys.exit(0 if ok else 1)
