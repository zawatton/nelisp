#!/usr/bin/env python3
"""check-sweep.py swing|stable SWEEP.json [--limit 2.0] [--stat median|min] [--only PREFIX]

SWEEP.json is a meas.py result (keys "ROW|ENGINE@0"): one engine per padded relink of the
same compiled units.  Per row the metric is cycles per trip (r7-maphash-bc: elapsed ns per
trip, whose 2-N cycle difference is dominated by GC/startup noise) and the spread is
max/min - 1 over the engines' medians (--stat min: over each engine's best repetition, which
interference from other processes can only make slower); instructions per trip must agree within
0.5%.  --only PREFIX keeps the engines whose label starts with PREFIX.
  swing   exit 0 iff some row spreads by more than LIMIT % at equal instructions
  stable  exit 0 iff the rows of ROWS (the six swing rows) are all present and
          none spreads by more than LIMIT %, at equal instructions
Prints one line per row: spread, metric range, instruction range, the op-cache miss share
range and its correlation with the metric over the engines.
"""
import json, sys

ROWS = ['s6-nrev', 'r4-car', 'r4-nth', 'r7-maphash-bc', 's6-cons1', 's6-concat']


def corr(a, b):
    n = len(a)
    if n < 3:
        return float('nan')
    ma, mb = sum(a) / n, sum(b) / n
    va = sum((x - ma) ** 2 for x in a); vb = sum((y - mb) ** 2 for y in b)
    if va == 0 or vb == 0:
        return float('nan')
    return sum((x - ma) * (y - mb) for x, y in zip(a, b)) / (va * vb) ** 0.5


def main():
    mode, path = sys.argv[1], sys.argv[2]
    limit = float(sys.argv[sys.argv.index('--limit') + 1]) if '--limit' in sys.argv else 2.0
    stat = sys.argv[sys.argv.index('--stat') + 1] if '--stat' in sys.argv else 'median'
    only = sys.argv[sys.argv.index('--only') + 1] if '--only' in sys.argv else ''
    d = json.load(open(path))
    rows = {}
    for v in d.values():
        if not v['key'].startswith(only):
            continue
        rows.setdefault(v['row'], []).append(v)
    ok_swing = False; ok_stable = all(r in rows for r in ROWS)
    for row in sorted(rows):
        vs = rows[row]
        key = 'elapsed_ns' if row == 'r7-maphash-bc' else 'cycles'
        m = [v['median'][key] if stat == 'median' else v['range'][key][0] for v in vs]
        ins = [v['median']['instr'] for v in vs]
        spread = (max(m) / min(m) - 1) * 100
        ins_ok = max(ins) / min(ins) - 1 <= 0.005 and all(v['same'] for v in vs)
        oc = [v['median'].get('ocmiss', 0) / v['median']['cycles'] * 100 for v in vs]
        print(f'{row:14s} n={len(vs):2d} {stat} {key}: spread={spread:5.1f}% [{min(m):.1f},{max(m):.1f}] '
              f'instr [{min(ins):.1f},{max(ins):.1f}] eq={ins_ok} ocmiss% [{min(oc):.1f},{max(oc):.1f}] '
              f'corr={corr(oc, m):+.2f}')
        if ins_ok and spread > limit:
            ok_swing = True
        if row in ROWS and (not ins_ok or spread > limit):
            ok_stable = False
    ok = ok_swing if mode == 'swing' else ok_stable
    print(f'{mode}: {"PASS" if ok else "FAIL"} (limit {limit}%, {stat}{", engines " + only + "*" if only else ""})')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
