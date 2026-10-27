#!/usr/bin/env python3
"""matrix-agg.py OUTDIR > matrix.txt : cand/v28 per row; MATRIX-OK when no row is slower than 1.03x."""
import json, os, statistics, sys
HERE = os.path.dirname(os.path.abspath(__file__)); ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
M = os.path.dirname(ROOT) + '/resume-4943-20261024/target/integ-1045/matrix'
specs = json.load(open(M + '/specs.json')); d = sys.argv[1]; rows = []; miss = []
for sp in specs:
    p = f"{d}/{sp['id']}/report.json"
    if not os.path.exists(p): miss.append(sp['id']); continue
    e = json.load(open(p))['engines']; m = {k: v['inner_ns_per_step']['median'] for k, v in e.items()}
    rows.append((sp['id'], m, e['cand']['inner_ns_per_step'].get('pairs_faster_than_v28')))
print('# regalloc matrix: ns/trip, median of 5 paired reps (v28, cand, GNU alternating), CPU 13\n')
print('| workload | v28 | cand | gnu | cand/v28 | cand/GNU | cand faster |')
slow = []
for i, m, f in sorted(rows, key=lambda r: -r[1]['cand'] / r[1]['v28']):
    r = m['cand'] / m['v28']
    if r > 1.03: slow.append(i)
    print(f"| {i} | {m['v28']:.1f} | {m['cand']:.1f} | {m['gnu']:.1f} | {r:.3f} | {m['cand']/m['gnu']:.2f} | {f}/5 |")
print(f"\ngeomean cand/v28 {statistics.geometric_mean([m['cand']/m['v28'] for _, m, _ in rows]):.3f} rows {len(rows)}")
for x in miss: print('MISSING', x)
print('SLOWER-THAN-1.03:', ' '.join(slow) or 'none')
if not miss and not slow and len(rows) == len(specs): print('MATRIX-OK')
