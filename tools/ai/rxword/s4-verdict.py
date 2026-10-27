#!/usr/bin/env python3
"""S4 verdict from the recorded paired_bench reports (target/rxword/evidence/s4-*.json).
Checks: hl row and slow-layout far row at least 10x faster than the v25 baseline, fast-layout far row
not slower, regression rows reg-sum-bc / r3-mix within 3% of v25.  Exit 0 iff all hold."""
import json, os, sys
D = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', '..', 'target', 'rxword', 'evidence')
def med(row, eng):
    r = json.load(open(os.path.join(D, f's4-{row}.json')))['engines'][eng]['inner_ns_per_step']
    return r['median']
ok = True
def check(name, cond, detail):
    global ok
    print(('PASS ' if cond else 'FAIL ') + name + ' ' + detail); ok = ok and cond
for row, need in (('hl-row', 10), ('far-slow', 10)):
    v, b, g = med(row, 'v25'), med(row, 'b2'), med(row, 'gnu')
    check(f'{row} speedup>={need}x', v / b >= need, f'v25={v:.0f}ns b2={b:.0f}ns gnu={g:.0f}ns speedup={v/b:.1f}x vs-gnu={b/g:.1f}x')
v, b, g = med('far-fast', 'v25'), med('far-fast', 'b2'), med('far-fast', 'gnu')
check('far-fast not slower', b <= v * 1.03, f'v25={v:.0f}ns b2={b:.0f}ns gnu={g:.0f}ns')
for row in ('reg-sum-bc', 'r3-mix'):
    v, b = med(row, 'v25'), med(row, 'b2')
    check(f'{row} within 3% of v25', abs(b / v - 1) <= 0.03, f'v25={v:.2f}ns b2={b:.2f}ns ratio={b/v:.4f}')
sys.exit(0 if ok else 1)
