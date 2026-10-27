#!/usr/bin/env python3
"""lexbc S4 checks.
usage: s4check.py ir FILE      -- FILE has lines "tm-fts-13 ENGINE Ir=<n> binds=<n>" for v26, cand, gnu;
                                  passes iff cand binds <= gnu binds and cand Ir <= 0.70 * v26 Ir.
       s4check.py paired DIR   -- DIR/<row>/report.json from paired_bench.py (engines v26, cand, gnu);
                                  passes iff tm-fts-13 cand median < v26 median and reg-sum-bc, r3-mix
                                  cand median <= 1.03 * v26 median."""
import json, os, re, sys
mode, path = sys.argv[1], sys.argv[2]
ok = True
if mode == 'ir':
    v = {}
    for l in open(path):
        m = re.match(r'tm-fts-13 (\S+) Ir=([\d.]+) binds=([\d.]+)', l)
        if m: v[m.group(1)] = (float(m.group(2)), float(m.group(3)))
    ok = (v['cand'][1] <= v['gnu'][1]) and (v['cand'][0] <= 0.70 * v['v26'][0])
    print('IR', 'OK' if ok else 'RED', 'cand/v26 Ir=%.3f' % (v['cand'][0] / v['v26'][0]),
          'binds cand=%g gnu=%g v26=%g' % (v['cand'][1], v['gnu'][1], v['v26'][1]))
else:
    for row, lim in (('tm-fts-13', None), ('reg-sum-bc', 1.03), ('r3-mix', 1.03)):
        r = json.load(open(os.path.join(path, row, 'report.json')))
        e = r['engines']
        base, cand = e['v26']['inner_ns_per_step']['median'], e['cand']['inner_ns_per_step']['median']
        good = cand < base if lim is None else cand <= lim * base
        ok = ok and good
        print('PAIRED', row, 'OK' if good else 'RED', 'v26=%.1f cand=%.1f gnu=%.1f ratio=%.3f' %
              (base, cand, e['gnu']['inner_ns_per_step']['median'], cand / base))
sys.exit(0 if ok else 1)
