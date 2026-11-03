#!/usr/bin/env python3
"""Evaluate an R4 campaign directory against protocol-frozen.json. Exit 0 iff complete and consistent.
Prints medians; with --claim also requires the frozen GC claim to hold."""
import re, statistics, sys, json
from pathlib import Path
d = Path(sys.argv[1]); claim = '--claim' in sys.argv
rows = [dict(re.findall(r'(\w+)=(\S+)', l)) for l in (d/'raw.txt').read_text().splitlines()]
err = []
gc = {v: [int(r['median_ns']) for r in rows if r['row'] == 'gc-pause' and r['variant'] == v] for v in ('none', 'vector', 'region')}
for v, xs in gc.items():
    if len(xs) != 7: err.append(f'gc {v}: {len(xs)} samples')
if any(r['check'] != '1023' for r in rows if r['row'] == 'gc-pause'): err.append('gc check value')
wa = {}
for v in ('vector', 'region'):
    per = []
    for rep in map(str, range(7)):
        s = {r['n']: r for r in rows if r['row'] == 'word-access' and r['variant'] == v and r['rep'] == rep}
        if set(s) != {'262144', '1048576'}: err.append(f'wa {v} rep {rep} incomplete'); continue
        exp = {n: n*(n-1)//2 for n in (262144, 1048576)}
        if any(int(s[str(n)]['sum']) != exp[n] for n in exp): err.append(f'wa {v} rep {rep} wrong sum')
        per.append((int(s['1048576']['inner_ns']) - int(s['262144']['inner_ns'])) / (1048576 - 262144))
    wa[v] = per
if (d/'stderr.txt').exists() and (d/'stderr.txt').stat().st_size: err.append('stderr not empty')
if err: print('ERRORS', err); sys.exit(1)
m = {v: statistics.median(xs) for v, xs in gc.items()}
w = {v: statistics.median(xs) for v, xs in wa.items()}
out = {'gc_pause_median_ms': {v: round(x/1e6, 3) for v, x in m.items()},
       'gc_pause_per_rep_ms': {v: [round(x/1e6, 3) for x in xs] for v, xs in gc.items()},
       'region_vs_vector_gc_pct': round((m['region']-m['vector'])/m['vector']*100, 1),
       'region_vs_none_gc_pct': round((m['region']-m['none'])/m['none']*100, 1),
       'word_access_ns_per_step_median': {v: round(x, 1) for v, x in w.items()},
       'region_over_vector_word_access': round(w['region']/w['vector'], 3)}
print(json.dumps(out, indent=1))
ok = m['region'] <= 0.9*m['vector'] and abs(m['region']-m['none']) <= 0.1*m['none']
print('GC claim (region >=10% below vector and within 10% of none):', 'HOLDS' if ok else 'NOT SUPPORTED')
sys.exit(0 if (ok or not claim) else 1)
