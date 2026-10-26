#!/usr/bin/env python3
"""Compare per-test statuses of a NeLisp binary against survey5's v17 results.
usage: regress-compare.py SURVEY5_RESULTS_TSV RUN_DIR   (RUN_DIR holds <file>.tsv written by gnutests.py --out)"""
import sys, os, collections
base = collections.defaultdict(dict)
for ln in open(sys.argv[1]):
    f = ln.rstrip('\n').split('\t')
    if len(f) >= 5:
        base[f[0].replace('/', '_')][f[1]] = f[4]
tot = collections.Counter(); worse = []; better = []
for rel, tests in sorted(base.items()):
    p = os.path.join(sys.argv[2], rel + '.tsv')
    if not os.path.exists(p):
        continue
    new = dict(l.rstrip('\n').split('\t') for l in open(p))
    for t, st in tests.items():
        ns = new.get(t, 'MISSING')
        tot[(st == 'PASS', ns == 'PASS')] += 1
        if st == 'PASS' and ns != 'PASS':
            worse.append((rel, t, ns))
        if st != 'PASS' and ns == 'PASS':
            better.append((rel, t, st))
print('v17 pass -> new pass %d, v17 pass -> new not pass %d, v17 not pass -> new pass %d, both not pass %d' % (
    tot[(True, True)], tot[(True, False)], tot[(False, True)], tot[(False, False)]))
print('NOT-PASS NOW (was PASS on v17):')
for r in worse: print('  ', *r)
print('PASS NOW (was not PASS on v17): %d' % len(better))
c = collections.Counter(r for r, _, _ in better)
for r, n in c.most_common(): print('  ', n, r)
