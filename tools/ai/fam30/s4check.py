#!/usr/bin/env python3
"""s4check.py LOG [--min-speedup X] [--vs-log LOG2 --max-ratio Y]: read paired_bench tables.
--min-speedup X: v34 ns/step / fam30 ns/step >= X in LOG.
--vs-log LOG2 --max-ratio Y: fam30 ns/step in LOG <= Y * fam30 ns/step in LOG2 (bc row against its interpreted twin)."""
import re, sys
def ns(path, eng):
    for l in open(path):
        m = re.match(r'\s*' + eng + r'\s+([\d.]+)\s', l)
        if m: return float(m.group(1))
    sys.exit('no %s row in %s' % (eng, path))
a = sys.argv[1:]; log = a[0]
if '--min-speedup' in a:
    x = float(a[a.index('--min-speedup') + 1]); s = ns(log, 'v34') / ns(log, 'fam30')
    print('speedup %.2f (need >= %.2f)' % (s, x)); sys.exit(0 if s >= x else 1)
if '--vs-log' in a:
    y = float(a[a.index('--max-ratio') + 1]); r = ns(log, 'fam30') / ns(a[a.index('--vs-log') + 1], 'fam30')
    print('ratio %.3f (need <= %.3f)' % (r, y)); sys.exit(0 if r <= y else 1)
