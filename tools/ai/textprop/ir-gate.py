#!/usr/bin/env python3
"""ir-gate.py BEFORE.txt AFTER.txt [put]: per-call Ir of the six operations, A2/v28.  Default: every operation is lower and
next-single-property-change, add-text-properties and remove-text-properties are at most half.  'put': put-text-property at most 0.5."""
import sys
b = {l.split()[0]: float(l.split()[-1]) for l in open(sys.argv[1])}
a = {l.split()[0]: float(l.split()[-1]) for l in open(sys.argv[2])}
r = {k: a[k] / b[k] for k in ('gtp', 'nspc', 'pt', 'atp', 'rtp0', 'tpa')}
print({k: round(v, 3) for k, v in r.items()})
if len(sys.argv) > 3 and sys.argv[3] == 'put':
    sys.exit(0 if r['pt'] <= 0.5 else 1)
sys.exit(0 if all(v < 1 for v in r.values()) and r['nspc'] <= 0.55 and r['atp'] <= 0.5 and r['rtp0'] <= 0.5 else 1)
