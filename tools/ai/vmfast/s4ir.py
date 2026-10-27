#!/usr/bin/env python3
"""s4ir.py IR.TXT : S4.2 check. IR.TXT lines: ROW V25_IR CAND_IR (callgrind Ir per trip, same two-N).
Exit 0 iff lists-memq-bc, tm-fts-13 and reg-sum-bc are each at least 40 percent below v25."""
import sys
rows = {}
for l in open(sys.argv[1]):
    p = l.split()
    if len(p) >= 3 and not l.startswith('#'): rows[p[0]] = (float(p[1]), float(p[2]))
ok = True
for r in ('lists-memq-bc', 'tm-fts-13', 'reg-sum-bc'):
    if r not in rows: print(r, 'missing'); ok = False; continue
    a, b = rows[r]; red = 1 - b / a
    print(f'{r}\tv25={a:.1f}\tcand={b:.1f}\treduction={red*100:.1f}%\t{"OK" if red >= 0.40 else "SHORT"}')
    ok = ok and red >= 0.40
sys.exit(0 if ok else 1)
