#!/usr/bin/env python3
"""rows-gate.py BASE.json LANE.json GNU.json [geomean|ptp]: the lane's text-property rows (tools/ai/textprop/rows30.json)
against v28.  geomean: PASS iff the geometric-mean speedup over the 30 rows is >= 3.  ptp: PASS iff the put-text-property
rows (tp-put-100, tp-put-whole-100k, buffers-textprops-*) take at most half the v28 time on average.  No mode: both."""
import json, math, sys
b, l, g = (json.load(open(p)) for p in sys.argv[1:4])
rows = [r for r in b if 'ns' in b[r] and r in l and 'ns' in l[r]]
sp = {r: b[r]['ns'] / l[r]['ns'] for r in rows}
gm = math.exp(sum(map(math.log, sp.values())) / len(sp))
ptp = [r for r in rows if r.startswith('tp-put') or r.startswith('buffers-textprops')]
ptp_ratio = sum(l[r]['ns'] / b[r]['ns'] for r in ptp) / len(ptp)
for r in rows: print(f"{r:34s} v28 {b[r]['ns']:12.0f} lane {l[r]['ns']:12.0f} x{sp[r]:6.2f} gnu {g.get(r, {}).get('ns', 0):10.0f}")
print(f"ROWS {len(rows)} geomean speedup {gm:.2f} ptp new/old {ptp_ratio:.2f}")
mode = sys.argv[4] if len(sys.argv) > 4 else 'both'
ok_g = len(rows) >= 28 and gm >= 3
ok_p = ptp_ratio <= 0.5
sys.exit(0 if (ok_g if mode == 'geomean' else ok_p if mode == 'ptp' else ok_g and ok_p) else 1)
