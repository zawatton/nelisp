#!/usr/bin/env python3
"""spread.py BINARY ROW [RUNS=20]: run-to-run cycle spread at the calibrated N (cycles per trip, startup included)."""
import sys,statistics,os
sys.path.insert(0,os.path.dirname(os.path.abspath(__file__)))
import measure
b,rid=sys.argv[1],sys.argv[2]; runs=int(sys.argv[3]) if len(sys.argv)>3 else 20
hi=measure.cal[rid][3]; hi=int(float(f'{hi:.2g}')); drv=f'{measure.M}/drv/{rid}.el'
v=[]
for _ in range(runs):
    c,i,_r=measure.one(b,drv,hi); v.append(c/hi)
v.sort()
print(f'{rid} runs={runs} cyc/trip min={v[0]:.2f} med={statistics.median(v):.2f} max={v[-1]:.2f} spread(max-min)/med={(v[-1]-v[0])/statistics.median(v)*100:.2f}% stdev%={statistics.pstdev(v)/statistics.mean(v)*100:.2f}')
