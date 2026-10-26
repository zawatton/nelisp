#!/usr/bin/env python3
"""delta.py BINARY ROW BASE-ROW MAX : passes when instr/trip(ROW) - instr/trip(BASE-ROW) <= MAX."""
import os,sys
sys.path.insert(0,os.path.dirname(os.path.abspath(__file__)))
import measure
b,row,base,mx=sys.argv[1],sys.argv[2],sys.argv[3],float(sys.argv[4])
i1=measure.row(b,row)[0]; i0=measure.row(b,base)[0]
print(f'{row} {i1:.1f} - {base} {i0:.1f} = {i1-i0:.1f} (max {mx})')
sys.exit(0 if i1-i0<=mx else 1)
