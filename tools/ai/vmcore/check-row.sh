#!/bin/sh
# Usage: check-row.sh BINARY ROW MAX-INSTR-PER-TRIP [MIN-INSTR-PER-TRIP]
# Passes when the row's instructions per trip (two-N difference, pmcrun2) is within the bounds.
# Needs MATRIX (matrix dir with drv/ and calib.json) and PMC (compiled pmcrun2).
d=$(dirname "$0")
REPS=${REPS:-1} python3 "$d/measure.py" "$1" "$2" | python3 -c '
import re,sys
l=sys.stdin.read(); m=re.search(r"instr/trip=([\d.]+)",l); v=float(m[1]); mx=float(sys.argv[1]); mn=float(sys.argv[2]) if len(sys.argv)>2 else 0
print(l.strip(), "bound", mn, mx); sys.exit(0 if mn<=v<=mx else 1)' "$3" "${4:-0}"
