#!/bin/bash
# Doc 239 S1.2: build28 (exact classes) vs build27 on the frozen evidence:
# correctness equal (rows, kernel-diff, semantic-diff, guard checks, epoch and
# reader probes); vector-allocate64 Ir/step at least 1200 below build27; no
# other row above build27; arena statistics after the N=80000 row not above
# build27 by more than 5% (field-wise, all fields).
D=$(cd "$(dirname "$0")" && pwd)
for f in rows kd sd guards reader; do cmp -s "$D/b27-$f.txt" "$D/b28-$f.txt" || exit 1; done
grep -q '^"PROBE OK' "$D/b28-epoch.out" || grep -q 'PROBE OK' "$D/b28-epoch.out" || exit 1
awk 'NR==FNR{a[$1]=$2;next} {if(!($1 in a)) bad=1; else if($1=="vector-allocate64"){if(!($2<=a[$1]-1200)) bad=1} else if($2>a[$1]) bad=1} END{exit bad}' "$D/cg27-per-step.txt" "$D/cg28-per-step.txt" || exit 1
[ "$(wc -l < "$D/cg28-per-step.txt")" -eq 8 ] || exit 1
python3 -I - "$D/b27-arena.txt" "$D/b28-arena.txt" <<'PY' || exit 1
import sys,re
a=[int(x) for x in re.findall(r'-?\d+',open(sys.argv[1]).read())]; b=[int(x) for x in re.findall(r'-?\d+',open(sys.argv[2]).read())]
# field 0 is a process address (varies per run); compare the rest
sys.exit(0 if len(a)==len(b)>3 and all(y<=x*1.05 for x,y in zip(a[1:],b[1:])) else 1)
PY
