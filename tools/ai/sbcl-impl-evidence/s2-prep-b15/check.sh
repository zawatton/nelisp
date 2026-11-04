#!/bin/sh
# S2.10: application preparation median (FIRST_FORM_COMPLETE -> APPLICATION_STATE_READY) over 7 fresh
# build15 runs is >= 30% below build13's 6.40 s, every run passes the phase checker, and all nine
# benchmark rows give the same BENCH-CHECK results as build13.
cd "$(dirname "$0")" || exit 1
n=0
for w in phase-out/*.wrapper.json; do
  id=$(basename "$w" .wrapper.json)
  python3 -I check_phase_events.py --wrapper "$w" --driver "phase-out/$id.events.sexp" >/dev/null || exit 1
  n=$((n+1))
done
test "$n" -eq 7 || exit 1
python3 -I - <<'P' || exit 1
import glob, re, statistics, sys
v = []
for f in glob.glob('phase-out/*.events.sexp'):
    e = {m.group(1): int(m.group(2)) for m in re.finditer(r"\(([A-Z_]+) (\d+) nil\)", open(f).read())}
    v.append((e['APPLICATION_STATE_READY'] - e['FIRST_FORM_COMPLETE']) / 1e9)
m = statistics.median(v)
print(f"prep median {m:.3f} s (limit {6.40 * 0.7:.3f})")
sys.exit(0 if m <= 6.40 * 0.7 else 1)
P
for r in integer-sum function-call2 nested-function-call2 vector-write vector-read vector-allocate64 string-slice16 string-concat16 hash-get1024; do
  a=$(grep "^$r " rows-b13.txt | grep -o 'result=[^ ]*'); b=$(grep "^$r " rows-b15.txt | grep -o 'result=[^ ]*')
  test -n "$a" && test "$a" = "$b" || { echo "result differs: $r"; exit 1; }
done
# S2.2 contract for the trimmed driver: 3 alternating plain/phased pairs on build15
# (integer-sum N=16M, 2 warmups): stdout equal after masking measured fields,
# stderr byte-identical, phase events valid.
mask() { sed -E 's/elapsed=[0-9.e-]+/elapsed=X/; s/resolution=[0-9.e-]+/resolution=X/' "$1"; }
for i in 1 2 3; do
  mask contract/plain$i.out > .a; mask contract/phased$i.out > .b
  cmp -s .a .b && cmp -s contract/plain$i.err contract/phased$i.err && grep -q 'route=named-fx-default' contract/plain$i.out || { rm -f .a .b; echo "contract pair $i differs"; exit 1; }
done
rm -f .a .b
for w in contract/phase-out/*.wrapper.json; do
  id=$(basename "$w" .wrapper.json)
  python3 -I check_phase_events.py --wrapper "$w" --driver "contract/phase-out/$id.events.sexp" >/dev/null || exit 1
done
test "$(ls contract/phase-out/*.wrapper.json | wc -l)" -eq 3 || exit 1
echo PASS
