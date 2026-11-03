#!/bin/sh
# S2.13: build20 with the worktree harness (prelude/helper loads removed, plan reuse, input recheck,
# baseline shortcut, digest memo, xdigit loop, read-circle fast path, cl-seq removed from the producer
# and the driver templates).  7 alternating fresh runs per variant on CPU 5: preparation median of the
# current harness <= 542 ms (50% of build15's 1084), every run passes the phase checker, all nine rows
# equal build15, and 3 plain/phased pairs satisfy the S2.2 contract.
cd "$(dirname "$0")" || exit 1
for d in phase-out-cur phase-out-base; do
  n=0
  for w in $d/*.wrapper.json; do
    id=$(basename "$w" .wrapper.json)
    python3 -I check_phase_events.py --wrapper "$w" --driver "$d/$id.events.sexp" >/dev/null || exit 1
    n=$((n+1))
  done
  test "$n" -eq 7 || exit 1
done
python3 -I - <<'P' || exit 1
import glob, re, statistics, sys
def med(d):
    v = []
    for f in glob.glob(d + '/*.events.sexp'):
        e = {m.group(1): int(m.group(2)) for m in re.finditer(r"\(([A-Z_]+) (\d+) nil\)", open(f).read())}
        v.append((e['APPLICATION_STATE_READY'] - e['FIRST_FORM_COMPLETE']) / 1e6)
    return statistics.median(v)
c, b = med('phase-out-cur'), med('phase-out-base')
print(f"prep median cur {c:.1f} ms, base {b:.1f} ms (limit 542)")
sys.exit(0 if c <= 542 else 1)
P
for r in integer-sum function-call2 nested-function-call2 vector-write vector-read vector-allocate64 string-slice16 string-concat16 hash-get1024; do
  a=$(grep "^$r " rows-b15.txt | grep -o 'result=[^ ]*'); b=$(grep "^$r " rows-cur.txt | grep -o 'result=[^ ]*')
  test -n "$a" && test "$a" = "$b" || { echo "result differs: $r"; exit 1; }
done
mask() { sed -E 's/elapsed=[0-9.e-]+/elapsed=X/; s/resolution=[0-9.e-]+/resolution=X/' "$1"; }
for i in 1 2 3; do
  mask contract/plain$i.out > .a; mask contract/phased$i.out > .b
  cmp -s .a .b && cmp -s contract/plain$i.err contract/phased$i.err || { rm -f .a .b; echo "contract pair $i differs"; exit 1; }
done
rm -f .a .b
echo PASS
