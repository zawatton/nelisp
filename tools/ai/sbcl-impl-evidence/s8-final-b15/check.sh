#!/bin/sh
# Build15 final campaign checks.  Modes:
#   s66: hash-get1024 kernel <= 75% of build13 (169.9 ns) and hash semantics unchanged.
#   s73: vector-allocate64 meets the frozen threshold, alloc semantics unchanged, no gate regression.
cd "$(dirname "$0")" || exit 1
python3 -I evaluate.py >/dev/null || exit 1
case "$1" in
s66)
  python3 -I -c "import json,sys; m=json.load(open('summary.json'))['rows']['hash-get1024']['kernel_ns_per_step_median']['new']; print(m); sys.exit(0 if m <= 0.75*169.9 else 1)" || exit 1
  python3 -I evaluate.py --row hash-get1024 >/dev/null || exit 1
  python3 -I sd/compare.py sd/hash-gnu.out sd/b15-hash-string-cases.out | grep -qx 'compared A=273 B=273 mismatches=0' || exit 1
  python3 -I kd/compare.py kd/gnu.out kd/b15-nelisp.out | grep -qx 'compared A=121 B=121 mismatches=0' || exit 1
  grep -q 'HASH-CUSTOM equal=t hashcalls=1024 testcalls=1024' checks.out && test "$(grep -c 'equal=t' checks.out)" -eq 11 && test ! -s checks.err || exit 1 ;;
s73)
  python3 -I evaluate.py --row vector-allocate64 >/dev/null || exit 1
  python3 -I sd/compare.py sd/alloc-gnu.out sd/b15-alloc-cases.out | grep -qx 'compared A=15 B=15 mismatches=0' || exit 1
  grep -q 'GATE-COUNT: 142 cases, 17 failures' gates-b15.txt && test -z "$(comm -3 gates-b15.fail gates-b7.fail)" || exit 1 ;;
*) exit 2 ;;
esac
echo PASS
