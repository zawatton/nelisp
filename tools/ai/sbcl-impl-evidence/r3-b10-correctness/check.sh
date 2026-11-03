#!/bin/sh
# R3.2: build10 keeps S8 correctness: rows, guards, semantic/alloc/bignum/kernel diffs, regression guard.
cd "$(dirname "$0")" || exit 1
for r in rows/*.out; do grep -q 'route=named-fx-default' "$r" && grep -q 'BENCH-CHECK' "$r" || exit 1; done
test "$(ls rows/*.out | wc -l)" -eq 9 || exit 1
test ! -s checks.err || exit 1
test "$(grep -c 'equal=t' checks.out)" -eq 11 || exit 1
grep -q 'HASH-CUSTOM equal=t hashcalls=1024 testcalls=1024' checks.out || exit 1
grep -q 'LOGAND-FSET equal=t calls=12288' checks.out || exit 1
python3 -I sd/compare.py sd/alloc-gnu.out sd/b10-alloc-cases.out >/dev/null || exit 1
python3 -I sd/compare.py sd/bignum-gnu.out sd/b10-bignum-cases.out | grep -qx 'compared A=65 B=65 mismatches=0' || exit 1
python3 -I sd/compare.py sd/gnu.out sd/b10-cases.out | grep '^DIFF' | sort > .n; python3 -I sd/compare.py sd/gnu.out sd/b7-cases.out | grep '^DIFF' | sort > .o
cmp -s .n .o && test "$(wc -l < .n)" -eq 12 || { rm -f .n .o; exit 1; }; rm -f .n .o
python3 -I kd/compare.py kd/gnu.out kd/b10-nelisp.out | grep -qx 'compared A=121 B=121 mismatches=0' || exit 1
test "$(grep -c 'NATIVE-PROBE .* identity-eq=t row=300' kd/b10-nelisp.out)" -eq 6 || exit 1
grep -q 'GATE-COUNT: 142 cases, 17 failures' gates/b10.txt || exit 1
test -z "$(comm -3 gates/b10.fail gates/b7.fail)" || exit 1
echo PASS
