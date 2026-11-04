#!/bin/sh
# Final-image correctness: rows, guards, semantic/bignum/alloc/kernel diffs.
cd "$(dirname "$0")" || exit 1
for r in rows/*.out; do grep -q 'route=named-fx-default' "$r" && grep -q 'BENCH-CHECK' "$r" || exit 1; done
test "$(ls rows/*.out | wc -l)" -eq 9 || exit 1
test ! -s checks.err || exit 1
test "$(grep -c 'equal=t' checks.out)" -eq 11 || exit 1
grep -q 'HASH-CUSTOM equal=t hashcalls=1024 testcalls=1024' checks.out || exit 1
grep -q 'LOGAND-FSET equal=t calls=12288' checks.out || exit 1
python3 -I sd/compare.py sd/alloc-gnu.out sd/b7-alloc-cases.out >/dev/null || exit 1
test "$(python3 -I sd/compare.py sd/bignum-gnu.out sd/b7-bignum-cases.out | grep -c '^DIFF')" -eq 1 || exit 1
python3 -I sd/compare.py sd/bignum-gnu.out sd/b7-bignum-cases.out | grep -q '^DIFF round-pp' || exit 1
test "$(python3 -I sd/compare.py sd/gnu.out sd/b7-cases.out | grep -c '^DIFF')" -eq 12 || exit 1
python3 -I kd/compare.py kd/gnu.out kd/b7-nelisp.out >/dev/null || exit 1
echo PASS
