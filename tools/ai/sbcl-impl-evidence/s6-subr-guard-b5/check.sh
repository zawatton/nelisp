#!/bin/sh
cd "$(dirname "$0")" || exit 1
test ! -s subr.err || exit 1
for s in gethash concat substring; do grep -q "SUBR-FSET $s equal=t calls=1024 expected=1024" subr.out || exit 1; done
grep -q 'SUBR-FSET make-vector equal=t calls=1025' subr.out || exit 1
test "$(grep -c 'SUBR-RESTORED .* equal=t' subr.out)" -eq 4 || exit 1
grep -q 'HASH-CUSTOM equal=t hashcalls=1024 testcalls=1024' subr.out || exit 1
for r in rows/*.out; do grep -q 'route=named-fx-default' "$r" && grep -q 'BENCH-CHECK' "$r" || exit 1; done
test "$(ls rows/*.out | wc -l)" -eq 9
