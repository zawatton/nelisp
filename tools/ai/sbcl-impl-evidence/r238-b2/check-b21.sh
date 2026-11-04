#!/bin/bash
# Doc 238 B2.1 (rewritten 2026-11-01, user approved): the native -> native
# site in function-call2 takes the guarded direct entry (callgrind on build27:
# nc_30001 -> nc_d_30000 once per step, no nc_call edge); a redefinition of the
# callee between two calls of the warmed site takes effect on the next call
# and the restore returns to the native callee (run/b2/drv-b2.el on build27);
# kernel-diff equals b21; function-call2 Ir per step is not above b21.
D=$(cd "$(dirname "$0")" && pwd)
X=$D/cg27-function-call2-edges.txt
grep -q $'^nc_30001\tnc_d_30000\t1.000$' "$X" || exit 1
grep -q $'\tnc_call\t' "$X" && exit 1
for t in FSET RESTORE ADVICE ADVICE-REMOVED SEMANTIC-CHANGE SEMANTIC-RESTORE; do
  grep -q "^B2-$t ok=t$" "$D/b27-checks.out" || exit 1; done
grep -q 'ok=nil' "$D/b27-checks.out" && exit 1
cmp -s "$D/b21-kd.txt" "$D/b27-kd.txt" || exit 1
a=$(awk '$1=="function-call2"{print $2}' "$D/cg21-per-step.txt"); b=$(awk '$1=="function-call2"{print $2}' "$D/cg27-per-step.txt")
awk -v a="$a" -v b="$b" 'BEGIN{exit !(a!="" && b!="" && b<=a)}'
