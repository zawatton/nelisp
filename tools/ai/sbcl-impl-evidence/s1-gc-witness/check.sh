#!/bin/bash
# Re-verifies S1.3 from saved outputs in this directory. Exit 0 iff all hold.
d=$(cd "$(dirname "$0")" && pwd); fail=0
f(){ echo "FAIL: $*"; fail=1; }
for t in vector-allocate64 string-concat16 integer-sum; do
  [ "$(cat $d/$t.rc)" = 0 ] || f "$t rc"
  grep -q 'route=named-fx-default' $d/$t.out || f "$t route"
  grep -q '^BENCH-CHECK n=1048576 result=1048576:' $d/$t.out || f "$t BENCH-CHECK"
  [ ! -s $d/$t.err ] || f "$t stderr non-empty"
  b=$(sed -n 's/.*before=\([0-9]*\).*/\1/p' $d/$t.wit); a=$(sed -n 's/.*after=\([0-9]*\).*/\1/p' $d/$t.wit)
  [ -n "$a" ] && [ -n "$b" ] || { f "$t witness unparsable"; continue; }
  delta=$((a-b)); echo "$t collections_in_kernel=$delta"
  case $t in integer-sum) [ $delta -eq 0 ] || f "$t expected 0 collections";; *) [ $delta -ge 1 ] || f "$t expected >=1 collections";; esac
done
n=neg-vector-allocate64
if [ "$(cat $d/$n.rc)" != 0 ] && ! grep -q BENCH-CHECK $d/$n.out; then echo "negative control rejected (rc=$(cat $d/$n.rc))"; else f "negative control accepted"; fi
[ $fail = 0 ] && echo "S1.3 PASS" || echo "S1.3 FAIL"; exit $fail
