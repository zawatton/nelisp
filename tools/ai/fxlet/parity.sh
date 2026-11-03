#!/bin/sh
# fxlet dynamic-binding parity corpus: BIN's output of run.el (source flavour, run by the fx interpreter, and
# GNU-compiled byte code, bc.el made by mkbc.el) must equal GNU 31.1's (expected-src, expected-bc).
# Usage: sh tools/ai/fxlet/parity.sh BIN [mutant-caught]
#   mutant-caught: negative control; a copy of expected-src with one answer changed must NOT match.
cd "$(dirname "$0")" || exit 1
BIN=$1; MODE=$2
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
[ -x "$BIN" ] || { echo "parity: no binary $BIN"; exit 2; }
O=$(mktemp -d) || exit 1
r=0
for fl in src bc; do
  ( ulimit -v 8388608; FL_FLAV=$fl timeout 240 "$BIN" --load run.el -- < /dev/null > "$O/out-$fl" 2> "$O/err-$fl" )
  exp=expected-$fl
  if [ "$MODE" = mutant-caught ] && [ $fl = src ]; then sed 's/^recur (210 10)$/recur (211 10)/' expected-src > "$O/mut"; exp=$O/mut; fi
  if cmp -s "$O/out-$fl" "$exp"; then echo "parity $fl: equal ($(grep -c . $exp) lines)"; else [ "$MODE" = mutant-caught ] && [ $fl = bc ] && continue; r=1; echo "parity $fl: DIFF"; diff "$O/out-$fl" "$exp" | head -30; fi
done
rm -rf "$O"
if [ "$MODE" = mutant-caught ]; then [ $r = 1 ] && { echo "parity: mutant caught"; exit 0; }; echo "parity: mutant NOT caught"; exit 1; fi
exit $r
