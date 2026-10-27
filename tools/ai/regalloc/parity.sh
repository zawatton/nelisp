#!/bin/sh
# regalloc parity: BIN's output of run.el (this lane's corpus) and of tools/ai/vmfast/run.el must
# equal GNU 31.1's (expected / ../vmfast/expected, made with `emacs -Q --batch -l run.el` after
# `emacs -Q --batch -l mkbc.el`).
# Usage: sh tools/ai/regalloc/parity.sh BIN [mutant-caught]
#   mutant-caught: negative control; a copy of expected with one answer changed must NOT match.
cd "$(dirname "$0")" || exit 1
BIN=$1; MODE=$2
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
[ -x "$BIN" ] || { echo "parity: no binary $BIN"; exit 2; }
O=$(mktemp -d) || exit 1
( ulimit -v 8388608; timeout 240 "$BIN" --load run.el -- < /dev/null > "$O/out" 2> "$O/err" )
( ulimit -v 8388608; cd ../vmfast && timeout 240 "$BIN" --load run.el -- < /dev/null > "$O/vout" 2> "$O/verr" )
exp=expected
if [ "$MODE" = mutant-caught ]; then sed 's/^sum ((6500 1000)/sum ((6501 1000)/' expected > "$O/mut"; exp=$O/mut; fi
r=0
if cmp -s "$O/out" "$exp"; then echo "parity: regalloc equal ($(grep -c . expected) lines)"; else r=1; diff "$O/out" "$exp" | head -20; fi
if cmp -s "$O/vout" ../vmfast/expected; then echo "parity: vmfast equal ($(grep -c . ../vmfast/expected) lines)"; else r=1; diff "$O/vout" ../vmfast/expected | head -10; fi
rm -rf "$O"
if [ "$MODE" = mutant-caught ]; then [ $r = 1 ] && { echo "parity: mutant caught"; exit 0; }; echo "parity: mutant NOT caught"; exit 1; fi
exit $r
