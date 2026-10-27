#!/bin/sh
# vmfast2 VM parity corpus: BIN's output of run.el must equal GNU 31.1's (expected, made by
# `emacs -Q --batch -l run.el > expected` after `emacs -Q --batch -l mkbc.el`).
# Usage: sh tools/ai/vmfast2/parity.sh BIN [mutant-caught]
#   mutant-caught: negative control; a copy of expected with one answer changed must NOT match.
cd "$(dirname "$0")" || exit 1
BIN=$1; MODE=$2
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
[ -x "$BIN" ] || { echo "parity: no binary $BIN"; exit 2; }
O=$(mktemp -d) || exit 1
( ulimit -v 8388608; timeout 240 "$BIN" --load run.el -- < /dev/null > "$O/out" 2> "$O/err" )
exp=expected
if [ "$MODE" = mutant-caught ]; then sed 's/^bad52 (error "Invalid byte opcode: op=52, ptr=2")/bad52 (error "Invalid byte opcode: op=52, ptr=1")/' expected > "$O/mut"; exp=$O/mut; fi
if cmp -s "$O/out" "$exp"; then r=0; echo "parity: equal ($(grep -c . expected) lines)"; else r=1; diff "$O/out" "$exp" | head -20; fi
rm -rf "$O"
if [ "$MODE" = mutant-caught ]; then [ $r = 1 ] && { echo "parity: mutant caught"; exit 0; }; echo "parity: mutant NOT caught"; exit 1; fi
exit $r
