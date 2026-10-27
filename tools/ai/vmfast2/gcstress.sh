#!/bin/sh
# vmfast2 GC stress: BIN's output of gcstress.el must equal v30's and GNU 31.1's.
# Usage: sh tools/ai/vmfast2/gcstress.sh BIN
cd "$(dirname "$0")" || exit 1
BIN=$1
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
V30=$(realpath ../../../../resume-4943-20261024/target/integ-1047-build/dynamic/nelisp-dynamic)
[ -x "$BIN" ] || { echo "gcstress: no binary $BIN"; exit 2; }
O=$(mktemp -d) || exit 1
( ulimit -v 8388608; timeout 280 "$BIN" --load gcstress.el -- < /dev/null > "$O/cand" 2>/dev/null )
( ulimit -v 8388608; timeout 280 "$V30" --load gcstress.el -- < /dev/null > "$O/v30" 2>/dev/null )
( ulimit -v 8388608; timeout 280 /usr/local/bin/emacs -Q --batch -l gcstress.el > "$O/gnu" 2>/dev/null )
a=$(cat "$O/cand"); b=$(cat "$O/v30"); g=$(cat "$O/gnu")
rm -rf "$O"
[ -n "$a" ] && [ "$a" = "$b" ] && [ "$a" = "$g" ] && { echo "gcstress: equal to v30 and GNU ($(echo "$a" | wc -c) bytes)"; exit 0; }
echo "gcstress: DIFF"; echo "cand: $a" | cut -c1-300; echo "v30:  $b" | cut -c1-300; echo "gnu:  $g" | cut -c1-300; exit 1
