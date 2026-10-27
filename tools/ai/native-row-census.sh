#!/bin/bash
# native-row-census.sh REPO [BINARY]
#
# Stale-row gate for lisp/nelisp-fxir-native-data.el.  For EVERY row of REPO's
# lisp/nelisp-fxir-native-data.el it re-harvests the row from REPO's current
# sources (tools/ai/native-row-census.el: the defun is read from REPO/lisp,
# REPO/scripts -- including the definitions scripts/nelisp-standalone-build.el
# embeds as strings --, defined as a lexical closure, compiled by the same
# machinery as tools/ai/nc1-harvest.el) and compares the whole row (nlocals,
# callees, fxir, arity) with the stored one.  A function with no source
# definition is checked from BINARY's image closure (NRC-NOTE line), a primitive
# in the image is skipped (NRC-NOTE); build BINARY from REPO to make those
# current.  Macro/defvar changes are not re-read: only defuns are re-evaluated.
#
# Output: `STALE NAME ID reason' per mismatch, `DUP-ID ID NAME1 NAME2' /
# `DUP-NAME NAME ID1 ID2' per duplicate, a final
#   native-row-census: rows=N stale=S dup=D
# Exit 0 iff stale=0 and dup=0 (1: red, 2: the census did not complete).
# BINARY defaults to $NELISP_BIN, else the newest REPO/target/*-build/dynamic/nelisp-dynamic.
# Runs about 10 s under `ulimit -v 8388608'.  Set NRC_FULL=1 to also print NRC-NOTE lines.
set -u
REPO=${1:?usage: native-row-census.sh REPO [BINARY]}
REPO=$(cd "$REPO" && pwd) || exit 2
HERE=$(cd "$(dirname "$0")" && pwd)
BIN=${2:-${NELISP_BIN:-}}
if [ -z "$BIN" ]; then
  BIN=$(ls -t "$REPO"/target/*-build/dynamic/nelisp-dynamic 2>/dev/null | head -1)
fi
[ -n "$BIN" ] && [ -x "$BIN" ] && BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN") || { echo "native-row-census: no BINARY (pass one or set NELISP_BIN)" >&2; exit 2; }
[ -f "$REPO/lisp/nelisp-fxir-native-data.el" ] || { echo "native-row-census: no native data in $REPO" >&2; exit 2; }
ulimit -v 8388608
out=$(mktemp) || exit 2
(cd "$REPO" && NRC_REPO="$REPO" timeout 900 "$BIN" --load "$HERE/native-row-census.el" > "$out" 2>&1)
rc=$?
if [ "${NRC_FULL:-0}" = 1 ]; then cat "$out"; else grep -v '^NRC-NOTE' "$out"; fi
if ! grep -q '^native-row-census: rows=' "$out"; then
  echo "native-row-census: did not complete (rc=$rc); last lines:" >&2; tail -5 "$out" >&2
  rm -f "$out"; exit 2
fi
rm -f "$out"
exit $rc
