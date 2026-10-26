#!/bin/bash
# check-parity.sh BINARY CORPUS-NAME [ALLOWED_DIVERGING] : exit 0 iff diverging <= allowed
HERE=$(cd "$(dirname "$0")" && pwd); B=$1; c=$2; allowed=${3:-0}
OUT=${TMPDIR:-/tmp}/buflocal1-check-$c; rm -rf "$OUT"; mkdir -p "$OUT"
res=$(OUT=$OUT "$HERE/cmp.sh" "$B" "$c") || exit 2
echo "$res"; n=${res##*diverging=}; [ "$n" -le "$allowed" ]
