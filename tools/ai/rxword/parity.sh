#!/bin/sh
# usage: parity.sh NELISP_BINARY   (exit 0 iff the corpus output equals GNU Emacs 31.1's)
here=$(cd "$(dirname "$0")" && pwd); bin=$1; out=${PARITY_OUT:-${TMPDIR:-/tmp}/rxword-parity.$$}
mkdir -p "$out" || exit 2
( ulimit -v 8388608; timeout ${PARITY_TIMEOUT:-300} "$bin" -Q --batch ${PARITY_PRELOAD:+--load "$PARITY_PRELOAD"} --load "$here/parity-corpus.el" > "$out/nl.txt" 2> "$out/nl.err" < /dev/null )
timeout 300 /usr/local/bin/emacs -Q --batch -l "$here/parity-corpus.el" > "$out/gnu.txt" 2> "$out/gnu.err" < /dev/null
grep -v '^nil$' "$out/nl.txt" > "$out/nl.cmp"; cp "$out/gnu.txt" "$out/gnu.cmp"
# PARITY_MUTATE=1: negative control, damages one NeLisp line so the comparison must fail.
[ -n "${PARITY_MUTATE:-}" ] && sed -i '500s/ / X /' "$out/nl.cmp"
if cmp -s "$out/nl.cmp" "$out/gnu.cmp" && [ "$(tail -1 "$out/gnu.cmp" | cut -c1-9)" = ";; cases " ]; then echo "PARITY OK $(tail -1 "$out/gnu.cmp")"; exit 0; fi
diff "$out/nl.cmp" "$out/gnu.cmp" | head -${PARITY_LINES:-20}; echo "PARITY DIFF (outputs in $out)"; exit 1
