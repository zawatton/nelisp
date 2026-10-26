#!/bin/sh
# usage: check.sh BINARY SET   (SET: labels names radix mods misc all)
# Runs tools/ai/reader1/corpus.el on GNU Emacs and on BINARY (NeLisp) and requires identical output.
# Known remaining differences (documented in target/reader1/REPORT.md) are listed in
# tools/ai/reader1/known-diffs.txt as exact "SOURCE<TAB>MODE" prefixes and are ignored.
BIN=$1; SET=${2:-all}
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=${TMPDIR:-/tmp}/reader1-check.$$
mkdir -p "$OUT" || exit 1
(ulimit -v 8388608; READER1_SET=$SET timeout 120 emacs -Q --batch -l "$HERE/corpus.el" > "$OUT/gnu" 2>/dev/null) || { echo "gnu run failed"; exit 2; }
(ulimit -v 8388608; READER1_SET=$SET timeout 250 "$BIN" --load "$HERE/corpus.el" -- > "$OUT/nl" 2>"$OUT/nl.err") || { echo "nelisp run failed"; tail -3 "$OUT/nl.err"; exit 3; }
if [ -s "$HERE/known-diffs.txt" ]; then
  grep -a -v -F -f "$HERE/known-diffs.txt" "$OUT/gnu" > "$OUT/gnu.f"; grep -a -v -F -f "$HERE/known-diffs.txt" "$OUT/nl" > "$OUT/nl.f"
else cp "$OUT/gnu" "$OUT/gnu.f"; cp "$OUT/nl" "$OUT/nl.f"; fi
n=$(diff "$OUT/gnu.f" "$OUT/nl.f" | grep -c '^<')
total=$(wc -l < "$OUT/gnu.f")
grep -a -v "^nil\$" "$OUT/gnu.f" > "$OUT/g2"; grep -a -v "^nil\$" "$OUT/nl.f" > "$OUT/n2"; mv "$OUT/g2" "$OUT/gnu.f"; mv "$OUT/n2" "$OUT/nl.f"; n=$(diff "$OUT/gnu.f" "$OUT/nl.f" | grep -c "^<"); total=$(wc -l < "$OUT/gnu.f")
echo "reader1 corpus set=$SET: $n of $total lines differ"
[ "$n" = 0 ] && [ "$total" -gt 0 ] && { rm -rf "$OUT"; exit 0; }
diff "$OUT/gnu.f" "$OUT/nl.f" | head -${SHOW:-6} | cut -c1-200
exit 1
