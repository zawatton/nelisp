#!/bin/sh
# Differential corpus: NeLisp standalone binary vs host GNU Emacs.
# Env: NELISP_BIN (default ./target/nelisp).  Exit 0 iff outputs are identical
# and the corpus holds at least 40 cases.
bin=${NELISP_BIN:-./target/nelisp}
here=$(cd "$(dirname "$0")" && pwd)
el="$here/large-buffer-regex-corpus.el"
out=${LBC_OUT:-${TMPDIR:-/tmp}/lbc.$$}
mkdir -p "$out" || exit 1
# The standalone --load echoes the value of the last form; drop that line.
( ulimit -v 8388608; timeout 280 "$bin" --load "$el" -- ) 2>&1 | sed '${/^nil$/d}' > "$out/nelisp.txt"
emacs --batch -Q -l "$el" > "$out/host.txt" 2>&1
total=$(sed -n 's/^TOTAL //p' "$out/host.txt")
[ "${total:-0}" -ge 40 ] || { echo "corpus too small or host failed: total=$total"; exit 1; }
if diff "$out/host.txt" "$out/nelisp.txt" > "$out/diff.txt"; then
  echo "corpus identical: $total cases"; exit 0
fi
echo "corpus DIFFERS: $(grep -c '^<' "$out/diff.txt") host-only lines; see $out/diff.txt"
exit 1
