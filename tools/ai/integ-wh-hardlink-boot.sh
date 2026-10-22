#!/usr/bin/env bash
# integ-wh-hardlink-boot.sh [BIN]: a binary whose companion .cold image has a second hard link must still start from
# that image (about 20 ms), not boot the prelude (about 9 s).  Copies BIN and its companions to a scratch directory,
# links the copy of the .cold, and times (princ (+ 1 2)).  Exit 0 iff the boot takes under 2 s.  The binary's own
# link count and a symlinked binary do not matter; the .cold file's link count does.
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
T=$(mktemp -d "$PWD/target/hardlink-boot.XXXXXX") || exit 2
trap 'rm -rf "$T"' EXIT
for f in "$BIN" "$BIN".*; do [ -f "$f" ] && cp "$f" "$T/nelisp${f#"$BIN"}"; done
ln "$T/nelisp.cold" "$T/other.cold" || exit 2
s=$(date +%s%N)
(ulimit -v 8388608; "$T/nelisp" --eval '(princ (+ 1 2))' > "$T/out" 2>&1)
ms=$(( ($(date +%s%N) - s) / 1000000 ))
echo "HARDLINK-BOOT ${ms} ms with the .cold at link count 2 (expected under 2000; 20 with one link)"
[ "$ms" -lt 2000 ]
