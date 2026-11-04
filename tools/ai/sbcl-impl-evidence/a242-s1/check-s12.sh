#!/bin/bash
# Doc 242 S1.2. COLD_LOAD_BIN=<binary> (default build32). A bare start executes at most 53,389,779 Ir
# (50% of build31's 106,779,559), then the Doc 241 v4 correctness gates run on the same binary.
BIN=${COLD_LOAD_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on32/nelisp-dynamic}
HERE=$(cd "$(dirname "$0")" && pwd)
A241=$HERE/../a241-s3
LIMIT=53389779
[ -x "$BIN" ] || { echo "no binary $BIN"; exit 1; }
command -v valgrind > /dev/null && command -v callgrind_annotate > /dev/null || { echo "valgrind/callgrind_annotate missing"; exit 1; }
W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
echo '(kill-emacs 0)' > "$W/nop.el"
(ulimit -v 8388608; cd "$W" && timeout 600 valgrind --tool=callgrind --callgrind-out-file="$W/cg.out" "$BIN" --load "$W/nop.el" -- > "$W/run.log" 2>&1)
[ -s "$W/cg.out" ] || { echo "callgrind produced no output"; tail -3 "$W/run.log"; exit 1; }
IR=$(callgrind_annotate "$W/cg.out" 2> /dev/null | grep -m1 'PROGRAM TOTALS' | grep -o '^[[:space:]]*[0-9,]*' | tr -d ', ')
case "$IR" in ''|*[!0-9]*) echo "could not parse PROGRAM TOTALS"; exit 1;; esac
echo "bare start Ir: $IR (limit $LIMIT, build31 106779559)"
[ "$IR" -le "$LIMIT" ] || { echo "Ir above limit"; exit 1; }
"$A241/probe-equal.sh" "$BIN" || { echo "probe-equal failed"; exit 1; }
IMAGE_SIZE_BIN=$BIN bash "$A241/check-s12v4.sh" || { echo "check-s12v4 failed"; exit 1; }
IMAGE_SIZE_BIN=$BIN bash "$A241/check-s22v4.sh" || { echo "check-s22v4 failed"; exit 1; }
echo S1.2-PASS
