#!/usr/bin/env bash
# Usage: tools/ai/intscan-bench.sh BINARY [bench|chain N]
# bench: run tools/ai/intscan-bench.el.  chain N: hold the head of an
# N-long chain of 16-byte alloc-bytes blocks (word 0 = next) as a Lisp
# integer, collect, and print "chain n=N ok gc-ms=..." if the process survives.
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=$(realpath "$1" 2>/dev/null); mode=${2:-bench}; n=${3:-1000}
[ -x "$bin" ] || { echo "usage: $0 BINARY [bench|chain N]" >&2; exit 2; }
tmp=$(mktemp -d) || exit 3
trap 'rm -rf "$tmp"' EXIT
if [ "$mode" = chain ]; then
  cat > "$tmp/run.el" <<EOL
(defvar ib-hold nil)
(let ((i 0) (prev 0))
  (while (< i $n)
    (let ((b (alloc-bytes 16 8))) (ptr-write-u64 b 0 prev) (ptr-write-u64 b 8 0) (setq prev b))
    (setq i (1+ i)))
  (setq ib-hold (list prev)))
(let ((t0 (float-time))) (garbage-collect)
  (princ (format "chain n=%d ok gc-ms=%.1f\n" $n (* 1000 (- (float-time) t0)))))
EOL
else
  echo "(load \"$root/tools/ai/intscan-bench.el\")" > "$tmp/run.el"
fi
(ulimit -v 8388608; cd "$tmp" && timeout 300 "$bin" --load "$tmp/run.el" 2>&1) | grep -E '^(case=|chain |done)'
exit "${PIPESTATUS[0]}"
