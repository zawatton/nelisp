#!/bin/bash
# bench.sh BIN OUT: time case-calls.txt on BIN with h.el (ns per call), one line per call.
here=$(cd "$(dirname "$0")" && pwd); bin=$1; out=$2
mkdir -p "$here/cwd" && cd "$here/cwd" || exit 2
( ulimit -v 8388608; OKS="$here/case-calls.txt" timeout 300 "$bin" --load "$here/h2.el" < /dev/null ) 2>/dev/null | grep -E '^(T|ERR) ' > "$out"
[ "$(wc -l < "$out")" -eq "$(wc -l < "$here/case-calls.txt")" ]
