#!/bin/bash
# usage: run-nl.sh BINARY OUT [slow.el]   (runs under the 8 GiB cap)
d=$(cd "$(dirname "$0")" && pwd)
if [ -n "${3:-}" ]; then export C1_SLOW="$3"; fi
(ulimit -v 8388608; timeout 300 "$1" --load "$d/run.el" -- > "$2" 2> "$2.err"; echo "rc=$?" >> "$2.err")
