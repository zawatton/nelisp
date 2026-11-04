#!/bin/sh
# Usage: run-nelisp.sh IMAGE [OUTNAME] [CASES]; writes OUTNAME (default nelisp.out)
# and OUTNAME.err.  CASES defaults to cases.el in this directory.
D=$(cd "$(dirname "$0")" && pwd)
IMAGE=${1:?usage: run-nelisp.sh IMAGE [OUTNAME] [CASES]}
OUT=${2:-nelisp.out}
CASES=${3:-cases.el}
(ulimit -v 8388608; ulimit -c 0; cd "$D" && timeout 300 "$IMAGE" --load "$D/$CASES" < /dev/null >"$D/$OUT" 2>"$D/$OUT.err")
echo "exit=$?"
