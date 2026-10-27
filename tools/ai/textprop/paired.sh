#!/bin/sh
# paired.sh BIN OUTDIR : paired wall time (7 reps, CPU 15) of key rows: base v28 vs lane vs GNU, plus reg-sum-bc and r3-mix.
# BASE=path of the v28 binary.  The row list and trip counts are in this script.
D=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$D/../../.." && pwd)
R=${RESURVEY24:-$REPO/../resume-4943-20261024/target/resurvey24}
M=${MATRIX:-$REPO/../resume-4943-20261024/target/integ-1034/matrix}
BASE=${BASE:-$REPO/../resume-4943-20261024/target/integ-1046-build/dynamic/nelisp-dynamic}
BIN=$1; OUT=$2; PB=/home/madblack-21/Cowork/Notes/.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py
mkdir -p $OUT
run() { # name driver low high   (retries while CPU 15 or its sibling is busy)
  n=0
  while [ $n -lt 12 ]; do
    rm -rf $OUT/$1
    ( ulimit -v 8388608; timeout 3000 python3 $PB --engine base=$BASE --engine lane=$BIN --engine gnu=/usr/local/bin/emacs \
        --driver $2 --low $3 --high $4 --reps 7 --cpu 15 --out $OUT/$1 > $OUT/$1.log 2>&1 )
    rc=$?
    grep -q "is busy" $OUT/$1.log || break
    n=$((n+1)); sleep 20
  done
  echo "$1 rc=$rc busy-retries=$n"
}
while read name sv lo hi; do
  run $name $R/survey$sv/drv/$name.el $lo $hi
done <<EOT
tp-put-100 4 6000 24000
tp-add-props-100 4 4800 19200
tp-get-text-property 4 8000 32000
tp-next-single-prop-walk 4 11 44
tp-text-property-any 4 7500 30000
tp-buffer-string-props 4 10 40
tp-concat-propertized-10 4 4000 16000
tp-put-whole-100k 4 40000 160000
buffers-textprops-i 2 16000 64000
EOT
run reg-sum-bc $M/drv/reg-sum-bc.el 750000 3000000
run r3-mix $M/drv/r3-mix.el 37500 150000
