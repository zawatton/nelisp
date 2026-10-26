#!/bin/bash
# usage: stageBC.sh ROOT BIN [SUFFIXES]   (default "B C")
R=$1; BIN=$2; SUF=${3:-"B C"}
export TMPDIR=$R/target/tmp
ulimit -v 8388608
D=$R/target/bcadopt/probe/run
mkdir -p $R/target/bcadopt/cwd/b; cd $R/target/bcadopt/cwd/b || exit 1
while IFS= read -r n; do
  for m in $SUF; do
    timeout 15 "$BIN" --load "$D/$n.$m.el" -- > "$D/$n.$m.out" 2> "$D/$n.$m.err" < /dev/null
    echo $? > "$D/$n.$m.rc"
  done
done < ${NAMES:-$D/names.txt}
echo done
