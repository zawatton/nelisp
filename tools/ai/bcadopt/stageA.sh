#!/bin/bash
# usage: stageA.sh ROOT  (runs from ROOT)
cd "$1" || exit 1
export TMPDIR=$PWD/target/tmp
ulimit -v 8388608
rm -f target/bcadopt/work/tuples.tsv target/bcadopt/work/stageA-fail.txt
mkdir -p target/bcadopt/cwd/a
n=0
while IFS= read -r line; do
  printf '%s\n' "$line" > target/bcadopt/cwd/a/one.tsv
  name=${line%%$'\t'*}
  ( cd target/bcadopt/cwd/a && BCCANDS=$PWD/one.tsv BCOUT=$PWD/one.out timeout 30 /usr/local/bin/emacs --batch -Q -l ../../work/gen-tuples.el >/dev/null 2>&1 ) 
  rc=$?
  if [ $rc -ne 0 ] || [ ! -f target/bcadopt/cwd/a/one.out ]; then echo "$name rc=$rc" >> target/bcadopt/work/stageA-fail.txt; else cat target/bcadopt/cwd/a/one.out >> target/bcadopt/work/tuples.tsv; fi
  rm -f target/bcadopt/cwd/a/one.out
  n=$((n+1))
done < target/bcadopt/work/cands.tsv
echo "stageA done n=$n"
