#!/bin/sh
# libcomp-census-check.sh BINARY   (run from the repository root)
# Runs tools/ai/libcomp-census.el on the four acceptance rows and passes when
# every Lisp function called at least once per trip runs native code, except
# the per-generic dispatcher trampoline (`libcomp-g', an `eval'd lambda no
# build can harvest) and the method bodies, and no row is stale.
set -u
BIN=${1:?binary}
ulimit -v 8388608
out=$(LIBCOMP_ROWS="buffers-re-search-simple-i buffers-markers-i adv-run-hooks-3 obj-generic-type-str" \
      timeout 500 "$BIN" --load tools/ai/libcomp-census.el 2>&1)
echo "$out" | grep '^SUMMARY'
bad=$(echo "$out" | awk '$1=="HOT" && $3!="libcomp-g" && $3!="<method>" && $4>=1.0 && $5!="native"')
stale=$(echo "$out" | grep -c 'row-stale')
n=$(echo "$out" | grep -c '^SUMMARY')
[ -n "$bad" ] && echo "NOT NATIVE:" && echo "$bad"
echo "stale=$stale summaries=$n"
test -z "$bad" && test "$stale" = 0 && test "$n" = 4
