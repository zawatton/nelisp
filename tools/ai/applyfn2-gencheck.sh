#!/bin/sh
# applyfn2 S2: split applyfn units, word entries, Fapply spread, offline (tools/ai/applyfn2-gencheck.el).
# usage: applyfn2-gencheck.sh [TREE]   (TREE defaults to this worktree)
D=$(cd "$(dirname "$0")" && pwd)
cd "${1:-$D/../..}" || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/applyfn2gen.$$
timeout 590 emacs --batch -Q -L lisp -L src -L scripts -l "$D/applyfn2-gencheck.el" > $O 2>&1; rc=$?
grep -E "^(CHECK|GENCHECK|GEN-ERROR|SPLIT-ERROR|LISTSIDE)" $O; grep -m1 "^Error\|^Debugger" $O; rm -f $O; exit $rc
