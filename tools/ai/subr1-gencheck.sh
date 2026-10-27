#!/bin/sh
# subr1 S2: Subr objects and argv entries, offline (tools/ai/subr1-gencheck.el).
# usage: subr1-gencheck.sh [TREE]   (TREE defaults to this worktree)
D=$(cd "$(dirname "$0")" && pwd)
cd "${1:-$D/../..}" || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/subr1gen.$$
timeout 500 emacs --batch -Q -L lisp -L src -L scripts -l "$D/subr1-gencheck.el" > $O 2>&1; rc=$?
grep -E "^(CHECK|GENCHECK|PARSE-ERROR)" $O; grep -m1 "^Error" $O; rm -f $O; exit $rc
