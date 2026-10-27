#!/bin/sh
# callpath2 S2: per-call costs from compiled code, offline (tools/ai/callpath2-gencheck.el).
# usage: callpath2-gencheck.sh [TREE]   (TREE defaults to this worktree)
D=$(cd "$(dirname "$0")" && pwd)
cd "${1:-$D/../..}" || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/callpath2gen.$$
timeout 550 emacs --batch -Q -L lisp -L src -L scripts -l "$D/callpath2-gencheck.el" > $O 2>&1; rc=$?
grep -E "^(CHECK|GENCHECK|PARSE-ERROR)" $O; grep -m1 "^Error\|^Debugger" $O; rm -f $O; exit $rc
