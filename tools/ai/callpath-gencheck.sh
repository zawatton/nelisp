#!/bin/sh
# callpath S2: direct Subr calls, offline (tools/ai/callpath-gencheck.el).
# usage: callpath-gencheck.sh [TREE]   (TREE defaults to this worktree)
D=$(cd "$(dirname "$0")" && pwd)
cd "${1:-$D/../..}" || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/callpathgen.$$
timeout 550 emacs --batch -Q -L lisp -L src -L scripts -l "$D/callpath-gencheck.el" > $O 2>&1; rc=$?
grep -E "^(CHECK|GENCHECK|PARSE-ERROR)" $O; grep -m1 "^Error\|^Debugger" $O; rm -f $O; exit $rc
