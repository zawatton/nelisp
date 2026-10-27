#!/bin/sh
# wordroute S2: argv word entries for list-walking handlers, offline (tools/ai/wordroute-gencheck.el).
# usage: wordroute-gencheck.sh [TREE]   (TREE defaults to this worktree)
D=$(cd "$(dirname "$0")" && pwd)
cd "${1:-$D/../..}" || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/wordroutegen.$$
timeout 550 emacs --batch -Q -L lisp -L src -L scripts -l "$D/wordroute-gencheck.el" > $O 2>&1; rc=$?
grep -E "^(CHECK|GENCHECK|GEN-ERROR|LISTSIDE)" $O; grep -m1 "^Error\|^Debugger" $O; rm -f $O; exit $rc
