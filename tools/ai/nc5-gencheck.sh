#!/bin/sh
# nc5 S2.1/S2.2: generated activation and direct-call shape (tools/ai/nc5-gencheck.el).
cd "$(dirname "$0")/../.." || exit 1
ulimit -v 8388608
O=${TMPDIR:-/tmp}/nc5gen.$$
timeout 280 emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/nc5-gencheck.el > $O 2>&1; rc=$?
tail -3 $O; rm -f $O; exit $rc
