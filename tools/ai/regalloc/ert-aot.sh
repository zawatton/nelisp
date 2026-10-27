#!/bin/sh
# ert-aot.sh OUTDIR [COMPILER-DIR] : run every test/nelisp-aot-*-test.el (plus vmreg/hotcold/threaded)
# on host Emacs, one process per file (timeout 300 s), with lisp/ from the worktree, or with
# COMPILER-DIR's nelisp-aot-compiler.el loaded first.  Writes OUTDIR/<file>.log and OUTDIR/summary.
cd "$(dirname "$0")/../../.." || exit 1
O=$1; C=$2; mkdir -p "$O"; : > "$O/summary"
for f in test/nelisp-aot-*-test.el test/nelisp-hotcold-test.el test/nelisp-bytecode-core-threaded-test.el; do
  b=$(basename "$f" .el)
  pre=""; [ -n "$C" ] && pre="-l $C/nelisp-aot-compiler.el"
  ( ulimit -v 8388608; timeout 300 /usr/local/bin/emacs -Q --batch -L lisp -L src -L scripts -L test $pre -l "$f" -f ert-run-tests-batch-and-exit > "$O/$b.log" 2>&1 < /dev/null ); rc=$?
  echo "$b rc=$rc $(grep -a -E '^Ran [0-9]+ tests' "$O/$b.log" | tail -1)" >> "$O/summary"
done
