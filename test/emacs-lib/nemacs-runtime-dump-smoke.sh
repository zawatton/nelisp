#!/bin/sh
# Smoke for `make bake-runtime-dump': the dump starts, reflects the process
# environment, reseeds `random', and the launcher refuses a missing dump.
# Usage: NELISP_HOME=ROOT NELISP_BIN=BIN NEMACS_RUNTIME_DUMP=FILE sh this
set -u
bin=${NELISP_BIN:?}; dump=${NEMACS_RUNTIME_DUMP:?}; root=${NELISP_HOME:?}
exec_sh="$root/scripts/nemacs-runtime-exec.sh"
fail=0
check() { if [ "$2" = "$3" ]; then echo "PASS $1"; else echo "FAIL $1 ($2 != $3)"; fail=1; fi; }
run() { NELISP_HOME=$root "$exec_sh" "$bin" /nonexistent.nlri "$dump" 1 "$1" 2>&1; }
# 1. starts and sees the live environment (not the dumping process's).
out=$(FOO_DUMP_SMOKE=live run '(progn (princ (getenv "FOO_DUMP_SMOKE")) (kill-emacs 0))')
check env-live "$out" live
# 2. reinit reseeds random: two starts differ.
a=$(run '(progn (princ (random 1000000000)) (kill-emacs 0))'); b=$(run '(progn (princ (random 1000000000)) (kill-emacs 0))')
[ "$a" != "$b" ]; check random-reseeded 0 $?
# negative control: without the reinit the seed is frozen in the heap.
c=$(NELISP_HOME=$root "$bin" --cold-load-from "$dump" --eval '(progn (princ (random 1000000000)) (kill-emacs 0))' 2>&1)
d=$(NELISP_HOME=$root "$bin" --cold-load-from "$dump" --eval '(progn (princ (random 1000000000)) (kill-emacs 0))' 2>&1)
check frozen-seed-without-reinit "$c" "$d"
# 3. the launcher refuses a missing dump instead of falling back silently.
NELISP_HOME=$root "$exec_sh" "$bin" /x.nlri /nonexistent.dump 1 '(kill-emacs 0)' >/dev/null 2>&1
[ $? -ne 0 ]; check missing-dump-refused 0 $?
exit $fail
