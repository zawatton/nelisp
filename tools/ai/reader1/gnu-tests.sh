#!/bin/sh
# usage: gnu-tests.sh BINARY FILE MIN-PASS
# Runs one of GNU Emacs 31.1's own test files (test/src/lread-tests.el, print-tests.el, test/lisp/json-tests.el,
# subr-tests.el) with the ERT shim in target/reader1/rt on BINARY and succeeds when the file loads completely
# and at least MIN-PASS tests pass.  The GNU test tree is read from GNU_TEST_DIR (default
# $HOME/Downloads/emacs-31.1/test); the runner pins itself to CPU 11 and kills a hung test after 30 s.
BIN=$1; F=$2; MIN=$3
HERE=$(cd "$(dirname "$0")" && pwd)
RT=$HERE/../../../target/reader1/rt
case $F in
  lread) REL=src/lread-tests.el;; print) REL=src/print-tests.el;; json) REL=lisp/json-tests.el;; subr) REL=lisp/subr-tests.el;;
  *) echo "unknown file $F"; exit 2;;
esac
TAG=chk$$
OUT=$(cd "$RT" && NL_BIN=$BIN TAG=$TAG timeout 900 python3 run.py "$REL" 2>&1 | tail -1)
rm -f "$RT/res-$TAG.tsv" "$RT"/logs/*-$TAG-*.log
echo "$OUT"
PASS=$(echo "$OUT" | sed -n "s/.*'PASS': \([0-9]*\).*/\1/p"); PASS=${PASS:-0}
# ALLOW_LOADHANG=1: tolerate forms whose evaluation hangs (print-tests print-circle: macroexpansion of a circular
# literal, circular-structure lane), but never a reader failure (#READFAIL).
case "$OUT" in *"#READFAIL"*) echo "reader failure"; exit 1;; esac
[ -z "$ALLOW_LOADHANG" ] && case "$OUT" in *"loadfail: #"*) echo "load failure"; exit 1;; esac
[ "$PASS" -ge "$MIN" ]
