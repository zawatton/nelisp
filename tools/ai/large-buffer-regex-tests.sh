#!/bin/sh
# Existing regexp tests (host ERT) plus the standalone regexp smoke.
# Env: NELISP_BIN (default ./target/nelisp), EMACS (default emacs).
emacs=${EMACS:-emacs}
bin=${NELISP_BIN:-./target/nelisp}
rc=0
$emacs --batch -Q -L lisp -L src -L test -l ert \
  -l test/nelisp-regexp-diff-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
$emacs --batch -Q -L lisp -L src -L test -l ert \
  -l test/nelisp-regexp-diff-test.el -f ert-run-tests-batch-and-exit >/dev/null 2>&1 || rc=1
( cd packages/nelisp-regex && $emacs --batch -Q -L src -L test -L ../../lisp -L ../../src -l ert \
  -l test/nelisp-regex-test.el -f ert-run-tests-batch-and-exit >/dev/null 2>&1 ) || rc=1
STANDALONE_BIN=$bin make standalone-reader-regexp-lead-filter-smoke >/dev/null 2>&1 || rc=1
echo "regex tests rc=$rc"
exit $rc
