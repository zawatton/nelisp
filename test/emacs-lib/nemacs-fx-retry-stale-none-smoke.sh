#!/bin/sh
# Smoke for `nemacs-runtime-image-preload--retry-stale-fx': a named function
# whose word-VM entry is a stale `none' is compiled again and runs faster;
# a body the VM declines stays `none' without an error.  Negative control:
# NEMACS_FXRETRY_SKIP=1 skips the retry and the same checks must fail.
# Usage: NELISP_HOME=ROOT NELISP_BIN=BIN sh this
set -u
bin=${NELISP_BIN:?}; root=${NELISP_HOME:?}
here=$(cd "$(dirname "$0")/../.." && pwd)
tmp=$(mktemp -d) || exit 2
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
cat >"$tmp/t.el" <<EOT
;;; -*- lexical-binding: t; -*-
(load "$here/scripts/nemacs-runtime-image-preload.el" nil t t)
(defun fxr-good (x) (+ x 1))
(defun fxr-bad (a a) a)
(defun fxr-unq (a) (+ a 2))
(defun fxr-body (f) (nthcdr 3 (nelisp--closure-cons (symbol-function f))))
(puthash (fxr-body 'fxr-good) 'none nelisp--fx-table)
(puthash (fxr-body 'fxr-bad) 'none nelisp--fx-table)
(remhash (fxr-body 'fxr-unq) nelisp--fx-pending)
(remhash (fxr-body 'fxr-unq) nelisp--fx-table)
(let* ((skip (getenv "NEMACS_FXRETRY_SKIP"))
       (n (if skip 0 (nemacs-runtime-image-preload--retry-stale-fx)))
       (good (gethash (fxr-body 'fxr-good) nelisp--fx-table))
       (bad (gethash (fxr-body 'fxr-bad) nelisp--fx-table))
       (queued (gethash (fxr-body 'fxr-unq) nelisp--fx-pending)))
  (princ (format "recovered-ok=%s good-compiled=%s bad-still-none=%s unq-queued=%s\n"
                 (and (integerp n) (>= n 1)) (vectorp good) (eq bad 'none) (eq queued 'fxr-unq))))
EOT
run() { NELISP_HOME=$root "$bin" --load "$tmp/t.el" 2>&1 </dev/null | head -1; }
out=$(run)
want="recovered-ok=t good-compiled=t bad-still-none=t unq-queued=t"
if [ "$out" = "$want" ]; then echo "PASS retry-recompiles-stale-none"; else echo "FAIL retry ($out)"; fail=1; fi
neg=$(NEMACS_FXRETRY_SKIP=1 run)
if [ "$neg" = "$want" ]; then echo "FAIL negative-control-passed-without-retry"; fail=1; else echo "PASS negative-control-fails-without-retry ($neg)"; fi
exit ${fail:-0}
