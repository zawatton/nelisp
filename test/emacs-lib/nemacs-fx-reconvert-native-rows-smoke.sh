#!/bin/sh
# Smoke for `nemacs-runtime-image-preload--reconvert-native-rows' (F284/F285).
# A row function whose word-VM record was compiled before its callees existed
# (nconc before cddr) is not converted to its native row; the pass compiles it
# again and converts it, and `mapcan', which applies `nconc', links to the new
# record.  The stale state is forced here so the check does not depend on boot
# order.  (The stale `none' of `nelisp--rris' needs the regexp library of a
# replayed image and is checked by the regfix probes; the generic stale-none
# retry is covered by nemacs-fx-retry-stale-none-smoke.sh.)  Negative control:
# NEMACS_RECONV_SKIP=1 skips the passes and the same checks must fail.
# Usage: NELISP_HOME=ROOT NELISP_BIN=BIN sh this
set -u
bin=${NELISP_BIN:?}; root=${NELISP_HOME:?}
here=$(cd "$(dirname "$0")/../.." && pwd)
tmp=$(mktemp -d) || exit 2
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
cat >"$tmp/t.el" <<EOT
;;; -*- lexical-binding: t; -*-
(load "$here/scripts/nemacs-runtime-image-preload.el" nil t t)
(defun rc-body (f) (nthcdr 3 (nelisp--closure-cons (symbol-function f))))
(defun rc-native-p (f)
  (let ((r (gethash (rc-body f) nelisp--fx-table)))
    (and (vectorp r) (integerp (aref r 1)) (< (aref r 1) -16) (memq f nelisp-nc--converted) t)))
;; Force the stale state: a plain VM record, not converted.
(dolist (f '(nconc mapcan))
  (setq nelisp-nc--converted (delq f nelisp-nc--converted))
  (remhash (rc-body f) nelisp--fx-table) (remhash (rc-body f) nelisp--fx-pending)
  (nelisp-nc--orig-register-fn f (symbol-function f)))
(let* ((skip (getenv "NEMACS_RECONV_SKIP"))
       (before (list (rc-native-p 'nconc) (rc-native-p 'mapcan)))
       (n (unless skip
            (nemacs-runtime-image-preload--retry-stale-fx)
            (nemacs-runtime-image-preload--reconvert-native-rows)))
       )
  (princ (format "stale-before=%s nconc-native=%s mapcan-native=%s mapcan-ok=%s reconverted=%s\n"
                 (equal before '(nil nil)) (rc-native-p 'nconc) (rc-native-p 'mapcan)
                 (equal (mapcan #'list '(1 2 3)) '(1 2 3)) (and (integerp n) (>= n 1)))))
EOT
run() { NELISP_HOME=$root "$bin" --load "$tmp/t.el" 2>&1 </dev/null | head -1; }
out=$(run)
want="stale-before=t nconc-native=t mapcan-native=t mapcan-ok=t reconverted=t"
if [ "$out" = "$want" ]; then echo "PASS reconvert-native-rows"; else echo "FAIL reconvert ($out)"; fail=1; fi
neg=$(NEMACS_RECONV_SKIP=1 run)
if [ "$neg" = "$want" ]; then echo "FAIL negative-control-passed-without-pass"; fail=1; else echo "PASS negative-control-fails-without-pass ($neg)"; fi
exit ${fail:-0}
